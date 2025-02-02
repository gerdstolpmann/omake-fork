(*
 * Keep a cache that determines whether we _really_ need to run
 * a command to build a file.
 *
 * The basic idea is this: for each file, remember the command
 * that built it, its Digest, and the Digest of the files it
 * depends on.  If we want to build the node again, and all
 * the parts are the same, then we don't actually need to build
 * it.
 *
 * The same principle works for scanners.
 *)


let debug_cache =
   Lm_debug.create_debug (**)
      { debug_name = "cache";
        debug_description = "Display debugging information for the cache";
        debug_value = false
      }

(*
 * An index table.
 * The indexes are small enough that (-) is a correct ordering function.
 *)
module IndexCompare =
struct
   type t = int
   let compare = (-)
end

module IndexTable = Lm_map.LmMakeList (IndexCompare);;

module IndexNodeTable =
struct
   type t = Omake_node.NodeSet.t IndexTable.t

   let empty = IndexTable.empty

   let add table i node =
      IndexTable.filter_add table i (fun nodes ->
            match nodes with
               Some nodes ->
                  Omake_node.NodeSet.add nodes node
             | None ->
                  Omake_node.NodeSet.singleton node)

   let find = IndexTable.find
end;;

(*
 * Directory entry is a directory or node.
 *)
type dir_entry_core =
   LazyEntryCore of Omake_node.Dir.t * string
 | DirEntryCore  of Omake_cache_type.dir_entry

type dir_listing_item = dir_entry_core ref Lm_string_set.StringTable.t

type dir_listing = dir_listing_item list

(*
 * Executable listing.
 *)
type exe_entry_core =
   ExeEntryCore of (Omake_node.Dir.t * string) list
 | ExeEntryNodes of Omake_node.Node.t list

type exe_listing_item = exe_entry_core ref Lm_string_set.StringTable.t

type exe_listing = exe_listing_item list

type compact_stats = string
  (* File stats as a single string (only for comparison) *)

(*
 * Stats of a file.
 * FreshStats: we have taken the stats during this program run
 * OldStats: we took the stats during a prior run of this program
 *)
(* %%MAGICBEGIN%% *)
type node_stats =
   FreshStats   of compact_stats
 | OldStats     of compact_stats
(* %%MAGICEND%% *)

(*
 * For each file, we keep the stats and the digest.
 *)
(* %%MAGICBEGIN%% *)
type node_memo =
   { nmemo_stats  : node_stats;
     nmemo_digest : Digest.t option;  (* None = not yet computed *)
   }
(* %%MAGICEND%% *)

(*
 * The memo is a record of what we have done during
 * a previous run of this program.
 *
 * The index is a hash over the deps and the commands.
 * The result is mainly used for the scanner, to return
 * the results of the previous scan.
 *
 * We don't really care what the commands are, we just
 * care what their digest is.
 *)
(* %%MAGICBEGIN%% *)
type 'a memo =
   { memo_index        : int;
     memo_deps         : Omake_node.NodeSet.t;
     memo_targets_tab  : tab_entry Omake_node.NodeTable.t;
     memo_deps_tab     : tab_entry Omake_node.NodeTable.t;
     memo_commands     : Omake_command_type.command_digest;
     memo_result       : 'a Omake_cache_type.memo_result;
     memo_updated      : int;
   }

 and tab_entry = 
   (string * Digest.t option) option
   (* None = node does not exist
      Some(cstat,None) = node exists, but digest is unknown
      Some(cstat,Some digest)
    *)

(* %%MAGICEND%% *)

(*
 * The memos are classified by functions.
 * Each function has a table of memos, and an index.
 *)
type key = int

type 'a cache_info =
   { mutable cache_memos     : 'a memo Omake_node.NodeTable.t;
     mutable cache_index     : IndexNodeTable.t
   }

(*
 * The cache remembers all the build commands we did last time.
 *)
type stat = Unix.LargeFile.stats

type cache =
   { (* State *)
     mutable cache_nodes             : node_memo Omake_node.NodeTable.t;
     mutable cache_exists            : bool Omake_node.NodeTable.t;
     mutable cache_exists_num        : int;
     mutable cache_exists_lru        : Omake_node.Node.t list;

     mutable cache_info              : Omake_cache_type.memo_deps cache_info array;
     mutable cache_static_values     : Omake_value_type.obj memo Omake_value_util.ValueTable.t;
     mutable cache_memo_values       : Omake_value_type.obj memo Omake_value_util.ValueTable.t;
     mutable cache_file_stat_count   : int;  (* only succeessful stats are counted *)
     mutable cache_digest_count      : int;

     (* Path lookups *)
     mutable cache_dirs         : (stat option * dir_listing_item) Omake_node.DirTable.t;
     mutable cache_path         : (stat option list * dir_listing_item) Omake_node.DirListTable.t;
     mutable cache_exe_path     : (stat option list * exe_listing_item) Omake_node.DirListTable.t;

     (* Delayed updates *)
     mutable cache_info_delayed : (key * Omake_node.Node.t) Queue.t;

     (* Updates of memos increase the cache_time *)
     mutable cache_time : int;
   }

(*
 * The version of the cache that is saved in the file also
 * contains a table of all nodes.
 *)
(* %%MAGICBEGIN%% *)
type cache_save =
   { save_cache_nodes        : node_memo Omake_node.NodeTable.t;
     save_cache_info         : Omake_cache_type.memo_deps memo Omake_node.NodeTable.t array;
     save_cache_value        : Omake_value_type.obj memo Omake_value_util.ValueTable.t;
     save_cache_time         : int;
   }
(* %%MAGICEND%% *)

(*
 * Squash stat code. (Returned for directories, non-regular files, and nodes
 * where contents do not matter.)
 *)
let squash_stat = "squash"

(*
 * The cache type.
 *)
type t = cache

(*
 * Memo functions for the cache.
 *)
let scanner_fun         = 0
let rule_fun            = 1
let env_fun             = 2
let include_fun         = 3

let env_target  = Omake_node.Node.create_phony_global ".ENV"

(************************************************************************
 * Printing.
 *)

(*
 * Print a digest.
 *)
let pp_print_digest buf digest =
   let s =
      match digest with
         Some(cstats,digest) ->
            "<" ^ cstats ^ "/" ^ Lm_string_util.hexify digest ^ ">"
       | None ->
            "<none>"
   in
      Lm_printf.pp_print_string buf s

(*
 * Print a digest table.
 *)
let pp_print_node_digest_table buf deps =
   Omake_node.NodeTable.iter (fun node digest ->
         Format.fprintf buf "@ %a = %a" (**)
            Omake_node.pp_print_node node
            pp_print_digest digest) deps

(*
 * Print a result.
 *)
let pp_print_memo_result _pp_print_result buf result =
   match result with
      Omake_cache_type.MemoSuccess _ ->
         Format.fprintf buf "@ success"
    | MemoFailure code ->
         Format.fprintf buf "@ failed(%d)" code

let name_of node =
  let buf = Buffer.create 80 in
  let formatter = Format.formatter_of_buffer buf in
  Omake_node.pp_print_node formatter node;
  Format.pp_print_flush formatter();
  Buffer.contents buf


let string_of_cmdhash =
  function
  | None -> "n/a"
  | Some dg -> Lm_string_util.hexify dg


(*
 * Print a memo.
 *)
(* let pp_print_memo buf memo = *)
(*    let { memo_index    = index; *)
(*          memo_targets  = targets; *)
(*          memo_deps     = deps; *)
(*          memo_result   = _result; *)
(*          memo_commands = commands *)
(*        } = memo *)
(*    in *)
(*       Format.fprintf buf "@[<hv 0>@[<hv 3>memo {@ index = %d;@ @[<b 3>targets =%a@];@ @[<b 3>deps =%a@]@ @[<hv 3>commands =%a@]@]@ }@]" (\**\) *)
(*          index *)
(*          pp_print_node_digest_table targets *)
(*          pp_print_node_digest_table deps *)
(*          pp_print_digest commands *)

(************************************************************************
 * Persistence.
 *)

(*
 * Create the cache.  We save the cache in a file, so try to
 * load it from that file.
 *)
let magic_number = Omake_magic.cache_magic
let input_magic  = Omake_magic.input_magic
let output_magic = Omake_magic.output_magic

(*
 * Create a new cache.
 *)
let create () =
   { cache_nodes           = Omake_node.NodeTable.empty;
     cache_exists          = Omake_node.NodeTable.empty;
     cache_exists_num      = 0;
     cache_exists_lru      = [];
     cache_info            = [||];
     cache_static_values   = Omake_value_util.ValueTable.empty;
     cache_memo_values     = Omake_value_util.ValueTable.empty;
     cache_file_stat_count = 0;
     cache_digest_count    = 0;
     cache_dirs            = Omake_node.DirTable.empty;
     cache_path            = Omake_node.DirListTable.empty;
     cache_exe_path        = Omake_node.DirListTable.empty;
     cache_info_delayed    = Queue.create();
     cache_time            = 0;
   }

let rehash cache =
   cache.cache_dirs <- Omake_node.DirTable.empty;
   cache.cache_path <- Omake_node.DirListTable.empty;
   cache.cache_exe_path <- Omake_node.DirListTable.empty

let stats { cache_file_stat_count = stat_count;
            cache_digest_count = digest_count;
            _
    } =
   stat_count, digest_count

(*
 * When the file is saved, remove all stat information,
 * as well as any files that don't exist.
 *)
let save_of_cache cache =
   let { cache_nodes  = cache_nodes;
         cache_info   = cache_info;
         cache_static_values = cache_values;
         _
       } = cache
   in
   let cache_nodes =
      Omake_node.NodeTable.fold (fun nodes target nmemo ->
            match nmemo.nmemo_stats with
               FreshStats stats
             | OldStats stats ->
                  let nmemo = { nmemo with nmemo_stats = OldStats stats } in
                     Omake_node.NodeTable.add nodes target nmemo
      ) Omake_node.NodeTable.empty cache_nodes
   in
   let cache_info = Array.map (fun info -> info.cache_memos) cache_info in
      { save_cache_nodes = cache_nodes;
        save_cache_info  = cache_info;
        save_cache_value = cache_values;
        save_cache_time = cache.cache_time
      }

(*
 * Rebuild the index from the memos.
 *)
let create_index memos =
  Omake_node.NodeTable.fold (fun (memos, index) target memo ->
    let { memo_result = result;
          memo_index = hash;
          _
        } = memo
    in
    match result with
      Omake_cache_type.MemoSuccess _ ->
      let index = IndexNodeTable.add index hash target in
      memos, index
    | MemoFailure _ ->
      let memos = Omake_node.NodeTable.remove memos target in
      memos, index) (memos, IndexNodeTable.empty) memos

(*
 * Rebuild the cache from the saved version.
 *)
let cache_of_save options save =
   let { save_cache_nodes = cache_nodes;
         save_cache_info  = cache_info;
         _
       } = save
   in
   let flush_scanner = Omake_options.opt_flush_dependencies options in
   let cache_info =
      Array.mapi (fun idx memos ->
            if flush_scanner && idx = scanner_fun then
               { cache_memos = Omake_node.NodeTable.empty;
                 cache_index = IndexNodeTable.empty
               }
            else
               let memos, index = create_index memos in
                  { cache_memos = memos;
                    cache_index = index
                  }) cache_info
   in
   let cache_values =
      if Omake_options.opt_flush_static options then
         Omake_value_util.ValueTable.empty
      else
         save.save_cache_value
   in
      { (create ()) with cache_nodes  = cache_nodes;
                         cache_info   = cache_info;
                         cache_static_values = cache_values;
                         cache_time = save.save_cache_time;
      }

(*
 * Load the old cache from a file.
 *)
let from_channel options inx =
   if Lm_debug.debug debug_cache then
     Format.eprintf "Loading cache@.";
   if Omake_options.opt_flush_cache options then
      create ()
   else
      let magic = input_magic inx in
      let _ =
         if magic <> magic_number then
            raise (Sys_error "bad magic number")
      in
      let save = Marshal.from_channel inx in
         cache_of_save options save

(*
 * Save the cache to the file.
 *)
let to_channel outx cache =
   if Lm_debug.debug debug_cache then
     Format.eprintf "Saving cache@.";
   output_magic outx magic_number;
   Marshal.to_channel outx (save_of_cache cache) []

(************************************************************************
 * File existence.
 *)

(* Caching the existence of files seems to have a good performance effect on
   Windows. There is no benefit on Linux. We try to keep the size of the
   cache small (fast lookups, no waste of RAM), as the locality of the
   accesses is high.

   NB. In older omake versions there was even a cache for stat records.
   We don't need this the complete records for regular builds, though. Only the
   file existence is important.
 *)

let ex_limit = 100
  (* we start a GC pass at 2*ex_limit and keep only ex_limit entries (lru) *)

let ex_query cache node =
  let ex_flag = Omake_node.NodeTable.find cache.cache_exists node in
  cache.cache_exists_lru <- node :: cache.cache_exists_lru;
  ex_flag

exception Ex_result of bool Omake_node.NodeTable.t * int

let ex_collect cache =
  let old = cache.cache_exists in
  let ex, num =
    try
      List.fold_left
        (fun (ex,num) node ->
           try
             if num >= ex_limit then
               raise(Ex_result(ex,num))
             else if not (Omake_node.NodeTable.mem ex node) then
               let ex_flag = Omake_node.NodeTable.find old node in
               (Omake_node.NodeTable.add ex node ex_flag, num+1)
             else
               (ex,num)
           with Not_found -> (ex,num)
        )
        (Omake_node.NodeTable.empty, 0)
        cache.cache_exists_lru
    with
      | Ex_result(ex,num) -> (ex,num) in
  cache.cache_exists <- ex;
  cache.cache_exists_num <- num;
  cache.cache_exists_lru <- []


let ex_set cache node ex_flag =
   if Lm_debug.debug debug_cache then
     Format.eprintf "Caching existence: ex_set(%s,%B)@."
                    (name_of node) ex_flag;
  cache.cache_exists_lru <- node :: cache.cache_exists_lru;
  try
    let old_flag = Omake_node.NodeTable.find cache.cache_exists node in
    if ex_flag <> old_flag then
      cache.cache_exists <- 
        Omake_node.NodeTable.add cache.cache_exists node ex_flag
  with
    | Not_found ->
        cache.cache_exists <- 
          Omake_node.NodeTable.add cache.cache_exists node ex_flag;
        cache.cache_exists_num <- cache.cache_exists_num + 1;
        if cache.cache_exists_num > 2*ex_limit then
          ex_collect cache


let ex_reset cache node =
   if Lm_debug.debug debug_cache then
     Format.eprintf "Resetting existence: ex_reset(%s)@."
                    (name_of node);
  cache.cache_exists <- 
    Omake_node.NodeTable.remove cache.cache_exists node
  (* we don't update cache_exists_num, which is only an upper bound *)
  


(************************************************************************
 * Stat.
 *)

(*
 * Reset the information about a node (probably because
 * we ran a command that changed it).
 *)
let reset cache node =
   if Lm_debug.debug debug_cache then
     Format.eprintf "Oldifying stats: reset(%s)@." (name_of node);
   let nodes = cache.cache_nodes in
   let nodes =
      try
         Omake_node.NodeTable.filter_remove nodes node (fun nmemo ->
               match nmemo.nmemo_stats with
                  FreshStats stats
                | OldStats stats ->
                     Some { nmemo with nmemo_stats = OldStats stats })
      with
         Not_found ->
            nodes
   in
   cache.cache_nodes <- nodes;
   ex_reset cache node

let reset_set cache nodes =
   Omake_node.NodeSet.iter (reset cache) nodes

let reset_table cache nodes =
   Omake_node.NodeTable.iter (fun node _ -> reset cache node) nodes

let compact_stats s =
  let open Unix.LargeFile in
  Printf.sprintf "%x:%x:%Lx:%Lx"
    s.st_dev
    s.st_ino
    s.st_size
    (Int64.bits_of_float s.st_mtime)

(*
 * Test whether stats are equal enough that we think the
 * file is up-to-date.
 *)
let stats_equal stat1 stat2 =
   stat1 = stat2

(* let pp_print_stat buf (stat : Unix.LargeFile.stats) = *)
(*   match stat with  *)
(*     { st_ino  = ino; *)
(*       st_kind = _kind; *)
(*       st_size = size; *)
(*       st_mtime = mtime; *)
(*       _ *)
(*     } -> *)
(*     Format.fprintf buf "ino = %d, size = %Ld, mtime = %g" ino size mtime *)

let digest_file name _file_size =
  Digest.file name

let stat_file_update_digest ?old cache node =
  let nodes = cache.cache_nodes in
  let name = Omake_node.Node.fullname node in
  ( try
      let stats = Unix.LargeFile.stat name in
      let cstats = compact_stats stats in
      ex_set cache node true;
      if stats.Unix.LargeFile.st_kind <> Unix.S_REG then (
        if Lm_debug.debug debug_cache then
          Format.eprintf "Removing stats (type change): \
                          stat_file_update_digest(%s)@." (name_of node);
        cache.cache_nodes <- Omake_node.NodeTable.remove nodes node;
        Some (cstats, squash_stat)
      )
      else (
        cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
        let digest =
          (* Old: do a conditional update of the digest *)
          match old with
            | Some ({ nmemo_digest = Some digest; _ },cstats') 
                  when stats_equal cstats cstats' ->
                if Lm_debug.debug debug_cache then
                  Format.eprintf "Keeping stats and digest: \
                                  stat_file_update_digest(%s) dg=%s@."
                                 (name_of node) (Lm_string_util.hexify digest);
                digest
            | _ ->
                let digest = digest_file name stats.Unix.LargeFile.st_size in
                if Lm_debug.debug debug_cache then
                  Format.eprintf "Renewing stats and digest: \
                                  stat_file_update_digest(%s) dg=%s@."
                                 (name_of node) (Lm_string_util.hexify digest);
                cache.cache_digest_count <- succ cache.cache_digest_count;
                digest in
        let nmemo =
          { nmemo_stats = FreshStats cstats;
            nmemo_digest = Some digest
          } in
        cache.cache_nodes <- Omake_node.NodeTable.add nodes node nmemo;
        Some (cstats, digest)
      )
    with
      | Unix.Unix_error _
      | Sys_error _ ->
          if Lm_debug.debug debug_cache then
            Format.eprintf "Removing stats (lookup error): \
                            stat_file_update_digest(%s)@." (name_of node);
          cache.cache_nodes <- Omake_node.NodeTable.remove nodes node;
          ex_set cache node false;
          None
  )

(*
 * Stat a file. (Returns compact_stat * digest.)
 *)
let stat_file cache node =
   let nodes = cache.cache_nodes in
   let old_nmemo =
      try Some (Omake_node.NodeTable.find nodes node) with
         Not_found ->
            None in
   match old_nmemo with
     | Some { nmemo_stats = FreshStats cstats;
              nmemo_digest = Some digest
            } ->
         (* We've recently computed the digest for this file. *)
         Some(cstats, digest)

     | Some { nmemo_stats = FreshStats _;
              nmemo_digest = None
            } ->
         (* The stats are recent, but no digest yet. So do a full update *)
         stat_file_update_digest cache node

     | Some ({ nmemo_stats = OldStats cstats ; _} as nmemo) ->
         (* We have no recent record of this file.
            Get current stats.  If they match, then
            use current digest; otherwise recompute.
          *)
         stat_file_update_digest ~old:(nmemo, cstats) cache node

     | None ->
         (* We've never seen this file before. Get complete stats,
             including a digest.
          *)
         stat_file_update_digest cache node

let compact_stat_file cache node =
  (* get only compact_stat *)
  let nodes = cache.cache_nodes in
  let name = Omake_node.Node.fullname node in
  let old_nmemo =
    try Some (Omake_node.NodeTable.find nodes node) with
        Not_found ->
      None in
  match old_nmemo with
    | Some { nmemo_stats = FreshStats cstats; _ } ->
        if Lm_debug.debug debug_cache then
          Format.eprintf "Keeping stats: \
                          compact_stat_file(%s)@." (name_of node);
        Some cstats
    | _ ->
        ( try
            let stats = Unix.LargeFile.stat name in
            let cstats = compact_stats stats in
            ex_set cache node true;
            if stats.Unix.LargeFile.st_kind <> Unix.S_REG then (
              if Lm_debug.debug debug_cache then
                Format.eprintf "Removing stats (type change): \
                                compact_stat_file(%s)@." (name_of node);
              cache.cache_nodes <- Omake_node.NodeTable.remove nodes node
            ) else (
              let old_digest =
                match old_nmemo with
                  | Some ({ nmemo_stats = OldStats ocstats ; _} as nmemo) ->
                      if ocstats = cstats then (
                        if Lm_debug.debug debug_cache then
                          Format.eprintf
                            "Keeping stats and digest: \
                             compact_stat_file(%s)@." (name_of node);
                        nmemo.nmemo_digest
                      )
                      else (
                        if Lm_debug.debug debug_cache then
                          Format.eprintf
                            "Renewing stats and removing digest: \
                             compact_stat_file(%s)@." (name_of node);
                        None
                      )
                  | _ -> None in
              let nmemo =
                { nmemo_stats = FreshStats cstats;
                  nmemo_digest = old_digest;
                } in
              cache.cache_nodes <- Omake_node.NodeTable.add nodes node nmemo;
            );
            Some cstats
          with
            | Unix.Unix_error _
            | Sys_error _ ->
                if Lm_debug.debug debug_cache then
                  Format.eprintf
                    "Removing stats (lookup error): \
                     compact_stat_file(%s)@." (name_of node);
                cache.cache_nodes <- Omake_node.NodeTable.remove nodes node;
                ex_set cache node false;
                None
        )


let has_file_stat_1 ?compact_stat ?digest cache node : bool option =
  (* Check whether a file has certain stats:
      - [compact_stat]: if passed, checks whether the file exists and has
        these stats
      - [digest]: if passed, checks whether the file exists and has this
        digest
 
     Returns None if the file does not exist. Otherwise [Some b], and [b]
     says whether the above criteria are fulfilled.
   *)
  let st_check cstats =
    match compact_stat with
      | Some c -> stats_equal c cstats
      | None -> true in
  let check cstats dg_opt =
    try
      Some
        ( st_check cstats &&
            match digest, dg_opt with
              | (Some d1, Some d2) -> d1 = d2
              | (Some d1, None) ->
                  ( match stat_file cache node with
                      | Some(cstats1, dg) -> st_check cstats1 && d1 = dg
                      | None -> raise Not_found
                  )
              | (None, _) -> true
        )
    with Not_found -> None in

  let nodes = cache.cache_nodes in
  let old_nmemo =
    try Some (Omake_node.NodeTable.find nodes node) with
        Not_found -> None in
  match old_nmemo with
    | Some { nmemo_stats = FreshStats cstats;
             nmemo_digest = Some digest
           } ->
        check cstats (Some digest)

    | Some { nmemo_stats = FreshStats cstats;
             nmemo_digest = None
           } ->
        check cstats None

    | _ ->
        if digest = None then
          match compact_stat_file cache node with
            | None -> None
            | Some cstats -> check cstats None
        else
          match stat_file cache node with
            | None -> None
            | Some(cstats,dg) -> check cstats (Some dg)


let has_file_stat ?compact_stat ?digest cache node =
  if Lm_debug.debug debug_cache then (
    let result =  has_file_stat_1 ?compact_stat ?digest cache node in
    Format.eprintf
      "has_file_stat(%s,compact=%s,digest=%s) = %s@."
      (name_of node)
      (match compact_stat with
         | None -> "n/a"
         | Some cstat -> cstat
      )
      (string_of_cmdhash digest)
      (match result with
         | None -> "None"
         | Some b -> string_of_bool b
      );
    result
  )
  else
    has_file_stat_1 ?compact_stat ?digest cache node


let unix_filename_exists path =
  let basedir = Filename.dirname path in
  let basename = Filename.basename path in
  basename = "." || basename = ".." || Array.exists (String.equal basename) (Sys.readdir basedir)


let stat_unix_node cache ~force node =
  (* We used to cache this information, but don't do this anymore. Hence,
     the [force] flag is ignored: we always re-stat
   *)
  ignore(force);
  let name = Omake_node.Node.fullname node in
  try
    let s = Unix.LargeFile.stat name in
    if not @@ unix_filename_exists name then raise (Sys_error "");
    ex_set cache node true;
    s
  with
      Unix.Unix_error _
    | Sys_error _ ->
        ex_set cache node false;
         raise Not_found

(*
 * lstat versions, for not following symlinks.
 *)

let lstat_unix_node _cache ~force node =
  (* We used to cache this information, but don't do this anymore. Hence,
     the [force] flag is ignored: we always re-stat
   *)
  ignore(force);
  let name = Omake_node.Node.fullname node in
  try
    let s = Unix.LargeFile.lstat name in
    if not @@ unix_filename_exists name then raise (Sys_error "");
    s
  with
      Unix.Unix_error _
    | Sys_error _ ->
         raise Not_found

(*
 * Cached stat.
 *)
let stat_unix cache ?(force=false) ?(follow_symlinks=true) node =
   let node =
      match Omake_node.Node.kind node with
         NodePhony
       | NodeScanner ->
            raise Not_found
       | NodeNormal ->
            node
       | NodeOptional
       | NodeSquashed
       | NodeExists ->
            Omake_node.Node.core node
   in
      if follow_symlinks then
         stat_unix_node cache ~force node
      else
         lstat_unix_node cache ~force node

(*
 * Check if a file is a directory.
 *)
let is_dir cache ?(force=false) ?(follow_symlinks=true) node =
   try (stat_unix cache ~force ~follow_symlinks node).Unix.LargeFile.st_kind = Unix.S_DIR with
      Not_found ->
         false

(*
 * Tests for whether a file is executable.
 * This really only works on Unix.
 *)
let euid =
   try Unix.geteuid () with
      Unix.Unix_error _ ->
         0

let groups =
   try Array.to_list (Unix.getgroups ()) with
      Unix.Unix_error _ ->
         []

(*
 * Check if the node is phony first.
 *)
let stat cache node =
(* Printf.eprintf "stat node=%s\n%!" (name_of node); *)
   let core = Omake_node.Node.core node in
      match Omake_node.Node.kind node with
         NodePhony
       | NodeScanner ->
            None
       | NodeOptional
       | NodeNormal ->
            stat_file cache core
       | NodeSquashed
       | NodeExists ->
            (match stat_file cache core with
               | Some(cstats,_) ->
                   Some(cstats,squash_stat)
               | None ->
                   None
            )


let has_stat ?compact_stat ?digest cache node : bool option =
   let core = Omake_node.Node.core node in
      match Omake_node.Node.kind node with
         NodePhony
       | NodeScanner ->
            None
       | NodeOptional
       | NodeNormal ->
            has_file_stat ?compact_stat ?digest cache core
       | NodeSquashed
       | NodeExists ->
            (match stat_file cache core with
               | Some _ ->
                   Some true
               | None ->
                   None
            )


let compact_stat cache node =
   let core = Omake_node.Node.core node in
      match Omake_node.Node.kind node with
         NodePhony
       | NodeScanner ->
            None
       | NodeOptional
       | NodeNormal
       | NodeSquashed
       | NodeExists ->
            compact_stat_file cache core
  

(*
 * Turn a set into a table of stat info.
 *)
let stat_set cache nodes =
   Omake_node.NodeSet.fold (fun table node ->
         Omake_node.NodeTable.add table node (stat cache node)) 
    Omake_node.NodeTable.empty nodes

let stat_set_for_add ?(compact=false) cache nodes =
   Omake_node.NodeSet.fold (fun table node ->
         let out =
           if compact then
             match compact_stat cache node with
               | None -> None
               | Some cstat -> Some (cstat, None)
           else
             match stat cache node with
               | None -> None
               | Some(cstat,dg) -> Some (cstat, Some dg) in
         Omake_node.NodeTable.add table node out) 
    Omake_node.NodeTable.empty nodes


let complement_stat_set_for_add cache node_tab =
  Omake_node.NodeTable.mapi
    (fun node info_opt ->
       match info_opt with
         | None ->
             None
         | Some(cstat, None) ->
             ( match stat cache node with
                 | None -> info_opt   (* "None" would be surprising *)
                 | Some(cstat_new, digest) ->
                     if cstat = cstat_new then
                       Some(cstat, Some digest)
                     else
                       info_opt  (* be conservative here *)
             )
         | Some(_, Some _) ->
             info_opt
    )
    node_tab



let stat_table cache nodes =
   Omake_node.NodeTable.mapi (fun node _ -> stat cache node) nodes

(*
 * Force a stat.
 *)
let force_stat cache node =
   reset cache node;
   stat cache node

let force_stat_set cache nodes =
   Omake_node.NodeSet.fold (fun table node ->
         Omake_node.NodeTable.add table node (force_stat cache node)) 
    Omake_node.NodeTable.empty nodes

let force_stat_table cache nodes =
   Omake_node.NodeTable.mapi (fun node _ -> force_stat cache node) nodes

(*
 * Check if the stat changed.
 *)
let stat_changed cache node =
   let old_digest =
      try (Omake_node.NodeTable.find cache.cache_nodes node).nmemo_digest with
         Not_found ->
            None
   in
   match force_stat cache node with
     | Some(_, dg) -> old_digest <> Some dg
     | None -> old_digest <> None

(*
 * Check if a file exists.
 *)
let exists cache ?(force=false) node =
   try
     if force then raise Not_found;
     ex_query cache node
   with
     | Not_found ->
         try
           ignore (stat_unix cache ~force node); true
         with
             Not_found -> Omake_node.Node.always_exists node

let exists_dir cache ?(force=false) dir =
   exists cache ~force (Omake_node.Node.node_of_dir dir)

(************************************************************************
 * Adding to the cache.
 *)

(*
 * Hash a set of deps and commands.
 * The commands is a digest.
 *)
let hash_index deps commands =
   let index =
      Omake_node.NodeSet.fold (fun index node ->
            index lxor (index lsl 4) lxor (index lsr 4) lxor (Omake_node.Node.hash node)) 0 deps
   in
   let index = index lxor (index lsl 4) lxor (index lsr 4) lxor (Hashtbl.hash commands) in
      index land 0x3fffffff

(*
 * Expand the cache_info if necessary.
 *)
let get_info cache key =
   let cache_info = cache.cache_info in
   let len = Array.length cache_info in
   let cache_info =
      if key >= Array.length cache_info then
         begin
            let cache_info' =
               Array.init (succ key) (fun _ ->
                     { cache_memos = Omake_node.NodeTable.empty;
                       cache_index = IndexNodeTable.empty
                     })
            in
               Array.blit cache_info 0 cache_info' 0 len;
               cache.cache_info <- cache_info';
               cache_info'
         end
      else
         cache_info
   in
      cache_info.(key)

(*
 * Add a command.
 *)
let add cache key target targets deps commands result =
  if Lm_debug.debug debug_cache then
    Format.eprintf
      "Adding rule digests: add(key:%d,target:%s)@." key (name_of target);

   let index = hash_index deps commands in
   let memo =
      { memo_index        = index;
        memo_deps         = deps;
        memo_targets_tab  = stat_set_for_add ~compact:true cache targets;
        memo_deps_tab     = stat_set_for_add ~compact:true cache deps;
        memo_result       = result;
        memo_commands     = commands;
        memo_updated      = cache.cache_time
      }
   in
   let info = get_info cache key in
   info.cache_memos <- Omake_node.NodeTable.add info.cache_memos target memo;
   info.cache_index <- IndexNodeTable.add info.cache_index index target;
   cache.cache_time <- cache.cache_time + 1;
   (* schedule to add the digests later: *)
   Queue.add (key, target) cache.cache_info_delayed


let is_digest_missing node_tab =
  Omake_node.NodeTable.exists
    (fun _ info_opt ->
       match info_opt with
         | Some(_, None) -> true
         | _ -> false
    )
    node_tab


let add_digests cache key target =
  try
    let info = get_info cache key in
    let memo = Omake_node.NodeTable.find info.cache_memos target in
    let p1 = is_digest_missing memo.memo_targets_tab in
    let p2 = is_digest_missing memo.memo_deps_tab in
    if p1 || p2 then (
      if Lm_debug.debug debug_cache then
        Format.eprintf
          "Updating rule digests: add_digests(key:%d,target:%s)@."
          key (name_of target);
      let memo' =
        { memo with
          memo_targets_tab = 
            (if p1 then complement_stat_set_for_add cache memo.memo_targets_tab
             else memo.memo_targets_tab);
          memo_deps_tab = 
            (if p1 then complement_stat_set_for_add cache memo.memo_deps_tab
             else memo.memo_deps_tab);
          memo_updated = cache.cache_time
        } in
      info.cache_memos <- Omake_node.NodeTable.add info.cache_memos target memo';
      cache.cache_time <- cache.cache_time + 1;
    )
  with
    | Not_found -> ()


(*
 * Check the target digest.
 *)

let targets_equal_1 cache targets_tab =
  (* A quick check only using Unix.stat. Not_found if files are missing *)
  Omake_node.NodeTable.forall
    (fun target old_stat ->
       match old_stat with
         | Some (cstats, _) ->
             ( match has_stat ~compact_stat:cstats cache target with
                 | Some b -> b
                 | None -> raise Not_found  (* target does not exist *)
             )
         | None -> (* the recorded target did not exist as file *)
             has_stat cache target = None
    )
    targets_tab                        


let targets_equal_2 cache targets_tab =
  (* A deep check using digests (if available) *)
  Omake_node.NodeTable.forall
    (fun target old_stat ->
       match old_stat with
         | Some (_, Some digest) ->
             has_stat ~digest cache target = Some true
         | Some (cstats, None) ->
             has_stat ~compact_stat:cstats cache target = Some true
         | None -> (* the recorded target did not exist as file *)
             has_stat cache target = None
    )
    targets_tab                        
    
let targets_equal cache targets_tab =
  try
    targets_equal_1 cache targets_tab || targets_equal_2 cache targets_tab
  with
    | Not_found -> false

(*
 * Check if deps are the same.
 * This returns true if the deps are equal (deps: the nodes as of now;
 * deps_tab: the recorded properties from the last run).
 * If not, it either returns false, or raises Not_found.
 *)

let deps_equal_1 cache deps deps_tab =
  (* A quick check only using Unix.stat. Not_found if files are missing *)
  let count1 = Omake_node.NodeSet.cardinal deps in
  let count2 = Omake_node.NodeTable.cardinal deps_tab in
  (count1 = count2) && 
    Omake_node.NodeSet.for_all
      (fun dep ->
         match Omake_node.NodeTable.find deps_tab dep with
           | Some(cstats,_) ->
               ( match has_stat ~compact_stat:cstats cache dep with
                   | None -> raise Not_found
                   | Some ok -> ok
               )
           | None -> (* the recorded dep did not exist as file *)
               has_stat cache dep = None
      )
      deps


let deps_equal_2 cache deps deps_tab =
  (* A deep check using digests (if available) *)
  let count1 = Omake_node.NodeSet.cardinal deps in
  let count2 = Omake_node.NodeTable.cardinal deps_tab in
  (count1 = count2) && 
    Omake_node.NodeSet.for_all
      (fun dep ->
         match Omake_node.NodeTable.find deps_tab dep with
           | Some (_, Some digest) ->
               has_stat ~digest cache dep = Some true
           | Some (cstats, None) ->
               has_stat ~compact_stat:cstats cache dep = Some true
           | None -> (* the recorded dep did not exist as file *)
               has_stat cache dep = None
      )
      deps
      
let deps_equal cache deps deps_tab =
  try
    deps_equal_1 cache deps deps_tab || deps_equal_2 cache deps deps_tab
  with
    | Not_found -> false

(*
 * Find a memo from the deps and commands. Note that there can be several
 * memos for a pair (deps,commands) when the rule has several effects. In
 * that case we return the most recently updated version.
 *)
let find_memo cache key deps commands =
   let { cache_memos = memos;
         cache_index = index
       } = get_info cache key
   in
   let hash = hash_index deps commands in
   let rec search best_time best targets =
     match targets with
       | target :: targets ->
           let memo = Omake_node.NodeTable.find memos target in
           let { memo_index = hash';
                 memo_deps  = deps';
                 memo_commands = commands';
                 memo_updated = time;
                 _
               } = memo
           in
            if time > best_time &&
               hash' = hash &&
               commands = commands' &&
               Omake_node.NodeSet.equal deps deps'
            then
              search time (Some memo) targets
            else
              search best_time best targets
    | [] ->
        ( match best with
            | None -> raise Not_found
            | Some memo -> memo
        ) in
   let targets = Omake_node.NodeSet.to_list (IndexNodeTable.find index hash) in
   search (-1) None targets

(*
 * A memo has not changed if all the deps and the target
 * have the same digests.
 *)
let up_to_date cache key deps commands =
   try
      let memo = find_memo cache key deps commands in
         if targets_equal cache memo.memo_targets_tab &&
              deps_equal cache deps memo.memo_deps_tab
         then
           match memo.memo_result with
               MemoSuccess _ ->
               true
             | MemoFailure _ ->
                 false
         else
           false
   with
       Not_found -> false

let up_to_date_status cache key deps commands =
  try
    let memo = find_memo cache key deps commands in
    let p1 = targets_equal cache memo.memo_targets_tab in
    let p2 = lazy(deps_equal cache deps memo.memo_deps_tab) in
    if Lm_debug.debug debug_cache then
      Format.eprintf "up_to_date_status(cmdhash=%s): \
                      targets_equal=%B deps_equal=%B"
                     (string_of_cmdhash commands) p1 (Lazy.force p2);
    if p1 && Lazy.force p2 then (
      match memo.memo_result with
        | Omake_cache_type.MemoSuccess _ ->
            if Lm_debug.debug debug_cache then
              Format.eprintf " status=StatusSuccess@.";
            Omake_cache_type.StatusSuccess
        | MemoFailure code ->
            if Lm_debug.debug debug_cache then
              Format.eprintf " status=StatusFailure@.";
            StatusFailure code
    ) else (
      if Lm_debug.debug debug_cache then
        Format.eprintf " status=StatusUnknown@.";
      StatusUnknown
    )
  with
    Not_found ->
    if Lm_debug.debug debug_cache then
      Format.eprintf " status=StatusUnknown (Not_found)@.";
    StatusUnknown

(*
 * A memo has not changed if all the deps and the target
 * have the same digests.
 *)
let target_results results =
  match results with
    Omake_cache_type.MemoFailure _ ->
    raise Not_found
  | MemoSuccess deps ->
    deps

let find_result cache key deps commands =
   let memo = find_memo cache key deps commands in
   let { memo_targets_tab = targets_tab;
         memo_deps_tab = deps_tab;
         memo_result  = result;
         _
       } = memo
   in
      if targets_equal cache targets_tab && deps_equal cache deps deps_tab then
         target_results result
      else
         raise Not_found

(*
 * Find the result of a run, without the commands.
 *)
let find_result_sloppy cache key target =
   let { cache_memos = memos ; _} = get_info cache key in
   let memo = Omake_node.NodeTable.find memos target in
      match memo.memo_result with
         MemoFailure _ ->
            raise Not_found
       | MemoSuccess deps ->
            deps

(************************************************************************
 * Delayed stat requests
 *)

let process_delayed_digests cache =
  if Lm_debug.debug debug_cache then
    Format.eprintf
      "Delayed stats: <start>@.";
  ( try
      while true do
        let (key, target) = Queue.take cache.cache_info_delayed in
        add_digests cache key target
      done
    with
      | Queue.Empty ->
          ()
  );
  if Lm_debug.debug debug_cache then
    Format.eprintf
      "Delayed stats: <end>@."

(************************************************************************
 * Values.  In this case, we use the key to find the memo.
 *)

(*
 * Get the memo entry from the key.
 *)
let get_value cache key is_static =
   Omake_value_util.ValueTable.find (if is_static then cache.cache_static_values else cache.cache_memo_values) key

(*
 * Find a memo from the deps and commands.
 *)
let find_value_memo cache key is_static deps1 commands1 =
   let memo = get_value cache key is_static in
   let hash1 = hash_index deps1 commands1 in
   let { memo_index = hash2;
         memo_deps  = deps2;
         memo_commands = commands2;
         _
       } = memo
   in
      if hash1 = hash2 && commands1 = commands2 && 
           Omake_node.NodeSet.equal deps1 deps2 then
        memo
      else
         raise Not_found

let find_value cache key is_static deps commands =
   let memo = find_value_memo cache key is_static deps commands in
   let { memo_targets_tab = targets_tab;
         memo_deps_tab = deps_tab;
         memo_result  = result;
         _
       } = memo
   in
      if targets_equal cache targets_tab && deps_equal cache deps deps_tab then
         target_results result
      else
         raise Not_found

let add_value cache key is_static deps commands result =
   let index = hash_index deps commands in
   let memo =
      { memo_index       = index;
        memo_deps        = deps;
        memo_targets_tab = Omake_node.NodeTable.empty;
        memo_deps_tab    = stat_set_for_add ~compact:false cache deps;
        memo_result      = result;
        memo_commands    = commands;
        memo_updated     = cache.cache_time
      } in
   cache.cache_time <- cache.cache_time + 1;
   if is_static then
     cache.cache_static_values <- Omake_value_util.ValueTable.add cache.cache_static_values key memo
   else
     cache.cache_memo_values <- Omake_value_util.ValueTable.add cache.cache_memo_values key memo

(************************************************************************
 * Directory listings.
 *)

(*
 * When auto-rehash is in effect, we need to stat the directories
 * on every lookup.
 *)
let stat_dir cache dir =
   let name = Omake_node.Dir.fullname dir in
      try
         let stat = Unix.LargeFile.stat name in
            cache.cache_file_stat_count <- succ cache.cache_file_stat_count;
            Some stat
      with
         Unix.Unix_error _ ->
            None

let stat_dirs cache dirs =
   List.map (stat_dir cache) dirs

let stats_equal_opt stat1 stat2 =
   match stat1, stat2 with
      Some stat1, Some stat2 ->
         stats_equal stat1 stat2
    | None, None ->
         true
    | None, Some _
    | Some _, None ->
         false

let rec stats_equal_opt_list stats1 stats2 =
   match stats1, stats2 with
      stat1 :: stats1, stat2 :: stats2 ->
         stats_equal_opt stat1 stat2 && stats_equal_opt_list stats1 stats2
    | [], [] ->
         true
    | _ :: _, []
    | [], _ :: _ ->
         false

let check_stat auto_rehash (stat_old, entries) stat_new =
   if auto_rehash && not (stats_equal_opt stat_old (Lazy.force stat_new)) then
      raise Not_found
   else
      entries

let check_stats auto_rehash (stats_old, entries) stats_new =
   if auto_rehash && not (stats_equal_opt_list stats_old (Lazy.force stats_new)) then
      raise Not_found
   else
      entries

(*
 * List a directory.
 *)
let  list_directory _cache dir =
   let dirx =
      try Unix.opendir (Omake_node.Dir.fullname dir) with
         Unix.Unix_error _ ->
            raise Not_found
   in
   let rec list entries =
      let name =
         try Some (Unix.readdir dirx) with
            Unix.Unix_error _
          | End_of_file ->
               None
      in
         match name with
            Some "."
          | Some ".." ->
               list entries
          | Some name ->
               let entry = ref (LazyEntryCore (dir, name)) in
               let entries = Lm_string_set.StringTable.add entries name entry in
                  list entries
          | None ->
               entries
   in
   let entries = list Lm_string_set.StringTable.empty in
      Unix.closedir dirx;
      entries

(*
 * Get the directory listing as a Lm_string_set.StringTable.
 *)
let ls_dir cache auto_rehash dir =
   let stat = lazy (stat_dir cache dir) in
      try check_stat auto_rehash (Omake_node.DirTable.find cache.cache_dirs dir) stat with
         Not_found ->
            let entries = list_directory cache dir in
            let stat = Lazy.force stat in
               cache.cache_dirs <- Omake_node.DirTable.add cache.cache_dirs dir (stat, entries);
               entries

(*
 * Path version.
 *)
let ls_path cache auto_rehash dirs =
   let key = Omake_node.DirListHash.create dirs in
   let stats = lazy (stat_dirs cache dirs) in
      try check_stats auto_rehash (Omake_node.DirListTable.find cache.cache_path key) stats with
         Not_found ->
            (* Fold together the tables *)
            let stats = Lazy.force stats in
            let entries =
               List.fold_left (fun entries1 dir ->
                     try
                        let entries2 = ls_dir cache auto_rehash dir in
                           Lm_string_set.StringTable.fold Lm_string_set.StringTable.add entries1 entries2
                     with
                        Not_found ->
                           entries1) Lm_string_set.StringTable.empty (List.rev dirs)
            in
               cache.cache_path <- Omake_node.DirListTable.add cache.cache_path key (stats, entries);
               entries

(*
 * Resolve an entry in the listing.
 *)
let listing_find_item cache listing s =
  let entry_ref = Lm_string_set.StringTable.find listing s in
  match !entry_ref with
    DirEntryCore entry ->
    entry
  | LazyEntryCore (dir, s) ->
    let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
    let entry =
      if is_dir cache node then
        Omake_cache_type.DirEntry (Omake_node.Dir.chdir dir s)
      else
        NodeEntry node
    in
    entry_ref := DirEntryCore entry;
    entry

(*
 * The execution path is a little harder, and it is quite different
 * on Win32 and Unix.
 *
 * On Win32:
 *    - File permission doesn't matter
 *    - Files without suffix, and with .com, .exe, .bat and .cmd suffixes are executable
 *
 * On Cygwin:
 *    - Files without suffix must be executable
 *    - Files with .com, .exe, .bat and .cmd suffixes are executable
 *
 * On Unix:
 *    - There is no .exe suffix
 *    - Only files that are executable count
 *)
let win32_suffixes = [".com"; ".exe"; ".bat"; ".cmd"]

let ls_exe_path_win32 cache auto_rehash dirs =
   let key = Omake_node.DirListHash.create dirs in
   let stats = lazy (stat_dirs cache dirs) in
      try check_stats auto_rehash (Omake_node.DirListTable.find cache.cache_exe_path key) stats with
         Not_found ->
            let entries =
               List.fold_left (fun entries1 dir ->
                     try
                        let entries2 = ls_dir cache auto_rehash dir in
                           Lm_string_set.StringTable.fold (fun entries name _ ->
                                 let info = dir, name in
                                 let name = String.lowercase_ascii name in
                                 let entries = Lm_string_set.StringMTable.add entries name info in
                                    if List.exists (Filename.check_suffix name) win32_suffixes then
                                       let name = Filename.chop_extension name in
                                          Lm_string_set.StringMTable.add entries name info
                                    else
                                       entries) entries1 entries2
                     with
                        Not_found ->
                           entries1) Lm_string_set.StringMTable.empty (List.rev dirs)
            in
            let entries =
               Lm_string_set.StringMTable.fold_all (fun entries name info ->
                     Lm_string_set.StringTable.add entries name (ref (ExeEntryCore info))) 
                Lm_string_set.StringTable.empty entries
            in
            let stats = Lazy.force stats in
               cache.cache_exe_path <- Omake_node.DirListTable.add cache.cache_exe_path key (stats, entries);
               entries

let ls_exe_path_unix cache auto_rehash dirs =
  let key = Omake_node.DirListHash.create dirs in
  let stats = lazy (stat_dirs cache dirs) in
  try check_stats auto_rehash (Omake_node.DirListTable.find cache.cache_exe_path key) stats with
    Not_found ->
    let entries =
      List.fold_left (fun entries1 dir ->
        try
          let entries2 = ls_dir cache auto_rehash dir in
          Lm_string_set.StringTable.fold (fun entries name _ ->
            Lm_string_set.StringMTable.add entries name (dir, name)) entries1 entries2
        with
          Not_found ->
          entries1) Lm_string_set.StringMTable.empty (List.rev dirs)
    in
    let entries =
      Lm_string_set.StringMTable.fold_all (fun entries name info ->
        Lm_string_set.StringTable.add entries name (ref (ExeEntryCore info))) 
        Lm_string_set.StringTable.empty entries
    in
    let stats = Lazy.force stats in
    cache.cache_exe_path <- Omake_node.DirListTable.add cache.cache_exe_path key (stats, entries);
    entries

(*
 * Find the first entry that is executable.
 *)
let is_exe_file cache node =
   try
      let { Unix.LargeFile.st_kind = kind;
            Unix.LargeFile.st_perm = perm;
            Unix.LargeFile.st_uid = uid;
            Unix.LargeFile.st_gid = gid;
            _
          } = stat_unix cache node
      in
         (kind = Unix.S_REG)
         && ((perm land 0o001) <> 0
             || (List.mem gid groups && (perm land 0o010) <> 0)
             || (uid = euid && (perm land 0o100) <> 0))
   with
      Unix.Unix_error _
    | Not_found ->
         false

let is_exe_win32 cache dir s =
  let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
  if is_dir cache node then
    None
  else
    Some node

let is_exe_unix cache dir s =
  let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
  if is_exe_file cache node then
    Some node
  else
    None

let is_exe_cygwin cache dir s =
  let node = Omake_node.Node.create_node Omake_node.no_mount_info Omake_node.Mount.empty dir s in
  if List.exists (Filename.check_suffix s) win32_suffixes || is_exe_file cache node then
    Some node
  else
    None

let ls_exe_path, is_exe, name_exe, exe_suffixes =
   if Sys.os_type = "Win32" then
      ls_exe_path_win32, is_exe_win32, String.lowercase_ascii, "" :: win32_suffixes
   else if Sys.os_type = "Cygwin" then
      ls_exe_path_win32, is_exe_cygwin, String.lowercase_ascii, "" :: win32_suffixes
   else
      ls_exe_path_unix, is_exe_unix, (fun s -> s), [""]

let  search_exe cache entries =
   Lm_list_util.some_map (fun (dir, s) ->
         is_exe cache dir s) entries

let exe_find_nodes cache listing s =
   let entry_ref = Lm_string_set.StringTable.find listing (name_exe s) in
      match !entry_ref with
         ExeEntryCore entries ->
            let nodes = search_exe cache entries in
               entry_ref := ExeEntryNodes nodes;
               nodes
       | ExeEntryNodes nodes ->
            nodes

let exe_find_nodes_all cache listing s =
   try exe_find_nodes cache listing s with
      Not_found ->
         []

let exe_find_item cache listing s =
   match exe_find_nodes cache listing s with
      node :: _ ->
         node
    | [] ->
         raise Not_found

(*
 * Find all entries with the given prefix.
 *)
let exe_complete_prefix cache s items listing =
   Lm_string_set.StringTable.fold (fun items s2 entry_ref ->
         if Lm_string_util.is_string_prefix s s2 then
            let nodes =
               match !entry_ref with
                  ExeEntryCore entries ->
                     let nodes = search_exe cache entries in
                        entry_ref := ExeEntryNodes nodes;
                        nodes
                | ExeEntryNodes nodes ->
                     nodes
            in
               if nodes = [] then
                  items
               else
                  Lm_string_set.StringSet.add items s2
         else
            items) items listing

(************************************************************************
 * Redefine the functions to work on directory groups, where each group may
 * specify auto-rehashing.
 *)
let rec listing_find cache listings s =
   match listings with
      [listing] ->
         listing_find_item cache listing s
    | listing :: listings ->
         (try listing_find_item cache listing s with
             Not_found ->
                listing_find cache listings s)
    | [] ->
         raise Not_found

let rec exe_find cache listings s =
   match listings with
      [listing] ->
         exe_find_item cache listing s
    | listing :: listings ->
         (try exe_find_item cache listing s with
             Not_found ->
                exe_find cache listings s)
    | [] ->
         raise Not_found

let exe_find_all cache listings s =
   List.flatten (List.map (fun listing -> exe_find_nodes_all cache listing s) listings)

let exe_complete cache listings s =
   List.fold_left (exe_complete_prefix cache s) Lm_string_set.StringSet.empty listings

let ls_dir cache auto_rehash dir =
   [ls_dir cache auto_rehash dir]

let ls_path cache groups =
   List.map (fun (auto_rehash, dirs) ->
         ls_path cache auto_rehash dirs) groups

let ls_exe_path cache groups =
   List.map (fun (auto_rehash, dirs) ->
         ls_exe_path cache auto_rehash dirs) groups
