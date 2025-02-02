(* Experimental builtins for accelerating ocamldep postprocessing *)

include Omake_pos.Make (struct let name = "Omake_builtin_ocamldep" end)
open Printf
open Omake_ir

(*
 * \begin{doc}
 * \fun{ocamldep-print-buildable-deps}
 *
 * \begin{verbatim}
 *    $(ocamldep-postproc path, enable_cmx)
 *        path : Dir array
 *        enable_cmx : Bool
 * \end{verbatim}
 *
 * Postprocesses an ocamldep invocation with -modules switch.
 *
 * Accesses the global \verb+EXT_OBJ+ for getting the ".o" suffix.
 * \end{doc}
 *)

let is_instanceof_unbuildable_exception obj =
  Omake_env.venv_instanceof obj Omake_symbol.unbuildable_exception_sym


let target_is_buildable cache venv pos node =
  try
    Omake_target.target_is_buildable cache venv pos node
  with Omake_value_type.RaiseException (_, obj) when is_instanceof_unbuildable_exception obj ->
    false

let ocamldep_postproc venv pos loc args =
  let cache = Omake_env.venv_cache venv in

  let search_in_path path name =
    try
      let name1 = String.uncapitalize_ascii name
      and name2 = String.capitalize_ascii name in
      let names = (* check name with uncapitalized case first *) [name1; name2] in
      match Omake_target.target_is_buildable_in_path_1 cache venv pos path names with
        | Some node ->
            Some (Omake_env.venv_nodename venv node)
        | None ->
            None
    with
      | Omake_value_type.RaiseException (_, obj) when is_instanceof_unbuildable_exception obj ->
          None in

  let accumulate_over_path path deps suffix =
    List.rev (List.fold_left
                (fun acc dep ->
                  match search_in_path path (dep ^ suffix) with
                  | Some node -> node :: acc
                  | None -> acc)
                []
                deps) in

  let pos = string_pos "ocamldep-print-buildable-deps" pos in

  let stdin_var = Omake_env.venv_find_var venv pos loc Omake_var.stdin_var in
  let stdin_prim, stdin_close_flag = Omake_value.in_channel_of_any_value venv pos stdin_var in
  let stdin_fd = Omake_env.venv_find_channel venv pos stdin_prim in

  let stdout_var = Omake_env.venv_find_var venv pos loc Omake_var.stdout_var in
  let stdout_prim, stdout_close_flag = Omake_value.out_channel_of_any_value venv pos stdout_var in
  let stdout_fd = Omake_env.venv_find_channel venv pos stdout_prim in

  let ext_obj_val = Omake_builtin_util.get_sym venv pos loc "EXT_OBJ" in
  let ext_obj = Omake_value.string_of_value venv pos ext_obj_val in

  let mount_table =
    let cwd = Omake_env.venv_dir venv in
      List.sort
        (fun (dest_a, _src_a) (dest_b, _src_b) ->
          let delta_length = String.length dest_b - String.length dest_a in
            if delta_length = 0 then String.compare dest_a dest_b else delta_length)
        (List.map
           (fun (dest_dir, src_dir) -> Omake_node.Dir.name cwd dest_dir, Omake_node.Dir.name cwd src_dir)
           (Omake_env.venv_get_mount_listing venv)) in

  let fix_vmounted_path path =
      List.fold_left
        (fun patched_path (dest, src) -> Lm_string_util.substitute_prefix src dest patched_path)
        path
        mount_table in

  let output_dep path target deps enable_cmx =
    let cmideps = accumulate_over_path path deps ".cmi" in
    let cmideps_str = String.concat " " (List.map String.escaped cmideps) in
    if Filename.check_suffix target ".mli" then
      begin
        if cmideps <> [] then
          let targetbase = fix_vmounted_path (Filename.chop_suffix target ".mli") in
            Lm_channel.output_string
              stdout_fd
              (sprintf
                 "%s.cmi: %s\n"
                 (String.escaped targetbase)
                 cmideps_str)
      end
    else
      if Filename.check_suffix target ".ml" then
        begin
          let targetbase = fix_vmounted_path (Filename.chop_suffix target ".ml") in
          let targetbase_esc = String.escaped targetbase in
          let cmxdeps =
            if enable_cmx then
              accumulate_over_path path deps ".cmx"
            else
              [] in
          let alldeps = cmideps @ cmxdeps in
          let alldeps_str = String.concat " " (List.map String.escaped alldeps) in
            if cmideps <> [] || cmxdeps <> [] then
              begin
                if cmideps <> [] then
                  Lm_channel.output_string
                    stdout_fd
                    (sprintf
                       "%s.cmo: %s\n"
                       targetbase_esc
                       cmideps_str);
                Lm_channel.output_string
                  stdout_fd
                  (sprintf
                     "%s.cmx %s%s: %s\n"
                     targetbase_esc
                     targetbase_esc
                     ext_obj
                     alldeps_str)
              end
        end
      else
        raise (Omake_value_type.OmakeException (loc_pos loc pos, StringError "illegal filename suffix")) in

  match args with
    | [path; enable_cmx] ->
         let path = Omake_value.values_of_value venv pos path in
         let path = Omake_eval.path_of_values venv pos path "." in
         let path = List.flatten (List.map snd path) in
         let path = List.map (fun dir -> dir, Omake_env.venv_lookup_target_dir venv dir) path in
         let enable_cmx = Omake_value.bool_of_value venv pos enable_cmx in

         let current = ref None in
         let process_current() =
           match !current with
             | None -> ()
             | Some (target, deps) ->
                  current := None;
                  output_dep path target deps enable_cmx in
           begin
             try
             while true do
               let line = Lm_channel.input_line stdin_fd in
                 if Lm_string_util.contains line ':' then
                   begin
                 process_current ();
                 let k = Lm_string_util.strchr line ':' in
                 let target = String.sub line 0 k in
                 let rest = String.sub line (k + 1) (String.length line - k - 1) in
                 let words = Lm_string_util.tokens "" Lm_string_util.white rest in
                 current := Some (target, words)
               end
                 else
                   begin
                 match !current with
                   | None -> ()
                   | Some (target, words) ->
                        let more_words = Lm_string_util.tokens "" Lm_string_util.white line in
                        current := Some (target, words @ more_words)
               end
             done;
             assert false
           with
             | End_of_file -> ()
         end;
         process_current ();

         if stdin_close_flag then
           Omake_env.venv_close_channel venv pos stdin_prim;
         if stdout_close_flag then
           Omake_env.venv_close_channel venv pos stdout_prim;
         Omake_value_type.ValNone

    | _ ->
         raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, List.length args)))



(* register *)

let () =
  let builtin_funs =
    [true, "ocamldep-postproc", ocamldep_postproc, ArityExact 2] in
  let builtin_info =
    { Omake_builtin_type.builtin_empty with
      builtin_funs = builtin_funs;
    }
  in
  Omake_builtin.register_builtin builtin_info
