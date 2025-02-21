(*
 * Generate magic numbers.
 *)


type mode =
  | CacheFiles
  | OmcFiles
  | OmoFiles
  | NoFiles

(* Get the version from the version.txt file. *)
let read_version_from_file inx =
   let line = Lm_string_util.trim (input_line inx) in
   if line = "" then raise End_of_file;
   line





let read_version version_txt : string =
  let file = version_txt in
  let inx = open_in file in
  let version =
    try read_version_from_file inx with
      End_of_file ->
      Printf.eprintf "The %s file does not contain a version number\n" file;
      flush stderr;
      exit 1
  in
  close_in inx;
  if Lm_string_util.contains_any version "()$\" " then
    version
  else if String.contains version '-' then
    let dash = String.index version '-' in
    let release = String.sub version (dash + 1) ((String.length version) - dash - 1) in
    if String.length release > 4 && String.sub release 0 4 = "0.rc" then
      let release = String.sub release 4 ((String.length release) - 4) in
      Printf.sprintf "%s (release candidate %s)" (String.sub version 0 dash) release
    else
      Printf.sprintf "%s (release %s)" (String.sub version 0 dash) release
  else
    version ^ " (development snapshot)"


let digest_len = 4 + String.length (Digest.string "hello world")

let shorten_version s =
   if Lm_string_util.contains_any s " -" then
      String.sub s 0 (Lm_string_util.index_set s " -")
   else
      s




(* Figure out if the line is a magic directive. *)
(*
{[
magic_line_type "(\* %%MAGICEND%% *\)";;
`END
magic_line_type "(* %%MAGICBEGIN%% *)";;
`Begin
]}
*)
let magic_line_type line  =
  let gbuf = Buffer.create 200  in
  let len = String.length line in
  let rec loop mode i =
    if i <> len then
      let c = line.[i] in
      if c = '%' then
        match mode with
        | `Idle ->
          loop `Start (i+1)
        | `Start ->
          loop `Scanning (i+1)
        | `Scanning -> ()
      else begin
        if mode = `Scanning then
          Buffer.add_char gbuf c;
        loop mode (i+1)
      end
  in
  let () = loop `Idle 0 in
  let code = Buffer.contents gbuf in
  Buffer.clear gbuf;
  begin match code with
    | "MAGICBEGIN" -> `Begin
    | "MAGICEND" -> `End
    | _ -> `None
  end

let copy_magic_file outx inp : unit =
  let rec copy inx magic_flag =
    let line = input_line inx in
    match magic_line_type line with
    | `Begin ->
      copy inx true
    | `End ->
      copy inx false
    | `None ->
      if magic_flag then
        begin
          output_string outx line;
          output_char outx '\n'
        end;
      copy inx magic_flag in
  match open_in inp with
  | exception  Sys_error _ ->
    Printf.eprintf "Can't open %s\n" inp;
    flush stderr;
    exit 1
  | inx ->
    try copy inx false with End_of_file -> close_in inx


let omake_magic (buf : out_channel)
    version_txt
    default_save_interval
    libdir
    cache_files
    omc_files
    omo_files
    vars
  : unit =

  let digest_files filename code filenames =
    match open_out_bin filename with
    | exception Sys_error _ ->
      Printf.eprintf "Can't open temporary_file %s\n" filename;
      flush stderr;
      exit 2
    | outx ->
      let digest =
        List.iter (copy_magic_file outx) filenames;
        close_out outx;
        Digest.file filename
      in
      String.escaped (code ^ digest) in
  let version = read_version version_txt in
  let tm =
  try
    Unix.(localtime (float_of_string (Sys.getenv "SOURCE_DATE_EPOCH")))
  with
    Not_found -> Unix.(localtime (Unix.time ())) in
  Printf.fprintf buf {|
let default_save_interval = %F
let input_magic inx = let s = Bytes.make %d ' ' in really_input inx s 0 %d; Bytes.to_string s
let output_magic = output_string
let cache_magic = "%s"
let ir_magic = "%s"
let obj_magic = "%s"
let lib_dir = "%s"
let version = "%s"
let version_message = "OMake %s:\n    build [%s %s %d %02d:%02d:%02d %d]\n    on %s"
|}
       default_save_interval
       digest_len
       digest_len
       (digest_files ".cache.magic" ".odb" cache_files)
       (digest_files ".omc.magic" ".omc" omc_files)
       (digest_files ".omo.magic" ".omo" omo_files)
       (String.escaped libdir)
       (String.escaped (shorten_version version))
       (String.escaped version)
       [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|].(tm.tm_wday)
       [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|].(tm.tm_mon)
       tm.tm_mday
       tm.tm_hour
       tm.tm_min
       tm.tm_sec
       (tm.tm_year + 1900)
       (String.escaped (Unix.gethostname ()));
  List.iter
    (fun (name,value) ->
       Printf.fprintf buf "let %s = %S\n" name value
    )
    vars;
  flush buf

let omake_root buf version_txt name =
  (* Copy a file from input to output. *)
  let copy_file buf inx =
    let rec copy () =
      let line = input_line inx in
      output_string buf line;
      output_char buf '\n';
      copy () in
    try copy () with End_of_file -> () in

   let version = shorten_version (read_version version_txt) in
   let version =
      if Lm_string_util.contains_any version "()$\" " then
         Printf.sprintf "$'''%s'''" version
      else
         version
   in
   let inx = open_in name in
   Printf.fprintf buf "#\n# Required version of omake\n#\nOMakeVersion(%s, %s)\n\n" version version;
   copy_file buf inx;
   Printf.fprintf buf "\n";
   close_out buf;
   close_in inx






let libdir      = ref None
let version_txt = ref "version.txt"
let mode        = ref NoFiles
let make_magic  = ref false
let make_root   = ref None
let output_file = ref None
let cache_files = ref []
let omc_files   = ref []
let omo_files   = ref []
let default_save_interval = ref 15.0
let vars        = ref []



let anon s =
  let p =
    match !mode with
    | CacheFiles -> cache_files
    | OmcFiles -> omc_files
    | OmoFiles -> omo_files
    | NoFiles ->
      Printf.eprintf "You specified an anonymous file.  Use --help for help.";
      exit 3 in
  p := s :: !p

let spec : (string * Arg.spec * string) list =
   ["--magic", Set make_magic, "generate the omake_magic.ml file";

    "--cache-files", Unit (fun () -> mode := CacheFiles), "specify the magic files for the cache magic number";
    "--omc-files", Unit (fun () -> mode := OmcFiles), "specify the files to scan for the IR magic number";
    "--omo-files", Unit (fun () -> mode := OmoFiles), "specify the files to scan for the object magic number";
    "--version", String (fun s -> version_txt := s), "specify the version.txt file";

    "-o",String (fun s -> output_file := Some s), "set the output file";
    "--lib", String (fun s -> libdir := Some s), "specify the location of the library directory";
    "--root", String (fun s -> make_root := Some s),  "generate the OMakeroot file";
    "--default_save_interval",
    Float (fun f -> default_save_interval := f), "specify the default .omakedb save interval";
    "--var", String (fun s ->
                     let (name,value) =
                       try
                         let p = String.index s '=' in
                         (String.sub s 0 p,
                          String.sub s (p+1) (String.length s - p - 1)
                         )
                       with Not_found -> failwith ("bad --var: " ^ s) in
                     vars := (name,value) :: !vars
                    ), "another variable, format name=value";
   ]

let usage = "Generate special files"


(************************************************************************
 * Main function.
 *)
let main () =
  Arg.parse spec anon usage;
  let buf =
    match !output_file with
    | Some name ->
      open_out_bin name
    | None ->
      raise (Invalid_argument "use the -o option to specify an output file")
  in
  let libdir =
    match !libdir with
    | Some s -> Filename.concat s "omake"
    | None -> Filename.concat (Filename.dirname (Unix.getcwd ())) "lib"
  in

  if !make_magic then
    omake_magic buf !version_txt !default_save_interval libdir !cache_files !omc_files !omo_files !vars
  else
    match !make_root with
    | Some name ->
      omake_root  buf !version_txt name
    | None ->
      Arg.usage spec usage;
      exit 2

let _ =
   main ()
