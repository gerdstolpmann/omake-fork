(*
 * Symbols used everywhere.
 * Eventually, we should collect all the global symbols and
 * put them here.
 *)


let braces_sym                 = Lm_symbol.add "{}"
let builtin_sym                = Lm_symbol.add "$builtin"
let map_sym                    = Lm_symbol.add "$map"
let pervasives_sym             = Lm_symbol.add "Pervasives"
let object_sym                 = Lm_symbol.add "Object"
let int_object_sym             = Lm_symbol.add "Int"
let float_object_sym           = Lm_symbol.add "Float"
let string_object_sym          = Lm_symbol.add "String"
let sequence_object_sym        = Lm_symbol.add "Sequence"
let array_object_sym           = Lm_symbol.add "Array"
let fun_object_sym             = Lm_symbol.add "Fun"
let rule_object_sym            = Lm_symbol.add "Rule"
let file_object_sym            = Lm_symbol.add "File"
let dir_object_sym             = Lm_symbol.add "Dir"
let body_object_sym            = Lm_symbol.add "Body"
let in_channel_object_sym      = Lm_symbol.add "InChannel"
let out_channel_object_sym     = Lm_symbol.add "OutChannel"
let in_out_channel_object_sym  = Lm_symbol.add "InOutChannel"
let map_object_sym             = Lm_symbol.add "Map"
let shell_object_sym           = Lm_symbol.add "Shell"
let select_object_sym          = Lm_symbol.add "Select"
let pipe_object_sym            = Lm_symbol.add "Pipe"
let stat_object_sym            = Lm_symbol.add "Stat"
let passwd_object_sym          = Lm_symbol.add "Passwd"
let group_object_sym           = Lm_symbol.add "Group"
let lexer_object_sym           = Lm_symbol.add "Lexer"
let parser_object_sym          = Lm_symbol.add "Parser"
let location_object_sym        = Lm_symbol.add "Location"
let target_object_sym          = Lm_symbol.add "Target"
let options_object_sym         = Lm_symbol.add "Options"
let var_object_sym             = Lm_symbol.add "Var"
let tm_object_sym              = Lm_symbol.add "Tm"

let wild_sym                   = Lm_symbol.add "%"
let explicit_target_sym        = Lm_symbol.add "$EXPLICIT-TARGET"

let current_prec_sym           = Lm_symbol.add "current-prec"

let lex_sym                    = Lm_symbol.add "lex"
let name_sym                   = Lm_symbol.add "name"
(* let value_sym                  = Lm_symbol.add "value" *)
let lexer_sym                  = Lm_symbol.add "lexer"
let val_sym                    = Lm_symbol.add "val"

let read_sym                   = Lm_symbol.add "read"
let write_sym                  = Lm_symbol.add "write"
let error_sym                  = Lm_symbol.add "error"

let st_dev_sym                 = Lm_symbol.add "st_dev"
let st_ino_sym                 = Lm_symbol.add "st_ino"
let st_kind_sym                = Lm_symbol.add "st_kind"
let st_perm_sym                = Lm_symbol.add "st_perm"
let st_nlink_sym               = Lm_symbol.add "st_nlink"
let st_uid_sym                 = Lm_symbol.add "st_uid"
let st_gid_sym                 = Lm_symbol.add "st_gid"
let st_rdev_sym                = Lm_symbol.add "st_rdev"
let st_size_sym                = Lm_symbol.add "st_size"
let st_atime_sym               = Lm_symbol.add "st_atime"
let st_mtime_sym               = Lm_symbol.add "st_mtime"
let st_ctime_sym               = Lm_symbol.add "st_ctime"

let pw_name_sym                = Lm_symbol.add "pw_name"
let pw_passwd_sym              = Lm_symbol.add "pw_passwd"
let pw_uid_sym                 = Lm_symbol.add "pw_uid"
let pw_gid_sym                 = Lm_symbol.add "pw_gid"
let pw_gecos_sym               = Lm_symbol.add "pw_gecos"
let pw_dir_sym                 = Lm_symbol.add "pw_dir"
let pw_shell_sym               = Lm_symbol.add "pw_shell"

let gr_name_sym                = Lm_symbol.add "gr_name"
let gr_passwd_sym              = Lm_symbol.add "gr_passwd"
let gr_gid_sym                 = Lm_symbol.add "gr_gid"
let gr_mem_sym                 = Lm_symbol.add "gr_mem"

let tm_sec_sym                 = Lm_symbol.add "tm_sec"
let tm_min_sym                 = Lm_symbol.add "tm_min"
let tm_hour_sym                = Lm_symbol.add "tm_hour"
let tm_mday_sym                = Lm_symbol.add "tm_mday"
let tm_mon_sym                 = Lm_symbol.add "tm_mon"
let tm_year_sym                = Lm_symbol.add "tm_year"
let tm_wday_sym                = Lm_symbol.add "tm_wday"
let tm_yday_sym                = Lm_symbol.add "tm_yday"
let tm_isdst_sym               = Lm_symbol.add "tm_isdst"
let tm_time_sym                = Lm_symbol.add "tm_time"
let tm_format_sym              = Lm_symbol.add "tm_format"

let target_sym                 = Lm_symbol.add "target"
let target_effects_sym         = Lm_symbol.add "effects"
let scanner_deps_sym           = Lm_symbol.add "scanner-deps"
let static_deps_sym            = Lm_symbol.add "static-deps"
let build_deps_sym             = Lm_symbol.add "build-deps"
let build_values_sym           = Lm_symbol.add "build-values"
let build_commands_sym         = Lm_symbol.add "build-commands"
let output_file_sym            = Lm_symbol.add "output-file"

let argv_sym                   = Lm_symbol.add "argv"
let star_sym                   = Lm_symbol.add "*"
let at_sym                     = Lm_symbol.add "@"
let amp_sym                    = Lm_symbol.add "&"
let lt_sym                     = Lm_symbol.add "<"
let gt_sym                     = Lm_symbol.add ">"
let plus_sym                   = Lm_symbol.add "+"
let hat_sym                    = Lm_symbol.add "^"
let zero_sym                   = Lm_symbol.add "0"

let runtime_exception_sym      = Lm_symbol.add "RuntimeException"
let unbuildable_exception_sym  = Lm_symbol.add "UnbuildableException"
let parse_loc_sym              = Lm_symbol.add "parse-loc"
let loc_sym                    = Lm_symbol.add "loc"
let pos_sym                    = Lm_symbol.add "position"
let message_sym                = Lm_symbol.add "message"

let stdin_sym                  = Lm_symbol.add "stdin"
let stdout_sym                 = Lm_symbol.add "stdout"
let stderr_sym                 = Lm_symbol.add "stderr"
let printexitvalue_sym         = Lm_symbol.add "printexitvalue"

let targets_sym                = Lm_symbol.add "TARGETS"

let glob_options_sym           = Lm_symbol.add "GLOB_OPTIONS"
let glob_allow_sym             = Lm_symbol.add "GLOB_ALLOW"
let glob_ignore_sym            = Lm_symbol.add "GLOB_IGNORE"

let this_sym                   = Lm_symbol.add "this"
let dynamic_sym                = Lm_symbol.add "dynamic"
let static_sym                 = Lm_symbol.add "static"

let allow_empty_subdirs_sym    = Lm_symbol.add "ALLOW_EMPTY_SUBDIRS"

let abort_on_command_error_sym     = Lm_symbol.add "ABORT_ON_COMMAND_ERROR"
let exit_on_uncaught_exception_sym = Lm_symbol.add "EXIT_ON_UNCAUGHT_EXCEPTION"
let create_subdirs_sym             = Lm_symbol.add "CREATE_SUBDIRS"

let scanner_mode_sym               = Lm_symbol.add "SCANNER_MODE"

let history_file_sym               = Lm_symbol.add "history-file"
let history_length_sym             = Lm_symbol.add "history-length"

let build_summary_sym              = Lm_symbol.add "BUILD_SUMMARY"

(*
 * Special symbols.
 *)
let concat_sym    = Lm_symbol.add "concat"

let if_sym        = Lm_symbol.add "if"
let else_sym      = Lm_symbol.add "else"
let elseif_sym    = Lm_symbol.add "elseif"
let switch_sym    = Lm_symbol.add "switch"
let select_sym    = Lm_symbol.add "select"
let case_sym      = Lm_symbol.add "case"
let do_sym        = Lm_symbol.add "do"
let while_sym     = Lm_symbol.add "while"
let default_sym   = Lm_symbol.add "default"
let include_sym   = Lm_symbol.add "include"
let section_sym   = Lm_symbol.add "section"
let try_sym       = Lm_symbol.add "try"
let catch_sym     = Lm_symbol.add "catch"
let when_sym      = Lm_symbol.add "when"
let finally_sym   = Lm_symbol.add "finally"
let curry_sym     = Lm_symbol.add "curry"
let private_sym   = Lm_symbol.add "private"
let protected_sym = Lm_symbol.add "protected"
let public_sym    = Lm_symbol.add "public"
let global_sym    = Lm_symbol.add "global"
let const_sym     = Lm_symbol.add "const"
let rule_sym      = Lm_symbol.add "rule"
let system_sym    = Lm_symbol.add "system"
let open_sym      = Lm_symbol.add "open"
let autoload_sym  = Lm_symbol.add "autoload"
let declare_sym   = Lm_symbol.add "declare"
let return_sym    = Lm_symbol.add "return"
let export_sym    = Lm_symbol.add "export"
let value_sym     = Lm_symbol.add "value"
let file_sym      = Lm_symbol.add "__FILE__"
let file_id_sym   = Lm_symbol.add "__ID__"
let foreach_sym   = Lm_symbol.add "foreach"
let fun_sym       = Lm_symbol.add "fun"
let set_sym       = Lm_symbol.add "set"

let neg_fun_sym       = Lm_symbol.add "neg"
let add_fun_sym       = Lm_symbol.add "add"
let sub_fun_sym       = Lm_symbol.add "sub"
let mul_fun_sym       = Lm_symbol.add "mul"
let div_fun_sym       = Lm_symbol.add "div"
let mod_fun_sym       = Lm_symbol.add "mod"
let lsl_fun_sym       = Lm_symbol.add "lsl"
let lsr_fun_sym       = Lm_symbol.add "lsr"
let asr_fun_sym       = Lm_symbol.add "asr"
let lxor_fun_sym      = Lm_symbol.add "lxor"
let lor_fun_sym       = Lm_symbol.add "lor"
let land_fun_sym      = Lm_symbol.add "land"
let and_fun_sym       = Lm_symbol.add "and"
let or_fun_sym        = Lm_symbol.add "or"
let le_fun_sym        = Lm_symbol.add "le"
let lt_fun_sym        = Lm_symbol.add "lt"
let equal_fun_sym     = Lm_symbol.add "equal"
let nequal_fun_sym    = Lm_symbol.add "nequal"
let ge_fun_sym        = Lm_symbol.add "ge"
let gt_fun_sym        = Lm_symbol.add "gt"
let nth_fun_sym       = Lm_symbol.add "nth"

let memo_rule_sym       = Lm_symbol.add "memo-rule"

let empty_map_sym       = Lm_symbol.add "empty-map"
let create_map_sym      = Lm_symbol.add "create-map"
let create_lazy_map_sym = Lm_symbol.add "create-lazy-map"

(*
 * Awk values.
 *)
let awk_sym       = Lm_symbol.add "awk"
let nf_sym        = Lm_symbol.add "NF"
let rs_sym        = Lm_symbol.add "RS"
let fs_sym        = Lm_symbol.add "FS"

let filename_sym  = Lm_symbol.add "FILENAME"
let fnr_sym       = Lm_symbol.add "FNR"

let fsubst_sym    = Lm_symbol.add "fsubst"

(*
 * The applications that can have cases.
 *)
let cases_syms    = [awk_sym; fsubst_sym]
let cases_set     = List.fold_left Lm_symbol.SymbolSet.add Lm_symbol.SymbolSet.empty cases_syms

let clauses_syms  = [case_sym; default_sym; when_sym; catch_sym; finally_sym; do_sym]
let clauses_set   = List.fold_left Lm_symbol.SymbolSet.add Lm_symbol.SymbolSet.empty clauses_syms

(*
 * Colon symbols.
 *)
let normal_sym   = Lm_symbol.add ":normal:"
let optional_sym = Lm_symbol.add ":optional:"
let exists_sym   = Lm_symbol.add ":exists:"
let squash_sym   = Lm_symbol.add ":squash:"
let effects_sym  = Lm_symbol.add ":effects:"
let scanner_sym  = Lm_symbol.add ":scanner:"
let values_sym   = Lm_symbol.add ":value:"
let key_sym      = Lm_symbol.add ":key:"

(*
 * Builtin functions.
 *)
let extends_sym    = Lm_symbol.add "extends"
let omakeflags_sym = Lm_symbol.add "OMakeFlags"
let omakeargv_sym  = Lm_symbol.add "OMakeArgv"

(*
 * Symbols.
 *)
let prompt_sym      = Lm_symbol.add "prompt"
let ignoreeof_sym   = Lm_symbol.add "ignoreeof"

let cwd_sym         = Lm_symbol.add "CWD"
let stdroot_sym     = Lm_symbol.add "STDROOT"
let stdlib_sym      = Lm_symbol.add "STDLIB"
let ostype_sym      = Lm_symbol.add "OSTYPE"
let path_sym        = Lm_symbol.add "PATH"
let auto_rehash_sym = Lm_symbol.add "AUTO_REHASH"

let omakepath_sym = Lm_symbol.add "OMAKEPATH"

let oshell_sym    = Lm_symbol.add "OSHELL"

let cdpath_sym    = Lm_symbol.add "cdpath"
