include Omake_pos.Make (struct let name = "Omake_builtin_sys" end);;


(************************************************************************
 * Passwd database access.
 *)

(*
 * \begin{doc}
 * \obj{Passwd}
 *
 * The \verb+Passwd+ object represents an entry in the system's user database.
 * It contains the following fields.
 *
 * \begin{description}
 * \itemidx{pw\_name}: the login name.
 * \itemidx{pw\_passwd}: the encrypted password.
 * \itemidx{pw\_uid}: user id of the user.
 * \itemidx{pw\_gid}: group id of the user.
 * \itemidx{pw\_gecos}: the user name or comment field.
 * \itemidx{pw\_dir}: the user's home directory.
 * \itemidx{pw\_shell}: the user's default shell.
 * \end{description}
 *
 * Not all the fields will have meaning on all operating systems.
 *
 * \twofuns{getpwnam}{getpwuid}
 *
 * \begin{verbatim}
 *     $(getpwnam name...) : Passwd
 *        name : String
 *     $(getpwuid uid...) : Passwd
 *        uid : Int
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+getpwnam+ function looks up an entry by the user's login and the \verb+getpwuid+
 * function looks up an entry by user's numerical id (uid). If no entry is found, an exception
 * will be raised.
 *
 * \fun{getpwents}
 *
 * \begin{verbatim}
 *     $(getpwents) : Array
 * \end{verbatim}
 *
 * The \verb+getpwents+ function returns an array of \verb+Passwd+ objects, one for every user
 * fund in the system user database. Note that depending on the operating system and on the setup
 * of the user database, the returned array may be incomplete or even empty.
 * \end{doc}
 *)

let create_passwd_obj obj passwd =
   let obj = Omake_env.venv_add_field_internal obj Omake_symbol.pw_name_sym   (ValString passwd.Unix.pw_name) in
   let obj = Omake_env.venv_add_field_internal obj Omake_symbol.pw_passwd_sym (ValString passwd.Unix.pw_passwd) in
   let obj = Omake_env.venv_add_field_internal obj Omake_symbol.pw_uid_sym    (ValInt    passwd.Unix.pw_uid) in
   let obj = Omake_env.venv_add_field_internal obj Omake_symbol.pw_gid_sym    (ValInt    passwd.Unix.pw_gid) in
   let obj = Omake_env.venv_add_field_internal obj Omake_symbol.pw_gecos_sym  (ValString passwd.Unix.pw_gecos) in
   let obj = Omake_env.venv_add_field_internal obj Omake_symbol.pw_dir_sym    (ValString passwd.Unix.pw_dir) in
   let obj = Omake_env.venv_add_field_internal obj Omake_symbol.pw_shell_sym  (ValString passwd.Unix.pw_shell) in
      Omake_value_type.ValObject obj

let getpwnam venv pos loc args =
  let pos = string_pos "getpwnam" pos in
  let obj = Omake_env.venv_find_object_or_empty venv Omake_var.passwd_object_var in
  let user =
    match args with
      [user] -> Omake_eval.string_of_value venv pos user
    | _ ->
      raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
  in
  let passwd =
    try Unix.getpwnam user with
      Not_found ->
      raise (Omake_value_type.OmakeException (loc_pos loc pos, StringStringError ("unknown user", user)))
  in
  create_passwd_obj obj passwd

let getpwuid venv pos loc args =
   let pos = string_pos "getpwuid" pos in
   let obj = Omake_env.venv_find_object_or_empty venv Omake_var.passwd_object_var in
   let uid =
      match args with
         [uid] -> Omake_value.int_of_value venv pos uid
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let passwd =
      try Unix.getpwuid uid with
         Not_found ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, StringIntError ("unknown uid", uid)))
   in
      create_passwd_obj obj passwd

let getpwents venv _pos _loc _args =
  let obj = Omake_env.venv_find_object_or_empty venv Omake_var.passwd_object_var in
  let ents = List.map (create_passwd_obj obj) (Lm_unix_util.getpwents ()) in
  Omake_value_type.ValArray ents

(************************************************************************
 * Group database access.
*)

(*
 * \begin{doc}
 * \obj{Group}
 *
 * The \verb+Group+ object represents an entry in the system's user group database.
 * It contains the following fields.
 *
 * \begin{description}
 * \itemidx{gr\_name}: the group name.
 * \itemidx{gr\_group}: the encrypted password.
 * \itemidx{gr\_gid}: group id of the group.
 * \itemidx{gr\_mem}: the group member's user names.
 * \end{description}
 *
 * Not all the fields will have meaning on all operating systems.
 *
 * \twofuns{getgrnam}{getgrgid}
 *
 * \begin{verbatim}
 *     $(getgrnam name...) : Group
 *        name : String
 *     $(getgrgid gid...) : Group
 *        gid : Int
 *     raises RuntimeException
 * \end{verbatim}
 *
 * The \verb+getgrnam+ function looks up a group entry by the group's name and the \verb+getgrgid+
 * function looks up an entry by groups's numerical id (gid). If no entry is found, an exception
 * will be raised.
 *
 * \end{doc}
 *)
let create_group_obj obj group =
  let gr_mem = Array.fold_right (fun s x -> Omake_value_type.ValString s::x) group.Unix.gr_mem [] in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.gr_name_sym   (ValString group.Unix.gr_name) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.gr_passwd_sym (ValString group.Unix.gr_passwd) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.gr_gid_sym    (ValInt    group.Unix.gr_gid) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.gr_mem_sym    (ValArray  gr_mem) in
  Omake_value_type.ValObject obj

let getgrnam venv pos loc args =
   let pos = string_pos "getgrnam" pos in
   let obj = Omake_env.venv_find_object_or_empty venv Omake_var.group_object_var in
   let user =
      match args with
         [user] -> Omake_eval.string_of_value venv pos user
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let group =
      try Unix.getgrnam user with
         Not_found ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, StringStringError ("unknown user", user)))
   in
      create_group_obj obj group

let getgrgid venv pos loc args =
   let pos = string_pos "getgruid" pos in
   let obj = Omake_env.venv_find_object_or_empty venv Omake_var.group_object_var in
   let gid =
      match args with
         [gid] -> Omake_value.int_of_value venv pos gid
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let group =
      try Unix.getgrgid gid with
         Not_found ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, StringIntError ("unknown gid", gid)))
   in
      create_group_obj obj group

(*
 * \begin{doc}
 * \fun{tgetstr}
 *
 * \begin{verbatim}
 *    $(tgetstr id) : String
 *       id : String
 * \end{verbatim}
 *
 * The \verb+tgetstr+ function looks up the terminal capability with the indicated \verb+id+.
 * This assumes the terminfo to lookup is given in the \verb+TERM+ environment variable. This
 * function returns an empty value if the given terminal capability is not defined.
 *
 * Note: if you intend to use the value returned by \verb+tgetstr+ inside the shell
 * \hypervarn{prompt}, you need to wrap it using the \hyperfun{prompt-invisible}.
 * \end{doc}
 *)
let tgetstr venv pos loc args =
  let pos = string_pos "tgetstr" pos in
  match args with
    [arg] ->
    begin match Lm_terminfo.tgetstr (Omake_eval.string_of_value venv pos arg) with
      Some s -> Omake_value_type.ValData s
    | None -> ValNone
    end
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let str_wrap name f v _ pos loc args =
  let pos = string_pos name pos in
  if args <> [] then
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))
  else
    match f v with
      Some s -> Omake_value_type.ValData s
    | None -> ValNone

(*
 * \begin{doc}
 * \twofuns{xterm-escape-begin}{xterm-escape-end}
 *
 * \begin{verbatim}
 *    $(xterm-escape-begin) : String
 *    $(xterm-escape-end) : String
 * \end{verbatim}
 *
 * The \verb+xterm-escape-begin+ and \verb+xterm-escape-end+ functions return the escape sequences
 * that can be used to set the XTerm window title. Will return empty values if this capability is
 * not available.
 *
 * Note: if you intend to use these strings inside the shell \hypervarn{prompt}, you need to use
 * \verb+$(prompt_invisible_begin)$(xterm-escape-begin)+ and
 * \verb+$(xterm-escape-end)$(prompt_invisible_end)+.
 * \end{doc}
 *)
let xterm_escape_begin = str_wrap "xterm-escape-begin" Lm_terminfo.xterm_escape_begin ()
let xterm_escape_end   = str_wrap "xterm-escape-end"   Lm_terminfo.xterm_escape_end   ()

(*
 * \begin{doc}
 * \fun{xterm-escape}
 *
 * \begin{verbatim}
 *    $(xterm-escape s) : Sequence
 * \end{verbatim}
 *
 * When the \verb+TERM+ environment variable indicates that the XTerm title setting capability is available,
 * \verb+$(xterm-escape s)+ is equivalent to \verb+$(xterm-escape-begin)s$(xterm-escape-end)+. Otherwise, it
 * returns an empty value.
 *
 * Note: if you intend to use the value returned by \verb+xterm-escape+ inside the shell
 * \hypervarn{prompt}, you need to wrap it using the \hyperfun{prompt-invisible}.
 * \end{doc}
 *
 * Implemented in Pervasives.om
 *)

(*
 * \begin{doc}
 * \twofuns{prompt-invisible-begin}{prompt-invisible-end}
 *
 * \begin{verbatim}
 *    $(prompt-invisible-begin) : String
 *    $(prompt-invisible-end) : String
 * \end{verbatim}
 *
 * The \verb+prompt-invisible-begin+ and \verb+prompt-invisible-end+ functions return the escape sequences
 * that must used to mark the ``invisible'' sections of the shell \hypervarn{prompt} (such as various escape sequences).
 * \end{doc}
 *)
let opt_wrap f = function
   Some x -> Some (f x)
 | None -> None

let prompt_invisible_begin = str_wrap "prompt-invisible-begin" (opt_wrap fst) Lm_readline.prompt_invisible
let prompt_invisible_end   = str_wrap "prompt-invisible-end"   (opt_wrap snd) Lm_readline.prompt_invisible

(*
 * \begin{doc}
 * \fun{prompt-invisible}
 *
 * \begin{verbatim}
 *    $(prompt-invisible s) : Sequence
 * \end{verbatim}
 *
 * The \verb+prompt-invisible+ will wrap its argument with \verb+$(prompt-invisible-begin)+ and
 * \verb+$(prompt-invisible-end)+. All the `invisible'' sections of the shell \hypervarn{prompt} (such as various
 * escape sequences) must be wrapped this way.
 * \end{doc}
 *
 * Implemented in Pervasives.om
 *)


(*
 * \begin{doc}
 * \fun{gettimeofday}
 *
 * \begin{verbatim}
 *    $(gettimeofday) : Float
 * \end{verbatim}
 *
 * The \verb+gettimeofday+ function returns the time of day in seconds
 * since January 1, 1970.
 *
 * \end{doc}
 *)
let gettimeofday _ pos loc args =
  let pos = string_pos "gettimeofday" pos in
  match args with
    [] ->
    Omake_value_type.ValFloat (Unix.gettimeofday ())
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 0, List.length args)))

(*
 * \begin{doc}
 * \obj{Tm}
 * The \verb+Tm+ object is a structure that represents the time and date.
 *
 * \begin{description}
 * \itemidx{tm\_sec} \verb+: Int+ Seconds (0--59).
 * \itemidx{tm\_min} \verb+: Int+ Minutes (0--59).
 * \itemidx{tm\_hour} \verb+: Int+ Hours (0--23).
 * \itemidx{tm\_mday} \verb+: Int+ Day of the month (0--31).
 * \itemidx{tm\_mon} \verb+: Int+ Month (0--11).
 * \itemidx{tm\_year} \verb+: Int+ Year (minus 1900).
 * \itemidx{tm\_wday} \verb+: Int+ Day of the week (0--6, Sunday is 0).
 * \itemidx{tm\_yday} \verb+: Int+ Day of the year (0--365).
 * \itemidx{tm\_isdst} \verb+: Bool+ True iff daylight savings time is in effect.
 * \itemidx{tm\_format} template \verb+: String+ Format date and time according to template.
 * \end{description}
 *
 * Method~\verb+tm_format+ is modeled after
 * \footahref{https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html}{POSIX~strftime},
 * however it does not implement any locale-related functionality.
 * The format string \verb+template+ consists of zero or more
 * conversion specifications and ordinary characters.  Each
 * time-conversion specification is introduced by a \verb+%+~character
 * after which the following appear in sequence:
 *
 * \begin{itemize}
 *   \item An optional
 *         \begin{itemize}
 *           \item Zero character~\verb+0+, which forces the padding
 *                 character to be \verb+0+ or a
 *           \item Plus character~\verb/+/, which also forces the
 *                 padding character to be \verb+0+, but in addition
 *                 changes how year (\verb+F+, \verb+G+, and \verb+Y+)
 *                 and century (\verb+C+) formats are treated.  If the
 *                 given field width for these formats is large enough
 *                 to accommodate for a \verb/+/- or \verb/-/-sign the
 *                 sign will be printed.  \verb+0+-padding after the
 *                 sign is added to get the requested field width if
 *                 necessary.
 *         \end{itemize}
 *   \item An optional minimum field width.
 *   \item An optional \verb+E+ or \verb+O+ modifier, which parsed but
 *         ignored as there is no locale support implemented.
 *   \item A conversion specifier character that defines the type of
 *         conversion.
 * \end{itemize}
 *
 * The following conversion specifiers are supported:
 *
 * \begin{itemize}
 *   \item Literal conversions
 *   \begin{itemize}
 *     \item \verb+%+: A percent sign.
 *     \item \verb+n+: A newline-character.
 *     \item \verb+t+: A tab-character.
 *   \end{itemize}
 *
 *   \item Date conversions
 *   \begin{itemize}
 *     \item \verb+A+: Full weekday name.
 *     \item \verb+a+: Abbreviated weekday name.
 *     \item \verb+B+: Full month name.
 *     \item \verb+b+: Abbreviated month name.
 *     \item \verb+C+: Century, this is the year divided by 100.
 *     \item \verb+d+: Day of the month.
 *     \item \verb+e+: Day of the month; default padding character is a
 *                     space.
 *     \item \verb+G+: ISO~8601 year based on complete weeks.
 *     \item \verb+g+: ISO~8601 year based on complete weeks
 *                     modulo~100, this is the last two digits of
 *                     \verb+G+.
 *     \item \verb+h+: Same as \verb+b+.
 *     \item \verb+j+: Day of year.
 *     \item \verb+m+: Month.
 *     \item \verb+U+: Week number of the year, where the first Sunday
 *                     of January is the first day of week~1.  Days in
 *                     the new year before this are in week~0.  See
 *                     also \verb+V+ and \verb+W+.
 *     \item \verb+u+: ISO~8601 numeric day of week, where 1 means
 *                     ``Monday'', 2 means ``Tuesday'', \dots, and 7
 *                     means ``Sunday''.
 *     \item \verb+V+: ISO~8601 week number.  See also \verb+U+ and
 *                     \verb+W+.
 *     \item \verb+W+: Week number of the year, where the first Monday
 *                     of January is the first day of week~1.  Days in
 *                     the new year before this are in week~0.  See
 *                     also \verb+U+ and \verb+V+.
 *     \item \verb+w+: Numeric day of week, where 0 means ``Sunday'',
 *                     1 means ``Monday'', \dots, and 6 means
 *                     ``Saturday''.
 *     \item \verb+Y+: Year.
 *   \end{itemize}
 *
 *   \item Time conversions
 *   \begin{itemize}
 *     \item \verb+H+: Hour in 24h format.
 *     \item \verb+I+: Hour in 12h format.
 *     \item \verb+k+: Hour in 24h format; default padding character is
 *                     a space.
 *     \item \verb+l+: Hour in 12h format; default padding character is
 *                     a space.
 *     \item \verb+M+: Minute.
 *     \item \verb+P+: Morning (``a.m.'') or afternoon (``p.m.'')
 *                     indicator in lowercase letters.
 *     \item \verb+p+: Morning (``A.M.'') or afternoon (``P.M.'')
 *                     indicator in uppercase letters.
 *     \item \verb+S+: Second.
 *     \item \verb+s+: Integral number of seconds since the Epoch.
 *     \item \verb+Z+: Abbreviated name of time zone.
 *     \item \verb+z+: Time zone in numeric form.
 *   \end{itemize}
 *
 *   \item Predefined compound date/time conversions
 *   \begin{itemize}
 *     \item \verb+c+: \verb+%a %d %b %Y %I:%M:%S %p %Z+
 *     \item \verb+D+: \verb+%m/%d/%y+
 *     \item \verb+F+: \verb+%Y-%m-%d+; the ISO~8601 date format.
 *     \item \verb+R+: \verb+%H:%M+
 *     \item \verb+r+: \verb+%I:%M:%S %p+
 *     \item \verb+T+: \verb+%H:%M:%S+; the ISO~8601 time format.
 *     \item \verb+X+: Same as \verb+r+.
 *     \item \verb+x+: \verb+%m/%d/%Y+
 *   \end{itemize}
 *
 *   Optional minimum-width and padding-character flags refer to the
 *   compound as a whole not to the individual conversions.
 * \end{itemize}
 *
 * The return type of method~\verb+tm_format+ \emph{always} is String.
 *
 * Example of a user-defined date and time (close to ISO~8601 format):
 * \begin{verbatim}
 *     now = $(gettimeofday)
 *     datetime = $(gmtime $(now))
 *     println($(datetime.tm_format $'%Y-%m-%dT%H:%M:%S%z'))
 * \end{verbatim}
 * \end{doc}
 *)


module StringFormatTime :
sig
  val strftime : string -> Unix.tm -> string
end
  =
  struct
    type time_zone = {
        name : string;
        zone : int;    (*  Difference to GMT in seconds, if
                        *      zone > 0  =>  West of Greenwich,
                        *      zone < 0  =>  East of Greenwich *)
        is_daylight_saving : bool
      }

    external get_time_zone : unit -> time_zone = "get_time_zone"


    type posix_datetime_format_alternatives =
      | StandardFormat
      | AlternativeE
      | AlternativeO


    let is_digit a_character =
      a_character >= '0' && a_character <= '9'
    and digit_value a_character =
      Char.code a_character - Char.code '0'


    let full_weekday_name = function
      | 0 -> "Sunday"
      | 1 -> "Monday"
      | 2 -> "Tuesday"
      | 3 -> "Wednesday"
      | 4 -> "Thursday"
      | 5 -> "Friday"
      | 6 -> "Saturday"


    let abbreviated_weekday_name a_weekday_index =
      String.sub (full_weekday_name a_weekday_index) 0 3


    let full_month_name = function
      |  0 -> "January"
      |  1 -> "February"
      |  2 -> "March"
      |  3 -> "April"
      |  4 -> "May"
      |  5 -> "June"
      |  6 -> "July"
      |  7 -> "August"
      |  8 -> "September"
      |  9 -> "October"
      | 10 -> "November"
      | 11 -> "December"


    let abbreviated_month_name a_month_index =
      String.sub (full_month_name a_month_index) 0 3


    let uppercase_noon_side an_hour =
      if an_hour < 12 then "AM" else "PM"


    let lowercase_noon_side an_hour =
      String.lowercase_ascii (uppercase_noon_side an_hour)


    (*  From OCaml 4.08 on there is [Float.round]. *)
    let round_float x =
      match classify_float x with
      | FP_zero | FP_infinite | FP_nan -> x
      | FP_subnormal -> copysign 0.0 x
      | FP_normal ->
         let pred_one_half = 0x1.fffffffffffffp-2 in
           if x >= 0.0 then
             floor (x +. pred_one_half)
           else
             ceil (x -. pred_one_half)


    let linear_datetime : (float * Unix.tm -> float) = fst
    and broken_down_datetime : (float * Unix.tm -> Unix.tm) = snd


    let january_datetime a_day_of_month a_year =
      Unix.mktime {Unix.tm_sec = 0;  tm_min = 0;  tm_hour = 0;
                   tm_mday = a_day_of_month;  tm_mon = 0;  tm_year = a_year;
                   tm_wday = -1;  tm_yday = -1;  tm_isdst = false}


    (*  Answer the datetime when [a_weekday] occurs first in [a_year]. *)
    let first_weekday_in_year a_weekday a_year =
      let new_year's_day = broken_down_datetime (january_datetime 1 a_year) in
        january_datetime (1 + (7 + a_weekday - new_year's_day.Unix.tm_wday) mod 7) a_year


    let week_number a_start_weekday a_datetime =
      let linear_begin_of_year, begin_of_year =
        first_weekday_in_year a_start_weekday a_datetime.Unix.tm_year in
        if linear_datetime (Unix.mktime a_datetime) < linear_begin_of_year then
          0
        else
          1 + (a_datetime.Unix.tm_yday - begin_of_year.Unix.tm_yday) / 7


    module Iso_8601 =
      struct
        (*  Yet another way to determine whether the first week of [a_year] is incomplete with
         *  respect to the ISO 8601 definition: If the first Monday precedes the first Thursday, the
         *  first week of [a_year] is ISO 8601 incomplete.
         *
         *  Compare for example [cal 1 2010] and [cal 1 2020] for incomplete and complete first
         *  weeks respectively.  *)
        let incomplete_first_week a_year =
          let first_monday = broken_down_datetime (first_weekday_in_year 1 a_year)
          and first_thursday = broken_down_datetime (first_weekday_in_year 4 a_year) in
            first_monday.Unix.tm_mday < first_thursday.Unix.tm_mday


        let days_in a_year =
          let first_monday = broken_down_datetime (first_weekday_in_year 1 a_year)
          and new_year's_eve =
            broken_down_datetime (Unix.mktime {Unix.tm_sec = 0;  tm_min = 0;  tm_hour = 0;
                                               tm_mday = 31;  tm_mon = 11;  tm_year = a_year;
                                               tm_wday = -1;  tm_yday = -1;  tm_isdst = false}) in
            1 + new_year's_eve.Unix.tm_yday - first_monday.Unix.tm_yday


        let week_number a_datetime =
          let linear_first_monday, first_monday = first_weekday_in_year 1 a_datetime.Unix.tm_year in
            if incomplete_first_week a_datetime.Unix.tm_year then
              begin
                if linear_datetime (Unix.mktime a_datetime) < linear_first_monday then
                  let days_in_previous_year = 1 +
                                                days_in (pred a_datetime.Unix.tm_year) +
                                                a_datetime.Unix.tm_yday in
                    (*  Integer division truncates which is why we add 1 for the already commenced
                     *  week; the second 1 accounts for the first ISO week starting at 1 not 0.  *)
                    1 + 1 + days_in_previous_year / 7
                else
                  1 + (a_datetime.Unix.tm_yday - first_monday.Unix.tm_yday) / 7
              end
            else
              1 + (7 + a_datetime.Unix.tm_yday - first_monday.Unix.tm_yday) / 7


        let year a_datetime =
          if incomplete_first_week a_datetime.Unix.tm_year then
            begin
              let linear_first_monday = linear_datetime (first_weekday_in_year 1 a_datetime.Unix.tm_year) in
                if linear_datetime (Unix.mktime a_datetime) < linear_first_monday then
                  pred a_datetime.Unix.tm_year
                else
                  a_datetime.Unix.tm_year
            end
          else
            a_datetime.Unix.tm_year
      end (* module Iso_8601 *)


    let strftime a_format_template a_datetime =
      let previous_index = ref 0
      and index = ref 0
      and formatted_datetime = Buffer.create 64
      and padding_character = ref None
      and extended_year_format = ref false
      and minimum_field_width = ref None
      and modifier = ref StandardFormat
      in
        let add_padding' a_fill_character a_length =
          if a_length >= 1 && a_fill_character <> '\000' then
            Buffer.add_string formatted_datetime (String.make a_length a_fill_character)
        and add_char' a_character = Buffer.add_char formatted_datetime a_character
        and add_string' a_string = Buffer.add_string formatted_datetime a_string
        and add_int' an_integer = Buffer.add_string formatted_datetime (string_of_int an_integer)
        and pad_width a_default_minimum_width an_item_width =
          let field_width =
            match !minimum_field_width with
            | None -> a_default_minimum_width
            | Some width -> width in
            max 0 (field_width - an_item_width)
        in
          let add_padding a_fill_character a_pad_width =
            add_padding'
              (match !padding_character with
               | Some c -> c
               | None -> a_fill_character)
              a_pad_width
          in
            let add_char a_character =
              add_padding ' ' (pad_width 1 1);
              add_char' a_character
            and add_string a_string =
              add_padding ' ' (pad_width 0 (String.length a_string));
              add_string' a_string
            and add_int a_default_minimum_width an_integer =
              let s = string_of_int an_integer in
                add_padding '0' (pad_width a_default_minimum_width (String.length s));
                add_string' s
            and format_extended_year a_default_minimum_width an_integer =
              let pad_with_zero a_width = String.make a_width '0'
              and s = string_of_int an_integer in
                let initial_pad_width = pad_width a_default_minimum_width (String.length s) in
                  match !minimum_field_width with
                  | None -> pad_with_zero initial_pad_width ^ s
                  | Some minimum_width ->
                     if minimum_width <= a_default_minimum_width then
                       pad_with_zero initial_pad_width ^ s
                     else
                       (if an_integer >= 0 then "+" else "-") ^ pad_with_zero (initial_pad_width - 1) ^ s
            in
              begin
                try
                  while true do
                    previous_index := !index;
                    if a_format_template.[!index] = '%' then
                      begin
                        incr index;

                        begin
                          match a_format_template.[!index] with
                          | '0' ->
                             padding_character := Some '0';
                             incr index
                          | '+' ->
                             padding_character := Some '0';
                             extended_year_format := true;
                             incr index
                          | _any_other_character -> ()
                        end;

                        if is_digit a_format_template.[!index] then
                          begin
                            let width = ref (digit_value a_format_template.[!index]) in
                              incr index;
                              while is_digit a_format_template.[!index] do
                                width := !width * 10 + digit_value a_format_template.[!index];
                                incr index
                              done;
                              minimum_field_width := Some !width
                          end;

                        begin
                          match a_format_template.[!index] with
                          | 'E' ->
                             modifier := AlternativeE;
                             incr index
                          | 'O' ->
                             modifier := AlternativeO;
                             incr index
                          | _any_other_character -> ()
                        end;

                        begin
                          match a_format_template.[!index] with
                          | '%' -> add_char '%'
                          | 'A' -> add_string (full_weekday_name a_datetime.Unix.tm_wday)
                          | 'a' -> add_string (abbreviated_weekday_name a_datetime.Unix.tm_wday)
                          | 'B' -> add_string (full_month_name a_datetime.Unix.tm_mon)
                          | 'b' | 'h' -> add_string (abbreviated_month_name a_datetime.Unix.tm_mon)
                          | 'C' ->
                             let century = (1900 + a_datetime.Unix.tm_year) / 100 in
                               if !extended_year_format then
                                 add_string (format_extended_year 2 century)
                               else
                                 add_int 2 century
                          | 'c' ->
                             (*  LC_ALL= date +%c       ->  Mon 27 Jan 2020 10:30:09 AM CET
                              *  LC_ALL=POSIX date +%c  ->  Mon Jan 27 10:30:48 2020  *)
                             add_string (Printf.sprintf
                                           "%s %02i %s %04i %02i:%02i:%02i %s %s"
                                           (abbreviated_weekday_name a_datetime.Unix.tm_wday)
                                           a_datetime.Unix.tm_mday
                                           (abbreviated_month_name a_datetime.Unix.tm_mon)
                                           (1900 + a_datetime.Unix.tm_year)
                                           (a_datetime.Unix.tm_hour mod 12)
                                           a_datetime.Unix.tm_min
                                           a_datetime.Unix.tm_sec
                                           (uppercase_noon_side a_datetime.Unix.tm_hour)
                                           (get_time_zone ()).name)
                          | 'D' ->
                             add_string (Printf.sprintf
                                           "%02i/%02i/%02i"
                                           (1 + a_datetime.Unix.tm_mon)
                                           a_datetime.Unix.tm_mday
                                           (a_datetime.Unix.tm_year mod 100))
                          | 'd' -> add_int 2 a_datetime.Unix.tm_mday
                          | 'e' ->
                             if !padding_character = None then padding_character := Some ' ';
                             add_int 2 a_datetime.Unix.tm_mday
                          | 'F' ->
                             let year, month, day = 1900 + a_datetime.Unix.tm_year,
                                                    1 + a_datetime.Unix.tm_mon,
                                                    a_datetime.Unix.tm_mday in
                               add_string (if !extended_year_format then
                                             begin
                                               begin
                                                 match !minimum_field_width with
                                                 | None -> ()
                                                 | Some width -> minimum_field_width := Some (width - 6)
                                               end;
                                               Printf.sprintf
                                                 "%s-%02i-%02i"
                                                 (format_extended_year 4 year) month day
                                             end
                                           else
                                             Printf.sprintf "%04i-%02i-%02i" year month day)
                          | 'G' ->
                             let year = 1900 + Iso_8601.year a_datetime in
                               if !extended_year_format then
                                 add_string (format_extended_year 4 year)
                               else
                                 add_int 4 year
                          | 'g' -> add_int 2 (Iso_8601.year a_datetime mod 100)
                          | 'H' -> add_int 2 a_datetime.Unix.tm_hour
                          | 'I' -> add_int 2 (a_datetime.Unix.tm_hour mod 12)
                          | 'j' -> add_int 3 (1 + a_datetime.Unix.tm_yday)
                          | 'k' -> (* Non POSIX extension *)
                             if !padding_character = None then padding_character := Some ' ';
                             add_int 2 a_datetime.Unix.tm_hour
                          | 'l' -> (* Non POSIX extension *)
                             if !padding_character = None then padding_character := Some ' ';
                             add_int 2 (a_datetime.Unix.tm_hour mod 12)
                          | 'M' -> add_int 2 a_datetime.Unix.tm_min
                          | 'm' -> add_int 2 (1 + a_datetime.Unix.tm_mon)
                          | 'n' -> add_char '\n'
                          | 'P' -> (* Non POSIX extension *)
                             add_string (lowercase_noon_side a_datetime.Unix.tm_hour)
                          | 'p' -> add_string (uppercase_noon_side a_datetime.Unix.tm_hour)
                          | 'R' ->
                             add_string (Printf.sprintf
                                           "%02i:%02i"
                                           a_datetime.Unix.tm_hour
                                           a_datetime.Unix.tm_min)
                          | 'r' ->
                             add_string (Printf.sprintf
                                           "%02i:%02i:%02i %s"
                                           (a_datetime.Unix.tm_hour mod 12)
                                           a_datetime.Unix.tm_min
                                           a_datetime.Unix.tm_sec
                                           (uppercase_noon_side a_datetime.Unix.tm_hour))
                          | 'S' -> add_int 2 a_datetime.Unix.tm_sec
                          | 's' -> (* Non POSIX extension *)
                             let seconds_since_epoch, _tm = Unix.mktime a_datetime in
                               add_int 1 (int_of_float (round_float seconds_since_epoch))
                          | 'T' ->
                             add_string (Printf.sprintf
                                           "%02i:%02i:%02i"
                                           a_datetime.Unix.tm_hour
                                           a_datetime.Unix.tm_min
                                           a_datetime.Unix.tm_sec)
                          | 't' -> add_char '\t'
                          | 'U' -> add_int 2 (week_number 0 a_datetime)
                          | 'u' ->
                             add_int 1 (if a_datetime.Unix.tm_wday = 0 then 7 else a_datetime.Unix.tm_wday)
                          | 'V' -> add_int 2 (Iso_8601.week_number a_datetime)
                          | 'W' ->  add_int 2 (week_number 1 a_datetime)
                          | 'w' -> add_int 1 a_datetime.Unix.tm_wday
                          | 'X' ->
                             (*  LC_ALL= date +%X       ->  10:39:51 AM
                              *  LC_ALL=POSIX date +%X  ->  10:39:51  *)
                             add_string (Printf.sprintf
                                           "%02i:%02i:%02i %s"
                                           (a_datetime.Unix.tm_hour mod 12)
                                           a_datetime.Unix.tm_min
                                           a_datetime.Unix.tm_sec
                                           (uppercase_noon_side a_datetime.Unix.tm_hour))
                          | 'x' ->
                             (*  LC_ALL= date +%x       ->  01/30/2020
                              *  LC_ALL=POSIX date +%x  ->  01/30/20  *)
                             add_string (Printf.sprintf
                                           "%02i/%02i/%04i"
                                           (1 + a_datetime.Unix.tm_mon)
                                           a_datetime.Unix.tm_mday
                                           (1900 + a_datetime.Unix.tm_year))
                          | 'Y' ->
                             let year = 1900 + a_datetime.Unix.tm_year in
                               if !extended_year_format then
                                 add_string (format_extended_year 4 year)
                               else
                                 add_int 4 year
                          | 'y' -> add_int 2 (a_datetime.Unix.tm_year mod 100)
                          | 'Z' -> add_string (get_time_zone ()).name
                          | 'z' ->
                             let tz = get_time_zone () in
                               let delta_hours = tz.zone / 3600
                               and delta_minutes = (tz.zone mod 3600) / 60 in
                                 add_char' (if delta_hours <= 0 then '+' else '-');
                                 add_padding '0' (pad_width 0 5);
                                 add_string' (Printf.sprintf "%02i%02i" (abs delta_hours) delta_minutes)
                          | _unknown_format_specifier ->
                             (*  Answer the whole format specification, which we could not parse.  *)
                             add_string (String.sub
                                           a_format_template
                                           !previous_index
                                           (!index - !previous_index + 1))
                        end;

                        padding_character := None;
                        extended_year_format := false;
                        minimum_field_width := None;
                        modifier := StandardFormat
                      end
                    else
                      add_char' a_format_template.[!index];
                    incr index
                  done
                with Invalid_argument _index -> ()
              end;

              Buffer.contents formatted_datetime
  end (* StringFormatTime *)


let format_method_name = "Tm.format"
let format_method_symbol = Lm_symbol.add format_method_name

let tm_method_format venv pos loc args _kargs =
  let pos' = string_pos format_method_name pos in
    let get an_instance_variable =
      Omake_eval.string_of_value venv pos'
        (Omake_env.venv_find_field_internal_exn (Omake_env.venv_this venv) an_instance_variable) in
      let get_int an_instance_variable = int_of_string (get an_instance_variable) in
        match args with
          format_template_value :: [] ->
           let format_template = Omake_eval.string_of_value venv pos' format_template_value
           and datetime = {Unix.tm_sec = get_int Omake_symbol.tm_sec_sym;
                            tm_min = get_int Omake_symbol.tm_min_sym;
                            tm_hour = get_int Omake_symbol.tm_hour_sym;
                            tm_mday = get_int Omake_symbol.tm_mday_sym;
                            tm_mon = get_int Omake_symbol.tm_mon_sym;
                            tm_year = get_int Omake_symbol.tm_year_sym;
                            tm_wday = get_int Omake_symbol.tm_wday_sym;
                            tm_yday = get_int Omake_symbol.tm_yday_sym;
                            tm_isdst = get Omake_symbol.tm_isdst_sym = "true"} in
             let formatted_datetime = StringFormatTime.strftime format_template datetime in
               venv, Omake_value_type.ValString formatted_datetime
        | _ ->
           raise (Omake_value_type.OmakeException (loc_pos loc pos',
                                                   ArityMismatch (ArityExact 1, List.length args)))

let tm_object venv info =
  let obj = Omake_env.venv_find_object_or_empty venv Omake_var.tm_object_var in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_sec_sym   (ValInt info.Unix.tm_sec) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_min_sym   (ValInt info.Unix.tm_min) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_hour_sym  (ValInt info.Unix.tm_hour) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_mday_sym  (ValInt info.Unix.tm_mday) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_mon_sym   (ValInt info.Unix.tm_mon) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_year_sym  (ValInt info.Unix.tm_year) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_wday_sym  (ValInt info.Unix.tm_wday) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_yday_sym  (ValInt info.Unix.tm_yday) in
  let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_isdst_sym
      (if info.Unix.tm_isdst then Omake_builtin_util.val_true else Omake_builtin_util.val_false) in
  let obj =
    Omake_env.venv_add_field_internal
      obj
      Omake_symbol.tm_format_sym
      (Omake_value_type.ValPrim (Omake_ir.ArityExact 1,
                                 false,
                                 Omake_ir.ApplyNonEmpty,
                                 (Omake_env.venv_add_prim_fun
                                    (Omake_env.venv_with_object venv obj)
                                    format_method_symbol
                                    tm_method_format)))
  in
    obj

(*
 * \begin{doc}
 * \twofuns{gmtime}{localtime}
 * \begin{verbatim}
 *    $(gmtime time) : tm
 *    $(localtime time) : tm
 *        time : Float
 * \end{verbatim}
 *
 * Convert the time in seconds since the Unix epoch to calendar format.
 * The function \verb+gmtime+ assumes UTC (Coordinated Universal Time);
 * the function \verb+localtime+ uses the local time zone.
 * \end{doc}
 *)

let gmtime venv pos loc args =
  let pos = string_pos "gmtime" pos in
  match args with
    [now] ->
    let info = Unix.gmtime (Omake_value.float_of_value venv pos now) in
    Omake_value_type.ValObject (tm_object venv info)
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

let localtime venv pos loc args =
  let pos = string_pos "gmtime" pos in
  match args with
    [now] ->
    let info = Unix.localtime (Omake_value.float_of_value venv pos now) in
    Omake_value_type.ValObject (tm_object venv info)
  | _ ->
    raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * \begin{doc}
 * \twofuns{mktime}{normalize-time}
 * \begin{verbatim}
 *    $(mktime tm) : Float
 *    $(normalize-time tm) : Tm
 *        tm : Tm
 * \end{verbatim}
 *
 * Convert the calendar time to time in seconds since the Unix epoch.
 * Assumes the local time zone.
 *
 * The fields \verb+tm_wday+, \verb+tm_mday+, \verb+tm_yday+ are ignored.
 * The other components are not restricted to their normal ranges and will be
 * normalized as needed.
 *
 * The function \verb+normalize-time+ normalizes the
 * calendar time.  The returned object contains an additional field
 * \verb+tm_time : Float+ that represents the time in seconds since the Unix epoch
 * (the same value returned by \verb+mktime+).
 * \end{doc}
 *)
let mktime_aux select venv pos loc args =
   let pos = string_pos "mktime" pos in
   let obj =
      match args with
         [obj] ->
            Omake_eval.eval_object venv pos obj
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))
   in
   let info =
      { Unix.tm_sec   = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_sec_sym);
        Unix.tm_min   = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_min_sym);
        Unix.tm_hour  = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_hour_sym);
        Unix.tm_mday  = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_mday_sym);
        Unix.tm_mon   = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_mon_sym);
        Unix.tm_year  = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_year_sym);
        Unix.tm_wday  = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_wday_sym);
        Unix.tm_yday  = Omake_value.int_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_yday_sym);
        Unix.tm_isdst = Omake_eval.bool_of_value venv pos (Omake_env.venv_find_field_internal obj pos Omake_symbol.tm_isdst_sym)
      }
   in
      select (Unix.mktime info)

let mktime = mktime_aux (fun (secs, _) -> Omake_value_type.ValFloat secs)
let normalize_tm venv pos loc args =
  mktime_aux (fun (secs, tm) ->
    let obj = tm_object venv tm in
    let obj = Omake_env.venv_add_field_internal obj Omake_symbol.tm_time_sym (Omake_value_type.ValFloat secs) in
    Omake_value_type.ValObject obj) venv pos loc args

(************************************************************************
 * Tables.
*)

let () =
  let builtin_funs = [
    true, "gettimeofday",  gettimeofday,  Omake_ir.ArityExact 0;
    true, "getpwnam",      getpwnam,      ArityExact 1;
    true, "getpwuid",      getpwuid,      ArityExact 1;
    true, "getpwents",     getpwents,     ArityExact 0;
    true, "getgrnam",      getgrnam,      ArityExact 1;
    true, "getgrgid",      getgrgid,      ArityExact 1;
    true, "tgetstr",       tgetstr,       ArityExact 1;
    true, "xterm-escape-begin", xterm_escape_begin, ArityExact 0;
    true, "xterm-escape-end",   xterm_escape_end,   ArityExact 0;
    true, "prompt-invisible-begin", prompt_invisible_begin, ArityExact 0;
    true, "prompt-invisible-end",   prompt_invisible_end,   ArityExact 0;
    true, "gmtime",        gmtime,        ArityExact 1;
    true, "localtime",     localtime,     ArityExact 1;
    true, "mktime",        mktime,        ArityExact 1;
    true, "normalize-tm",  normalize_tm,  ArityExact 1;
  ] in
  let builtin_info =
    { Omake_builtin_type.builtin_empty with builtin_funs = builtin_funs }
  in
  Omake_builtin.register_builtin builtin_info

