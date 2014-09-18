type buffer = buffer_aux Lazy.t
 and buffer_aux = { is_empty     : bool     (* Is the buffer empty? *)
                  ; name         : string   (* The name of the buffer. *)
                  ; lnum         : int      (* Current line number. *)
                  ; bol          : int      (* Offset to current line. *)
                  ; length       : int      (* Length of the buffer. *)
                  ; contents     : string   (* Contents of the line. *)
                  ; mutable next : buffer } (* Rest of the buffer. *)

let rec read (lazy b as b0) i =
  if b.is_empty then ('\255', b0, 0) else
  match compare i b.length with
  | -1 -> b.contents.[i], b0, i+1
  | 0  -> '\n', b.next, 0
  | _  -> read b.next (b.length - i - 1)

let rec get (lazy b) i =
  if b.is_empty then '\255' else
  match compare i b.length with
  | -1 -> b.contents.[i]
  | 0  -> '\n'
  | _  -> get b.next (b.length - i - 1)

let empty_buffer fn lnum bol =
  let rec res =
    lazy { is_empty = true
         ; name     = fn
         ; lnum     = lnum
         ; bol      = bol
         ; length   = 0
         ; contents = ""
         ; next     = res }
  in res

let is_empty (lazy b) = b.is_empty

let fname (lazy b) = b.name

let line_num (lazy b) = b.lnum

let line_beginning (lazy b) = b.bol

let line (lazy b) = b.contents

let lexing_position str pos =
  let bol = line_beginning str in
  Lexing.({ pos_fname = fname str
          ; pos_lnum  = line_num str
          ; pos_cnum  = bol +pos
          ; pos_bol   = bol })

let line_num_directive =
  Str.regexp "[ \t]*\\([0-9]+\\)[ \t]*\\([\"]\\([^\"]*\\)[\"]\\)?[ \t]*$"

let define_directive =
  Str.regexp "[ \t]*define[ \t]*\\([^ \t]*\\)[ \t]*\\([^ \n\t\r]*\\)[ \t]*"

let if_directive =
  Str.regexp "[ \t]*if"

let ifdef_directive =
  Str.regexp "[ \t]*if[ \t]*def[ \t]*\\([^ \t]*\\)[ \t]*"

let ifundef_directive =
  Str.regexp "[ \t]*if[ \t]*ndef[ \t]*\\([^ \t]*\\)[ \t]*"

let ifversion_directive =
  Str.regexp "[ \t]*if[ \t]*version[ \t]*\\([<>=]*\\)[ \t]*\\([0-9]+\\)[.]\\([0-9]+\\)[ \t]*"

let test_directive fname num line =
  if Str.string_match ifdef_directive line 1 then
    let macro_name = Str.matched_group 1 line in
    try ignore (Sys.getenv macro_name); true with Not_found -> false
  else if Str.string_match ifundef_directive line 1 then
    let macro_name = Str.matched_group 1 line in
    try ignore (Sys.getenv macro_name); false with Not_found -> true
  else if Str.string_match ifversion_directive line 1 then
    let predicat = Str.matched_group 1 line in
    let major' = Str.matched_group 2 line in
    let minor' = Str.matched_group 3 line in
    try
      let predicat =
        match predicat with
        | "<>" -> (<>) | "=" -> (=) | "<" -> (<)
        | ">" -> (>) | "<=" -> (<=) | ">=" -> (>=)
        | _ -> raise Exit
      in
      let version =
        try
          Sys.getenv "OCAMLVERSION"
        with
          Not_found -> Sys.ocaml_version
      in
      let major, minor =
        match  Str.split (Str.regexp_string ".") version with
        | major ::  minor :: _ ->
           let major = int_of_string major in
           let minor = int_of_string minor in
           major, minor
        | _ -> assert false
      in
      predicat (major, minor) (int_of_string major', int_of_string minor')
    with _ ->
      Printf.eprintf "file: %s, line %d: bad predicate version\n%!" fname num;
      exit 1
  else (
    Printf.eprintf "file: %s, line %d: unknown #if directive\n%!" fname num;
    exit 1)

let else_directive =
  Str.regexp "[ \t]*else[ \t]*"

let elif_directive =
  Str.regexp "[ \t]*elif[ \t]*"

let endif_directive =
  Str.regexp "[ \t]*endif[ \t]*"

type cont_info =
    Else | Endif | EndOfFile | Elif of bool

let buffer_from_fun fname get_line data =
  let rec fn fname active num bol cont =
    begin
      let num = num + 1 in
      try
        let line = get_line data in
        let len = String.length line in
        let bol' = bol + len + 1 in (* +1 for newline, should be 2 on windows ? *)
        (fun () ->
           if len > 0 && line.[0] = '#' then
             if Str.string_match line_num_directive line 1 then
               let num =
                 int_of_string (Str.matched_group 1 line)
               in
               let fname =
                 try Str.matched_group 3 line with Not_found -> fname
               in
               fn fname active num bol' cont
             else if Str.string_match define_directive line 1 then
               let macro_name = Str.matched_group 1 line in
               let value = Str.matched_group 2 line in
               Unix.putenv macro_name value;
               fn fname active num bol' cont
             else if Str.string_match if_directive line 1 then
               let b = test_directive fname num line in
               fn fname (b && active) num bol' (
                    let rec cont' b = fun fname (status:cont_info) num bol ->
                      match status with
                      | EndOfFile ->
                         Printf.eprintf "file: %s, line %d: expecting '#else' or '#endif'" fname num;
                         exit 1
                      | Endif -> fn fname active num bol cont
                      | Else ->
                         fn fname (not b && active) num bol
                            (fun fname (status:cont_info) num bol ->
                             match status with
                             | Elif _ | Else | EndOfFile ->
                                                  Printf.eprintf "file: %s, line %d: expecting '#endif'" fname num;
                                                  exit 1
                             | Endif -> fn fname active num bol cont)
                      | Elif b' ->
                         fn fname (not b && b' && active) num bol (cont' (b || b'))
                    in
                    cont' b)
             else if Str.string_match elif_directive line 1 then
               let b = test_directive fname num line in
               cont fname (Elif b) num bol'
             else if Str.string_match else_directive line 1 then
               cont fname Else num bol'
             else if Str.string_match endif_directive line 1 then
               cont fname Endif num bol'
             else if active then (
               { is_empty = false; name = fname; lnum = num; bol; length = len ; contents = line ;
                 next = lazy (fn fname active num bol' cont) })
             else fn fname active num  bol' cont
           else if active then (
               { is_empty = false; name = fname; lnum = num; bol; length = len ; contents = line ;
                 next = lazy (fn fname active num bol' cont) })
           else fn fname active num bol' cont)
      with
        End_of_file -> fun () -> cont fname EndOfFile num bol
    end ()
  in
  lazy (fn fname true 0 0 (fun fname status line bol ->
  match status with
  | Else ->
     Printf.eprintf "file: %s, extra '#else'" fname;
     exit 1
  | Elif _ ->
     Printf.eprintf "file: %s, extra '#elif'" fname;
     exit 1
  | Endif ->
     Printf.eprintf "file: %s, extra '#endif'" fname;
     exit 1
  | EndOfFile ->
     Lazy.force (empty_buffer fname line bol)))

let buffer_from_channel ?(filename="") ch =
  buffer_from_fun filename input_line ch

let buffer_from_file filename =
  let ch = open_in filename in
  buffer_from_fun filename input_line ch

let get_string_line (str, p) =
  let len = String.length str in
  let start = !p in
  if start >= len then raise End_of_file;
  while (!p < len && str.[!p] <> '\n' && str.[!p] <> '\r') do
    incr p
  done;
  let _end = !p in
  incr p;
  if !p < len && ((str.[!p] = '\n' && str.[!p - 1] = '\r') ||
                    (str.[!p] = '\r' && str.[!p - 1] = '\n')) then incr p;
  let len' = _end - start in
  if start = 0 && len' = len then str else String.sub str start len'

let buffer_from_string ?(filename="") str =
  let data = (str, ref 0) in
  buffer_from_fun filename get_string_line data
