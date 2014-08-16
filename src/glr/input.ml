
type buffer = { fname : string; lnum : int; length : int; contents : string; mutable next : buffer }

let rec empty_buffer fname lnum = 
  let rec res = { fname; lnum; length = 0; contents = ""; next = res } in
  res

let is_empty b = b == b.next

let fname b = b.fname

let line_num b = b.lnum

let line b = b.contents

let rec read b i =
  if is_empty b then ('\255', b, 0) else
  match compare i b.length with
    -1 -> b.contents.[i], b, i+1
  | 0 -> '\n', b.next, 0
  | _ -> read b (b.length - i - 1)
 
let push b fname lnum contents =
  let length = String.length contents in
  let res = { fname; lnum; length; contents; next = empty_buffer fname (lnum+1) } in
  b.next <- res;
  res

let line_num_directive =
  Str.regexp "[ \t]*\\([0-9]+\\)[ \t]*\\([\"]\\([^\"]*\\)[\"]\\)?[ \t]*$"

let define_directive =
  Str.regexp "[ \t]*if[ \t]*def[ \t]*\\([^ \t]\\)[ \t]*\\([\"]\\([^\"]\\)[\"]\\)[ \t]*"

let if_directive =
  Str.regexp "[ \t]*if"

let ifdef_directive =
  Str.regexp "[ \t]*if[ \t]*def[ \t]*\\([^ \t]\\)[ \t]*"

let ifundef_directive =
  Str.regexp "[ \t]*if[ \t]*undef[ \t]*\\([^ \t]\\)[ \t]*"

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
      let major, minor = 
	match  Str.split (Str.regexp_string ".") Sys.ocaml_version with
	| major ::  minor :: _ ->
	   let major = int_of_string major in
	   let minor = int_of_string minor in
	   major, minor
	| _ -> assert false
      in
      predicat (major, minor) (int_of_string major', int_of_string minor')
    with _ -> 
      Printf.eprintf "file: %s, line %d: bad predicate version" fname num;
      exit 1
  else (
    Printf.eprintf "file: %s, line %d: unknown #if directive" fname num;
    exit 1)

let else_directive =
  Str.regexp "[ \t]*else[ \t]*"

let endif_directive =
  Str.regexp "[ \t]*endif[ \t]*"


let buffer_from_fun name get_line data =
  let rec fn fname active num res =
    begin
      try
	let num = num + 1 in
	let line = get_line data in
	(fun () -> 
	 let len = String.length line in
	 if len = 0 then
	   fn fname active num res
	 else
	   if line.[0] = '#' then
	     if Str.string_match line_num_directive line 1 then
	       let num =
		 int_of_string (Str.matched_group 1 line)
	       in
	       let fname = 
		 try Str.matched_group 3 line with Not_found -> fname
	       in
	       fn fname active num res
	     else if Str.string_match define_directive line 1 then
	       let macro_name = Str.matched_group 1 line in
	       let value = Str.matched_group 3 line in
	       Unix.putenv macro_name value;
	       fn fname active num res
	     else if Str.string_match if_directive line 1 then
	       let b = test_directive fname num line in
	       let status = fn fname (b && active) num res in
	       match status with
               | `End_of_file ->
		  Printf.eprintf "file: %s, line %d: expecting '#else' or '#endif'" fname num;
		  exit 1
	       | `Endif -> fn fname active num res
	       | `Else -> 
		  let status = fn fname (not b && active) num res in 
		  match status with 
		  | `Else | `End_of_file ->
		    Printf.eprintf "file: %s, line %d: expecting '#endif'" fname num;
		    exit 1
		  | `Endif -> fn fname active num res
	     else if Str.string_match else_directive line 1 then
	       `Else
	     else if Str.string_match endif_directive line 1 then
	       `Endif
	     else (
	       let res = 
		 if active then push res fname num line
		 else res
	       in
	       fn fname active num res)
	   else (
	     let res = 
	       if active then push res fname num line
	       else res
	     in
	     fn fname active num res))
      with
	End_of_file -> fun () -> `End_of_file
    end ()
  in
  let start = empty_buffer name 0 in
  let status = fn name true 0 start in
  match status with
  | `Else ->
     Printf.eprintf "file: %s, extra '#else'" name;
     exit 1
  | `Endif ->
     Printf.eprintf "file: %s, extra '#endif'" name;
     exit 1
  | `End_of_file -> 
     start.next
  
let buffer_from_channel name ch = buffer_from_fun name input_line ch

let buffer_from_file name =
  let ch = open_in name in
  buffer_from_fun name input_line ch

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
  String.sub str start (_end - start)

let buffer_from_string name str =
  let data = (str, ref 0) in
  buffer_from_fun name get_string_line data
