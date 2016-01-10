open Decap

let blank = blank_regexp ''[ \t\n\r]*''

let float_re = ''[0-9]+\([.][0-9]+\)?\([eE][-]?[0-9]+\)?''
let ident_re = ''[a-zA-Z_'][a-zA-Z0-9_']*''

type calc_prio = Sum | Prod | Pow | Atom

let env = Hashtbl.create 101

let parser expression prio =
  | f:RE(float_re) when prio = Atom -> float_of_string f
  | id:RE(ident_re) when prio = Atom ->
      (try Hashtbl.find env id
       with Not_found ->
	 Printf.eprintf "Unbound %s\n%!" id; raise Exit)
  | '(' e:(expression Sum) ')' when prio = Atom -> e
  | e:(expression Atom) "**" e':(expression Pow) when prio = Pow -> e ** e'
  | e:(expression Prod) fn:{'*' -> ( *. ) | '/' -> ( /. )} e':(expression Pow) when prio = Prod ->
     fn e e'
  | e:(expression Sum) fn:{'+' -> ( +. ) | '-' -> ( -. )} e':(expression Prod) when prio = Sum ->
     fn e e'
  | e:(expression Prod) when prio = Sum -> e
  | e:(expression Pow) when prio = Prod -> e
  | e:(expression Atom) when prio = Pow -> e
  | '-' - e:(expression Pow) when prio = Pow -> -. e
  | '+' - e:(expression Pow) when prio = Pow -> e

let parser arith =
  | id:RE(ident_re) CHR('=') e:(expression Sum) -> Hashtbl.add env id e; e
  | e:(expression Sum) -> e

let _ =
  if Unix.((fstat (descr_of_in_channel Pervasives.stdin)).st_kind = S_REG)
  then
      handle_exception (fun () ->
	let x = parse_channel arith blank stdin in
	Printf.printf "=> %f\n" x) ()
  else
    try
      while true do
	handle_exception (fun () ->
	  Printf.printf ">> %!";
	  let x = parse_string arith blank (input_line stdin) in
	  Printf.printf "=> %f\n%!" x) ()

      done
  with End_of_file -> ()
