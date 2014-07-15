open Glr

let blank = blank_regexp (Str.regexp "[ \t\n\r]*")

let arith_sum
  = declare_grammar () 

let re_float = "[0-9]+\\([.][0-9]+\\)?\\([eE][-]?[0-9]+\\)?"

let arith_atom =
  glr
    f:RE(re_float)[float_of_string (groupe 0)]
  | STR"(" s:arith_sum STR")"
  end 

let arith_pow = declare_grammar () 
let _ = set_grammar arith_pow 
   glr
    a:arith_atom r:{STR"**" b:arith_pow -> fun x -> x ** b}?[fun x -> x] -> r a
        (* ?[...] avoid to use None | Some for option *)
   end

let arith_prod =
  glr
    a:arith_pow 
    f:{op:RE"[*]\\|/" b:arith_pow
           -> fun f x -> if op = "*" then f x *. b else if b = 0.0 then raise Give_up else f x /. b}*[fun x -> x]
                   (* *[...] avoid to use lists for repetition *)
      -> f a
  end

let _ = set_grammar arith_sum
  glr
    a:arith_prod
    f:{op:RE"[+]\\|-" b:arith_prod
           -> fun f x -> if op = "+" then f x +. b else f x -. b}*[fun x -> x]
      -> f a
  end

let arith =
  glr
    a:arith_sum EOF -> a
  end

let _ =
  if Unix.((fstat (descr_of_in_channel Pervasives.stdin)).st_kind = S_REG)
  then
      try
	let x = parse_channel arith blank stdin in
	Printf.printf "=> %f\n" x
      with
	Parse_error n -> Printf.fprintf stderr "Parse error after char %d\n%!" n
      | Ambiguity(n,p) -> Printf.fprintf stderr "Ambiguous expression from %d to %d\n%!" n p
  else
    try
      while true do
	try
	  Printf.printf ">> %!";
	  let x = parse_string arith blank (input_line stdin) in
	  Printf.printf "=> %f\n%!" x
	with
	  Parse_error n -> Printf.fprintf stderr "Parse error after char %d\n%!" n
	| Ambiguity(n,p) -> Printf.fprintf stderr "Ambiguous expression from %d to %d\n%!" n p
      done
  with End_of_file -> ()
