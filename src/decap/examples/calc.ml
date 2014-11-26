open Decap

let blank = blank_regexp (Str.regexp "[ \t\n\r]*")

let arith_sum
  = declare_grammar "arith_sum"

let re_float = "[0-9]+\\([.][0-9]+\\)?\\([eE][-]?[0-9]+\\)?"

parser arith_atom =
  | f:RE(re_float)[float_of_string (groupe 0)]
  | CHR('-') f:RE(re_float)[float_of_string (groupe 0)] -> -. f
  | STR"(" s:arith_sum STR")"

let arith_pow = declare_grammar "arith_pow"
let _ = set_grammar arith_pow 
   parser
    a:arith_atom r:{STR"**" b:arith_pow -> fun x -> x ** b}?[fun x -> x] -> r a
        (* ?[...] avoid to use None | Some for option *)

let arith_prod =
  parser
    a:arith_pow 
    f:{op:RE"[*]\\|/" b:arith_pow
           -> fun f x -> if op = "*" then f x *. b else if b = 0.0 then raise (Give_up "Division by 0") else f x /. b}*[fun x -> x]
                   (* *[...] avoid to use lists for repetition *)
      -> f a

let _ = set_grammar arith_sum
  parser
    a:arith_prod
    f:{op:RE"[+]\\|-" b:arith_prod
           -> fun f x -> if op = "+" then f x +. b else f x -. b}*[fun x -> x]
      -> f a

let _ =
  if Unix.((fstat (descr_of_in_channel Pervasives.stdin)).st_kind = S_REG)
  then handle_exception (fun () ->
		     let x = parse_channel arith_sum blank stdin in
		     Printf.printf "=> %f\n" x) ()
  else
    try
      while true do
	handle_exception (fun () ->
	  Printf.printf ">> %!";
	  let x = parse_string arith_sum blank (input_line stdin) in
	  Printf.printf "=> %f\n%!" x) ()
      done
  with End_of_file -> ()
