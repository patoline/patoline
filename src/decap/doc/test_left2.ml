
open Decap

let _ = active_debug := false
			  
let parser a : char list grammar =
  | DEBUG"a0" EMPTY -> []
  | DEBUG"bc1" lb:b DEBUG"bc2" lc:c DEBUG"bc3" "bc" DEBUG"bc4" -> lb@lc@['b';'c']
  | DEBUG"cb1" lc:c DEBUG"cb2" lb:b DEBUG"cb3" "cb" DEBUG"cb4" -> lc@lb@['c';'b']

and b : char list grammar = 
  | DEBUG"b0" EMPTY -> []
  | DEBUG"ac1" la:a DEBUG"ac2" lc:c DEBUG"ac3" "ac" DEBUG"ac4" -> la@lc@['a';'c']
  | DEBUG"ca1" lc:c DEBUG"ca2" la:a DEBUG"ca3" "ca" DEBUG"ca4" -> lc@la@['c';'a'] 

and c : char list grammar = 
  | DEBUG"c0" EMPTY -> []
  | DEBUG"ab1" la:a DEBUG"ab2" lb:b DEBUG"ab3" "ab" DEBUG"ab4" -> la@lb@['a';'b']
  | DEBUG"ba1" lb:b DEBUG"ba2" la:a DEBUG"ba3" "ba" DEBUG"ba4" -> lb@la@['b';'a']

let compare s l =
  let s' = String.create (List.length l) in
  let rec fn i = function
      [] -> ()
    | c::l' -> s'.[i] <- c; fn (i+1) l'
  in
  fn 0 l;
  Printf.printf "-> %S\n%!" s';
  assert (s' = s)

let (^^) s l = List.map (fun x -> s ^ x) l 				 
let rec gena suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genb (genc ("bc"^^suffix) i) j @
	       genc (genb ("cb"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genb suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := gena (genc ("ac"^^suffix) i) j @
	       genc (gena ("ca"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genc suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := gena (genb ("ab"^^suffix) i) j @
	       genb (gena ("ba"^^suffix) i) j @ !res
    done;
    !res
  else suffix

	 
let parser main = a

(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
(*  Printf.printf "PARSING: %S\n%!" "bcbcbacaabbc";
  handle_exception (parse_string main blank) "bcbcbacaabbc";*)
  let n = int_of_string (Sys.argv.(1)) in
  for i = max 0 (-n) to abs n do
    let l = gena [""] i in
    List.iter (
	fun s -> Printf.printf "PARSING: %S %!" s;
		 compare s (handle_exception (parse_string main blank) s))
	      l;
  done

