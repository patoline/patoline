(* bcbcbacaabbc  = aafdea
   aafdea
   A
    A
    -C
     -B
   -  -C
       -A
   *)
open Decap

let _ = active_debug := false

let parser a : char list grammar =
  | DEBUG"a0" EMPTY -> []
  | DEBUG"bc1" lb:b DEBUG"bc2" lc:c DEBUG"bc3" "a" DEBUG"bc4" -> lb@lc@['a']
  | DEBUG"cb1" lc:c DEBUG"cb2" lb:b DEBUG"cb3" "b" DEBUG"cb4" -> lc@lb@['b']

and b : char list grammar =
  | DEBUG"b0" EMPTY -> []
  | DEBUG"ac1" la:a DEBUG"ac2" lc:c DEBUG"ac3" "c" DEBUG"ac4" -> la@lc@['c']
  | DEBUG"ca1" lc:c DEBUG"ca2" la:a DEBUG"ca3" "d" DEBUG"ca4" -> lc@la@['d']

and c : char list grammar =
  | DEBUG"c0" EMPTY -> []
  | DEBUG"ab1" la:a DEBUG"ab2" lb:b DEBUG"ab3" "e" DEBUG"ab4" -> la@lb@['e']
  | DEBUG"ba1" lb:b DEBUG"ba2" la:a DEBUG"ba3" "f" DEBUG"ba4" -> lb@la@['f']

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
      res := genb (genc ("a"^^suffix) i) j @
	       genc (genb ("b"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genb suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := gena (genc ("c"^^suffix) i) j @
	       genc (gena ("d"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genc suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := gena (genb ("e"^^suffix) i) j @
	       genb (gena ("f"^^suffix) i) j @ !res
    done;
    !res
  else suffix


let parser main = a

(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
  let n = int_of_string (Sys.argv.(1)) in
  for i = max 0 (-n) to abs n do
    let l = gena [""] i in
    List.iter (
	fun s -> Printf.printf "PARSING: %S %!" s;
		 compare s (handle_exception (parse_string main blank) s))
	      l;
  done
