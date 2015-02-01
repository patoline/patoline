open Decap

let _ = active_debug := true

let parser a : char list grammar =
  | DEBUG"a1" EMPTY DEBUG"a1'" -> []
  | DEBUG"a2" l:a DEBUG"a2'" 'a' DEBUG"a2''" -> 'a'::l
  | DEBUG"a3" l:b DEBUG"a3'" 'c' DEBUG"a3''" -> 'c'::l

and b : char list grammar =
  | DEBUG"b1" EMPTY DEBUG"b1'" -> []
  | DEBUG"b2" l:b DEBUG"b2'" 'b' DEBUG"b2''" -> 'b'::l
  | DEBUG"b3" l:a DEBUG"b3'" 'd' DEBUG"b3''" -> 'd'::l

let compare s l =
  let s' = String.create (List.length l) in
  let rec fn i = function
      [] -> ()
    | c::l' -> s'.[i] <- c; fn (i+1) l'
  in
  fn 0 (List.rev l);
  Printf.printf "-> %S\n%!" s';
  assert (s' = s)


let (^^) s l = List.map (fun x -> s ^ x) l

let rec gena suffix n =
  if n > 0 then
    gena ("a"^^suffix) (n-1) @
      genb ("c"^^suffix) (n-1)
  else suffix

and genb suffix n =
  if n > 0 then
    genb ("b"^^suffix) (n-1) @
      gena ("d"^^suffix) (n-1)
  else suffix

let parser main = l:a

(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
  let n = int_of_string (Sys.argv.(1)) in
  for i = max (-n) 0 to abs n do
    let l = gena [""] i in
    List.iter (
	fun s -> Printf.eprintf "PARSING: %S %!" s;
		 compare s (handle_exception (parse_string main blank) s))
	      l;
  done
