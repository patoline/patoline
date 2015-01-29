
open Decap

let _ = active_debug := false

let parser a : char list grammar =
  DEBUG"a1" EMPTY DEBUG"a1'" -> [] | DEBUG"a2" l:a l':b DEBUG"a2'" 'a' DEBUG"a2''" -> 'a'::(l' @ l)

and b : char list grammar =
  DEBUG"a1" EMPTY DEBUG"a1'" -> [] | DEBUG"a2" l:b l':a DEBUG"a2'" 'b' DEBUG"a2''" -> 'b'::(l' @ l)


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
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := gena (genb ("a"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genb suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genb (gena ("b"^^suffix) i) j @ !res
    done;
    !res
  else suffix
	 
let parser main = l:a
			   
(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
  let s = "a" in
  Printf.eprintf "PARSING: %S\n%!" s;
  compare s (handle_exception (parse_string main blank) s);
  for i = 0 to int_of_string (Sys.argv.(1)) do
    let l = gena [""] i in
    List.iter (
	fun s -> Printf.eprintf "PARSING: %S\n%!" s;
		 compare s (handle_exception (parse_string main blank) s))
	      l;
  done

