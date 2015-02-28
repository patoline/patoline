open Test_kind
open Decap

let blank = no_blank

let _ = Printf.eprintf "basic tests ...%!"

let a = char 'a' 'a'
let b = char 'b' 'b'
let c = char 'c' 'c'
let d = char 'd' 'd'
let e = char 'e' 'e'
let f = char 'f' 'f'
let a' = apply (fun _ _ -> ()) a

let _ = parse_string a blank "a"
let _ = parse_string b blank "b"
let _ = parse_string c blank "c"

let abc = alternatives [a;b;c]
let abc' = apply (fun _ _ -> ()) abc

let _ = parse_string abc blank "a"
let _ = parse_string abc blank "b"
let _ = parse_string abc blank "c"

let astar = fixpoint () a'

let _ = parse_string astar blank ""
let _ = parse_string astar blank "a"
let _ = parse_string astar blank "aa"
let _ = parse_string astar blank "aaa"
let _ = parse_string astar blank "aaaa"
let _ = parse_string astar blank "aaaaa"

let aplus = fixpoint1 () a'

let _ = try parse_string aplus blank ""; assert false with Parse_error _ -> ()
let _ = parse_string aplus blank "a"
let _ = parse_string aplus blank "aa"
let _ = parse_string aplus blank "aaa"
let _ = parse_string aplus blank "aaaa"
let _ = parse_string aplus blank "aaaaa"

let abcstar = fixpoint () abc'

let _ = parse_string abcstar blank ""
let _ = parse_string abcstar blank "a"
let _ = parse_string abcstar blank "ac"
let _ = parse_string abcstar blank "aba"
let _ = parse_string abcstar blank "abca"
let _ = parse_string abcstar blank "acbab"
let _ = parse_string abcstar blank "abccba"

let abcplus = fixpoint1 () abc'

let _ = try parse_string abcplus blank ""; assert false with Parse_error _ -> ()
let _ = parse_string abcplus blank "a"
let _ = parse_string abcplus blank "ac"
let _ = parse_string abcplus blank "aba"
let _ = parse_string abcplus blank "abca"
let _ = parse_string abcplus blank "acbab"
let _ = parse_string abcplus blank "abccba"

let astar2 = declare_grammar "astar2"
let _ = set_grammar astar2 (alternatives [empty (); sequence a astar2 (fun _ _ -> ())])

let _ = parse_string astar2 blank ""
let _ = parse_string astar2 blank "a"
let _ = parse_string astar2 blank "aa"
let _ = parse_string astar2 blank "aaa"
let _ = parse_string astar2 blank "aaaa"
let _ = parse_string astar2 blank "aaaaa"

let astar3 = declare_grammar "astar3"
let _ = set_grammar astar3 (alternatives
			      [apply (fun _ -> ()) b;
			       dependent_sequence astar3 (fun _ -> apply (fun _ -> ()) a)])

let _ = parse_string astar3 blank "b"
let _ = parse_string astar3 blank "ba"
let _ = parse_string astar3 blank "baa"
let _ = parse_string astar3 blank "baaa"
let _ = parse_string astar3 blank "baaaa"
let _ = parse_string astar3 blank "baaaaa"


let debug' s g =
  sequence (debug s) g (fun () () -> ())

let astar3 = declare_grammar "astar3"
let _ = set_grammar astar3 (alternatives [empty (); sequence astar3 a (fun _ _ -> ())])

let _ = parse_string astar3 blank ""
let _ = parse_string astar3 blank "a"
let _ = parse_string astar3 blank "aa"
let _ = parse_string astar3 blank "aaa"
let _ = parse_string astar3 blank "aaaa"
let _ = parse_string astar3 blank "aaaaa"

let abo = sequence a (option '0' b) (fun _ _ -> ())

let _ = parse_string abo blank "a"
let _ = parse_string abo blank "ab"
let _ = try parse_string abo blank "b"; assert false with Parse_error _ -> ()

let aboc = sequence abo c (fun _ _ -> ())

let _ = parse_string aboc blank "ac"
let _ = parse_string aboc blank "abc"
let _ = try parse_string aboc blank "bc"; assert false with Parse_error _ -> ()

let abo' = apply (fun _ _ -> ()) abo

let _ = parse_string abo' blank "a" ()
let _ = parse_string abo' blank "ab" ()
let _ = try parse_string abo' blank "b" (); assert false with Parse_error _ -> ()

let abostar = fixpoint () (apply (fun _ _ -> ()) abo)

let _ = parse_string abostar blank ""
let _ = parse_string abostar blank "a"
let _ = parse_string abostar blank "ab"
let _ = parse_string abostar blank "aab"
let _ = parse_string abostar blank "aba"
let _ = parse_string abostar blank "abab"

let _ = Printf.eprintf "OK\n%!"
let _ = Printf.eprintf "two mutually recursive grammars test ...%!"

let mutrec2a = declare_grammar "mutrec2a"
let mutrec2b = declare_grammar "mutrec2b"
let snoc la c = c :: la
let _ = set_grammar mutrec2a
		    (cache (alternatives [
			 empty [];
			 sequence mutrec2a a snoc;
			 sequence mutrec2b c snoc]))
let _ = set_grammar mutrec2b
		    (cache (alternatives [
			 empty [];
			 sequence mutrec2b b snoc;
			 sequence mutrec2a d snoc]))

let (^^) s l = List.map (fun x -> s ^ x) l

let rec genmutrec2a suffix n =
  if n > 0 then
    genmutrec2a ("a"^^suffix) (n-1) @
      genmutrec2b ("c"^^suffix) (n-1)
  else suffix

and genmutrec2b suffix n =
  if n > 0 then
    genmutrec2b ("b"^^suffix) (n-1) @
      genmutrec2a ("d"^^suffix) (n-1)
  else suffix

let test gen parse m =
  let compare s l =
    let s' = String.create (List.length l) in
    let rec fn i = function
	[] -> ()
      | c::l' -> s'.[i] <- c; fn (i+1) l'
    in
    fn 0 (List.rev l);
    Printf.printf "-> %S\n%!" s';
    assert (s' = s)
  in

  for i = 0 to m do
    let l = gen [""] i in
    List.iter (
	fun s -> Printf.printf "PARSING: %S %!" s;
		 compare s (handle_exception (parse_string parse blank) s))
	      l;
  done

let _ = test genmutrec2a mutrec2a (test_cases (6, 8, 10))
let _ = Printf.eprintf "1%!"
let _ = test genmutrec2b mutrec2b (test_cases (6, 8, 10))
let _ = Printf.eprintf "2 OK\n%!"

let _ = Printf.eprintf "three mutually recursive grammars test ...%!"

let mutrec3a = declare_grammar "mutrec3a"
let mutrec3b = declare_grammar "mutrec3b"
let mutrec3c = declare_grammar "mutrec3c"

let _ = set_grammar mutrec3a
		    (cache (alternatives [
			 empty [];
			 sequence mutrec3b a snoc;
			 sequence mutrec3c b snoc]))
let _ = set_grammar mutrec3b
		   (cache  (alternatives [
			 empty [];
			 sequence mutrec3a c snoc;
			 sequence mutrec3c d snoc]))
let _ = set_grammar mutrec3c
		   (cache (alternatives [
			 empty [];
			 sequence mutrec3a e snoc;
			 sequence mutrec3b f snoc]))

let rec genmutrec3a suffix n =
  if n > 0 then
    genmutrec3b ("a"^^suffix) (n-1) @
      genmutrec3c ("b"^^suffix) (n-1)
  else suffix

and genmutrec3b suffix n =
  if n > 0 then
    genmutrec3a ("c"^^suffix) (n-1) @
      genmutrec3c ("d"^^suffix) (n-1)
  else suffix

and genmutrec3c suffix n =
  if n > 0 then
    genmutrec3a ("e"^^suffix) (n-1) @
      genmutrec3b ("f"^^suffix) (n-1)
  else suffix

let _ = test genmutrec3a mutrec3a (test_cases (5, 7, 9))
let _ = Printf.eprintf "1%!"
let _ = test genmutrec3b mutrec3b (test_cases (5, 7, 9))
let _ = Printf.eprintf "2%!"
let _ = test genmutrec3c mutrec3c (test_cases (5, 7, 9))
let _ = Printf.eprintf "3 OK\n%!"

let _ = Printf.eprintf "three mutually bi-recursive grammars test ...%!"

let mutbirec3a = declare_grammar "mutbirec3a"
let mutbirec3b = declare_grammar "mutbirec3b"
let mutbirec3c = declare_grammar "mutbirec3c"

let (@@) l1 l2 = l2 @ l1

let _ = set_grammar mutbirec3a
		    (cache (alternatives [
			 empty [];
			 sequence mutbirec3b (sequence mutbirec3c a snoc) (@@);
			 sequence mutbirec3c (sequence mutbirec3b b snoc) (@@)]))
let _ = set_grammar mutbirec3b
		    (cache (alternatives [
			 empty [];
			 sequence mutbirec3a (sequence mutbirec3c c snoc) (@@);
			 sequence mutbirec3c (sequence mutbirec3a d snoc) (@@)]))
let _ = set_grammar mutbirec3c
		    (cache (alternatives [
			 empty [];
			 sequence mutbirec3a (sequence mutbirec3b e snoc) (@@);
			 sequence mutbirec3b (sequence mutbirec3a f snoc) (@@)]))

let rec genmutbirec3a suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genmutbirec3b (genmutbirec3c ("a"^^suffix) i) j @
	       genmutbirec3c (genmutbirec3b ("b"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genmutbirec3b suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genmutbirec3a (genmutbirec3c ("c"^^suffix) i) j @
	       genmutbirec3c (genmutbirec3a ("d"^^suffix) i) j @ !res
    done;
    !res
  else suffix

and genmutbirec3c suffix n =
  if n > 0 then
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := genmutbirec3a (genmutbirec3b ("e"^^suffix) i) j @
	       genmutbirec3b (genmutbirec3a ("f"^^suffix) i) j @ !res
    done;
    !res
  else suffix

let _ = test genmutbirec3a mutbirec3a (test_cases (3, 4, 5))
let _ = Printf.eprintf "1%!"
let _ = test genmutbirec3b mutbirec3b (test_cases (3, 4, 5))
let _ = Printf.eprintf "2%!"
let _ = test genmutbirec3c mutbirec3c (test_cases (3, 4, 5))
let _ = Printf.eprintf "3 OK\n%!"

let gA = declare_grammar "gA" and gB = declare_grammar "gB"

let _ = set_grammar gA
  (alternatives [char 'x' ['x'];
		 sequence gB (sequence (char 'a' ()) gB (fun _ x -> x))
		   (fun x y -> y @ 'a' :: x )])

let _ = set_grammar gB
  (alternatives [gA;
		 sequence gA (sequence (char 'b' ()) gA (fun _ x -> x))
     (fun x y -> y @ 'b' :: x)])

let rec gengA suffix n =
  if n = 1 then "x" ^^ suffix else if n <= 0 then [] else
    let res = ref [] in
    for i = 0 to n - 1 do
      let j = n - 1 - i in
      res := gengB ("a"^^(gengB suffix i)) j @ !res
    done;
    !res

and gengB suffix n =
    let res = ref (gengA suffix n) in
    if n > 0 then
      for i = 0 to n - 1 do
	let j = n - 1 - i in
	res := gengA ("b"^^(gengA suffix i)) j @ !res
      done;
    !res
let _ = Printf.eprintf "two mutually recursive grammars that revealed bugs ...%!"

let _ = test gengA gA (test_cases (7, 11, 13))
let _ = Printf.eprintf "gA%!"
let _ = test gengB gB (test_cases (7, 11, 13))
let _ = Printf.eprintf "gB%!"
let _ = Printf.eprintf " OK%!\n"
