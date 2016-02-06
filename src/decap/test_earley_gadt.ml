open Decap2
open Input

let _ = debug_lvl := 0

let char0 : char -> 'a -> 'a symbol
  = fun c a ->
    let msg = Printf.sprintf "%C" c in
    let fn buf pos =
      let c', buf', pos' = read buf pos in
      if c = c' then (a,buf',pos') else expected msg
    in
    Term(fn)

let bnf1 = declare_grammar "As"
let _ = set_grammar bnf1 [Empty 0; next (char 'a' ()) (next bnf1 (Empty(fun n () -> n + 1)))]
let bnf10 = [next bnf1 (next (eof ()) (Empty(fun _ n -> n)))]
let fn n = Printf.eprintf "semantics : %d\n%!" n

let _ = fn (parse_string bnf10 no_blank "")
let _ = fn (parse_string bnf10 no_blank "a")
let _ = fn (parse_string bnf10 no_blank "aaa")
let _ = fn (parse_string bnf10 no_blank (String.make 50 'a'))
let _ = fn (parse_string bnf10 no_blank (String.make 1000 'a'))
let _ = fn (parse_string bnf10 no_blank (String.make 5000 'a'))
let _ = fn (parse_string bnf10 no_blank (String.make 10000 'a'))
let _ = fn (parse_string bnf10 no_blank (String.make 50000 'a'))
let _ = fn (parse_string bnf10 no_blank (String.make 100000 'a'))
let _ = fn (parse_string bnf10 no_blank (String.make 500000 'a'))

let bnf2 = declare_grammar "As"
let _ = set_grammar bnf2 [Empty 0; next bnf2 (next (char 'a' ())  (Empty(fun () n -> n + 1)))]
let bnf20 = [next bnf2 (next (eof ()) (Empty(fun _ n -> n)))]

let _ = fn (parse_string bnf20 no_blank "")
let _ = fn (parse_string bnf20 no_blank "a")
let _ = fn (parse_string bnf20 no_blank "aaa")
let _ = fn (parse_string bnf20 no_blank (String.make 50 'a'))
let _ = fn (parse_string bnf20 no_blank (String.make 1000 'a'))
let _ = fn (parse_string bnf20 no_blank (String.make 5000 'a'))
let _ = fn (parse_string bnf20 no_blank (String.make 10000 'a'))
let _ = fn (parse_string bnf20 no_blank (String.make 50000 'a'))
let _ = fn (parse_string bnf20 no_blank (String.make 100000 'a'))
let _ = fn (parse_string bnf20 no_blank (String.make 500000 'a'))

let g = fun y x -> 10 * x + y

let bnfnum = regexp "[0-9]+" (fun f -> int_of_string (f 0))

let bnfsum = declare_grammar "sum"
let bnfprod = declare_grammar "prod"
let bnfpow = declare_grammar "pow"
let bnfatom = declare_grammar "atom"

let _ = set_grammar bnfsum
  (alternatives [bnfprod;
		sequence3 bnfsum (char '+' ()) bnfprod (fun x _ y -> x + y);
		sequence3 bnfsum (char '-' ()) bnfprod (fun x _ y -> x - y)])

let _ = set_grammar bnfprod
  (alternatives [bnfpow;
		sequence3 bnfprod (char '*' ()) bnfpow (fun x _ y -> x * y);
		sequence3 bnfprod (char '/' ()) bnfpow (fun x _ y -> x / y)])

let rec ( ** ) a b =
  if b <= 0 then 1 else if b = 1 then a else
  let c = a ** (b/2) in
  let d = c * c in
  if b mod 2 = 0 then d else a * d

let _ = set_grammar bnfpow
  (alternatives [bnfatom;
		 sequence (ignore_next_blank (char '-' ())) bnfpow (fun _ x -> -x);
		 sequence3 bnfatom (string "**" ()) bnfpow (fun x _ y -> x ** y)])

let _ = set_grammar bnfatom
  (alternatives [bnfnum;
		 sequence3 (char '(' ()) bnfsum (char ')' ()) (fun _ x _ -> x)])

let blank = blank_regexp "[ \n\t\r]*"

let top = sequence bnfsum (eof ()) (fun x _ -> x)

let _ = fn (parse_string top blank "124")
let _ = fn (parse_string top blank "123 + 456")
let _ = fn (parse_string top blank "123 + 456 * 2")
let _ = fn (parse_string top blank "(123 + 456) * 2")
let _ = fn (parse_string top blank "2 ** 2 ** 2")
let _ = fn (parse_string top blank "2 ** 2 ** -2")
let _ = Printf.printf "Two tests with parse error:\n%!"
let _ = try handle_exception (fun () -> fn (parse_string top blank "2 ** 2a ** 2")) () with _ -> ()
let _ = try handle_exception (fun () -> fn (parse_string top blank "2 ** 2 ** - 2")) () with _ -> ()

let rec bbcc n =
  if n = 0 then empty 0
    else sequence3 (char 'b' ()) (bbcc (n-1)) (char 'c' ()) (fun _ _ _ -> n)
let aabbcc =
  dependent_sequence bnf2 bbcc

let _ = fn (parse_string aabbcc blank "aabbcc")
let _ = fn (parse_string aabbcc blank "aaaaabbbbbccccc")

let rec bb n =
  if n = 0 then empty 0
  else sequence (bb (n-1)) (char 'b' ()) (fun _ _ -> n)

let rec cc n =
  if n = 0 then empty 0
  else sequence (cc (n-1)) (char 'c' ()) (fun _ _ -> n)

let aabbcc2 =
  dependent_sequence bnf2 (fun n -> sequence (bb n) (cc n) (fun _ n -> n))

let _ = fn (parse_string aabbcc2 blank "aabbcc")
let _ = fn (parse_string aabbcc2 blank "aaaaabbbbbccccc")

let word = regexp "[a-z]+" (fun f n -> n+1)

let blank1 = blank_regexp "[ \t]*"

let paragraph = change_layout (revfixpoint1 0 word) blank1

let text = revfixpoint [] (apply (fun a l -> a::l) paragraph)

let gn l = List.iter (fun n -> Printf.printf "%d " n) l; Printf.printf "\n%!"

let _ = fn (parse_string paragraph blank "aa aa abbbbb ccccc")
let _ = Printf.printf "One test with parse error:\n%!"
let _ = try handle_exception (fun () -> fn (parse_string paragraph blank "aa aa\nabbbbb ccccc")) () with _ -> ()
let _ = gn (parse_string text blank "aa aa abbbbb ccccc")
let _ = gn (parse_string text blank "aa aa abbbbb ccccc\nzeg zeg  zge gzgz zeg zg z\n zfez\n\n zf  zgf ze")

let long n =
  let s = String.create (2*n*n) in
  for i = 0 to n*n-1 do
    s.[2*i] <- 'a';
    s.[2*i+1] <- if (i+1) mod n = 0 then '\n' else ' '
  done;
  s

let _ = gn (parse_string text blank (long 10))
let _ = gn (parse_string text blank (long 100))
let _ = gn (parse_string text blank (long 1000))

let file =
  let name = "/tmp/large.txt" in
  let ch = open_out name in
  output_string ch (long 1000);
  close_out ch;
  let ch = open_in name in
  gn (parse_channel text blank ch);
  close_in ch;
  Sys.remove name
