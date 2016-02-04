open Decap2
open Input

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
		 sequence (char '-' ()) bnfpow (fun _ x -> -x);
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
