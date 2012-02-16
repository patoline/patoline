open CamomileLibrary

let readInt f n0=
  let rec readInt_ n x=
    if n=n0 then x else
      readInt_ (n+1) ((x lsl 8) + (input_byte f))
  in
    readInt_ 0 0

let buf=String.create 4

let readInt2 f=
  really_input f buf 0 2;
  let d=((int_of_char buf.[0]) lsl 8) lor (int_of_char buf.[1]) in
    d

let readInt4 f=
  really_input f buf 0 4;
  let a=(int_of_char buf.[0]) lsl 8 in
  let b=(a lor (int_of_char buf.[1])) lsl 8 in
  let c=(b lor (int_of_char buf.[2])) lsl 8 in
  let d=c lor (int_of_char buf.[3]) in
    d


let int16 x=if x<=0x7f then x else x-0x10000

let round x=
  let c=ceil x in
    if (c-.x) < 0.5 && (c-.x)> -0.5 then int_of_float c else
      if c-.x=0.5 || c-.x=(-0.5) then
        if int_of_float (floor x) mod 2=0 then int_of_float (floor x) else int_of_float c
      else
        int_of_float (floor x)

let round_float x=
  let c=ceil x in
    if (c-.x) < 0.5 && (c-.x)> -0.5 then c else (floor x)

let is_infinite x=match classify_float x with FP_infinite->true | _->false

let rec span f=function
    []->([],[])
  | h::s when f h->let (a,b)=span f s in (h::a, b)
  | l->([],l)

let rec break f=function
    []->([],[])
  | h::s when not (f h)->let (a,b)=break f s in (h::a, b)
  | l->([],l)

let rec take x l=
  if x<=0 then [] else
    match l with
        []->[]
      | h::s->h::(take (x-1) s)

let rec drop x l=
  if x<=0 then l else
    match l with
        []->[]
      | _::s->drop (x-1) l

let concat_utf8 a b=
  let x=UTF8.Buf.create (UTF8.length a+UTF8.length b) in
    UTF8.Buf.add_string x a;
    UTF8.Buf.add_string x b;
    UTF8.Buf.contents


module IntMap=New_map.Make (struct type t=int let compare=compare end)
module StrMap=New_map.Make (String)
module IntSet=Set.Make (struct type t=int let compare=compare end)
module IntervalMap=New_map.Make (struct type t=int*int let compare=compare end)
