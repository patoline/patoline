let readInt f n0=
  let rec readInt_ n x=
    if n=n0 then x else
      readInt_ (n+1) ((x lsl 8) + (input_byte f))
  in
    readInt_ 0 0
      

let round x=
  let c=ceil x in
    if (c-.x) < 0.5 && (c-.x)> -0.5 then int_of_float c else int_of_float (floor x)
      
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
          
module IntMap=Map.Make (struct type t=int let compare=compare end)
module StrMap=Map.Make (String)
module IntSet=Set.Make (struct type t=int let compare=compare end)
