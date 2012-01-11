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

module IntMap=Map.Make (struct type t=int let compare=compare end)
module StrMap=Map.Make (String)
module IntSet=Set.Make (struct type t=int let compare=compare end)
