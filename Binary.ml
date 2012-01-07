let readInt f n0=
  let rec readInt_ n x=
    if n=n0 then x else
      readInt_ (n+1) ((x lsl 8) + (input_byte f))
  in
    readInt_ 0 0
      

module IntMap=Map.Make (struct type t=int let compare=compare end)
module StrMap=Map.Make (String)
module IntSet=Set.Make (struct type t=int let compare=compare end)
