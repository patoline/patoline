(* empty cases list *)
let f3 x = match x with

(* coercion sans parenthese *)
type t1 = A | B
let f38b x = match x:t1 with 
  | A -> B
  | B -> A
let f40 g x y = x:int + (g y:int):int

(* no parenthèses after of *)
type (-'a,+'b) blip = Blip of 'a -> 'b



