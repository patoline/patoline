(* empty cases list *)
let f3 x = match x with

(* coercion sans parenthese *)
type t1 = A | B
let f38b x = match x:t1 with 
  | A -> B
  | B -> A
let f40 g x y = (x:int) + (g y:int):int

(* no parenthèses after of *)
type (-'a,+'b) blip = Blip of 'a -> 'b

type x = A of int list

#ifdef TOTO
let toto = true
#else
let toto = false
#endif

#ifversion >= 4.01
let at_least_4_01 = true
#else
let at_least_4_01 = false
#endif

#ifversion >= 4.02
let at_least_4_02 = true
#else
let at_least_4_02 = false
#endif



