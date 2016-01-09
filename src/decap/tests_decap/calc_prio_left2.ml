open Decap
open Generate_calc

type calc_prio = Sum | Prod | Pow | Atom

let float_re = ''[0-9]+\([.][0-9]+\)?\([eE][-+]?[0-9]+\)?''
let float_num = parser
  f:RE(float_re) -> float_of_string f

let prod_sym = parser
  | '*' -> ( *. )
  | '/' -> ( /. )

let sum_sym = parser
  | '+' -> ( +. )
  | '-' -> ( -. )

let cached parser expr =
  | f:float_num -> (Atom,f)
  | '(' (_,e):expr ')' -> Atom,e
  | '-' (p,e):expr -> if p < Pow then give_up ""; Pow, -. e
  | '+' (p,e):expr -> if p < Pow then give_up ""; Pow, e
  | (p,e):expr ->>
	    { "**" (p',e'):expr when p > Pow ->
		   if p' < Pow then give_up ""; Pow, e ** e'
            | fn:prod_sym (p',e'):expr when p >= Prod ->
	  	   if p' <= Prod then give_up ""; Prod, fn e e'
            | fn:sum_sym (p',e'):expr when p >= Sum ->
		   if p' <= Sum then give_up ""; Sum, fn e e' }

(* The main loop *)
let _ = run (apply snd expr)
