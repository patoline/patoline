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

let parser expr =
  | f:float_num -> (Atom,f)
  | '(' (_,e):expr ')'  -> Atom,e
  | '-' (p,e):expr -> if p < Pow then give_up ""; Pow, -. e
  | '+' (p,e):expr -> if p < Pow then give_up ""; Pow, e
  | (conditional_sequence expr (fun (p,e) -> p > Pow) (parser "**" expr) (fun (p, e) (p', e') ->
					      if p' < Pow then give_up ""; Pow, e ** e'))
  | (conditional_sequence expr (fun (p,e) -> p >= Prod) (parser prod_sym expr) (fun (p, e) (fn,(p', e')) ->
					      if p' <= Prod then give_up ""; Pow, fn e e'))
  | (conditional_sequence expr (fun (p,e) -> p >= Sum) (parser sum_sym expr) (fun (p, e) (fn,(p', e')) ->
					      if p' <= Sum then give_up ""; Pow, fn e e'))

(* The main loop *)
let _ = run (apply snd expr)
