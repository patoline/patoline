open Decap
open Generate_calc

let _ = active_debug := false

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

let parser test =
  EMPTY | test 'a'

let cached parser expr prio : float grammar =
  | f:float_num                                               -> f
  | '(' e:(expr Sum) ')'                                      -> e
  | '-' e:(expr Pow)                        when prio <= Pow  -> -. e
  | '+' e:(expr Pow)                        when prio <= Pow  -> e
  | e:(expr Atom) "**" e':(expr Pow)        when prio <= Pow  -> e ** e'
  | e:(expr Prod) fn:prod_sym e':(expr Pow) when prio <= Prod -> fn e e'
  | e:(expr Sum) fn:sum_sym e':(expr Prod)  when prio <= Sum  -> fn e e'

(* The main loop *)
let _ = run (expr Sum)
