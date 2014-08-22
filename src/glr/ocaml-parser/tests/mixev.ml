(* $Id: mixev3.04.ml.txt,v 1.1 2005/02/24 01:45:32 garrigue Exp $ *)

(* Variables are common to lambda and expr *)

type var = [`Var of string]

let eval_var sub (`Var s as v : var) =
  try List.assoc s sub with Not_found -> v

(* The lambda language: free variables, substitutions, and evaluation *)

type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a]

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

let eval_lambda eval_rec subst : 'a lambda -> [> 'a lambda] = function
    #var as v -> eval_var subst v
  | `App(l1, l2) ->
      let l2' = eval_rec subst l2 in
      begin match eval_rec subst l1 with
        `Abs(s, body) ->
          eval_rec [s,l2'] body
      | l1' ->
          `App (l1', l2')
      end
  | `Abs(s, l1) ->
      let s' = gensym () in
      `Abs(s', eval_rec ((s,`Var s')::subst) l1)

let rec eval1 subst = eval_lambda eval1 subst

(* The expr language of arithmetic expressions *)

type 'a expr =
    [`Var of string | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a]

let map_expr f : 'a expr -> [> 'a expr] = function
    #var | `Num _ as e -> e
  | `Add(e1, e2) -> `Add (f e1, f e2)
  | `Mult(e1, e2) -> `Mult (f e1, f e2)

let eval_expr eval_rec subst (e : 'a expr) : 'a =
  match map_expr (eval_rec subst) e with
    #var as v -> eval_var subst v
  | `Add(`Num m, `Num n) -> `Num (m+n)
  | `Mult(`Num m, `Num n) -> `Num (m*n)
  | e -> e

let rec eval2 subst = eval_expr eval2 subst

(* The lexpr language, reunion of lambda and expr *)

type 'a lexpr = [ 'a lambda | 'a expr]

let eval_lexpr eval_rec subst : 'a lexpr -> [> 'a lexpr] = function
    #lambda as x -> eval_lambda eval_rec subst x
  | #expr as x -> eval_expr eval_rec subst x

let rec eval3 subst = eval_lexpr eval3 subst

(* A few examples:
eval1 [] (`App(`Abs("x",`Var"x"), `Var"y"));;
eval2 [] (`Add(`Mult(`Num 3, `Num 2), `Var"x"));;
eval3 [] (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5));;
*)
