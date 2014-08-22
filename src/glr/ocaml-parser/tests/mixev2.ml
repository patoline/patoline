(* $Id: mixev2.ml.txt,v 1.1 2005/02/24 01:45:32 garrigue Exp $ *)

(* Use sums rather than polymorphic variants *)
(* ocaml -rectypes mixev2.ml *)

type var = string

let eval_var wrap subst (s : var) =
  try List.assoc s subst with Not_found -> wrap s

type 'a lambda = VarL of var | Abs of string * 'a | App of 'a * 'a

let eval_lambda eval_rec wrap unwrap subst (l : 'a lambda) : 'a =
  match l with
    VarL v -> eval_var (fun x -> wrap (VarL x)) subst v
  | App(l1, l2) ->
      let l1' = eval_rec subst l1 and l2' = eval_rec subst l2 in
      begin match unwrap l1' with
        Some(Abs(s, body)) ->
          eval_rec ((s,l2')::subst) body
      | _ ->
          wrap (App (l1', l2'))
      end
  | Abs(s, l1) ->
      wrap (Abs(s, eval_rec (List.remove_assoc s subst) l1))

let id x = x
let some x = Some x
let rec eval1 subst = eval_lambda eval1 id some subst

type 'a expr = VarE of var | Num of int | Add of 'a * 'a | Mult of 'a * 'a

let map_expr f = function
  | Add(e1, e2) -> Add (f e1, f e2)
  | Mult(e1, e2) -> Mult (f e1, f e2)
  | VarE _ | Num _ as e -> e

let eval_expr eval_rec wrap unwrap subst e =
  let e' = map_expr (eval_rec subst) e in
  match map_expr unwrap e' with
    VarE v -> eval_var (fun x -> wrap (VarE x)) subst v
  | Add(Some(Num m), Some(Num n)) -> wrap (Num (m+n))
  | Mult(Some(Num m), Some(Num n)) -> wrap (Num (m*n))
  | _ -> wrap e'

let rec eval2 subst = eval_expr eval2 id some subst

type 'a lexpr = Lambda of 'a lambda | Expr of 'a expr

let eval_lexpr eval_rec wrap unwrap subst e =
  match e with
    Lambda l ->
      eval_lambda eval_rec (fun x -> wrap (Lambda x))
        (fun x -> match unwrap x with Some(Lambda x) -> Some x
                  | _ -> None)
        subst l
  | Expr e -> 
      eval_expr eval_rec (fun x -> Expr x)
        (fun x -> match unwrap x with Some(Expr x) -> Some x
                  | _ -> None)
        subst e

let rec eval3 subst = eval_lexpr eval3 id some subst
;;

(* A few examples *)
eval1 [] (App(Abs("x",VarL"x"), VarL"y"));;
eval2 [] (Add(Mult(Num 3, Num 2), VarE"x"));;
eval3 [] (Expr(Add(Lambda(App(Lambda(Abs("x",Expr(Mult(Expr(VarE"x"),Expr(VarE"x"))))),Expr(Num 2))), Expr(Num 5))));;

