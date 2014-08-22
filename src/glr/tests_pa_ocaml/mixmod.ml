(* $Id: mixmod.ml.txt,v 1.1 2005/02/24 01:45:32 garrigue Exp $ *)

(* Basic interfaces *)

(* The types involved in our recursion *)
module type ET = sig type exp end
(* The recursive operations on our our types *)
module type E =
  sig
    module T : ET
    val eval : (string * T.exp) list -> T.exp -> T.exp
  end
(* A functor building the type of the conversion module between two types *)
module CW(T : ET)(L : ET) = struct
  module type C = sig
    val inj : L.exp -> T.exp
    val proj : T.exp -> L.exp option
  end
end
(* The identity conversion module *)
module CF = struct
  let inj x = x
  let proj x = Some x
end

(* Variables are common to lambda and expr *)

module VarT = struct
  type exp = [`Var of string]
end
module Var(E : E)(C : CW(E.T)(VarT).C) =
  struct
    module T = VarT
    let eval sub (`Var s as v : T.exp) =
      try List.assoc s sub with Not_found -> C.inj v
  end

(* The lambda language: free variables, substitutions, and evaluation *)
let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

module LamT(T : ET) = struct
  type exp = [VarT.exp | `Abs of string * T.exp | `App of T.exp * T.exp]
end
module Lam(E : E)(C : CW(E.T)(LamT(E.T)).C) =
  struct
    module T = LamT(E.T)
    module CVar = struct
      type t = VarT.exp
      (* All conversion modules use the same code, but we need to
         know the concrete types to build our coercions *)
      let inj = (C.inj :> t -> _)
      let proj x = match C.proj x with
        Some #t as y -> y
      | _ -> None
    end
    module LVar = Var(E)(CVar)

    let eval subst : T.exp -> E.T.exp = function
        #CVar.t as v -> LVar.eval subst v
      | `App(l1, l2) ->
          let l2' = E.eval subst l2 in
          let l1' = E.eval subst l1 in
          begin match C.proj l1' with
            Some (`Abs (s, body)) ->
              E.eval [s,l2'] body
          | _ ->
              C.inj (`App (l1', l2'))
          end
      | `Abs(s, l1) ->
          let s' = gensym () in
          C.inj (`Abs(s', E.eval ((s,C.inj (`Var s'))::subst) l1))
  end

module rec LamF : E with module T = LamT(LamF.T) = Lam(LamF)(CF)
let e1 = LamF.eval [] (`App(`Abs("x",`Var"x"), `Var"y"));;

(* The expr language of arithmetic expressions *)

module ExprT(T : ET) = struct
  type exp =
      [ `Var of string | `Num of int
      | `Add of T.exp * T.exp | `Mult of T.exp * T.exp]
end

module Expr(E : E)(C : CW(E.T)(ExprT(E.T)).C) =
  struct
    module T = ExprT(E.T)
    module CVar = struct
      type t = VarT.exp
      let inj = (C.inj :> t -> _)
      let proj x = match C.proj x with
        Some #t as y -> y
      | _ -> None
    end
    module LVar = Var(E)(CVar)

    let map f : T.exp -> T.exp = function
        #CVar.t | `Num _ as e -> e
      | `Add(e1, e2) -> `Add (f e1, f e2)
      | `Mult(e1, e2) -> `Mult (f e1, f e2)

    let eval subst (e : T.exp) =
      let e' = map (E.eval subst) e in
      let e'' = C.inj e' in
      match e' with
        #CVar.t as v -> LVar.eval subst v
      | `Add(e1, e2) ->
          begin match C.proj e1, C.proj e2 with
            Some(`Num m), Some (`Num n) -> C.inj (`Num (m+n))
          | _ -> e''
          end
      | `Mult(e1, e2) ->
          begin match C.proj e1, C.proj e2 with
            Some(`Num m), Some (`Num n) -> C.inj (`Num (m*n))
          | _ -> e''
          end
      | _ -> e''
  end

module rec ExprF : E with module T = ExprT(ExprF.T) = Expr(ExprF)(CF)
let e2 = ExprF.eval [] (`Add(`Mult(`Num 3, `Num 2), `Var"x"));;

(* The lexpr language, reunion of lambda and expr *)

module LExprT(T : ET) = struct
  type exp = [ LamT(T).exp | ExprT(T).exp ]
end
module LExpr(E : E)(C : CW(E.T)(LExprT(E.T)).C) =
  struct
    module T = LExprT(E.T)
    module CLam = struct
      type t = LamT(E.T).exp
      let inj = (C.inj :> t -> _)
      let proj x = match C.proj x with
        Some #t as y -> y
      | _ -> None
    end
    module SLam = Lam(E)(CLam)
    module CExpr = struct
      type t = ExprT(E.T).exp
      let inj = (C.inj :> t -> _)
      let proj x = match C.proj x with
        Some #t as y -> y
      | _ -> None
    end
    module SExpr = Expr(E)(CExpr)

    let eval subst = function
        #CLam.t as x -> SLam.eval subst x
      | #CExpr.t as x -> SExpr.eval subst x
  end

module rec LExprF : E with module T = LExprT(LExprF.T) = LExpr(LExprF)(CF)
let e3 =
  LExprF.eval [] (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5))
