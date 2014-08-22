(* $Id: mixin2.ml.txt,v 1.1 2005/02/24 01:45:32 garrigue Exp $ *)

(* Full fledge version, using objects to structure code *)

open StdLabels
open MoreLabels

(* Use maps for substitutions and sets for free variables *)

module Subst = Map.Make(struct type t = string let compare = compare end)
module Names = Set.Make(struct type t = string let compare = compare end)

(* To build recursive objects *)

let lazy_fix make =
  let rec obj = lazy (make obj) in
  Lazy.force obj

let (!!) = Lazy.force

(* The basic operations *)

class type ['a, 'b] ops =
  object
    method free : 'b -> Names.t
    method subst : sub:'a Subst.t -> 'b -> 'a
    method eval : 'b -> 'a
  end

(* utility: define methods of a lazy object as fields (must eta-expand) *)
class ['a] lazy_ops (ops : ('a,'a) #ops Lazy.t) =
  object
    val free = fun x -> !!ops#free x
    val subst = fun ~sub -> !!ops#subst ~sub
    val eval = fun x -> !!ops#eval x
  end

(* Variables are common to lambda and expr *)

type var = [`Var of string]

class ['a] var_ops = object (self : ('a, var) #ops)
  constraint 'a = [> var]
  method subst ~sub (`Var s as x) =
    try Subst.find s sub with Not_found -> x
  method free (`Var s) =
    Names.singleton s
  method eval (#var as v) = v
end

(* The lambda language: free variables, substitutions, and evaluation *)

type 'a lambda = [`Var of string | `Abs of string * 'a | `App of 'a * 'a]

let next_id =
  let current = ref 3 in
  fun () -> incr current; !current

class ['a] lambda_ops ops =
  object (self : ('a, 'a lambda) #ops)
    constraint 'a = [> 'a lambda]
    val var : 'a var_ops = new var_ops
    inherit ['a] lazy_ops ops

    method free = function
        #var as x -> var#free x
      | `Abs (s, t) -> Names.remove s (free t)
      | `App (t1, t2) -> Names.union (free t1) (free t2)

    method map ~f = function
        #var as x -> x
      | `Abs (s, t) -> `Abs(s, f t)
      | `App (t1, t2) -> `App (f t1, f t2)

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | `Abs(s, t) as l ->
          let used = free t in
          let used_expr =
            Subst.fold sub ~init:[]
              ~f:(fun ~key ~data acc ->
                if Names.mem s used then data::acc else acc) in
          if List.exists used_expr ~f:(fun t -> Names.mem s (free t)) then
            let name = s ^ string_of_int (next_id ()) in
            `Abs(name,
                 subst ~sub:(Subst.add ~key:s ~data:(`Var name) sub) t)
          else
            self#map ~f:(subst ~sub:(Subst.remove s sub)) l
      | `App _ as l ->
          self#map ~f:(subst ~sub) l

    method eval l =
      match self#map ~f:eval l with
        `App(`Abs(s,t1), t2) ->
          eval (subst ~sub:(Subst.add ~key:s ~data:t2 Subst.empty) t1)
      | t -> t
  end

(* Operations specialized to lambda *)

let lambda = lazy_fix (new lambda_ops)

(* The expr language of arithmetic expressions *)

type 'a expr =
    [ `Var of string | `Num of int | `Add of 'a * 'a
    | `Neg of 'a | `Mult of 'a * 'a]

class ['a] expr_ops ops =
  object (self : ('a, 'a expr) #ops)
    constraint 'a = [> 'a expr]
    val var : 'a var_ops = new var_ops
    inherit ['a] lazy_ops ops

    method free = function
        #var as x -> var#free x
      | `Num _ -> Names.empty
      | `Add(x, y) -> Names.union (free x) (free y)
      | `Neg x -> free x
      | `Mult(x, y) -> Names.union (free x) (free y)

    method map ~f = function
        #var as x -> x
      | `Num _ as x -> x
      | `Add(x, y) -> `Add(f x, f y)
      | `Neg x -> `Neg(f x)
      | `Mult(x, y) -> `Mult(f x, f y)

    method subst ~sub = function
        #var as x -> var#subst ~sub x
      | #expr as e -> self#map ~f:(subst ~sub) e

    method eval (#expr as e) =
      match self#map ~f:eval e with
        `Add(`Num m, `Num n) -> `Num (m+n)
      | `Neg(`Num n) -> `Num (-n)
      | `Mult(`Num m, `Num n) -> `Num (m*n)
      | e -> e
  end

(* Specialized versions *)

let expr = lazy_fix (new expr_ops)

(* The lexpr language, reunion of lambda and expr *)

type 'a lexpr = [ 'a lambda | 'a expr ]

class ['a] lexpr_ops (ops : ('a,'a) #ops Lazy.t) =
  let lambda = new lambda_ops ops in
  let expr = new expr_ops ops in
  object (self : ('a, 'a lexpr) #ops)
    constraint 'a = [> 'a lexpr]
    method free = function
        #lambda as x -> lambda#free x
      | #expr as x -> expr#free x

    method subst ~sub = function
        #lambda as x -> lambda#subst ~sub x
      | #expr as x -> expr#subst ~sub x

    method eval = function
        #lambda as x -> lambda#eval x
      | #expr as x -> expr#eval x
  end

let lexpr = lazy_fix (new lexpr_ops)

(* A few examples:
lambda#eval (`App(`Abs("x",`Var"x"), `Var"y"));;
expr#eval (`Add(`Mult(`Num 3,`Neg(`Num 2)), `Var"x"));;
lexpr#eval (`Add(`App(`Abs("x",`Mult(`Var"x",`Var"x")),`Num 2), `Num 5));;
*)
