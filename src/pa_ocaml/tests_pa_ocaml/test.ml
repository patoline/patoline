open List

let xy = 3
let x = 10.0
let f x = x
let g x y z =
  if x 
  then y
  else z

let app x y = x y

let f0 = fun x y z -> x y z

let f0'_y_ _x_= _x_

let f1 = function
    1 -> 0
  | 2 -> 1
  | x -> x

let f2 x = match x with
| 1 -> 0
| 2 -> 1
| x -> x

(*let f3 x = match x with*)

let f4 g x = try g x with _ -> f1 x

let f5 x y z = x y, y z

let f6 x y = x lsl y

let f7 x y = x * x lsl 2 + y * x lsr x

let rec fact n = if n <= 0 then 1 else n * fact (n - 1)

let rec f8 a b = if a = 0 then 1 else f8 (a - 1) b + g8 a b
  and g8 a b = if b = 0 then 1 else g8 a (b - 1) + f8 a b

let f9 ~a ~b:b' ?c:(c'=0) ?(d=0) x = a + b' + c' + d + x
let _ = f9 ~a:2 ~b:3 ~c:4 5
let _ = f9 ~a:2 ~b:3 ~d:4 5

let f10 x = None, Some x

let f11 x = (x, x, x)

let f12 = (100_000, 100_000l, 100_000L, 100_000n)
let f12_hex = (0x100_000, 0x100_000l, 0x100_000L, 0x100_000n)
let f12_oct = (0o100_000, 0o100_000l, 0o100_000L, 0o100_000n)
let f12_bin = (0b100_000, 0b100_000l, 0b100_000L, 0b100_000n)

let f13 = true, false, ()

let f14 = (`A, `a, `a_B, `C true, `D (true, 42))

let f15 x y = x::y 

let f16 x = if x > 2 then let y = x + 3 in x + y else x - 1

(* Ne parse qu'avec l'option --ext *)
let f17 = 2 * let x = 3 in x + 2

let f18 x y z = x ; y ; z

(* Ne parse qu'avec l'option --ext *)
let f19 x f z = if x >= z then let y = x + z in f z ; f x

let f20 = [ ], [ 1 ], [ 1; 2; 3; 4], [ 1; 2; 3; 4; ]

let f21 = let vide = [| |] in [| 1 |], [| 1; 2; 3; 4|], [| 1; 2; 3; 4; |]

let f22 = { contents = 1 }

let f23 = { f22 with contents = 2 ; }

let f24 = let contents = 3 in { contents }

let f25 = "toto"

let f26 = let s = f25 in s.[0] <- 'x'; s.[1]

let f27 = [|1;2|]

let f28 = f27.(0) <- f27.(1) + 1

let f29 = [|[|1;2|];[|1;2|]|]

let f30 = f29.(0).(0) <- f29.(1).(1) + 1

let f31 = String.blit

let f32 = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 2
let _ = f32.{0} <- 1.0 ; f32.{1} <- 2.

let f33 = Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout 2 2
let _ = f33.{0,0} <- 1.0 ; f33.{0,1} <- 2.0

let f34 x = -x, -. (float_of_int x), -x + - x * -x - - x - x  

let f35 y = !y

let f36 n = let n = ref n and r = ref 1 in while !n > 1 do r := !r * !n; decr n done; !r

let f37 n = let r = ref 1 in for i = 2 to n do r := !r * n done; !r

type t1 = A | B

let f38 x = match x with A -> B | B -> A
let f38b x = match (x:t1) with 
  | A -> B
  | B -> A

let f39 = function A -> B | B -> A

let f40 g x y = (x:int) + ((g (y:int)):int)

let f41 = "toto", "ta
  ta", "ti\
        ti"

class type int_cl = object val x : int method get : int end

module M1 = struct
  type t = int
  let compare = (-)
end


module M2 = Set.Make (M1)
module M3 = Map.Make (M1)

external idt : 'a -> 'a = "%idt"

module type MT1 = 
  sig
    val f : int -> int
    external idt : 'a -> 'a = "%idt"
    type t 
    type u = A | B
    module type A = sig val g : float -> float end
    module M : A
    module F (A:A) : A
    class type int = object val x : int method get : int end
  end

module M4 : MT1 =
  struct
    let f x = x + 1
    external idt : 'a -> 'a = "%idt"
    type t = int
    type u = A | B
    module type A = sig val g : float -> float end
    module M = struct let g x = x +. 1. end
    module F (A:A) = struct open A let g x = g (g x) end
    class type int = object val x : int method get : int end
  end 

exception Invalid_arg of string * string
exception Invalid_arg2 = Invalid_arg

exception Exit = Pervasives.Exit
exception NF = Not_found

type abstract

type bool_ = True
           | False

type 'a list_ = Nil
              | Cons of 'a * 'a list_

type 'a tree = Empty
             | Node of 'a * 'a tree * 'a tree

type inttree = int tree

type coord = { x : int ; y : int ; z : int }

type ('a, 'b) pierce = (('a -> 'b) -> 'a) -> 'a

type ('a, 'b) mod_pon = ('a -> 'b) -> 'a -> 'b

type +'a blop = Blop of 'a

type (-'a, +'b) blip = Blip of ('a -> 'b)

type dummy = { mutable c : int; d : string -> bool }

let f42 = lazy (2 + 2)

let f43:int = match f42 with lazy n -> n

let f44 =
  let module M1 = struct
    type t = int
    let compare = (-)
  end in
  let module M2 = Set.Make (M1) in
  let module M3 = Map.Make (M1) in
  assert (Obj.repr M2.mem <> Obj.repr M3.mem)

let remove_duplicates comparison_fun string_list =
  let module StringSet =
    Set.Make(struct type t = string
                    let compare = comparison_fun end) in
  StringSet.elements
    (List.fold_right StringSet.add string_list StringSet.empty)

module rec A : sig
		 type t = Leaf of string | Node of ASet.t
		 val compare: t -> t -> int
               end
  =
  struct
    type t = Leaf of string | Node of ASet.t
    let compare t1 t2 =
      match (t1, t2) with
      | (Leaf s1, Leaf s2) -> Pervasives.compare s1 s2
      | (Leaf _, Node _) -> 1
      | (Node _, Leaf _) -> -1
      | (Node n1, Node n2) -> ASet.compare n1 n2
  end
   and ASet : Set.S with type elt = A.t
				    = Set.Make(A)

module M : sig
  type t = private A | B of int
  val a : t
  val b : int -> t
end
  = struct
  type t = A | B of int
  let a = A
  let b n = assert (n > 0); B n
end

module N : sig
  type t = private int
  val of_int: int -> t
  val to_int: t -> int
end
  = struct
  type t = int
  let of_int n = assert (n >= 0); n
  let to_int n = n
end

let _ = N.((of_int 3 :> int) = 2)

module MN = struct
  type t = M.t = private A | B of int
end


module M5 : sig type c = private < x : int; .. > val o : c end =
  struct
    class c = object method x = 3 method y = 2 end
    let o = new c
  end

module F(X : sig type c = private < x : int; .. > end) =
  struct
    let get_x (o : X.c) = o#x
  end
module G(X : sig type c = private < x : int; y : int; .. > end) =
  struct
    include F(X)
    let get_y (o : X.c) = o#y
  end


type t = [ `A of int | `B of bool ]
type u = private [< t > `A ]
type v = private [> t ]

let f : 'a.('a -> 'a) = fun x -> x

type 'a t2 = Leaf of 'a | Node of ('a * 'a) t2
let rec depth : 'a. 'a t2 -> 'b = function
    Leaf _ -> 1
  | Node x -> 1 + depth x


let f45 (type t) () =
  let module M = struct exception E of t end in
  (fun x -> M.E x), (function M.E x -> Some x | _ -> None)

let sort_uniq (type s) (cmp : s -> s -> int) =
  let module S = Set.Make(struct type t = s let compare = cmp end) in
  fun l ->
  S.elements (List.fold_right S.add l S.empty)

