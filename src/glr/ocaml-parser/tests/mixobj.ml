(* $Id: mixobj.ml.txt,v 1.1 2005/02/24 01:45:32 garrigue Exp $ *)

(* Base class: evaluation, and extraction of numeric values *)
class virtual base = object (self : 'a)
  method eval (s : (string * 'a) list) = self
  method value : int option = None
end

class num x = object
  inherit base
  val x : int = x
  method value = Some x
end

class var ~name = object (self)
  inherit base
  val name : string = name
  method eval s =
    try List.assoc name s with Not_found -> self
end

(* A class for numeric binary operators *)
(* Note that we need a virtual method to construct new num objects,
   to allow further extension *)
class virtual binop x y = object (self : 'a)
  inherit base
  val x : 'a = x
  val y : 'a = y
  method private virtual num : int -> 'a
  method private virtual op : int -> int -> int
  method eval s =
    let x' = x#eval s and y' = y#eval s in
    match x'#value, y'#value with
      Some m, Some n -> self#num (self#op m n)
    | _ -> {< x = x'; y = y' >}
end

class virtual plus x y = object
  inherit binop x y
  method private op = (+)
end

class virtual mult x y = object
  inherit binop x y
  method private op = ( * )
end

(* As the result of num is supposed to have the same type as self,
   the method may only be defined in a final class (no longer extensible.)
   They are available since ocaml 3.08 *)
let plus x y = object
  inherit plus x y
  method private num = new num
end

let mult x y = object
  inherit mult x y
  method private num = new num
end

let e1 = plus (new num 3) (new var "x")

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n

class apply x y = object (self : 'a)
  inherit base
  val x : 'a = x
  val y : 'a = y
  method apply (z : 'a) = {< x = self; y = z >}
  method eval s =
    let x' = x#eval s and y' = y#eval s in
    x'#apply y'
end

class virtual lambda ~name ~body = object (self : 'a)
  inherit base
  val name : string = name
  val body : 'a = body
  method private virtual var : name:string -> 'a
  method eval s =
    let name' = gensym () in
    let s' = (name, self#var ~name:name') :: s in
    {< name = name'; body = body#eval s' >}
  method apply arg =
    body#eval [name,arg]
end

class noapply = object (_ : 'a)
  method apply (y : 'a) : 'a = failwith "Not a function"
end

class num' x = object
  inherit num x
  inherit noapply
end

class virtual plus' x y = object 
  inherit plus x y
  inherit noapply
end

class virtual mult' x y = object 
  inherit mult x y
  inherit noapply
end

let plus' x y = object
  inherit plus' x y
  method private num = new num'
end

let mult' x y = object
  inherit mult' x y
  method private num = new num'
end

(* Again, since we pass self to the apply class, we must be final *)
let var' ~name = object (self)
  inherit var ~name
  method apply = new apply self
end

let lambda ~name ~body = object
  inherit lambda ~name ~body
  method private var = var'
end

let e2 = new apply (lambda "x" (plus' (var' "x") (new num' 1))) (new num' 3)

let e3 =
  plus' (new apply (lambda "x" (mult' (var' "x") (var' "x"))) (new num' 2))
    (new num' 5)

let n1 = (e1#eval ["x", new num 5])#value 
let n2 = (e2#eval [])#value
let n3 = (e3#eval [])#value
