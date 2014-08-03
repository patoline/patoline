open List

let xy = 3
let x = 10.0
let f x = x
let g x y z = if x then y else z

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

let f3 x = match x with

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

let f26 = let s = f25 in s.[0] <- 'x' ; s.[1]

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

let f36 n = let n = ref n and r = ref 1 in while !n > 1 do !r := !r * !n; decr !n done; !r

let f37 n = let r = ref 1 in for i = 2 to n do !r := !r * n; done; !r
