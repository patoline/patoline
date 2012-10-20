let add (x,y,z) (x',y',z') =
  x +. x', y +. y', z+. z'

let sub (x,y,z) (x',y',z') =
  x -. x', y -. y', z -. z'

let dot(x,y,z) (x',y',z') =
  x *. x'+. y *. y'+. z *. z'

let norm(x,y,z) =
  sqrt (dot (x,y,z) (x,y,z))

let dist u v = norm (sub u v)

let zero = 0., 0., 0.
 
let mul a (x,y,z) = a *. x, a *. y, a *. z

let normalize v = mul (1.0 /. norm v) v

let vecp(x,y,z) (x',y',z') =
  (z' *. y -. z *. y',
   x' *. z -. x *. z',
   y' *. x -. x' *. y)

let pi = acos(0.0) *. 2.0
