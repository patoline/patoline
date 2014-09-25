class counter = object
  val mutable n = 0
  method incr = n <- n+1
  method get = n
end

let stack init = object
  val mutable s = init

  method pop =
    match s with
    | []       -> None
    | hd :: tl -> s <- tl; Some hd

  method push hd = 
    s <- hd :: s
end

let s = stack [3; 2; 1]

let _ = s#push 42
let fortytwo = s#pop

let _ = if fortytwo <> Some 42 then assert false

type shape = < area : float >

type square = < area : float; width : float >

let square w = object
  method area  = w * w
  method width = w
end

type circle = < area : float; radius : float >

let circle r = object
  method area   = 3.14 *. r ** 2.0
  method radius = r
end
