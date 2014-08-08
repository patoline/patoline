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

if fortytwo <> Some 42 then assert false
