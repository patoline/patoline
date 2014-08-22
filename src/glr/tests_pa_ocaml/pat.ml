type point = { x: int; y: int }

let _ = let x = 1 and y = 2 in { x = x; y = y }
let _ = let x = 1 and y = 2 in { x; y }
let _ = let x = 1 and y = 2 in { x = x; y }

let f = fun {x; y} -> x + y
