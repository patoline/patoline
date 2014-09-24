
let g y = do f y where f x = x * x

let k y = let try a = g y in a * a with Not_found -> 3


