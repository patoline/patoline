
let e = <expr:"2+2">

let f x y t p = <expr:"3 * $x$ * (match $y$ : $t$ with $p$ -> $y$ | _ -> $x$) + 2"> 

let g x y = <str_item:"let a = $x$ and b = $y$">

let g x y = <sig_item:"val x : $y$">

 
