
type roman_digit  = { i : string ; v : string }

let roman ?(capital=true) n = 
  if n <= 0 or n > 3888 then raise (Invalid_argument "roman");
  let s = if capital then
      [| { i = "I"; v = "V" } ; { i = "X"; v = "L" }  ; { i = "C"; v = "D" } ; { i = "M"; v = "" } |]
    else
      [| { i = "i"; v = "v" } ; { i = "x"; v = "l" }  ; { i = "c"; v = "d" } ; { i = "m"; v = "" } |] 
  in 
  let digit s = function
    | 1 -> s.i, 0
    | 2 -> s.i ^ s.i, 0
    | 3 -> s.i ^ s.i ^ s.i, 0
    | 4 -> s.i ^ s.v, 0
    | 5 -> s.v, 0
    | 6 -> s.v ^ s.i, 0
    | 7 -> s.v ^ s.i ^ s.i, 0
    | 8 -> s.v ^ s.i ^ s.i ^ s.i, 0
    | 9 -> s.i, 1
    | _ -> "", 0
  in
  let rec fn l n = 
    let d, carry = digit s.(l) (n mod 10) in
    let l = l + 1 in
    let d = if carry = 1 then d ^ s.(l).i else d in
    let n = n / 10 in
    if n = 0 then d else
      fn l n ^ d
  in
  fn 0 n

let alphabetic  ?(capital=true) n = 
  if n <= 0 then raise (Invalid_argument "alphabetic");
  let start = (if capital then Char.code 'A' else Char.code 'a') - 1in
  let rec fn n =
    let d = String.make 1 (Char.chr (start + n mod 26)) in
    let n = n / 26 in
    if n = 0 then d else fn n ^ d
  in
  fn n

    
    
