
let file = open_in "./UnicodeData.txt"

type uchar = string array

let parse_line s = 
  let rec scan_field i = if i < String.length s then 
      match s.[i] with 
      | ';' -> i
      | a -> scan_field (succ i) 
    else i
  in
  let rec scan res n i =
    if n < 15 then
      let j = scan_field i in
      let field = String.sub s i (j-i) in 
      let _ = res.(n) <- field in
      scan res (succ n) (succ j)
    else
      ()
  in
  let res = Array.make 15 "" in
  let _ = scan res 0 0 in
  res
     
let subscripts : uchar list ref = ref []
let superscripts : uchar list ref = ref []

let rec string_forall i len p s = 
  if i >= String.length s then true else
    let len = 
      if i+len-1 < String.length s then len else
	String.length s - i
    in
    let max = i + len in
    let rec string_forall_rec res k = 
      if k < max then
	string_forall_rec ((p s.[k] k) && res) (succ k)
      else res
    in string_forall_rec true i


let is_superscript c = 
  string_forall 0 11 (fun char i -> char = "SUPERSCRIPT".[i]) c.(1)

let is_subscript c = 
  string_forall 0 9 (fun char i -> char = "SUBSCRIPT".[i]) c.(1)

let _ = try
	  while true do
	    let s = input_line file in
	    let c = parse_line s in
	    if is_superscript c then
	      superscripts := c :: !superscripts
	    else 	 
	      if is_subscript c then
	      subscripts := c :: !subscripts
	      else ()
	  done
  with End_of_file -> ()


let _ = close_in file

