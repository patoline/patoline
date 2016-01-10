open Decap

type 'a tree =
  Leaf of 'a
| Node of 'a tree * 'a tree
| Or of 'a tree * 'a tree

let merge a b = Or(a,b)

let parser* gram =
  w:''[a-zA-Z0-9]+'' gram*[fun x y -> Node(y,x)] ->

  declare_grammar ()
let _ = set_grammar gram
  (dependant_merge_sequence merge (regexp ''[a-zA-Z0-9]+'' (fun groupe -> Leaf (groupe 0)))
    (fun x -> merge_fixpoint merge x (apply (fun x y -> Node(y,x)) gram)))

let rec count = function
    Leaf _ -> 1
  | Node(a,b) -> count a * count b
  | Or(a,b) -> count a + count b

let rec size = function
    Leaf _ -> 1
  | Node(a,b) | Or(a,b) -> size a + size b

let rec str n = if n = 0 then "" else if n = 1 then "a " else
    let n' = n / 2 in
    let s1 = str n' in
    if n mod 2 = 0 then s1 ^ s1 else s1 ^ s1 ^ "a "

let blank = blank_regexp ''[ \t\n\r]*''

let n = if Array.length Sys.argv <> 2 then 10 else int_of_string (Sys.argv.(1))
let _ =
  for i = 1 to n do
    let t = parse_string (sequence gram (eof ()) (fun x _ -> x)) blank (str i) in
    let n = count t and s = size t in
    Printf.printf "%d => size = %d, catalan = %d, compression = %f\n%!" i s n (float n *. float i /. float s)
  done
