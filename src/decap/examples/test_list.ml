open Decap

type 'a tree =
  Leaf of 'a
| Node of 'a tree * 'a tree

let gram = declare_grammar ()

let _ = set_grammar gram (glr*
  x:RE"[a-zA-Z0-9]+"[Leaf (groupe 0)] ys:gram* -> List.fold_left (fun acc y -> Node(acc,y)) x ys
  end)

let all = glr*
  l:gram EOF
  end

let rec print ch = function
    Leaf s -> Printf.fprintf ch "%s" s
  | Node(a,b) -> Printf.fprintf ch "<%a,%a>" print a print b

let rec size = function
    Leaf _ -> 1
  | Node(a,b) -> size a + size b

let rec str n = if n = 0 then "" else if n = 1 then "a " else
    let n' = n / 2 in
    let s1 = str n' in
    if n mod 2 = 0 then s1 ^ s1 else s1 ^ s1 ^ "a "

let blank = blank_regexp (Str.regexp "[ \t\n\r]*")

let n = if Array.length Sys.argv <> 2 then 10 else int_of_string (Sys.argv.(1))
let _ =
  for i = 1 to n do
    let ts = parse_string all blank (str i) in
    let n = List.length ts in
    Printf.printf "%d => catalan = %d, %a\n%!" i n print (List.hd ts)
  done

