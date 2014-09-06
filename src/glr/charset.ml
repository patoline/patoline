type charset = int array

let used, mask, shift, size = match Sys.word_size with
    32 -> 0x7fffffff, 15, 4, 256 / 16
  | 64 -> 0x7fffffffffffffff, 31, 5, 256 / 32
  | _ -> failwith "word_size is neither 32 nor 64, whoah !!!!"

let empty_charset = Array.make size 0

let full_charset = Array.make size used
 
let get s c =
  let i = Char.code c in
  s.(i lsr shift) land (1 lsl (i land mask)) <> 0

let addq s c =
  let i = Char.code c in
  s.(i lsr shift) <- s.(i lsr shift) lor (1 lsl (i land mask))

let delq s c =
  let i = Char.code c in
  s.(i lsr shift) <- s.(i lsr shift) land (lnot (1 lsl (i land mask)))

let add s c =
  let i = Char.code c in
  let s = Array.copy s in
  s.(i lsr shift) <- s.(i lsr shift) lor (1 lsl (i land mask));
  s

let del s c =
  let i = Char.code c in
  let s = Array.copy s in
  s.(i lsr shift) <- s.(i lsr shift) land (lnot (1 lsl (i land mask)));
  s
  
let union s1 s2 = 
  Array.mapi (fun i x -> x lor s2.(i)) s1

let singleton =
  let tbl = Array.init 256 (fun i ->
    let c = Char.chr i in
    add empty_charset c)
  in
  fun c -> tbl.(Char.code c)

let print_charset ch s =
  match s with None -> Printf.fprintf ch "None" | Some s ->
  Printf.fprintf ch "{";
  for i = 0 to 255 do
    if get s (Char.chr i) then Printf.fprintf ch "%s" (Char.escaped (Char.chr i))
  done;
  Printf.fprintf ch "}"

type char_tree =
  Any
| Leaf of charset
| Node of char_tree array
