type charset = int array

let used, mask, shift, size =
  match Sys.word_size with
  | 32 -> 0x7fffffff, 15, 4, 256 / 16
  | 64 -> 0x7fffffffffffffff, 31, 5, 256 / 32
  | _  -> assert false (* Cannot happen... *)

let empty_charset = Array.make size 0
let full_charset  = Array.make size used
 
let mem cs c =
  let i = Char.code c in
  cs.(i lsr shift) land (1 lsl (i land mask)) <> 0

let addq cs c =
  let i = Char.code c in
  cs.(i lsr shift) <- cs.(i lsr shift) lor (1 lsl (i land mask))

let add cs c =
  let i = Char.code c in
  let cs = Array.copy cs in
  cs.(i lsr shift) <- cs.(i lsr shift) lor (1 lsl (i land mask));
  cs

let delq cs c =
  let i = Char.code c in
  cs.(i lsr shift) <- cs.(i lsr shift) land (lnot (1 lsl (i land mask)))

let del cs c =
  let i = Char.code c in
  let cs = Array.copy cs in
  cs.(i lsr shift) <- cs.(i lsr shift) land (lnot (1 lsl (i land mask)));
  cs
  
let union cs1 cs2 = 
  Array.mapi (fun i x -> x lor cs2.(i)) cs1

let singleton =
  let tbl = Array.init 256 (fun i -> add empty_charset (Char.chr i)) in
  fun c -> tbl.(Char.code c)

let copy = Array.copy

let list_of_charset cs =
  let res = ref [] in
  for i = 0 to 255 do
    let c = Char.chr i in
    if mem cs c then res := Char.escaped c :: !res
  done;
  !res

let print_charset oc cs =
  match cs with
  | None -> Printf.fprintf oc "None"
  | Some cs ->
      begin
        Printf.fprintf oc "{";
        for i = 0 to 255 do
          if mem cs (Char.chr i) then
            Printf.fprintf oc "%s" (Char.escaped (Char.chr i))
        done;
        Printf.fprintf oc "}"
      end

type char_tree = Any
               | Leaf of charset
               | Node of char_tree array
