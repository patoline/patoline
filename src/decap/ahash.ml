(* This is part of OCaml, we just changed the comparison for
   polymorphic hashtbl for it to work on function, where
   we use physical equality. 

   This is only useful when the hash function produces collision
 *)

let compare x y =
  try Pervasives.compare x y = 0 with _ -> x == y
					      
let hash x = Hashtbl.hash x
let hash_param n1 n2 x = Hashtbl.hash_param n1 n2 x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    mutable seed: int;                        (* for randomization *)
    initial_size: int;                        (* initial array size *)
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

(* To pick random seeds if requested *)

let randomized_default =
  let params =
    try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
    try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
  String.contains params 'R'

let randomized = ref randomized_default

let randomize () = randomized := true

let prng = lazy (Random.State.make_self_init())

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create ?(random = !randomized) initial_size =
  let s = power_2_above 16 initial_size in
  let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
  { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- Empty
  done

let reset h =
  let len = Array.length h.data in
  if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make h.initial_size Empty
  end

let copy h = { h with data = Array.copy h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = indexfun h key in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done
  end

let key_index h key =
  (hash_param 10 100 key) land (Array.length h.data - 1)

let add h key info =
  let i = key_index h key in
  let bucket = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize key_index h

let remove h key =
  let rec remove_bucket = function
    | Empty ->
        Empty
    | Cons(k, i, next) ->
        if compare k key
        then begin h.size <- h.size - 1; next end
        else Cons(k, i, remove_bucket next) in
  let i = key_index h key in
  h.data.(i) <- remove_bucket h.data.(i)

let rec find_rec key = function
  | Empty ->
      raise Not_found
  | Cons(k, d, rest) ->
      if compare key k then d else find_rec key rest

let find h key =
  match h.data.(key_index h key) with
  | Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if compare key k1 then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if compare key k2 then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if compare key k3 then d3 else find_rec key rest3

let find_all h key =
  let rec find_in_bucket = function
  | Empty ->
      []
  | Cons(k, d, rest) ->
      if compare k key
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket h.data.(key_index h key)

let replace h key info =
  let rec replace_bucket = function
    | Empty ->
        raise Not_found
    | Cons(k, i, next) ->
        if compare k key
        then Cons(key, info, next)
        else Cons(k, i, replace_bucket next) in
  let i = key_index h key in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- Cons(key, info, l);
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize key_index h

let mem h key =
  let rec mem_in_bucket = function
  | Empty ->
      false
  | Cons(k, d, rest) ->
      compare k key || mem_in_bucket rest in
  mem_in_bucket h.data.(key_index h key)

let iter f h =
  let rec do_bucket = function
    | Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, d, rest) ->
        do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu
