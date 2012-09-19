(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

exception Out_of_bounds

module type STRING = sig

  type t

  type char

  val length : t -> int

  val empty : t
    
  val singleton : char -> t
    
  val append : t -> t -> t

  val get : t -> int -> char

  (* [sub t ofs len] *)
  val sub : t -> int -> int -> t

  val iter_range : (char -> unit) -> t -> int -> int -> unit

  val print : Format.formatter -> t -> unit

end

module type ROPE = sig

  include STRING

  val set : t -> int -> char -> t

  val delete : t -> int -> t

  val insert_char : t -> int -> char -> t

  val insert : t -> int -> t -> t

  module Cursor : sig

    type cursor
      
    val create : t -> int -> cursor
      
    val position : cursor -> int
      
    val to_rope : cursor -> t
      
    val move_forward : cursor -> int -> cursor
      
    val move_backward : cursor -> int -> cursor

    val move : cursor -> int -> cursor

    val get : cursor -> char
      
    val set : cursor -> char -> cursor

    val insert_char : cursor -> char -> cursor

    val insert : cursor -> t -> cursor

(* NYI
    val delete : cursor -> cursor
*)

    val print : Format.formatter -> cursor -> unit

  end

end

module type CONTROL = sig

  val small_length : int

  val maximal_height : int

end 

module Make(S : STRING)(C : CONTROL) = struct

  type t = 
    (* s,ofs,len with 0 <= ofs < len(s), ofs+len <= len(s) *)
    | Str of S.t * int * int 
    (* t1,t2,len,height with 0 < len t1, len t2 *)
    | App of t * t * int * int  

  type char = S.char

  let empty = Str (S.empty, 0, 0)

  let length = function
    | Str (_,_,n) 
    | App (_,_,n,_) -> n

  let of_string s = Str (s, 0, S.length s)

  let singleton c = of_string (S.singleton c)

  let height = function
    | Str _ -> 0
    | App (_, _, _, h) -> h

  (* smart constructor *)
  let mk_app t1 t2 = 
    App (t1, t2, length t1 + length t2, 1 + max (height t1) (height t2))

  let app = function
    | Str (_,_,0), t | t, Str (_,_,0) -> 
	t
    | Str (s1, ofs1, len1), Str (s2, ofs2, len2) 
	when len1 <= C.small_length && len2 <= C.small_length ->
	Str 
	  (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2), 0, len1 + len2)
    | App (t1, Str (s1, ofs1, len1), _, _), Str (s2, ofs2, len2)
	when len1 <= C.small_length && len2 <= C.small_length ->
	App (t1, 
	     Str 
	       (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2), 
		0, len1+len2),
	     length t1 + len1 + len2,
	     1 + height t1)
    | Str (s1, ofs1, len1), App (Str (s2, ofs2, len2), t2, _, _)
	when len1 <= C.small_length && len2 <= C.small_length ->
	App (Str 
	       (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2), 
		0, len1+len2),
	     t2, 
	     len1 + len2 + length t2,
	     1 + height t2)
    | t1, t2 -> 
	App (t1, t2, length t1 + length t2, 1 + max (height t1) (height t2))

  let append t1 t2 = app (t1, t2)
  let (++) = append

  let balance t =
    let rec to_list ((n, l) as acc) = function
      | Str _ as x -> n + 1, x :: l
      | App (t1, t2, _, _) -> to_list (to_list acc t2) t1
    in
    let rec build n l = 
      assert (n >= 1);
      if n = 1 then begin
	match l with
	  | [] -> assert false
	  | x :: r -> x, r
      end else
	let n' = n / 2 in
	let t1, l = build n' l in
	let t2, l = build (n - n') l in
	mk_app t1 t2, l
    in
    let n, l = to_list (0, []) t in
    let t, l = build n l in
    assert (l = []);
    t

  let rec unsafe_get t i = match t with
    | Str (s, ofs, _) ->
	S.get s (ofs + i)
    | App (t1, t2, _, _) ->
	let n1 = length t1 in
	if i < n1 then unsafe_get t1 i else unsafe_get t2 (i - n1)

  let get t i = 
    if i < 0 || i >= length t then raise Out_of_bounds;
    unsafe_get t i

  (* assumption: 0 <= start < stop <= len(t) *)
  let rec mksub start stop t =
    if start = 0 && stop = length t then
      t 
    else match t with
      | Str (s, ofs, _) -> 
	  Str (s, ofs+start, stop-start)
      | App (t1, t2, _, _) ->
	  let n1 = length t1 in
	  if stop <= n1 then mksub start stop t1 
	  else if start >= n1 then mksub (start-n1) (stop-n1) t2
	  else app (mksub start n1 t1, mksub 0 (stop-n1) t2)

  let sub t ofs len = 
    let stop = ofs + len in
    if ofs < 0 || len < 0 || stop > length t then raise Out_of_bounds;
    if len = 0 then empty else mksub ofs stop t

  let rec safe_iter_range f i n = function
    | Str (s, ofs, _) -> 
	S.iter_range f s (ofs+i) n
    | App (t1, t2, _, _) -> 
	let n1 = length t1 in
	if i + n <= n1 then 
	  safe_iter_range f i n t1
	else if i >= n1 then 
	  safe_iter_range f (i - n1) n t2
	else begin 
	  safe_iter_range f i n1 t1; 
	  safe_iter_range f (i - n1) (n - n1) t2 
	end

  let iter_range f t ofs len =
    if ofs < 0 || len < 0 || ofs+len > length t then raise Out_of_bounds;
    safe_iter_range f ofs len t

  let rec print fmt = function
    | Str (s, ofs, len) -> S.print fmt (S.sub s ofs len) (* TODO: IMPROVE? *)
    | App (t1, t2, _, _) -> print fmt t1; print fmt t2

  (* assumption: 0 <= i < len t *)
  let rec set_rec i c = function
    | Str (s, ofs, len) when i = 0 -> 
	app (singleton c, Str (s, ofs+1, len-1))
    | Str (s, ofs, len) when i = len-1 -> 
	app (Str (s, ofs, len-1), singleton c)
    | Str (s, ofs, len) ->
	app (Str (s, ofs, i), app (singleton c, Str (s, ofs+i+1, len-i-1)))
    | App (t1, t2, _, _) ->
	let n1 = length t1 in
	if i < n1 then 
	  app (set_rec i c t1, t2) 
	else 
	  app (t1, set_rec (i-n1) c t2)

  (* set t i c = sub t 0 i ++ singleton c ++ sub t (i+1) (length t-i-1) *)
  let set t i c =
    let n = length t in
    if i < 0 || i >= n then raise Out_of_bounds;
    set_rec i c t

  (* assumption: 0 <= i < len t *)
  let rec delete_rec i = function
    | Str (_, _, 1) -> 
	assert (i = 0); empty
    | Str (s, ofs, len) when i = 0 -> 
	Str (s, ofs+1, len-1)
    | Str (s, ofs, len) when i = len-1 -> 
	Str (s, ofs, len-1)
    | Str (s, ofs, len) -> 
	app (Str (s, ofs, i), Str (s, ofs+i+1, len-i-1))
    | App (t1, t2, _, _) -> 
	let n1 = length t1 in
	if i < n1 then 
	  app (delete_rec i t1, t2) 
	else 
	  app (t1, delete_rec (i-n1) t2)

  (* delete t i = sub t 0 i ++ sub t (i + 1) (length t - i - 1) *)
  let delete t i =
    let n = length t in
    if i < 0 || i >= n then raise Out_of_bounds;
    delete_rec i t

  (* assumption: 0 <= i < len t *)
  let rec insert_rec i r = function
    | Str _ as s when i = 0 -> 
	app (r, s)
    | Str (_, _, len) as s when i = len -> 
	app (s, r)
    | Str (s, ofs, len) -> 
	Str (s, ofs, i) ++ r ++ Str (s, ofs+i, len-i)
    | App (t1, t2, _, _) -> 
	let n1 = length t1 in
	if i < n1 then 
	  app (insert_rec i r t1, t2) 
	else 
	  app (t1, insert_rec (i-n1) r t2)

  (* insert t i r = sub t 0 i ++ r ++ sub t i (length t - i) *)
  let insert t i r =
    let n = length t in
    if i < 0 || i > n then raise Out_of_bounds;
    insert_rec i r t

  let insert_char t i c = 
    insert t i (singleton c)

  (* cursors *)
  module Cursor = struct

  type path = 
    | Top
    | Left of path * t
    | Right of t * path

  type cursor = {
    rpos: int;    (* position of the cursor relative to the current leaf *)
    lofs: int;    (* offset of the current leaf wrt whole rope *)
    leaf: t;      (* the leaf i.e. Str (s,ofs,len) *)
    path: path;   (* context = zipper *)
  }
  (* INVARIANT: 0 <= rpos <= len 
                rpos = len iff we are located at the end of the whole rope *)

  let position c = c.lofs + c.rpos

  (* cursor -> rope *)
  let rec unzip t = function
    | Top -> t
    | Left (p, tr) -> unzip (app (t, tr)) p
    | Right (tl, p) -> unzip (app (tl, t)) p
	
  let to_rope c = 
    unzip c.leaf c.path

  let create r i =
    let rec zip lofs p = function
      | Str (_, _, len) as leaf  ->
	  assert (lofs <= i && i <= lofs+len);
	  { rpos = i-lofs; lofs = lofs; leaf = leaf; path = p }
      | App (t1, t2, _, _) ->
	  let n1 = length t1 in
	  if i < lofs + n1 then
	    zip lofs (Left (p, t2)) t1
	  else
	    zip (lofs + n1) (Right (t1, p)) t2
    in
    if i < 0 || i > length r then raise Out_of_bounds;
    zip 0 Top r

  let get c = match c.leaf with
    | Str (s, ofs, len) ->
	let i = c.rpos in
	if i = len then raise Out_of_bounds;
	S.get s (ofs + i)
    | App _ ->
	assert false
 
  (* TODO: improve using concatenations when lengths <= small_length *)
  let set c x = match c.leaf with
    | Str (s, ofs, len) ->
	let i = c.rpos in
 	if i = len then raise Out_of_bounds;
	let leaf = Str (S.singleton x, 0, 1) in
	if i = 0 then
	  if len = 1 then
	    { c with leaf = leaf }
	  else
	    { c with 
		leaf = leaf; 
		path = Left (c.path, Str (s, ofs + 1, len - 1)) }
	else if i = len - 1 then
	  { lofs = c.lofs + len - 1;
	    rpos = 0;
	    leaf = leaf;
	    path = Right (Str (s, ofs, len - 1), c.path) }
	else 
	  { lofs = c.lofs + i;
	    rpos = 0;
	    leaf = leaf;
	    path = Left (Right (Str (s, ofs, i) , c.path), 
			 Str (s, ofs + i + 1, len - i - 1)) }
    | App _ ->
	assert false

  let rec concat_path p1 p2 = match p1 with
    | Top -> p2
    | Left (p, r) -> Left (concat_path p p2, r)
    | Right (l, p) -> Right (l, concat_path p p2)
 
  (* TODO: improve using concatenations when lengths <= small_length *)
  let insert c r = match c.leaf with
    | Str (s, ofs, len) ->
	let i = c.rpos in
	let cr = create r 0 in
	if i = 0 then
	  { cr with 
	      lofs = c.lofs;
	      path = concat_path cr.path (Left (c.path, c.leaf)) }
	else if i = len then
	  { cr with
	      lofs = c.lofs + len;
	      path = concat_path cr.path (Right (c.leaf, c.path)) }
	else 
	  { cr with
	      lofs = c.lofs + i;
	      path = concat_path cr.path 
	               (Left (Right (Str (s, ofs, i) , c.path), 
			      Str (s, ofs + i, len - i))) }
    | App _ ->
	assert false

  let insert_char c x = 
    insert c (of_string (S.singleton x))
 
  (* moves to start of next leaf (on the right) if any, 
     or raises [Out_of_bounds] *)
  let next_leaf c =
    let lofs = c.lofs + length c.leaf in
    let rec down p = function
      | Str _ as leaf -> { rpos = 0; lofs = lofs; leaf = leaf; path = p }
      | App (t1, t2, _, _) -> down (Left (p, t2)) t1
    in
    let rec up t = function
      | Top -> raise Out_of_bounds
      | Right (l, p) -> up (mk_app l t) p
      | Left (p, r) -> down (Right (t, p)) r
    in
    up c.leaf c.path

  let rec move_forward_rec c n = match c.leaf with
    | Str (_, _, len) ->
	let rpos' = c.rpos + n in
	if rpos' < len then
	  { c with rpos = rpos' }
	else if rpos' = len then begin 
	  try next_leaf c with Out_of_bounds -> { c with rpos = rpos' } 
	end else (* rpos' > len *)
	  let c = next_leaf c in
	  move_forward_rec c (rpos' - len) (* TODO: IMPROVE? *)
    | App _ ->
	assert false

  let move_forward c n =
    if n < 0 then invalid_arg "Rop.move_forward";
    if n = 0 then c else move_forward_rec c n

  (* moves to the end of previous leaf (on the left) if any,
     raises [Out_of_bounds] otherwise *)
  let prev_leaf c =
    let rec down p = function
      | Str (_, _, len) as leaf -> 
	  { rpos = len; lofs = c.lofs - len; leaf = leaf; path = p }
      | App (t1, t2, _, _) -> down (Right (t1, p)) t2
    in
    let rec up t = function
      | Top -> raise Out_of_bounds
      | Right (l, p) -> down (Left (p, t)) l
      | Left (p, r) -> up (mk_app t r) p
    in
    up c.leaf c.path

  let rec move_backward_rec c n = match c.leaf with
    | Str (_, _, len) ->
	let rpos' = c.rpos - n in
	if rpos' >= 0 then
	  { c with rpos = rpos' }
	else (* rpos' < 0 *)
	  let c = prev_leaf c in
	  move_backward_rec c (-rpos')
    | App _ ->
	assert false

  let move_backward c n =
    if n < 0 then invalid_arg "Rop.move_backward";
    if n = 0 then c else move_backward_rec c n

  let move c n =
    if n = 0 then c 
    else if n > 0 then move_forward_rec c n
    else move_backward_rec c (-n)
      
  let rec leftmost lofs p = function
    | Str _ as leaf -> { rpos = 0; lofs = lofs; leaf = leaf; path = p }
    | App (t1, t2, _, _) -> leftmost lofs (Left (p, t2)) t1

  let delete c = match c.leaf with
    | Str (s, ofs, len) ->
	let i = c.rpos in
 	if i = len then raise Out_of_bounds;
	if i = 0 then
	  if len = 1 then begin
	    match c.path with
	      | Top -> 
		  { c with leaf = empty }
	      | Left (p, t) -> 
		  (* leftmost c.lofs p r *)
		  let r = to_rope { c with leaf = t; path = p } in 
		  create r c.lofs
	      | Right (t, p) -> (* TODO: IMPROVE *)
		  let r = to_rope { c with leaf = t; path = p } in 
		  create r c.lofs
	  end else
	    { c with leaf = Str (s, ofs + 1, len - 1) }
	else if i = len - 1 then
	  next_leaf { c with leaf = Str (s, ofs, len - 1) }
	else
	  { lofs = c.lofs + i;
	    rpos = 0;
	    leaf = Str (s, ofs + i + 1, len - i - 1);
	    path = Right (Str (s, ofs, i), c.path) }
    | App _ ->
	assert false

  let print fmt c = (* TODO: improve *)
    let r = to_rope c in
    let i = position c in
    let before = sub r 0 i in
    let after = sub r i (length r - i) in
    print fmt before; Format.fprintf fmt "|"; print fmt after

  end (* Cursor *)

end

(********

(* cursors *)

type path = 
  | Top
  | Left of path * t
  | Right of t * path

type cursor = {
  pos : int;    (* position of the cursor *)
  lofs: int;    (* offset of the current leaf *)
  leaf: t;      (* the leaf i.e. Str (str,ofs,len) *)
  path: path;   (* context = zipper *)
}

(* cursor -> rope *)
let rec unzip t = function
  | Top -> t
  | Left (p, tr) -> unzip (mk_app t tr) p
  | Right (tl, p) -> unzip (mk_app tl t) p

let rope_of_cursor pos = 
  unzip pos.leaf pos.path

let rec zip_leftmost p = function
  | Str _ as leaf -> { pos = 0; lofs = 0; leaf = leaf; path = p }
  | App (t1, t2, _, _) -> zip_leftmost (Left (p, t2)) t1

let start = zip_leftmost Top

let cursor_at_start c = c.pos = 0

let rec cursor_pop = function
  | { pos = 0; lofs = 0; leaf = Str (s, ofs, 1); path = p } as c ->
      String.unsafe_get s ofs, 
      begin match p with
	| Top -> { c with leaf = empty; path = Top }
	| Left (p, r) -> zip_leftmost p r
	| Right _ -> assert false
      end
  | { pos = 0; lofs = 0; leaf = Str (_, _, 0); path = p } ->
      assert (p = Top);
      raise Not_found
  | { pos = 0; lofs = 0; leaf = Str (s, ofs, len) } as c ->
      String.unsafe_get s ofs, 
      { c with leaf = Str (s, ofs+1, len-1) }
  | _ ->
      assert false

let moveto r i =
  let rec zip lofs p = function
    | Str (_, _, len) as leaf when lofs <= i && i < lofs+len ->
	{ pos = i; lofs = lofs; leaf = leaf; path = p }
    | Str _ ->
	Format.eprintf "len(r) = %d lofs = %d i = %d@." (len r) lofs i;
	assert false
    | App (t1, t2, _, _) ->
	let n1 = len t1 in
	if i < lofs + n1 then
	  zip lofs (Left (p, t2)) t1
	else
	  zip (lofs + n1) (Right (t1, p)) t2
  in
  zip 0 Top r

let cursor_get_or_exit c i = match c with
  | { lofs = lofs; leaf = Str (s, ofs, len) } when lofs <= i && i < lofs+len ->
      String.unsafe_get s (ofs + i - lofs)
  | _ ->
      raise Exit

let rec cursor_get c i = match c with
  | { lofs = lofs; leaf = Str (s, ofs, len) } when lofs <= i && i < lofs+len ->
      String.unsafe_get s (ofs + i - lofs)
  | { lofs = lofs; leaf = Str (_, _, ls); path = p } ->
      let rec lookup_rope lofs ls = function
	| Top -> 
	    raise Not_found
	| Left (p, r) ->
	    let j = i - (lofs + ls) in
	    let lr = len r in
	    if 0 <= j && j < lr then get r j else lookup_rope lofs (ls+lr) p
	| Right (l, p) ->
	    let ll = len l in
	    let llofs = lofs - ll in
	    let j = i - llofs in
	    if 0 <= j && j < ll then get l j else lookup_rope llofs (ll+ls) p
      in
      lookup_rope lofs ls p
  | _ ->
      assert false

*****)

(* flat strings *)
module Str = struct
  include String
  let get = unsafe_get
  type char = Char.t
  let empty = ""
  let singleton = String.make 1
  let append = (^)
  let print = Format.pp_print_string
  let iter_range f s ofs len = (* safe *)
    for i = ofs to ofs+len-1 do f (String.unsafe_get s i) done
end

module Control = struct let small_length = 256 let maximal_height = max_int end
module S = Make(Str)(Control)


(* ropes with function leaves *)
module SorF = 
struct
  type t = S of string | F of int * (int -> char)
  type char = Char.t
  let length = function S s -> String.length s | F (n,_) -> n
  let empty = S ""
  let singleton c = S (String.make 1 c)
  let append t1 t2 = match t1, t2 with
    | S s1, S s2 -> S (s1 ^ s2)
    | S s1, F (n2, f2) -> 
	let n1 = String.length s1 in
	F (n1 + n2, fun i -> if i < n1 then s1.[i] else f2 (i - n1))
    | F (n1, f1), S s2 ->
	let n2 = String.length s2 in
	F (n1 + n2, fun i -> if i < n1 then f1 i else s2.[i - n1])
    | F (n1, f1), F (n2, f2) ->
	F (n1 + n2, fun i -> if i < n1 then f1 i else f2 (i - n1))
  let get t i = match t with S s -> s.[i] | F (_,f) -> f i
  let sub t ofs len = match t with
    | S s -> S (String.sub s ofs len)
    | F (n, f) -> F (len, fun i -> f (i - ofs))
  let iter_range f t ofs len = match t with
    | S s -> for i = ofs to ofs+len-1 do f s.[i] done
    | F (_,fs) -> for i = ofs to ofs+len-1 do f (fs i) done
  let print fmt = function
    | S s -> Format.pp_print_string fmt s
    | F (n, f) -> for i = 0 to n-1 do Format.fprintf fmt "%c" (f i) done
end
module SF =
struct
  module Ctrl = struct end
  include Make(SorF)(Control)
  let of_function n f = of_string (SorF.F (n, f))
  let of_string s = of_string (SorF.S s)
end


(* ropes of any type (using arrays as flat sequences) *)

module type PrintableType = sig 
  type t
  val print : Format.formatter -> t -> unit 
end

module MakeArray(X : PrintableType) = struct

  module A = struct
    type char = X.t
    type t = X.t array
    let length = Array.length
    let empty = [||]
    let singleton l = [|l|]
    let append = Array.append
    let get = Array.get
    let sub = Array.sub
    let iter_range f a ofs len = for i = ofs to ofs+len-1 do f a.(i) done
    let print fmt a = Array.iter (X.print fmt) a
  end

  module C = struct let small_length = 256 let maximal_height = max_int end

  include Make(A)(C)

  let of_array = of_string

  let create n v = of_string (Array.create n v)

  let init n f = of_string (Array.init n f)

end

