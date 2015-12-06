(** Type of a prefix tree. *)
type 'a tree = Node of 'a option * (char * 'a tree) list

(** Empty prefix tree. *)
let empty : 'a tree = Node(None, [])

(** [is_empty t] returns [true] if the tree contains no indexed value. *)
let is_empty : 'a tree -> bool = fun t ->
  let rec is_empty (Node(vo,l)) =
    match vo with
    | None   -> List.for_all (fun (_,t) -> is_empty t) l
    | Some _ -> false
  in
  is_empty t

(* Auxiliary function to obtain the characters of a string. *)
let string_to_char_list : string -> char list = fun s ->
  let chars = ref [] in
  String.iter (fun c -> chars := c :: !chars) s;
  List.rev !chars
 
(* Auxiliary function to obtain a tree with only one branch. *)
let linear_branch : char list -> 'a -> 'a tree = fun cs v ->
  let aux c acc = Node(None, [(c,acc)]) in
  List.fold_right aux cs (Node(Some v, []))

(* Build a prefix tree with only one stored value. *)
let singleton : string -> 'a -> 'a tree = fun s ->
  linear_branch (string_to_char_list s)

(** [add s v t] inserts the value [v] with the key string [s] in the tree
    [t]. If a value is already there, it is overwritten. *)
let add : string -> 'a -> 'a tree -> 'a tree = fun s v t ->
  let rec insert v cs (Node(vo,l)) =
    match (cs, vo) with
    | ([]   , None  ) -> Node(Some v, l)
    | ([]   , Some _) -> Node(Some v, l)
    | (c::cs, v'    ) ->
        let l =
          try
            let t = List.assoc c l in
            (c, insert v cs t) :: (List.remove_assoc c l)
          with Not_found -> (c, linear_branch cs v) :: l
        in
        Node(v', l)
  in
  insert v (string_to_char_list s) t

(** [remove s t] removes the value stored with string key [s] if any. *)
let remove : string -> 'a tree -> 'a tree = fun s t ->
  let rec remove cs (Node(vo,l)) =
    match cs with
    | []    -> Node(None,l)
    | c::cs ->
        begin
          try
            let t = remove cs (List.assoc c l) in
            Node(vo, (c,t) :: (List.remove_assoc c l))
          with Not_found -> Node(vo,l)
        end
  in
  remove (string_to_char_list s) t

(** [find s t] finds the value matching exactly the string [s] in the tree
    [t]. If there is no such value, the exception [Not_found] is thrown. *)
let find : string -> 'a tree -> 'a = fun s t ->
  let rec find cs (Node(vo,l)) =
    match (cs, vo) with
    | ([]   , None  ) -> raise Not_found
    | ([]   , Some v) -> v
    | (c::cs, _     ) -> find cs (List.assoc c l)
  in
  find (string_to_char_list s) t

(** [mem s t] returns [true] if the string [s] is mapped in [t]. *)
let mem : string -> 'a tree -> bool = fun s t ->
  try ignore (find s t); true with Not_found -> false

(** [every_prefix s t] finds the values stored with keys that are prefix of
    [s] in the tree [t]. The function returns a list of couples of the value
    and the number of characters consumed for the value. The entries are
    sorted in order of the longest prefix. *)
let every_prefix : string -> 'a tree -> (int * 'a) list = fun s t ->
  let rec find n found cs (Node(vo,l)) =
    match (cs, vo) with
    | ([]   , None  ) -> found
    | ([]   , Some v) -> (n,v) :: found
    | (c::cs, None  ) ->
        begin
          try find (n+1) found cs (List.assoc c l)
          with Not_found -> found
        end
    | (c::cs, Some v) ->
        begin
          let found = (n,v) :: found in
          try find (n+1) found cs (List.assoc c l)
          with Not_found -> found
        end
  in
  find 0 [] (string_to_char_list s) t

(** [longest_prefix s t] finds the value matching the longest prefix of [s]
    in the tree [t]. The function returns a couple of the value and the
    number of characters consumed. If there is no such value, the exception
    [Not_found] is thrown. *)
let longest_prefix : string -> 'a tree -> int * 'a = fun s t ->
  match every_prefix s t with
  | []   -> raise Not_found
  | r::_ -> r

(** Folding function on prefix trees. *)
let fold : ('b -> string -> 'a -> 'b) -> 'b -> 'a tree -> 'b = fun f e t ->
  let rec fold path acc (Node(vo,l)) =
    let acc =
      match vo with
      | None   -> acc
      | Some v -> f acc path v
    in
    let aux acc (c,t) =
      fold (path ^ (String.make 1 c)) acc t
    in
    List.fold_left aux acc l
  in
  fold "" e t

(** [bindings t] computes the list of all the bindings in the tree [t]. *)
let bindings : 'a tree -> (string * 'a) list =
  fold (fun acc k v -> (k,v) :: acc) []

(** [keys t] returns the list of all the keys bound in [t]. *)
let keys : 'a tree -> string list =
  fold (fun acc k _ -> k :: acc) []

(** [cardinal t] returns the number of bound keys in [t]. *)
let cardinal : 'a tree -> int =
  fold (fun acc _ _ -> acc + 1) 0
