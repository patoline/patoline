open Extra

module type TreeData =
  sig
    type node
    type leaf
  end

module Make(D : TreeData) =
  struct
    include D

    type tree =
      | Node of (node * tree IntMap.t)
      | Leaf of leaf

    let node : node -> tree = fun d -> Node (d, IntMap.empty)
    let leaf : leaf -> tree = fun d -> Leaf d

    type zipper = tree * (int * (node * tree IntMap.t)) list

    let tree_to_zipper : tree -> zipper = fun t -> (t, [])

    let empty : node -> zipper = fun d -> tree_to_zipper (node d)

    let is_root : zipper -> bool = fun (_, l) -> l = []

    let is_node : zipper -> bool = fun (t, _) ->
      match t with
      | Node(_,_) -> true
      | Leaf(_)   -> false

    let up : zipper -> zipper = fun (t, l) ->
      match l with
      | []             -> invalid_arg "Zipper.up"
      | (i,(d,m)) :: l -> (Node (d, IntMap.add i t m), l)

    let up_n : int -> zipper -> zipper = fun n z ->
      let rec up_n n z =
        if n < 0 then invalid_arg "Zipper.up_n";
        if n = 0 then z else up_n (n-1) (up z)
      in try up_n n z with _ -> invalid_arg "Zipper.up_n"

    let top : zipper -> zipper = fun z ->
      let rec top z =
        if snd z = [] then z else top (up z)
      in top z

    let zipper_to_tree : zipper -> tree = fun z -> fst (top z)

    let down : zipper -> int -> zipper = fun (t,m) i ->
      match t with
      | Node(dt,mt) -> (try (IntMap.find i mt, (i, (dt,mt)) :: m)
                        with Not_found -> invalid_arg "Zipper.down")
      | Leaf(_)     -> invalid_arg "Zipper.down"

    let down_path : zipper -> int list -> zipper = fun z l ->
      let rec down_path z = function
        | []   -> z
        | i::l -> down_path (down z i) l
      in try down_path z l with _ -> invalid_arg "Zipper.down_path"

    let indices : zipper -> int list = fun (t,_) ->
      match t with
      | Node(_,m) -> List.map fst (IntMap.bindings m)
      | Leaf(_)   -> invalid_arg "Zipper.indices"

    let min_index : zipper -> int = fun (t,_) ->
      match t with
      | Node(_,m) -> fst (IntMap.min_binding m)
      | Leaf(_)   -> invalid_arg "Zipper.min_index"

    let max_index : zipper -> int = fun (t,_) ->
      match t with
      | Node(_,m) -> fst (IntMap.max_binding m)
      | Leaf(_)   -> invalid_arg "Zipper.max_index"

    let down_first : zipper -> zipper = fun z ->
      try down z (min_index z) with _ -> invalid_arg "Zipper.down_first"

    let down_last : zipper -> zipper = fun z ->
      try down z (max_index z) with _ -> invalid_arg "Zipper.down_last"

    let new_child : zipper -> tree -> int -> zipper = fun (t,m) c i ->
      match t with
      | Node(dt,mt) -> if IntMap.mem i mt then invalid_arg "Zipper.new_child";
                       (c, (i, (dt,mt)) :: m)
      | Leaf(_)     -> invalid_arg "Zipper.new_child"

    let new_last_child : zipper -> tree -> zipper = fun z t ->
      let i = try (max_index z) + 1 with _ -> 0 in
      try new_child z t i with _ -> invalid_arg "Zipper.new_last_child"

    let new_first_child : zipper -> tree -> zipper = fun z t ->
      let i = try (min_index z) - 1 with _ -> 0 in
      try new_child z t i with _ -> invalid_arg "Zipper.new_last_child"

    let has_child : zipper -> int -> bool = fun z i ->
      try List.mem i (indices z) with _ -> invalid_arg "Zipper.has_child"

    let remove_child : zipper -> int -> zipper = fun (t,m) i ->
      match t with
      | Node(dt,mt) -> let mt' = IntMap.remove i mt in
                       if mt == mt' then invalid_arg "Zipper.remove_child";
                       (Node(dt,mt'), m)
      | Leaf(_)     -> invalid_arg "Zipper.remove_child"
  end
