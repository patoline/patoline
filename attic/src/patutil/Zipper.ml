module type TreeData =
  sig
    type node
    type tree

    val tree_of_node : node -> tree
    val node_of_tree : tree -> node

    val get_child : node -> int -> tree
    val set_child : node -> int -> tree -> node
    val remove_child : node -> int -> node
    val has_child : node -> int -> bool
    val min_index : node -> int
    val max_index : node -> int
  end

module Make(D : TreeData) =
  struct
    type tree = D.tree
    type node = D.node

    type zipper = tree * (int * node) list

    let tree_to_zipper t =
      (t, [])

    let empty d =
      tree_to_zipper (D.tree_of_node d)

    let is_root (_, l) =
      l = []

    let up (t, l) =
      match l with
      | []         -> invalid_arg "Zipper.up"
      | (i,d) :: l -> (D.tree_of_node (D.set_child d i t), l)

    let up_n n z =
      let rec up_n n z =
        if n < 0 then invalid_arg "Zipper.up_n";
        if n = 0 then z else up_n (n-1) (up z)
      in try up_n n z with _ -> invalid_arg "Zipper.up_n"

    let top z =
      let rec top z =
        if is_root z then z else top (up z)
      in top z

    let zipper_to_tree z =
      fst (top z)

    let down (t,m) i =
      try
        let d = D.node_of_tree t in
        (D.get_child d i, (i, d) :: m)
      with _ -> invalid_arg "Zipper.down"

    let down_path z l =
      let rec down_path z = function
        | []   -> z
        | i::l -> down_path (down z i) l
      in try down_path z l with _ -> invalid_arg "Zipper.down_path"

    let min_index (t, _) =
      D.min_index (D.node_of_tree t)

    let max_index (t, _) =
      D.max_index (D.node_of_tree t)

    let down_first ((t, l) as z) =
      try
        down z (D.min_index (D.node_of_tree t))
      with _ -> invalid_arg "Zipper.down_first"

    let down_last ((t, l) as z) =
      try
        down z (D.max_index (D.node_of_tree t))
      with _ -> invalid_arg "Zipper.down_last"

    let new_child (t,m) c i =
      let d = try D.node_of_tree t with _ -> invalid_arg "Zipper.new_child" in
      if D.has_child d i
      then invalid_arg "Zipper.new_child";
      (c, (i, d) :: m)

    let new_last_child z t =
      let i = try 1 + D.max_index (D.node_of_tree t) with _ -> 0 in
      try new_child z t i with _ -> invalid_arg "Zipper.new_last_child"

    let new_first_child z t =
      let i = try 1 + D.min_index (D.node_of_tree t) with _ -> 0 in
      try new_child z t i with _ -> invalid_arg "Zipper.new_last_child"

    let has_child (t, _) i =
      try
        D.has_child (D.node_of_tree t) i
      with _ -> invalid_arg "Zipper.has_child"

    let remove_child (t, m) i =
      try
        (D.tree_of_node (D.remove_child (D.node_of_tree t) i), m)
      with _ -> invalid_arg "Zipper.remove_child"
  end
