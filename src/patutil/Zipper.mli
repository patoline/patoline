(** Module implementing zippers on arbitrarily branching trees. *)

(** Module defining the types carried by the node or leaf of a tree. *)
module type TreeData =
  sig
    type node
    type leaf
  end

(** Functor to build tree and zipper data structurs with data on the nodes
    and on the leaves. *)
module Make(D : TreeData) :
  sig
    type node = D.node
    type leaf = D.leaf

    (** Type of a tree. *)
    type tree =
      (** A node carries data and a map of children. *)
      | Node of (node * tree UsualMake.IntMap.t)
      (** A leaf only carries data. *)
      | Leaf of leaf

    (** Build a node with no children using the provided node data. *)
    val node : node -> tree

    (** Build a leaf using the provided node data. *)
    val leaf : leaf -> tree

    (** Type of a zipper. *)
    type zipper = tree * (int * (node * tree UsualMake.IntMap.t)) list

    (** Convert a tree into a zipper pointing at its root. *)
    val tree_to_zipper : tree -> zipper

    (** Obtain the tree represented by the given zipper. *)
    val zipper_to_tree : zipper -> tree

    (** Build an empty zipper consisting of a single node with no child. *)
    val empty : node -> zipper

    (** Return [true] if the zipper points at the root of the tree. *)
    val is_root : zipper -> bool

    (** Return [true] if the zipper points to a node. *)
    val is_node : zipper -> bool

    (** Go up one level in the zipper. Raises [Invalid_argument] if the
        zipper points at the root of the tree. *)
    val up : zipper -> zipper

    (** Applies [up] the given number of times. Raises [Invalid_argument] if
        the zipper reaches the root of the tree too soon. *)
    val up_n : int -> zipper -> zipper

    (** Move the zipper to the root of the tree. *)
    val top : zipper -> zipper

    (** Move down the i-th branch of the pointed tree. This function raises
        [Invalid_argument] if no such branch exist, or if the pointed tree
        is not a node. *)
    val down : zipper -> int -> zipper

    (** Move down branches along the given path. Raises [Invalid_argument] if
        the path is not valid. *)
    val down_path : zipper -> int list -> zipper

    (** Return the list of valid children indices for the pointed tree.
        Raises [Invalid_argument] if the pointed tree is not a node. *)
    val indices : zipper -> int list

    (** Return the minimum child index for the pointed tree. Raises
        [Invalid_argument] if the pointed tree is not a node. *)
    val min_index : zipper -> int

    (** Return the maximum child index for the pointed tree. Raises
        [Invalid_argument] if the pointed tree is not a node. *)
    val max_index : zipper -> int

    (** Move down the child with the lowest index. The function raises
       [Invalid_argument] if the pointed tree is not a node or if it does
       not have any child. *)
    val down_first : zipper -> zipper

    (** Same as [down_first] with the last child. *)
    val down_last : zipper -> zipper

    (** Add a new child under the pointed node, at the given index. If the
        pointed tree is not a node, or if there is already a child at the
        given index, the exception [Invalid_argument] is raised. *)
    val new_child : zipper -> tree -> int -> zipper

    (** Add a new child with the highest indext under the current node. The
        exception [Invalid_argument] is raised if the pointed tree is not
        a node. *)
    val new_last_child : zipper -> tree -> zipper

    (** Same as [new_last_child] but the inserted child will have the lowest
        index under the node. *)
    val new_first_child : zipper -> tree -> zipper

    (** Return true if the pointed node has a child at the given index. The
        exception [Invalid_argument] is raised if the pointed tree is not a
        node. *)
    val has_child : zipper -> int -> bool

    (** Remove the child at the given index in the node pointed to by the
        zipper. If the zipper does not point to a node, or if the child
        does not exist, the exception [Invalid_argument] is raised. *)
    val remove_child : zipper -> int -> zipper
  end
