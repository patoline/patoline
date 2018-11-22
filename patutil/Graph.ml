
module Make (X : Set.OrderedType) = struct

  type color = Visited | Visiting | Virgin

  type node = { id : X.t ; mutable sons : node list ; mutable color : color }

  type graph = node list

  open Stack

  let total_order graph =
    let rec visit res stack node =
      if node.color = Visited
      then
        continue res stack
      else
        if (node.color = Visiting) || (List.for_all (fun son -> son.color = Visited) node.sons)
        then let _ = node.color <- Visited in continue (node :: res) stack
        else
    let _ = node.color <- Visiting in
    let _ = push node stack in
    match node.sons with
      | [] -> assert false  (* Because we've checked that not all sons are already visited *)
      | x :: sons ->
        let _ = List.iter (fun son -> push son stack) sons in
        visit res stack x
    and continue res stack =
      if is_empty stack
      then res
      else
        visit res stack (pop stack)
    in
    let stack = create () in
    let _ = List.iter
      (fun node -> push node stack)
      graph
    in
    let res = continue [] stack in
    List.map (fun node -> node.id) res



end
