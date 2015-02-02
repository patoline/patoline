let eq_option eq o1 o2 =
  match o1, o2 with
  | None   , None    -> true
  | Some e1, Some e2 -> eq e1 e2
  | _ -> false

let eq_list eq l1 l2 =
  try List.for_all2 eq l1 l2 with Invalid_argument _ -> false

