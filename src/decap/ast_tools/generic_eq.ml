open Asttypes
open Parsetree
open Longident

(* Generic functions *)
let eq_option eq o1 o2 =
  match o1, o2 with
  | None   , None    -> true
  | Some e1, Some e2 -> eq e1 e2
  | _ -> false

let eq_list eq l1 l2 =
  try List.for_all2 eq l1 l2 with Invalid_argument _ -> false

let rec eq_longident i1 i2 =
  match i1, i2 with
  | Lident s1     , Lident s2      -> s1 = s2
  | Ldot (e1,s1)  , Ldot (e2, s2)  -> eq_longident e1 e2 && s1 = s2
  | Lapply (e1,f1), Lapply (e2,f2) -> eq_longident e1 e2 && eq_longident f1 f2
  | _ -> false

