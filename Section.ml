open Binary

type index = int IntMap.t

let increment lvl index:index =
  let cur_index =
    try IntMap.find lvl index with Not_found -> 0 
  in
  let index = IntMap.add lvl (cur_index + 1) index in
  let index = IntMap.fold (fun l _ acc ->
    if l < lvl then IntMap.remove l acc else acc) index index in
  index

let index_to_string subst kern font fsize index =
  let str = 
    IntMap.fold (fun l s acc ->
      string_of_int s ^ "." ^ acc) index ""
  in
  Util.glyph_of_string subst kern font fsize str

