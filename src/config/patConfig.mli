type patoconfig =
  { fonts_dir    : string * string list
  ; plugins_dir  : string * string list
  ; grammars_dir : string * string list
  ; hyphen_dir   : string * string list
  ; drivers      : string list
  ; has_patonet  : bool
  ; max_iter     : int
  ; user_dir     : string }

val get_patoconfig : unit -> patoconfig
