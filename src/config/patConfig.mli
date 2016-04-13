type patoconfig =
  { fonts_dir    : string * string list
  ; plugins_dir  : string * string list
  ; grammars_dir : string * string list
  ; hyphen_dir   : string * string list }

val get_patoconfig : unit -> patoconfig
