type patoconfig =
  { mutable fonts_dir    : string * string list
  ; mutable plugins_dir  : string * string list
  ; mutable grammars_dir : string * string list
  ; mutable hyphen_dir   : string * string list
  ; mutable drivers      : string list
  ; mutable has_patonet  : bool
  ; mutable max_iter     : int
  ; mutable user_dir     : string }

val patoconfig : patoconfig

exception File_not_found of string

val findFont    : string -> string
val findPlugin  : string -> string
val findGrammar : string -> string
val findHyphen  : string -> string

val add_fonts_dir    : string -> unit
val add_plugins_dir  : string -> unit
val add_grammars_dir : string -> unit
val add_hyphen_dir   : string -> unit
