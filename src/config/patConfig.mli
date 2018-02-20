type patoconfig =
  { mutable fonts_dir    : string * string list
  ; mutable grammars_dir : string * string list
  ; mutable hyphen_dir   : string * string list
  ; mutable drivers      : string list
  ; mutable formats      : string list
  ; mutable has_patonet  : bool
  ; mutable max_iter     : int
  ; mutable user_dir     : string }

val patoconfig : patoconfig

(* These functions may raise [Not_found]. *)
val findFont    : string -> string
val findGrammar : string -> string
val findHyphen  : string -> string

val add_fonts_dir    : string -> unit
val add_grammars_dir : string -> unit
val add_hyphen_dir   : string -> unit

val print_config : out_channel -> unit
