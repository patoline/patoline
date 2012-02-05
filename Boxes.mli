(** The main algorithm of TeX' (line-breaking and page-breaking) *)

open Util
exception Impossible

val lineBreak :
  (line->parameters) ->
  ?figures:'a array -> box array array -> (parameters * ((float * float * box) list)) array array
