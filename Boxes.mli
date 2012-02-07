(** The main algorithm of TeX' (line-breaking and page-breaking) *)

open Util
exception Impossible

type pages=((parameters * ((float * float * box) list)) array array)

val compression:box array array->(parameters*line)->float
val lineBreak :
  (line->parameters) ->
  ?figures:drawingBox array ->
  box array array ->
  (error_log list)*((parameters*line) list array)
