(** The main algorithm of TeX' (line-breaking and page-breaking) *)

open Util
exception Impossible


val compression:box array array->(parameters*line)->float
val lineBreak :
  measure:(line->float)->
  parameters:(line->parameters) ->
  ?figures:drawingBox array ->
  box array array ->
  (error_log list)*((parameters*line) list array)
