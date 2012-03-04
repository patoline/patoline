(** The main algorithm of TeX' (line-breaking and page-breaking) *)

open Util
exception Impossible


val typeset :
  completeLine:(box array array -> line -> bool -> line list) array->
  parameters:(box array array -> drawingBox array -> parameters->line->parameters) array ->
  ?badness:(line->parameters->line->parameters->float) ->
  ?figures:drawingBox array ->
  box array array ->
  (error_log list)*((parameters*line) list array)
