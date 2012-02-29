(** The main algorithm of TeX' (line-breaking and page-breaking) *)

open Util
exception Impossible


val typeset :
  measure:(line->float)->
  parameters:(parameters->line->float->float->float->parameters) ->
  ?badness:(line->parameters->line->parameters->float) ->
  ?figures:drawingBox array ->
  box array array ->
  (error_log list)*((parameters*line) list array)
