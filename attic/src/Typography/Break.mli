(** Line and page breaking algorithm *)

type optim_error =
  | Normal
  | No_solution    of string
  | Overfull_line  of string
  | Underfull_line of string
  | Widow          of string
  | Orphan         of string

val message : optim_error -> string

val is_last : Box.box array -> int -> bool

type figurePosition = Placed of Box.line | Flushed | Begun

module type OrderedHashableType =
  sig
    type t
    val compare : t -> t -> int
    val hash : t -> int
  end

module Make(Line : OrderedHashableType with type t = Box.line) :
  sig
    module LineMap : Map.S with type key = Line.t
    module ColMap  : Map.S with type key = float * float * Box.line * float * float * Box.line

    val print_graph : string -> 'a
      -> ('b * 'c * 'd * 'e * 'f * LineMap.key * 'g * 'h) LineMap.t
      -> ('i * LineMap.key) list -> unit

    val typeset :
        ?initial_line:Box.line->
        completeLine:(Box.box array array ->
                      Box.drawingBox array ->
                      figurePosition Extra.IntMap.t ->
                      Line.t Box.MarkerMap.t -> Box.line -> bool -> Line.t list)
                     array ->
        figures:Box.drawingBox array ->
        figure_parameters:(Box.box array array ->
                           Box.drawingBox array ->
                           Box.parameters ->
                           figurePosition Extra.IntMap.t ->
                           Line.t Box.MarkerMap.t ->
                           LineMap.key -> Box.line -> Box.parameters)
                          array ->
        parameters:(Box.box array array ->
                    Box.drawingBox array ->
                    Box.parameters ->
                    figurePosition Extra.IntMap.t ->
                    Line.t Box.MarkerMap.t ->
                    LineMap.key -> Line.t -> Box.parameters)
                   array ->
        new_page:(Box.frame_zipper -> Box.frame_zipper) array ->
        new_line:(Box.line->Box.parameters->
                  Box.line->Box.parameters->Box.frame_zipper->
                  float->float) array ->
        badness:(Box.box array array ->
                 Box.drawingBox array ->
                 figurePosition Extra.IntMap.t ->
                 LineMap.key ->
                 Box.box array ->
                 int ->
                 Box.parameters ->
                 float ->
                 Line.t ->
                 Box.box array -> int -> Box.parameters -> float -> float)
             array ->
        states:int list array ->
        Box.box array array ->
        optim_error list *
          Box.frame *
          figurePosition Extra.IntMap.t * Line.t Box.MarkerMap.t
  end
