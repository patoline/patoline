type figurePosition = Placed of Line.line | Flushed | Begun
module type Line =
  sig type t val compare : t -> t -> int val hash : t -> int end
module Make :
  functor
    (L : sig
           type t = Line.line
           val compare : t -> t -> int
           val hash : t -> int
         end) ->
    sig
      module LineMap :
        sig
          type key = L.t
          type 'a t = 'a Map.Make(L).t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val max_binding : 'a t -> key * 'a
          val choose : 'a t -> key * 'a
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        end
      module ColMap :
        sig
          type key = float * float * Line.line * float * float * Line.line
          type +'a t
          val empty : 'a t
          val is_empty : 'a t -> bool
          val mem : key -> 'a t -> bool
          val add : key -> 'a -> 'a t -> 'a t
          val singleton : key -> 'a -> 'a t
          val remove : key -> 'a t -> 'a t
          val merge :
            (key -> 'a option -> 'b option -> 'c option) ->
            'a t -> 'b t -> 'c t
          val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
          val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
          val iter : (key -> 'a -> unit) -> 'a t -> unit
          val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
          val for_all : (key -> 'a -> bool) -> 'a t -> bool
          val exists : (key -> 'a -> bool) -> 'a t -> bool
          val filter : (key -> 'a -> bool) -> 'a t -> 'a t
          val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
          val cardinal : 'a t -> int
          val bindings : 'a t -> (key * 'a) list
          val min_binding : 'a t -> key * 'a
          val max_binding : 'a t -> key * 'a
          val choose : 'a t -> key * 'a
          val split : key -> 'a t -> 'a t * 'a option * 'a t
          val find : key -> 'a t -> 'a
          val map : ('a -> 'b) -> 'a t -> 'b t
          val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        end
      val typeset :
        completeLine:(Box.box array array ->
                      Box.drawingBox array ->
                      figurePosition Util.IntMap.t ->
                      L.t Box.UserMap.t -> Line.line -> bool -> L.t list)
                     array ->
        figures:Box.drawingBox array ->
        figure_parameters:(Box.box array array ->
                           Box.drawingBox array ->
                           Line.parameters ->
                           figurePosition Util.IntMap.t ->
                           L.t Box.UserMap.t ->
                           LineMap.key -> Line.line -> Line.parameters)
                          array ->
        parameters:(Box.box array array ->
                    Box.drawingBox array ->
                    Line.parameters ->
                    figurePosition Util.IntMap.t ->
                    L.t Box.UserMap.t ->
                    LineMap.key -> L.t -> Line.parameters)
                   array ->
        badness:(Box.box array array ->
                 Box.drawingBox array ->
                 figurePosition Util.IntMap.t ->
                 LineMap.key ->
                 Box.box array ->
                 int ->
                 Line.parameters ->
                 float ->
                 L.t ->
                 Box.box array -> int -> Line.parameters -> float -> float)
                array ->
        Box.box array array ->
        TypoLanguage.message list * (Line.parameters * L.t) list array *
        figurePosition Util.IntMap.t * L.t Box.UserMap.t
    end
