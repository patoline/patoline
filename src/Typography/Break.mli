val is_last : 'a Box.box array -> int -> bool
type figurePosition = Placed of Line.line | Flushed | Begun
module Make :
  functor (L : sig type t = Line.line val compare : t -> t -> int end) ->
    functor (User : Map.OrderedType) ->
      sig
        module User : sig type t = User.t val compare : t -> t -> int end
        module UMap :
          sig
            type key = User.t
            type 'a t = 'a New_map.Make(User).t
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
          completeLine:(UMap.key Box.box array array ->
                        Box.drawingBox array ->
                        figurePosition Util.IntMap.t ->
                        Line.line UMap.t ->
                        Line.line -> bool -> Line.line list)
                       array ->
          figures:Box.drawingBox array ->
          figure_parameters:(UMap.key Box.box array array ->
                             Box.drawingBox array ->
                             Line.parameters ->
                             figurePosition Util.IntMap.t ->
                             Line.line UMap.t ->
                             Line.line -> Line.parameters)
                            array ->
          parameters:(UMap.key Box.box array array ->
                      Box.drawingBox array ->
                      Line.parameters ->
                      figurePosition Util.IntMap.t ->
                      Line.line UMap.t -> Line.line -> Line.parameters)
                     array ->
          badness:(Line.line ->
                   UMap.key Box.box array ->
                   int ->
                   Line.parameters ->
                   float ->
                   Line.line ->
                   UMap.key Box.box array ->
                   int -> Line.parameters -> float -> float) ->
          UMap.key Box.box array array ->
          Language.message list * (Line.parameters * Line.line) list array *
          figurePosition Util.IntMap.t * Line.line UMap.t
      end
