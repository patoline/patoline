val is_last : 'a Boxes.box array -> int -> bool
type figurePosition = Placed of Boxes.line | Flushed | Begun
module Make :
  functor (Line : sig type t = Boxes.line val compare : t -> t -> int end) ->
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
          completeLine:(UMap.key Boxes.box array array ->
                        Boxes.drawingBox array ->
                        figurePosition Util.IntMap.t ->
                        Boxes.line UMap.t ->
                        Boxes.line -> bool -> Boxes.line list)
                       array ->
          figures:Boxes.drawingBox array ->
          figure_parameters:(UMap.key Boxes.box array array ->
                             Boxes.drawingBox array ->
                             Boxes.parameters ->
                             figurePosition Util.IntMap.t ->
                             Boxes.line UMap.t ->
                             Boxes.line -> Boxes.parameters)
                            array ->
          parameters:(UMap.key Boxes.box array array ->
                      Boxes.drawingBox array ->
                      Boxes.parameters ->
                      figurePosition Util.IntMap.t ->
                      Boxes.line UMap.t -> Boxes.line -> Boxes.parameters)
                     array ->
          badness:(Boxes.line ->
                   UMap.key Boxes.box array ->
                   int ->
                   Boxes.parameters ->
                   float ->
                   Boxes.line ->
                   UMap.key Boxes.box array ->
                   int -> Boxes.parameters -> float -> float) ->
          UMap.key Boxes.box array array ->
          Log.error_log list * (Boxes.parameters * Boxes.line) list array *
          figurePosition Util.IntMap.t * Boxes.line UMap.t
      end
