val is_last : 'a Util.box array -> int -> bool
type figurePosition = Placed of Util.line | Flushed | Begun
module Make :
  functor (Line : sig type t = Util.line val compare : t -> t -> int end) ->
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
        module LineMap :
          sig
            type key = Line.t
            type 'a t = 'a New_map.Make(Line).t
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
            type key = float * float * Util.line * float * float * Util.line
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
        val haut : UMap.key Util.box array ref
        val max_haut : int ref
        val bas : UMap.key Util.box array ref
        val max_bas : int ref
        val writeBox : 'a Util.box array ref -> int -> 'a Util.box -> unit
        val readBox : 'a array ref -> int -> 'a
        val typeset :
          completeLine:(UMap.key Util.box array array ->
                        Util.drawingBox array ->
                        figurePosition Binary.IntMap.t ->
                        LineMap.key UMap.t ->
                        Util.line -> bool -> LineMap.key list)
                       array ->
          figures:Util.drawingBox array ->
          figure_parameters:(UMap.key Util.box array array ->
                             Util.drawingBox array ->
                             Util.parameters ->
                             figurePosition Binary.IntMap.t ->
                             LineMap.key UMap.t ->
                             Util.line -> Util.parameters)
                            array ->
          parameters:(UMap.key Util.box array array ->
                      Util.drawingBox array ->
                      Util.parameters ->
                      figurePosition Binary.IntMap.t ->
                      LineMap.key UMap.t -> LineMap.key -> Util.parameters)
                     array ->
          badness:(LineMap.key ->
                   UMap.key Util.box array ->
                   int ->
                   Util.parameters ->
                   float ->
                   LineMap.key ->
                   UMap.key Util.box array ->
                   int -> Util.parameters -> float -> float) ->
          UMap.key Util.box array array ->
          Log.error_log list * (Util.parameters * LineMap.key) list array *
          LineMap.key UMap.t
      end
