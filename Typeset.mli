module type User =
  sig
    type t
    val compare : t -> t -> int
    val figureRef : int -> t
    val figure : int -> t
    val beginFigure : t -> int
    val flushedFigure : t -> int
    val isFigure : t -> bool
    val figureNumber : t -> int
  end
module type Typeset =
  sig
    module User : User
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
      completeLine:(UMap.key Util.box array array ->
                    Util.drawingBox array ->
                    Util.line UMap.t -> Util.line -> bool -> Util.line list)
                   array ->
      figures:Util.drawingBox array ->
      figure_parameters:(UMap.key Util.box array array ->
                         Util.drawingBox array ->
                         Util.parameters ->
                         Util.line UMap.t -> Util.line -> Util.parameters)
                        array ->
      parameters:(UMap.key Util.box array array ->
                  Util.drawingBox array ->
                  Util.parameters ->
                  Util.line UMap.t -> Util.line -> Util.parameters)
                 array ->
      badness:(Util.line ->
               UMap.key Util.box array ->
               int ->
               Util.parameters ->
               float ->
               Util.line ->
               UMap.key Util.box array ->
               int -> Util.parameters -> float -> float) ->
      UMap.key Util.box array array ->
      Util.error_log list * (Util.parameters * Util.line) list array *
      Util.line UMap.t
  end
module Make :
  functor (Line : sig type t = Util.line val hash:t->int val compare : t -> t -> int end) ->
    functor (User : User) ->
      sig
        module User :
          sig
            type t = User.t
            val compare : t -> t -> int
            val figureRef : int -> t
            val figure : int -> t
            val beginFigure : t -> int
            val flushedFigure : t -> int
            val isFigure : t -> bool
            val figureNumber : t -> int
          end
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
          completeLine:(UMap.key Util.box array array ->
                        Util.drawingBox array ->
                        Util.line UMap.t ->
                        Util.line -> bool -> Util.line list)
                       array ->
          figures:Util.drawingBox array ->
          figure_parameters:(UMap.key Util.box array array ->
                             Util.drawingBox array ->
                             Util.parameters ->
                             Util.line UMap.t ->
                             Util.line -> Util.parameters)
                            array ->
          parameters:(UMap.key Util.box array array ->
                      Util.drawingBox array ->
                      Util.parameters ->
                      Util.line UMap.t -> Util.line -> Util.parameters)
                     array ->
          badness:(Util.line ->
                   UMap.key Util.box array ->
                   int ->
                   Util.parameters ->
                   float ->
                   Util.line ->
                   UMap.key Util.box array ->
                   int -> Util.parameters -> float -> float) ->
          UMap.key Util.box array array ->
          Util.error_log list * (Util.parameters * Util.line) list array *
          Util.line UMap.t
      end
