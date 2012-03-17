val print_graph :
  string ->
  'a ->
  (float * 'b * Util.LineMap.key * 'c) Util.LineMap.t ->
  ('d * Util.LineMap.key) list -> unit
val print_simple_graph :
  string -> 'a -> (float * 'b * Util.LineMap.key * 'c) Util.LineMap.t -> unit
val is_last : 'a Util.box array -> int -> bool
module type User =
  sig
    type t
    val compare : t -> t -> int
    val figureRef : int -> t
    val figure : int -> t
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
    val haut : UMap.key Util.box array ref
    val max_haut : int ref
    val bas : UMap.key Util.box array ref
    val max_bas : int ref
    val writeBox : 'a Util.box array ref -> int -> 'a Util.box -> unit
    val readBox : 'a array ref -> int -> 'a
    val typeset :
      completeLine:(UMap.key Util.box array array ->
                    Util.drawingBox array ->
                    Util.LineMap.key UMap.t ->
                    Util.line -> bool -> Util.LineMap.key list)
                   array ->
      figures:Util.drawingBox array ->
      figure_parameters:(UMap.key Util.box array array ->
                         Util.drawingBox array ->
                         Util.parameters ->
                         Util.LineMap.key UMap.t ->
                         Util.line -> Util.parameters)
                        array ->
      parameters:(UMap.key Util.box array array ->
                  Util.drawingBox array ->
                  Util.parameters ->
                  Util.LineMap.key UMap.t ->
                  Util.LineMap.key -> Util.parameters)
                 array ->
      badness:(Util.LineMap.key ->
               UMap.key Util.box array ->
               int ->
               Util.parameters ->
               float ->
               Util.LineMap.key ->
               UMap.key Util.box array ->
               int -> Util.parameters -> float -> float) ->
      UMap.key Util.box array array ->
      Util.error_log list * (Util.parameters * Util.LineMap.key) list array *
      Util.LineMap.key UMap.t
  end
module Make :
  functor (User : User) ->
    sig
      module User :
        sig
          type t = User.t
          val compare : t -> t -> int
          val figureRef : int -> t
          val figure : int -> t
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
      val haut : UMap.key Util.box array ref
      val max_haut : int ref
      val bas : UMap.key Util.box array ref
      val max_bas : int ref
      val writeBox : 'a Util.box array ref -> int -> 'a Util.box -> unit
      val readBox : 'a array ref -> int -> 'a
      val typeset :
        completeLine:(UMap.key Util.box array array ->
                      Util.drawingBox array ->
                      Util.LineMap.key UMap.t ->
                      Util.line -> bool -> Util.LineMap.key list)
                     array ->
        figures:Util.drawingBox array ->
        figure_parameters:(UMap.key Util.box array array ->
                           Util.drawingBox array ->
                           Util.parameters ->
                           Util.LineMap.key UMap.t ->
                           Util.line -> Util.parameters)
                          array ->
        parameters:(UMap.key Util.box array array ->
                    Util.drawingBox array ->
                    Util.parameters ->
                    Util.LineMap.key UMap.t ->
                    Util.LineMap.key -> Util.parameters)
                   array ->
        badness:(Util.LineMap.key ->
                 UMap.key Util.box array ->
                 int ->
                 Util.parameters ->
                 float ->
                 Util.LineMap.key ->
                 UMap.key Util.box array ->
                 int -> Util.parameters -> float -> float) ->
        UMap.key Util.box array array ->
        Util.error_log list *
        (Util.parameters * Util.LineMap.key) list array *
        Util.LineMap.key UMap.t
    end
