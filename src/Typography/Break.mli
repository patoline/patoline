val is_last : Box.box array -> int -> bool
type figurePosition = Placed of Layout.line | Flushed | Begun
module type Line =
  sig type t val compare : t -> t -> int val hash : t -> int end
module Make :
  functor
    (L : sig
           type t = Layout.line
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
          type key =
              float * float * Layout.line * float * float * Layout.line
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
      val print_graph :
        string ->
        'a ->
        ('b * 'c * 'd * 'e * 'f * LineMap.key * 'g * 'h) LineMap.t ->
        ('i * LineMap.key) list -> unit
      val typeset :
        completeLine:(Box.box array array ->
                      Box.drawingBox array ->
                      figurePosition Util.IntMap.t ->
                      L.t Box.UserMap.t -> Layout.line -> bool -> L.t list)
                     array ->
        figures:Box.drawingBox array ->
        figure_parameters:(Box.box array array ->
                           Box.drawingBox array ->
                           Layout.parameters ->
                           figurePosition Util.IntMap.t ->
                           L.t Box.UserMap.t ->
                           LineMap.key -> Layout.line -> Layout.parameters)
                          array ->
        parameters:(Box.box array array ->
                    Box.drawingBox array ->
                    Layout.parameters ->
                    figurePosition Util.IntMap.t ->
                    L.t Box.UserMap.t ->
                    LineMap.key -> L.t -> Layout.parameters)
                   array ->
        new_page:(Layout.frame_zipper -> Layout.frame_zipper) array ->
        new_line:(Layout.line->Layout.parameters->
                  Layout.line->Layout.parameters->float->float) array ->
        badness:(Box.box array array ->
                 Box.drawingBox array ->
                 figurePosition Util.IntMap.t ->
                 LineMap.key ->
                 Box.box array ->
                 int ->
                 Layout.parameters ->
                 float ->
                 L.t ->
                 Box.box array -> int -> Layout.parameters -> float -> float)
                array ->
        Box.box array array ->
        TypoLanguage.message list *
          ((Layout.frame_zipper*Layout.placed_line list) array) *
          figurePosition Util.IntMap.t * L.t Box.UserMap.t
    end
