module type User =
  sig type t val compare : t -> t -> int val citation : int -> t end
module Make :
  functor (User : User) ->
    sig
      val typeset :
        completeLine:(UMap.key Util.box array array ->
                      Util.line -> bool -> Util.LineMap.key list)
                     array ->
        parameters:(UMap.key Util.box array array ->
                    Util.drawingBox array ->
                    Util.parameters -> Util.LineMap.key -> Util.parameters)
                   array ->
        ?badness:(Util.LineMap.key ->
                  Util.parameters ->
                  Util.LineMap.key -> Util.parameters -> float) ->
        ?figures:Util.drawingBox array ->
        UMap.key Util.box array array ->
        Util.error_log list * (Util.parameters * Util.LineMap.key) list array
    end
