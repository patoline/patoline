type page = {
  mutable pageFormat : float * float;
  mutable pageContents : OutputCommon.contents list;
}
module type Driver =
  sig
    val filename : string -> string
    val output :
      ?structure:OutputCommon.structure -> page array -> string -> unit
  end
module Output :
  functor (M : Driver) ->
    sig
      val output :
        'a Document.tree ->
        Document.user Util.box array array ->
        Util.drawingBox array ->
        'b Document.environment ->
        (Util.parameters * Util.line) list array -> string -> unit
    end
