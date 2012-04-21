(** Sortie avec des pages, des numéros, etc, pour du papier ou assimilés *)
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
        ('a,Document.tag) Document.tree ->
        Document.user Boxes.box array array ->
        Boxes.drawingBox array ->
        'a Document.environment ->
        (Boxes.parameters * Boxes.line) list array -> string -> unit
    end
