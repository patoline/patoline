(** Sortie avec des pages, des numéros, etc, pour du papier ou assimilés *)
type page = {
  mutable pageFormat : float * float;
  mutable pageContents : OutputCommon.contents list;
}
val defaultPage:page

module type Driver =
  sig
    val filename : string -> string
    val output :
      ?structure:OutputCommon.structure -> page array -> string -> unit
  end
