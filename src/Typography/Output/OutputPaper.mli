type page = {
  mutable pageFormat : float * float;
  mutable pageContents : OutputCommon.raw list;
}
val defaultPage:page

module type Driver =
  sig
    val output :
      ?structure:OutputCommon.structure -> page array -> string -> unit
    val output': 
      ?structure:OutputCommon.structure -> page array array -> string -> unit

  end

val drivers : (string, (module Driver)) Hashtbl.t

val load_driver : string -> unit
