(* Type of a single page. *)
type page = {
  mutable size : float * float;
  mutable contents : RawContent.raw list;
}

(* Creates an empty page of the given size. *)
val empty_page : (float * float) -> page

(* Available metadata fields to appear in the structure of the document. *)
type meta_field =
  | Contributor | Coverage | Creator | Date | Description | Format
  | Identifier | Language | Publisher | Relation | Rights | Source
  | Subject | Title | Type

(* Type representing the structure of a document. It contains metadata but
also position of sections / chapters. *)
type structure = {
  mutable name : string;
  mutable metadata : (meta_field * string) list;
  mutable raw_name : RawContent.raw list;
  mutable tags : (string * string) list;
  mutable page : int;
  mutable struct_x : float;
  mutable struct_y : float;
  mutable children : structure array;
}

(* Dummy structure with no information. *)
val empty_structure : structure

(* Debuging function. *)
val print_structure : structure -> unit

(* Interface of a Patoline driver. *)
module type OutputDriver =
  sig
    val output : ?structure:structure -> page array -> string -> unit
    val output' : ?structure:structure -> page array array -> string -> unit
  end

(* Function providing a more or less cannonical way of builing an "output'"
function given an "output" function. *)
val output_to_prime : (?structure:structure -> page array -> 'b -> 'c)
  -> ?structure:structure -> page array array -> 'b -> 'c

(* Similar, but going the other direction. *)
val output_from_prime :
  (?structure:structure -> 'a array array -> 'b -> 'c) ->
  ?structure:structure -> 'a array -> 'b -> 'c
