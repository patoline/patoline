(**
 * Common structures and functions used to implement Patoline drivers.
 *
 * A {e driver} in Patoline implements the very last part of the
 * document processing, after the document content has been typeset and
 * arranged into an array of pages. The driver is responsible of
 * serializing the content to some output medium, such as a PDF
 * document, a SVG image, or even displaying the content on screen.
 *)

(** Type of a single page. *)
type page = {
  mutable size : float * float;
  (** Size of a page, given as a pair of floats [(width, height)]
   * expressed in millimeters. *)

  mutable contents : RawContent.raw list;
  (** Typeset content belonging to the page. *)
}

(** Creates an empty page of the given size. *)
val empty_page : (float * float) -> page

(** Available metadata fields to appear in the structure of the document. *)
type meta_field =
  | Contributor | Coverage | Creator | Date | Description | Format
  | Identifier | Language | Publisher | Relation | Rights | Source
  | Subject | Title | Type

(** Type representing the structure of a document. It contains metadata
 * but also position of sections and chapters. *)
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

(** Dummy structure with no information. *)
val empty_structure : structure

(** Debuging function which prints to stdout the given structure. *)
val print_structure : structure -> unit

(** Interface of a Patoline driver. *)
module type OutputDriver =
  sig
    val output : ?structure:structure -> page array -> string -> unit
    val output' : ?structure:structure -> page array array -> string -> unit
  end

(** Function providing a more or less cannonical way of builing an [output']
 * function given an [output] function. *)
val output_to_prime : (?structure:structure -> page array -> 'b -> 'c)
  -> ?structure:structure -> page array array -> 'b -> 'c

(** Similar to [output_to_prime], but going the other direction. *)
val output_from_prime :
  (?structure:structure -> 'a array array -> 'b -> 'c) ->
  ?structure:structure -> 'a array -> 'b -> 'c

(** File name of an alternative pre-compiled input. *)
val input_bin : string option ref
(** When using the {!module:Bin} driver, the final typeset document
 * content is saved as marshalled content. It can be loaded later and
 * fed to another driver which outputs the actual document without
 * rebuilding the whole document. This reference holds the name of the
 * file which holds the marshalled content. When it is not [None], most
 * formats prefer to load this content instead of doing the whole
 * document optimisation (the marshalled content is loaded even if the
 * source document has changed). *)

(** Name of the driven used to compile the current document. *)
val driver : string option ref
(** This value is usually set using the ["--driver"] command-line option
    when running patoline. *)
