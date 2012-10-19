(** Types des dessins, de la structure du document,… La bonne méthode pour dessiner dans un document est d'utiliser {!Box.drawingBox} sur une liste de {!contents} *)
type lineCap = Butt_cap | Round_cap | Proj_square_cap
type lineJoin = Miter_join | Round_join | Bevel_join
type rgb = { red : float; green : float; blue : float; }
type color = RGB of rgb
val rgb:float->float->float->color
val mix:float->color->color->color
val black : color
val white : color
val black : color
val blue : color
val green : color
val red : color
val yellow : color
val gray : color

type path_parameters = {
  close : bool;
  strokingColor : color option;
  fillColor : color option;
  lineCap : lineCap;
  lineJoin : lineJoin;
  lineWidth : float;
  dashPattern : float list;
}
val default : path_parameters
type glyph = {
  glyph_x : float;
  glyph_y : float;
  glyph_color : color;
  glyph_size : float;
  glyph : Fonts.glyph;
}
type image= { image_file:string; image_x:float; image_y:float; image_height:float;image_width:float }
type metadata=
    Contributor
  | Coverage
  | Creator
  | Date
  | Description
  | Format
  | Identifier
  | Language
  | Publisher
  | Relation
  | Rights
  | Source
  | Subject
  | Title
  | Type

type link = {
  mutable link_x0 : float;
  mutable link_y0 : float;
  mutable link_x1 : float;
  mutable link_y1 : float;
  uri:string;is_internal:bool;
  dest_page : int;
  dest_x : float;
  dest_y : float;
  link_contents : contents list
}

and contents =
    Glyph of glyph
  | Path of path_parameters * Bezier.curve array list
  | Link of link
  | Image of image
  | States of contents list*Util.IntSet.t

val translate : float -> float -> contents -> contents
val resize : float -> contents -> contents

type bounding_box_opt = {
  ignore_negative_abcisse : bool;
  ignore_after_glyphWidth : bool;
  ignore_under_base_line : bool}

val bounding_box_opt : bounding_box_opt -> contents list -> float * float * float * float
val bounding_box : contents list -> float * float * float * float
val bounding_box_kerning : contents list -> float * float * float * float
val bounding_box_full : contents list -> float * float * float * float

val circle : float -> (float array * float array) array
val rectangle : (float*float) -> (float*float)->(float array * float array) array

type structure = {
  mutable name : string;
  mutable metadata : (metadata*string) list;
  mutable displayname : contents list;
  mutable tags:(string*string) list;
  mutable page : int;
  mutable struct_x : float;
  mutable struct_y : float;
  mutable substructures : structure array;
}
val print_structure : structure -> unit

val output_to_prime : 
  (?structure:structure -> 'b array -> 'c -> 'd) -> 
    ?structure:structure -> 'b array array -> 'c -> 'd

val output_from_prime : 
  (?structure:structure -> 'b array array -> 'c -> 'd) ->
    ?structure:structure -> 'b array -> 'c -> 'd
