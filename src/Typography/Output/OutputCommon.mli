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
type link = {
  mutable link_x0 : float;
  mutable link_y0 : float;
  mutable link_x1 : float;
  mutable link_y1 : float;
  uri:string;
  dest_page : int;
  dest_x : float;
  dest_y : float;
}
type image= { image_file:string; image_x:float; image_y:float; image_height:float;image_width:float }

type contents =
    Glyph of glyph
  | Path of path_parameters * Bezier.curve array list
  | Link of link
  | Image of image

val translate : float -> float -> contents -> contents
val resize : float -> contents -> contents
val bounding_box : contents list -> float * float * float * float
val circle : float -> (float array * float array) array
val rectangle : (float*float) -> (float*float)->(float array * float array) array
type structure = {
  mutable name : string;
  mutable displayname : contents list;
  mutable page : int;
  mutable struct_x : float;
  mutable struct_y : float;
  mutable substructures : structure array;
}
