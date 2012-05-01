(** Types des dessins, de la structure du document,… La bonne méthode pour dessiner dans un document est d'utiliser {!Boxes.drawing} sur une liste de {!contents} *)
type lineCap = Butt_cap | Round_cap | Proj_square_cap
type lineJoin = Miter_join | Round_join | Bevel_join
type rgb = { red : float; green : float; blue : float; }
type color = RGB of rgb
val rgb:float->float->float->color
val black : color
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
  link_x0 : float;
  link_y0 : float;
  link_x1 : float;
  link_y1 : float;
  dest_page : int;
  dest_x : float;
  dest_y : float;
}
type contents =
    Glyph of glyph
  | Path of path_parameters * Bezier.curve array list
  | Link of link
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
