module Buf :
  sig
    type buf = Buffer.t
    val create : int -> buf
    val contents : buf -> CamomileLibrary.UTF8.t
    val clear : buf -> unit
    val reset : buf -> unit
    val add_char : buf -> CamomileLibrary.UChar.t -> unit
    val add_string : buf -> CamomileLibrary.UTF8.t -> unit
    val add_buffer : buf -> buf -> unit
  end
type lineCap = Butt_cap | Round_cap | Proj_square_cap
type lineJoin = Miter_join | Round_join | Bevel_join
type rgb = { red : float; green : float; blue : float; }
type color = RGB of rgb
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
  | Path of path_parameters * Bezier.curve array
  | Link of link
type structure = {
  mutable name : string;
  mutable displayname :contents list;
  mutable page : int;
  mutable struct_x : float;
  mutable struct_y : float;
  mutable substructures : structure array;
}
val translate : float -> float -> contents -> contents
val bounding_box : contents list -> float * float * float * float
type page = {
  mutable pageFormat : float * float;
  mutable pageContents : contents list;
}
module type Driver =
  sig
    val filename : string -> string
    val output : ?structure:structure -> page array -> string -> unit
  end
module Pdf : Driver
