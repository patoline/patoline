(** Output drivers. The game here is to build the whole document in
    memory, then to pass it to an output driver. This is because
    drivers are allowed to make important choices, even as to how to
    make pages (such as in beamer presentation). *)

(** Structure of the document *)
type structure = {
  mutable name : string;
  mutable page : int;
  mutable struct_x : float;
  mutable struct_y : float;
  mutable substructures : structure array;
}

(** Paramètres de dessin : comment les courbes se terminent *)
type lineCap = Butt_cap | Round_cap | Proj_square_cap

(** Comment elles se rejoignent *)
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

(** Of course, a line is a special type of Bezier curve with only two
    control points, so we do not need anything else for the moment. *)
type contents =
    Glyph of glyph
  | Path of path_parameters * Bezier.curve array
  | Link of link
type page = {
  mutable pageFormat : float * float;
  mutable pageContents : contents list;
}
module type Driver =
  sig
    val filename : string -> string
    val output : ?structure:structure -> page array -> string -> unit
  end

(** The only driver implemented (yet) *)
module Pdf : Driver
