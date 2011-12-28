module type Driver =
  sig
    type driver
    type params = string
    val init : params -> driver
    val begin_page : driver -> float * float -> unit
    val end_page : driver -> unit
    val moveto : driver -> float * float -> unit
    val lineto : driver -> float * float -> unit
    val curveto :
      driver -> float * float -> float * float -> float * float -> unit
    val close : driver -> unit
    val stroke : driver -> unit
    val fill : driver -> unit
    val fill_stroke : driver -> unit
    val close_stroke : driver -> unit
    val closePath : driver -> unit
    val text : driver -> float * float -> int -> Fonts.glyph list -> unit
  end

module Pdf : Driver
