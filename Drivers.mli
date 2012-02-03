(** Common interface for output drivers of TeX' *)
open Fonts
module type Driver =
  sig
    type driver
    type params = string
    val filename:string->string

    val init : params -> driver
      (** First function to call before using the driver *)

    val close : driver -> unit
      (** Last function to call, for some drivers need a footer *)
      
    val begin_page : driver -> float * float -> unit
      (** Starts a new page *)
    val end_page : driver -> unit
      (** Flushes the current page *)
    val moveto : driver -> float * float -> unit
    val lineto : driver -> float * float -> unit
    val curveto :
      driver -> float * float -> float * float -> float * float -> unit
    val dash_pattern : driver -> float list -> unit

    val stroke : driver -> unit
    val fill : driver -> unit
    val fill_stroke : driver -> unit
    val close_stroke : driver -> unit
    val closePath : driver -> unit
    val text : driver -> float * float -> float -> Fonts.glyph list -> unit
  end

module Pdf : Driver
