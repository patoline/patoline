(** Common interface for output drivers of TeX' *)
open Fonts
type lineCap=Butt_cap | Round_cap | Proj_square_cap
type lineJoin=Miter_join | Round_join | Bevel_join

(** The three components of a color need to be between 0. and 1. *)
type color={ red:float; green:float; blue:float }
val black:color

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
    (* val dash_pattern : driver -> float list -> unit *)
    (* val line_width : driver -> float -> unit *)
    (* val line_cap : driver -> lineCap -> unit *)
    (* val line_join : driver -> lineJoin -> unit *)

    val stroke :
           ?color:color ->
           ?dash_pattern:float list ->
           ?line_width:float ->
           ?line_cap:lineCap -> ?line_join:lineJoin -> driver -> unit
    val fill: ?color:color -> driver-> unit
    val fill_stroke:
           ?color:color ->
           ?dash_pattern:float list ->
           ?line_width:float ->
           ?line_cap:lineCap -> ?line_join:lineJoin -> driver -> unit
    val close_stroke:
           ?color:color ->
           ?dash_pattern:float list ->
           ?line_width:float ->
           ?line_cap:lineCap -> ?line_join:lineJoin -> driver -> unit
    val closePath : driver -> unit
    val begin_alternative_text:driver->string->unit
    val end_alternative_text:driver->unit
    val text : ?color:color-> ?kerning:(float*float)->driver -> float * float -> float -> Fonts.glyph -> unit
  end

module Pdf : Driver
