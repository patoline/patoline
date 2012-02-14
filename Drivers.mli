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
module Buffer :
  sig
    type t = Buf.buf
    val empty : Buf.buf
    val clear : Buf.buf -> unit
    val add_string : Buffer.t -> string -> unit
    val to_string : Buffer.t -> string
  end
type lineCap = Butt_cap | Round_cap | Proj_square_cap
type lineJoin = Miter_join | Round_join | Bevel_join
type color = { red : float; green : float; blue : float; }
val black : color
module type Driver =
  sig
    type driver
    type params = string
    val filename : string -> string
    val init : params -> driver
    val close : driver -> unit
    val begin_page : driver -> float * float -> unit
    val end_page : driver -> unit
    val moveto : driver -> float * float -> unit
    val lineto : driver -> float * float -> unit
    val curveto :
      driver -> float * float -> float * float -> float * float -> unit
    val stroke :
      ?color:color ->
      ?dash_pattern:float list ->
      ?line_width:float ->
      ?line_cap:lineCap -> ?line_join:lineJoin -> driver -> unit
    val fill : ?color:color -> driver -> unit
    val fill_stroke :
      ?color:color ->
      ?dash_pattern:float list ->
      ?line_width:float ->
      ?line_cap:lineCap -> ?line_join:lineJoin -> driver -> unit
    val close_stroke :
      ?color:color ->
      ?dash_pattern:float list ->
      ?line_width:float ->
      ?line_cap:lineCap -> ?line_join:lineJoin -> driver -> unit
    val closePath : driver -> unit
    val begin_alternative_text : driver -> string -> unit
    val end_alternative_text : driver -> unit
    val text :
      ?color:color ->
      ?kerning:float * float ->
      driver -> float * float -> float -> Fonts.glyph -> unit
    val link : driver -> float * float -> float * float -> string -> unit
    val destination : driver -> string -> float * float -> unit
  end
module Pdf : Driver
