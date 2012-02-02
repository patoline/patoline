(** Defines the type of the boxes used in the optimizer. It is
    essential that all lengths defined here be expressed in
    typographical units, i.e. 1/1000 of a "metal box" *)
type box =
    GlyphBox of (float * glyph)
  | Kerning of box FontsTypes.kerningBox
  | Glue of glueBox
  | Drawing of drawingBox
  | Hyphen of hyphenBox
  | Mark of int
  | Empty
and glyph = {
  contents : CamomileLibrary.UTF8.t; (** The "meaning" of a glyph box, may contain several letters *)
  glyph : Fonts.glyph; (** The actual glyph *)
  width : float; (** The width, in typographic units=1/1000 *)
  x0 : float; (** lower-left corner of the bounding box *)
  x1 : float; (** lower-right corner of the bounding box *)
  y0 : float; (** upper-left corner of the bounding box *)
  y1 : float; (** upper-right corner of the bounding box *)
}
and 'a kerningBox='a FontsTypes.kerningBox
and drawing =
    Curve of (float * float * Bezier.curve)
  | Glyph of (float * float * float * glyph)
and glueBox = {
  glue_min_width : float;
  glue_max_width : float;
  glue_badness : float -> float;
}
and drawingBox = {
  drawing_min_width : float;
  drawing_max_width : float;
  drawing_y0 : float -> float;
  drawing_y1 : float -> float;
  drawing_badness : float -> float;
  drawing : float -> drawing list;
}
and hyphenBox = {
  hyphen_normal : box array;
  hyphenated : (box array * box array) array;
}

val is_glyph : box -> bool
val is_glue : box -> bool
val box_width : float -> box -> float

(** The smallest and the largest widths a box can take *)
val box_interval : box -> (float*float)
val boxes_interval : box array -> (float*float)

val lower_y : box -> float -> float
val upper_y : box -> float -> float

(** Loading a glyph is expensive in time and memory. This caches the
glyphs by font name, i.e.  if two different fonts have the same name,
the result is the glyph loaded on the first call with this font
name *)
val glyphCache :
  Fonts.font -> Binary.IntMap.key -> CamomileLibrary.UTF8.t -> glyph
val glyph_of_string :
  Fonts.font -> float -> CamomileLibrary.UTF8.t -> box list
val hyphenate : Hyphenate.ptree -> Fonts.font -> float -> CamomileLibrary.UTF8.t -> box list
val knuth_h_badness : float -> float -> float
