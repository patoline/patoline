type glyphBox = {
  contents : CamomileLibrary.UTF8.t;
  glyph : Fonts.glyph;
  size : float;
  width : float;
  x0 : float;
  x1 : float;
  y0 : float;
  y1 : float;
}
type glueBox = {
  glue_min_width : float;
  glue_max_width : float;
  glue_badness : float -> float;
}
type drawingBox = {
  drawing_min_width : float;
  drawing_max_width : float;
  drawing_y0 : float -> float;
  drawing_y1 : float -> float;
  drawing_badness : float -> float;
}
type box = GlyphBox of glyphBox | Glue of glueBox | Drawing of drawingBox
exception Impossible
val isGlue : box -> bool
type line = {
  paragraph : int;
  lineStart : int;
  lineEnd : int;
  lastFigure : int;
  height : int;
  paragraph_height : int;
}

type parameters = {
  format : float * float;
  lead : float;
  measure : float;
  line_height : int;
}
val box_width : float -> box -> float
val lower_y : box -> float -> float
val upper_y : box -> float -> float
val lineBreak :
  parameters -> ?figures:'a array -> box array array -> box list array array
