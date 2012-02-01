open Util
exception Impossible
val isGlue : box -> bool
type line = {
  paragraph : int;
  lineStart : int;
  lineEnd : int;
  hyphenStart:int;
  hyphenEnd:int;
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
  parameters -> ?figures:'a array -> box array array -> (float * float * box) list array array
