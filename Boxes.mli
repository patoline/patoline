(** The main algorithm of TeX' (line-breaking and page-breaking) *)

open Util
exception Impossible

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

val lineBreak :
  parameters -> ?figures:'a array -> box array array -> (float * float * box) list array array
