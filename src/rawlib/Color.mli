(* Type of a color. *)
type color

(* Build a color given its components. *)
val rgb  : float -> float -> float -> color
val rgba : float -> float -> float -> float -> color
val hsv  : float -> float -> float -> color

(* Obtain a color from a CSS color HEX value. Example: "#FFFFFF", "#012345". *)
val colorHex : string -> color

(* Set the alpha channel of a color. *)
val with_alpha : float -> color -> color

(* Obtain the value of the alpha channel of a color. *)
val alpha   : color -> float
val to_rgb  : color -> float * float * float
val to_rgba : color -> float * float * float * float

(* Weighted mixing of two colors. *)
val mix : float -> color -> color -> color

(* Several predifined colors. *)
val black  : color
val white  : color
val red    : color
val green  : color
val blue   : color
val orange : color
val purple : color
val pink   : color
val yellow : color
val gray   : color
val grey   : color
