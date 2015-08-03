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
val aliceBlue : color
val antiqueWhite : color
val aqua : color
val aquamarine : color
val azure : color
val beige : color
val bisque : color
val black : color
val blanchedAlmond : color
val blue : color
val blueViolet : color
val brown : color
val burlyWood : color
val cadetBlue : color
val chartreuse : color
val chocolate : color
val coral : color
val cornflowerBlue : color
val cornsilk : color
val crimson : color
val cyan : color
val darkBlue : color
val darkCyan : color
val darkGoldenRod : color
val darkGray : color
val darkGreen : color
val darkKhaki : color
val darkMagenta : color
val darkOliveGreen : color
val darkOrange : color
val darkOrchid : color
val darkRed : color
val darkSalmon : color
val darkSeaGreen : color
val darkSlateBlue : color
val darkSlateGray : color
val darkTurquoise : color
val darkViolet : color
val deepPink : color
val deepSkyBlue : color
val dimGray : color
val dodgerBlue : color
val fireBrick : color
val floralWhite : color
val forestGreen : color
val fuchsia : color
val gainsboro : color
val ghostWhite : color
val gold : color
val goldenRod : color
val gray : color
val green : color
val greenYellow : color
val honeyDew : color
val hotPink : color
val indianRed : color
val indigo : color
val ivory : color
val khaki : color
val lavender : color
val lavenderBlush : color
val lawnGreen : color
val lemonChiffon : color
val lightBlue : color
val lightCoral : color
val lightCyan : color
val lightGoldenRodYellow : color
val lightGray : color
val lightGreen : color
val lightPink : color
val lightSalmon : color
val lightSeaGreen : color
val lightSkyBlue : color
val lightSlateGray : color
val lightSteelBlue : color
val lightYellow : color
val lime : color
val limeGreen : color
val linen : color
val magenta : color
val maroon : color
val mediumAquaMarine : color
val mediumBlue : color
val mediumOrchid : color
val mediumPurple : color
val mediumSeaGreen : color
val mediumSlateBlue : color
val mediumSpringGreen : color
val mediumTurquoise : color
val mediumVioletRed : color
val midnightBlue : color
val mintCream : color
val mistyRose : color
val moccasin : color
val navajoWhite : color
val navy : color
val oldLace : color
val olive : color
val oliveDrab : color
val orange : color
val orangeRed : color
val orchid : color
val paleGoldenRod : color
val paleGreen : color
val paleTurquoise : color
val paleVioletRed : color
val papayaWhip : color
val peachPuff : color
val peru : color
val pink : color
val plum : color
val powderBlue : color
val purple : color
val rebeccaPurple : color
val red : color
val rosyBrown : color
val royalBlue : color
val saddleBrown : color
val salmon : color
val sandyBrown : color
val seaGreen : color
val seaShell : color
val sienna : color
val silver : color
val skyBlue : color
val slateBlue : color
val slateGray : color
val snow : color
val springGreen : color
val steelBlue : color
val tan : color
val teal : color
val thistle : color
val tomato : color
val turquoise : color
val violet : color
val wheat : color
val white : color
val whiteSmoke : color
val yellow : color
val yellowGreen : color

val grey : color
