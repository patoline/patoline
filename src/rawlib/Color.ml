type rgb =
  { red   : float
  ; green : float
  ; blue  : float
  ; alpha : float }

type color =
  | RGB of rgb

(* Produce a color from RGB values between 0.0 and 1.0. *)
let rgb red green blue = RGB { red ; green ; blue ; alpha = 1.0 }

(* Produce a color from RGBA values between 0.0 and 1.0. *)
let rgba red green blue alpha = RGB { red ; green ; blue ; alpha }

(* Produce a color from HSV values. *)
let hsv h s v =
  let h' = h /. 60.0 in
  let c = v *. s in
  let h'mod2 = h' -. (float_of_int (2 * (int_of_float (h' /. 2.0)))) in
  let x = c *. (1.0 -. (abs_float (h'mod2 -. 1.0))) in
  if h' < 1.0 then rgb c x 0.0 else
  if h' < 2.0 then rgb x c 0.0 else
  if h' < 3.0 then rgb 0.0 c x else
  if h' < 4.0 then rgb 0.0 x c else
  if h' < 5.0 then rgb x 0.0 c else
  if h' < 6.0 then rgb c 0.0 x else
  rgb 0.0 0.0 0.0

(* Read a color from a string with the format "#RRGGBB" in hexadecimal. *)
let colorHex s =
  let (rx,gx,bx) = Scanf.sscanf s "#%2X%2X%2X" (fun r g b -> (r,g,b)) in
  let r = float_of_int rx /. 255. in
  let g = float_of_int gx /. 255. in
  let b = float_of_int bx /. 255. in
  rgb r g b

(* Sets the alpha channel of a color. *)
let with_alpha alpha = function
  | RGB rgb -> RGB { rgb with alpha }

(* Get the alpha channel of a color. *)
let alpha = function
  | RGB rgb -> rgb.alpha

(* Get the RGB value of a color. *)
let to_rgb = function
  | RGB rgb -> (rgb.red, rgb.green, rgb.blue)

(* Get the RGBA value of a color. *)
let to_rgba = function
  | RGB rgb -> (rgb.red, rgb.green, rgb.blue, rgb.alpha)

(* Mix two colors given a coeficient between 0.0 and 1.0. *)
let mix c cola colb =
  match (cola, colb) with
  | (RGB rgba, RGB rgbb) ->
      let c' = 1.0 -. c in
      let red   = c *. rgba.red   +. c' *. rgbb.red   in
      let green = c *. rgba.green +. c' *. rgbb.green in
      let blue  = c *. rgba.blue  +. c' *. rgbb.blue  in
      let alpha = c *. rgba.alpha +. c' *. rgbb.alpha in
      RGB { red ; green ; blue ; alpha }
                           
(* Some predifined colors. *)
let aliceBlue = rgb 0.941176 0.972549 1.000000
let antiqueWhite = rgb 0.980392 0.921569 0.843137
let aqua = rgb 0.000000 1.000000 1.000000
let aquamarine = rgb 0.498039 1.000000 0.831373
let azure = rgb 0.941176 1.000000 1.000000
let beige = rgb 0.960784 0.960784 0.862745
let bisque = rgb 1.000000 0.894118 0.768627
let black = rgb 0.000000 0.000000 0.000000
let blanchedAlmond = rgb 1.000000 0.921569 0.803922
let blue = rgb 0.000000 0.000000 1.000000
let blueViolet = rgb 0.541176 0.168627 0.886275
let brown = rgb 0.647059 0.164706 0.164706
let burlyWood = rgb 0.870588 0.721569 0.529412
let cadetBlue = rgb 0.372549 0.619608 0.627451
let chartreuse = rgb 0.498039 1.000000 0.000000
let chocolate = rgb 0.823529 0.411765 0.117647
let coral = rgb 1.000000 0.498039 0.313725
let cornflowerBlue = rgb 0.392157 0.584314 0.929412
let cornsilk = rgb 1.000000 0.972549 0.862745
let crimson = rgb 0.862745 0.078431 0.235294
let cyan = rgb 0.000000 1.000000 1.000000
let darkBlue = rgb 0.000000 0.000000 0.545098
let darkCyan = rgb 0.000000 0.545098 0.545098
let darkGoldenRod = rgb 0.721569 0.525490 0.043137
let darkGray = rgb 0.662745 0.662745 0.662745
let darkGreen = rgb 0.000000 0.392157 0.000000
let darkKhaki = rgb 0.741176 0.717647 0.419608
let darkMagenta = rgb 0.545098 0.000000 0.545098
let darkOliveGreen = rgb 0.333333 0.419608 0.184314
let darkOrange = rgb 1.000000 0.549020 0.000000
let darkOrchid = rgb 0.600000 0.196078 0.800000
let darkRed = rgb 0.545098 0.000000 0.000000
let darkSalmon = rgb 0.913725 0.588235 0.478431
let darkSeaGreen = rgb 0.560784 0.737255 0.560784
let darkSlateBlue = rgb 0.282353 0.239216 0.545098
let darkSlateGray = rgb 0.184314 0.309804 0.309804
let darkTurquoise = rgb 0.000000 0.807843 0.819608
let darkViolet = rgb 0.580392 0.000000 0.827451
let deepPink = rgb 1.000000 0.078431 0.576471
let deepSkyBlue = rgb 0.000000 0.749020 1.000000
let dimGray = rgb 0.411765 0.411765 0.411765
let dodgerBlue = rgb 0.117647 0.564706 1.000000
let fireBrick = rgb 0.698039 0.133333 0.133333
let floralWhite = rgb 1.000000 0.980392 0.941176
let forestGreen = rgb 0.133333 0.545098 0.133333
let fuchsia = rgb 1.000000 0.000000 1.000000
let gainsboro = rgb 0.862745 0.862745 0.862745
let ghostWhite = rgb 0.972549 0.972549 1.000000
let gold = rgb 1.000000 0.843137 0.000000
let goldenRod = rgb 0.854902 0.647059 0.125490
let gray = rgb 0.501961 0.501961 0.501961
let green = rgb 0.000000 0.501961 0.000000
let greenYellow = rgb 0.678431 1.000000 0.184314
let honeyDew = rgb 0.941176 1.000000 0.941176
let hotPink = rgb 1.000000 0.411765 0.705882
let indianRed = rgb 0.803922 0.360784 0.360784
let indigo = rgb 0.294118 0.000000 0.509804
let ivory = rgb 1.000000 1.000000 0.941176
let khaki = rgb 0.941176 0.901961 0.549020
let lavender = rgb 0.901961 0.901961 0.980392
let lavenderBlush = rgb 1.000000 0.941176 0.960784
let lawnGreen = rgb 0.486275 0.988235 0.000000
let lemonChiffon = rgb 1.000000 0.980392 0.803922
let lightBlue = rgb 0.678431 0.847059 0.901961
let lightCoral = rgb 0.941176 0.501961 0.501961
let lightCyan = rgb 0.878431 1.000000 1.000000
let lightGoldenRodYellow = rgb 0.980392 0.980392 0.823529
let lightGray = rgb 0.827451 0.827451 0.827451
let lightGreen = rgb 0.564706 0.933333 0.564706
let lightPink = rgb 1.000000 0.713725 0.756863
let lightSalmon = rgb 1.000000 0.627451 0.478431
let lightSeaGreen = rgb 0.125490 0.698039 0.666667
let lightSkyBlue = rgb 0.529412 0.807843 0.980392
let lightSlateGray = rgb 0.466667 0.533333 0.600000
let lightSteelBlue = rgb 0.690196 0.768627 0.870588
let lightYellow = rgb 1.000000 1.000000 0.878431
let lime = rgb 0.000000 1.000000 0.000000
let limeGreen = rgb 0.196078 0.803922 0.196078
let linen = rgb 0.980392 0.941176 0.901961
let magenta = rgb 1.000000 0.000000 1.000000
let maroon = rgb 0.501961 0.000000 0.000000
let mediumAquaMarine = rgb 0.400000 0.803922 0.666667
let mediumBlue = rgb 0.000000 0.000000 0.803922
let mediumOrchid = rgb 0.729412 0.333333 0.827451
let mediumPurple = rgb 0.576471 0.439216 0.858824
let mediumSeaGreen = rgb 0.235294 0.701961 0.443137
let mediumSlateBlue = rgb 0.482353 0.407843 0.933333
let mediumSpringGreen = rgb 0.000000 0.980392 0.603922
let mediumTurquoise = rgb 0.282353 0.819608 0.800000
let mediumVioletRed = rgb 0.780392 0.082353 0.521569
let midnightBlue = rgb 0.098039 0.098039 0.439216
let mintCream = rgb 0.960784 1.000000 0.980392
let mistyRose = rgb 1.000000 0.894118 0.882353
let moccasin = rgb 1.000000 0.894118 0.709804
let navajoWhite = rgb 1.000000 0.870588 0.678431
let navy = rgb 0.000000 0.000000 0.501961
let oldLace = rgb 0.992157 0.960784 0.901961
let olive = rgb 0.501961 0.501961 0.000000
let oliveDrab = rgb 0.419608 0.556863 0.137255
let orange = rgb 1.000000 0.647059 0.000000
let orangeRed = rgb 1.000000 0.270588 0.000000
let orchid = rgb 0.854902 0.439216 0.839216
let paleGoldenRod = rgb 0.933333 0.909804 0.666667
let paleGreen = rgb 0.596078 0.984314 0.596078
let paleTurquoise = rgb 0.686275 0.933333 0.933333
let paleVioletRed = rgb 0.858824 0.439216 0.576471
let papayaWhip = rgb 1.000000 0.937255 0.835294
let peachPuff = rgb 1.000000 0.854902 0.725490
let peru = rgb 0.803922 0.521569 0.247059
let pink = rgb 1.000000 0.752941 0.796078
let plum = rgb 0.866667 0.627451 0.866667
let powderBlue = rgb 0.690196 0.878431 0.901961
let purple = rgb 0.501961 0.000000 0.501961
let rebeccaPurple = rgb 0.400000 0.200000 0.600000
let red = rgb 1.000000 0.000000 0.000000
let rosyBrown = rgb 0.737255 0.560784 0.560784
let royalBlue = rgb 0.254902 0.411765 0.882353
let saddleBrown = rgb 0.545098 0.270588 0.074510
let salmon = rgb 0.980392 0.501961 0.447059
let sandyBrown = rgb 0.956863 0.643137 0.376471
let seaGreen = rgb 0.180392 0.545098 0.341176
let seaShell = rgb 1.000000 0.960784 0.933333
let sienna = rgb 0.627451 0.321569 0.176471
let silver = rgb 0.752941 0.752941 0.752941
let skyBlue = rgb 0.529412 0.807843 0.921569
let slateBlue = rgb 0.415686 0.352941 0.803922
let slateGray = rgb 0.439216 0.501961 0.564706
let snow = rgb 1.000000 0.980392 0.980392
let springGreen = rgb 0.000000 1.000000 0.498039
let steelBlue = rgb 0.274510 0.509804 0.705882
let tan = rgb 0.823529 0.705882 0.549020
let teal = rgb 0.000000 0.501961 0.501961
let thistle = rgb 0.847059 0.749020 0.847059
let tomato = rgb 1.000000 0.388235 0.278431
let turquoise = rgb 0.250980 0.878431 0.815686
let violet = rgb 0.933333 0.509804 0.933333
let wheat = rgb 0.960784 0.870588 0.701961
let white = rgb 1.000000 1.000000 1.000000
let whiteSmoke = rgb 0.960784 0.960784 0.960784
let yellow = rgb 1.000000 1.000000 0.000000
let yellowGreen = rgb 0.603922 0.803922 0.196078

let grey = gray
