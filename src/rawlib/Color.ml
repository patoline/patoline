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
  let r = float_of_int rx /. 256. in
  let g = float_of_int gx /. 256. in
  let b = float_of_int bx /. 256. in
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
let black  = rgb 0.0 0.0 0.0
let white  = rgb 1.0 1.0 1.0
let red    = rgb 1.0 0.0 0.0
let green  = rgb 0.0 1.0 0.0
let blue   = rgb 0.0 0.0 1.0
let orange = rgb 1.0 0.5 0.0
let purple = rgb 0.5 0.0 1.0
let pink   = rgb 1.0 0.0 0.5
let yellow = rgb 1.0 1.0 0.0
let gray   = mix 0.5 white black
let grey   = gray
