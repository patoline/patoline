let patoline_weight_to_fc = function
  | FontPattern.Regular -> Fontconfig.Properties.Regular
  | FontPattern.Bold -> Fontconfig.Properties.Bold
  | FontPattern.Black -> Fontconfig.Properties.Black

let patoline_slant_to_fc = function
  | FontPattern.Roman -> Fontconfig.Properties.Roman
  | FontPattern.Italic -> Fontconfig.Properties.Italic

open Fontconfig.Properties
open Fontconfig.Pattern

let findFont fontspath pat =
  let fn p = ignore (Fontconfig.Config.app_font_add_dir p) in
  List.iter fn fontspath;
  Fontconfig.Pattern.get_string (find_font
    Fontconfig.Properties.(make
      [Family pat.FontPattern.family;
       Slant (patoline_slant_to_fc pat.FontPattern.slant);
       Weight (patoline_weight_to_fc pat.FontPattern.weight);
      ]
    )
  ) "file" 0

let findFont pat =
  let (p, ps) = PatConfig.(patoconfig.fonts_dir) in
  findFont (p :: ps) pat
