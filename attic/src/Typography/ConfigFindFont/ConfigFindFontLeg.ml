(** Finds a font given some criteria, as a FontPattern.t list. *)
open FontPattern

let alegreya_variants = function
  | {family = _; slant = Roman; weight = Regular} -> "Regular"
  | {family = _; slant = Italic; weight = Regular} -> "Italic"
  | {family = _; slant = Roman; weight = Bold} -> "Bold"
  | {family = _; slant = Italic; weight = Bold} -> "BoldItalic"
  | {family = _; slant = Roman; weight = Black} -> "Black"
  | {family = _; slant = Italic; weight = Black} -> "BlackItalic"

let dejavusans_variants = function
  | {family = _; slant = Roman; weight = Regular} -> ""
  | {family = _; slant = Italic; weight = Regular} -> "-Oblique"
  | {family = _; slant = Roman; weight = Bold} -> "-Bold"
  | {family = _; slant = Italic; weight = Bold} -> "-BoldOblique"
  | {family = _; slant = Roman; weight = Black} -> "-Bold"
  | {family = _; slant = Italic; weight = Black} -> "-BoldOblique"

let dejavuserif_variants = function
  | {family = _; slant = Roman; weight = Regular} -> ""
  | {family = _; slant = Italic; weight = Regular} -> "-Italic"
  | {family = _; slant = Roman; weight = Bold} -> "-Bold"
  | {family = _; slant = Italic; weight = Bold} -> "-BoldItalic"
  | {family = _; slant = Roman; weight = Black} -> "-Bold"
  | {family = _; slant = Italic; weight = Black} -> "-BoldItalic"

let freesans_variants = function
  | {family = _; slant = Roman; weight = Regular} -> ""
  | {family = _; slant = Italic; weight = Regular} -> "Oblique"
  | {family = _; slant = Roman; weight = Bold} -> "Bold"
  | {family = _; slant = Italic; weight = Bold} -> "BoldOblique"
  | {family = _; slant = Roman; weight = Black} -> "Bold"
  | {family = _; slant = Italic; weight = Black} -> "BoldOblique"

let freeserif_variants = function
  | {family = _; slant = Roman; weight = Regular} -> ""
  | {family = _; slant = Italic; weight = Regular} -> "Italic"
  | {family = _; slant = Roman; weight = Bold} -> "Bold"
  | {family = _; slant = Italic; weight = Bold} -> "BoldItalic"
  | {family = _; slant = Roman; weight = Black} -> "Bold"
  | {family = _; slant = Italic; weight = Black} -> "BoldItalic"

let bitstreamverasans_variants = function
  | {family = _; slant = Roman; weight = Regular} -> "Roman"
  | {family = _; slant = Italic; weight = Regular} -> "Oblique"
  | {family = _; slant = Roman; weight = Bold} -> "Bold"
  | {family = _; slant = Italic; weight = Bold} -> "BoldOb"
  | {family = _; slant = Roman; weight = Black} -> "Bold"
  | {family = _; slant = Italic; weight = Black} -> "BoldOb"

let texgyrecursor_variants = function
  | {family = _; slant = Roman; weight = Regular} -> "regular"
  | {family = _; slant = Italic; weight = Regular} -> "italic"
  | {family = _; slant = Roman; weight = Bold} -> "bold"
  | {family = _; slant = Italic; weight = Bold} -> "bolditalic"
  | {family = _; slant = Roman; weight = Black} -> "bold"
  | {family = _; slant = Italic; weight = Black} -> "bolditalic"

let lmromancaps_variants = function
  | {family = _; slant = Roman; weight = Regular} -> "regular"
  | {family = _; slant = Italic; weight = Regular} -> "oblique"
  | _ -> raise Not_found

let pat_to_name pat =
  match pat.family with
  | "Neo Euler" -> "Euler/euler.otf"

  | "Alegreya" ->
      "Alegreya/Alegreya-" ^ (alegreya_variants pat) ^ ".otf"

  | "Alegreya SC" ->
      "Alegreya/AlegreyaSC-" ^ (alegreya_variants pat) ^ ".otf"

  | "Asana Math" -> "Asana-Math/Asana-Math.otf"

  | "Philosopher" ->
      "Philosopher/Philosopher-" ^ (alegreya_variants pat) ^ ".otf"

  | "Bitstream Vera Sans Mono" ->
      "BitstreamVeraSansMono/BitstreamVeraSansMono-" ^
      (bitstreamverasans_variants pat) ^ ".otf"

  | "Euler" -> "AMS/ams.otf"

  | "TeX Gyre Cursor" ->
      "TexGyreCursor/texgyrecursor-" ^ (texgyrecursor_variants pat) ^ ".otf"

  | "Latin Modern Roman" ->
      "lmodern/lmroman10-" ^ (texgyrecursor_variants pat) ^".otf"

  | "Latin Modern Roman Caps" ->
      "lmodern/lmromancaps10-" ^ (lmromancaps_variants pat) ^".otf"

  | "Latin Modern Mono" ->
      "lmodern/lmmono10-" ^ (texgyrecursor_variants pat) ^".otf"

  | "Latin Modern Mono Caps" ->
      "lmodern/lmmonocaps10-" ^ (lmromancaps_variants pat) ^".otf"

  | "Latin Modern Math" ->
      "lmodern/lmmath.otf"

  | "DejaVu Sans" ->
      "DejaVu-2.34/DejaVuSans" ^ (dejavusans_variants pat) ^".ttf"

  | "DejaVu Sans Mono" ->
      "DejaVu-2.34/DejaVuSansMono" ^ (dejavusans_variants pat) ^".ttf"

  | "DejaVu Sans Condensed" ->
      "DejaVu-2.34/DejaVuSansCondensed" ^ (dejavusans_variants pat) ^".ttf"

  | "DejaVu Serif" ->
      "DejaVu-2.34/DejaVuSerif" ^ (dejavuserif_variants pat) ^".ttf"

  | "DejaVu Serif Condensed" ->
      "DejaVu-2.34/DejaVuSerifCondensed" ^ (dejavuserif_variants pat) ^".ttf"

  | "Free Serif" -> "FreeSerif"^ (freeserif_variants pat) ^ ".ttf"

  | "Free Sans" -> "FreeSans"^ (freesans_variants pat) ^ ".ttf"

  | "Free Mono" -> "FreeMono"^ (freesans_variants pat) ^ ".ttf"

  | _ -> raise Not_found


let findFont pat =
  let open PatConfig in
  findFont (pat_to_name pat)
