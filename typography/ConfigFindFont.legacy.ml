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

(** WenQuanYi Zen Hei [.ttc] at common Linux paths (package [fonts-wqy-zenhei]). *)
let wqy_zenhei_path _pat =
  let fname = "wqy-zenhei.ttc" in
  let dirs =
    [
      "/usr/share/fonts/truetype/wqy";
      "/usr/local/share/fonts/truetype/wqy";
    ]
  in
  let rec try_dir = function
    | [] -> raise Not_found
    | d :: ds ->
        let p = Filename.concat d fname in
        if Sys.file_exists p then p else try_dir ds
  in
  try_dir dirs

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

  | "WenQuanYi Zen Hei" -> wqy_zenhei_path pat

  (* Fandol fonts (vendored): the default ctex font set, used by the
     reference Chinese book template. *)
  | "FandolSong" ->
      (match pat.slant, pat.weight with
       | _, (Bold | Black) -> "Fandol/FandolSong-Bold.ttf"
       | _ -> "Fandol/FandolSong-Regular.ttf")
  | "FandolHei" ->
      (match pat.slant, pat.weight with
       | _, (Bold | Black) -> "Fandol/FandolHei-Bold.ttf"
       | _ -> "Fandol/FandolHei-Regular.ttf")
  | "FandolKai" -> "Fandol/FandolKai-Regular.ttf"
  | "FandolFang" -> "Fandol/FandolFang-Regular.ttf"

  (* Windows system fonts (found through [system_font_dirs]). *)
  | "Times New Roman" ->
      (match pat.slant, pat.weight with
       | Roman, Regular -> "times.ttf"
       | Italic, Regular -> "timesi.ttf"
       | Roman, Bold -> "timesbd.ttf"
       | Italic, Bold -> "timesbi.ttf"
       | Roman, Black -> "timesbd.ttf"
       | Italic, Black -> "timesbi.ttf")

  | "SimSun" -> "simsun.ttc"              (* 宋体 *)
  | "SimHei" -> "simhei.ttf"              (* 黑体 *)
  | "KaiTi" -> "simkai.ttf"               (* 楷体 *)
  | "FangSong" -> "simfang.ttf"           (* 仿宋 *)
  | "Microsoft YaHei" ->
      (match pat.weight with
       | Bold | Black -> "msyhbd.ttc"
       | Regular -> "msyh.ttc")
  | "DengXian" ->
      (match pat.weight with
       | Bold | Black -> "dengb.ttf"
       | Regular -> "deng.ttf")

  | _ -> raise Not_found

(** Latin Modern is not vendored (no [lmodern/] directory under [fonts/]).
    Fall back to DejaVu for text and Asana-Math for math so the article
    formats do not raise [Not_found] when they reach for LM. *)
let lmodern_fallback pat =
  match pat.family with
  | "Latin Modern Math" -> { pat with family = "Asana Math" }
  | "Latin Modern Mono" | "Latin Modern Mono Caps" ->
      { pat with family = "Bitstream Vera Sans Mono" }
  | "Latin Modern Roman" | "Latin Modern Roman Caps" ->
      { pat with family = "DejaVu Serif" }
  | _ -> pat

(** Directories searched when a font is not vendored under [fonts/].
    On Windows this covers the per-machine font directory
    (%WINDIR%\Fonts, %LOCALAPPDATA%…\Fonts); on Unix the usual
    fontconfig paths are tried.  This is what makes system CJK fonts
    (SimSun 宋体, SimHei 黑体, KaiTi 楷体, FangSong 仿宋, Microsoft
    YaHei 微软雅黑) and Times New Roman reachable. *)
let system_font_dirs () =
  let acc = ref [] in
  let add d = if Sys.file_exists d then acc := d :: !acc in
  (try add (Filename.concat (Sys.getenv "WINDIR") "Fonts")
   with Not_found -> ());
  (try
     add (Filename.concat
            (Filename.concat (Sys.getenv "LOCALAPPDATA") "Microsoft")
            (Filename.concat "Windows" "Fonts"))
   with Not_found -> ());
  add "/usr/share/fonts";
  add "/usr/local/share/fonts";
  add "/usr/share/fonts/truetype";
  !acc

let findFont pat =
  let open Patconfig.Config in
  let lookup pat =
    let name = pat_to_name pat in
    if String.length name > 0 && name.[0] = '/' && Sys.file_exists name then name
    else findFont name
  in
  let in_system pat =
    let name = pat_to_name pat in
    let rec go = function
      | [] -> raise Not_found
      | d :: ds ->
          let p = Filename.concat d name in
          if Sys.file_exists p then p else go ds
    in
    go (system_font_dirs ())
  in
  try lookup pat
  with Not_found ->
    (try lookup (lmodern_fallback pat)
     with Not_found -> in_system pat)
