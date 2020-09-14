let datadir = Filename.concat BuildInfo.share "patoline"
let dataSubdir dir = Filename.concat datadir dir

let fonts_dir    = dataSubdir "fonts"
let grammars_dir = dataSubdir "grammars"
let hyphen_dir   = dataSubdir "hyphen"

let default_unicode_data_file = dataSubdir "unicode/unicode.data"

let extra_fonts_dir    = []
let extra_grammars_dir = []
let extra_hyphen_dir   = []

(* FIXME generate dynamically (using findlib?). *)
let formats =
  [ "DefaultFormat"
  ; "SimpleSlides"
  ; "FormatThese"
  ; "FormatWeb"
  ; "FormatLetter"
  ; "FormatSlides"
  ; "FormatMemoire"
  ; "FormatLivre"
  ; "LMFormat"
  ; "FormatArticle" ]

(* FIXME generate dynamically (using findlib?). *)
let drivers =
  [ "Bin"
  ; "DriverCairo"
  ; "DriverGL"
  ; "DriverImage"
  ; "Html"
  ; "Net"
  ; "None"
  ; "Patonet"
  ; "Pdf"
  ; "SVG" ]
