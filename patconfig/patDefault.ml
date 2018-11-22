let share =
  try
    let (ic, _, _) as cs =
      let env = Unix.environment () in
      Unix.open_process_full "opam var share" env
    in
    let res = input_line ic in
    ignore (Unix.close_process_full cs);
    res
  with _ -> "/usr/local/share"

let fonts_dir    = Filename.concat share "patoline/fonts"
let grammars_dir = Filename.concat share "patoline/grammars"
let hyphen_dir   = Filename.concat share "patoline/hyphen"

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
