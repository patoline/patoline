let share =
  try
    let (ic, _, _) as cs =
      let env = Unix.environment () in
      Unix.open_process_full "opam var share" env
    in
    (* [opam var share] prints CRLF on Windows: [input_line] only strips
       the LF, so drop any trailing CR as well or every path derived
       from [share] silently points at a non-existent "share\r"
       directory (which in turn makes the default grammar, hyphenation
       dictionaries and vendored fonts unfindable). *)
    let res = String.trim (input_line ic) in
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
  ; "FormatZhBook"
  ; "LMFormat"
  ; "FormatArticle" ]

(* FIXME generate dynamically (using findlib?). *)
let drivers =
  [ "Bin"
  ; "DriverCairo"
  ; "DriverGL"
  ; "DriverImage"
  ; "DriverHarfbuzz"
  ; "DriverPdfium"
  ; "Html"
  ; "Net"
  ; "None"
  ; "Patonet"
  ; "Pdf"
  ; "SVG" ]
