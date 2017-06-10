open FTypes
module OpT = Opentype

type font =
  | CFF of CFF.font
  | OpT of OpT.font

let uniqueName = function
  | CFF f -> CFF.uniqueName f
  | OpT f -> OpT.uniqueName f

let fontName ?index:(index=0) = function
  | CFF f -> CFF.fontName ~index:index f
  | OpT f -> OpT.fontName ~index:index f

let loadFont ?offset:(offset = 0) ?size:(size = None) fn =
  (* Check if the font file exists. *)
  if not (Sys.file_exists fn) then
    failwith (Printf.sprintf "Font file %S not found..." fn);
  (* Load the font file according to its extension. *)
  let extension_in = List.exists (Filename.check_suffix fn) in
  if extension_in OpT.extensions then
    OpT (OpT.loadFont ~offset ~size fn)
  else if extension_in CFF.extensions then
    CFF (CFF.loadFont ~offset ~size fn)
  else
    invalid_arg "Font format not supported (unknown extension)"

let cardinal  = function CFF f -> CFF.cardinal  f | OpT f -> OpT.cardinal  f

let glyph_of_uchar : font -> UChar.t -> int = fun font c ->
  match font with
  | CFF f -> CFF.glyph_of_uchar f c
  | OpT f -> OpT.glyph_of_uchar f c

let glyph_of_uchar : font -> UChar.t -> int =
  let cache = Hashtbl.create 101 in
  let glyph_of_uchar font c =
    try Hashtbl.find cache (font, c) with Not_found ->
    let i = glyph_of_uchar font c in
    Hashtbl.add cache (font, c) i; i
  in glyph_of_uchar

let glyph_of_char : font -> char -> int = fun font c ->
  glyph_of_uchar font (UChar.of_char c)

type glyph =
  | CFFGlyph of CFF.glyph
  | OpTGlyph of OpT.glyph

let loadGlyph font ?index:(index=0) gl =
  match font with
  | CFF f -> CFFGlyph (CFF.loadGlyph f ~index:index gl)
  | OpT f -> OpTGlyph (OpT.loadGlyph f ~index:index gl)

let outlines = function
  | CFFGlyph gl -> CFF.outlines gl
  | OpTGlyph gl -> OpT.outlines gl

let glyphFont = function
  | CFFGlyph gl -> CFF (CFF.glyphFont gl)
  | OpTGlyph gl -> OpT (OpT.glyphFont gl)

let glyphNumber = function
  | CFFGlyph gl -> CFF.glyphNumber gl
  | OpTGlyph gl -> OpT.glyphNumber gl

let glyphWidth = function
  | CFFGlyph gl -> CFF.glyphWidth gl
  | OpTGlyph gl -> OpT.glyphWidth gl

let glyphContents = function
  | CFFGlyph gl -> CFF.glyphContents gl
  | OpTGlyph gl -> OpT.glyphContents gl

let glyph_y0 = function
  | CFFGlyph gl -> CFF.glyph_y0 gl
  | OpTGlyph gl -> OpT.glyph_y0 gl

let glyph_y1 = function
  | CFFGlyph gl -> CFF.glyph_y1 gl
  | OpTGlyph gl -> OpT.glyph_y1 gl

let glyph_x0 = function
  | CFFGlyph gl -> CFF.glyph_x0 gl
  | OpTGlyph gl -> OpT.glyph_x0 gl

let glyph_x1 = function
  | CFFGlyph gl -> CFF.glyph_x1 gl
  | OpTGlyph gl -> OpT.glyph_x1 gl

let glyphName = function
  | CFFGlyph gl -> CFF.glyphName gl
  | OpTGlyph gl -> OpT.glyphName gl

let font_features = function
  | CFF f -> CFF.font_features f
  | OpT f -> OpT.font_features f

type feature_set =
  | CFFFeature_set of CFF.feature_set
  | OpTFeature_set of OpT.feature_set

let select_features font l =
  match font with
  | CFF f -> CFFFeature_set (CFF.select_features f l)
  | OpT f -> OpTFeature_set (OpT.select_features f l)

let apply_features font set glyphs =
  match (font, set) with
  | (CFF f, CFFFeature_set s) -> CFF.apply_features f s glyphs
  | (OpT f, OpTFeature_set s) -> OpT.apply_features f s glyphs
  | _                         -> glyphs

let positioning =
  let cache = Hashtbl.create 101 in
  let positioning font glyphs =
    match font with
	  | CFF f -> CFF.positioning f glyphs
	  | OpT f -> OpT.positioning f glyphs
  in
  let positioning font glyphs =
    try Hashtbl.find cache (font, glyphs) with Not_found ->
    let r = positioning font glyphs in
    Hashtbl.add cache (font, glyphs) r; r
  in positioning

type fontInfo =
  | CFFInfo of CFF.fontInfo
  | OpTInfo of OpT.fontInfo

let fontInfo =
  let cache = Hashtbl.create 101 in
  let fontInfo font =
    match font with
	  | CFF f -> CFFInfo (CFF.fontInfo f)
    | OpT f -> OpTInfo (OpT.fontInfo f)
  in
  let fontInfo font =
    try Hashtbl.find cache font with Not_found ->
    let i = fontInfo font in
    Hashtbl.add cache font i; i
  in fontInfo

let subset font info b c =
  match (font, info) with
  | (CFF f, CFFInfo i) -> CFF.subset f i b c
  | (OpT f, OpTInfo i) -> OpT.subset f i b c
  | _                  -> assert false

let setName info name =
  match info with
  | CFFInfo f -> CFF.setName f name
  | OpTInfo f -> OpT.setName f name

let add_kerning info a =
  match info with
  | CFFInfo i -> CFF.add_kerning i a
  | OpTInfo i -> OpT.add_kerning i a

let cff_only : font -> CFF.font = function
  | CFF f           -> f
  | OpT (OpT.CFF f) -> OpT.(f.cff_font)
  | _               -> invalid_arg "Not a CFF font"

let is_cff : font -> bool = function
  | CFF _           -> true
  | OpT (OpT.CFF _) -> true
  | _               -> false
