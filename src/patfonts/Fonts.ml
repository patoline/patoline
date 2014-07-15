open CamomileLibrary
open FTypes

exception Not_supported

type font = CFF of CFF.font
          | Opentype of Opentype.font

let uniqueName = function
  | CFF x      -> CFF.uniqueName x
  | Opentype x -> Opentype.uniqueName x

let fontName ?index:(index=0) = function
  | CFF x      -> CFF.fontName ~index:index x
  | Opentype x -> Opentype.fontName ~index:index x

#ifdef BAN_COMIC_SANS
let ban_comic_sans font =
  let low = String.lowercase (fontName font).full_name in
  let is_substring s1 s0 i0 =
    let rec sub i j=
      if i>String.length s0-String.length s1 then -1 else
        if j>=String.length s1 then i else
          if s0.[i+j]=s1.[j] then sub i (j+1) else
            sub (i+1) 0
    in
    sub i0 0
  in
  let comic = is_substring "comic" low 0 in
  let sans  = is_substring "sans" low comic in
  if comic<0 && sans<0
    then
      (output_string stderr (TypoLanguage.message TypoLanguage.Ban_comic_sans);
       exit 1)
    else ()
#endif

(** loadFont pretends it can recognize font file types, but it
    actually only looks at the extension in the file name *)
let loadFont ?offset:(off = 0) ?size:(_ = 0) f =
  let size = in_channel_length (Util.open_in_bin_cached f) in
  let font =
    if Filename.check_suffix f ".otf" || Filename.check_suffix f ".ttf"
    then Opentype (Opentype.loadFont ~offset:off f ~size:size)
    else if Filename.check_suffix f ".cff"
    then CFF (CFF.loadFont ~offset:off f ~size:size)
    else raise Not_supported
  in
#ifdef BAN_COMIC_SANS
  ban_comic_sans font;
#endif
  font

let cardinal = function
  | CFF x      -> CFF.cardinal x
  | Opentype x -> Opentype.cardinal x

let ascender = function
  | CFF x      -> CFF.ascender x
  | Opentype x -> Opentype.ascender x

let descender = function
  | CFF x      -> CFF.descender x
  | Opentype x -> Opentype.descender x

let glyph_of_uchar =
  let cache = Hashtbl.create 1001 in
  fun f c ->
    try Hashtbl.find cache (c,f) 
    with Not_found ->
      let r = match f with
	              | CFF x      -> CFF.glyph_of_uchar x c
	              | Opentype x -> Opentype.glyph_of_uchar x c
      in
      Hashtbl.add cache (c,f) r; r

let glyph_of_char f c = glyph_of_uchar f (UChar.of_char c)



type glyph = CFFGlyph of CFF.glyph
           | OpentypeGlyph of Opentype.glyph

let loadGlyph f ?index:(index=0) g =
  match f with
    | CFF x      -> CFFGlyph (CFF.loadGlyph x ~index:index g)
    | Opentype x -> OpentypeGlyph (Opentype.loadGlyph x ~index:index g)

let outlines gl =
  match gl with
    | CFFGlyph x      -> CFF.outlines x
    | OpentypeGlyph x -> Opentype.outlines x

let glyphFont = function
  | CFFGlyph x      -> CFF (CFF.glyphFont x)
  | OpentypeGlyph x -> Opentype (Opentype.glyphFont x)

let glyphNumber = function
  | CFFGlyph x      -> CFF.glyphNumber x
  | OpentypeGlyph x -> Opentype.glyphNumber x

let glyphWidth = function
  | CFFGlyph x      -> CFF.glyphWidth x
  | OpentypeGlyph x -> Opentype.glyphWidth x

let glyphContents = function
  | CFFGlyph x      -> CFF.glyphContents x
  | OpentypeGlyph x -> Opentype.glyphContents x

let glyph_y0 = function
  | CFFGlyph x      -> CFF.glyph_y0 x
  | OpentypeGlyph x -> Opentype.glyph_y0 x

let glyph_y1 = function
  | CFFGlyph x      -> CFF.glyph_y1 x
  | OpentypeGlyph x -> Opentype.glyph_y1 x

let glyph_x0 = function
  | CFFGlyph x      -> CFF.glyph_x0 x
  | OpentypeGlyph x -> Opentype.glyph_x0 x

let glyph_x1 = function
  | CFFGlyph x      -> CFF.glyph_x1 x
  | OpentypeGlyph x -> Opentype.glyph_x1 x

let glyphName = function
  | CFFGlyph x      -> CFF.glyphName x
  | OpentypeGlyph x -> Opentype.glyphName x

let font_features = function
  | CFF x      -> CFF.font_features x
  | Opentype x -> Opentype.font_features x

type feature_set = CFFFeature_set of CFF.feature_set
                 | OpentypeFeature_set of Opentype.feature_set

let select_features f l =
  match f with
    | CFF x      -> CFFFeature_set (CFF.select_features x l)
    | Opentype x -> OpentypeFeature_set (Opentype.select_features x l)

let apply_features font set glyphs =
  match font,set with
    | CFF x,      CFFFeature_set y      -> CFF.apply_features x y glyphs
    | Opentype x, OpentypeFeature_set y -> Opentype.apply_features x y glyphs
    | _                                 -> glyphs

let positioning =
  let cache = Hashtbl.create 101 in
  fun f glyphs -> 
    try Hashtbl.find cache (glyphs,f)
    with Not_found ->
      let r = match f with
	              | CFF x      -> CFF.positioning x glyphs
	              | Opentype x -> Opentype.positioning x glyphs
      in
      Hashtbl.add cache (glyphs,f) r; r



type fontInfo = CFFInfo of CFF.fontInfo
              | OpentypeInfo of Opentype.fontInfo

let fontInfo =
  let cache = Hashtbl.create 101 in
  fun f -> 
    try Hashtbl.find cache f
    with Not_found ->
      let r = match f with
	              | CFF x      -> CFFInfo (CFF.fontInfo x)
                | Opentype x -> OpentypeInfo (Opentype.fontInfo x)
      in
      Hashtbl.add cache f r; r

let subset font info b c =
  match font, info with
    | CFF x,      CFFInfo y      -> CFF.subset x y b c
    | Opentype x, OpentypeInfo y -> Opentype.subset x y b c
    | _                          -> assert false

let setName info name =
  match info with
    | CFFInfo f      -> CFF.setName f name
    | OpentypeInfo f -> Opentype.setName f name

let add_kerning info a =
  match info with
    | CFFInfo i      ->CFF.add_kerning i a
    | OpentypeInfo i ->Opentype.add_kerning i a
