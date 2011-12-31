open Drivers
open Binary
open Boxes
open Constants

module UTF8=Batteries.UTF8
module UChar=Batteries.UChar
module DynArray=Batteries.DynArray

let glyphCache_=ref StrMap.empty

let glyphCache gl=
  let font=try StrMap.find (Fonts.fontName !current_font) !glyphCache_ with
        Not_found->(let fontCache=ref IntMap.empty in
                      glyphCache_:=StrMap.add (Fonts.fontName !current_font) fontCache !glyphCache_;
                      fontCache)
  in
  let code=UChar.code gl in
    try IntMap.find code !font with
        Not_found->
          (let loaded=Fonts.loadGlyph !current_font (Fonts.glyph_of_char !current_font gl) in
             font:=IntMap.add code loaded !font;
             loaded)

let glyph_of_string fsize str =
  let str = UTF8.of_string str in
  let len = UTF8.length str in
  let res = ref [] in
  for i = 0 to len - 1 do 
    res := UTF8.get str i :: ! res
  done ;  
  List.map (fun c ->
    let gl=glyphCache c in
    GlyphBox { contents=UTF8.of_char c; glyph=gl; size = fsize; width=fsize*.(Fonts.glyphWidth gl)/.1000. }) !res 

let dyn_array_of_list l =
    let a = DynArray.create() in
    List.iter (fun x -> DynArray.add a x) (List.rev l);
    a
