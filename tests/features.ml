open Fonts.Opentype
open Fonts.FTypes

let f=loadFont "AGaramondPro-Regular.otf"

let _=
  List.iter print_subst (select_features f [SmallCapitals]);
  let chars=['T';'i';'t';'r''e'] in
  let nums=List.map (fun x->{ empty_glyph with glyph_index=glyph_of_char f) chars in
    List.iter (Printf.printf "%d\n") glyphs;
    List.fold_left apply nums (select_features f [SmallCapitals])
