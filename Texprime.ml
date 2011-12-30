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
            
let paragraphs=DynArray.create ()

let add_paragraph text=
  if DynArray.empty paragraphs then DynArray.add paragraphs (DynArray.create());
  let fsize=float_of_int !current_size in
  let was_newline=ref false in
    for i=0 to UTF8.length text-1 do
      let char=UTF8.get text i in
        if UChar.is_newline char then
          (if !was_newline && DynArray.length (DynArray.last paragraphs) > 0 then
             DynArray.add paragraphs (DynArray.create())
           else
             was_newline:=true)
        else
          (was_newline:=false;
           if UChar.is_whitespace char then
             DynArray.add (DynArray.last paragraphs) (Glue (2.*. fsize/.9., fsize/.3., fsize/.2.))
           else
             (let gl=glyphCache char in
                DynArray.add (DynArray.last paragraphs) 
                  (GlyphBox { contents=UTF8.of_char char; glyph=gl; width=fsize*.(Fonts.glyphWidth gl)/.1000. }))
          )
    done;
    DynArray.add (DynArray.last paragraphs) (Glue (0.,0.,0.))
          


module M=Output.Routine(Pdf)



let spec=[]


let _=
  let filename=ref [] in
    Arg.parse spec (fun x->filename:=x::(!filename)) "Usage :";    
    match !filename with
        []->Printf.printf "no input files\n"
      | h::_->
          let file=
            let op=open_in h in
            let str=String.create (in_channel_length op) in
              Pervasives.really_input op str 0 (in_channel_length op);
              close_in op;
              str
          in
            add_paragraph (UTF8.of_string file);
            let pages=lineBreak paragraphs in
              M.output_routine !filename pages
