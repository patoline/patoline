(** XPlacement, YPlacement, XAdvance, YAdvance *)
open CamomileLibrary
type 'a kerningBox= { advance_height:float; advance_width:float; kern_x0:float; kern_y0:float; kern_contents:'a }
type glyph_ids=KernID of (glyph_ids kerningBox) | GlyphID of (UTF8.t*int)

let rec glyph_id_cont=function
    KernID x->glyph_id_cont x.kern_contents
  | GlyphID (_,x)->x

let rec glyph_id_utf8=function
    KernID x->glyph_id_utf8 x.kern_contents
  | GlyphID (x,_)->x
