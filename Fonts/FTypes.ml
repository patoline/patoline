(** XPlacement, YPlacement, XAdvance, YAdvance *)
open CamomileLibrary
open Binary

type 'a kerningBox= { advance_height:float; advance_width:float; kern_x0:float; kern_y0:float; kern_contents:'a }

type glyph_id = { glyph_utf8:CamomileLibrary.UTF8.t; glyph_index:int }
let empty_glyph= { glyph_utf8=UTF8.init 0 (fun _->UChar.chr 0); glyph_index=0 }
type glyph_ids=KernID of (glyph_ids kerningBox) | GlyphID of glyph_id

module type Font=(
  sig
    type font
    type glyph
    val loadFont: ?offset:int-> ?size:int->string->font
    val glyph_of_char:font->char->int
    val glyph_of_uchar:font->UChar.t->int
    val loadGlyph:font-> ?index:int ->glyph_id->glyph
    val outlines:glyph->(float array*float array) list
    val glyphFont:glyph->font
    val glyphNumber:glyph->glyph_id
    val glyphWidth:glyph->float
    val fontName:?index:int->font->string
    val substitutions:font->glyph_id list->glyph_id list
    val positioning:font->glyph_ids list->glyph_ids list
  end)

let kern contents=match contents with
    KernID x->x
  | GlyphID x->{ advance_height=0.; advance_width=0.; kern_x0=0.;kern_y0=0.; kern_contents=contents }
let rec glyph_id_cont=function
    KernID x->glyph_id_cont x.kern_contents
  | GlyphID x->x.glyph_index

let rec glyph_id_utf8=function
    KernID x->glyph_id_utf8 x.kern_contents
  | GlyphID x->x.glyph_utf8


type ligature= { ligature_glyphs:int array; ligature:int }
type subst= { original_glyphs:int array; subst_glyphs:int array }
type chain= { before:IntSet.t array; input:IntSet.t array; after:IntSet.t array }
type substitution=
    Alternative of int array
  | Subst of subst
  | Ligature of ligature
  | Chain of chain
  | Context of (int*(substitution list)) array

let apply glyphs0 subst=match subst with
    Ligature lig->
      (let rec apply_lig i buffer glyphs=
         if i>=Array.length lig.ligature_glyphs then (
           { glyph_utf8=UTF8.Buf.contents buffer; glyph_index=lig.ligature }::glyphs
         ) else (
           match glyphs with
               []->[]
             | h::s when (h.glyph_index=lig.ligature_glyphs.(i))->(
                 UTF8.Buf.add_string buffer h.glyph_utf8;
                 apply_lig (i+1) buffer s
               )
             | _->[]
         )
       in
       let rec apply_all buf l=match l with
           []->[]
         | h::s->
             (UTF8.Buf.clear buf;
              match apply_lig 0 buf l with
                  []->h::(apply_all buf s)
                | l'->(apply_all (UTF8.Buf.create 2) l')
             )
       in
         apply_all (UTF8.Buf.create 2) glyphs0
      )
  | _->glyphs0
