(** XPlacement, YPlacement, XAdvance, YAdvance *)
open CamomileLibrary
open Binary

type 'a kerningBox= { advance_height:float; advance_width:float; kern_x0:float; kern_y0:float; kern_contents:'a }

type glyph_id = { glyph_utf8:CamomileLibrary.UTF8.t; glyph_index:int }
let empty_glyph= { glyph_utf8=UTF8.init 0 (fun _->UChar.chr 0); glyph_index=0 }
type glyph_ids=KernID of (glyph_ids kerningBox) | GlyphID of glyph_id

let kern contents=match contents with
    KernID x->x
  | GlyphID x->{ advance_height=0.; advance_width=0.; kern_x0=0.;kern_y0=0.; kern_contents=contents }
let rec glyph_id_cont=function
    KernID x->glyph_id_cont x.kern_contents
  | GlyphID x->x.glyph_index

let rec glyph_id_utf8=function
    KernID x->glyph_id_utf8 x.kern_contents
  | GlyphID x->x.glyph_utf8

exception Unknown_feature of string
type features=
    Alternates
  | SmallCapitals
  | CaseSensitiveForms
  | DiscretionaryLigatures
  | Denominators
  | Fractions
  | StandardLigatures
  | LiningFigures
  | LocalizedForms
  | Numerators
  | OldStyleFigures
  | Ordinals
  | Ornaments
  | ProportionalFigures
  | StylisticAlternates
  | ScientificInferiors
  | Subscript
  | Superscript
  | Titling
  | TabularFigures
  | SlashedZero

let print_feature=function
    Alternates -> Printf.printf "Alternates\n"
  | SmallCapitals -> Printf.printf "SmallCapitals\n"
  | CaseSensitiveForms -> Printf.printf "CaseSensitiveForms\n"
  | DiscretionaryLigatures -> Printf.printf "DiscretionaryLigatures\n"
  | Denominators -> Printf.printf "Denominators\n"
  | Fractions -> Printf.printf "Fractions\n"
  | StandardLigatures -> Printf.printf "StandardLigatures\n"
  | LiningFigures -> Printf.printf "LiningFigures\n"
  | LocalizedForms -> Printf.printf "LocalizedForms\n"
  | Numerators -> Printf.printf "Numerators\n"
  | OldStyleFigures -> Printf.printf "OldStyleFigures\n"
  | Ordinals -> Printf.printf "Ordinals\n"
  | Ornaments -> Printf.printf "Ornaments\n"
  | ProportionalFigures -> Printf.printf "ProportionalFigures\n"
  | StylisticAlternates -> Printf.printf "StylisticAlternates\n"
  | ScientificInferiors -> Printf.printf "ScientificInferiors\n"
  | Subscript -> Printf.printf "Subscript\n"
  | Superscript -> Printf.printf "Superscript\n"
  | Titling -> Printf.printf "Titling\n"
  | TabularFigures -> Printf.printf "TabularFigures\n"
  | SlashedZero -> Printf.printf "SlashedZero\n"

type ligature= { ligature_glyphs:int array; ligature:int }
type subst= { original_glyphs:int array; subst_glyphs:int array }
type chain= { before:IntSet.t array; input:IntSet.t array; after:IntSet.t array }
type substitution=
    Alternative of int array
  | Subst of subst
  | Ligature of ligature
  | Chain of chain
  | Context of (int*(substitution list)) array

open Format
open Printf
let print_int_array x=
  open_box 0;
  Printf.printf "[| ";
  for i=0 to Array.length x-1 do
    (if i=Array.length x-1 then Printf.printf "%d" else Printf.printf "%d; ") (x.(i)) done;
  Printf.printf " |]";
  close_box ()

let print_int_list x=
  open_box 0;
  Printf.printf "[ ";
  let rec print=function
      []->printf "]"
    | [h]->printf "%d ]" h
    | h::s->(printf "%d; " h;print s)
  in
    print x;
    close_box ()

let print_subst=function
    Alternative x->(printf "Alternative ";print_int_array x;printf "\n")
  | Subst x->(printf "Subst { original_glyphs=";open_box 1; print_int_array x.original_glyphs;close_box();
              printf "; subst_glyphs=";open_box 1; print_int_array x.subst_glyphs; close_box();
              printf " }\n")
  | Ligature x->(printf "Ligature { ligature_glyphs=";open_box 1; print_int_array x.ligature_glyphs;close_box();
                 printf "; ligature= %d }\n" x.ligature)
  | Chain x->(printf "Chain { ... }\n")
  | Context _->(printf "Context { ... }\n")



let apply_ligature glyphs0 lig=
  let rec apply_lig i buffer glyphs=
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


let apply_subst glyphs0 x=
  if Array.length x.original_glyphs = Array.length x.subst_glyphs then (
    let rec replace a i=
      if i>=Array.length x.subst_glyphs then a else
        (match a with
             []->[]
           | h::s->{ h with glyph_index=x.subst_glyphs.(i)}::(replace s (i+1)))
    in
    let rec matches a i=i>=Array.length x.original_glyphs ||
      (match a with
           []->false
         | h::s->(h.glyph_index=x.original_glyphs.(i) && matches s (i+1))
      )
    in
    let rec apply_all l=match l with
        []->[]
      | _ when (matches l 0) -> apply_all (replace l 0)
      | h::s->h::(apply_all s)
    in
      apply_all glyphs0
  ) else (
    let rec matches a i=
      if i>=Array.length x.original_glyphs then (
        Array.to_list (Array.map (fun u->{ glyph_utf8=""; glyph_index=u }) x.subst_glyphs)
      ) else (
        match a with
            []->[]
          | h::s when h.glyph_index=x.original_glyphs.(i)->matches s (i+1)
          | _->[]
      )
    in
    let rec apply_all l=match l with
        []->[]
      | h::s->(
          match matches l 0 with
              []->h::apply_all s
            | x->apply_all x
        )
    in
      apply_all glyphs0
  )




let apply_alternative glyphs0 alt i=List.map (fun x->if x.glyph_index=alt.(0) then { x with glyph_index=alt.(i) } else x) glyphs0

let apply glyphs0 subst=match subst with
    Alternative x -> glyphs0
  | Subst x -> apply_subst glyphs0 x
  | Ligature x -> apply_ligature glyphs0 x
  | _->glyphs0



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
    val select_features:font->features list->substitution list
    val substitutions:font->glyph_id list->glyph_id list
    val positioning:font->glyph_ids list->glyph_ids list
  end)
