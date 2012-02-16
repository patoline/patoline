(** All common types used for fonts, as well as helpers for "font features" *)
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

let str_of_feature=function
    Alternates -> "Alternates\n"
  | SmallCapitals ->  "SmallCapitals\n"
  | CaseSensitiveForms -> "CaseSensitiveForms\n"
  | DiscretionaryLigatures -> "DiscretionaryLigatures\n"
  | Denominators -> "Denominators\n"
  | Fractions -> "Fractions\n"
  | StandardLigatures -> "StandardLigatures\n"
  | LiningFigures -> "LiningFigures\n"
  | LocalizedForms -> "LocalizedForms\n"
  | Numerators -> "Numerators\n"
  | OldStyleFigures -> "OldStyleFigures\n"
  | Ordinals -> "Ordinals\n"
  | Ornaments -> "Ornaments\n"
  | ProportionalFigures -> "ProportionalFigures\n"
  | StylisticAlternates -> "StylisticAlternates\n"
  | ScientificInferiors -> "ScientificInferiors\n"
  | Subscript -> "Subscript\n"
  | Superscript ->  "Superscript\n"
  | Titling -> "Titling\n"
  | TabularFigures -> "TabularFigures\n"
  | SlashedZero -> "SlashedZero\n"
let print_feature x=Printf.printf "%s" (str_of_feature x)

type subst= { original_glyphs:int array; subst_glyphs:int array }
type chain= { before:IntSet.t array; input:IntSet.t array; after:IntSet.t array }
type substitution=
    Alternative of int array
  | Subst of subst
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
  | Chain x->(printf "Chain { ... }\n")
  | Context _->(printf "Context { ... }\n")



let apply_ligature glyphs0 lig=
  let rec apply_lig i buffer glyphs=
    if i>=Array.length lig.original_glyphs then (
      { glyph_utf8=UTF8.Buf.contents buffer; glyph_index=lig.subst_glyphs.(0) }::glyphs
    ) else (
      match glyphs with
          []->[]
        | h::s when (h.glyph_index=lig.original_glyphs.(i))->(
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
  ) else if Array.length x.subst_glyphs=1 then apply_ligature glyphs0 x else (
    let rec matches a i=
      if i>=Array.length x.original_glyphs then (
        (Array.to_list (Array.map (fun u->{ glyph_utf8=""; glyph_index=u }) x.subst_glyphs))@a
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
  | _->glyphs0



module type Font=(
  sig
    (** To understand this interface, one must understand the
        difference between a glyph and a character. While most UTF8
        characters can have corresponding glyphs, a glyph may not
        represent exactly one character. Sometimes it will represent
        more than one character (as in the case of ligatures) and
        sometimes none (as in the case of ornaments). To ease the
        manipulation of glyphs, this modules allows to manipulate
        integers representing their indices in the font, for instance
        to write glyph substitutions more easily, and also to load
        informations such as their outlines. *)

    (** Type font *)
    type font
    type glyph

    (** Loads a font in the memory from a file name. Actual
        implementations may keep the file open *)
    val loadFont: ?offset:int-> ?size:int->string->font

    (** Computes the index of a glyph corresponding to a given
        character in the font, but loses the link between the character
        and the glyph. It is your responsibility to maintain this link
        with the [glyph_id] type *)
    val glyph_of_char:font->char->int
    val glyph_of_uchar:font->UChar.t->int

    (** Load the actual glyph, which gives more precise knownledge
        about the glyph : its width and outlines, for instance *)
    val loadGlyph:font-> ?index:int ->glyph_id->glyph

    (** Outlines of the glyph as a list of Bezier curves, each given
        by two arrays of coefficients of Bernstein polynomials. The
        degree of the polynomial is the length of the array minus
        one. *)
    val outlines:glyph->(float array*float array) list
    val glyphFont:glyph->font
    val glyphNumber:glyph->glyph_id
    val glyphWidth:glyph->float
    val fontName:?index:int->font->string

    (** Lists all the available features of the font *)
    val font_features:font->features list

    (** Converts a given list of features into a list of corresponding substitutions *)
    val select_features:font->features list->substitution list

    (** Applies the available positioning information to a glyph
        list. This can be used for kerning, but not only *)
    val positioning:font->glyph_ids list->glyph_ids list
  end)
