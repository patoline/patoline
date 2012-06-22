open Typography
open Typography.Fonts
open Typography.Fonts.FTypes
(* open Typography.Constants *)
open Typography.Document
open Util
open Box
open Line
(* open Binary *)
open CamomileLibrary
module CM = CamomileLibraryDefault.Camomile.CaseMap.Make(CamomileLibrary.UTF8)


let boxes_width env contents = 
  let boxes = boxify_scoped env contents in
  let w = List.fold_left
    (fun w x -> let _,a,_ = Box.box_interval x in w +. a)
    0.
    boxes
  in
  boxes, w

let boxes_y0 boxes = 
  List.fold_left
    (fun res box -> min res (Box.lower_y box 0.))
    0.
    boxes

let boxes_y1 boxes = 
  List.fold_left
    (fun res box -> max res (Box.upper_y box 0.))
    0.
    boxes


module Euler=DefaultFormat.Euler

module MakeFormat (D:DocumentStructure)
  (Default : module type of DefaultFormat.Format(D)) = struct

  module MathFonts = DefaultFormat.MathFonts
  include Default

  let title=Default.title
  let author=Default.author
  let institute=Default.institute

  let caml x = x
  let displayedFormula=Default.displayedFormula


module Env_definition=Default.Make_theorem
  (struct
    let refType="definition"
    let counter="definition"
    let counterLevel=0
    let display num=alternative Bold [T ("Definition "^num^"."); (T " ")]
   end)
module Env_theorem=Default.Make_theorem
  (struct
    let refType="theorem"
    let counter="theorem"
    let counterLevel=0
    let display num=alternative Bold [T ("Theorem "^num^"."); (T " ")]
   end)
module Env_lemma=Default.Make_theorem
  (struct
    let refType="lemma"
    let counter="lemma"
    let counterLevel=0
    let display num=alternative Bold [T ("Lemma "^num^"."); (T " ")]
   end)
module Env_proposition=Default.Make_theorem
  (struct
    let refType="proposition"
    let counter="proposition"
    let counterLevel=0
    let display num=alternative Bold [T ("Proposition "^num^"."); (T " ")]
   end)
module Env_corollary=Default.Make_theorem
  (struct
    let refType="corollary"
    let counter="corollary"
    let counterLevel=0
    let display num=alternative Bold [T ("Corollary "^num^"."); (T " ")]
   end)
(* module Env_proof=Default.Proof *)

  let equation contents = 
    let pars a b c d e f g={(parameters a b c d e f g) with
                              min_height_before=
        if g.lineStart=0 then a.lead else 0.;
                              min_height_after=
        if g.lineEnd>=Array.length b.(g.paragraph) then a.lead else 0.
                           }
    in
    newPar ~environment:(fun env -> { env with par_indent = [] })
      D.structure Complete.normal pars
      [ Env (fun env ->Document.incr_counter "equation" env) ;
        C (fun env ->
	     let _,w = boxes_width env contents in
	     let _,x = StrMap.find "equation" env.counters in
	     let num,w' = boxes_width env
	       (italic [T "(";
		        T (string_of_int (1 + List.hd x));
		        T ")" ]) in
             let w0=(env.normalMeasure -. w)/.2. in
             let w1=env.normalMeasure -. w'-.w0-.w in
             B(fun _->[glue w0 w0 w0])::
               contents@
               [B (fun _->glue w1 w1 w1 :: num)]
	  )];
    []

(* Default.Make_theorem *)
(*   (struct *)
(*     let refType="equation" *)
(*     let counter="equation" *)
(*     let counterLevel=0 *)
(*     let display num=toggleItalic [T ("("^num^")"); (T " ")] *)
(*    end) *)





  open Util
  (* open Binary *)


  let utf8Char x=[T (UTF8.init 1 (fun _->UChar.chr x))]
  let glyph x=
    B (fun env->
         let code={glyph_utf8=""; glyph_index=x } in
           [GlyphBox { (Box.glyphCache env.font code) with
                         OutputCommon.glyph_color=env.fontColor;
                         OutputCommon.glyph_size=env.size
                     }]
      )
  let q _=utf8Char 8220
  let qq _=utf8Char 8221

end

module Format (D : DocumentStructure) =
  MakeFormat (D) (DefaultFormat.Format(D))

module Output = DefaultFormat.Output
