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

open Bibi
exception No_bib of string
let bib:((string*int*user content list) IntMap.t) ref=ref IntMap.empty
let bibfile="biblio.bibi"
let raw_cite x=
  let num a b=try
    let _,y,_=(IntMap.find a !bib) in y
  with
      Not_found->
        let key=if IntMap.is_empty !bib then 1 else
          (let _,(_,b,_)=IntMap.max_binding !bib in
             b+1)
        in
          bib:=IntMap.add a (bibfile,key, b) !bib;
          key
  in
    match Bibi.bibitem bibfile x with
        []-> raise (No_bib (Printf.sprintf "Request gave no results : %s\n" x))
      | [a,b]->[T (Printf.sprintf "[%d]" (num a b))]
      | (a,b)::_::_->(
          Printf.fprintf stderr "Warning : more than one result for request : %s\n" x;
          [T (Printf.sprintf "[%d]" (num a b))]
        )

let cite ?title:(title="") ?year:(year=None) (authors:string list) =
  raw_cite ("title like '%" ^ title ^ "%' AND id IN " ^
    (String.concat " AND id IN " 
       (List.map
	  (fun author -> 
	    ("(SELECT article FROM authors_publications WHERE author IN (SELECT id FROM "^
		"authors WHERE name like '%" ^ author ^ "%'))"))
	  authors)))

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

module Format=functor (D:DocumentStructure)->struct

  module Default=DefaultFormat.Format(D)

  module MathFonts = DefaultFormat.MathFonts
  include Default

  let title=Default.title
  let author=Default.author
  let institute=Default.institute

  let caml x = x
  let displayedFormula=Default.displayedFormula


let thebibliography ()=
    List.iter (fun (a,(_,b,c))->
                 let params env a1 a2 a3 a4 a5 line=
                   let p=parameters env a1 a2 a3 a4 a5 line in
                     if line.lineStart=0 then (
                       let num=boxify_scoped env [T (Printf.sprintf "[%d]" b); (T " ")] in
                       let w=List.fold_left (fun w0 b->let (_,w,_)=box_interval b in w0+.w) 0. num in
                         { p with left_margin=p.left_margin-.w; measure=p.measure+.w }
                     ) else
        	       p
                 in
                   newPar D.structure ~environment:(fun x -> { x with par_indent = [] })
                     Complete.normal params
                     (T (Printf.sprintf "[%d]" b)::(T " ")::c)) (IntMap.bindings !bib)


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
  newPar ~environment:(fun env -> { env with par_indent = [] }) 
    D.structure Complete.normal parameters 
    [ Env (fun env ->   Document.incr_counter "equation" env) ;
      B (fun env ->
	let boxes,w = boxes_width env contents in 
	let drawn_boxes = Box.draw_boxes boxes in 
	let _,x = StrMap.find "equation" env.counters in
	let num,w' = boxes_width env
	  (italic [T "(";
		   T (string_of_int (1 + List.hd x));
		   T ")" ]) in 
	let drawn_num = Box.draw_boxes num in 
	let y0 = boxes_y0 boxes in
	let y1 = boxes_y1 boxes in
	let y = 0.5 *. (y0 +. y1) in
	[Drawing {
	  drawing_min_width = env.normalMeasure ;
	  drawing_max_width = env.normalMeasure ;
	  drawing_nominal_width = env.normalMeasure ;
	  drawing_badness = (fun _ -> 0.) ;
	  drawing_y0 = min y0 (boxes_y0 num) ;
	  drawing_y1 = max y1 (boxes_y1 num) ;
	  drawing_contents = (fun _ -> 
	    (List.map (OutputCommon.translate (0.5 *. 
						 (env.normalMeasure -. w)) 0.) 
	       drawn_boxes) @
	      (List.map (OutputCommon.translate (env.normalMeasure -. w') y) drawn_num))
	}]
      )] ;
  D.structure := lastChild !D.structure ;
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

module Output = DefaultFormat.Output
