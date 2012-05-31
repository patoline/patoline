


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

module Euler=DefaultFormat.Euler

module Format=functor (D:DocumentStructure)->struct

  module Default=DefaultFormat.Format(D)
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

module Env_equation=Default.Make_theorem
  (struct
    let refType="equation"
    let counter="equation"
    let counterLevel=0
    let display num=toggleItalic [T ("("^num^")"); (T " ")]
   end)


module Env_Diagram (Args : sig val arg1 : string end)(Args' : sig val env : user environment end) = struct
  open Diagrams.Diagram
    let stack = ref []
    let env = Args'.env

    let node style contents = 
      let a = node env style contents in
      stack := a :: !stack ;
      a

    let coordinate p = 
      let a = coordinate env p in
      let _ = stack := a :: !stack in
      a

    let edge style a controls b =
      let e = edge style a controls b in
      stack := e :: !stack ;
      e

    let edges style edge_list = 
      let l = edges style edge_list in
      stack := l @ !stack ;
      l

    let matrix style lines = 
      let node, m = matrix env style lines in
      stack := node :: ((List.flatten (Array.to_list (Array.map Array.to_list m))) @ !stack) ;
      node, m

    let make () = 
      let fig = Box.drawing_inline ~offset:(0.) 
	(List.fold_left (fun res gentity -> List.rev_append gentity.contents res)
	   []
	   !stack)
      in
      stack := [] ; fig

  end



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
