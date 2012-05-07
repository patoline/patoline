


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
  type user=Document.user
  module Default=DefaultFormat.Format(D)

  let defaultEnv=Default.defaultEnv
  let title=Default.title
  let author=Default.author
  let institute=Default.institute

  let caml x = x

let postprocess_tree tree=
  let with_title=match tree with
      Node n->
        let par=Paragraph {
          par_contents=n.displayname;
          par_env=resize_env 8.;
          par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                             user_positions=env2.user_positions });
          par_parameters=
            (fun a b c d e f g->
               { (center a b c d e f g) with
                   min_height_after=2.*.a.normalLead;
                   min_height_before=2.*.a.normalLead });
          par_completeLine=Complete.normal }
        in
          fst (up (newChildBefore (tree,[]) par))
    | _->tree
  in
  let rec sectionize depth=function
      Node n when List.mem Structural n.node_tags ->
        let section_name=
          if List.mem Numbered n.node_tags  then
            [C (fun env->
                  let a,b=try StrMap.find "structure" env.counters with Not_found -> -1,[] in
                  let _,path'=try StrMap.find "path" env.counters with Not_found -> -1,[] in
                  let path=drop 1 b in
                    B (fun _->[User (Structure path')])
                    ::T (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev path)))
                    :: T " "
                    ::n.displayname
               )]
          else
            B (fun env->
                 let _,path=try StrMap.find "path" env.counters with Not_found -> -1,[] in
                   [User (Structure path)])::
              n.displayname
        in
        let par=Paragraph {
          par_contents=section_name;
          par_env=(fun env->
                     let a,b=try StrMap.find "structure" env.counters with Not_found -> -1,[] in
                     let path=drop 1 b in
                       { (envAlternative (Opentype.oldStyleFigures::env.fontFeatures) Caps env) with
                           size=(if List.length path <= 1 then sqrt phi else
                                   sqrt (sqrt phi))*.env.size;
                       });
          par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                             user_positions=env2.user_positions });
          par_parameters=
            (fun a b c d e f g->
              { (parameters a b c d e f g) with
                   min_page_before= 0;
                   min_page_after= 0;
                   (* if depth=0 && f.lineStart=0 then 1 else 0; *)
                   min_height_after=2.*.a.normalLead;
                   min_height_before=2.*.a.normalLead });
          par_completeLine=Complete.normal }
        in
          fst (up (newChildBefore (
                     Node { n with children=IntMap.map (sectionize (depth+1)) n.children }, []) par
                  ))
    | a->a
  in
  let with_chapters=match with_title with
      Node n->Node { n with children=IntMap.map (sectionize 0) n.children }
    | _->with_title
  in
    with_chapters

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
module Env_abstract = Default.Env_abstract	
module Env_center = Default.Env_center	


    module Env_equation = struct

      let refType = "equation"
      let counter = "equation"
      let counterLevel=0
      let display num=[T ("("^num^")")]

      let reference name=generalRef refType name

      let do_begin_env ()=
        D.structure:=newChildAfter !D.structure (Node empty);
        Default.env_stack:=snd !D.structure :: !Default.env_stack

      let do_end_env ()=
        let rec first_par=function
            Paragraph p->
              Paragraph { p with
                            par_parameters=(fun a b c d e f g->
                                              { (parameters a b c d e f g) with
                                                  min_height_before=
                                                  if g.lineStart=0 then 2.*.a.lead else a.lead });
                            par_contents=
                  Env (fun env->incr_counter ~level:counterLevel env counter)::
                    CFix (fun env->
                            let lvl,num=try (StrMap.find counter env.counters) with
                                Not_found -> -1,[0]
                            in
                            let _,str_counter=try
                              StrMap.find "structure" env.counters
                            with Not_found -> -1,[0]
                            in
                            let sect_num=drop (max 1 (List.length str_counter - lvl+1))
                              str_counter
                            in
                              display (String.concat "." (List.map (fun x->string_of_int (x+1)) ((List.rev sect_num)@num)))
                         )::
                    T " "::
                    p.par_contents
                        }
          | Node n->
              let k0=try fst (IntMap.min_binding n.children) with Not_found->0 in
              let paragraph=IntMap.singleton k0
                (first_par (Paragraph
                              { par_contents=[]; par_env=(fun x->x);
                                par_post_env=
                                  (fun env1 env2 -> { env1 with names=env2.names;
                                                        counters=env2.counters;
                                                        user_positions=env2.user_positions });
                                par_parameters=parameters; par_completeLine=Complete.normal
                              }))
              in
                Node { n with children=IntMap.fold (fun k a b->IntMap.add (k+1) a b)
                    n.children paragraph }
          | x -> x
        in
        let rec last_par=function
            Paragraph p->
              Paragraph { p with
                            par_parameters=(fun a b c d e f g->
                                              { (p.par_parameters a b c d e f g) with
                                                  min_height_after=
                                                  if g.lineEnd>=Array.length b.(g.paragraph) then 2.*.a.lead else a.lead });
                        }
          | Node n->
              let k0,a0=IntMap.max_binding n.children in
                Node { n with children=IntMap.add k0 (last_par a0) n.children }
          | x -> x
        in
        let stru=match follow (top !D.structure) (List.rev (List.hd !Default.env_stack)) with
            Node n,_->
              (try
                 let a,b=IntMap.min_binding n.children in
                   Node { n with children = IntMap.add a (first_par b) n.children }
               with
                   Not_found->first_par (Node n))
          | x,_->first_par x
        in
        let center p = { p with par_parameters=Document.do_center p.par_parameters } in
	  D.structure := up (map_paragraphs center (last_par stru),List.hd !Default.env_stack);
        Default.env_stack:=List.tl !Default.env_stack
    end

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
      let fig = Box.drawing ~offset:(0.) 
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
