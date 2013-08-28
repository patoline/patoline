(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
open Typography
open Typography.Fonts
open Typography.Fonts.FTypes
open Typography.Document
open Typography.Util
open Typography.ConfigUtil
open CamomileLibrary
open Typography.Box
open Printf
module CM = CamomileLibraryDefault.Camomile.CaseMap.Make(CamomileLibrary.UTF8)

let id x=x
let emph x=toggleItalic x
let from x=emph (tT "(from"::tT " "::x@[tT")"])

let mathsT t0=
  let t1=if t0.[String.length t0-1]=' ' then tT t0::[tT" "] else [tT t0] in
  let t=if t0.[0]=' ' then tT" "::t1 else t1 in
  let l=
    Maths.Ordinary (Maths.noad (fun env st->boxify_scoped
                                  { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
                                  t ))
  in
  [l]

let mathsText t0=
  [Maths.Ordinary (Maths.noad (fun env st->boxify_scoped
                                 { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
                                 t0 ))]


let skip x=bB (fun env->let w=env.size*.x in [glue w w w])
(* let loadFamily dir reg= *)
(*   let shorter l= *)
(*     (List.sort (fun a b->compare (String.length a) (String.length b)) l) *)
(*   in *)
(*   let fam alt l= *)
(*     let it,regular=List.partition (fun x->Str.string_match (Str.regexp_case_fold "italic") x 0) l in *)
(*       match shorter it,shorter regular with *)
(*           h::_,h'::_->[alt,((simpleFamilyMember (Fonts.loadFont h)), *)
(*                             ((simpleFamilyMember (Fonts.loadFont h'))))] *)
(*         | h::_,_ *)
(*         | _,h::_->[alt, ((simpleFamilyMember (Fonts.loadFont h)), *)
(*                          (simpleFamilyMember (Fonts.loadFont h)))] *)
(*         | [],[]->[] *)
(*   in *)
(*   let names=List.filter (fun x->Str.string_match reg x 0) (Array.to_list (Sys.readdir dir)) in *)
(*   let bold,a=List.partition (fun x->Str.string_match (Str.regexp_case_fold "bold") x 0) names in *)
(*   let caps,b=List.partition (fun x-> *)
(*                                Str.string_match (Str.regexp_case_fold "-sc\\.\\|_sc\\.\\|caps") x 0) a in *)
(*   let demi,c=List.partition (fun x-> *)
(*                                Str.string_match (Str.regexp_case_fold "demi") x 0) b in *)
(*   let regular=shorter c in *)
(*   let cacaps= *)
(*     match caps with *)
(*         _::_->fam Caps caps *)
(*       | []-> *)
(*           let c=Lazy.lazy_from_fun ( *)
(*             fun () -> *)
(*               let f=Fonts.loadFont (findFont "PalatinoLTStd-Roman.otf") in *)
(*               let subst=Fonts.select_features f [Opentype.smallCapitals] in *)
(*                 (f, *)
(*                  (fun x->CM.uppercase x), *)
(*                  (fun glyphs -> List.fold_left apply glyphs subst), *)
(*                  (fun x->x)) *)
(*           ) *)
(*           in *)
(*             [Caps, (c,c)] *)
(*   in *)
(*     (fam Regular regular) *)
(*     @(fam Bold bold) *)
(*     @(cacaps) *)
(*     @(fam Demi demi) *)

include (DefaultFormat:module type of DefaultFormat with module Format:=DefaultFormat.Format)

module Format=functor (D:DocumentStructure)->struct

  module Default=DefaultFormat.Format(D)
  include (Default:module type of Default with module Output:=Default.Output)



let postprocess_tree tree=
  let with_title=tree
    (* match tree with *)
    (*   Node n when n.displayname<>[]-> *)
    (*     let par=Paragraph { *)
    (*       par_contents=n.displayname; *)
    (*       par_paragraph=(-1);par_states=IntSet.empty; *)
    (*       par_env=resize_env 3.; *)
    (*       par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters; *)
    (*                                          user_positions=env2.user_positions }); *)
    (*       par_badness=badness; *)
    (*       par_parameters= *)
    (*         (fun a b c d e f g line-> *)
    (*            { (center a b c d e f g line) with *)
    (*              min_height_after= *)
    (*                if line.lineEnd>=Array.length (b.(line.paragraph)) then 2.*.a.normalLead else 0.; *)
    (*              min_height_before=0. }); *)
    (*       par_completeLine=Complete.normal } *)
    (*     in *)
    (*       fst (up (newChildBefore (tree,[]) par)) *)
    (* | _->tree *)
  in
  let rec sectionize path=function
      Node n when List.mem_assoc "structural" n.node_tags ->
        let section_name=
          if path=[] then (
            [C (fun env->
              let h= -.env.size/.phi in
              let sz=2.5 in

              let buf=ref [||] in
              let nbuf=ref 0 in
              let env'=boxify buf nbuf env n.displayname in
              let boxes=Array.to_list (Array.sub !buf 0 !nbuf) in
              let users=
                List.filter (fun x->match x with
                    Marker _->true | _->false)
                  boxes
              in

              let num=
                if List.mem_assoc "numbered" n.node_tags then
                  let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                  List.map (OutputCommon.in_order 1)
                    (draw {env with size=env.size*.(sz-.h);fontColor=OutputCommon.gray }
                       [tT (String.concat "." (List.map (fun x->string_of_int (x+1))
                                                 (List.rev (drop 1 b))))])
                else
                  []
              in
              let x0,x1=if num=[] then 0.,0. else
                  let x0,_,x1,_=OutputCommon.bounding_box num in x0,x1
              in
              let w=if num=[] then 0. else env.size in

              let text=
                let dr=try
                         snd (IntMap.min_binding (
                           OutputDrawing.minipage {env with hyphenate=(fun _->[||]);
                             normalLeftMargin=0.;
                             normalMeasure=env.normalMeasure-.(x1-.x0)/.2.-.w;
                             size=env.size*.sz}
                             (paragraph n.displayname)
                         ))
                  with
                      Not_found->empty_drawing_box
                in
                List.map (OutputCommon.in_order 1)
                  (dr.drawing_contents dr.drawing_nominal_width)
              in
              let dr=drawing (
                (List.map (OutputCommon.translate (-.w-.x1) h) num)@
                  text
              )
              in
              let dr={dr with drawing_contents=(fun w_->
                List.map (OutputCommon.translate ((x0-.x1)/.2.) 0.) (dr.drawing_contents w_)
              )}
              in
              bB (fun _->
                users@
                  [Marker (Structure path);
                   Marker AlignmentMark;
                   Drawing (dr)])::
                Env(fun _->env')::[]
            )]
          ) else (
            if List.mem_assoc "numbered" n.node_tags  then
              [C (fun env->
                let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                bB (fun _->[Marker (Structure path)])
                ::tT (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev (drop 1 b))))
                ::tT " "
                ::n.displayname
              )]
            else
              bB (fun env->[Marker (Structure path)])::
                n.displayname
          )
        in
        let par=Paragraph {
          par_contents=section_name;
          par_paragraph=(-1);par_states=IntSet.empty;
          par_env=(fun env->
                     let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in

                     { (envAlternative (Fonts.Opentype.oldStyleFigures::env.fontFeatures)
                          (if List.length b>=4 then Regular else Caps) env) with
                       hyphenate=(fun _->[||]);
                         size=(if List.length b=1 then sqrt phi else
                                 if List.length b <= 2 then sqrt (sqrt phi) else
                                   if List.length b = 3 then sqrt (sqrt (sqrt phi)) else 1.)*.env.size;
                     });
          par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                             user_positions=env2.user_positions });
          par_badness=badness;
          par_parameters=
            if path=[] then
              (fun env a1 a2 a3 a4 a5 a6 line->
                let p=parameters env a1 a2 a3 a4 a5 a6 line in
                if not p.absolute && line.lineStart=0 then (
                  let rec findMark w j=
                    if j>=line.lineEnd then 0. else
                      if a1.(line.paragraph).(j) = Marker AlignmentMark then w else
                        let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                        findMark (w+.ww) (j+1)
                  in
                  let w=findMark 0. 0 in
                  { p with
                    left_margin=p.left_margin-.w;
                    min_lines_after=if line.lineEnd>=Array.length a1.(line.paragraph) then 4 else 1;
                    min_page_before = (
                      if line.paragraph=0 then 0 else
                      if path=[] && line.lineStart<=0 then (
                        let minimal=max p.min_page_before 1 in
                        minimal+((1+max 0 (page line)+minimal) mod 2)
                      ) else p.min_page_before
                    );
                    measure=p.measure+.w }
                ) else
                  p
              )
            else
              (fun a b c d e f g line->
                let param=parameters a b c d e f g line in
                { param with
                  min_page_before = (
                    if path=[] && line.lineStart<=0 then (
                      let minimal=max param.min_page_before 1 in
                      minimal+((page g+minimal) mod 2)
                    ) else param.min_page_before
                  );
                  min_lines_before=2;
                  min_lines_after=
                    if path=[] then
                      if line.lineEnd>=Array.length b.(line.paragraph) then 3 else 0
                    else
                      if line.lineEnd>=Array.length b.(line.paragraph) then 2 else 0;
                  not_last_line=true }
              );
          par_completeLine=Complete.normal }
        in
          Node { n with children=
              IntMap.add
                (try fst (IntMap.min_binding n.children)-1 with Not_found->0)
                par
                (IntMap.mapi (fun k a->sectionize (k::path) a) n.children)
               }
    | a->a
  in
  let with_chapters=match with_title with
      Node n->Node { n with children=IntMap.map (sectionize []) n.children }
    | _->with_title
  in
    with_chapters

module Output (M:OutputPaper.Driver)=struct
  module Def=Default.Output(M)
  include Def
  let output out_params (structure:Document.tree) (defaultEnv:Document.environment) file=
    Def.basic_output out_params (postprocess_tree structure) defaultEnv file
end

  let minipage=OutputDrawing.minipage
  let displayedFormula=Default.displayedFormula
  let node=DefaultFormat.node
  let paragraph=DefaultFormat.paragraph


  let alegreya=
    [ Regular,
      (Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Roman; weight=Regular}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [168;175] {glyph_utf8="fi";glyph_index=245};
                make_ligature [168;181] {glyph_utf8="fl";glyph_index=246};
                make_ligature [168;177] {glyph_utf8="fj";glyph_index=383};
                make_ligature [175;177] {glyph_utf8="ij";glyph_index=176};
               ]),
            (fun x->x)),
       Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Italic; weight=Regular}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [162;170] {glyph_utf8="fi";glyph_index=477};
                make_ligature [162;175] {glyph_utf8="fl";glyph_index=478};
                make_ligature [162;171] {glyph_utf8="fj";glyph_index=482};
                make_ligature [170;171] {glyph_utf8="ij";glyph_index=476};
               ]),
            (fun x->x)));
      Bold,
      (Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Roman; weight=Bold}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [168;175] {glyph_utf8="fi";glyph_index=245};
                make_ligature [168;181] {glyph_utf8="fl";glyph_index=246};
                make_ligature [168;177] {glyph_utf8="fj";glyph_index=383};
                make_ligature [175;177] {glyph_utf8="ij";glyph_index=176};
               ]),
            (fun x->x)),
       Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Italic; weight=Bold}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [162;170] {glyph_utf8="fi";glyph_index=477};
                make_ligature [162;175] {glyph_utf8="fl";glyph_index=478};
                make_ligature [162;171] {glyph_utf8="fj";glyph_index=482};
                make_ligature [170;171] {glyph_utf8="ij";glyph_index=476};
               ]),
            (fun x->x)));

      Caps,
      (
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Alegreya SC"; slant=Roman; weight=Regular})
        )),
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Alegreya SC"; slant=Italic; weight=Regular})
        ))
      );

      Regular,
      (
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Roman; weight=Regular})
        )),
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Italic; weight=Regular})
        ))
      );

      Bold,
      (
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Roman; weight=Bold})
        )),
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Italic; weight=Bold})
        ))
      );
    ]

  let replace_utf8 x y z=
    Str.global_replace x
      (UTF8.init 1 (fun _->UChar.chr y)) z
  let defaultEnv=
    let size=3.8 in
    let env=(envFamily alegreya Default.defaultEnv) in
    {  env with
         size=size;
         show_boxes=false;
         lead=5.5;
         mathsEnvironment=
        (* Array.map (fun x->{x with Mathematical.kerning=false }) *)
          env.mathsEnvironment;
         word_substitutions=
        (fun x->List.fold_left (fun y f->f y) x
           [
             replace_utf8 (Str.regexp_string "``") 8220;
             replace_utf8 (Str.regexp_string "''") 8221
           ]
        );
         counters=StrMap.add "figure" (2,[]) StrMap.empty
    }
  let title=Default.title
  module Env_definition=Default.Make_theorem
    (struct
       let refType="definition"
       let counter="definition"
       let counterLevel=3
       let display num=alternative Bold [tT (Printf.sprintf "Definition %s" num); tT " "]
     end)

  module Env_theorem=struct
    module Th=Default.Make_theorem
      (struct
         let refType="theorem"
         let counter="theorem"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Theorem %s" num); tT " "]
       end)
    include Th
    module Env_proof=Default.Proof
  end
  module Env_proposition=struct
    module Th=Default.Make_theorem
      (struct
         let refType="theorem"
         let counter="theorem"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Proposition %s" num); tT " "]
       end)
    include Th
    module Env_proof=Default.Proof
  end
  module Env_corollary=struct
    module Th=Default.Make_theorem
      (struct
         let refType="theorem"
         let counter="theorem"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Corollary %s" num); tT " "]
       end)
    include Th
    module Env_proof=Default.Proof
  end
  module Env_lemma=struct
    module Th=Default.Make_theorem
      (struct
         let refType="theorem"
         let counter="theorem"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Lemma %s" num); tT " "]
       end)
    include Th
    module Env_proof=Default.Proof
  end
  module Env_openproblem=struct
    module Th=Default.Make_theorem
      (struct
         let refType="openproblem"
         let counter="openproblem"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Open problem %s" num); tT " "]
       end)
    include Th
  end
  module Env_conjecture=struct
    module Th=Default.Make_theorem
      (struct
         let refType="openproblem"
         let counter="openproblem"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Conjecture %s" num); tT " "]
       end)
    include Th
  end
  module Env_algorithm=struct
    module Th=Default.Make_theorem
      (struct
         let refType="algorithm"
         let counter="algorithm"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Algorithm %s" num); tT " "]
       end)
    include Th
  end
  module Env_exercise=struct
    module Th=Default.Make_theorem
      (struct
         let refType="exercise"
         let counter="exercise"
         let counterLevel=3
         let display num=alternative Bold [tT (Printf.sprintf "Exercise %s" num); tT " "]
       end)
    include Th
  end

  open Util


  let utf8Char x=[tT (UTF8.init 1 (fun _->UChar.chr x))]
  let glyph x=
    bB (fun env->
         let code={glyph_utf8=""; glyph_index=x } in
           [GlyphBox { (glyphCache env.font code) with
                         OutputCommon.glyph_color=env.fontColor;
                         OutputCommon.glyph_size=env.size
                     }]
      )
  let q _=utf8Char 8220
  let qq _=utf8Char 8221




  let figure_here ?(parameters=center) ?(name="") ?(caption=[]) ?(scale=1.) drawing=
    let drawing' env=
      let dr_=drawing env in
      let dr=
        if scale<>1. then
          match resize scale (Drawing dr_) with Drawing f->f | _->assert false
        else dr_
      in
      let lvl,num=try StrMap.find "figure" env.counters with _-> -1,[] in
      let _,str_counter=try StrMap.find "_structure" env.counters with Not_found -> -1, [] in
      let sect_num=drop (List.length str_counter - max 0 lvl+1) str_counter in
      let caption=
        Box.drawing (
          draw_boxes env (
            boxify_scoped env (
              [ tT "Figure"; tT " ";
                tT (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev (num@sect_num)))) ]
              @(if caption=[] then [] else tT" "::tT"-"::tT" "::caption)
            )
          )
        )
      in
      let fig=if caption.drawing_nominal_width<=dr.drawing_nominal_width then
        drawing_blit dr
          ((dr.drawing_nominal_width-.caption.drawing_nominal_width)/.2.)
          (dr.drawing_y0-.2.*.caption.drawing_y1) caption
      else
        drawing_blit caption
          ((caption.drawing_nominal_width-.dr.drawing_nominal_width)/.2.)
          (2.*.caption.drawing_y1-.dr.drawing_y0) dr
      in
      { fig with drawing_y1=fig.drawing_y1-.fig.drawing_y0+.env.lead/.2.;
          drawing_y0=(-.env.lead/.2.);
          drawing_contents=(fun x->List.map (OutputCommon.translate 0. (-.fig.drawing_y0)) (fig.drawing_contents x)) }
    in
    let par a b c d e f g line=
      let p=parameters a b c d e f g line in
      { p with
        measure=a.normalMeasure;
        left_margin=a.normalLeftMargin+.(a.normalMeasure-.line.nom_width)/.2.;
        absolute=true;
      }
    in
    (match !D.structure with
         Paragraph _,_->go_up D.structure;
       | x->());
    newPar D.structure ~environment:(fun env->{env with par_indent=[]}) Complete.normal par
      (Env (incr_counter "figure")::bB (fun env->[Drawing (drawing' env)])::label name)



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
      (fun res box -> min res (Box.lower_y box))
      0.
      boxes

  let boxes_y1 boxes =
    List.fold_left
      (fun res box -> max res (Box.upper_y box))
      0.
      boxes

  let equation contents =
    let pars a b c d e f g line={(parameters a b c d e f g line) with
                              min_height_before=
        if line.lineStart=0 then a.lead else 0.;
                              min_height_after=
        if line.lineEnd>=Array.length b.(line.paragraph) then a.lead else 0.
                           }
    in
    newPar ~environment:(fun env -> { env with par_indent = [] })
      D.structure Complete.normal pars
      [ Env (fun env ->Document.incr_counter "equation" env) ;
        C (fun env ->
	     let _,w = boxes_width env contents in
	     let _,x = try StrMap.find "equation" env.counters with _-> -1,[] in
	     let num,w' = boxes_width env
	       (italic [tT "(";
		        tT (string_of_int (1 + List.hd x));
		        tT ")" ]) in
             let w0=(env.normalMeasure -. w)/.2. in
             let w1=env.normalMeasure -. w'-.w0-.w in
             bB(fun _->[glue w0 w0 w0])::
               contents@
               [bB (fun _->glue w1 w1 w1 :: num)]
	  )];
    []

end
