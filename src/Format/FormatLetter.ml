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
open Fonts
open FTypes
open Typography.Document
open Util
open UsualMake
open Typography.Box
open Printf

let id x=x
let emph x=toggleItalic x

module Format=functor (D:DocumentStructure)->struct

  module Default=DefaultFormat.Format(D)
  include (Default:module type of Default with module Output:=Default.Output)

  let subject_text: content list ref = ref (toggleItalic [tT "Subject:";tT" "])

  let rec lines s=try
    let idx=String.index s '\n' in
    unspace (String.sub s 0 idx)::lines (String.sub s (idx+1) (String.length s-idx-1))
  with
      Not_found->if s="" then [] else [s]
  let rec repeat x n=if n<=0 then [] else x::(repeat x (n-1))

  let node=DefaultFormat.node
  let paragraph=DefaultFormat.paragraph
  let postprocess_tree tree=
    fst (
      top (
        newChildBefore (tree,[])
          (fst
             (DefaultFormat.paragraph
                [ bB (fun env->
                       let w=env.normalMeasure/.2. in
                       let sender=match tree with
                           Node n->(try List.assoc "sender" n.node_tags with Not_found->"")
                         | _ -> ""
                       in
                       let recipient=match tree with
                           Node n->(try List.assoc "receiver" n.node_tags with Not_found->"")
                         | _ -> ""
                       in
                       let lines_recipient=lines recipient in
                       let lines_sender=
                         let l=lines sender in
                           repeat " " (List.length lines_recipient-List.length l)@l
                       in
                       let pars_sender=node (List.map (fun l->paragraph [tT l]) (lines_sender)) in
                       let pars_recip=node (List.map (fun l->paragraph [tT l]) (lines_recipient)) in
                       let minip_sender=try
                                          snd (IntMap.min_binding
                                                 (let d,_,_ = OutputDrawing.minipage' { env with
                                                   normalMeasure=w;
                                                   par_indent=[]} pars_sender in d)
                                          )
                         with Not_found->empty_drawing_box
                       in
                       let minip_recip=try
                                         snd (IntMap.min_binding
                                                (let d,_,_ = OutputDrawing.minipage' { env with normalMeasure=w;
                                                  par_indent=[] } pars_recip in d)
                                         )
                         with Not_found->empty_drawing_box
                       in
                       let x0=min 0. (env.normalMeasure/.2.-.minip_sender.drawing_nominal_width) in
                       let x1=max (env.normalMeasure/.2.)
                         (env.normalMeasure-.minip_recip.drawing_nominal_width) in
                       let y0=round_float ((minip_recip.drawing_y0-.minip_sender.drawing_y0)
                                           /.env.normalLead)*.env.normalLead in
                       let y1=0. in
                       let c0=(minip_sender.drawing_contents minip_sender.drawing_nominal_width) in
                       let c1=(minip_recip.drawing_contents minip_recip.drawing_nominal_width) in
                       let contents=
                         (List.map (Raw.translate x0 y0) c0)@
                           (List.map (Raw.translate x1 y1) c1)
                       in
                         [Drawing {
                            drawing_min_width= env.normalMeasure;
                            drawing_max_width= env.normalMeasure;
                            drawing_nominal_width= env.normalMeasure;
			    drawing_width_fixed = true;
			    drawing_adjust_before = false;
                            drawing_y0= min (minip_sender.drawing_y0+.y0) (minip_recip.drawing_y0+.y1);
                            drawing_y1= max (minip_sender.drawing_y1+.y0) (minip_recip.drawing_y1+.y1);
                            drawing_badness=(fun _->0.);
                            drawing_break_badness=0.;
                            drawing_states=[];
                            drawing_contents=(fun _->contents)
                          }]
                    )]
             ))))

  let sender x=
    D.structure:=follow
      (tag (fst (top !D.structure)) ["sender",x], [])
      (List.map fst (snd !D.structure))

  let receiver x=
    D.structure:=follow
      (tag (fst (top !D.structure)) ["receiver",x], [])
      (List.map fst (snd !D.structure))

  open Unix
  let lieu_date_text  = ref (fun lieu x ->
    (Printf.sprintf "%s, le %d/%d/%d" lieu x.tm_mday (x.tm_mon + 1) (1900 + x.tm_year)))


  let lieu_date lieu x =
      newPar D.structure Complete.normal ragged_right
        ((vspaceBefore 10.) @ [tT (!lieu_date_text lieu x)])
  let dear x=(vspaceBefore 13.)@(vspaceAfter 3.)@x
  let subject x= !subject_text @ x


  module Output (M:Driver.OutputDriver)=struct
    module O=Default.Output(M)
    type output=O.output
    let outputParams=O.outputParams
    let output out_params structure defaultEnv file=
      O.output out_params (postprocess_tree structure) defaultEnv file
  end

end
