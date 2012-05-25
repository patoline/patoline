open Typography
open Typography.Fonts
open Typography.Fonts.FTypes
open Typography.Document
open Typography.Util
open CamomileLibrary
open Typography.Box
open Typography.Line
open Printf

let id x=x
let emph x=toggleItalic x

module Format=functor (D:DocumentStructure)->struct

  module Default=DefaultFormat.Format(D)
  include Default

  let subject_text: user content list ref = ref (toggleItalic [T "Subject:";T" "]) 

  let rec lines s=try
    let idx=String.index s '\n' in
      String.sub s 0 idx::lines (String.sub s (idx+1) (String.length s-idx-1))
  with
      Not_found->if s="" then [] else [s]
  let rec repeat x n=if n<=0 then [] else x::(repeat x (n-1))

  let postprocess_tree tree=
    fst (
      top (
        newChildBefore (tree,[])
          (fst
             (paragraph
                [ B (fun env->
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
                       let pars_sender=node (List.map (fun l->paragraph [T l]) (lines_sender)) in
                       let pars_recip=node (List.map (fun l->paragraph [T l]) (lines_recipient)) in
                       let minip_sender=(Default.minipage { env with
                                                              normalMeasure=w;
                                                              par_indent=[]} pars_sender).(0) in
                       let minip_recip=(Default.minipage { env with normalMeasure=w;
                                                             par_indent=[] } pars_recip).(0) in
                       let height=max
                         (minip_sender.drawing_y1-.minip_sender.drawing_y0)
                         (minip_recip.drawing_y1-.minip_recip.drawing_y0)
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
                         (List.map (OutputCommon.translate x0 y0) c0)@
                           (List.map (OutputCommon.translate x1 y1) c1)
                       in
                         [Drawing {
                            drawing_min_width= env.normalMeasure;
                            drawing_max_width= env.normalMeasure;
                            drawing_nominal_width= env.normalMeasure;
                            drawing_y0= min (minip_sender.drawing_y0+.y0) (minip_recip.drawing_y0+.y1);
                            drawing_y1= max (minip_sender.drawing_y1+.y0) (minip_recip.drawing_y1+.y1);
                            drawing_badness=(fun _->0.);
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
        ((vspaceBefore 10.) @ [T (!lieu_date_text lieu x)])
  let dear x=(vspaceBefore 13.)@(vspaceAfter 3.)@x
  let subject x= !subject_text @ x

end

module Output=DefaultFormat.Output
