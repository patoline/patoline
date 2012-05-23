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
  let lang_T x=[T x]

  let node l=Document.Node {Document.empty with Document.children=List.fold_left (fun m (l,_)->Util.IntMap.add (Util.IntMap.cardinal m) l m) Util.IntMap.empty l}, []

  let paragraph cont=
    (Paragraph {par_contents=cont; par_env=(fun x->x);
                par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters; user_positions=env2.user_positions });
                par_parameters=parameters; par_completeLine=Typography.Complete.normal }, [])


  type 'a tableParams={ widths:'a environment->float array; h_spacing:float; v_spacing:float }

  let table params tab=
    [ B (fun env->
           let widths0=params.widths env in
           let widths=Array.make (Array.length widths0) 0. in
           let heights=Array.make (Array.length tab) 0. in
           let tab_formatted=Array.mapi
             (fun i x->
                Array.mapi (fun j y->
                              let minip=(Default.minipage { env with normalMeasure=widths0.(j) } y).(0) in
                                widths.(j)<-max widths.(j) (minip.drawing_max_width);
                                heights.(i)<-max heights.(i) (minip.drawing_y1-.minip.drawing_y0);
                                minip
                           ) x
             )
             tab
           in
           let contents=ref [] in
           let x=ref 0. in
           let y=ref 0. in
           let y'=ref 0. in
           let max_y=ref (-.infinity) in
           let min_y=ref infinity in
           let ymin=ref 0. in
           let ymax=ref 0. in
             for i=0 to Array.length tab_formatted-1 do
               x:=0.;
               ymin:=0.;
               ymax:= -.infinity;
               let conts=ref [] in
                 for j=0 to Array.length tab_formatted.(i)-1 do
                   let cont=tab_formatted.(i).(j) in
                     conts:=(List.map (OutputCommon.translate !x 0.)
                               (cont.drawing_contents (widths.(j)))) @ (!conts);
                     ymin := min !ymin cont.drawing_y0;
                     ymax := max !ymax cont.drawing_y1;
                     x:= !x +. widths.(j) +. params.h_spacing
                 done;
                 contents:=(List.map (OutputCommon.translate 0. !y) !conts)@(!contents);
                 max_y:=max !max_y (!y+. !ymax);
                 min_y:=min !min_y (!y+. !ymin);
                 y:=(!y)-. !ymax +. !ymin;
             done;

             [Drawing {
                drawing_min_width= !x;
                drawing_max_width= !x;
                drawing_nominal_width= !x;
                drawing_y0= !min_y;
                drawing_y1= !max_y;
                drawing_badness=(fun _->0.);
                drawing_contents=(fun _-> List.map (OutputCommon.translate 0. 0.) !contents)
              }]
        )]

  let replace_utf8 x y z=
    Str.global_replace x
      (UTF8.init 1 (fun _->UChar.chr y)) z
  let defaultEnv=
    let size=3.8 in
    let env=Default.defaultEnv in
      {  env with
           size=size;
           mathsEnvironment=
          Array.map (fun x->{x with Mathematical.kerning=false })
            env.mathsEnvironment;
           word_substitutions=
          (fun x->List.fold_left (fun y f->f y) x
             [
               replace_utf8 (Str.regexp_string "``") 8220;
               replace_utf8 (Str.regexp_string "''") 8221
             ]
          );
           counters=List.fold_left (fun m (a,b)->StrMap.add a b m) StrMap.empty
          ["_structure",(-1,[0]);
           "_figure",(-1,[0]);
           "figure",(2,[0])];
    }
  let title=Default.title
  let author=Default.author
  let institute=Default.institute

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
  let date x=
    let date=Printf.sprintf "%d/%d/%d" x.tm_mday x.tm_mon x.tm_year in
      newPar D.structure Complete.normal ragged_right
        ((vspaceBefore 10.) @ [T date])
  let dear x=(vspaceBefore 13.)@(vspaceAfter 3.)@x
  let subject x=(toggleItalic [T"Subject:";T" "]) @ x


  let utf8Char x=[T (UTF8.init 1 (fun _->UChar.chr x))]
  let glyph x=
    B (fun env->
         let code={glyph_utf8=""; glyph_index=x } in
           [GlyphBox { (glyphCache env.font code) with
                         OutputCommon.glyph_color=env.fontColor;
                         OutputCommon.glyph_size=env.size
                     }]
      )
end

module Output=DefaultFormat.Output
