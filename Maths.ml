open Typography
open Drivers
open Util
open Constants
open Fonts.FTypes

type style=
    Display
  | Display'
  | Text
  | Text'
  | Script
  | Script'
  | ScriptScript
  | ScriptScript'

let int_of_style=function
    Display->0
  | Display'->1
  | Text->2
  | Text'->3
  | Script->4
  | Script'->5
  | ScriptScript->6
  | ScriptScript'->7

type mathsEnvironment={
  fonts:(Fonts.font * float) array;
  numerator_spacing:float;
  denominator_spacing:float;
  sub1:float;
  sub2:float;
  sup1:float;
  sup2:float;
  sup3:float;
  sub_drop:float;
  sup_drop:float;
  default_rule_thickness:float;
  subscript_distance:float;
  superscript_distance:float
}

let default=
  let font=Fonts.loadFont "euler.otf" in
    {
      fonts=[|font,1.;
              font,1.;
              font,1.;
              font,1.;
              font,0.35;
              font,0.35;
              font,0.2;
              font,0.2;
            |];
      numerator_spacing=0.02;
      denominator_spacing=0.02;
      sub1= 0.2;
      sub2= 0.1;
      sup1=0.35;
      sup2=0.2;
      sup3=0.3;
      sub_drop=0.;
      sup_drop=1.;
      default_rule_thickness=0.05;
      subscript_distance= 0.12;
      superscript_distance= 0.12
    }

type noad= { nucleus:box list;
             subscript_left:math list; superscript_left:math list;
             subscript_right:math list; superscript_right:math list }

and binary= { bin_priority:int; bin_drawing:noad; bin_left:math list; bin_right:math list }
and fraction= { numerator:math list; denominator:math list; line:Drivers.path_parameters }
and operator= { op_priority:int; op_noad:noad; op_contents:math list }
and math=
    Ordinary of noad
  | Binary of binary
  | Fraction of fraction
  | Relation of binary
  | Operator of operator

let noad n={ nucleus=n; subscript_left=[]; superscript_left=[]; subscript_right=[]; superscript_right=[] }



let symbol font size c=
  {
    glyph_x=0.;glyph_y=0.; glyph_size=size; glyph_color=black;
    glyph=Fonts.loadGlyph font { empty_glyph with glyph_index=Fonts.glyph_of_char font c}
  }


let cramp=function
    Display->Display'
  | Display'->Display'
  | Text->Text'
  | Text'->Text'
  | Script->Script'
  | Script'->Script'
  | ScriptScript-> ScriptScript'
  | ScriptScript'-> ScriptScript'

let is_cramped=function
    Display'
  | Text'
  | Script'
  | ScriptScript'-> true
  | _->false

let nonscript=function Display | Display' | Text | Text' -> true | _->false

let scriptStyle=function
    Display
  | Text->Script
  | Display'
  | Text'->Script
  | Script
  | ScriptScript-> ScriptScript
  | Script'
  | ScriptScript'-> ScriptScript'

let superStyle x=scriptStyle x
let subStyle x=cramp (scriptStyle x)

let rec last=function
    []->raise Not_found
  | [h]->h
  | _::s->last s

let rec bezier_of_boxes=function
    []->[]
  | Glyph g::s->
      let out=Fonts.outlines g.glyph in
        (List.map (fun (x,y)->Array.map (fun xx->g.glyph_x+.xx *. g.glyph_size/.1000.) x,
                     Array.map (fun xx->g.glyph_y+.xx *. g.glyph_size/.1000.) y)
           (List.concat out)) @ (bezier_of_boxes s)
  | Path (_,p)::s->
      (List.concat (List.map Array.to_list p))@(bezier_of_boxes s)
  | _::s->bezier_of_boxes s

let rec cut l n=
  if n=0 then [],l else
    match l with
        []->[],[]
      | h::s->let a,b=cut s (n-1) in h::a,s

let dist xa ya xb yb=sqrt ((xa-.xb)*.(xa-.xb) +. (ya-.yb)*.(ya-.yb))

let min_dist d (xa,ya) (xb,yb)=
  let d'=ref infinity in
    for i=0 to Array.length xa-1 do
      for j=0 to Array.length xb-1 do
        d':=min !d' (dist xa.(i) ya.(i) xb.(j) yb.(j))
      done
    done;
    if !d'<=d then min d (Bezier.distance (xa,ya) (xb,yb)) else d

let line (a,b) (c,d)=[|a;c|], [|b;d|]

let rec draw_maths mathsEnv style mlist0=
  let noSpace=[] in
  let thinSpace = [glue (1./.6.) (1./.6.) (1./.6.)] in
  let medSpace =  [glue 0. (2./.9.) (1./.3.)] in
  let thickSpace = [glue (5./.18.) (5./.18.) (5./.9.)] in
  let font,size = mathsEnv.fonts.(int_of_style style) in

  let rec draw=function
      []->[]
    | Ordinary n::s->(
        let (w0,w1,w2)=List.fold_left (fun (x0,x1,x2) b->let a,b,c=box_interval b in (x0+.a, x1+.b,x2+.c))
          (0.,0.,0.) n.nucleus
        in
          if n.superscript_right<>[] ||
            n.superscript_left<>[] ||
            n.subscript_right<>[] ||
            n.subscript_left<>[] then (

              let l1=bezier_of_boxes (draw_boxes n.nucleus) in
              let x0,y0,x1,y1=bounding_box [Path (Drivers.default,[Array.of_list l1])] in


              let x_height=
                let x=Fonts.loadGlyph font ({empty_glyph with glyph_index=Fonts.glyph_of_char font 'x'}) in
                  (Fonts.glyph_y1 x)/.1000.
              in

              let u,v=match last n.nucleus with
                  GlyphBox _ | Kerning _ -> 0., 0.
                | _-> y1 -. mathsEnv.sup_drop, -.y0-. mathsEnv.sub_drop
              in

              let a=draw_boxes (draw_maths mathsEnv (superStyle style) n.superscript_right) in
              let b=draw_boxes (draw_maths mathsEnv (superStyle style) n.superscript_left) in
              let c=draw_boxes (draw_maths mathsEnv (subStyle style) n.subscript_right) in
              let d=draw_boxes (draw_maths mathsEnv (subStyle style) n.subscript_left) in
              let bezier=Array.map bezier_of_boxes [| a;b;c;d |] in
              let bb=Array.map (fun l->bounding_box [Path (Drivers.default, [Array.of_list l])]) bezier in
              let xoff=Array.make 4 0. in


              let y_place sup sub=
                let xa0,ya0,xa1,ya1=bb.(sub) in
                let xb0,yb0,xb1,yb1=bb.(sup) in

                  if bezier.(sup)=[] then (
                    0., max v (max mathsEnv.sub1 (ya1 -. 4./.5. *. abs_float x_height))
                  ) else (
                    let p=
                      if style=Display then mathsEnv.sup1 else
                        if is_cramped style then mathsEnv.sup3 else mathsEnv.sup2
                    in
                    let u=max u (max p (-.yb0 +. (abs_float x_height)/.4.)) in
                      if bezier.(sub)=[] then (
                        u,0.
                      ) else (
                        let v=max v mathsEnv.sub2 in
                          if (u+.yb0) -. (ya1-.v) > 4.*.mathsEnv.default_rule_thickness then (
                            u,v
                          ) else (
                            let v=4.*.mathsEnv.default_rule_thickness -. (u+.yb0) +. ya1 in
                            let psi=4./.5.*.abs_float x_height -. (u+.yb0) in
                              if psi > 0. then (u+.psi, v-.psi) else (u,v)
                          )
                      )
                  )
              in


              let off_ya,off_yc = y_place 0 2 in
              let off_yb,off_yd = y_place 1 3 in

              let xoff=
                let xa0,_,xa1,_=bb.(0) in
                let xb0,_,xb1,_=bb.(1) in
                let xc0,_,xc1,_=bb.(2) in
                let xd0,_,xd1,_=bb.(3) in
                  [| x1-.xa0; x0 -.xb1; x1-.xc0; x0-.xd1 |]
              in
              let yoff=[| off_ya;off_yb; -.off_yc; -.off_yd |] in


              let dist=Array.mapi (fun i l->
                                     let ll = List.map (fun (x,y)->Array.map (fun x0->x0 +. xoff.(i)) x,
                                                          Array.map (fun x0->x0 +. yoff.(i)) y) l
                                     in
                                       List.fold_left (fun a b->List.fold_left (fun c d->min_dist c b d) a ll) infinity l1
                                  ) bezier
              in
              let dr=(draw_boxes n.nucleus)
                @ (List.map (translate (xoff.(0)-.dist.(0)+.mathsEnv.superscript_distance) yoff.(0)) a)
                @ (List.map (translate (xoff.(1)+.dist.(1)+.mathsEnv.superscript_distance) yoff.(1)) b)
                @ (List.map (translate (xoff.(2)-.dist.(2)+.mathsEnv.subscript_distance) yoff.(2)) c)
                @ (List.map (translate (xoff.(3)+.dist.(3)+.mathsEnv.subscript_distance) yoff.(3)) d)
              in
              let (a0,a1,a2,a3) = bounding_box dr in
                [ Drawing ({ drawing_min_width=a2-.a0;
                             drawing_nominal_width=a2-.a0;
                             drawing_max_width=a2-.a0;
                             drawing_y0=a1;
                             drawing_y1=a3;
                             drawing_badness=(fun _->0.);
                             drawing_contents=(fun _->dr) }) ]
            ) else
              n.nucleus
      )@(draw s)

    | Binary b::s->(
        let left=try (
          match last b.bin_left with
              Ordinary _
            | Fraction _->medSpace
            | Relation _->[]
            | Binary _->[]                (* à vérifier dans le TeXbook *)
        ) with _->noSpace
        in
        let right=
          match List.hd b.bin_right with
              Ordinary _
            | Fraction _->medSpace
            | Relation _->[]
            | Binary _->[]
        in

          (draw_maths mathsEnv style b.bin_left)@left @ (draw_maths mathsEnv style [Ordinary b.bin_drawing]) @ right@(draw_maths mathsEnv style b.bin_right)

      )@(draw s)

    | Relation b::s->(
        let left=try (
          match last b.bin_left with
              Ordinary _
            | Fraction _->thickSpace
            | Relation _->[]
            | Binary _->[]                (* à vérifier dans le TeXbook *)
        ) with _->noSpace
        in
        let right=try
          match List.hd b.bin_right with
              Ordinary _
            | Fraction _->thickSpace
            | Relation _->[]
            | Binary _->[]
        with _->noSpace
        in

          (draw_maths mathsEnv style b.bin_left)@left @ (draw_maths mathsEnv style [Ordinary b.bin_drawing]) @ right@(draw_maths mathsEnv style b.bin_right)

      )@(draw s)
    | (Fraction f)::s->(

        let ba=draw_boxes (draw_maths mathsEnv (match style with
                                               Display -> Text | Display' -> Text'
                                             | _ -> superStyle style)
                             f.numerator) in
        let bb=draw_boxes (draw_maths mathsEnv (match style with
                                               Display -> Text | Display' -> Text'
                                             | _ -> superStyle style)
                             f.denominator) in
        let x0a,y0a,x1a,y1a=bounding_box ba in
        let x0b,y0b,x1b,y1b=bounding_box bb in
        let wa=x1a-.x0a in
        let wb=x1b-.x0b in
        let w=max wa wb in
          [ Drawing (drawing ~offset:(-.y0b) (

                       (if f.line.lineWidth = 0. then [] else [Path (f.line, [ [|line (0.,0.) (w,0.)|] ]) ])@
                         (List.map (translate ((w-.wa)/.2.) (-.y0a+.1.)) ba)@
                         (List.map (translate ((w-.wb)/.2.) (-.y1b-.1.)) bb)

                     ))]

      )@(draw s)

  in
  let result=draw mlist0 in
  let _,size=mathsEnv.fonts.(int_of_style style) in
      List.map (resize size) result

let gl env st c=
  let font,s=(env.fonts.(int_of_style st)) in
  GlyphBox { (glyphCache font { empty_glyph with glyph_index=Fonts.glyph_of_char font c})
             with glyph_size=1.; glyph_x=0.; glyph_y=0. }

let a =
  let env=default in
  let style=Display in
    Ordinary  { (noad [gl env style 'a']) with
                  superscript_right=[Ordinary (noad [gl env style '0'])];
                  superscript_left=[];
                  subscript_right=[Ordinary (noad [gl env style '0'])];
                  subscript_left=[] }
  (* Ordinary  (noad [gl 'a']) *)


let bx= (
  Binary { bin_priority=0; bin_drawing=(noad [gl default Display '+']);
           bin_left=[a]; bin_right=[a] }
)

(* let f=draw_boxes ( *)
(*   draw_maths defaultEnv Display (\* [Ordinary (noad [gl '+'])] *\) *)
(*     [Fraction { numerator=[a]; denominator=[a]; line={Drivers.default with lineWidth = 0.5 }}] *)
(* ) *)

let u=
  (* let (a,b,c,d)=bounding_box bx in *)
    [| { pageFormat=(100.,100.); pageContents=
           List.map (translate 20. 20.) (
             List.map (Drivers.resize 10.) (
               (* (Path ({Drivers.default with lineWidth=0.1}, rectangle (a,b) (c,d))):: *)
               draw_boxes (draw_maths default Display [a])
             )
           ) }
    |]

let _=
  Drivers.Pdf.output u "maths.pdf"
