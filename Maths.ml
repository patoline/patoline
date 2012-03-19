open CamomileLibrary
open Binary
open Typography
open OutputCommon
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
  mathsFont:Fonts.font;
  mathsSize:float;
  mathsSubst:glyph_id list -> glyph_id list;
  mathsSymbols:int Binary.StrMap.t;
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
  superscript_distance:float;
  limit_subscript_distance:float;
  limit_superscript_distance:float;
  open_dist:float;
  close_dist:float;
  kerning:bool
}

let default_env=
    {
      mathsFont=Fonts.loadFont "Otf/euler.otf";
      mathsSubst=(fun x->x);
      mathsSize=1.;
      mathsSymbols=List.fold_left (fun m (a,b)->Binary.StrMap.add a b m) Binary.StrMap.empty [
        "leftarrow", 232;
        "uparrow", 233;
        "rightarrow", 234;
        "downarrow", 235;
        "leftrightarrow",236;
        "updownarrow",237;
        "forall", 263;
        "exists", 265;
        "notexists", 266;
        "int",783
      ];
      numerator_spacing=0.08;
      denominator_spacing=0.08;
      sub1= 0.1;
      sub2= 0.1;
      sup1=0.5;
      sup2=0.5;
      sup3=0.5;
      sub_drop=0.1;
      sup_drop=0.2;
      default_rule_thickness=0.05;
      subscript_distance= 0.15;
      superscript_distance= 0.1;
      limit_subscript_distance= 0.12;
      limit_superscript_distance= 0.12;
      open_dist=0.2;
      close_dist=0.2;
      kerning=true
    }

let default=[|
  default_env;
  default_env;
  default_env;
  default_env;
  { default_env with mathsSize=2./.3. };
  { default_env with mathsSize=2./.3. };
  { default_env with mathsSize=4./.9. };
  { default_env with mathsSize=4./.9. }
|]

type 'a noad= { mutable nucleus: mathsEnvironment -> style ->  'a box list;
                mutable subscript_left:'a math list; mutable superscript_left:'a math list;
                mutable subscript_right:'a math list; mutable superscript_right:'a math list }

and 'a binary_type =
    Invisible
  | Normal of bool * 'a noad * bool (* the boolean remove spacing at left or right when true *)

and 'a binary= { bin_priority:int; bin_drawing:'a binary_type; bin_left:'a math list; bin_right:'a math list }
and 'a fraction= { numerator:'a math list; denominator:'a math list; line:OutputCommon.path_parameters }
and 'a operator= { op_noad:'a noad; op_limits:bool; op_left_spacing:float; op_right_spacing:float; op_left_contents:'a math list; op_right_contents:'a math list }
and 'a math=
    Ordinary of 'a noad
  | Binary of 'a binary
  | Fraction of 'a fraction
  | Operator of 'a operator
  | Decoration of (mathsEnvironment -> style -> 'a box list -> 'a box list)*('a math list)


let noad n={ nucleus=n; subscript_left=[]; superscript_left=[]; subscript_right=[]; superscript_right=[] }



(* let symbol font size c= *)
(*   { *)
(*     glyph_x=0.;glyph_y=0.; glyph_size=size; glyph_color=black; *)
(*     glyph=Fonts.loadGlyph font { empty_glyph with glyph_index=Fonts.glyph_of_char font c} *)
(*   } *)


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

let rec contents=function
    GlyphBox x->(Fonts.glyphNumber x.glyph).glyph_utf8
  | Kerning x->contents x.kern_contents
  | _->""


let rec draw_maths mathsEnv style mlist=
  let env=mathsEnv.(int_of_style style) in

    match mlist with

        []->[]
      | Ordinary n::s->(
          (* attacher les indices et les exposants n'est pas
             complètement trivial. on utilise la règle de Knuth pour
             la position verticale, puis un calcul de distance pour la
             position horizontale. D'ailleurs, il est assez faux de
             déplacer les indices de la distance qu'on calcule, mais
             c'est rapide et ça marche bien dans la plupart des
             cas. *)


	    let nucleus = n.nucleus env style in
            if n.superscript_right<>[] ||
              n.superscript_left<>[] ||
              n.subscript_right<>[] ||
              n.subscript_left<>[] then (

                let l1=bezier_of_boxes (draw_boxes nucleus) in
                let x0,y0,x1,y1=bounding_box [Path (OutputCommon.default,[Array.of_list l1])] in


                let x_height=
                  let x=Fonts.loadGlyph env.mathsFont ({empty_glyph with glyph_index=Fonts.glyph_of_char env.mathsFont 'x'}) in
                    (Fonts.glyph_y1 x)/.1000.
                in
                let u,v=if contents (last nucleus) <> "" then  0., 0. else
                  y1 -. env.sup_drop*.env.mathsSize, -.y0+. env.sub_drop*.env.mathsSize
                in

                let a=draw_boxes (draw_maths mathsEnv (superStyle style) n.superscript_right) in
                let b=draw_boxes (draw_maths mathsEnv (superStyle style) n.superscript_left) in
                let c=draw_boxes (draw_maths mathsEnv (subStyle style) n.subscript_right) in
                let d=draw_boxes (draw_maths mathsEnv (subStyle style) n.subscript_left) in
                let bezier=Array.map bezier_of_boxes [| a;b;c;d |] in
                let bb=Array.map (fun l->bounding_box [Path (OutputCommon.default, [Array.of_list l])]) bezier in

                let y_place sup sub=
                  let xa0,ya0,xa1,ya1=bb.(sub) in
                  let xb0,yb0,xb1,yb1=bb.(sup) in
                    if bezier.(sup)=[] then (
                      0., max v (max (env.sub1*.env.mathsSize) (ya1 -. 4./.5. *. abs_float x_height*.env.mathsSize))
                    ) else (
                      let p=
                        if style=Display then env.sup1 else
                          if is_cramped style then env.sup3 else env.sup2
                      in
                      let u=max u (max (p*.env.mathsSize) (-.yb0 +. (abs_float x_height*.env.mathsSize)/.4.)) in
                        if bezier.(sub)=[] then (
                          u,0.
                        ) else (
                          let v=max v (env.sub2*.env.mathsSize) in
                            if (u+.yb0) -. (ya1-.v) > 4.*.env.default_rule_thickness then (
                              u,v
                            ) else (
                              let v=4.*.env.default_rule_thickness*.env.mathsSize -. (u+.yb0) +. ya1 in
                              let psi=4./.5.*.abs_float x_height*.env.mathsSize -. (u+.yb0) in
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
                                       if env.kerning then
                                         let ll = List.map (fun (x,y)->Array.map (fun x0->x0 +. xoff.(i)) x,
                                                              Array.map (fun x0->x0 +. yoff.(i)) y) l
                                         in
                                           List.fold_left (fun a b->List.fold_left (fun c d->min_dist c b d) a ll) infinity l1
                                       else
                                         0.
                                    ) bezier
                in
                let dr=(draw_boxes nucleus)
                  @ (List.map (translate (xoff.(0)-.dist.(0)+.env.mathsSize*.env.superscript_distance) yoff.(0)) a)
                  @ (List.map (translate (xoff.(1)+.dist.(1)-.env.mathsSize*.env.superscript_distance) yoff.(1)) b)
                  @ (List.map (translate (xoff.(2)-.dist.(2)+.env.mathsSize*.env.subscript_distance) yoff.(2)) c)
                  @ (List.map (translate (xoff.(3)+.dist.(3)-.env.mathsSize*.env.subscript_distance) yoff.(3)) d)
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
                nucleus
        )@(draw_maths mathsEnv style s)

      | Binary b::s->(
          let gl=if b.bin_priority=0 then glue (1./.6.) (1./.6.) (1./.6.) else
            if b.bin_priority=1 then glue 0. (2./.9.) (1./.3.) else
              glue (5./.18.) (5./.18.) (5./.9.)
          in
	  match b.bin_drawing with
	    Invisible ->
            (draw_maths mathsEnv style b.bin_left)@
              (resize env.mathsSize gl)::
              (draw_maths mathsEnv style b.bin_right)
	  | Normal(no_sp_left, op, no_sp_right) ->
            (draw_maths mathsEnv style b.bin_left)@
              (if no_sp_left then [] else [resize env.mathsSize gl])@
              (draw_maths mathsEnv style [Ordinary op])@
              (if no_sp_right then [] else [resize env.mathsSize gl])@
              (draw_maths mathsEnv style b.bin_right)
        )@(draw_maths mathsEnv style s)

      | (Fraction f)::s->(

        let hx =
          let x=Fonts.loadGlyph env.mathsFont
	    ({empty_glyph with glyph_index=Fonts.glyph_of_char env.mathsFont 'x'}) in
          (Fonts.glyph_y1 x)/.2000.
        in

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
            Drawing ({ drawing_min_width=w;
                       drawing_nominal_width=w;
                       drawing_max_width=w;
                       drawing_y0=y0b-.y1b-.env.mathsSize*.(env.denominator_spacing+.f.line.lineWidth/.2.);
                       drawing_y1=y1a-.y0a+.env.mathsSize*.(env.numerator_spacing+.f.line.lineWidth/.2.);
                       drawing_badness=(fun _->0.);
                       drawing_contents=(fun _->
                                           (if f.line.lineWidth = 0. then [] else
                                              [Path ({f.line with lineWidth=f.line.lineWidth*.env.mathsSize},
                                                     [ [|line (0.,hx) (w,hx)|] ]) ])@
                                             (List.map (translate ((w-.wa)/.2.) (hx +. -.y0a+.env.mathsSize*.(env.numerator_spacing+.f.line.lineWidth/.2.))) ba)@
                                             (List.map (translate ((w-.wb)/.2.) (hx +. -.y1b-.env.mathsSize*.(env.denominator_spacing+.f.line.lineWidth/.2.))) bb)
                                        ) }) :: (draw_maths mathsEnv style s)
        )
      | Operator op::s ->(

          (* Ici, si op.op_limits est faux, c'est facile : c'est comme
             pour Ordinary. Sinon, il faut calculer la densité de
             l'encre en haut et en bas du symbole pour centrer les
             indices et exposants. Si on passe op.op_limits=true et
             qu'il y a des indices des deux côtés, ils ne sont
             positionnés en-dessous que du côté où c'est possible *)

          let op_noad=
            if op.op_limits then (
              let op', sup=
                if op.op_noad.superscript_right=[] || op.op_noad.superscript_left=[] then
                  ({ op.op_noad with superscript_right=[]; superscript_left=[] },
                   op.op_noad.superscript_right @ op.op_noad.superscript_left)
                else
                  op.op_noad, []
              in
              let op'', sub=
                if op.op_noad.subscript_right=[] || op.op_noad.subscript_left=[] then
                  ({ op' with subscript_right=[]; subscript_left=[] },
                   op.op_noad.subscript_right @ op.op_noad.subscript_left)
                else
                  op', []
              in
              let drawn_op=draw_boxes (draw_maths mathsEnv style [Ordinary op'']) in

              let ba=draw_boxes (draw_maths mathsEnv (superStyle style) sup) in
              let bb=draw_boxes (draw_maths mathsEnv (subStyle style) sub) in
              let x0,y0,x1,y1=bounding_box drawn_op in
              let x0a,y0a,x1a,y1a=bounding_box ba in
              let x0b,y0b,x1b,y1b=bounding_box bb in

              let collide y=
                let line=[|x0;x1|], [|y;y|] in
                let rec make_ints area x=function
                    (h1,_)::(h2,_)::s->
                      make_ints
                        (area +. (h2-.h1)*.(x1-.x0))
                        (x +. (x0+.(h2+.h1)*.(x1-.x0)/.2.) *. (h2-.h1)*.(x1-.x0))
                        s
                  | _ ->x,area
                in
                  make_ints 0. 0. (
                    List.sort compare (List.concat (List.map (Bezier.intersect line) (bezier_of_boxes drawn_op)))
                  )
              in
              let n=5 in
              let rec center i y weight x=
                if i>n then
                  if weight=0. then
                    (x0+.x1)/.2.          (* solution naive s'il n'y a aucune collision *)
                  else
                    x/.weight
                else
                  let x',w=collide (y+.(float_of_int i)*.(y1-.y0)/.(10.*.float_of_int n)) in
                    center (i+1) y (weight+.w) (x +. x')
              in
              let xsup=center 1 (y1-.(y1-.y0)/.10.) 0. 0. in
              let xsub=center 1 y0 0. 0. in

              let yoff=
                y0-.y1a-.env.limit_subscript_distance*.env.mathsSize -. y0a
              in
                [ Drawing (drawing ~offset:(yoff) (

                             drawn_op @
                               (List.map (translate (xsup-.(x1a+.x0a)/.2.) (y1-.y0a+.env.limit_superscript_distance*.env.mathsSize)) ba)@
                               (List.map (translate (xsub-.(x1b+.x0b)/.2.) (y0-.y1a-.env.limit_subscript_distance*.env.mathsSize)) bb)

                           ))]

            ) else draw_maths mathsEnv style [Ordinary op.op_noad]
          in
          let left=draw_boxes (draw_maths mathsEnv style op.op_left_contents) in
          let bezier_left=bezier_of_boxes left in
          let right=draw_boxes (draw_maths mathsEnv style op.op_right_contents) in
          let bezier_right=bezier_of_boxes right in
          let bezier_op=bezier_of_boxes (draw_boxes op_noad) in
          let (x0_r,y0_r,x1_r,y1_r)=bounding_box right in
          let (x0_l,y0_l,x1_l,y1_l)=bounding_box left in
          let (x0_op,y0_op,x1_op,y1_op)=bounding_box (draw_boxes op_noad) in

          let lr = List.map (fun (x,y)->Array.map (fun x0->x0 +. x1_op-.x0_r) x, y) bezier_right in
          let dist_r=if env.kerning then
            List.fold_left (fun a b->List.fold_left (fun c d->min_dist c b d) a lr) infinity bezier_op
          else 0.
          in

          let ll = List.map (fun (x,y)->Array.map (fun x0->x0 -. x1_l+.x0_op) x, y) bezier_left in
          let dist_l=if env.kerning then
            List.fold_left (fun a b->List.fold_left (fun c d->min_dist c b d) a ll) infinity bezier_op
          else 0.
          in

          let x_op=if left=[] then 0. else (x1_l-.dist_l+.op.op_left_spacing) in
          let x_right=x_op+.x1_op-.x0_r -. dist_r+.(if right=[] then 0. else op.op_right_spacing) in

            (Drawing {
               drawing_min_width=x_op;
               drawing_nominal_width=x_op;
               drawing_max_width=x_op;
               drawing_y0=y0_l;
               drawing_y1=y1_l;
               drawing_badness=(fun _->0.);
               drawing_contents=(fun _->left)})::

              (Drawing {
                 drawing_min_width=x_right-.x_op;
                 drawing_nominal_width=x_right-.x_op;
                 drawing_max_width=x_right-.x_op;
                 drawing_y0=y0_op;
                 drawing_y1=y1_op;
                 drawing_badness=(fun _->0.);
                 drawing_contents=(fun _->draw_boxes op_noad)})::
              (Drawing {
                 drawing_min_width=x1_r-.x0_r;
                 drawing_nominal_width=x1_r-.x0_r;
                 drawing_max_width=x1_r-.x0_r;
                 drawing_y0=y0_l;
                 drawing_y1=y1_l;
                 drawing_badness=(fun _->0.);
                 drawing_contents=(fun _->right)})::
              (draw_maths mathsEnv style s)
        )
      | Decoration (rebox, inside) :: s ->(
          (rebox env style ((draw_maths mathsEnv style inside))) @ (draw_maths mathsEnv style s)
        )



let dist_boxes a b=
  let left=draw_boxes a in
  let bezier_left=bezier_of_boxes left in
  let right=draw_boxes b in
  let bezier_right=bezier_of_boxes right in
  let (x0_r,y0_r,x1_r,y1_r)=bounding_box right in
  let (x0_l,y0_l,x1_l,y1_l)=bounding_box left in
  let lr = List.map (fun (x,y)->Array.map (fun x0->x0+.x1_l-.x0_r) x, y) bezier_right in
    List.fold_left (fun a b->List.fold_left (fun c d->min_dist c b d) a bezier_left) infinity lr


let glyphs c env st=
  let font=env.mathsFont in
  let s=env.mathsSize in
  let rec make_it idx=
    if UTF8.out_of_range c idx then [] else (
      { glyph_utf8=UTF8.init 1 (fun _->UTF8.look c idx);
        glyph_index=Fonts.glyph_of_uchar font (UTF8.look c idx) } :: make_it (UTF8.next c idx)
    )
  in
    List.map (fun gl->GlyphBox { (glyphCache font gl) with glyph_size=s}) (env.mathsSubst (make_it (UTF8.first c)))

exception Unknown_symbol of string

let symbol s env st=try
  let font=env.mathsFont in
    [ GlyphBox { (glyphCache font { empty_glyph with glyph_index=Binary.StrMap.find s env.mathsSymbols }) with glyph_size=env.mathsSize} ]
with
    Not_found->raise (Unknown_symbol s)

(* let gl_font env st font c= *)
(*   let _,s=(env.fonts.(int_of_style st)) in *)
(*   let rec make_it idx= *)
(*     if UTF8.out_of_range c idx then [] else ( *)
(*       (GlyphBox { (glyphCache font { empty_glyph with glyph_index=Fonts.glyph_of_uchar font (UTF8.look c idx)}) *)
(*                   with glyph_size=s; glyph_x=0.; glyph_y=0. }) :: (make_it (UTF8.next c idx)) *)
(*     ) *)
(*   in *)
(*     make_it (UTF8.first c) *)

(* let int env st= *)
(*   let font,s=(env.fonts.(int_of_style st)) in *)
(*     Drawing (drawing ~offset:(-0.8) [ *)
(*       Glyph { (glyphCache font { empty_glyph with glyph_index= 701 }) with glyph_size=1.; glyph_y= -0.8 } *)
(*     ]) *)

(* let rightarrow env st= *)
(*   let font,s=(env.fonts.(int_of_style st)) in *)
(*     Drawing (drawing ~offset:(-0.8) [ *)
(*       Glyph { (glyphCache font { empty_glyph with glyph_index= 334 }) with glyph_size=1. } *)
(*     ]) *)

(* let leftarrow env st= *)
(*   let font,s=(env.fonts.(int_of_style st)) in *)
(*     Drawing (drawing ~offset:(-0.8) [ *)
(*       Glyph { (glyphCache font { empty_glyph with glyph_index= 332 }) with glyph_size=1. } *)
(*     ]) *)

let open_close left right env style box=
  let s=env.mathsSize in
  let left=draw_boxes left in
  let bezier_left=bezier_of_boxes left in

  let mid=draw_boxes box in
  let bezier_mid=bezier_of_boxes mid in

  let right=draw_boxes right in
  let bezier_right=bezier_of_boxes right in

  let (x0_r,y0_r,x1_r,y1_r)=bounding_box right in
  let (x0_l,y0_l,x1_l,y1_l)=bounding_box left in
  let (x0,y0,x1,y1)=bounding_box mid in

  let l0 = List.map (fun (x,y)->Array.map (fun x->x+.x1_l-.x0) x, y) bezier_mid in
  let dist0=List.fold_left (fun a b->List.fold_left (fun c d->min_dist c b d) a bezier_left) infinity l0 in

  let l1 = List.map (fun (x,y)->Array.map (fun x->x+.x1-.x0_r) x, y) bezier_right in
  let dist1=List.fold_left (fun a b->List.fold_left (fun c d->min_dist c b d) a bezier_mid) infinity l1 in


    (Drawing {
       drawing_min_width=x1_l-.dist0 +. env.open_dist*.s;
       drawing_nominal_width=x1_l-.dist0 +. env.open_dist*.s;
       drawing_max_width=x1_l-.dist0 +. env.open_dist*.s;
       drawing_y0=y0_l;
       drawing_y1=y1_l;
       drawing_badness=(fun _->0.);
       drawing_contents=(fun _->left)})::

    (Drawing {
       drawing_min_width=x1-.x0-.dist1  +. env.close_dist*.s;
       drawing_nominal_width=x1-.x0-.dist1  +. env.close_dist*.s;
       drawing_max_width=x1-.x0-.dist1 +. env.close_dist*.s;
       drawing_y0=y0;
       drawing_y1=y1;
       drawing_badness=(fun _->0.);
       drawing_contents=(fun _->draw_boxes box)})::
    (Drawing {
       drawing_min_width=x1_r-.x0_r;
       drawing_nominal_width=x1_r-.x0_r;
       drawing_max_width=x1_r-.x0_r;
       drawing_y0=y0_r;
       drawing_y1=y1_r;
       drawing_badness=(fun _->0.);
       drawing_contents=(fun _->right)})::[]
