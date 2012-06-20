open CamomileLibrary
open Util
open OutputCommon
open Box
open Fonts.FTypes
open Document
open Document.Mathematical

let env_style env style=match style with
    Display->env.(0)
  | Display'->env.(1)
  | Text->env.(2)
  | Text'->env.(3)
  | Script->env.(4)
  | Script'->env.(5)
  | ScriptScript->env.(6)
  | ScriptScript'->env.(7)

type 'a noad= { mutable nucleus: 'a Document.environment -> Mathematical.style ->  'a box list;
                mutable subscript_left:'a math list; mutable superscript_left:'a math list;
                mutable subscript_right:'a math list; mutable superscript_right:'a math list }

and 'a binary_type =
    Invisible
  | Normal of bool * 'a noad * bool (* the boolean remove spacing at left or right when true *)

and 'a binary= { bin_priority:int; bin_drawing:'a binary_type; bin_left:'a math list; bin_right:'a math list }
and 'a fraction= { numerator:'a math list; denominator:'a math list; line: 'a Document.environment->style->OutputCommon.path_parameters }
and 'a operator= { op_noad:'a noad; op_limits:bool; op_left_spacing:float; op_right_spacing:float; op_left_contents:'a math list; op_right_contents:'a math list }
and 'a math=
    Ordinary of 'a noad
  | Glue of float
  | Env of ('a Document.environment->'a Document.environment)
  | Scope of ('a Document.environment->Mathematical.style->'a math list)
  | Binary of 'a binary
  | Fraction of 'a fraction
  | Operator of 'a operator
  | Decoration of ('a Document.environment -> Mathematical.style -> 'a box list -> 'a box list)*('a math list)


let noad n={ nucleus=n; subscript_left=[]; superscript_left=[]; subscript_right=[]; superscript_right=[] }

let style x = Env (fun env -> { env with mathStyle = x })


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

let apply_head f envs = match envs with
  | [] -> []
  | env :: rest -> (f env) :: rest
let superStyle x = apply_head (fun env -> { env with mathStyle = scriptStyle x })
let subStyle x = apply_head (fun env -> { env with mathStyle = cramp (scriptStyle x) })
let numeratorStyle x = apply_head (fun env -> { env with mathStyle = 
					      (match x with
                                                      Display -> Text | Display' -> Text'
                                                    | _ -> scriptStyle x) })
let denominatorStyle = numeratorStyle


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
  (*let d'=ref infinity in
    for i=0 to Array.length xa-1 do
      for j=0 to Array.length xb-1 do
        d':=min !d' (dist xa.(i) ya.(i) xb.(j) yb.(j))
      done
    done;
    if !d'<=d then*) 
  let dn = Bezier.distance (xa,ya) (xb,yb) in
  let d0 = Bezier.distance1 (xa.(0),ya.(0)) (xb,yb) in
  let d1 = Bezier.distance1 (xa.(Array.length xa-1),ya.(Array.length ya-1)) (xb,yb) in
  let d2 = Bezier.distance1 (xb.(0),yb.(0)) (xa,ya) in
  let d3 = Bezier.distance1 (xb.(Array.length xb-1),yb.(Array.length yb-1)) (xa,ya) in
    min (min d dn) (min (min d0 d1) (min d2 d3))

let max_array x =
  Array.fold_left max (-. infinity) x

let min_array x =
  Array.fold_left min (infinity) x


let min_dist_left_right l1 l2 =
  let l1 = List.map (fun (x,y) -> max_array x, (x, y)) l1 in
  let l2 = List.map (fun (x,y) -> min_array x, (x, y)) l2 in
  let l1 = List.sort (fun (x,_) (x',_) -> compare x' x) l1 in
  let l2 = List.sort (fun (x,_) (x',_) -> compare x x') l2 in
  let rec fn acc l1 l2 =
    match l1, l2 with
      ((x,a)::l1') as l1, (y,b)::l2 ->
	if (y -. x) >= acc then acc else
	  let acc = min_dist acc a b in
	  let rec gn acc l1 =
	    match l1 with
	      (x,a)::l1' ->
		if (y -. x) >= acc then acc else
		  let acc = min_dist acc a b in
		  gn acc l1'
	    | [] -> acc
	  in
	  let acc = gn acc l1' in
	  fn acc l1 l2
    | _ -> acc
  in
  fn infinity l1 l2
 
let line (a,b) (c,d)=[|a;c|], [|b;d|]

let rec contents=function
    GlyphBox x->(Fonts.glyphNumber x.glyph).glyph_utf8
  | Kerning x->contents x.kern_contents
  | _->""


let rec draw env_stack mlist=
  let env=match env_stack with []->assert false | h::s->h in
  let style = env.mathStyle in
  let mathsEnv=env_style env.mathsEnvironment style in

    match mlist with

        []->[]
      | h::Glue x::Glue y::s->draw env_stack (h::Glue (x+.y)::s)
      | Glue _::s->draw env_stack s
      | h::Glue x::s->(
          let left=draw env_stack [h] in
            (match List.rev left with
                 []->[]
               | h0::s0->List.rev (Kerning { (Fonts.FTypes.empty_kern h0) with Fonts.FTypes.advance_width=x }::s0))
              @ (draw env_stack s)
        )
      | Scope l::s->(
          (draw env_stack (l env style))@(draw env_stack s)
        )
      | Env f::s->
          draw (f (List.hd env_stack)::env_stack) s
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
                  let x_h=let x=Fonts.loadGlyph (Lazy.force mathsEnv.mathsFont)
                    ({empty_glyph with glyph_index=Fonts.glyph_of_char (Lazy.force mathsEnv.mathsFont) 'x'}) in
                    (Fonts.glyph_y1 x)/.1000.
                  in
                    if x_h=infinity || x_h= -.infinity then 1./.phi else x_h
                in
                let u,v=if contents (last nucleus) <> "" then  0., 0. else
                  y1 -. mathsEnv.sup_drop*.mathsEnv.mathsSize*.env.size,
                  -.y0+. mathsEnv.sub_drop*.mathsEnv.mathsSize*.env.size
                in

                let sub_env=drop (List.length env_stack-1) env_stack in
                let a=draw_boxes (draw (superStyle style sub_env) n.superscript_right) in
                let b=draw_boxes (draw (superStyle style sub_env) n.superscript_left) in
                let c=draw_boxes (draw (subStyle style sub_env) n.subscript_right) in
                let d=draw_boxes (draw (subStyle style sub_env) n.subscript_left) in
                let bezier=Array.map bezier_of_boxes [| a;b;c;d |] in
                let bb=Array.map (fun l->bounding_box [Path (OutputCommon.default, [Array.of_list l])]) bezier in

                let y_place sup sub=
                  let xa0,ya0,xa1,ya1=bb.(sub) in
                  let xb0,yb0,xb1,yb1=bb.(sup) in
                    if bezier.(sup)=[] then (
                      0., max v (max (mathsEnv.sub1*.mathsEnv.mathsSize*.env.size) (ya1 -. 4./.5. *. abs_float x_height*.mathsEnv.mathsSize*.env.size))
                    ) else (
                      let p=
                        if style=Display then mathsEnv.sup1 else
                          if is_cramped style then mathsEnv.sup3 else mathsEnv.sup2
                      in
                      let u=max u (max (p*.mathsEnv.mathsSize*.env.size) (-.yb0 +. (abs_float x_height*.mathsEnv.mathsSize*.env.size)/.4.)) in
                        if bezier.(sub)=[] then (
                          u,0.
                        ) else (
                          let v=max v (mathsEnv.sub2*.mathsEnv.mathsSize*.env.size) in
                            if (u+.yb0) -. (ya1-.v) > 4.*.mathsEnv.default_rule_thickness then (
                              u,v
                            ) else (
                              let v=4.*.mathsEnv.default_rule_thickness*.mathsEnv.mathsSize*.env.size -. (u+.yb0) +. ya1 in
                              let psi=4./.5.*.abs_float x_height*.mathsEnv.mathsSize*.env.size -. (u+.yb0) in
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
                                       if mathsEnv.kerning then
                                         let ll = List.map (fun (x,y)->Array.map (fun x0->x0 +. xoff.(i)) x,
                                                              Array.map (fun x0->x0 +. yoff.(i)) y) l
                                         in if i mod 2 = 0 then 
					       min_dist_left_right l1 ll
					   else
					       min_dist_left_right ll l1
                                        else
                                         0.
                                    ) bezier
                in
                let dr=(draw_boxes nucleus)
                  @ (if n.superscript_right=[] then [] else
                      List.map (translate (xoff.(0)-.dist.(0)+.
					     mathsEnv.mathsSize*.env.size*.mathsEnv.superscript_distance) yoff.(0)) a)
                  @ (if n.superscript_left=[] then [] else
		      List.map (translate (xoff.(1)+.dist.(1)-.
					     mathsEnv.mathsSize*.env.size*.mathsEnv.superscript_distance) yoff.(1)) b)
                  @ (if n.subscript_right=[] then [] else
		      List.map (translate (xoff.(2)-.dist.(2)+.
					     mathsEnv.mathsSize*.env.size*.mathsEnv.subscript_distance) yoff.(2)) c)
                  @ (if n.subscript_left=[] then [] else
		      List.map (translate (xoff.(3)+.dist.(3)-.
					     mathsEnv.mathsSize*.env.size*.mathsEnv.subscript_distance) yoff.(3)) d)
                in
                let (a0,a1,a2,a3) = bounding_box dr in
                  [ Drawing ({ drawing_min_width=a2-.a0;
                               drawing_nominal_width=a2-.a0;
                               drawing_max_width=a2-.a0;
                               drawing_y0=a1;
                               drawing_y1=a3;
                               drawing_badness=(fun _->0.);
                               drawing_contents=(fun _->List.map (translate (-.a0) 0.) dr) }) ]
              ) else
                nucleus
        )@(draw env_stack s)

      | Binary b::s->(

          let rec find_priority=function
              Binary b0->
                List.fold_left (fun p x->min p (find_priority x))
                  (List.fold_left (fun p x->min p (find_priority x)) b0.bin_priority b0.bin_left)
                  (b0.bin_right)
            | Operator op->Array.length mathsEnv.priorities - 1
            | _->Array.length mathsEnv.priorities - 1
          in
          let priorities=mathsEnv.priorities in
          let prio=find_priority (Binary b) in
          let fact=
              (priorities.(prio) -. priorities.(b.bin_priority))
              /.priorities.(b.bin_priority)
          in
          let gl=(1.+.fact*.fact) *. priorities.(b.bin_priority) *. mathsEnv.priority_unit in
          let gl0=glue gl gl gl in
          let gl1=match gl0 with
              Box.Glue x->Drawing x
            | x->x
          in
	    match b.bin_drawing with
	        Invisible ->
                  (draw env_stack b.bin_left)@
                    (resize (mathsEnv.mathsSize*.env.size*.mathsEnv.invisible_binary_factor) gl1)::
                    (draw env_stack b.bin_right)
	      | Normal(no_sp_left, op, no_sp_right) ->
                  (draw env_stack b.bin_left)@
                    (if no_sp_left then [] else [resize (mathsEnv.mathsSize*.env.size) gl0])@
                    (draw env_stack [Ordinary op])@
                    (if no_sp_right then [] else [resize (mathsEnv.mathsSize*.env.size) gl1])@
                    (draw env_stack b.bin_right)
        )@(draw env_stack s)

      | (Fraction f)::s->(

        let hx =
          let x=Fonts.loadGlyph (Lazy.force mathsEnv.mathsFont)
	    ({empty_glyph with glyph_index=Fonts.glyph_of_char (Lazy.force mathsEnv.mathsFont) 'o'}) in
          (Fonts.glyph_y1 x)/.2000.
        in

        let ba=draw_boxes (draw (numeratorStyle style env_stack) f.numerator) in
        let bb=draw_boxes (draw (denominatorStyle style env_stack) f.denominator) in
          let x0a,y0a,x1a,y1a=bounding_box ba in
          let x0b,y0b,x1b,y1b=bounding_box bb in
          let wa=x1a-.x0a in
          let wb=x1b-.x0b in
          let w=max wa wb in
          let ln=f.line env style in
            Drawing ({ drawing_min_width=w;
                       drawing_nominal_width=w;
                       drawing_max_width=w;
                       drawing_y0=y0b-.y1b-.mathsEnv.mathsSize*.env.size*.(mathsEnv.denominator_spacing+.ln.lineWidth/.2.);
                       drawing_y1=y1a-.y0a+.mathsEnv.mathsSize*.env.size*.(mathsEnv.numerator_spacing+.ln.lineWidth/.2.);
                       drawing_badness=(fun _->0.);
                       drawing_contents=(fun _->
                                           (if ln.lineWidth = 0. then [] else
                                              [Path ({ln with lineWidth=ln.lineWidth*.mathsEnv.mathsSize*.env.size},
                                                     [ [|line (0.,hx) (w,hx)|] ]) ])@
                                             (List.map (translate ((w-.wa)/.2.) (hx +. -.y0a+.mathsEnv.mathsSize*.env.size*.(mathsEnv.numerator_spacing+.ln.lineWidth/.2.))) ba)@
                                             (List.map (translate ((w-.wb)/.2.) (hx +. -.y1b-.mathsEnv.mathsSize*.env.size*.(mathsEnv.denominator_spacing+.ln.lineWidth/.2.))) bb)
                                        ) }) :: (draw env_stack s)
        )
      | Operator op::s ->(

          (* Ici, si op.op_limits est faux, c'est facile : c'est comme
             pour Ordinary. Sinon, il faut calculer la densité de
             l'encre en haut et en bas du symbole pour centrer les
             indices et exposants. Si on passe op.op_limits=true et
             qu'il y a des indices des deux côtés, ils ne sont
             positionnés en-dessous que du côté où c'est possible *)

          let check_inf x=if x= -.infinity || x=infinity then 0. else x in
          let op_noad=
            if op.op_limits && (style=Display || style=Display') then (
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
              let drawn_op=draw_boxes (draw env_stack [Ordinary op'']) in

              let ba=draw_boxes (draw (superStyle style env_stack) sup) in
              let bb=draw_boxes (draw (subStyle style env_stack) sub) in
              let x0,y0,x1,y1=bounding_box drawn_op in
              let x0a_,y0a_,x1a_,y1a_=bounding_box ba in
              let x0b_,y0b_,x1b_,y1b_=bounding_box bb in
              let x0a,y0a,x1a,y1a=(check_inf x0a_,check_inf y0a_,check_inf x1a_,check_inf y1a_) in
              let x0b,y0b,x1b,y1b=(check_inf x0b_,check_inf y0b_,check_inf x1b_,check_inf y1b_) in
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
              let xsup,xsub=
                if mathsEnv.kerning then (
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
                    (center 1 (y1-.(y1-.y0)/.10.) 0. 0.,
                     center 1 y0 0. 0.)
                ) else (
                  (x1+.x0)/.2.,(x1+.x0)/.2.
                )
              in
              let miny=y0-.y1b-.mathsEnv.limit_subscript_distance*.
                mathsEnv.mathsSize*.env.size +. y0b
              in
              let maxy=y1-.y0a+.mathsEnv.limit_superscript_distance*.
                mathsEnv.mathsSize*.env.size +. y1a
              in
              let xoff=min x0 (min (xsup-.(x1a-.x0a)/.2.) (xsub-.(x1b-.x0b)/.2.)) in
                [ Drawing {
                    drawing_min_width=max (x1-.x0) (max (x1a-.x0a) (x1b-.x0b));
                    drawing_nominal_width=max (x1-.x0) (max (x1a-.x0a) (x1b-.x0b));
                    drawing_max_width=max (x1-.x0) (max (x1a-.x0a) (x1b-.x0b));
                    drawing_y0=miny;
                    drawing_y1=maxy;
                    drawing_badness=(fun _->0.);
                    drawing_contents=
                      (fun _->
                         List.map (translate (-.xoff) 0.) drawn_op @
                           (List.map (translate (xsup-.xoff-.(x1a+.x0a)/.2.) (y1-.y0a+.mathsEnv.limit_superscript_distance*.mathsEnv.mathsSize*.env.size)) ba)@
                           (List.map (translate (xsub-.xoff-.(x1b+.x0b)/.2.) (y0-.y1b-.mathsEnv.limit_subscript_distance*.mathsEnv.mathsSize*.env.size)) bb)
                      ) }]

            ) else draw env_stack [Ordinary op.op_noad]
          in
          let left=draw_boxes (draw env_stack op.op_left_contents) in
          let bezier_left=bezier_of_boxes left in
          let right=draw_boxes (draw env_stack op.op_right_contents) in
          let bezier_right=bezier_of_boxes right in
          let bezier_op=bezier_of_boxes (draw_boxes op_noad) in
          let (x0_r_,y0_r_,x1_r_,y1_r_)=bounding_box right in
          let (x0_r,y0_r,x1_r,y1_r)=(check_inf x0_r_,
                                     check_inf y0_r_,
                                     check_inf x1_r_,
                                     check_inf y1_r_)
          in
          let (x0_l_,y0_l_,x1_l_,y1_l_)=bounding_box left in
          let (x0_l,y0_l,x1_l,y1_l)=(check_inf x0_l_,
                                     check_inf y0_l_,
                                     check_inf x1_l_,
                                     check_inf y1_l_)
          in

          let (x0_op,y0_op,x1_op,y1_op)=bounding_box (draw_boxes op_noad) in

          let lr = List.map (fun (x,y)->Array.map (fun x0->x0 +. x1_op-.x0_r) x, y) bezier_right in
          let dist_r=if mathsEnv.kerning then
	      min_dist_left_right bezier_op lr
          else 0.
          in

          let ll = List.map (fun (x,y)->Array.map (fun x0->x0 -. x1_l+.x0_op) x, y) bezier_left in
          let dist_l=if mathsEnv.kerning then
	      min_dist_left_right ll bezier_op
            else 0.
          in
          let x_op=if left=[] then 0. else (x1_l-.dist_l+.op.op_left_spacing) in
          let x_right=x_op+.x1_op-.x0_r -. dist_r+.(if right=[] then 0. else op.op_right_spacing) in
          let gl=match glue (4./.9.) (4./.9.) (9./.18.) with
              Box.Glue x->Drawing x
            | x->x
          in
            (Drawing {
               drawing_min_width=x_op;
               drawing_nominal_width=x_op;
               drawing_max_width=x_op;
               drawing_y0=y0_l;
               drawing_y1=y1_l;
               drawing_badness=(fun _->0.);
               drawing_contents=(fun _->left)})
              ::(if left<>[] then [gl] else [])
              @[Drawing {
                 drawing_min_width=x_right-.x_op;
                 drawing_nominal_width=x_right-.x_op;
                 drawing_max_width=x_right-.x_op;
                 drawing_y0=y0_op;
                 drawing_y1=y1_op;
                 drawing_badness=(fun _->0.);
                 drawing_contents=(fun _->draw_boxes op_noad)}]
              @(if right<>[] then [gl] else [])
              @[Drawing {
                 drawing_min_width=x1_r-.x0_r;
                 drawing_nominal_width=x1_r-.x0_r;
                 drawing_max_width=x1_r-.x0_r;
                 drawing_y0=y0_l;
                 drawing_y1=y1_l;
                 drawing_badness=(fun _->0.);
                 drawing_contents=(fun _->right)}]
              @(draw env_stack s)
        )
      | Decoration (rebox, inside) :: s ->(
          (rebox env style ((draw env_stack inside))) @ (draw env_stack s)
        )


let dist_boxes a b=
  let left=draw_boxes a in
  let bezier_left=bezier_of_boxes left in
  let right=draw_boxes b in
  let bezier_right=bezier_of_boxes right in
  let (x0_r,y0_r,x1_r,y1_r)=bounding_box right in
  let (x0_l,y0_l,x1_l,y1_l)=bounding_box left in
  let lr = List.map (fun (x,y)->Array.map (fun x0->x0+.x1_l-.x0_r) x, y) bezier_right in
  min_dist_left_right bezier_left lr


let glyphs c envs st=
  let env=env_style envs.mathsEnvironment st in
  let font=Lazy.force env.mathsFont in
  let s=env.mathsSize*.envs.size in
  let rec make_it idx=
    if UTF8.out_of_range c idx then [] else (
      { glyph_utf8=UTF8.init 1 (fun _->UTF8.look c idx);
        glyph_index=Fonts.glyph_of_uchar font (UTF8.look c idx) } :: make_it (UTF8.next c idx)
    )
  in
    List.map (fun gl->GlyphBox { (glyphCache font gl) with glyph_size=s}) (env.mathsSubst (make_it (UTF8.first c)))


let change_fonts env font=
    { env with
        mathsEnvironment=Array.map (fun x->{x with mathsSubst=(fun x->x); mathsFont=Lazy.lazy_from_val font}) env.mathsEnvironment }

let symbol ?name:(name="") font n envs st=
  let env=env_style envs.mathsEnvironment st in
  let s=env.mathsSize*.envs.size in
  let rec make_it=function
      []->[]
    | h::s->
        { glyph_utf8="";
          glyph_index=h } :: make_it s
  in
  let gls=match make_it n with
      []->[]
    | h::s->{ h with glyph_utf8=name^" " }::s
  in
    List.map (fun gl->GlyphBox { (glyphCache font gl) with glyph_size=s}) gls


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

let open_close left right env_ style box=
  let env=env_style env_.mathsEnvironment style in
  let s=env.mathsSize*.env_.size in
  let left=draw_boxes (left env_ style) in
  let bezier_left=bezier_of_boxes left in

  let mid=draw_boxes box in
  let bezier_mid=bezier_of_boxes mid in

  let right=draw_boxes (right env_ style) in
  let bezier_right=bezier_of_boxes right in

  let (x0_r,y0_r,x1_r,y1_r)=bounding_box right in
  let (x0_l,y0_l,x1_l,y1_l)=bounding_box left in
  let (x0,y0,x1,y1)=bounding_box mid in

  let l0 = List.map (fun (x,y)->Array.map (fun x->x+.x1_l-.x0) x, y) bezier_mid in
  let dist0=if env.kerning then min_dist_left_right bezier_left l0 else 0. in

  let l1 = List.map (fun (x,y)->Array.map (fun x->x+.x1-.x0_r) x, y) bezier_right in
  let dist1=if env.kerning then min_dist_left_right bezier_mid l1 else 0. in


    (Drawing {
       drawing_min_width=x1_l-.dist0 +. if env.kerning then env.open_dist*.s else 0.;
       drawing_nominal_width=x1_l-.dist0 +. if env.kerning then env.open_dist*.s else 0.;
       drawing_max_width=x1_l-.dist0 +. if env.kerning then env.open_dist*.s else 0.;
       drawing_y0=y0_l;
       drawing_y1=y1_l;
       drawing_badness=(fun _->0.);
       drawing_contents=(fun _->(left (*  @ [(Path ({ default with lineWidth = 0.01 ; close = true},  
        [rectangle (x0_l,y0_l) (x1_l,y1_l)]))] *) ))})::
      box@ 
(*
    (Drawing {
       drawing_min_width=x1-.x0-.dist1  +. if env.kerning then env.close_dist*.s else 0.;
       drawing_nominal_width=x1-.x0-.dist1  +. if env.kerning then env.close_dist*.s else 0.;
       drawing_max_width=x1-.x0-.dist1 +. if env.kerning then env.close_dist*.s else 0.;
       drawing_y0=y0;
       drawing_y1=y1;
       drawing_badness=(fun _->0.);
       drawing_contents=(fun _->(draw_boxes box  @ [(Path ({ default with lineWidth = 0.01 ; close = true},  
        [rectangle (x0,y0) (x1,y1)]))] ))})::
*)
      [Drawing {
         drawing_min_width=x1_r-.x0_r-.dist1  +. if env.kerning then env.close_dist*.s else 0.;
         drawing_nominal_width=x1_r-.x0_r-.dist1  +. if env.kerning then env.close_dist*.s else 0.;
         drawing_max_width=x1_r-.x0_r-.dist1  +. if env.kerning then env.close_dist*.s else 0.;
         drawing_y0=y0_r;
         drawing_y1=y1_r;
         drawing_badness=(fun _->0.);
         drawing_contents=(fun _-> List.map (translate (-. dist1 +. if env.kerning then env.close_dist*.s else 0.) (0.0)) (right (* @ [(Path ({ default with lineWidth = 0.01 ; close = true},  
                                      [rectangle (x0_r,y0_r) (x1_r,y1_r)]))]*) ))}]
