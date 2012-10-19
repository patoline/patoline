open CamomileLibrary
open Util
open OutputCommon
open Box
open Fonts.FTypes
open Document
open Document.Mathematical

let debug_kerning = ref false

let env_style env style=match style with
    Display->env.(0)
  | Display'->env.(1)
  | Text->env.(2)
  | Text'->env.(3)
  | Script->env.(4)
  | Script'->env.(5)
  | ScriptScript->env.(6)
  | ScriptScript'->env.(7)

type ('a,'b) noad= { mutable nucleus: 'b;
                mutable subscript_left:'a math list; mutable superscript_left:'a math list;
                mutable subscript_right:'a math list; mutable superscript_right:'a math list }

and 'a nucleus = Document.environment -> Mathematical.style ->  box list
and 'a nucleuses = (Document.environment -> Mathematical.style ->  box list) list

and 'a binary_type =
    Invisible
  | Normal of bool * ('a,'a nucleus) noad * bool (* the boolean remove spacing at left or right when true *)

and 'a binary= { bin_priority:int; bin_drawing:'a binary_type; bin_left:'a math list; bin_right:'a math list }
and 'a fraction= { numerator:'a math list; denominator:'a math list; line: Document.environment->style->OutputCommon.path_parameters }
and 'a operator= { op_noad:('a,'a nucleuses) noad ; op_limits:bool; op_left_contents:'a math list; op_right_contents:'a math list }
and 'a math=
    Ordinary of ('a,'a nucleus) noad 
  | Glue of drawingBox
  | Env of (Document.environment->Document.environment)
  | Scope of (Document.environment->Mathematical.style->'a math list)
  | Binary of 'a binary
  | Fraction of 'a fraction
  | Operator of 'a operator
  | Decoration of (Document.environment -> Mathematical.style -> box list -> box list)*('a math list)

let noad n={ nucleus=n; subscript_left=[]; superscript_left=[]; subscript_right=[]; superscript_right=[] }

let style x = Env (fun env -> { env with mathStyle = x })

let fraction a b=Fraction {
  numerator=a;
  denominator=b;
  line=(fun env style->{OutputCommon.default with lineWidth = (env_style env.mathsEnvironment style).default_rule_thickness})
}
let bin_invisible prio left right=
  Binary { bin_priority=prio; bin_drawing=Invisible; bin_left=left; bin_right=right }
let bin prio drawing left right=
  Binary { bin_priority=prio; bin_drawing=drawing; bin_left=left; bin_right=right }
let op_limits a b c=
  Operator { op_noad=b; op_limits=true; op_left_contents=a; op_right_contents=c}
let op_nolimits a b c=
  Operator { op_noad=b; op_limits=false; op_left_contents=a; op_right_contents=c}
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

let interval_point_dist (xa,ya) (xb,yb) (x,y) =
  let xv = xb -. xa and yv = yb -. ya in
  let xw = x -. xa and yw = y -. ya in
  let u = (xv *. xw +. yv *. yw) /. (xv *. xv +. yv *. yv) in
  if 0.0 <= u && u <= 1.0 then
    let xm = xb *. u +. xa *. (1.0 -. u) -. x
    and ym = yb *. u +. ya *. (1.0 -. u) -. y in
    sqrt (xm *. xm +. ym *. ym)
  else if u < 0.0 then 
    sqrt (xw *. xw +. yw *. yw)
  else
    let xm = x -. xb and ym = y -. yb in
    sqrt (xm *. xm +. ym *. ym)

let interval_interval_dist (xa,ya) (xb,yb) (xa',ya') (xb',yb') =
  (* We assume the interval do not intersect and therefore, the distance is
     positive *)
  min (min (interval_point_dist (xa,ya) (xb,yb) (xa',ya'))
	 (interval_point_dist (xa,ya) (xb,yb) (xb',yb')))
    (min (interval_point_dist (xa',ya') (xb',yb') (xa,ya))
       (interval_point_dist (xa',ya') (xb',yb') (xb,yb)))

let min_dist' d (xa,ya) (xb,yb)=
  let d'=ref d in
  for i=0 to Array.length xa-1 do
    for j=0 to Array.length xb-1 do
      let pa = if i = Array.length xa-1 then (xa.(0), ya.(0)) else (xa.(i+1), ya.(i+1)) in
      let pb = if j = Array.length xb-1 then (xb.(0), yb.(0)) else (xb.(j+1), yb.(j+1)) in
      d':=min !d' (interval_interval_dist  (xa.(i), ya.(i)) pa
		                           (xb.(j), yb.(j)) pb)
    done
  done;
  !d'
    
let min_dist d (xa,ya) (xb,yb)=
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


let min_dist_left_right precise l1 l2 =
  let l1 = match precise with
      None -> l1
    | Some t ->
      List.fold_left (fun acc b -> Bezier.subdivise t b @ acc) [] l1
  in
  let l2 = match precise with
      None -> l2
    | Some t -> List.fold_left (fun acc b -> Bezier.subdivise t b @ acc) [] l2
  in
  let l1 = List.map (fun (x,y) -> max_array x, (x, y)) l1 in
  let l2 = List.map (fun (x,y) -> min_array x, (x, y)) l2 in
  let l1 = List.sort (fun (x,_) (x',_) -> compare x' x) l1 in
  let l2 = List.sort (fun (x,_) (x',_) -> compare x x') l2 in

  let min_dist = if precise = None then min_dist else min_dist' in
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

let adjust_space ?(absolute=false) env target minimum box_left box_right =

  if not env.kerning then target else
  
  let epsilon = 5e-2 in
  let bezier_left = bezier_of_boxes box_left in
  let bezier_right = bezier_of_boxes box_right in

  let delta =
    let (x0_l,y0_l,x1_l,y1_l)=bounding_box_kerning box_left in
    x1_l -. x0_l
  in
  
  let rec fn space =
    let ll = List.map (fun (x,y)->Array.map (fun x0-> x0 +. space +. delta) x, y) bezier_right in
    
    let dist = min_dist_left_right env.precise_kerning bezier_left ll in
    let new_space = space +. target -. dist in
    if !debug_kerning then
      Printf.printf "space = %f, new_space = %f, dist = %f, target = %f, minimum = %f\n"
	space new_space dist target minimum;
    if new_space < minimum then minimum else
    if dist <= target +. epsilon then new_space else
      fn new_space
  in
  let r = fn target in
  if absolute then r +. delta else r

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
      | Glue x::s->(
	  let right=draw env_stack s in
	  Box.Glue x :: right)
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

		let box_nucleus = draw_boxes env nucleus in
                let x0,y0,x1,y1=bounding_box box_nucleus in
                let is_letter=
                  match nucleus with
                      []->false
                    | h::s->
                      (match List.fold_left (fun _ x->x) h s with
                          GlyphBox g->(
                            let utf8=(Fonts.glyphNumber g.glyph).glyph_utf8 in
                            utf8.[0]<>'\\' &&
                              (let lastChar=UTF8.look utf8 (UTF8.last utf8) in
                               match CamomileLibraryDefault.Camomile.UCharInfo.general_category lastChar with
                                   (* Letters *)
                                   `Lu | `Ll | `Lt | `Lm | `Lo
                                 (* Numbers *)
                                 | `Nd | `Nl | `No -> true
                                 | _->false
                              )
                          )
                        | _->false)
                in
		if !debug_kerning then begin
		  Printf.printf "indices:\n" ;
		  Printf.printf "box nucleus: (%f,%f) (%f,%f)\n"  x0 x1 y0 y1;
		end;

                let x_height=
                  let x_h=let x=Fonts.loadGlyph (Lazy.force mathsEnv.mathsFont)
                    ({empty_glyph with glyph_index=Fonts.glyph_of_char (Lazy.force mathsEnv.mathsFont) 'x'}) in
                    (Fonts.glyph_y1 x)/.1000.
                  in
                    if x_h=infinity || x_h= -.infinity then 1./.phi else x_h
                in
                let u,v= if is_letter then  0., 0. else
                  y1 -. mathsEnv.sup_drop*.mathsEnv.mathsSize*.env.size,
                  -.y0+. mathsEnv.sub_drop*.mathsEnv.mathsSize*.env.size
                in

                let sub_env=drop (List.length env_stack-1) env_stack in
                let a=draw_boxes env (draw (superStyle style sub_env) n.superscript_right) in
                let b=draw_boxes env (draw (superStyle style sub_env) n.superscript_left) in
                let c=draw_boxes env (draw (subStyle style sub_env) n.subscript_right) in
                let d=draw_boxes env (draw (subStyle style sub_env) n.subscript_left) in
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

                let start =
                  let xa0,_,xa1,_=bb.(0) in
                  let xb0,_,xb1,_=bb.(1) in
                  let xc0,_,xc1,_=bb.(2) in
                  let xd0,_,xd1,_=bb.(3) in
                  [| x1 -. xa0 +. mathsEnv.mathsSize*.env.size*.mathsEnv.superscript_distance;
		       -. xb1 +. x0 -. mathsEnv.mathsSize*.env.size*.mathsEnv.superscript_distance;
		       x1 -. xc0 +. mathsEnv.mathsSize*.env.size*.mathsEnv.subscript_distance;
		       -. xd1 +. x0 -. mathsEnv.mathsSize*.env.size*.mathsEnv.subscript_distance 
		  |]
                in
		let xoff =
                  [| mathsEnv.mathsSize*.env.size*.mathsEnv.superscript_distance;
		     mathsEnv.mathsSize*.env.size*.mathsEnv.superscript_distance;
		     mathsEnv.mathsSize*.env.size*.mathsEnv.subscript_distance;
		     mathsEnv.mathsSize*.env.size*.mathsEnv.subscript_distance 
		  |]
                in
                let yoff=[| off_ya;off_yb; -.off_yc; -.off_yd |] in

                let xoff=Array.mapi (fun i l->
                  if mathsEnv.kerning then
		    let ll = List.map (translate 0.0 yoff.(i)) l in
		    let x0', _, x1', _ = bb.(i) in
		    let m = max ((x0 -. x1) /. 2.) ((x0' -. x1') /. 2.) in
                    if i mod 2 = 0 && xoff.(i) <> infinity && xoff.(i) <> -.infinity then 
		      adjust_space ~absolute:true mathsEnv xoff.(i) m box_nucleus ll
		    else
		      -. (adjust_space ~absolute:true mathsEnv xoff.(i) m ll box_nucleus)
                  else
                    start.(i)
                ) [|a;b;c;d|]
                in
                let dr=box_nucleus
                  @ (if n.superscript_right=[] then [] else
                      List.map (translate (xoff.(0)) yoff.(0)) a)
                  @ (if n.superscript_left=[] then [] else
		      List.map (translate (xoff.(1)) yoff.(1)) b)
                  @ (if n.subscript_right=[] then [] else
		      List.map (translate (xoff.(2)) yoff.(2)) c)
                  @ (if n.subscript_left=[] then [] else
		      List.map (translate (xoff.(3)) yoff.(3)) d)
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
	  let style_factor = match style with
	      Display | Display' | Text | Text'-> mathsEnv.priority_unit
	    | _ -> mathsEnv.priority_unit /. 1.5
	  in
	  let bin_left = draw env_stack b.bin_left in
	  let bin_right = draw env_stack b.bin_right in
	  let box_left = draw_boxes env bin_left in
	  let box_right = draw_boxes env bin_right in
          let (x0_r,y0_r,x1_r,y1_r)=bounding_box box_right in
          let (x0_l,y0_l,x1_l,y1_l)=bounding_box box_left in
	  let m_l = (x0_l -. x1_l)/.2.0 in
	  let m_r = (x0_r -. x1_r)/.2.0 in

	    match b.bin_drawing with
	        Invisible ->
		  let space = (mathsEnv.mathsSize*.env.size*.mathsEnv.invisible_binary_factor)*.
		    (1.+.fact*.fact) *. priorities.(b.bin_priority) *. style_factor
		  in
		  let dist = adjust_space mathsEnv space 0.0 box_left box_right in
		  let gl0=glue dist dist dist in
		  let gl0=match gl0 with
		      Box.Glue x->Drawing
                        { x with
                          drawing_badness=(fun w->100.*.(knuth_h_badness dist w))
                        }
		    | x->assert false
		  in
		    bin_left@
                    gl0::
                    bin_right

	    | Normal(no_sp_left, op, no_sp_right) ->

		let space = (mathsEnv.mathsSize*.env.size)
		  *. (1.+.fact*.fact) *. priorities.(b.bin_priority) *. style_factor in
		let op = draw env_stack [Ordinary op] in
		let box_op = draw_boxes env op in
		let (x0,_,x1,_) = bounding_box_full box_op in
		assert (x0 <= x1);
		let (x0',_,x1',_) = bounding_box_kerning box_op in
		let dist0 =
		  if bin_left = [] then 0.0 else
		  let space, m_r = if no_sp_left then 
		      max (mathsEnv.mathsSize*.env.size*.mathsEnv.denominator_spacing)
			(1.5 *. abs_float(x0 -. x0')), m_l
		    else space,m_l in
		  adjust_space mathsEnv space (m_r +. space) box_left box_op
		in
		let dist1 =
		  if bin_right= [] then 0.0 else
		  let space, m_r = if no_sp_right then 1.5 *. abs_float(x1 -. x1'), m_r
		    else space,m_r in
		  adjust_space mathsEnv space (m_r +. space) box_op box_right
		in
		(* computes distance left - right to avoid collisions above binary symbols *)
		let dist2 = 
		  if bin_left = [] or bin_right = [] then -. infinity else
		    let space = space /. 3.0 in
		    adjust_space mathsEnv space (max m_r m_l) box_left box_right
		in
		let dist1 = max dist1 (dist2 -. dist0 -. (x1 -. x0)) in
		let gl0=
		  if bin_left = [] then [] else (
		    match glue dist0 dist0 dist0 with
		        Box.Glue x->
		          [Drawing { x with
                            drawing_badness=(fun w->100.*.(knuth_h_badness dist0 w))
                           }]
		      | x->assert false
                  )
		in
		let gl1 =
		  if bin_right = [] then [] else (
		    match glue dist1 dist1 dist1 with
		        Box.Glue x->
		          [Box.Glue { x with
                            drawing_badness=(fun w->100.*.(knuth_h_badness dist1 w))
                          }]
		      | x->assert false
                  )
		in

                bin_left@
                  gl0@op@gl1@
                  bin_right
      )@(draw env_stack s)

      | (Fraction f)::s->(

        let ln=f.line env style in
        let hx =
          let x= Fonts.loadGlyph (Lazy.force mathsEnv.mathsFont)
	    ({empty_glyph with glyph_index=Fonts.glyph_of_char (Lazy.force mathsEnv.mathsFont) '-'}) in
          mathsEnv.mathsSize*.env.size*.(Fonts.glyph_y1 x +. Fonts.glyph_y0 x +.ln.lineWidth)/.2000.
        in

        let ba=draw_boxes env (draw (numeratorStyle style env_stack) f.numerator) in
        let bb=draw_boxes env (draw (denominatorStyle style env_stack) f.denominator) in
          let x0a,y0a,x1a,y1a=bounding_box ba in
          let x0b,y0b,x1b,y1b=bounding_box bb in
          let wa=x1a-.x0a in
          let wb=x1b-.x0b in
          let w=max wa wb in

            Drawing ({ drawing_min_width=w;
                       drawing_nominal_width=w;
                       drawing_max_width=w;
                       drawing_y0=hx +. y0b-.y1b-.mathsEnv.mathsSize*.env.size*.(mathsEnv.denominator_spacing+.ln.lineWidth/.2.);
                       drawing_y1=hx +. y1a-.y0a+.mathsEnv.mathsSize*.env.size*.(mathsEnv.numerator_spacing+.ln.lineWidth/.2.);
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

          let left=draw_boxes env (draw env_stack op.op_left_contents) in
          let right=draw_boxes env (draw env_stack op.op_right_contents) in
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
          (* FIXME : mettre les coefs and mathEnvs *)
	  let factor = if op.op_limits then
	      if (style=Display || style=Display') then 0.75 else 0.5
	    else
	      if (style=Display || style=Display') then 1.5 else 1.0
	  in
	  let op_noad_choice = 
	    let ll = op.op_noad.nucleus in
	    let lc = List.map (fun left ->
	      let left' = left env style in
	      let left'=draw_boxes env left' in
	      left, bounding_box left') ll
	    in
	    let rec fn = function
	      | [] -> assert false
	      | [c] -> c
	      | (left, (x0,y0,x1,y1)) as c :: l ->
		if (y1 -. y0) /.factor >= y1_l -. y0_l &&  (y1 -. y0) /. factor >= y1_r -. y0_r then c	else
		  fn  l	      
	    in
	    fst (fn lc)
	  in
	  let op_noad = { op.op_noad with nucleus = op_noad_choice } in
          let op_noad =
            if op.op_limits && (style=Display || style=Display') then (
              let op', sup=
                if op_noad.superscript_right=[] || op_noad.superscript_left=[] then
                  ({ op_noad with superscript_right=[]; superscript_left=[] },
                   op_noad.superscript_right @ op_noad.superscript_left)
                else
                  op_noad, []
              in
              let op'', sub=
                if op_noad.subscript_right=[] || op_noad.subscript_left=[] then
                  ({ op' with subscript_right=[]; subscript_left=[] },
                   op_noad.subscript_right @ op_noad.subscript_left)
                else
                  op', []
              in
              let drawn_op=draw_boxes env (draw env_stack [Ordinary op'']) in
              let x0,y0,x1,y1=bounding_box drawn_op in

              let ba=draw_boxes env (draw (superStyle style env_stack) sup) in
              let bb=draw_boxes env (draw (subStyle style env_stack) sub) in

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

            ) else draw env_stack [Ordinary op_noad]
          in

	  let box_op = draw_boxes env op_noad in
          let x0,y0,x1,y1=bounding_box box_op in
	  let half = (x0 -. x1)/.2. in
	  let dist_l = if left=[] then 0. else
	      adjust_space mathsEnv (mathsEnv.mathsSize*.env.size*.mathsEnv.left_op_dist) half
	    left box_op 
	  in

	  let dist_r = if right=[] then 0. else
	      adjust_space mathsEnv (mathsEnv.mathsSize*.env.size*.mathsEnv.right_op_dist) half
	    box_op right
	  in

            (if left = [] then [] else [Drawing {
               drawing_min_width=x1_l +. dist_l;
               drawing_nominal_width=x1_l +. dist_l;
               drawing_max_width=x1_l +. dist_l;
               drawing_y0=y0_l;
               drawing_y1=y1_l;
               drawing_badness=(fun _->0.);
               drawing_contents=(fun _->left)}])
              @[Drawing {
                 drawing_min_width=x1 +. dist_r;
                 drawing_nominal_width=x1 +. dist_r;
                 drawing_max_width=x1 +. dist_r;
                 drawing_y0=y0;
                 drawing_y1=y1;
                 drawing_badness=(fun _->0.);
                 drawing_contents=(fun _-> box_op)}]
              @(if right = [] then [] else [Drawing {
                 drawing_min_width=x1_r;
                 drawing_nominal_width=x1_r;
                 drawing_max_width=x1_r;
                 drawing_y0=y0_r;
                 drawing_y1=y1_r;
                 drawing_badness=(fun _->0.);
                 drawing_contents=(fun _-> right)}])
              @(draw env_stack s)
        )
      | Decoration (rebox, inside) :: s ->(
          (rebox env style ((draw env_stack inside))) @ (draw env_stack s)
        )


let dist_boxes env precise a b=
  let left=draw_boxes env a in
  let bezier_left=bezier_of_boxes left in
  let right=draw_boxes env b in
  let bezier_right=bezier_of_boxes right in
  let (x0_r,y0_r,x1_r,y1_r)=bounding_box right in
  let (x0_l,y0_l,x1_l,y1_l)=bounding_box left in
  let lr = List.map (fun (x,y)->Array.map (fun x0->x0+.x1_l-.x0_r) x, y) bezier_right in
  min_dist_left_right precise bezier_left lr


let glyphs (c:string) envs st=
  let env=env_style envs.mathsEnvironment st in
  let font=Lazy.force env.mathsFont in
  let s=env.mathsSize*.envs.size in
  let rec make_it idx=
    if UTF8.out_of_range c idx then [] else (
      { glyph_utf8=UTF8.init 1 (fun _->UTF8.look c idx);
        glyph_index=Fonts.glyph_of_uchar font (UTF8.look c idx) } :: make_it (UTF8.next c idx)
    )
  in
    List.map (fun gl->GlyphBox { (glyphCache font gl) with glyph_color=envs.fontColor; glyph_size=s}) (env.mathsSubst (make_it (UTF8.first c)))

let multi_glyphs cs envs st =
  List.map (fun c -> c envs st) cs

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
    List.map (fun gl->GlyphBox { (glyphCache font gl) with glyph_size=s; glyph_color=envs.fontColor}) gls


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
  let mid=draw_boxes env_ box in
  let (x0,y0,x1,y1)=bounding_box_kerning mid in
  let (x0',y0',x1',y1')=bounding_box_full mid in

  let (left, (x0_l',y0_l',x1_l',y1_l')), (right, (x0_r',y0_r',x1_r',y1_r')) =
    let ll = left env_ style and lr = right env_ style in
    let boxes = List.map (fun d ->
      let d=draw_boxes env_ d in
      (d, bounding_box_full d))
    in
    let ll = boxes ll in
    let lr = boxes lr in

    let rec select_size = function
      | [] -> [], (0.0, 0.0, 0.0, 0.0)
      | [c] -> c
      | (d, (x0',y0',x1',y1')) as c :: l ->
	(*FIXME: 1.05 coef should be in MathEnvs *)
	if y1 -. y0 <= (y1' -. y0') *. 1.05 then c else select_size l
    in
    select_size ll, select_size lr
  in
  
  let (x0_l,y0_l,x1_l,y1_l)=bounding_box_kerning left in
  let (x0_r,y0_r,x1_r,y1_r)=bounding_box_kerning right in

  let vertical_translation = max 0.0 (((y0_l' +. y0_r') /. 2.0 -. y0) /. 2.0) in
  let left = List.map (translate 0.0 (-.vertical_translation)) left in
  let right = List.map (translate 0.0 (-.vertical_translation)) right in

  let dist0 = if left = [] then 0.0 else 
      adjust_space env (s*.env.open_dist) (-.x1_l+.x0_l) left mid in
  let dist1 = if right = [] then 0.0 else
      adjust_space env (s*.env.close_dist) (-.x1_r+.x0_r) mid right in

  if !debug_kerning then begin
    Printf.printf "scale: %f\n" s;
    Printf.printf "boxes: (%f,%f) (%f,%f) (%f,%f)\n"
      x0_l x1_l x0 x1 x0_r x1_r;
    Printf.printf "full: (%f,%f) (%f,%f) (%f,%f)\n"
      x0_l' x1_l' x0' x1' x0_r' x1_r';
  end;

    (if left = [] then  [] else  [Drawing {
       drawing_min_width=x1_l +. dist0;
       drawing_nominal_width=x1_l +.dist0;
       drawing_max_width=x1_l +. dist0;
       drawing_y0=y0_l'-.vertical_translation;
       drawing_y1=y1_l'-.vertical_translation;
       drawing_badness=(fun _->0.);
       drawing_contents=(fun _-> left) (*  @ [(Path ({ default with lineWidth = 0.01 ; close = true},  
        [rectangle (x0_l,y0_l) (x1_l,y1_l)]))] *) }])@
    (Drawing {
       drawing_min_width=x1 +. dist1;
       drawing_nominal_width=x1 +. dist1;
       drawing_max_width=x1 +. dist1;
       drawing_y0=y0;
       drawing_y1=y1;
       drawing_badness=(fun _->0.);
       drawing_contents=(fun _-> mid);})::
    (if right = [] then  [] else
     [Drawing {
         drawing_min_width=x1_r;
         drawing_nominal_width=x1_r;
         drawing_max_width=x1_r;
         drawing_y0=y0_r'-.vertical_translation;
         drawing_y1=y1_r'-.vertical_translation;
         drawing_badness=(fun _->0.);
         drawing_contents=(fun _->  right);}])


let sqrts=ref [||]

let make_sqrt env_ style box=
  let env=env_style env_.mathsEnvironment style in
  let s=env.mathsSize*.env_.size in
  let under=draw_boxes env_ box in
  let (bx0,by0,bx1,by1)=bounding_box under in

  let f=Fonts.loadFont (findFont "../Fonts/Euler/euler.otf") in
  sqrts:=Array.map (fun x->Fonts.loadGlyph f { glyph_utf8="\\sqrt";glyph_index=x })
    [|693;694;695;696;697|];
  let h=by1 -. by0 +. (phi)*.env.sqrt_dist*.s in
  let glyphs= ! sqrts in
  let rec select i=
    if i>=Array.length glyphs-1 then i else (
      let h0=(Fonts.glyph_y1 glyphs.(i)-.Fonts.glyph_y0 glyphs.(i))*.s/.1000. in
      let h1=(Fonts.glyph_y1 glyphs.(i+1)-.Fonts.glyph_y0 glyphs.(i+1))*.s/.1000. in
      if h>h1 then
        select (i+1)
      else (
        if h>=(h1+.h0)/.3. then i+1 else i
      )
    )
  in
  let gl=glyphs.(select 0) in
  let out=Fonts.outlines gl in
  let out=
    Array.of_list
      (List.map (fun (a,b)->
                   Array.map (fun aa->aa*.s/.1000.) a,
                   Array.map (fun aa->aa*.s/.1000.) b) (List.hd out))
  in
  let i0=ref (-1) in (* courbe qui bouche la patte qui monte *)
  let i1=ref (-1) in (* courbe à droite de i0 *)
  let i2=ref (-1) in (* courbe à gauche de i0 *)
  let y0=ref (-.infinity) in
  let y1=ref (-.infinity) in

  for i=0 to Array.length out - 1 do
    let x,y=out.(i) in
    (* Courbe la plus à droite *)
    let yy0,yy1=
      if y.(0)<y.(Array.length y-1) then
        y.(0),y.(Array.length y-1)
      else if y.(0)>y.(Array.length y-1) then
        y.(Array.length y-1),y.(0)
      else
        if x.(0)>x.(Array.length x-1) then
          y.(Array.length y-1),y.(0)
        else
          y.(0),y.(Array.length x-1)
    in
    if yy0 >= !y0 && yy1 >= !y1 then (
      y0:=yy0;y1:=yy1;i0:=i;
      if x.(0)<x.(Array.length x-1) then
        (i1:=if i<Array.length out-1 then i+1 else 0;
         i2:=if i>=1 then i-1 else Array.length out-1)
      else
        (i1:=if i>=1 then i-1 else Array.length out-1;
         i2:=if i<Array.length out-1 then i+1 else 0)
    )
  done;

  let dx,dy=out.(!i0) in
  let dx0,dy0,dx1,dy1=if dx.(0)>dx.(Array.length dx-1) then
    dx.(0),dy.(0),dx.(Array.length dx-1),dy.(Array.length dy-1)
  else
    dx.(Array.length dx-1),dy.(Array.length dy-1),dx.(0),dy.(0)
  in
  let dd=sqrt ((dx1-.dx0)*.(dx1-.dx0)+.
                 (dy1-.dy0)*.(dy1-.dy0))
  in
  let p=
    if by1-.by0 <= (Fonts.glyph_y1 gl-.Fonts.glyph_y0 gl)*.s/.1000. then (
      (* Il faut raccourcir *)
      let shorten i y=
        let rx,ry=out.(i) in
        let ry'=Array.make (Array.length ry) 0. in
        for j=0 to Array.length ry'-1 do
          ry'.(j)<-ry.(j) -. y
        done;
        match (Bezier.bernstein_solve ry' 1e-5) with
            tt::_->(
              if ry.(0)<=ry.(1) then
                out.(i)<-(Bezier.restrict rx 0. tt, Bezier.restrict ry 0. tt)
              else
                out.(i)<-(Bezier.restrict rx tt 1., Bezier.restrict ry tt 1.)
            )
          | _->()
      in
      let y0'=by1-.by0+.env.sqrt_dist*.s*.(phi) in
      let y1'=y0'+.min (s/.30.) dd in
      shorten !i1 y0';
      shorten !i2 y1';
      let ddx0,ddy0=out.(!i1) in
      let ddx1,ddy1=out.(!i2) in
      let dx0,dx1=
        let dx0=
          if ddy0.(0)>ddy0.(Array.length ddy0-1) then
            ddx0.(0)
          else
            ddx0.(Array.length ddx0-1)
        in
        let dx1=
          if ddy1.(0)>ddy1.(Array.length ddy1-1) then
            ddx1.(0)
          else
            ddx1.(Array.length ddx1-1)
        in
        if dx0<dx1 then (dx1,dx0) else (dx0,dx1)
      in
      let xmax=max (bx1-.bx0) (Fonts.glyphWidth gl*.s/.1000.)/.phi in
      let path0=Array.sub out 0 !i0
      and path1=Array.sub out (!i0+1) (Array.length out- !i0-1) in
      let path2=
        [|[|dx0;dx0+.xmax|],[|y0';y0'|];
          [|dx0+.xmax;dx0+.xmax|],[|y0';y1'|];
          [|dx0+.xmax;dx1|],[|y1';y1'|];
        |]
      in
      let path=Array.concat [path0; path2; path1] in
      let tx=(xmax-.(bx1-.bx0))/.2. +. dx0 in
      let ty=by0-.env.sqrt_dist*.(phi-.1.) in
      let p=
        translate 0. ty
          (Path ({default with strokingColor=None;fillColor=Some black},
                 [path]
                ))
      in
      let (a,b,c,d)=bounding_box [p] in
      [Drawing {
         drawing_min_width=c-.a;
         drawing_nominal_width=c-.a;
         drawing_max_width=c-.a;
         drawing_y0=b;
         drawing_y1=d;
         drawing_badness=(fun _->0.);
         drawing_contents=
           (fun _->p::
              (List.map (translate tx 0.) under))
       }]
    ) else (
      (* Il faut rallonger *)
      let vx=dy1-.dy0
      and vy= -.(dx1-.dx0) in
      let tt=(max 0. (env.sqrt_dist*.s*.phi-.by0+.by1
                      +.Fonts.glyph_y0 gl*.s/.1000.
                      -.Fonts.glyph_y1 gl*.s/.1000.))/.vy in
      let tt'= tt+. ((dd-. (dy1-.dy0))/.(vy*.phi)) in
      let xmax=bx1-.bx0 in
      let path0=Array.sub out 0 !i0
      and path1=Array.sub out (!i0+1) (Array.length out- !i0-1) in
      let path2=
        [|[|dx0;dx0+.tt*.vx|], [|dy0;dy0+.tt*.vy|];
          [|dx0+.tt*.vx+.xmax;dx0+.tt*.vx+.xmax|],[|dy0+.tt*.vy;dy0+.tt*.vy|];
          [|dx0+.tt*.vx+.xmax;dx0+.tt*.vx+.xmax|],[|dy0+.tt*.vy;dy1+.tt'*.vy|];
          [|dx0+.tt*.vx+.xmax;dx1+.tt'*.vx|], [|dy1+.tt'*.vy;dy1+.tt'*.vy|];
          [|dx1+.tt'*.vx; dx1|], [|dy1+.tt'*.vy; dy1|]
        |]
      in
      let path=Array.concat [path0; path2; path1] in
      let tx=(xmax-.(bx1-.bx0))/.2. +. dx0 +. vx*.tt in
      let ty=by0-.env.sqrt_dist*.(phi-.1.) in
      let p=
        translate 0. ty
          (Path ({default with strokingColor=None;fillColor=Some black},
                 [path]
                ))
      in
      let (a,b,c,d)=bounding_box [p] in
      [Drawing {
         drawing_min_width=c-.a;
         drawing_nominal_width=c-.a;
         drawing_max_width=c-.a;
         drawing_y0=b;
         drawing_y1=d;
         drawing_badness=(fun _->0.);
         drawing_contents=
           (fun _->p::
              (List.map (translate tx 0.) under))
       }]
    )
  in
  p


let sqrt x=[Decoration (make_sqrt,x)]
