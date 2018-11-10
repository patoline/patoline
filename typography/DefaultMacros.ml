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

open Unicodelib
open Patutil
open Patoraw
open Patfonts
open Document
open Complete
open Break
open FTypes
open Extra
open Fonts
open Box
open Patconfig.PatConfig

let findFont = ConfigFindFont.findFont

(* Symboles et polices de maths *)


let mathsText t0=
  [Maths.Ordinary (Maths.node (fun env st->boxify_scoped
                                             { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
                                             t0 ))]
let mathcal a=[Maths.Scope(fun _ _-> Maths.Env (Euler.changeFont [Euler.Font `Cal]):: a)]
let cal a=mathcal a
let fraktur a=[Maths.Scope(fun _ _-> Maths.Env (Euler.changeFont [Euler.Font `Fraktur]) :: a)]
let mathbf a=[Maths.Scope(fun _ _-> Maths.Env (fun env -> Euler.changeFont [Euler.Graisse `Gras] (envAlternative Bold env)) :: a)]
let mathsc a=
  [Maths.Scope(fun _ _->
       Maths.Env (fun env->envAlternative Caps env)::
         Maths.Env (fun env->Maths.change_fonts env env.font)::
           a
  )]

let bbFont=Lazy.from_fun (fun ()->Fonts.loadFont
                                    (findFont FontPattern.({family = "Euler"; slant = Roman; weight = Regular})))

let mathbb a=[Maths.Scope (fun _ _->Maths.Env (fun env->Maths.change_fonts
                                                          (change_font (Lazy.force bbFont) env) (Lazy.force bbFont))::a)]

let mathrm a=[Maths.Scope(
                  fun _ _->Maths.Env (fun env->Maths.change_fonts env env.font)::a
             )]

let displayStyle a=[Maths.Scope(
                        fun _ _->Maths.Env (fun env-> { env with mathStyle
                                                                 = Mathematical.Display })::a
                   )]

let setStyle f a=[Maths.Scope(
                      fun _ _->Maths.Env (fun env-> List.hd(f env.mathStyle [env])):: a)]

let mathsize alpha a=[Maths.Scope(
                          fun _ _->Maths.Env (fun env-> { env with size=alpha })::a
                     )]

let mathSpace x =
  [Maths.Scope (fun env style ->
       let mathsEnv=Maths.env_style env.mathsEnvironment style in
       let x = x *. mathsEnv.Mathematical.mathsSize *. env.size in
       [Maths.Glue { drawing_min_width= x;
                     drawing_max_width= x;
                     drawing_y0=infinity; drawing_y1= -.infinity;
                     drawing_nominal_width= x;
                     drawing_width_fixed = true;
                     drawing_adjust_before = false;
                     drawing_contents=(fun _->[]);
                     drawing_states=[];
                     drawing_break_badness=0.;
                     drawing_badness=knuth_h_badness x }])]

let oline a=
  [Maths.Ordinary
     (Maths.node
        (fun envs st->
          let dr=draw_boxes envs (Maths.draw [envs] a) in
          let env=Maths.env_style envs.mathsEnvironment st in
          let (x0,y0,x1,y1)=RawContent.bounding_box_full dr in
          let drawn=(drawing ~offset:y0 dr) in
          let rul=(env.Mathematical.default_rule_thickness)*.env.Mathematical.mathsSize*.envs.size in
          [Drawing {
               drawn with
               drawing_y1=drawn.drawing_y1*.sqrt phi+.rul;
               drawing_contents=
                 (fun w->
                   RawContent.Path ({RawContent.default_path_param with
                                      RawContent.fillColor=Some envs.fontColor;
                                      RawContent.strokingColor=Some envs.fontColor;
                                      RawContent.lineWidth=rul},
                                    [[|[|x0;x1|],
                                       [|y1*.sqrt phi+.2.*.rul;y1*.sqrt phi+.2.*.rul|]|]])
                   ::drawn.drawing_contents w)
          }]
  ))]

let odiag a=
  [Maths.Ordinary
     (Maths.node
        (fun envs st->
          let dr=draw_boxes envs (Maths.draw [envs] a) in
          let env=Maths.env_style envs.mathsEnvironment st in
          let (x0,y0,x1,y1)=RawContent.bounding_box_full dr in
          let drawn=(drawing ~offset:y0 dr) in
          let rul=(env.Mathematical.default_rule_thickness)*.env.Mathematical.mathsSize*.envs.size in
          [Drawing {
               drawn with
               drawing_y1=drawn.drawing_y1*.sqrt phi+.rul;
               drawing_contents=
                 (fun w->
                   RawContent.Path ({RawContent.default_path_param with
                                      RawContent.fillColor=Some envs.fontColor;
                                      RawContent.strokingColor=Some envs.fontColor;
                                      RawContent.lineWidth=rul},
                                    [[|[|x0;x1|],[|y0;y1|]|]])
                   ::drawn.drawing_contents w)
          }]
  ))]

let mnot a=
  [Maths.Ordinary
     (Maths.node
        (fun envs st->
          let dr=draw_boxes envs (Maths.draw [envs] a) in
          let env=Maths.env_style envs.mathsEnvironment st in
          let (x0,y0,x1,y1)=RawContent.bounding_box_full dr in
          let drawn=(drawing ~offset:y0 dr) in
          let rul=(env.Mathematical.default_rule_thickness)*.env.Mathematical.mathsSize*.envs.size in
          let xm=(x1+.x0)/.2. in
          let y=(y1-.y0)*.phi/.4. in
          [Drawing {
               drawn with
               drawing_y0=drawn.drawing_y0-.y;
               drawing_y1=drawn.drawing_y1+.y;
               drawing_contents=
                 (fun w->
                   RawContent.Path ({RawContent.default_path_param with
                                      RawContent.fillColor=Some envs.fontColor;
                                      RawContent.strokingColor=Some envs.fontColor;
                                      RawContent.lineWidth=rul},
                                    [[|
                                        [|xm-.w/.10.;xm+.w/.10.|],
                                        [|y0-.y;y1+.y|]
                                      |]])
                   ::drawn.drawing_contents w)
          }]
  ))]


let oDot a =
  [Maths.Ordinary
     (Maths.node
        (fun envs st ->
          let env = Maths.env_style envs.mathsEnvironment st in
          let font = Lazy.force (env.Mathematical.mathsFont) in
          (* Find the dot glyph *)
          let utf8_dot = { glyph_index = (Fonts.glyph_of_uchar font (UChar.chr 0x22c5));
                           glyph_utf8 = "."} in
          let gl_dot = Fonts.loadGlyph font utf8_dot in
          let dot_y0 = (Fonts.glyph_y0 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          let dot_x0 = (Fonts.glyph_x0 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          let dot_x1 = (Fonts.glyph_x1 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          (* Bounding box of a *)
          let dr = draw_boxes envs (Maths.draw [envs] a) in
          let (x0,y0,x1,y1) = RawContent.bounding_box_full dr in

          let drawn=(drawing ~offset:y0 dr) in
          let rul=(env.Mathematical.default_rule_thickness)*.env.Mathematical.mathsSize*.envs.size in

          [Drawing {
               drawn with
               drawing_y1=drawn.drawing_y1*.sqrt phi+.rul;
               drawing_contents= (fun w ->
                 RawContent.Glyph {
                     RawContent.glyph_x = (x0 +. x1) /. 2. -. (dot_x1 +. dot_x0) /. 2.;
                     RawContent.glyph_kx = 0.;
                     RawContent.glyph_y = y1-.dot_y0 +. 2. *. rul;
                     RawContent.glyph_ky = 0.;
                     RawContent.glyph_order = 1;
                     RawContent.glyph_color = envs.fontColor;
                     RawContent.glyph_size = envs.size *. env.Mathematical.mathsSize;
                     RawContent.glyph = gl_dot
                   }::drawn.drawing_contents w
               )
          }]
        )
     )
  ]

let oDoubleDot a =
  [Maths.Ordinary
     (Maths.node
        (fun envs st ->
          let env = Maths.env_style envs.mathsEnvironment st in
          let font = Lazy.force (env.Mathematical.mathsFont) in
          (* Find the dot glyph *)
          let utf8_dot = { glyph_index = (Fonts.glyph_of_uchar font (UChar.chr 0x22c5));
                           glyph_utf8 = "."} in
          let gl_dot = Fonts.loadGlyph font utf8_dot in
          let dot_y0 = (Fonts.glyph_y0 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          let dot_x0 = (Fonts.glyph_x0 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          let dot_x1 = (Fonts.glyph_x1 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          let dot_r = (dot_x1 -. dot_x0) /. 2. in
          (* Bounding box of a *)
          let dr = draw_boxes envs (Maths.draw [envs] a) in
          let (x0,y0,x1,y1) = RawContent.bounding_box_full dr in

          let drawn=(drawing ~offset:y0 dr) in
          let rul=(env.Mathematical.default_rule_thickness)*.env.Mathematical.mathsSize*.envs.size in

          [Drawing {
               drawn with
               drawing_y1=drawn.drawing_y1*.sqrt phi+.rul;
               drawing_contents= (fun w ->
                 RawContent.Glyph {
                     RawContent.glyph_x = (x0 +. x1) /. 2. -. (dot_x1 +. dot_x0) /. 2. -. dot_r -. rul /. 2.;
                     RawContent.glyph_kx = 0.;
                     RawContent.glyph_y = y1-.dot_y0 +. 2. *. rul;
                     RawContent.glyph_ky = 0.;
                     RawContent.glyph_order = 1;
                     RawContent.glyph_color = envs.fontColor;
                     RawContent.glyph_size = envs.size *. env.Mathematical.mathsSize;
                     RawContent.glyph = gl_dot
                   }::RawContent.Glyph {
                          RawContent.glyph_x = (x0 +. x1) /. 2. -. (dot_x1 +. dot_x0) /. 2. +. dot_r +. rul /. 2.;
                          RawContent.glyph_kx = 0.;
                          RawContent.glyph_y = y1-.dot_y0 +. 2. *. rul;
                          RawContent.glyph_ky = 0.;
                          RawContent.glyph_order = 1;
                          RawContent.glyph_color = envs.fontColor;
                          RawContent.glyph_size = envs.size *. env.Mathematical.mathsSize;
                          RawContent.glyph = gl_dot
                        }::drawn.drawing_contents w
               )
          }]
        )
     )
  ]

let oHat a =
  [Maths.Ordinary
     (Maths.node
        (fun envs st ->
          let env = Maths.env_style envs.mathsEnvironment st in
          let font = Lazy.force (env.Mathematical.mathsFont) in
          (* Find the dot glyph *)
          let utf8_dot = { glyph_index = (Fonts.glyph_of_uchar font (UChar.chr 0x005e));
                           glyph_utf8 = "^"} in
          let gl_dot = Fonts.loadGlyph font utf8_dot in
          let dot_y0 = (Fonts.glyph_y0 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          let dot_x0 = (Fonts.glyph_x0 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          let dot_x1 = (Fonts.glyph_x1 gl_dot) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
          (* Bounding box of a *)
          let dr = draw_boxes envs (Maths.draw [envs] a) in
          let (x0,y0,x1,y1) = RawContent.bounding_box_full dr in

          let drawn=(drawing ~offset:y0 dr) in
          let rul=(env.Mathematical.default_rule_thickness)*.env.Mathematical.mathsSize*.envs.size in

          [Drawing {
               drawn with
               drawing_y1=drawn.drawing_y1*.sqrt phi+.rul;
               drawing_contents= (fun w ->
                 RawContent.Glyph {
                     RawContent.glyph_x = (x0 +. x1) /. 2. -. (dot_x1 +. dot_x0) /. 2.;
                     RawContent.glyph_kx = 0.;
                     RawContent.glyph_y = y1-.dot_y0 +. 2. *. rul;
                     RawContent.glyph_ky = 0.;
                     RawContent.glyph_order = 1;
                     RawContent.glyph_color = envs.fontColor;
                     RawContent.glyph_size = envs.size *. env.Mathematical.mathsSize;
                     RawContent.glyph = gl_dot
                   }::drawn.drawing_contents w
               )
          }]
        )
     )
  ]

let binomial a b=
  [Maths.Fraction { Maths.numerator=b; Maths.denominator=a;
                    Maths.line=(fun _ _->{RawContent.default_path_param with RawContent.fillColor=None;RawContent.strokingColor=None}) }]

(* Une chirurgie esthétique de glyphs. Ce n'est sans doute pas très
       bien fait, et il faut kerner en haut. Un truc generique pour
       allonger toutes les flêches est à réfléchir *)

let oRightArrow a=
  [Maths.Ordinary
     (Maths.node
        (fun envs st->
          let boxes=(Maths.draw [envs] a) in
          let boxes_w=
            (List.fold_left (fun w x->
                 let _,w_x,_=box_interval x in
                 w+.w_x) 0. boxes)
          in
          let dr=draw_boxes envs boxes in
          let (x0_,y0_,x1_,y1_)=RawContent.bounding_box_full dr in

          let env=Maths.env_style envs.mathsEnvironment st in
          let font=Lazy.force (env.Mathematical.mathsFont) in
          (* Extraction de la fleche native de la police *)
          let utf8_arr={glyph_index=(Fonts.glyph_of_uchar font (UChar.chr 0x2192));
                        glyph_utf8="\033\146"} in
          let gl_arr=Fonts.loadGlyph font utf8_arr in
          let arr=Fonts.outlines gl_arr in
          (* arr: (float array * float array) list list is a list of lists of pairs of arrays of floats *)
          (* w1 is the max of all x-coordinates in there, assuming the involved arrays are non-decreasing *)
          let w1=List.fold_left (List.fold_left (fun y (v,_)->max y (max v.(0) v.(Array.length v-1)))) 0. arr in
          (* Extrema de la fleche en ordonnee *)
          let y0,y1=List.fold_left (List.fold_left (fun (yy0,yy1) (_,v)->
                                        let a,b=Bezier.bernstein_extr v in
                                        min yy0 a, max yy1 b)) (0.,0.) arr in
          let size=envs.size*.env.Mathematical.mathsSize/.(1000.*.phi) in
          let x_space0=envs.size/.12.
          and x_space1=envs.size/.16.
          and y_space=env.Mathematical.default_rule_thickness*.envs.size*.2. in

          let arr'=
            List.map (fun x->
                Array.of_list (List.map (fun (u,v)->
                                   Array.map (fun y->if y>=w1/.4. then (y*.size)+.(max 0. (x1_-.w1*.size)+.x_space1) else y*.size-.x_space0) u,
                                   Array.map (fun y->y*.size-.y0+.y1_+.y_space) v
                                 ) x)) arr
          in
          [Drawing {
               drawing_nominal_width=max (w1*.size) boxes_w;
               drawing_min_width=max (w1*.size+.y_space) boxes_w;
               drawing_max_width=max (w1*.size+.y_space) boxes_w;
               drawing_width_fixed = true;
               drawing_adjust_before = false;
               drawing_y0=y0_;
               drawing_y1=y1_+.x_space1-.x_space0-.(y0+.y1)*.size;
               drawing_badness=(fun _->0.);
               drawing_break_badness=0.;
               drawing_states=[];
               drawing_contents=
                 (fun w->
                   RawContent.Path ({RawContent.default_path_param with
                                      RawContent.strokingColor=None;
                                      RawContent.fillColor=Some envs.fontColor
                                    },arr')
                   ::(List.map (RawContent.translate (max 0. ((w1*.size-.x1_)/.2.)) 0.) dr))
          }]
  ))]

let oLeftArrow a=
  [Maths.Ordinary
     (Maths.node
        (fun envs st->
          let boxes=(Maths.draw [envs] a) in
          let boxes_w=
            (List.fold_left (fun w x->
                 let _,w_x,_=box_interval x in
                 w+.w_x) 0. boxes)
          in
          let dr=draw_boxes envs boxes in
          let (x0_,y0_,x1_,y1_)=RawContent.bounding_box_full dr in

          let env=Maths.env_style envs.mathsEnvironment st in
          let font=Lazy.force (env.Mathematical.mathsFont) in
          let utf8_arr={glyph_index=(Fonts.glyph_of_uchar font (UChar.chr 0x2190));
                        glyph_utf8="\033\144"} in
          let gl_arr=Fonts.loadGlyph font utf8_arr in
          let arr=Fonts.outlines gl_arr in
          let w1=List.fold_left (List.fold_left (fun y (v,_)->max y (max v.(0) v.(Array.length v-1)))) 0. arr in
          let y0,y1=List.fold_left (List.fold_left (fun (yy0,yy1) (_,v)->
                                        let a,b=Bezier.bernstein_extr v in
                                        min yy0 a, max yy1 b)) (0.,0.) arr in
          let size=envs.size*.env.Mathematical.mathsSize/.(1000.*.phi) in
          let space=env.Mathematical.default_rule_thickness in
          let arr'=
            List.map (fun x->
                Array.of_list (List.map (fun (u,v)->
                                   Array.map (fun y->if y>=w1*.0.75 then (y*.size)+.(max 0. (x1_-.w1*.size)) else y*.size) u,
                                   Array.map (fun y->y*.size-.y0+.y1_+.space) v
                                 ) x)) arr
          in
          [Drawing {
               drawing_nominal_width=max (w1*.size) boxes_w;
               drawing_min_width=max (w1*.size) boxes_w;
               drawing_max_width=max (w1*.size) boxes_w;
               drawing_width_fixed = true;
               drawing_adjust_before = false;
               drawing_y0=y0_;
               drawing_y1=y1_+.space-.(y0+.y1)*.size;
               drawing_badness=(fun _->0.);
               drawing_break_badness=0.;
               drawing_states=[];
               drawing_contents=
                 (fun w->
                   RawContent.Path ({RawContent.default_path_param with
                                      RawContent.strokingColor=None;
                                      RawContent.fillColor=Some envs.fontColor
                                    },arr')
                   ::(List.map (RawContent.translate (max 0. ((w1*.size-.x1_)/.2.)) 0.) dr))
          }]
  ))]

let vec = oRightArrow

let cev = oLeftArrow

(*******************************************************)


let od = oDot

let odd = oDoubleDot

let hat = oHat

(* half of the equal sign, is a good centering line *)
let half_eq envs st =
  let open FTypes in
  let env = Maths.env_style envs.mathsEnvironment st in
  let font = Lazy.force (env.Mathematical.mathsFont) in
  (* Find the = glyph *)
  let utf8 = { glyph_index = (Fonts.glyph_of_uchar font (UChar.chr 0x003d));
               glyph_utf8 = "="} in
  let gl = Fonts.loadGlyph font utf8 in
  let dot_y1 = (Fonts.glyph_y1 gl) *. envs.size *. env.Mathematical.mathsSize /. 1000. in
  dot_y1 /. 2.

let matrix ?(extra=fun _ _ _ -> ()) a =
  [Maths.Ordinary
     (Maths.node
        (fun env st->
          let open Diagrams in
          let module Fig = MakeDiagram (struct let env = env end) in
          let open Fig in
          let max_len = List.fold_left (fun acc l -> max acc (List.length l)) 0 a in
          let enlarge default =
            let rec fn acc n l = match n, l with
              | 0, [] -> List.rev acc
              | 0, _::_ -> assert false
              | n, [] -> fn (default::acc) (n-1) []
              | n, x::l -> fn (x::acc) (n-1) l
            in
            fn [] max_len
          in
          let a = List.map (enlarge []) a in
          let anchor = enlarge `Base [] in
          let a = List.map (fun l -> List.map Maths.(setStyle matrixStyle) l) a in
          let m, ms = array
                        ~all_node_styles:[Matrix.allNodes Node.[innerSep 0.]]
                        ~main_node_style:Node.[innerSep 0.;at (0.,0.);anchor `SouthWest]
                        anchor a
          in
          let _ = extra (module Fig:Diagram) m ms in
          [ Drawing (Fig.make ~adjust_before:true ~vcenter:true ~width_fixed:false
                              ~offset:(half_eq env st) ())]))]

let ematrix m extra = matrix ~extra m

let colomnMatrix ?(extra=fun _ _ _ -> ()) c = matrix ~extra (List.map (fun x -> [x]) c)
let lineMatrix   ?(extra=fun _ _ _ -> ()) l = matrix ~extra [l]

let caml x = x
