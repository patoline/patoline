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
open Typography.Document
open Typography.OutputCommon
open Util
open Typography.Box
open Typography
open DefaultFormat
module Plot(D:Document.DocumentStructure)=struct

  let colors i n=
    let col=
      [| rgb 1. 0. 0.;
         rgb 0. 1. 0.;
         rgb 0. 0. 1.
      |]
    in
    if n<=Array.length col then col.(i) else
      let x=float_of_int (Array.length col) *. float_of_int i/.float_of_int n in
      let j=int_of_float x in
      let prop=x-.floor x in
      mix (1.-.prop) col.(j) col.((j+1) mod Array.length col)


  let line (a,b) (c,d)=[|a;c|],[|b;d|]
  let plot points=
    [bB (fun env->
      let minx,maxx,miny,maxy=
        let minx=ref 0. in
        let maxx=ref 0. in
        let miny=ref 0. in
        let maxy=ref 0. in
        List.iter (fun (_,l)->List.iter (fun (x,y)->
          minx:= min x !minx;
          miny:= min y !miny;
          maxx:= max x !maxx;
          maxy:= max y !maxy;
        ) l) points;
        floor !minx, ceil !maxx,
        floor !miny, ceil !maxy
      in
      let ratio=4./.3. in
      let scalex= ((env.normalMeasure/.phi)/.(maxx-.minx))
      and scaley= ((env.normalMeasure/.(ratio*.phi))/.(maxy-.miny))
      in
      let tx=ref [] in
      let ty=ref [] in
      if floor (maxx-.minx) < 1. || List.length points > (int_of_float (floor(maxx-.minx)*.5.))
      then (
    (* Graduation x plus petite *)
      ) else (
        let nx=min 10. (ceil (maxx-.minx)) in
        let inter=floor ((maxx-.minx)/.nx) in
        let inter=if inter>20. then 10.*.max 1. (floor (inter/.10.)) else inter in
        let x=ref 0. in
        while !x <= maxx-.minx do
          let label=draw_boxes env
            (Maths.kdraw
               [ { env with mathStyle = Document.Mathematical.Script } ]
               ([Maths.Ordinary
                    (Maths.noad
                       ((Maths.glyphs
                           (Printf.sprintf "%g" (minx+. !x)
                           )))) ])
            )
          in
          let x0,y0,x1,y1=bounding_box label in
          tx:=Path(default,
                   [[|line (scalex*. !x,0.) (scalex*. !x,-.1.)|]])
          ::(List.map (OutputCommon.translate (scalex*. !x-.(x1-.x0)/.2.) (-.2.-.y1) ) label)
          @(!tx);

          x:= !x+.inter
        done;
        tx:=Path(default,[[|line (0.,0.) (scalex*. (!x-.inter/.2.),0.)|]]):: !tx
      );
      if floor (maxy-.miny) < 1. || List.length points > (int_of_float (floor(maxy-.miny)*.5.))
      then (
    (* Graduation y plus petite *)
      ) else (
        let ny=min 10. (ceil (maxy-.miny)) in
        let inter=floor ((maxy-.miny)/.ny) in
        let inter=if inter>20. then 10.*.max 1. (floor (inter/.10.)) else inter in
        let y=ref 0. in
        while !y <= maxy-.miny do
          let label=draw_boxes env
            (Maths.kdraw
               [ { env with mathStyle = Document.Mathematical.Script } ]
               ([Maths.Ordinary
                    (Maths.noad
                       ((Maths.glyphs
                           (Printf.sprintf "%g" (miny+. !y)
                           )))) ])
            )
          in
          let x0,y0,x1,y1=bounding_box label in
          ty:=Path(default,
                   [[|line (0.,scaley*. !y) (-.1.,scaley*. !y)|]])
          ::(List.map (OutputCommon.translate (-.2.-.x1) (scaley*. !y-.(y1-.y0)/.2.)) label)
          @(!ty);

          y:= !y+.inter
        done;
        ty:=Path(default,[[|line (0.,0.) (0.,scaley*. (!y-.inter/.2.))|]]):: !ty
      );

      let drawn_curves=ref [] in

      let rec draw_curve l p=match l with
          []
        | [_]->List.rev p
        | (x1,y1)::(x2,y2)::s->
          draw_curve ((x2,y2)::s) (line (scalex*.x1,scaley*.y1) (scalex*.x2,scaley*.y2)::p)
      in
      let croix param (x,y)=
        [Path (param,[[|line (scalex*.x-.1.,scaley*.y) (scalex*.x+.1.,scaley*.y) |]]);
         Path (param,[[|line (scalex*.x,scaley*.y-.1.) (scalex*.x,scaley*.y+.1.) |]])]
      in
      let i=ref 0 in
      List.iter (fun (_,cur)->
        let par={default with strokingColor=Some (colors !i (List.length points))} in
        drawn_curves:=
          List.concat (List.map (croix par) cur)@
          (Path (par,[Array.of_list (draw_curve cur [])]))::(!drawn_curves);
        incr i
      ) points;

      let dr=drawing (!tx @ !ty @ !drawn_curves) in
      [Drawing {dr with drawing_y0=dr.drawing_y0-.env.size;drawing_y1=dr.drawing_y1+.env.size}]
    )]

  module Def=DefaultFormat.Format(D)
  open Def
  let legende points=
    [bB (fun env->
      let fact=3./.4. in
      let boxes=List.map (fun (c,_)->boxify_scoped {env with size=env.size*.fact} c) points in
      let w=
        min (env.normalMeasure/.3.)
          (List.fold_left (fun w b->let _,w1,_=boxes_interval (Array.of_list b) in
                                    max w w1) 0. boxes)
      in
      let params={ widths=(fun _->[|w;0.|]); h_spacing=5.; v_spacing=0. } in
      let t=Def.table params
        (Array.of_list (
          let i=ref (-1) in
          List.map (fun b->
            [|paragraph [bB (fun _->b)];
              paragraph [bB (fun env->
                incr i;
                let l=Path ({default with strokingColor=Some (colors !i (List.length points))},
                            [[|line (0.,0.) (env.size*.4.,0.)|]]) in
                let xsize=env.size/.2. in
                [Drawing (drawing ~offset:(xsize/.2.) [l])]
              )]
            |]) boxes))
      in
      let dr=drawing (draw_boxes env (boxify_scoped env t)) in
      [Drawing {dr with drawing_y0=dr.drawing_y0-.env.size;drawing_y1=dr.drawing_y1+.env.size}]

    )]

  let read_curve f=
    let i=open_in f in
    let rec read_file x=
      let p=
        try
          let l=input_line i in
          let ii=String.index l ' ' in
          Some (float_of_string (String.sub l 0 ii),
                float_of_string (String.sub l (ii+1) (String.length l-ii-1)))
        with
            _->None
      in
      match p with
          None->List.rev x
        | Some y->read_file (y::x)
    in
    read_file []
end
