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
open Bezier

let inverse m=
  let det=m.(0)*.m.(3) -. m.(1)*.m.(2) in
    [|
      m.(3)/.det; -.m.(1)/.det;
      -.m.(2)/.det; m.(0)/.det
    |]

let eval fx fy t=
  let rec eval eps t0 t1=
    let fx'=Bezier.derivee fx in
    let fy'=Bezier.derivee fy in
    let x0=Bezier.eval fx t0 in
    let y0=Bezier.eval fy t0 in
    let x1=Bezier.eval fx t1 in
    let y1=Bezier.eval fy t1 in
    let x0'=Bezier.eval fx' t0 in
    let y0'=Bezier.eval fy' t0 in
    let x1'=Bezier.eval fx' t1 in
    let y1'=Bezier.eval fy' t1 in
    let norm0=sqrt (x0'*.x0'+.y0'*.y0') in
    let norm1=sqrt (x1'*.x1'+.y1'*.y1') in
      if norm0>=1e-5 && norm1>=1e-5 || t1-.t0 >= 1e-5 then
        let xx0=x0-.y0'/.norm0 in
        let yy0=y0+.x0'/.norm0 in
        let xx1=x1-.y1'/.norm1 in
        let yy1=y1+.x1'/.norm1 in
          (xx0,yy0),(xx1,yy1)
      else
        eval (eps*.2.) (t-.eps) (t+.eps)
  in
    eval (sqrt min_float) t t

let rev a=Array.init (Array.length a) (fun i->a.(Array.length a-i-1))

let example ()=
  (Array.init 4 (fun _->Random.float 10.),
   Array.init 4 (fun _->Random.float 10.))


let (+..) a b=Bezier.plus a b
let (-..) a b=Bezier.minus a b
let ( *..) a b=Bezier.times a b

let scale a b=Array.map (fun x->x*.a) b
let cut fx fy=
  let fx'=Bezier.derivee fx in
  let fy'=Bezier.derivee fy in
  let fx''=Bezier.derivee fx' in
  let fy''=Bezier.derivee fy' in
  let a=(fx'*..fx'+..fy'*..fy') in
  let bx=(fy'*..fx'*..fx'' -.. fy''*..fx'*..fx') in
  let by=(fx'*..fy'*..fy'' -.. fx''*..fy'*..fy') in
  let eq_x=(fx'*..fx')*..(a*..a*..a) -.. bx*..bx in
  let eq_y=(fy'*..fy')*..(a*..a*..a) -.. by*..by in
  let rec filt=function
      []->[]
    | [h]->[h]
    | h1::h2::s when h2-.h1 < 1e-20 -> filt (h1::s)
    | h1::s->h1::(filt s)
  in
  let l=filt (
    List.sort compare ((bernstein_solve eq_x 1e-3) @ (bernstein_solve eq_y 1e-3) @
                         (bernstein_solve fx' 1e-3) @ (bernstein_solve fy' 1e-3))
  )
  in
    (* List.iter (fun t->let (x,y),(_,_)=eval fx fy t in *)
    (*              Graphics.draw_circle (int_of_float (x*.50.)) (int_of_float (y*.50.)) 10 *)
    (*           ) l; *)
    l
(* deux courbes, et deux vecteurs de controle *)
let elem_approx gx gy (ux,uy) (vx,vy)=              (* Moindres carrés *)
  (* Equation sur lambda, le coefficient du premier vecteur de contrôle *)
  if abs_float (ux*.vy -. uy*.vx) > 1e-12 then (   (* Si la matrice de la fin est inversible *)
    let eq0=Array.make 3 0. in
    let eq1=Array.make 3 0. in
    let n=1000 in
      for i=0 to n do
        let t=float_of_int i/.float_of_int n in
          eq0.(0)<-eq0.(0) +. 3.*.ux*.(1.-.t)*.(1.-.t)*.t;
          eq0.(1)<-eq0.(1) +. 3.*.vx*.(1.-.t)*.t*.t;
          eq0.(2)<-eq0.(2) +.
            (gx t)-.(gx 1.)*.t*.t*.t-.(gx 0.)*.(1.-.t)*.(1.-.t)*.(1.-.t)
          -. 3.*.(1.-.t)*.t*. ((gx 0.)*.(1.-.t) +. (gx 1.)*.t);

          eq1.(0)<-eq1.(0) +. 3.*.uy*.(1.-.t)*.(1.-.t)*.t;
          eq1.(1)<-eq1.(1) +. 3.*.vy*.(1.-.t)*.t*.t;
          eq1.(2)<-eq1.(2) +.
            (gy t)-.(gy 1.)*.t*.t*.t-.(gy 0.)*.(1.-.t)*.(1.-.t)*.(1.-.t)
          -. 3.*.(1.-.t)*.t*. ((gy 0.)*.(1.-.t) +. (gy 1.)*.t);
      done;
      let m=[|eq0.(0);eq0.(1);eq1.(0);eq1.(1)|] in
      let m'=inverse m in
      let a=m'.(0)*.eq0.(2) +. m'.(1)*.eq1.(2) in
      let b=m'.(2)*.eq0.(2) +. m'.(3)*.eq1.(2) in
        ([|gx 0.; gx 0.+.a*.ux; gx 1. +. b*.vx; gx 1.|],
         [|gy 0.; gy 0.+.a*.uy; gy 1. +. b*.vy; gy 1.|])
  ) else (
    let res=[|gx 0.;gx 1.|],
      [|gy 0.;gy 1.|]
    in
      res
  )
let rec approx_rec fx fy t0 t1=
  let ux=fx.(1)-.fx.(0) in
  let uy=fy.(1)-.fy.(0) in
  let vx=fx.(Array.length fx-2)-.fx.(Array.length fx-1) in
  let vy=fy.(Array.length fy-2)-.fy.(Array.length fy-1) in
  let ax,ay=elem_approx
    (fun t->
       fst (if t<0.5 then snd (eval fx fy t)
            else fst (eval fx fy t)))
    (fun t->
       snd (if t<0.5 then snd (eval fx fy t)
            else fst (eval fx fy t)))
    (ux,uy) (vx,vy)
  in
  (* let bx=Array.copy ax in *)
  (* let by=Array.copy ay in *)
  let step=1e-2 in
  let rec dist t d xd=
    if t>=1.-.step then d,xd else (
      let (x,y),_=eval fx fy t in
      let xx=Bezier.eval fx t in
      let yy=Bezier.eval fy t in
        dist (t+.step) (d+.(xx-.x)*.(xx-.x)+.(yy-.y)*.(yy-.y)) xd
    )
  in
  let d,xd=dist step 0. 0. in
    if d<=step*.100. || t1-.t0<0.1 then (
      [ax,ay]
    ) else (
      let m=0.5 in
      let m'=(1.-.m)*.t0+.m*.t1 in
        (approx_rec (restrict fx 0. m) (restrict fy 0. m) t0 m')
        @(approx_rec (restrict fx m 1.) (restrict fy m 1.) m' t1)
    )
let to_point x=int_of_float (x*.50.)
let approx fx_ fy_=
  let norm'=(derivee (fx_*..fx_ +.. fy_*..fy_)) in
  let norm''=(derivee norm') in
  let sols=(bernstein_solve_int norm'' 1e-3) in
  let rec premake_ints x y t0=function
      []->
        let fxx=restrict fx_ t0 1. in
        let fyy=restrict fy_ t0 1. in
          fxx.(0)<-x;
          fyy.(0)<-y;
          [(fxx,fyy)]
    | (h0,h1)::s->(
        let fxx=restrict fx_ t0 h0 in
        let fyy=restrict fy_ t0 h0 in
          fxx.(0)<-x;
          fyy.(0)<-y;
          (fxx,fyy) :: premake_ints (fxx.(Array.length fxx-1)) (fyy.(Array.length fyy-1)) h1 s
      )
  in
  let rec make_intervals x0 l=match l with
      []->[(x0,1.)]
    | h::s ->(x0,h):: make_intervals h s
  in
  let morceaux=List.concat
    (List.map
       (fun (fx,fy)->
          let cucu=make_intervals 0. (cut fx fy) in
            List.concat (
              List.map (fun (u,v)->
                          let fxx,fyy=restrict fx u v,restrict fy u v in
                            approx_rec fxx fyy u v
                       ) cucu
            )
       ) (premake_ints fx_.(0) fy_.(0) 0. sols)
    )
  in
    morceaux
