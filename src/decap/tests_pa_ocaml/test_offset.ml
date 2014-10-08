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
open Offset
let to_point x=int_of_float (x*.50.)
let draw fx fy=
  let n=1000 in
  let fx2=rev fx in
  let fy2=rev fy in
    for i=0 to n do
      let (x0,y0),(x1,y1)=eval fx fy (float_of_int i/.float_of_int n) in
      let (x2,y2),(x3,y3)=eval fx2 fy2 (float_of_int i/.float_of_int n) in
        Graphics.plot (to_point x0) (to_point y0);
        Graphics.plot (to_point x2) (to_point y2)
    done

let draw_bezier (fx,fy)=
  let n=1000 in
    for i=0 to n do
      let x=Bezier.eval fx (float_of_int i/.float_of_int n) in
      let y=Bezier.eval fy (float_of_int i/.float_of_int n) in
        Graphics.plot (to_point x) (to_point y);
    done

let _=
  Random.init 200;
  let x=ref
    (if Array.length Sys.argv>1 then (
       for i=1 to int_of_string Sys.argv.(1) do
         let _=example () in ()
       done;
       int_of_string Sys.argv.(1))
     else 0)
  in
    while true do
      let ex,ey=example () in
        Graphics.open_graph "";
        Graphics.clear_graph ();
        Graphics.set_color Graphics.black;
        draw ex ey;
        draw_bezier (ex, ey);
        Graphics.set_color Graphics.red;
        let col=ref false in
          List.iter (fun (x,y)->
                       if !col then Graphics.set_color Graphics.red else
                         Graphics.set_color Graphics.blue;
                       col:= not !col;
                       draw_bezier (x,y))
            (approx ex ey);
          List.iter (fun (x,y)->
                       if !col then Graphics.set_color Graphics.red else
                         Graphics.set_color Graphics.blue;
                       col:= not !col;
                       draw_bezier (x,y))
            (approx (rev ex) (rev ey));
          Printf.printf "%d\n" !x;
          flush stdout;
          let _=Graphics.wait_next_event [Graphics.Key_pressed] in incr x
    done
