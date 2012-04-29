open Offset
let to_point x=int_of_float (50.+.x*.50.)
let draw fx fy=
  let n=1000 in
  let fx2=rev fx in
  let fy2=rev fy in
    for i=0 to n do
      let (x,y),_=eval fx fy (float_of_int i/.float_of_int n) in
      let (x2,y2),_=eval fx2 fy2 (float_of_int i/.float_of_int n) in
        Graphics.plot (to_point x) (to_point y);
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
  (if Array.length Sys.argv>1 then
     for i=1 to int_of_string Sys.argv.(1) do
       let _=example () in ()
     done);
  let x=ref 0 in
    while true do
      let ex,ey=example () in
        Printf.printf "%d\n" !x;
        Graphics.open_graph "";
        Graphics.clear_graph ();
        Graphics.set_color Graphics.black;
        draw ex ey;
        Graphics.set_color Graphics.red;
        List.iter draw_bezier (approx ex ey);
        List.iter draw_bezier (approx (rev ex) (rev ey));
        flush stdout;
        let _=Graphics.wait_next_event [Graphics.Key_pressed] in incr x
    done
