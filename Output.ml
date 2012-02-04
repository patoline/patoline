(** Output routines. An output routine is just a functor taking a driver module *)

open Constants
open Boxes
open Util
open Drivers
module Routine=functor (M:Driver)->struct

  let output_routine filename parameters pages=
    let drv=M.init (M.filename filename) in
    let x0 = 10. in
    let y0 = 270. in
      for i=0 to Array.length pages-1 do

        M.begin_page drv a4 ;
        M.line_width drv 0.05 ;
        M.dash_pattern drv [3.;3.];
        M.moveto drv (10.0 , 270.0);
        M.lineto drv (10.0 , 270.0 -. parameters.lead *. (float_of_int parameters.lines_by_page));
        M.lineto drv (10.0 +. parameters.measure , 270.0 -. parameters.lead *. (float_of_int parameters.lines_by_page));
        M.lineto drv (10.0 +. parameters.measure , 270.0);
        M.lineto drv (10.0 , 270.0);
        (*STRANGE: if I don't do a stroke here, the color of the stroke below
         * is reset to black on the second page... *)
        (*M.stroke ~color:{red=0.5 ; blue=0.5 ; green=0.5} drv;
        M.line_width drv 0.01 ;
        for l=1 to parameters.lines_by_page do
          M.moveto drv (10.0 , 270.0 -. parameters.lead *. (float_of_int l));
          M.lineto drv (10.0 +. parameters.measure , 270.0 -. parameters.lead *. (float_of_int l))
        done;
        *)
        M.stroke ~color:{red=0.7 ; blue=0.7 ; green=0.7} drv;

        for j=0 to Array.length pages.(i)-1 do
          List.iter (fun (x,y,box)->
                       match box with
                           GlyphBox (size,a)->M.text drv (x0 +. x,y0-.y) size [a.glyph];
                         | Mark _->()
                         | _->()) pages.(i).(j)
        done;
        M.end_page drv;
      done;
      M.close drv

end
