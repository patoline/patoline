(** Output routines. An output routine is just a functor taking a driver module *)

open Constants
open Boxes
open Util
open Drivers
module Routine=functor (M:Driver)->struct

  let output_routine filename pages=
    let drv=M.init (M.filename filename) in
      if Array.length pages>=1 then (
        let y0 = 270. in
          for i=0 to Array.length pages-1 do
            let parameters,_=pages.(i).(0) in
              M.begin_page drv parameters.format;
              M.line_width drv 0.05 ;
              M.dash_pattern drv [3.;3.];
              (* M.moveto drv (x0 , 270.0); *)
              (* M.lineto drv (x0 , 270.0 -. parameters.lead *. (float_of_int parameters.lines_by_page-.1.)); *)
              (* M.moveto drv (x0 +. w , 270.0 -. parameters.lead *. (float_of_int parameters.lines_by_page-.1.)); *)
              (* M.lineto drv (x0 +. w , 270.0); *)

              M.line_width drv 0.01 ;
              for l=0 to parameters.lines_by_page-1 do
                let param=if Array.length pages.(i)>=parameters.lines_by_page then fst (pages.(i).(l)) else parameters in
                  M.moveto drv (param.left_margin , 270.0 -. param.lead *. (float_of_int l));
                  M.lineto drv (param.left_margin +. param.measure , 270.0 -. param.lead *. (float_of_int l))
              done;

              M.stroke ~color:{red=0.7 ; blue=0.7 ; green=0.7} drv;
              for j=0 to Array.length pages.(i)-1 do
                let param,line=pages.(i).(j) in
                  List.iter (fun (x,y,box)->
                               match box with
                                   GlyphBox (size,a)->M.text drv (param.left_margin +. x,y0-.y) size [a.glyph];
                                 | Mark _->()
                                 | _->()) line
              done;
              M.end_page drv;
          done
      );
      M.close drv

end
