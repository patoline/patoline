(** Output routines. An output routine is just a functor taking a driver module *)

open Constants
open Boxes
open Util
module Routine=functor (M:Drivers.Driver)->struct

  let output_routine filename parameters pages=
    let drv=M.init (M.filename filename) in
    let x0 = 10. in 
    let y0 = 270. in
      for i=0 to Array.length pages-1 do
        
        M.begin_page drv a4;
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
