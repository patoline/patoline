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
                           GlyphBox a->M.text drv (x0 +. x,y0-.y) a.size [a.glyph];
                         | Mark _->Printf.printf "mark page %d (%f,%f)\n" (i+1) x y;
                         | _->()) pages.(i).(j)
        done;
        M.end_page drv;
      done;
      M.close drv
        
end
