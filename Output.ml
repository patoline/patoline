open Constants
open Boxes

module Routine=functor (M:Drivers.Driver)->struct

  let output_routine filename lines=
    let drv=M.init (M.filename "test.pdf") in
      M.begin_page drv a4;
      let y=ref (pt_of_mm 270.) in
        List.iter (fun line->
                     let x=ref 100. in
                     let rec make_line l0=match l0 with []->() | _->
                       let rec nextWord l=match l with
                           []->([],0.,[])
                         | (Glue (_,a,_))::s->([],a,s)
                         | (GlyphBox a)::s->let u,v,w=nextWord s in (a.glyph::u,v+.a.width,w)
                       in
                       let (u,v,w)=nextWord l0 in
                         M.text drv (!x,!y) 12 u;
                         x:= !x +. v;
                         make_line w
                     in
                       make_line line;
                       y:= !y-.15.
                  ) lines;
        
        
        M.end_page drv;
        M.close drv;
end
