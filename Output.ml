open Constants
open Boxes

module Routine=functor (M:Drivers.Driver)->struct

  let output_routine filename pages=

    let drv=M.init (M.filename filename) in
    let lead=15. in
    let y0=pt_of_mm 270. in
      for i=0 to Array.length pages-1 do

        M.begin_page drv a4;

        for j=0 to Array.length pages.(i)-1 do
          let x=ref 100. in
          let rec make_line l0=match l0 with []->() | _->
            let rec nextWord l=match l with
                []->([],0.,[])
              | (Glue (_,a,_))::s->([],a,s)
              | (GlyphBox a)::s->let u,v,w=nextWord s in (a.glyph::u,v+.a.width,w)
            in
            let (u,v,w)=nextWord l0 in
              M.text drv (!x,y0-.lead*.(float_of_int j)) 12 u;
              x:= !x +. v;
              make_line w
          in
            make_line pages.(i).(j);
        done;
        M.end_page drv;
      done;
      M.close drv
end
