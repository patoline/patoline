open Constants
open Boxes

module Routine=functor (M:Drivers.Driver)->struct

  let output_routine filename parameters pages=

    let drv=M.init (M.filename filename) in
    let y0= 270. in
      for i=0 to Array.length pages-1 do

        M.begin_page drv a4;

        for j=0 to Array.length pages.(i)-1 do
          let x=ref 20. in
          let rec make_line l0=match l0 with []->() | _->
            let rec nextWord l=match l with
                []->([],0.,0.,[])
              | (Glue (_,a,_))::s->([],0.,a,s)
              | (GlyphBox a)::s->let u,s,v,w=nextWord s in (a.glyph::u,a.size, v+.a.width*.a.size/.1000.,w)
            in
            let (u,s,v,w)=nextWord l0 in
              M.text drv (!x,y0-.parameters.lead*.(float_of_int j)) s u;
              (* print_string "y=";print_float (y0-.parameters.lead*.(float_of_int j));print_newline(); *)
              x:= !x +. v;
              make_line w
          in
            make_line pages.(i).(j);
        done;
        M.end_page drv;
      done;
      M.close drv
end
