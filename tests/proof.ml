open Fonts.FTypes
open Fonts
open Bezier
open Drivers
open Constants
let f = try Sys.argv.(1) with _ -> "euler.otf"
let initial_glyph = try int_of_string Sys.argv.(2) with _ -> 0

let font= loadFont f
let nb_glyphs = Fonts.cardinal font


let w=
  let m=ref 0. in
    for i=0 to nb_glyphs-1 do
      let gl=loadGlyph font { empty_glyph with glyph_index=i } in
        m:=max !m (Fonts.glyphWidth gl)
    done;
    !m


let _=
  let left_margin=25. in
  let right_margin=5. in
  let bot=10. in
  let lead=5. in
  let bezier_au_lieu_des_glyphs=true in
  let max_line=10 in
  let size=((210.-.left_margin-.right_margin)*.100./.w) in
  let hspace= (w*.size/.1000.) in

  let rec make_pages i0 line y p=
    let rec make_line i maxi x y0 y1 l=
        if i>=maxi || i>=nb_glyphs then l, y0, y1 else (
          let gl=loadGlyph font { empty_glyph with glyph_index=i } in
            (make_line (i+1) maxi (x+. hspace)
               (min y0 (Fonts.glyph_y0 gl)) (max y1 (Fonts.glyph_y1 gl))
               (
                 if not bezier_au_lieu_des_glyphs then
                   Glyph { glyph_x=x;glyph_y=0.;glyph_color=black;glyph_size=size; glyph=gl }::l
                 else
                   translate x 0.
                     (resize (size/.1000.)
                        (Path ({Drivers.default with lineWidth=0.01},
                               (List.map (fun a->Array.of_list (List.rev a)) (outlines gl))))) :: l

               ))
        )
      in
      let l,y0,y1=make_line i0 (i0+10) 0. infinity (-.infinity) [] in
        if l=[] then [{ pageFormat=a4;
                        pageContents=p}] else (
          if line<max_line (* y -. (y1-.y0)*.size/.1000. >= bot *) then (
            let finaly= y -. y1*.size/.1000. in
            make_pages (i0+10) (line+1) (finaly +. y0*.size/.1000.-.lead ) ((Path ({Drivers.default with lineWidth=0.1}, [ [|[|0.;210.|],[|finaly;finaly|]|] ]))::
                                                              (* (Path ({Drivers.default with lineWidth=0.1}, [|[|0.;210.|],[|y;y|]|])):: *)
                                                              (List.map (translate left_margin (finaly)) l) @ p)
          ) else
            ({ pageFormat=a4;
               pageContents=p})::(make_pages i0 0 280. [])
        )
  in
    Drivers.Pdf.output (Array.of_list ((make_pages 0 0 280. []))) ((Filename.chop_extension f)^".pdf")
