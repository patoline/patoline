open CamomileLibrary
open Drivers
open Constants
open Fonts
open Bezier

let f = try Sys.argv.(1) with _ -> "AGaramondPro-Regular.otf"
let font=loadFont f

let cont=Glyph { glyph_x=105.;glyph_y=10.; glyph_size=5.; glyph_color=black; glyph=Fonts.loadGlyph font
    { FTypes.empty_glyph with
        FTypes.glyph_index=34} }


let (a,b) as u=([|0.; 100.; 200.;200.|], [|0.; 0.;100.;200.|])
let (c,d) as v=([|0.; 0.; 100.;200.|], [|200.; 100.;0.;0.|])
let l=List.concat (List.map ( fun (t0,_)->
                                let x,y=(eval a t0, eval b t0) in
                                  [ Path ({ default with lineWidth=0.1;  strokingColor=Some (RGB { red=1.;green=0.;blue=0. }) }, [| ([|0.; 210. |],[|y;y|]) |]);
                                    Path ({ default with lineWidth=0.1; strokingColor=Some (RGB { red=1.;green=0.;blue=0. }) }, [| ([|x; x |],[|0.;297.|]) |]) ]
                            ) (intersect u v))


let pages=[| { pageFormat=a4; pageContents=
                 [
                   Path ({ default with close=true; fillColor=Some black;strokingColor=Some (RGB { red=1.;green=1.;blue=0. }); lineWidth=0.1 }, [| a,b |]);
                   Path ({ default with lineWidth=0.1 }, [| c,d |]);
                   cont
                 ]@l
             } |]

let a={name="a"; page=0; struct_x=0.; struct_y=0.;substructures=[||] }
let b={name="b"; page=0; struct_x=0.; struct_y=0.;substructures=[||] }

let _=
  Pdf.output ~structure:{name="root"; page=0; struct_x=0.;struct_y=0.;substructures=[|a;b|] } pages "test.pdf"
