open CamomileLibrary
open Drivers
open Constants
open Fonts

let f = try Sys.argv.(1) with _ -> "AGaramondPro-Regular.otf"
let font=loadFont f

let cont=Glyph { glyph_x=10.;glyph_y=10.; glyph_size=5.; glyph_color=black; glyph=Fonts.loadGlyph font
    { FTypes.empty_glyph with
        FTypes.glyph_index=34} }
let cont'=Link { link_x0=100.; link_y0=100.; link_x1=120.; link_y1=120.;
                 dest_page=0; dest_x=10.; dest_y=200. }

let pages=[| { pageFormat=a4; pageContents=
                 [Path (default,
                        [| ([|10.; 50.|],[|10.;50.|]) |]) ] };
               { pageFormat=a4; pageContents=[cont'] } |]

let a={name="a"; page=0; struct_x=0.; struct_y=0.;substructures=[||] }
let b={name="b"; page=0; struct_x=0.; struct_y=0.;substructures=[||] }

let _=
  Pdf.output ~structure:{name="root"; page=0; struct_x=0.;struct_y=0.;substructures=[|a;b|] } pages "test.pdf"
