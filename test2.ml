open Drivers

let _=
  let drv=Pdf.init "test.pdf" in
  let font=Fonts.loadFont "AGaramondPro-Regular.otf" in
    Pdf.begin_page drv (100.,100.);
    
    Pdf.text drv (10.,10.) 12 [Fonts.loadGlyph font (Fonts.glyph_of_char font 'a')];
    
    Pdf.end_page drv;
    Pdf.close drv;
