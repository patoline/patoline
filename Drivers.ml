open Binary
open Constants

module Rope=struct
  type t=string
  let empty=""
  let append a b=a^b
  let of_string a=a
  let to_string a=a
end

module type Driver = sig
  (** Dans ce module, toutes les commandes prennent en argument des unités métriques *)
  type driver
  type params=string
  val filename:string->string
  val init : params -> driver
  val close : driver -> unit

  val begin_page : driver -> (float*float) ->unit
  val end_page : driver -> unit

  val moveto : driver->(float*float) -> unit
  val lineto : driver->(float*float) -> unit
  val curveto : driver->(float*float) -> (float*float) -> (float*float) -> unit

  val stroke:driver->unit
  val fill:driver->unit
  val fill_stroke:driver->unit
  val close_stroke:driver->unit
  val closePath:driver->unit

  val text:driver->(float*float)->float->Fonts.glyph list->unit

end



module Pdf = 
  (struct
     type params=string
         
     type pdfFont= { font:Fonts.font; fontObject:int; fontWidthsObj:int; mutable fontGlyphs:float IntMap.t }

     (* l'implémentation de pdf, par contre, est toute en unité pdf, i.e. 1 pt adobe = 1/72 inch *)
     type driver= { out_chan:out_channel;
                    mutable xref:int IntMap.t;
                    mutable pages:int IntMap.t;
                    mutable current_page:Rope.t;
                    mutable current_pageSize:float*float;

                    mutable vpos:float*float;
                    mutable pos:float*float;

                    mutable posT:float*float;

                    mutable isText : bool;
                    mutable fonts : pdfFont StrMap.t;
                    mutable pageFonts: (int*int) StrMap.t;
                    mutable currentFont:int;
                    mutable currentSize:float }
         
     let pageTree=1
       
     let filename file=try (Filename.chop_extension file)^".pdf" with _->file^".pdf"

     let init file=
       let out_chan=open_out file in
         output_string out_chan ("%PDF-1.7\n%"^String.make 4 (char_of_int 128)^"\n");
         { out_chan=out_chan; pages=IntMap.empty; xref=IntMap.singleton pageTree 0;
           current_pageSize=(0.,0.);
           current_page=Rope.empty;
           vpos=(0.,0.);
           pos=(infinity,infinity);
           posT=(0.,0.);
           isText=false;
           fonts=StrMap.empty;
           pageFonts=StrMap.empty;
           currentFont=(-1); currentSize=(-1.) }
           
     let resumeObject pdf n=
       flush pdf.out_chan;
       pdf.xref<-IntMap.add n (pos_out pdf.out_chan) pdf.xref;
       output_string pdf.out_chan ((string_of_int n)^" 0 obj\n")

     let beginObject pdf=
       let n=IntMap.cardinal pdf.xref in
         resumeObject pdf (n+1);
         n+1

     let futureObject pdf=
       let n=IntMap.cardinal pdf.xref in
         pdf.xref<-IntMap.add (n+1) (-1) pdf.xref;
         n+1

     let endObject pdf=output_string pdf.out_chan "\nendobj\n"


     let addFont pdf font=
       try StrMap.find (Fonts.fontName font) pdf.fonts with
           Not_found->
             match font with
                 Fonts.CFF x->raise Fonts.Not_supported
               | Fonts.Opentype (Opentype.CFF (x,_))->
                   ((* Font program *)
                     let program=(
                       let buf=String.create (x.CFF.size) in
                         seek_in x.CFF.file x.CFF.offset;
                         really_input x.CFF.file buf 0 x.CFF.size;
                         buf) in
                     let fontFile=beginObject pdf in
                       output_string pdf.out_chan ("<< /Length "^(string_of_int (String.length program))^
                                                     " /Subtype /CIDFontType0C >>\nstream\n");
                       output_string pdf.out_chan program;
                       output_string pdf.out_chan "\nendstream\n";
                       endObject pdf;
                       
                       (* Font descriptor -- A completer*)
                       
                       let fontName=CFF.fontName x in
                       let descr=beginObject pdf in
                       let (a,b,c,d)=CFF.fontBBox x in
                         output_string pdf.out_chan ("<< /Type /FontDescriptor"^
                                                       " /FontName /"^fontName^
                                                       " /Flags 4 /FontBBox ["^((string_of_int a)^" "^(string_of_int b)^" "^
                                                                                  (string_of_int c)^" "^(string_of_int d))^
                                                       "] /ItalicAngle "^(string_of_float (CFF.italicAngle x))^
                                                       " /Ascent 0"^
                                                       " /Descent 0"^
                                                       " /CapHeight 0"^
                                                       " /StemV 0"^
                                                       " /FontFile3 "^(string_of_int fontFile)^" 0 R >>");
                         endObject pdf;
                         
                         (* Widths *)
                         let w=futureObject pdf in
                           
                           
                         (* Font dictionary *)
                         let fontDict=beginObject pdf in
                           output_string pdf.out_chan ("<< /Type /Font /Subtype /CIDFontType0"^
                                                         " /BaseFont /"^fontName^
                                                         " /CIDSystemInfo << /Registry(Adobe) /Ordering(Identity) /Supplement 0 >>"^
                                                         " /W "^(string_of_int w)^" 0 R"^
                                                         " /FontDescriptor "^(string_of_int descr)^" 0 R >>");
                           endObject pdf;
                           
                           (* CID Font dictionary *)
                           let cidFontDict=beginObject pdf in
                             output_string pdf.out_chan ("<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /BaseFont /"^
                                                           fontName^" /DescendantFonts ["^(string_of_int fontDict)^" 0 R] >>");
                             endObject pdf;

                             let result={ font=font; fontObject=cidFontDict; fontWidthsObj=w; fontGlyphs=IntMap.empty } in
                               pdf.fonts<-StrMap.add (Fonts.fontName font) result pdf.fonts;
                               result
                   )
       


     let close pdf=

       (* Toutes les largeurs des polices *)
       StrMap.iter (fun _ x->
                      resumeObject pdf x.fontWidthsObj;
                      let (m0,_)=IntMap.min_binding x.fontGlyphs in
                        output_string pdf.out_chan ("[ "^(string_of_int m0)^" [ ");
                        for i=m0 to fst (IntMap.max_binding x.fontGlyphs) do
                          let w=try IntMap.find i x.fontGlyphs with Not_found->0. in
                            output_string pdf.out_chan ((string_of_float w)^" ");
                        done;
                        output_string pdf.out_chan "]]";
                        endObject pdf;
                   ) pdf.fonts;

       (* Ecriture du pageTree *)
       resumeObject pdf pageTree;(
         output_string pdf.out_chan
           ("<< /Type /Pages /Count "^(string_of_int (IntMap.cardinal pdf.pages))^
              " /Kids ["^(IntMap.fold
                            (fun _ a str->str^(if String.length str=0 then "" else " ")^(string_of_int a)^" 0 R") pdf.pages "")^
              "] >>"));
       endObject pdf;
       
       (* Metadata stream *)
       let meta=beginObject pdf in
         output_string pdf.out_chan "<< /Type /Metadata /Subtype XML /Length 0 >>\nstream\nendstream";
         endObject pdf;
       
       (* Ecriture du catalogue *)
       let cat=beginObject pdf in
         output_string pdf.out_chan ("<< /Type /Catalog /Metadata "^(string_of_int meta)^" 0 R /Pages "^
                                       (string_of_int pageTree)^" 0 R >>");
         endObject pdf;
       

         (* Ecriture de xref *)
         flush pdf.out_chan;
         let xref=pos_out pdf.out_chan in
           output_string pdf.out_chan ("xref\n0 "^(string_of_int (1+IntMap.cardinal pdf.xref))^"\n0000000000 65535 f \n");
           IntMap.iter (fun _ a->
                          let str=string_of_int a in
                            output_string pdf.out_chan ( (String.make (10-String.length str) '0')^str^" 00000 n \n")
                       ) pdf.xref;

           (* Trailer *)
           output_string pdf.out_chan ("trailer\n<< /Size "^(string_of_int (1+IntMap.cardinal pdf.xref))^
                                         " /Root "^(string_of_int cat)^" 0 R >>\nstartxref\n"^(string_of_int xref)^
                                         "\n%%EOF\n");
           close_out pdf.out_chan


     let begin_page pdf (x,y)=
       pdf.current_page<-Rope.empty;
       pdf.current_pageSize<-(pt_of_mm x,pt_of_mm y)
           
     let end_page pdf=
       if pdf.isText then (pdf.current_page<-Rope.append pdf.current_page (Rope.of_string " ET "); pdf.isText<-false);
       pdf.posT<-(0.,0.);
       pdf.currentFont<- -1;
       pdf.currentSize<- -1.;
       let str=Rope.to_string pdf.current_page in
       let contentObject=beginObject pdf in
         output_string pdf.out_chan ("<< /Length "^(string_of_int (String.length str))^" >>\nstream\n");
         output_string pdf.out_chan str;
         output_string pdf.out_chan "\nendstream";
         endObject pdf;

         let pageObject=beginObject pdf in
           output_string pdf.out_chan ("<< /Type /Page /Parent "^(string_of_int pageTree)^" 0 R /MediaBox [ 0 0 "^
                                         (string_of_int (int_of_float (fst pdf.current_pageSize)))^" "^
                                         (string_of_int (int_of_float (snd pdf.current_pageSize)))^" ] ");
           output_string pdf.out_chan "/Resources <<";
           output_string pdf.out_chan " /ProcSet [/PDF /Text] ";
           if StrMap.cardinal pdf.pageFonts >0 then (
             output_string pdf.out_chan " /Font << ";
             StrMap.iter (fun _ (a,b)->output_string pdf.out_chan ("/F"^string_of_int a^" "^string_of_int b^" 0 R ")) pdf.pageFonts;
             output_string pdf.out_chan " >> ");
           output_string pdf.out_chan (">> /Contents "^(string_of_int contentObject)^" 0 R >>");
           
           endObject pdf;
           
           pdf.pages<-IntMap.add (1+IntMap.cardinal pdf.pages) pageObject pdf.pages


     let moveto pdf (x,y)=pdf.vpos<-(pt_of_mm x, pt_of_mm y)
       
     let really_move pdf=
       if pdf.isText then pdf.current_page<-Rope.append pdf.current_page (Rope.of_string " ET ");
       if pdf.vpos <> pdf.pos then
         pdf.current_page <- Rope.append pdf.current_page 
           (Rope.of_string ((string_of_float (fst pdf.vpos)) ^ " " ^ (string_of_float (snd pdf.vpos)) ^ " m "))
     let end_path pdf=
       pdf.pos<-(0.,0.);
       pdf.vpos<-(infinity,infinity)

     let lineto pdf (x_,y_)=
       let (x,y) as pos=pt_of_mm x_, pt_of_mm y_ in
         really_move pdf;
         pdf.current_page <- Rope.append pdf.current_page
           (Rope.of_string ((string_of_float x)^" "^(string_of_float y)^" l "));
            pdf.pos<-pos;
            pdf.vpos<-pos

     let curveto pdf (x1_,y1_) (x2_,y2_) ((x3_,y3_) as pos)=
       let x1,y1,x2,y2,x3,y3=
         pt_of_mm x1_,pt_of_mm y1_,
         pt_of_mm x2_,pt_of_mm y2_,
         pt_of_mm x3_,pt_of_mm y3_ in
       really_move pdf;
       pdf.current_page <- Rope.append pdf.current_page
         (Rope.of_string ((string_of_float x1)^" "^(string_of_float y1)^" "^(string_of_float x2)^" "^
                            (string_of_float y2)^" "^(string_of_float x3)^" "^(string_of_float y3)^" "^" c "));
       pdf.pos<-pos;
       pdf.vpos<-pos
         
     let closePath pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " h ")
     let stroke pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " S ")
     let close_stroke pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " s ")
     let fill_stroke pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " b ")
     let fill pdf=
       end_path pdf;
       pdf.current_page <- Rope.append pdf.current_page (Rope.of_string " f ")

     let hexShow i=
       let result=String.create 4 in
       let hex x=
         if x<=9 then char_of_int (int_of_char '0'+x) else
           char_of_int (int_of_char 'a'-10+x)
       in
         result.[3]<-hex (i land 0x000f);
         result.[2]<-hex ((i land 0x00f0)lsr 4);
         result.[1]<-hex ((i land 0x0f00)lsr 8);
         result.[0]<-hex ((i land 0xf000)lsr 12);
         result
           
     let text pdf (x_,y_) size_ glyphs=
       let x,y=pt_of_mm x_, pt_of_mm y_ in
       let size=pt_of_mm size_ in
       if not pdf.isText then pdf.current_page<-Rope.append pdf.current_page (Rope.of_string " BT "); 
       pdf.isText<-true;

       let (x0,y0)=pdf.posT in
         pdf.current_page<-Rope.append pdf.current_page (Rope.of_string (string_of_float (x-.x0)^" "^(string_of_float (y-.y0))^" Td "));
         pdf.posT<-(x,y);
         let opened=ref false in
           List.iter (fun gl->
                        let fnt=Fonts.glyphFont gl in
                          (* Inclusion de la police sur la page *)
                        let idx=try fst (StrMap.find (Fonts.fontName fnt) pdf.pageFonts) with
                            Not_found->(
                              let card=StrMap.cardinal pdf.pageFonts in
                              let pdfFont=addFont pdf fnt in
                                pdf.pageFonts <- StrMap.add (Fonts.fontName fnt) (card, pdfFont.fontObject) pdf.pageFonts;
                                card
                            )
                        in
                        let pdfFont=StrMap.find (Fonts.fontName fnt) pdf.fonts in
                          if not (IntMap.mem (Fonts.glyphNumber gl) pdfFont.fontGlyphs) then
                            pdfFont.fontGlyphs<-IntMap.add (Fonts.glyphNumber gl) (Fonts.glyphWidth gl) pdfFont.fontGlyphs;

                          if idx <> pdf.currentFont || size <> pdf.currentSize then
                            pdf.current_page<-Rope.append pdf.current_page (Rope.of_string ((if !opened then "> Tj " else "")^"/F"^(string_of_int idx)^" "^(string_of_int (int_of_float size))^" Tf <"))
                          else
                            (if not !opened then  pdf.current_page<-Rope.append pdf.current_page (Rope.of_string "<"));
                          pdf.current_page <- Rope.append pdf.current_page (Rope.of_string (hexShow (Fonts.glyphNumber gl)));
                          
                          pdf.currentFont<-idx;
                          pdf.currentSize<-size;
                          opened:=true;
                   ) glyphs;
         if !opened then pdf.current_page<-Rope.append pdf.current_page (Rope.of_string "> Tj ");
   end:Driver)
