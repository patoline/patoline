open Binary
open Constants

module Buf=CamomileLibrary.UTF8.Buf
module Buffer=struct
  type t=Buf.buf
  let empty=Buf.create 256
  let clear a=Buf.clear a
  let add_string a b=Buffer.add_string a b
  let to_string a=Buffer.contents a
end

type lineCap=Butt_cap | Round_cap | Proj_square_cap
type lineJoin=Miter_join | Round_join | Bevel_join
type color={ red:float; green:float; blue:float }
let black={ red=0.; green=0.; blue=0. }
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

  val stroke: ?color:color -> ?dash_pattern:float list-> ?line_width:float-> ?line_cap:lineCap-> ?line_join:lineJoin->
    driver-> unit
  val fill: ?color:color -> driver-> unit
  val fill_stroke: ?color:color -> ?dash_pattern:float list-> ?line_width:float-> ?line_cap:lineCap-> ?line_join:lineJoin->
    driver->unit
  val close_stroke: ?color:color -> ?dash_pattern:float list-> ?line_width:float-> ?line_cap:lineCap-> ?line_join:lineJoin->
    driver->unit
  val closePath:driver->unit

  val text:?color:color->driver->(float*float)->float->Fonts.glyph list->unit

end



module Pdf =
  (struct
     type params=string

     type pdfFont= { font:Fonts.font; fontObject:int; fontWidthsObj:int; mutable fontGlyphs:float IntMap.t }

     (* l'implémentation de pdf, par contre, est toute en unité pdf, i.e. 1 pt adobe = 1/72 inch *)
     type driver= { out_chan:out_channel;
                    mutable xref:int IntMap.t;
                    mutable pages:int IntMap.t;
                    mutable current_page:Buffer.t;
                    mutable current_pageSize:float*float;

                    mutable vpos:float*float;
                    mutable pos:float*float;
                    mutable stroking_color:color;
                    mutable non_stroking_color:color;
                    mutable line_cap:lineCap;
                    mutable line_join:lineJoin;
                    mutable line_width:float;
                    mutable dash_pattern:float list;
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
           current_page=Buffer.empty;
           vpos=(0.,0.);
           pos=(infinity,infinity);
           posT=(0.,0.);
           stroking_color=black;non_stroking_color=black;
           line_cap=Butt_cap;
           line_join=Miter_join;
           line_width=1.0;
           dash_pattern=[];
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
               | Fonts.Opentype (FontOpentype.CFF (x,_))->
                   ((* Font program *)
                     let program=(
                       let buf=String.create (x.FontCFF.size) in
                         seek_in x.FontCFF.file x.FontCFF.offset;
                         really_input x.FontCFF.file buf 0 x.FontCFF.size;
                         buf) in
                     let fontFile=beginObject pdf in
                       output_string pdf.out_chan ("<< /Length "^(string_of_int (String.length program))^
                                                     " /Subtype /CIDFontType0C >>\nstream\n");
                       output_string pdf.out_chan program;
                       output_string pdf.out_chan "\nendstream\n";
                       endObject pdf;

                       (* Font descriptor -- A completer*)

                       let fontName=FontCFF.fontName x in
                       let descr=beginObject pdf in
                       let (a,b,c,d)=FontCFF.fontBBox x in
                         output_string pdf.out_chan ("<< /Type /FontDescriptor"^
                                                       " /FontName /"^fontName^
                                                       " /Flags 4 /FontBBox ["^((string_of_int a)^" "^(string_of_int b)^" "^
                                                                                  (string_of_int c)^" "^(string_of_int d))^
                                                       "] /ItalicAngle "^(string_of_float (FontCFF.italicAngle x))^
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
                            output_string pdf.out_chan ((string_of_int (round w))^" ");
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
       Buffer.clear pdf.current_page;
       pdf.current_pageSize<-(pt_of_mm x,pt_of_mm y);
       pdf.posT<-(0.,0.);
       pdf.currentFont<- -1;
       pdf.currentSize<- -1.;
       pdf.dash_pattern<-[];
       pdf.line_width <- 1.0;
       pdf.line_cap<-Butt_cap;
       pdf.line_join<-Miter_join;
       pdf.stroking_color<-black;
       pdf.non_stroking_color<-black

     let end_text pdf=
       if pdf.isText then (
         Buffer.add_string pdf.current_page " ET ";
         pdf.isText<-false;
         pdf.posT<- (0.,0.);
         pdf.currentFont<- -1;
         pdf.currentSize<- -1.
       )

     let end_page pdf=
       end_text pdf;
       let str=Buffer.to_string pdf.current_page in
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
       end_text pdf;
       if pdf.vpos <> pdf.pos then
         Buffer.add_string pdf.current_page
           ((string_of_float (fst pdf.vpos)) ^ " " ^ (string_of_float (snd pdf.vpos)) ^ " m ")
     let end_path pdf=
       pdf.pos<-(0.,0.);
       pdf.vpos<-(infinity,infinity)

     let lineto pdf (x_,y_)=
       let (x,y) as pos=pt_of_mm x_, pt_of_mm y_ in
         really_move pdf;
         Buffer.add_string pdf.current_page ((string_of_float x)^" "^(string_of_float y)^" l ");
         pdf.pos<-pos;
         pdf.vpos<-pos

     let curveto pdf (x1_,y1_) (x2_,y2_) ((x3_,y3_) as pos)=
       let x1,y1,x2,y2,x3,y3=
         pt_of_mm x1_,pt_of_mm y1_,
         pt_of_mm x2_,pt_of_mm y2_,
         pt_of_mm x3_,pt_of_mm y3_ in
         really_move pdf;
         Buffer.add_string pdf.current_page
         ((string_of_float x1)^" "^(string_of_float y1)^" "^(string_of_float x2)^" "^
            (string_of_float y2)^" "^(string_of_float x3)^" "^(string_of_float y3)^" "^" c ");
         pdf.pos<-pos;
         pdf.vpos<-pos

     let set_dash_pattern pdf l=
       end_text pdf;
       let l0=List.map (fun x->round (pt_of_mm x)) l in
         match l0 with
             []->(Buffer.add_string pdf.current_page "[] 0 d ")
           | h::s->(
               let rec pgcd a_ b_=if a_=b_ then a_ else
                 let (a,b)=if a_<b_ then (b_,a_) else (a_,b_) in
                 let div=a/b in
                 let rem=a-div*b in
                   if rem=0 then b else
                     pgcd b rem
               in
               let phase=List.fold_left pgcd h s in
                 Buffer.add_string pdf.current_page " [";
                 List.iter (fun x->Buffer.add_string pdf.current_page ((string_of_int (x/phase)) ^ " ")) l0;
                 Buffer.add_string pdf.current_page ("] "^(string_of_int phase)^" d ")
             )
     let set_line_width pdf w=
       if w <> pdf.line_width then (
         end_text pdf;
         pdf.line_width<-w;
         Buffer.add_string pdf.current_page (string_of_float w);
         Buffer.add_string pdf.current_page " w "
       )

     let set_line_join pdf j=
       if j<>pdf.line_join then (
         end_text pdf;
         pdf.line_join<-j;
         Buffer.add_string pdf.current_page (
             match j with
                 Miter_join->" 0 j "
               | Round_join->" 1 j "
               | Bevel_join->" 2 j "
                   (* | _->"" *)
         )
       )
     let set_line_cap pdf c=
       if c<>pdf.line_cap then (
         end_text pdf;
         pdf.line_cap<-c;
         Buffer.add_string pdf.current_page (
           match c with
               Butt_cap->" 0 J "
             | Round_cap->" 1 J "
             | Proj_square_cap->" 2 J "
                 (* | _->"" *)
         )
       )

     let closePath pdf=
       end_path pdf;
       Buffer.add_string pdf.current_page " h "

     let change_stroking_color pdf color=
       if color <> pdf.stroking_color then (
         end_text pdf;
         let r=max 0. (min 1. color.red) in
         let g=max 0. (min 1. color.green) in
         let b=max 0. (min 1. color.blue) in
           pdf.stroking_color<-color;
           Buffer.add_string pdf.current_page
             ((string_of_float r)^" "^(string_of_float g)^" "^(string_of_float b)^" RG ")
       )
     let change_non_stroking_color pdf color=
       if color <> pdf.non_stroking_color then (
         end_text pdf;
         let r=max 0. (min 1. color.red) in
         let g=max 0. (min 1. color.green) in
         let b=max 0. (min 1. color.blue) in
         pdf.non_stroking_color<-color;
         Buffer.add_string pdf.current_page ((string_of_float r)^" "^(string_of_float g)^" "^(string_of_float b)^" rg ")
       )

     let stroke ?(color:color=black)
         ?(dash_pattern=[])
         ?(line_width=1.)
         ?(line_cap=Butt_cap)
         ?(line_join=Miter_join)
         pdf=
       end_path pdf;
       set_dash_pattern pdf dash_pattern;
       set_line_width pdf line_width;
       set_line_cap pdf line_cap;
       set_line_join pdf line_join;
       change_stroking_color pdf color;
       Buffer.add_string pdf.current_page " S "
     let close_stroke ?(color:color=black)
         ?(dash_pattern=[])
         ?(line_width=1.)
         ?(line_cap=Butt_cap)
         ?(line_join=Miter_join)
         pdf=
       end_path pdf;
       set_dash_pattern pdf dash_pattern;
       set_line_width pdf line_width;
       set_line_cap pdf line_cap;
       set_line_join pdf line_join;
       change_stroking_color pdf color;
       Buffer.add_string pdf.current_page " s "

     let fill_stroke ?(color:color=black)
         ?(dash_pattern=[])
         ?(line_width=1.)
         ?(line_cap=Butt_cap)
         ?(line_join=Miter_join)
         pdf=
       end_path pdf;
       set_dash_pattern pdf dash_pattern;
       set_line_width pdf line_width;
       set_line_cap pdf line_cap;
       set_line_join pdf line_join;
       change_non_stroking_color pdf color;
       Buffer.add_string pdf.current_page " b "

     let fill ?(color:color=black)
         pdf=
       end_path pdf;
       change_non_stroking_color pdf color;
       Buffer.add_string pdf.current_page " f "

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

     let text ?(color:color=black) pdf (x_,y_) size_ glyphs=
       let x,y=pt_of_mm x_, pt_of_mm y_ in
       let size=pt_of_mm size_ in

         change_non_stroking_color pdf color;
         if not pdf.isText then Buffer.add_string pdf.current_page " BT ";
         pdf.isText<-true;

         let (x0,y0)=pdf.posT in
           if x0<>x || y0<>y then (
             Buffer.add_string pdf.current_page (string_of_float (x-.x0)^" "^string_of_float (y-.y0)^" Td ");
             pdf.posT<-(x,y)
           );
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

                            if idx <> pdf.currentFont || size <> pdf.currentSize then (
                              if !opened then Buffer.add_string pdf.current_page "> Tj ";
                              Buffer.add_string pdf.current_page "/F";
                              Buffer.add_string pdf.current_page (string_of_int idx);
                              Buffer.add_string pdf.current_page " ";
                              Buffer.add_string pdf.current_page (string_of_int (round size));
                              Buffer.add_string pdf.current_page " Tf <"
                            ) else (
                              if not !opened then Buffer.add_string pdf.current_page "<"
                            );
                            Buffer.add_string pdf.current_page (hexShow (Fonts.glyphNumber gl));

                            pdf.currentFont<-idx;
                            pdf.currentSize<-size;
                            opened:=true;
                       ) glyphs;
             if !opened then Buffer.add_string pdf.current_page "> Tj ";
   end:Driver)
