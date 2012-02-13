open Binary
open Constants
open CamomileLibrary
open FontsTypes

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

  val begin_alternative_text:driver->string->unit
  val end_alternative_text:driver->unit

  val text:?color:color-> ?kerning:(float*float)-> driver->(float*float)->float->Fonts.glyph->unit
  val link:driver->(float*float)->(float*float)->string->unit
  val destination:driver->string->(float*float)->unit
end



module Pdf =
  (struct
     type params=string

     type pdfFont= { font:Fonts.font; fontObject:int; fontWidthsObj:int; fontToUnicode:int; mutable fontGlyphs:Fonts.glyph IntMap.t }

     (* l'implémentation de pdf, par contre, est toute en unité pdf, i.e. 1 pt adobe = 1/72 inch *)
     type driver= { out_chan:out_channel;
                    mutable xref:int IntMap.t;
                    mutable pages:int IntMap.t;
                    mutable current_page:Buffer.t;
                    mutable current_pageSize:float*float;
                    mutable current_page_obj:int;

                    (* Graphics parameters *)
                    mutable vpos:float*float;
                    mutable pos:float*float;
                    mutable stroking_color:color;
                    mutable non_stroking_color:color;
                    mutable line_cap:lineCap;
                    mutable line_join:lineJoin;
                    mutable line_width:float;
                    mutable dash_pattern:float list;

                    (* Text parameters *)
                    mutable posT:float*float;
                    mutable posLine:float;
                    mutable opened_line:bool;
                    mutable opened_word:bool;
                    mutable isText : bool;
                    mutable alternative_text:bool;
                    mutable fonts : pdfFont StrMap.t;
                    mutable pageFonts: (int*int) StrMap.t;
                    mutable currentFont:int;
                    mutable currentSize:float;

                    (* Hypertext *)
                    mutable destinations:(int*int*float*float) StrMap.t;
                    mutable page_links:(int*float*float*float*float) list;
                  }

     let pageTree=1

     let filename file=try (Filename.chop_extension file)^".pdf" with _->file^".pdf"

     let init file=
       let out_chan=open_out file in
         output_string out_chan ("%PDF-1.7\n%"^String.make 4 (char_of_int 128)^"\n");
         { out_chan=out_chan; pages=IntMap.empty; xref=IntMap.singleton pageTree 0;
           current_pageSize=(0.,0.);
           current_page=Buffer.empty;
           current_page_obj=(-1);
           vpos=(0.,0.);
           pos=(infinity,infinity);
           posT=(0.,0.);
           posLine=0.;
           stroking_color=black;non_stroking_color=black;
           line_cap=Butt_cap;
           line_join=Miter_join;
           line_width=1.0;
           dash_pattern=[];
           opened_line=false;
           opened_word=false;
           isText=false;
           alternative_text=false;
           fonts=StrMap.empty;
           pageFonts=StrMap.empty;
           currentFont=(-1); currentSize=(-1.);
           destinations=StrMap.empty; page_links=[]
         }

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
                                                       "] /ItalicAngle "^(Printf.sprintf "%f" (FontCFF.italicAngle x))^
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
                           let toUnicode=futureObject pdf in
                           let cidFontDict=beginObject pdf in
                             Printf.fprintf pdf.out_chan
                               "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /BaseFont /%s /DescendantFonts [%d 0 R] /ToUnicode %d 0 R >>"
                               fontName fontDict toUnicode;
                             endObject pdf;

                             let result={ font=font; fontObject=cidFontDict; fontWidthsObj=w; fontToUnicode=toUnicode; fontGlyphs=IntMap.empty } in
                               pdf.fonts<-StrMap.add (Fonts.fontName font) result pdf.fonts;
                               result
                   )



     let close pdf=
       (* Les destinations *)
       StrMap.iter (fun _ (a,b,c,d)->
                      resumeObject pdf a;
                      Printf.fprintf pdf.out_chan "[ %d 0 R /XYZ %f %f null]" b c d;
                      endObject pdf
                   ) pdf.destinations;


       (* Tous les dictionnaires de unicode mapping *)
       StrMap.iter (fun _ x->
                      let buf=Buf.create 256 in
                        Buf.add_string buf "/CIDInit /ProcSet findresource begin\n12 dict begin\nbegincmap\n";
                        Buf.add_string buf "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def\n";
                        Buf.add_string buf "/CMapName /Adobe-Identity-UCS def\n/CMapType 2 def\n";
                        Buf.add_string buf "1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n";
                        let range=ref [] in
                        let one=ref [] in
                        let multRange=ref [] in
                        let rec make_cmap glyphs=
                          if not (IntMap.is_empty glyphs) then (
                            (* On commence par partitionner par premier octet (voir adobe technical note #5144) *)
                            let m0,_=IntMap.min_binding glyphs in
                            let a,b=
                              let a,gi,b=IntMap.split (m0 lor 0x00ff) glyphs in
                                (match gi with Some ggi->IntMap.add (m0 lor 0x00ff) ggi a | _->a), b
                            in
                            let one_char, mult_char=IntMap.partition (fun _ gl->UTF8.length ((Fonts.glyphNumber gl).glyph_utf8) = 1) a in
                            let rec unicode_diff a0=
                              if not (IntMap.is_empty a0) then (
                                let _,m0=IntMap.min_binding a0 in
                                let num0=Fonts.glyphNumber m0 in
                                let u,v=IntMap.partition (fun _ x->let num=Fonts.glyphNumber x in
                                                            num.glyph_index-(UChar.uint_code (UTF8.get num.glyph_utf8 0)) =
                                                              num0.glyph_index-(UChar.uint_code (UTF8.get num0.glyph_utf8 0))
                                                         ) a0
                                in
                                let _,m1=IntMap.max_binding u in
                                  if IntMap.cardinal u > 1 then (
                                    range:=(num0.glyph_index,(Fonts.glyphNumber m1).glyph_index,
                                            (UTF8.get num0.glyph_utf8 0))::(!range)
                                  ) else (
                                    one:=(num0.glyph_index, num0.glyph_utf8)::(!one)
                                  );
                                  unicode_diff v
                              )
                            in
                              unicode_diff one_char;
                              if not (IntMap.is_empty mult_char) then (
                                let _,m0=IntMap.min_binding mult_char in
                                let first=ref ((Fonts.glyphNumber m0).glyph_index) in
                                let last=ref ((Fonts.glyphNumber m0).glyph_index-1) in
                                let cur=ref [] in
                                  IntMap.iter (fun _ a->
                                                 let num=Fonts.glyphNumber a in
                                                   if num.glyph_index > (!last)+1 then (
                                                     (match !cur with
                                                          _::_::_->multRange:=(!first, List.rev !cur)::(!multRange)
                                                        | [h]->one:=(!first, h)::(!one)
                                                        | []->());
                                                     cur:=[]
                                                   );
                                                   if !cur=[] then
                                                     first:=num.glyph_index;
                                                   cur:=num.glyph_utf8::(!cur);
                                                   last:=num.glyph_index
                                              ) mult_char;

                                  match !cur with
                                      _::_::_->multRange:=(!first, List.rev !cur)::(!multRange)
                                    | [h]->one:=(!first, h)::(!one)
                                    | []->()

                              );
                              make_cmap b
                          )
                        in
                          make_cmap x.fontGlyphs;
                          let rec print_utf8 utf idx=
                            try
                              Buf.add_string buf (Printf.sprintf "%04x" (UChar.uint_code (UTF8.look utf idx)));
                              print_utf8 utf (UTF8.next utf idx)
                            with
                                _->()
                          in
                            if !one<>[] then (
                              Buf.add_string buf (Printf.sprintf "%d beginbfchar\n" (List.length !one));
                              List.iter (fun (a,b)->
                                           Buf.add_string buf (Printf.sprintf "<%04x> <" a);
                                           print_utf8 b (UTF8.first b);
                                           Buf.add_string buf ">\n") !one;
                              Buf.add_string buf "endbfchar\n"
                            );
                            if !range<>[] || !multRange<>[] then (
                              Buf.add_string buf (Printf.sprintf "%d beginbfrange\n" (List.length !range+List.length !multRange));
                              List.iter (fun (a,b,c)->Buf.add_string buf (Printf.sprintf "<%04x> <%04x> <%04x>\n"
                                                                            a b (UChar.uint_code c))) !range;
                              List.iter (fun (a,b)->
                                           Buf.add_string buf (Printf.sprintf "<%04x> <%04x> [" a (a+List.length b-1));
                                           List.iter (fun c->
                                                        Buf.add_string buf "<";
                                                        print_utf8 c (UTF8.first c);
                                                        Buf.add_string buf ">") b;
                                           Buf.add_string buf "]\n") !multRange;
                              Buf.add_string buf "endbfrange\n"
                            );
                            Buf.add_string buf "endcmap\n/CMapName currentdict /CMap defineresource pop\nend end\n";


                            resumeObject pdf x.fontToUnicode;
                            Printf.fprintf pdf.out_chan "<< /Length %d >>\nstream\n%s\nendstream"
                              (String.length (Buf.contents buf)) (Buf.contents buf);

                            endObject pdf
                   ) pdf.fonts;
       (* Toutes les largeurs des polices *)
       StrMap.iter (fun _ x->
                      resumeObject pdf x.fontWidthsObj;
                      let (m0,_)=IntMap.min_binding x.fontGlyphs in
                        Printf.fprintf pdf.out_chan "[ %d [ " m0;
                        for i=m0 to fst (IntMap.max_binding x.fontGlyphs) do
                          let w=try Fonts.glyphWidth (IntMap.find i x.fontGlyphs) with Not_found->0. in
                            Printf.fprintf pdf.out_chan "%d " (round w);
                        done;
                        Printf.fprintf pdf.out_chan "]]";
                        endObject pdf;
                   ) pdf.fonts;

       (* Ecriture du pageTree *)
       resumeObject pdf pageTree;(
         Printf.fprintf pdf.out_chan "<< /Type /Pages /Count %d /Kids [" (IntMap.cardinal pdf.pages);
         IntMap.iter (fun _ a->Printf.fprintf pdf.out_chan " %d 0 R" a) pdf.pages;
         Printf.fprintf pdf.out_chan "] >>";
       );
       endObject pdf;

       (* Metadata stream *)
       (* let meta=beginObject pdf in *)
       (*   output_string pdf.out_chan "<< /Type /Metadata /Subtype XML /Length 0 >>\nstream\nendstream"; *)
       (*   endObject pdf; *)

       (* Ecriture du catalogue *)
       let cat=beginObject pdf in
         Printf.fprintf pdf.out_chan "<< /Type /Catalog /Pages %d 0 R >>" pageTree;
         endObject pdf;


         (* Ecriture de xref *)
         flush pdf.out_chan;
         let xref=pos_out pdf.out_chan in
           Printf.fprintf pdf.out_chan "xref\n0 %d \n0000000000 65535 f \n" (1+IntMap.cardinal pdf.xref);
           IntMap.iter (fun _ a->Printf.fprintf pdf.out_chan "%010d 00000 n \n" a) pdf.xref;

           (* Trailer *)
           Printf.fprintf pdf.out_chan "trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n"  (1+IntMap.cardinal pdf.xref) cat xref;
           close_out pdf.out_chan


     let begin_page pdf (x,y)=
       Buffer.clear pdf.current_page;
       pdf.current_page_obj<-futureObject pdf;
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

     let begin_alternative_text pdf text=()
       (* if not pdf.isText then Buffer.add_string pdf.current_page " BT "; *)
       (* if pdf.opened_word then (Buffer.add_string pdf.current_page ">"; pdf.opened_word<-false); *)
       (* if pdf.opened_line then (Buffer.add_string pdf.current_page " ] TJ "; pdf.opened_line<-false; pdf.posLine<-0.); *)
       (* pdf.alternative_text<-true; *)
       (* Buffer.add_string pdf.current_page "/Span <</ActualText("; *)
       (* Buffer.add_string pdf.current_page text; *)
       (* Buffer.add_string pdf.current_page ")>> BDC" *)

     let end_alternative_text pdf=()
       (* if pdf.alternative_text then ( *)
       (*   pdf.alternative_text<-false; *)
       (*   if pdf.opened_word then (Buffer.add_string pdf.current_page ">"; pdf.opened_word<-false); *)
       (*   Buffer.add_string pdf.current_page " EMC " *)
       (* ) *)
     let end_text pdf=
       if pdf.isText then (
         if pdf.opened_word then (Buffer.add_string pdf.current_page ">"; pdf.opened_word<-false);
         if pdf.opened_line then (Buffer.add_string pdf.current_page "] TJ "; pdf.opened_line<-false; pdf.posLine<-0.);
         end_alternative_text pdf;
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

         resumeObject pdf pdf.current_page_obj;

         Printf.fprintf pdf.out_chan "<< /Type /Page /Parent %d 0 R /MediaBox [ 0 0 %d %d ] "
           pageTree (round (fst pdf.current_pageSize)) (round (snd pdf.current_pageSize));
         Printf.fprintf pdf.out_chan "/Resources << /ProcSet [/PDF /Text]";

           if StrMap.cardinal pdf.pageFonts >0 then (
             Printf.fprintf pdf.out_chan " /Font << ";
             StrMap.iter (fun _ (a,b)->Printf.fprintf pdf.out_chan "/F%d %d 0 R " a b) pdf.pageFonts;
             Printf.fprintf pdf.out_chan ">> "
           );
           Printf.fprintf pdf.out_chan ">> /Contents %d 0 R " contentObject;

           if pdf.page_links <> [] then (
             Printf.fprintf pdf.out_chan "/Annots [";
             List.iter (fun (a,x0,y0,x1,y1)->Printf.fprintf pdf.out_chan "<< /Type /Annot /Subtype /Link /Rect [%f %f %f %f] /Dest %d 0 R >> " x0 y0 x1 y1 a) pdf.page_links;
             Printf.fprintf pdf.out_chan "]";
             pdf.page_links<-[]
           );
           Printf.fprintf pdf.out_chan ">>";
           endObject pdf;

           pdf.pages<-IntMap.add (1+IntMap.cardinal pdf.pages) pdf.current_page_obj pdf.pages


     let moveto pdf (x,y)=pdf.vpos<-(pt_of_mm x, pt_of_mm y)

     let really_move pdf=
       end_text pdf;
       if pdf.vpos <> pdf.pos then
         Buffer.add_string pdf.current_page (Printf.sprintf "%f %f m " (fst pdf.vpos) (snd pdf.vpos))
     let end_path pdf=
       end_text pdf;
       pdf.pos<-(0.,0.);
       pdf.vpos<-(infinity,infinity)

     let lineto pdf (x_,y_)=
       let (x,y) as pos=pt_of_mm x_, pt_of_mm y_ in
         really_move pdf;
         Buffer.add_string pdf.current_page (Printf.sprintf "%f %f l " x y);
         pdf.pos<-pos;
         pdf.vpos<-pos

     let curveto pdf (x1_,y1_) (x2_,y2_) ((x3_,y3_) as pos)=
       let x1,y1,x2,y2,x3,y3=
         pt_of_mm x1_,pt_of_mm y1_,
         pt_of_mm x2_,pt_of_mm y2_,
         pt_of_mm x3_,pt_of_mm y3_ in
         really_move pdf;
         Buffer.add_string pdf.current_page (Printf.sprintf "%f %f %f %f %f %f c " x1 y1 x2 y2 x3 y3);
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
         Buffer.add_string pdf.current_page (Printf.sprintf "%f w " w);
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
           Buffer.add_string pdf.current_page (Printf.sprintf "%f %f %f RG " r g b);
       )
     let change_non_stroking_color pdf color=
       if color <> pdf.non_stroking_color then (
         end_text pdf;
         let r=max 0. (min 1. color.red) in
         let g=max 0. (min 1. color.green) in
         let b=max 0. (min 1. color.blue) in
           pdf.non_stroking_color<-color;
           Buffer.add_string pdf.current_page (Printf.sprintf "%f %f %f rg " r g b);
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


     let text ?(color:color=black) ?(kerning=(0.,0.)) pdf (x_,y_) size_ gl=
       let x,y=pt_of_mm x_, pt_of_mm y_ in
       let size=pt_of_mm size_ in

         change_non_stroking_color pdf color;
         if not pdf.isText then Buffer.add_string pdf.current_page " BT ";
         pdf.isText<-true;

         let (x0,y0)=pdf.posT in
           if y0<>y then (
             if pdf.opened_word then (Buffer.add_string pdf.current_page ">"; pdf.opened_word<-false);
             if pdf.opened_line then (Buffer.add_string pdf.current_page " ] TJ "; pdf.opened_line<-false; pdf.posLine<-0.);
             end_alternative_text pdf;
             Buffer.add_string pdf.current_page (Printf.sprintf "%f %f Td " (x-.x0) (y-.y0));
             pdf.posT<-(x,y)
           );
           let x1,y1=pdf.posT in

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
           let num=(Fonts.glyphNumber gl).FontsTypes.glyph_index in
             if not (IntMap.mem num pdfFont.fontGlyphs) then
               pdfFont.fontGlyphs<-IntMap.add num gl pdfFont.fontGlyphs;

             if idx <> pdf.currentFont || size <> pdf.currentSize then (
               if pdf.opened_word then (Buffer.add_string pdf.current_page ">"; pdf.opened_word<-false);
               if pdf.opened_line then (Buffer.add_string pdf.current_page "] TJ "; pdf.opened_line<-false; pdf.posLine<-0.);
               end_alternative_text pdf;
               Buffer.add_string pdf.current_page (Printf.sprintf "/F%d %d Tf " idx (round size));
               pdf.currentFont<-idx;
               pdf.currentSize<-size;
             );

             if not pdf.opened_line then (Buffer.add_string pdf.current_page " ["; pdf.opened_line<-true; pdf.posLine<-0.);

             if x1+.pdf.posLine <> x then (
               let str=Printf.sprintf "%f" (1000.*.(x1+.pdf.posLine -. x)/.size) in
               let i=ref 0 in
                 while !i<String.length str && (str.[!i]='0' || str.[!i]='.') do incr i done;
                 if !i<String.length str then (
                   if pdf.opened_word then (Buffer.add_string pdf.current_page ">"; pdf.opened_word<-false);
                   Buffer.add_string pdf.current_page str;
                   pdf.posLine<-(pdf.posLine -. size*.(float_of_string str)/.1000.);
                 )
             );
             if not pdf.opened_word then (Buffer.add_string pdf.current_page "<"; pdf.opened_word<-true);
             Buffer.add_string pdf.current_page (Printf.sprintf "%04x" num);
             pdf.posLine<- pdf.posLine +. round_float size*.Fonts.glyphWidth gl/.1000.

     let link pdf (x0,y0) (x1,y1) name=
       let obj,_,_,_=
         try
           StrMap.find name pdf.destinations
         with
             Not_found->
               (let obj=futureObject pdf in pdf.destinations<-StrMap.add name (obj, 0, 0., 0.) pdf.destinations; obj,0,0.,0.)
       in
         pdf.page_links<-(obj,x0,y0,x1,y1)::pdf.page_links

     let destination pdf name (x,y)=
       let obj=
         try
           let x,_,_,_=StrMap.find name pdf.destinations in x
         with
             Not_found->futureObject pdf
       in
         pdf.destinations<-StrMap.add name (obj, pdf.current_page_obj, x,y) pdf.destinations

   end:Driver)
