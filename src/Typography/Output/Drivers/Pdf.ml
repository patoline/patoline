open CamomileLibrary
open Printf
open Binary
open Constants
open Fonts.FTypes
open OutputCommon
open OutputPaper

module Buf=UTF8.Buf

let filename file=try (Filename.chop_extension file)^".pdf" with _->file^".pdf"

type pdfFont= { font:Fonts.font; fontObject:int; fontWidthsObj:int; fontToUnicode:int;
                mutable fontGlyphs:Fonts.glyph IntMap.t }
let output ?(structure:structure={name="";displayname=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let outChan=open_out fileName in
  let pageBuf=Buf.create 1000 in
  let xref=ref (IntMap.singleton 1 0) in (* Le pagetree est toujours l'objet 1 *)
  let fonts=ref StrMap.empty in
  let resumeObject n=
    flush outChan;
    xref:=IntMap.add n (pos_out outChan) !xref;
    fprintf outChan "%d 0 obj\n" n
  in
  let beginObject ()=
    let n=IntMap.cardinal !xref in
      resumeObject (n+1);
      n+1
  in
  let futureObject ()=
    let n=IntMap.cardinal !xref in
      xref:=IntMap.add (n+1) (-1) !xref;
      n+1
  in
  let endObject pdf=fprintf outChan "\nendobj\n" in
  let pdf_string str=
    let str'=String.create (2+2*UTF8.length str) in
      str'.[0]<-'\254';str'.[1]<- '\255';
      let rec fill idx i=
        if UTF8.out_of_range str idx then str' else (
          let code=UChar.code (UTF8.look str idx) in
            str'.[i]<-(char_of_int ((code lsr 8) land 0xff));
            str'.[i+1]<-(char_of_int (code land 0xff));
            fill (UTF8.next str idx) (i+2)
        )
      in
        fill (UTF8.first str) 2
  in
  let addFont font=
    try StrMap.find (Fonts.fontName font) !fonts with
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
                  let fontFile=beginObject () in
                    fprintf outChan ("<< /Length %d /Subtype /CIDFontType0C >>\nstream\n%s\nendstream\n")
                      (String.length program) program;
                    endObject();

                    (* Font descriptor -- A completer*)

                    let fontName=CFF.fontName x in
                    let descr=beginObject () in
                    let (a,b,c,d)=CFF.fontBBox x in
                      fprintf outChan "<< /Type /FontDescriptor /FontName /%s" fontName;
                      fprintf outChan " /Flags 4 /FontBBox [ %d %d %d %d ] /ItalicAngle %f " a b c d (CFF.italicAngle x);
                      fprintf outChan " /Ascent 0 /Descent 0 /CapHeight 0 /StemV 0 /FontFile3 %d 0 R >>" fontFile;
                      endObject();

                      (* Widths *)
                      let w=futureObject () in

                      (* Font dictionary *)
                      let fontDict=beginObject () in
                        fprintf outChan "<< /Type /Font /Subtype /CIDFontType0 /BaseFont /%s " fontName;
                        fprintf outChan "/CIDSystemInfo << /Registry(Adobe) /Ordering(Identity) /Supplement 0 >>";
                        fprintf outChan "/W %d 0 R /FontDescriptor %d 0 R >>" w descr;
                        endObject();

                        (* CID Font dictionary *)
                        let toUnicode=futureObject () in
                        let cidFontDict=beginObject () in
                          fprintf outChan
                            "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /BaseFont /%s " fontName;
                          fprintf outChan "/DescendantFonts [%d 0 R] /ToUnicode %d 0 R >>" fontDict toUnicode;
                          endObject();

                          let result={ font=font; fontObject=cidFontDict; fontWidthsObj=w;
                                       fontToUnicode=toUnicode; fontGlyphs=IntMap.empty } in
                            fonts:=StrMap.add (Fonts.fontName font) result !fonts;
                            result
                )

  in
  let pageObjects=Array.make (Array.length pages) 0 in
    for i=0 to Array.length pageObjects-1 do pageObjects.(i)<-futureObject ()
    done;

    fprintf outChan "%%PDF-1.7\n\128\128\128\128\n";
    for page=0 to Array.length pages-1 do
      Buf.clear pageBuf;
      let pageLinks=ref [] in
      let pageFonts=ref StrMap.empty in
      let currentFont=ref (-1) in
      let currentSize=ref (-1.) in
        (* Texte *)
      let isText=ref false in
      let openedWord=ref false in
      let openedLine=ref false in
      let xt=ref 0. in
      let yt=ref 0. in
      let xline=ref 0. in

      (* Dessins *)
      let strokingColor=ref black in
      let nonStrokingColor=ref black in
      let lineWidth=ref 1. in
      let lineJoin=ref Miter_join in
      let lineCap=ref Butt_cap in
      let dashPattern=ref [] in

      let close_line ()=
        if !openedWord then (Buf.add_string pageBuf ">"; openedWord:=false);
        if !openedLine then (Buf.add_string pageBuf " ] TJ ";
                             openedLine:=false; xline:=0.);
      in
      let close_text ()=
        close_line ();
        if !isText then (Buf.add_string pageBuf " ET "; isText:=false);
        xt:=0.; yt:=0.
      in
      let change_stroking_color col =
        if col<> !strokingColor then (
          close_text();
          match col with
              RGB color -> (
                close_text ();
                let r=max 0. (min 1. color.red) in
                let g=max 0. (min 1. color.green) in
                let b=max 0. (min 1. color.blue) in
                  strokingColor:=col;
                  Buf.add_string pageBuf (sprintf "%f %f %f RG " r g b);
              )
        )
      in
      let change_non_stroking_color col =
        if col<> !nonStrokingColor then (
          close_text();
          match col with
              RGB color -> (
                close_text ();
                let r=max 0. (min 1. color.red) in
                let g=max 0. (min 1. color.green) in
                let b=max 0. (min 1. color.blue) in
                  nonStrokingColor:=col;
                  Buf.add_string pageBuf (sprintf "%f %f %f rg " r g b);
              )
        )
      in
      let set_line_join j=
        if j<> !lineJoin then (
          close_text ();
          lineJoin:=j;
          Buf.add_string pageBuf (
            match j with
                Miter_join->" 0 j "
              | Round_join->" 1 j "
              | Bevel_join->" 2 j "
                  (* | _->"" *)
          )
        )
      in
      let set_line_cap c=
        if c<> !lineCap then (
          close_text ();
          lineCap:=c;
          Buf.add_string pageBuf (
            match c with
                Butt_cap->" 0 J "
              | Round_cap->" 1 J "
              | Proj_square_cap->" 2 J "
                  (* | _->"" *)
          )
        )
      in
      let set_line_width w=
        if w <> !lineWidth then (
          close_text ();
          lineWidth:=w;
          Buf.add_string pageBuf (sprintf "%f w " w);
        )
      in
      let set_dash_pattern l=
        if l<> !dashPattern then (
          close_text ();
          dashPattern:=l;
          let l0=List.map (fun x->round (pt_of_mm x)) l in
            match l0 with
                []->(Buf.add_string pageBuf "[] 0 d ")
              | h::s->(
                  let rec pgcd a_ b_=if a_=b_ then a_ else
                    let (a,b)=if a_<b_ then (b_,a_) else (a_,b_) in
                    let div=a/b in
                    let rem=a-div*b in
                      if rem=0 then b else
                        pgcd b rem
                  in
                  let phase=List.fold_left pgcd h s in
                    Buf.add_string pageBuf " [";
                    List.iter (fun x->Buf.add_string pageBuf (sprintf "%d " (x/phase))) l0;
                    Buf.add_string pageBuf (sprintf "] %d d " phase)
                )
        )
      in
      let rec output_contents=function
        | Glyph gl->(
            change_non_stroking_color gl.glyph_color;
            if not !isText then Buf.add_string pageBuf " BT ";
            isText:=true;
            let gx=pt_of_mm gl.glyph_x in
            let gy=pt_of_mm gl.glyph_y in
            let size=pt_of_mm gl.glyph_size in



              let fnt=Fonts.glyphFont (gl.glyph) in
                (* Inclusion de la police sur la page *)
              let idx=try fst (StrMap.find (Fonts.fontName fnt) !pageFonts) with
                  Not_found->(
                    let card=StrMap.cardinal !pageFonts in
                    let pdfFont=addFont fnt in
                      pageFonts := StrMap.add (Fonts.fontName fnt) (card, pdfFont.fontObject) !pageFonts;
                      card
                  )
              in
              let pdfFont=StrMap.find (Fonts.fontName fnt) !fonts in
              let num=(Fonts.glyphNumber gl.glyph).Fonts.FTypes.glyph_index in
                if not (IntMap.mem num pdfFont.fontGlyphs) then
                  pdfFont.fontGlyphs<-IntMap.add num gl.glyph pdfFont.fontGlyphs;
                if idx <> !currentFont || size <> !currentSize then (
                  close_line ();
                  Buf.add_string pageBuf (sprintf "/F%d %f Tf " idx size);
                  currentFont:=idx;
                  currentSize:=size;
                );
                if !yt<>gy || (not !openedLine) then (
                  close_line ();
                  Buf.add_string pageBuf (sprintf "%f %f Td " (gx-. !xt) (gy-. !yt));
                  xline:=0.;
                  xt:=gx;yt:=gy
                );

                if not !openedLine then (Buf.add_string pageBuf "["; openedLine:=true; xline:=0.);

                if !xt +. !xline <> gx then (
                  let str=sprintf "%f" (1000.*.(!xt+. !xline -. gx)/.size) in
                  let i=ref 0 in
                    while !i<String.length str && (str.[!i]='0' || str.[!i]='.' || str.[!i]='-') do incr i done;
                    if !i<String.length str then (
                      if !openedWord then (Buf.add_string pageBuf ">"; openedWord:=false);
                      Buf.add_string pageBuf str;
                      xline:= !xline -. size*.(float_of_string str)/.1000.;
                    )
                );
                if not !openedWord then (Buf.add_string pageBuf "<"; openedWord:=true);
                Buf.add_string pageBuf (sprintf "%04x" num);
                xline:= !xline +. size*.Fonts.glyphWidth gl.glyph/.1000.
          )
        | Path (params,[])->()
        | Path (params,paths) ->(
            close_text ();
            set_line_join params.lineJoin;
            set_line_cap params.lineCap;
            set_line_width (pt_of_mm params.lineWidth);
            set_dash_pattern params.dashPattern;
            (match params.strokingColor with
                 None->()
               | Some col -> change_stroking_color col);
            (match params.fillColor with
                 None->()
               | Some col -> change_non_stroking_color col);
            let rec are_valid x i=
              if i>=Array.length x then true else
                if x.(i) < infinity && x.(i)> -.infinity then are_valid x (i+1) else false
            in
              List.iter (fun path->
                           let (x0,y0)=path.(0) in
                             if are_valid x0 0 && are_valid y0 0 then (
                               Buf.add_string pageBuf (sprintf "%f %f m " (pt_of_mm x0.(0)) (pt_of_mm y0.(0)));
                               Array.iter (
                                 fun (x,y)->if are_valid x 0 && are_valid y 0 then (
                                   if Array.length x=2 && Array.length y=2 then (
                                     let x1=if Array.length x=2 then x.(1) else x.(0) in
                                     let y1=if Array.length y=2 then y.(1) else y.(0) in
                                       Buf.add_string pageBuf (sprintf "%f %f l " (pt_of_mm x1) (pt_of_mm y1));
                                   ) else if Array.length x=3 && Array.length y=3 then (
                                     Buf.add_string pageBuf (sprintf "%f %f %f %f %f %f c "
                                                               (pt_of_mm ((x.(0)+.2.*.x.(1))/.3.)) (pt_of_mm ((y.(0)+.2.*.y.(1))/.3.))
                                                               (pt_of_mm ((2.*.x.(1)+.x.(2))/.3.)) (pt_of_mm ((2.*.y.(1)+.y.(2))/.3.))
                                                               (pt_of_mm x.(2)) (pt_of_mm y.(2)));
                                   ) else if Array.length x=4 && Array.length y=4 then (
                                     Buf.add_string pageBuf (sprintf "%f %f %f %f %f %f c "
                                                               (pt_of_mm x.(1)) (pt_of_mm y.(1))
                                                               (pt_of_mm x.(2)) (pt_of_mm y.(2))
                                                               (pt_of_mm x.(3)) (pt_of_mm y.(3)));
                                   )
                                 )
                               ) path
                             )
                      ) paths;
            match params.fillColor, params.strokingColor with
                None, None-> Buf.add_string pageBuf "n "
              | None, Some col -> (
                  if params.close then Buf.add_string pageBuf "s " else
                    Buf.add_string pageBuf "S "
                )
              | Some col, None -> (Buf.add_string pageBuf "f ")
              | Some fCol, Some sCol -> (
                  if params.close then Buf.add_string pageBuf "b " else
                    Buf.add_string pageBuf "B "
                )
          )
        | Link l->pageLinks:= l:: !pageLinks
      in
        List.iter output_contents pages.(page).pageContents;
        close_text ();
        (* Objets de la page *)
        let contentObj=beginObject () in
        let contStr=Buf.contents pageBuf in
          fprintf outChan "<< /Length %d >>\nstream\n%s\nendstream"
            (String.length contStr) contStr;
          endObject ();
          resumeObject pageObjects.(page);
          let w,h=pages.(page).pageFormat in
            fprintf outChan "<< /Type /Page /Parent 1 0 R /MediaBox [ 0 0 %f %f ] " (pt_of_mm w) (pt_of_mm h);
            fprintf outChan "/Resources << /ProcSet [/PDF /Text] ";

            if StrMap.cardinal !pageFonts >0 then (
              fprintf outChan " /Font << ";
              StrMap.iter (fun _ (a,b)->fprintf outChan "/F%d %d 0 R " a b) !pageFonts;
              fprintf outChan ">> "
            );
            fprintf outChan ">> /Contents %d 0 R " contentObj;

            if !pageLinks <> [] then (
              fprintf outChan "/Annots [ ";
              List.iter (fun l->fprintf outChan
                           "<< /Type /Annot /Subtype /Link /Rect [%f %f %f %f] /Dest [ %d 0 R /XYZ %f %f null] /Border [0 0 0]  >> "
                           (pt_of_mm l.link_x0) (pt_of_mm l.link_y0)
                           (pt_of_mm l.link_x1) (pt_of_mm l.link_y1) pageObjects.(l.dest_page)
                           (pt_of_mm l.dest_x) (pt_of_mm l.dest_y)
                        ) !pageLinks;
              fprintf outChan "]";
            );

            fprintf outChan ">> ";
            endObject ();
    done;

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
                         let m0,g0=IntMap.min_binding glyphs in
                           if UTF8.length (Fonts.glyphNumber g0).glyph_utf8 <= 0 then
                             make_cmap (IntMap.remove m0 glyphs)
                           else (
                             let a,b=
                               let a,gi,b=IntMap.split (m0 lor 0x00ff) glyphs in
                                 (match gi with Some ggi->IntMap.add (m0 lor 0x00ff) ggi a | _->a), b
                             in
                             let one_char, mult_char=IntMap.partition (fun _ gl->UTF8.length ((Fonts.glyphNumber gl).glyph_utf8) = 1) a
                             in
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
                       )
                     in
                       make_cmap x.fontGlyphs;
                       let rec print_utf8 utf idx=
                         try
                           Buf.add_string buf (sprintf "%04x" (UChar.uint_code (UTF8.look utf idx)));
                           print_utf8 utf (UTF8.next utf idx)
                         with
                             _->()
                       in
                       let one_nonempty=List.filter (fun (_,b)->b<>"") !one in
                         if one_nonempty<>[] then (
                           Buf.add_string buf (sprintf "%d beginbfchar\n" (List.length !one));
                           List.iter (fun (a,b)->
                                        Buf.add_string buf (sprintf "<%04x> <" a);
                                        print_utf8 b (UTF8.first b);
                                        Buf.add_string buf ">\n"
                                     ) one_nonempty;
                           Buf.add_string buf "endbfchar\n"
                         );

                         let mult_nonempty=List.filter (fun (_,b)->b<>[])
                           (List.map (fun (a,b)->a, List.filter (fun c->c<>"") b) !multRange) in

                           if !range<>[] || mult_nonempty<>[] then (
                             Buf.add_string buf (sprintf "%d beginbfrange\n" (List.length !range+List.length !multRange));
                             List.iter (fun (a,b,c)->Buf.add_string buf (sprintf "<%04x> <%04x> <%04x>\n"
                                                                           a b (UChar.uint_code c))) !range;
                             List.iter (fun (a,b)->
                                          Buf.add_string buf (sprintf "<%04x> <%04x> [" a (a+List.length b-1));
                                          List.iter (fun c->
                                                       Buf.add_string buf "<";
                                                       print_utf8 c (UTF8.first c);
                                                       Buf.add_string buf ">") b;
                                          Buf.add_string buf "]\n"
                                       ) mult_nonempty;
                             Buf.add_string buf "endbfrange\n"
                           );
                           Buf.add_string buf "endcmap\n/CMapName currentdict /CMap defineresource pop\nend end\n";


                           resumeObject x.fontToUnicode;
                           fprintf outChan "<< /Length %d >>\nstream\n%s\nendstream"
                             (String.length (Buf.contents buf)) (Buf.contents buf);

                           endObject ()
                ) !fonts;
    (* Toutes les largeurs des polices *)
    StrMap.iter (fun _ x->
                   resumeObject x.fontWidthsObj;
                   let (m0,_)=IntMap.min_binding x.fontGlyphs in
                     fprintf outChan "[ %d [ " m0;
                     for i=m0 to fst (IntMap.max_binding x.fontGlyphs) do
                       let w=try Fonts.glyphWidth (IntMap.find i x.fontGlyphs) with Not_found->0. in
                         fprintf outChan "%d " (round w);
                     done;
                     fprintf outChan "]]";
                     endObject ();
                ) !fonts;


    (* Ecriture du pageTree *)
    flush outChan;
    xref:=IntMap.add 1 (pos_out outChan) !xref;
    fprintf outChan "1 0 obj\n<< /Type /Pages /Count %d /Kids [" (Array.length pages);
    Array.iter (fun a->fprintf outChan " %d 0 R" a) pageObjects;
    fprintf outChan "] >>";
    endObject ();

    (* Ecriture du catalogue *)
    let cat=futureObject () in
      if structure.name="" && Array.length structure.substructures=0 then (
        resumeObject cat;
        fprintf outChan "<< /Type /Catalog /Pages 1 0 R >>";
        endObject ()
      ) else (
        let count=ref 0 in
        let rec make_outlines str par=
          let hijosObjs=Array.map (fun _-> futureObject ()) str.substructures in
            for i=0 to Array.length str.substructures-1 do
              let (a,b)=make_outlines str.substructures.(i) hijosObjs.(i) in
                incr count;

                resumeObject hijosObjs.(i);
                fprintf outChan "<< /Title (%s) /Parent %d 0 R " (pdf_string str.substructures.(i).name) par;
                if i>0 then fprintf outChan "/Prev %d 0 R " hijosObjs.(i-1);
                if i<Array.length str.substructures-1 then fprintf outChan "/Next %d 0 R " hijosObjs.(i+1);
                if a>0 then
                  fprintf outChan "/First %d 0 R /Last %d 0 R /Count %d "
                    a b (Array.length str.substructures.(i).substructures);
                if str.substructures.(i).page>=0 then
                  fprintf outChan "/Dest [%d 0 R /XYZ %f %f null] " pageObjects.(str.substructures.(i).page)
                    (pt_of_mm str.substructures.(i).struct_x)
                    (pt_of_mm str.substructures.(i).struct_y);
                fprintf outChan ">> ";
                endObject ()
            done;
            if Array.length hijosObjs>0 then
              (hijosObjs.(0),hijosObjs.(Array.length hijosObjs-1))
            else
              (-1,-1)
        in


        let outlines=futureObject () in
        let a,b=make_outlines structure (* { name=""; page=0; struct_x=0.; struct_y=0.; substructures=[|structure|] } *) outlines in

          resumeObject outlines;
          fprintf outChan "<< /Type /Outlines /First %d 0 R /Last %d 0 R /Count %d >>" a b !count;
          endObject ();
          resumeObject cat;
          fprintf outChan "<< /Type /Catalog /Pages 1 0 R /Outlines %d 0 R >>" outlines;
          endObject ()
      );
      endObject ();

      (* Ecriture de xref *)
      flush outChan;
      let xref_pos=pos_out outChan in
        fprintf outChan "xref\n0 %d \n0000000000 65535 f \n" (1+IntMap.cardinal !xref);
        IntMap.iter (fun _ a->fprintf outChan "%010d 00000 n \n" a) !xref;

        (* Trailer *)
        fprintf outChan "trailer\n<< /Size %d /Root %d 0 R >>\nstartxref\n%d\n%%%%EOF\n"
          (1+IntMap.cardinal !xref) cat xref_pos;
        close_out outChan
