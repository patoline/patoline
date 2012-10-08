open Typography
open Fonts
open CamomileLibrary
open Printf
open Util
open Fonts.FTypes
open OutputCommon
open OutputPaper

let filename file=try (Filename.chop_extension file)^".pdf" with _->file^".pdf"

type pdfFont= { font:Fonts.font; fontObject:int; fontWidthsObj:int; fontToUnicode:int;
                fontFile:int;
                mutable fontGlyphs:(int*Fonts.glyph) IntMap.t;
                mutable revFontGlyphs:(Fonts.glyph) IntMap.t }


(* Ce flag sert essentiellement au démouchage des sous-ensembles de polices *)
#define SUBSET

#ifdef CAMLZIP
let stream buf=
  let tmp0=Filename.temp_file "txp_" "" in
  let tmp1=Filename.temp_file "txp_" "" in
  if Sys.file_exists tmp0 then Unix.unlink tmp0;
  let f0=open_out_bin tmp0 in
  Rbuffer.output_buffer f0 buf;
  close_out f0;
  let ic = open_in_bin tmp0
  and oc = open_out_bin tmp1 in
  Zlib.compress (fun buf -> input ic buf 0 (String.length buf))
    (fun buf len -> output oc buf 0 len);
  close_in ic;
  close_out oc;
  let f=open_in_bin tmp1 in
  let out_buf=Rbuffer.create 100000 in
  Rbuffer.add_channel out_buf f (in_channel_length f);
  close_in f;
  "/Filter [/FlateDecode]", out_buf
#else
  let stream buf="",buf
#endif

let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let fileName = filename fileName in
  if Sys.file_exists fileName then Unix.unlink fileName;
  let outChan=open_out_bin fileName in
  let pageBuf=Rbuffer.create 100000 in
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
    try StrMap.find (Fonts.uniqueName font) !fonts with
        Not_found->
          match font with
              Fonts.CFF _
            | Fonts.Opentype (Opentype.CFF _)
            | Fonts.Opentype (Opentype.TTF _)->(
              ((* Font program *)
                let fontFile=futureObject () in

                  (* Font descriptor -- A completer*)

                let fontName="PATOLIN+"^(Fonts.fontName font).postscript_name in
                let descr=beginObject () in
                let (a,b,c,d)=match font with
                    Fonts.CFF x->CFF.fontBBox x
                  | Fonts.Opentype x->Opentype.fontBBox x
                  (* | _->assert false *)
                in
                let italicAngle=match font with
                    Fonts.CFF x->CFF.italicAngle x
                  | Fonts.Opentype x->Opentype.italicAngle x
                  (* | _->assert false *)
                in
                fprintf outChan "<< /Type /FontDescriptor /FontName /%s" fontName;
                fprintf outChan " /Flags 4 /FontBBox [ %d %d %d %d ] /ItalicAngle %f " a b c d italicAngle;
                fprintf outChan " /Ascent 0 /Descent 0 /CapHeight 0 /StemV 0 /FontFile%d %d 0 R >>"
                  (match font with Fonts.Opentype (Opentype.TTF _)->2 | _->3)
                  fontFile;
                endObject();

                (* Widths *)
                let w=futureObject () in

                (* Font dictionary *)
                let fontDict=beginObject () in
                fprintf outChan "<< /Type /Font /Subtype /CIDFontType%d /BaseFont /%s "
                  (match font with Fonts.Opentype (Opentype.TTF _)->2 | _->0)
                  fontName;
                (match font with
                    Fonts.Opentype (Opentype.TTF _)->
                      fprintf outChan "/CIDToGIDMap /Identity "
                  | _->()
                );
                fprintf outChan "/CIDSystemInfo << /Registry(Adobe) /Ordering(Identity) /Supplement 0 >> ";
                fprintf outChan "/W %d 0 R " w;
                fprintf outChan "/FontDescriptor %d 0 R >>"  descr;
                endObject();

                (* CID Font dictionary *)
                let toUnicode=futureObject () in
                let cidFontDict=beginObject () in
                fprintf outChan
                  "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /BaseFont /%s " fontName;
                fprintf outChan "/DescendantFonts [%d 0 R] /ToUnicode %d 0 R >>" fontDict toUnicode;
                endObject();

                let result={ font=font; fontObject=cidFontDict; fontWidthsObj=w;
                             fontFile=fontFile;
                             fontToUnicode=toUnicode;
                             fontGlyphs=IntMap.singleton 0 (0,Fonts.loadGlyph font { glyph_utf8="";glyph_index=0 });
                             revFontGlyphs=IntMap.singleton 0 (Fonts.loadGlyph font { glyph_utf8="";glyph_index=0 }) } in
                fonts:=StrMap.add (Fonts.uniqueName font) result !fonts;
                result
              )
            )
  in
  let pageObjects=Array.make (Array.length pages) 0 in
    for i=0 to Array.length pageObjects-1 do pageObjects.(i)<-futureObject ()
    done;

    fprintf outChan "%%PDF-1.7\n%%ãõẽũ\n";
    for page=0 to Array.length pages-1 do
      Rbuffer.reset pageBuf;
      let pageLinks=ref [] in
      let pageImages=ref [] in
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
        if !openedWord then (Rbuffer.add_string pageBuf ">"; openedWord:=false);
        if !openedLine then (Rbuffer.add_string pageBuf " ] TJ ";
                             openedLine:=false; xline:=0.);
      in
      let close_text ()=
        close_line ();
        if !isText then (Rbuffer.add_string pageBuf " ET "; isText:=false);
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
                  Rbuffer.add_string pageBuf (sprintf "%f %f %f RG " r g b);
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
                  Rbuffer.add_string pageBuf (sprintf "%f %f %f rg " r g b);
              )
        )
      in
      let set_line_join j=
        if j<> !lineJoin then (
          close_text ();
          lineJoin:=j;
          Rbuffer.add_string pageBuf (
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
          Rbuffer.add_string pageBuf (
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
          Rbuffer.add_string pageBuf (sprintf "%f w " w);
        )
      in
      let set_dash_pattern l=
        if l<> !dashPattern then (
          close_text ();
          dashPattern:=l;
          match l with
              []->(Rbuffer.add_string pageBuf "[] 0 d ")
            | _::_->(
                Rbuffer.add_string pageBuf " [";
                List.iter (fun x->Rbuffer.add_string pageBuf (sprintf "%f " x)) l;
                Rbuffer.add_string pageBuf (sprintf "] 0. d ");
              )
        )
      in
      let rec output_contents=function
        | Glyph gl->(
            change_non_stroking_color gl.glyph_color;
            if not !isText then Rbuffer.add_string pageBuf " BT ";
            isText:=true;
            let gx=pt_of_mm gl.glyph_x in
            let gy=pt_of_mm gl.glyph_y in
            let size=pt_of_mm gl.glyph_size in



              let fnt=Fonts.glyphFont (gl.glyph) in
                (* Inclusion de la police sur la page *)
              let idx=try fst (StrMap.find (Fonts.uniqueName fnt) !pageFonts) with
                  Not_found->(
                    let card=StrMap.cardinal !pageFonts in
                    let pdfFont=addFont fnt in
                      pageFonts := StrMap.add (Fonts.uniqueName fnt) (card, pdfFont.fontObject) !pageFonts;
                      card
                  )
              in
              let pdfFont=StrMap.find (Fonts.uniqueName fnt) !fonts in
              let num=
#ifdef SUBSET
            let num0=(Fonts.glyphNumber gl.glyph).Fonts.FTypes.glyph_index in
            (try
               fst (IntMap.find num0 pdfFont.fontGlyphs)
             with
                 Not_found->(
                   let num1=IntMap.cardinal pdfFont.fontGlyphs in
                   pdfFont.fontGlyphs<-IntMap.add num0
                     (num1,gl.glyph) pdfFont.fontGlyphs;
                   pdfFont.revFontGlyphs<-IntMap.add num1
                     (gl.glyph) pdfFont.revFontGlyphs;
                   num1
                 )
            )
#else
  let num0=(Fonts.glyphNumber gl.glyph).Fonts.FTypes.glyph_index in
  pdfFont.fontGlyphs<-IntMap.add num0
    (num0,gl.glyph) pdfFont.fontGlyphs;
  pdfFont.revFontGlyphs<-IntMap.add num0
    (gl.glyph) pdfFont.revFontGlyphs;
  num0
#endif
              in
                (* Printf.fprintf stderr "%s %d -> %d\n" (Fonts.fontName fnt) num0 num; *)

                if idx <> !currentFont || size <> !currentSize then (
                  close_line ();
                  Rbuffer.add_string pageBuf (sprintf "/F%d %f Tf " idx size);
                  currentFont:=idx;
                  currentSize:=size;
                );
                if !yt<>gy || (not !openedLine) then (
                  close_line ();
                  Rbuffer.add_string pageBuf (sprintf "%f %f Td " (gx-. !xt) (gy-. !yt));
                  xline:=0.;
                  xt:=gx;yt:=gy
                );

                if not !openedLine then (Rbuffer.add_string pageBuf "["; openedLine:=true; xline:=0.);

                if !xt +. !xline <> gx then (
                  let str=sprintf "%f" (1000.*.(!xt+. !xline -. gx)/.size) in
                  let i=ref 0 in
                    while !i<String.length str && (str.[!i]='0' || str.[!i]='.' || str.[!i]='-') do incr i done;
                    if !i<String.length str then (
                      if !openedWord then (Rbuffer.add_string pageBuf ">"; openedWord:=false);
                      Rbuffer.add_string pageBuf str;
                      xline:= !xline -. size*.(float_of_string str)/.1000.;
                    )
                );
                if not !openedWord then (Rbuffer.add_string pageBuf "<"; openedWord:=true);
                Rbuffer.add_string pageBuf (sprintf "%04x" num);
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
                if Array.length path > 0 then (
                  let (x0,y0)=path.(0) in
                  if are_valid x0 0 && are_valid y0 0 then (
                    Rbuffer.add_string pageBuf (sprintf "%f %f m " (pt_of_mm x0.(0)) (pt_of_mm y0.(0)));
                    Array.iter (
                      fun (x,y)->if are_valid x 0 && are_valid y 0 then (
                        if Array.length x<=2 && Array.length y<=2 then (
                          let x1=if Array.length x=2 then x.(1) else x.(0) in
                          let y1=if Array.length y=2 then y.(1) else y.(0) in
                          Rbuffer.add_string pageBuf (sprintf "%f %f l " (pt_of_mm x1) (pt_of_mm y1));
                        ) else if Array.length x=3 && Array.length y=3 then (
                          Rbuffer.add_string pageBuf (sprintf "%f %f %f %f %f %f c "
                                                        (pt_of_mm ((x.(0)+.2.*.x.(1))/.3.)) (pt_of_mm ((y.(0)+.2.*.y.(1))/.3.))
                                                        (pt_of_mm ((2.*.x.(1)+.x.(2))/.3.)) (pt_of_mm ((2.*.y.(1)+.y.(2))/.3.))
                                                        (pt_of_mm x.(2)) (pt_of_mm y.(2)));
                        ) else if Array.length x=4 && Array.length y=4 then (
                          Rbuffer.add_string pageBuf (sprintf "%f %f %f %f %f %f c "
                                                        (pt_of_mm x.(1)) (pt_of_mm y.(1))
                                                        (pt_of_mm x.(2)) (pt_of_mm y.(2))
                                                        (pt_of_mm x.(3)) (pt_of_mm y.(3)));
                        )
                      )
                    ) path
                  ))
              ) paths;
            match params.fillColor, params.strokingColor with
                None, None-> Rbuffer.add_string pageBuf "n "
              | None, Some col -> (
                  if params.close then Rbuffer.add_string pageBuf "s " else
                    Rbuffer.add_string pageBuf "S "
                )
              | Some col, None -> (Rbuffer.add_string pageBuf "f ")
              | Some fCol, Some sCol -> (
                  if params.close then Rbuffer.add_string pageBuf "b " else
                    Rbuffer.add_string pageBuf "B "
                )
          )
        | Link l->pageLinks:= l:: !pageLinks
        | Image i->(
#ifdef CAMLIMAGES
            pageImages:=i::(!pageImages);
            let num=List.length !pageImages in
            close_text ();
            Rbuffer.add_string pageBuf
              (Printf.sprintf "q %f 0 0 %f %f %f cm /Im%d Do Q "
                 (pt_of_mm i.image_width) (pt_of_mm i.image_height)
                 (pt_of_mm i.image_x) (pt_of_mm i.image_y) num);
#endif
)
        | States (a,b)->List.iter output_contents a
      in
        List.iter output_contents pages.(page).pageContents;
        close_text ();
        (* Objets de la page *)
        let contentObj=beginObject () in
        let filt, data=stream pageBuf in
        let len=Rbuffer.length data in
          fprintf outChan "<< /Length %d %s>>\nstream\n" len filt;
          Rbuffer.output_buffer outChan data;
          fprintf outChan "\nendstream";
          endObject ();
          resumeObject pageObjects.(page);
          let w,h=pages.(page).pageFormat in
            fprintf outChan "<< /Type /Page /Parent 1 0 R /MediaBox [ 0 0 %f %f ] " (pt_of_mm w) (pt_of_mm h);
            fprintf outChan "/Resources << /ProcSet [/PDF /Text%s] "
              (if !pageImages=[] then "" else " /ImageB");
            if !pageImages<>[] then fprintf outChan " /XObject << ";
            let ii=ref 1 in
            let actual_pageImages=
              List.map (fun i->
                          let obj=futureObject () in
                          fprintf outChan "/Im%d %d 0 R" !ii obj;
                          incr ii;
                          (obj, !ii, i)) (List.rev !pageImages)
            in
            if !pageImages<>[] then fprintf outChan ">>";
            if StrMap.cardinal !pageFonts >0 then (
              fprintf outChan " /Font << ";
              StrMap.iter (fun _ (a,b)->fprintf outChan "/F%d %d 0 R " a b) !pageFonts;
              fprintf outChan ">> "
            );
            fprintf outChan ">> /Contents %d 0 R " contentObj;

            if !pageLinks <> [] then (
              fprintf outChan "/Annots [ ";
              List.iter (fun l->
                             if l.uri="" then
                               fprintf outChan
                                 "<< /Type /Annot /Subtype /Link /Rect [%f %f %f %f] /F 4 /Dest [ %d 0 R /XYZ %f %f null] /Border [0 0 0]  >> "
                                 (pt_of_mm l.link_x0) (pt_of_mm l.link_y0)
                                 (pt_of_mm l.link_x1) (pt_of_mm l.link_y1) pageObjects.(l.dest_page)
                                 (pt_of_mm l.dest_x) (pt_of_mm l.dest_y)
                             else
                               fprintf outChan
                                 "<< /Type /Annot /Subtype /Link /Rect [%f %f %f %f] /F 4 /A <</Type /Action /S /URI /URI (%s)>> /Border [0 0 0]  >> "
                                 (pt_of_mm l.link_x0) (pt_of_mm l.link_y0)
                                 (pt_of_mm l.link_x1) (pt_of_mm l.link_y1)
                                 l.uri
                        ) !pageLinks;
              fprintf outChan "]";
            );
            fprintf outChan ">> ";
            endObject ();

            if !pageImages<>[] then (
              List.iter (fun (obj,_,i)->
                           resumeObject obj;
#ifdef CAMLIMAGES
                           let image=(OImages.load i.image_file []) in
                           let w,h=Images.size image#image in
                             (match image#image_class with
                                  OImages.ClassRgb24->(
                                    let src=OImages.rgb24 image in
                                    let img_buf=Rbuffer.create (w*h*3) in
                                      for j=0 to h-1 do
                                        for i=0 to w-1 do
                                          let rgb = src#get i j in
                                          Rbuffer.add_char img_buf (char_of_int rgb.Images.r);
                                          Rbuffer.add_char img_buf (char_of_int rgb.Images.g);
                                          Rbuffer.add_char img_buf (char_of_int rgb.Images.b);
                                        done
                                      done;
                                      let a,b=stream img_buf in
                                      fprintf outChan "<< /Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace /DeviceRGB /BitsPerComponent 8 /Length %d %s>>\nstream\n" w h (Rbuffer.length b) a;
                                      Rbuffer.output_buffer outChan b;
                                      fprintf outChan "\nendstream";

                                  )
                                | OImages.ClassRgba32->(
                                    let src=OImages.rgba32 image in
                                    let img_buf=Rbuffer.create (w*h*3) in
                                      for j=0 to h-1 do
                                        for i=0 to w-1 do
                                          let rgb = src#get i j in
                                            Rbuffer.add_char img_buf (char_of_int rgb.Images.color.Images.r);
                                            Rbuffer.add_char img_buf (char_of_int rgb.Images.color.Images.g);
                                            Rbuffer.add_char img_buf (char_of_int rgb.Images.color.Images.b);
                                        done
                                      done;
                                      let a,b=stream img_buf in
                                      fprintf outChan "<< /Type /XObject /Subtype /Image /Width %d /Height %d /ColorSpace /DeviceRGB /BitsPerComponent 8 /Length %d %s>>\nstream\n" w h (Rbuffer.length b) a;
                                      Rbuffer.output_buffer outChan b;
                                      fprintf outChan "\nendstream";
                                  )
                                | _->()
                             );
                             image#destroy;
#endif
                             endObject ()
                        ) actual_pageImages
            )
    done;

    (* Tous les dictionnaires de unicode mapping *)
    let defaultutf8=String.make 1 (char_of_int 0) in
    let glyphutf8 x=
      let n=Fonts.glyphNumber x in
      if n.glyph_utf8="" then defaultutf8 else n.glyph_utf8
    in
    StrMap.iter (fun _ x->
      let buf=Rbuffer.create 100000 in
      Rbuffer.add_string buf "/CIDInit /ProcSet findresource begin\n12 dict begin\nbegincmap\n";
      Rbuffer.add_string buf "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def\n";
      Rbuffer.add_string buf "/CMapName /Adobe-Identity-UCS def\n/CMapType 2 def\n";
      Rbuffer.add_string buf "1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n";
      let range=ref [] in
      let one=ref [] in
      let multRange=ref [] in
      let rec make_cmap glyphs=
        if not (IntMap.is_empty glyphs) then (
          (* On commence par partitionner par premier octet (voir adobe technical note #5144) *)
          let m0,g0=IntMap.min_binding glyphs in
          let a,b=
            let a,gi,b=IntMap.split (m0 lor 0x00ff) glyphs in
            (match gi with Some ggi->IntMap.add (m0 lor 0x00ff) ggi a | _->a), b
          in
          let one_char, mult_char=IntMap.partition (fun _ gl->
            let utf8=(glyphutf8 gl) in
            UTF8.next utf8 0 > String.length utf8) a
          in
          (* recuperer les intervalles et singletons *)
          let rec unicode_diff a0=
            if not (IntMap.is_empty a0) then (
              let idx0,m0=IntMap.min_binding a0 in
              let num0=glyphutf8 m0 in
              let u,v=IntMap.partition (fun idx x->let num=glyphutf8 x in
                                                   idx-(UChar.uint_code (UTF8.get num 0)) =
                  idx0-(UChar.uint_code (UTF8.get num0 0))
              ) a0
              in
              let idx1,m1=IntMap.max_binding u in
              if IntMap.cardinal u > 1 then (
                range:=(idx0,idx1,UTF8.get num0 0)::(!range)
              ) else (
                one:=(idx0, num0)::(!one)
              );
              unicode_diff v
            )
          in
          unicode_diff one_char;
          if not (IntMap.is_empty mult_char) then (
            let idx0,m0=IntMap.min_binding mult_char in
            let first=ref idx0 in
            let last=ref (idx0-1) in
            let cur=ref [] in
            IntMap.iter (fun idx a->
              let num=glyphutf8 a in
              if idx > (!last)+1 then (
                (match !cur with
                    _::_::_->multRange:=(!first, List.rev !cur)::(!multRange)
                  | [h]->one:=(!first, h)::(!one)
                  | []->());
                cur:=[]
              );
              if !cur=[] then
                first:=idx;
              cur:=num::(!cur);
              last:=idx
            ) mult_char;

            match !cur with
                _::_::_->multRange:=(!first, List.rev !cur)::(!multRange)
              | [h]->one:=(!first, h)::(!one)
              | []->()

          );
          make_cmap b
        )
      in
      make_cmap x.revFontGlyphs;
      let rec print_utf8 utf idx=
        try
          Rbuffer.add_string buf (sprintf "%04x" (UChar.uint_code (UTF8.look utf idx)));
          print_utf8 utf (UTF8.next utf idx)
        with
            _->()
      in
      let one_nonempty=List.filter (fun (_,b)->b<>"") !one in
      if one_nonempty<>[] then (
        Rbuffer.add_string buf (sprintf "%d beginbfchar\n" (List.length !one));
        List.iter (fun (a,b)->
          Rbuffer.add_string buf (sprintf "<%04x> <" a);
          print_utf8 b (UTF8.first b);
          Rbuffer.add_string buf ">\n"
        ) one_nonempty;
        Rbuffer.add_string buf "endbfchar\n"
      );

      let mult_nonempty=List.filter (fun (_,b)->b<>[])
        (List.map (fun (a,b)->a, List.filter (fun c->c<>"") b) !multRange) in

      if !range<>[] || mult_nonempty<>[] then (
        Rbuffer.add_string buf (sprintf "%d beginbfrange\n" (List.length !range+List.length !multRange));
        List.iter (fun (a,b,c)->Rbuffer.add_string buf (sprintf "<%04x> <%04x> <%04x>\n"
                                                          a b (UChar.uint_code c))) !range;
        List.iter (fun (a,b)->
          Rbuffer.add_string buf (sprintf "<%04x> <%04x> [" a (a+List.length b-1));
          List.iter (fun c->
            Rbuffer.add_string buf "<";
            print_utf8 c (UTF8.first c);
            Rbuffer.add_string buf ">") b;
          Rbuffer.add_string buf "]\n"
        ) mult_nonempty;
        Rbuffer.add_string buf "endbfrange\n"
      );
      Rbuffer.add_string buf "endcmap\n/CMapName currentdict /CMap defineresource pop\nend end\n";


      resumeObject x.fontToUnicode;
      let filt, data=stream buf in
      let len=Rbuffer.length data in
      fprintf outChan "<< /Length %d %s>>\nstream\n" len filt;
      Rbuffer.output_buffer outChan data;
      fprintf outChan "\nendstream";
      endObject ()
    ) !fonts;



    (* Toutes les largeurs des polices *)
    StrMap.iter (fun _ x->
                   resumeObject x.fontWidthsObj;
#ifdef SUBSET
                   fprintf outChan "[ 0 [ ";
                   let (m0,_)=IntMap.min_binding x.revFontGlyphs in
                   let (m1,_)=IntMap.max_binding x.revFontGlyphs in
                   for i=m0 to m1 do
                     let w=try Fonts.glyphWidth (IntMap.find i x.revFontGlyphs) with Not_found->0. in
                     fprintf outChan "%d " (int_of_float w);
                   done;
                   (* IntMap.iter (fun i gl-> *)
                   (*   let w=Fonts.glyphWidth gl in *)
                   (*   fprintf outChan "%d " (round w)) x.revFontGlyphs; *)
                   fprintf outChan "]]";
#else
                    let (m0,(_,gl0))=IntMap.min_binding x.fontGlyphs in
                      fprintf outChan "[ %d [ " m0;
                    let f=Fonts.glyphFont gl0 in
                    for i=m0 to Fonts.cardinal f-1 do
                      let gl=Fonts.loadGlyph f { glyph_utf8="";glyph_index=i } in
                      let w=Fonts.glyphWidth gl in
                      fprintf outChan "%d " (round w);
                    done;
                    fprintf outChan "]]";
#endif
                   endObject ();
                ) !fonts;
    (* Les programmes des polices *)
    StrMap.iter (fun _ x->
                   resumeObject x.fontFile;
#ifdef SUBSET
                   let program=match x.font with
                       Fonts.Opentype (Opentype.CFF y)->(
                         let y=y.Opentype.cff_font in
                         let sub=CFF.subset y
                           {CFF.name=(CFF.fontName y)} IntMap.empty
                           (Array.of_list ((List.map (fun (_,gl)->(Fonts.glyphNumber gl))
                                              (IntMap.bindings x.revFontGlyphs))))
                         in
                         sub
                       )
                     | Fonts.CFF y->(
                       let sub=CFF.subset y {CFF.name=(CFF.fontName y)} IntMap.empty
                         (Array.of_list ((List.map (fun (_,gl)->(Fonts.glyphNumber gl))
                                            (IntMap.bindings x.revFontGlyphs))))
                       in
                       sub
                     )
                     | Fonts.Opentype (Opentype.TTF ttf as f)->(

                       let info=Opentype.fontInfo f in
                       let glyphs=(Array.of_list ((List.map (fun (_,gl)->(Fonts.glyphNumber gl))
                                                     (IntMap.bindings x.revFontGlyphs))))
                       in
                       let sub=Opentype.subset f info IntMap.empty glyphs in
                       sub
                     )
                   in
#else
                   let program=match x.font with
                       Fonts.Opentype (Opentype.CFF _)
                     | Fonts.CFF _->(
                       let y=match x.font with
                           Fonts.CFF x->x
                         | Fonts.Opentype (Opentype.CFF x)->x.Opentype.cff_font
                         | _->assert false
                       in
                       let file=open_in_bin_cached y.CFF.file in
                       seek_in file y.CFF.offset;
                       let buf=Rbuffer.create 100000 in
                       Rbuffer.add_channel buf file y.CFF.size;
                       buf
                     )
                     | Fonts.Opentype (Opentype.TTF ttf)->(
                       let file=open_in_bin_cached ttf.Opentype.ttf_file in
                       seek_in file ttf.Opentype.ttf_offset;
                       let buf=Rbuffer.create 100000 in
                       Rbuffer.add_channel buf file (in_channel_length file);
                       buf
                     )
                   (* | _->raise Fonts.Not_supported *)
                   in
#endif
                   let filt, data=stream program in
                   let subtype=match x.font with
                       Fonts.Opentype(Opentype.CFF _)
                     | Fonts.CFF _->"/Subtype /CIDFontType0C"
                     | Fonts.Opentype(Opentype.TTF _)->""
                   in
                   let len=Rbuffer.length data in
                   fprintf outChan "<< /Length %d %s %s>>\nstream\n" len subtype filt;
                   Rbuffer.output_buffer outChan data;
                   fprintf outChan "\nendstream";
                   endObject();
                ) !fonts;




    (* Ecriture du pageTree *)
    flush outChan;
    xref:=IntMap.add 1 (pos_out outChan) !xref;
    fprintf outChan "1 0 obj\n<< /Type /Pages /Count %d /Kids [" (Array.length pages);
    Array.iter (fun a->fprintf outChan " %d 0 R" a) pageObjects;
    fprintf outChan "] >>";
    endObject ();

    (* Ecriture du catalogue *)

    let rdf=Rbuffer.create 1000 in
    Rbuffer.add_string rdf"<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?>\n";

    Rbuffer.add_string rdf "<rdf:RDF
xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n";
    Rbuffer.add_string rdf "<rdf:Description rdf:about=\"\" xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\">\n<pdfaid:part>1</pdfaid:part>\n<pdfaid:conformance>A</pdfaid:conformance>\n</rdf:Description>\n";

    Rbuffer.add_string rdf "<rdf:Description rdf:about=\"\">\n";


    List.iter (fun (meta, cont)->
      let descr=match meta with
          Contributor->"contributor"
        | Coverage->"coverage"
        | Creator->"creator"
        | Date->"date"
        | Description->"description"
        | Format->"format"
        | Identifier->"identifier"
        | Language->"language"
        | Publisher->"publisher"
        | Relation->"relation"
        | Rights->"rights"
        | Source->"source"
        | Subject->"subject"
        | Title->"title"
        | Type->"type"
      in
      Rbuffer.add_string rdf (sprintf "<dc:%s>%s</dc:%s>\n" descr cont descr)
    ) structure.metadata;

    Rbuffer.add_string rdf "</rdf:Description>\n</rdf:RDF>\n";
    Rbuffer.add_string rdf "<?xpacket end='w'?>\n";

    let metadata=beginObject () in
    fprintf outChan "<< /Length %d /Type /Metadata /Subtype /XML >>\nstream\n"
      (Rbuffer.length rdf);
    Rbuffer.output_buffer outChan rdf;
    fprintf outChan "\nendstream\n";
    endObject ();

    let markinfo="<< /Marked true /UserProperties false /Suspects false >>" in
    let outputIntents="/OutputIntents [ << /Info (none) /Type /OutputIntent /S /GTS_PDFX /OutputConditionIdentifier (Blurb.com) /RegistryName (http://www.color.org/) >> ]"
    in


    (* Encore plus de metadonnees ! structtree *)
    let structTreeRoot=beginObject () in
    fprintf outChan "<< /Type /StructTreeRoot /S /Document >>\n";
    endObject ();

    let cat=futureObject () in
    if structure.name="" && Array.length structure.substructures=0 then (
      resumeObject cat;
      fprintf outChan "<< /Type /Catalog /Pages 1 0 R /Metadata %d 0 R /MarkInfo %s %s /StructTreeRoot %d 0 R >>" metadata markinfo outputIntents structTreeRoot;
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
      fprintf outChan "<< /Type /Catalog /Pages 1 0 R /Outlines %d 0 R /Metadata %d 0 R /MarkInfo %s %s /StructTreeRoot %d 0 R >>" outlines metadata markinfo outputIntents structTreeRoot;
      endObject ()
    );

    (* Ecriture de xref *)
    flush outChan;
    let xref_pos=pos_out outChan in
    fprintf outChan "xref\n0 %d \n0000000000 65535 f \n" (1+IntMap.cardinal !xref);
    IntMap.iter (fun _ a->fprintf outChan "%010d 00000 n \n" a) !xref;

    (* Trailer *)
    let file_id=(Digest.to_hex (Digest.string fileName)) in
    fprintf outChan "trailer\n<< /Size %d /Root %d 0 R /ID [(%s) (%s)] >>\nstartxref\n%d\n%%%%EOF\n"
      (1+IntMap.cardinal !xref) cat file_id file_id xref_pos;
    close_out outChan;
    Printf.fprintf stderr "File %s written.\n" fileName;
    flush stderr

let output' = output_to_prime output
