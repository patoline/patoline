(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
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
  let out_buf=Rbuffer.create 100000 in
  let buf_pos=ref 0 in
  Zlib.compress (fun zbuf->
    let m=min (Rbuffer.length buf- !buf_pos) (String.length zbuf) in
    for i=0 to m-1 do
      zbuf.[i]<-Rbuffer.nth buf (!buf_pos);
      incr buf_pos
    done;
    m
  )
    (fun buf len -> Rbuffer.add_substring out_buf buf 0 len);
  "/Filter [/FlateDecode]", out_buf
#else
  let stream buf="",buf
#endif


let unicode_to_idiot glUtf=
  let x=UChar.code (UTF8.look glUtf 0) in
  if x>0xff then 1+(x mod 254) else x


let maketype3 pages=
  let rec register_fonts x m=match x with
      []->m
    | (Glyph g)::s->
      let glFont=Fonts.glyphFont g.glyph in
      let glNum=(Fonts.glyphNumber g.glyph).glyph_index in
      let glUtf=(Fonts.glyphNumber g.glyph).glyph_utf8 in
      let name=Fonts.uniqueName glFont in
      let _,rev,f=try (StrMap.find name m) with Not_found->glFont,IntMap.empty,IntMap.empty in
      let ch=unicode_to_idiot glUtf in
      register_fonts s (
        if IntMap.mem glNum f then (
          m
        ) else (
          (* Printf.fprintf stderr "adding %S %S %d\n" glUtf name glNum;flush stderr; *)
          let fontVariant=(try 1+IntMap.find ch rev with Not_found->0) in
          StrMap.add name (
            glFont,
            IntMap.add ch fontVariant rev,
            IntMap.add glNum
              ((Fonts.glyphNumber g.glyph).glyph_utf8,
               fontVariant,
               Fonts.glyphWidth g.glyph,
               Fonts.outlines g.glyph)
              f
          ) m
      ))
    | Link l::s->
      register_fonts s (register_fonts l.link_contents m)
    | States l::s->
      register_fonts s (register_fonts l.states_contents m)
    | h::s->register_fonts s m
  in
  let fonts=Array.fold_left (fun m p->register_fonts (p.pageContents) m) StrMap.empty pages in
  fonts


let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=
  let pages=if Array.length pages>0 then pages else
      [|{pageFormat=(0.,0.);pageContents=[]}|]
  in
  let fileName = filename fileName in
  if Sys.file_exists fileName then Unix.unlink fileName;
  let outChan=open_out_bin fileName in
  let pageBuf=Rbuffer.create 100000 in
  let xref=ref (IntMap.singleton 1 0) in (* Le pagetree est toujours l'objet 1 *)
  (* let fonts=ref StrMap.empty in *)
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
  let pdf_string_buf=Rbuffer.create 1000 in
  let pdf_string utf=
    Rbuffer.clear pdf_string_buf;
    let rec fill i=
      try
        let code= (UTF8.look utf i) in
        (if is_space code then
            Rbuffer.add_string pdf_string_buf "20"
         else if UChar.uint_code code<=0xff then
           Rbuffer.add_string pdf_string_buf (sprintf "%02x" (UChar.uint_code code)));
        fill (UTF8.next utf i)
      with
          _->()
    in
    fill 0;
    Rbuffer.contents pdf_string_buf
  in
  let pageObjects=Array.make (Array.length pages) 0 in
  for i=0 to Array.length pageObjects-1 do pageObjects.(i)<-futureObject ()
  done;

  fprintf outChan "%%PDF-1.7\n%%ãõẽũ\n";


  let rec are_valid x i=
    if i>=Array.length x then true else
      if x.(i) < infinity && x.(i)> -.infinity then are_valid x (i+1) else false
  in
  let writePath buf pt_of_mm paths params=
    List.iter (fun path->
      if Array.length path > 0 then (
        let (x0,y0)=path.(0) in
        if are_valid x0 0 && are_valid y0 0 then (
          Rbuffer.add_string buf (sprintf "%f %f m " (pt_of_mm x0.(0)) (pt_of_mm y0.(0)));
          Array.iter (
            fun (x,y)->if are_valid x 0 && are_valid y 0 then (
              if Array.length x<=2 && Array.length y<=2 then (
                let x1=if Array.length x=2 then x.(1) else x.(0) in
                let y1=if Array.length y=2 then y.(1) else y.(0) in
                Rbuffer.add_string buf (sprintf "%g %g l " (pt_of_mm x1) (pt_of_mm y1));
              ) else if Array.length x=3 && Array.length y=3 then (
                Rbuffer.add_string buf (sprintf "%g %g %g %g %g %g c "
                                          (pt_of_mm ((x.(0)+.2.*.x.(1))/.3.)) (pt_of_mm ((y.(0)+.2.*.y.(1))/.3.))
                                          (pt_of_mm ((2.*.x.(1)+.x.(2))/.3.)) (pt_of_mm ((2.*.y.(1)+.y.(2))/.3.))
                                          (pt_of_mm x.(2)) (pt_of_mm y.(2)));
              ) else if Array.length x=4 && Array.length y=4 then (
                Rbuffer.add_string buf (sprintf "%g %g %g %g %g %g c "
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
  in



  (* Type 3 *)
  let type3Fonts=maketype3 pages in
  (* let code3 glyph= *)
  (*   (Fonts.glyphNumber glyph).glyph_utf8 *)
  (* in *)
  let pdftype3=
    StrMap.fold (fun k (font,var,glyphs) m->
      let maxVar=IntMap.fold (fun _ x m->max x m) var 0 in
      let fonts=Array.make (maxVar+1) (IntMap.empty) in
      IntMap.iter (fun k (a,b,c,d)->
        fonts.(b)<-IntMap.add k (a,c,d) fonts.(b)
      ) glyphs;
      let rec writeFonts i m=if i<=maxVar then (
        let x0=ref infinity and x1=ref (-.infinity) and y0=ref infinity and y1=ref (-.infinity) in
        IntMap.iter (fun _ (_,_,out)->
          List.iter (List.iter (fun (x,y)->
            Array.iter (fun u->x0:=min u !x0;x1:=max u !x1) x;
            Array.iter (fun u->y0:=min u !y0;y1:=max u !y1) y
          )) out
        ) fonts.(i);
        let pdffont=beginObject () in
        fprintf outChan "<< /Type /Font /Subtype /Type3 ";
        (* fprintf outChan "/Name %s " name; *)
        (if !x0<infinity && !x1>(-.infinity) && !y0<infinity && !y1>(-.infinity) then
            fprintf outChan "/FontBBox [%g %g %g %g] " !x0 !y0 !x1 !y1
         else
            fprintf outChan "/FontBBox [0 0 0 0] ");
        fprintf outChan "/FontMatrix [0.001 0 0 0.001 0 0] ";
        let charprocs_obj=futureObject () in
        fprintf outChan "/CharProcs %d 0 R " charprocs_obj;
        let enc=futureObject () in
        fprintf outChan "/Encoding %d 0 R " enc;

        let firstChar=IntMap.fold (fun _ (utf8,_,_) m->min (unicode_to_idiot utf8) m)
          fonts.(i) max_int
        in
        let lastChar=IntMap.fold (fun _ (utf8,_,_) m->max (unicode_to_idiot utf8) m)
          fonts.(i) min_int
        in
        fprintf outChan "/FirstChar %d " firstChar;
        fprintf outChan "/LastChar %d " lastChar;
        let widths=futureObject () in
        fprintf outChan "/Widths %d 0 R " widths;
        fprintf outChan ">>";
        endObject();


        let revFont=
          IntMap.fold (fun c (utf8,w,_) m->
            IntMap.add (unicode_to_idiot utf8) (c,w) m
          ) fonts.(i) IntMap.empty
        in
        resumeObject widths;
        fprintf outChan "[";
        for i=firstChar to lastChar do
          fprintf outChan "%g " (try let _,w=IntMap.find i revFont in w with Not_found->0.)
        done;
        fprintf outChan "]";
        endObject ();


        resumeObject charprocs_obj;
        fprintf outChan "<< ";
        let charprocs=IntMap.fold (fun _ (utf8,w,outlines) m->
          let o=futureObject () in
          fprintf outChan "/uni%04x %d 0 R " (unicode_to_idiot utf8) o;(* (UChar.code (UTF8.look utf8 0)) o; *)
          (o,w,outlines)::m
        ) fonts.(i) []
        in
        fprintf outChan ">>";
        endObject ();

        let bu=Rbuffer.create 1000 in
        List.iter (fun (o,w,outlines)->
          Rbuffer.clear bu;

          x0:=infinity;
          x1:=(-.infinity);
          y0:=infinity;
          y1:=(-.infinity);
          List.iter (List.iter (fun (x,y)->
            Array.iter (fun u->x0:=min u !x0;x1:=max u !x1) x;
            Array.iter (fun u->y0:=min u !y0;y1:=max u !y1) y
          )) outlines;
          Rbuffer.add_string bu (sprintf "%g %d %g %g %g %g d1 " w 0 !x0 !y0 !x1 !y1);
          writePath bu (fun x->x)(List.map (Array.of_list) outlines) OutputCommon.default;
          Rbuffer.add_string bu "f ";
          let filt, data=stream bu in
          let len=Rbuffer.length data in
          resumeObject o;
          fprintf outChan "<< /Length %d %s>>\nstream\n" len filt;
          Rbuffer.output_buffer outChan data;
          fprintf outChan "\nendstream";
          endObject ();
        ) charprocs;

        resumeObject enc;
        fprintf outChan "<< /Type /Encoding /Differences [";
        IntMap.iter (fun _ (utf8,_,_)->
          fprintf outChan "%d /uni%04x "
            (unicode_to_idiot utf8)
            (unicode_to_idiot utf8)
            (* (UChar.code (UTF8.look utf8 0)) *)
            (* (UChar.code (UTF8.look utf8 0)) *)
        ) fonts.(i);
        fprintf outChan "]>>";
        endObject ();
        let formerFont=try StrMap.find k m with Not_found->IntMap.empty in
        let variant=IntMap.fold (fun u v w->
          IntMap.add u pdffont w
        ) fonts.(i) formerFont
        in
        writeFonts (i+1) (StrMap.add k variant m)
      ) else m
      in
      writeFonts 0 m
    ) type3Fonts StrMap.empty
  in
  (* /Type 3 *)





  for page=0 to Array.length pages-1 do
    Rbuffer.reset pageBuf;
    let pageLinks=ref [] in
    let pageImages=ref [] in
    let pageFonts=ref IntMap.empty in
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
      if !openedWord then (Rbuffer.add_string pageBuf ")"; openedWord:=false);
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
      | Animation(r,_,_,_) ->
	List.iter output_contents r
      | Glyph gl->(
        change_non_stroking_color gl.glyph_color;
        if not !isText then Rbuffer.add_string pageBuf " BT ";
        isText:=true;
        let gx=pt_of_mm gl.glyph_x in
        let gy=pt_of_mm gl.glyph_y in
        let gx=match classify_float gx with FP_nan | FP_infinite->0.
          | _->gx
        in
        let gy=match classify_float gy with FP_nan | FP_infinite->0.
          | _->gy
        in
        let size=pt_of_mm gl.glyph_size in

        let fnt=Fonts.glyphFont (gl.glyph) in
        (* Inclusion de la police sur la page *)
        let idx=
          let pdfFont=StrMap.find (Fonts.uniqueName fnt) pdftype3 in
          let glidx=(Fonts.glyphNumber gl.glyph).glyph_index in
          let pdfFont=try IntMap.find glidx pdfFont with Not_found->0 in
          try
            IntMap.find pdfFont !pageFonts
          with
              Not_found->(
                let card=IntMap.cardinal !pageFonts in
                pageFonts := IntMap.add pdfFont card !pageFonts;
                card
              )
        in
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
            if !openedWord then (Rbuffer.add_string pageBuf ")"; openedWord:=false);
            Rbuffer.add_string pageBuf str;
            xline:= !xline -. size*.(float_of_string str)/.1000.;
          )
        );
        if not !openedWord then (Rbuffer.add_string pageBuf "("; openedWord:=true);
        let utf8=(Fonts.glyphNumber gl.glyph).glyph_utf8 in
        let c=char_of_int (unicode_to_idiot utf8) in
        if c='\\' || c='(' || c=')' then (
          Rbuffer.add_char pageBuf '\\';
          Rbuffer.add_char pageBuf c;
        ) else (
          Rbuffer.add_char pageBuf c
        );
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
        writePath pageBuf pt_of_mm paths params
      )
      | Link l->(
        pageLinks:= l:: !pageLinks;
        List.iter output_contents l.link_contents
      )
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
      | States s->List.iter output_contents s.states_contents
      | _->()
    in
    let sorted_pages=

        let x=List.fold_left (fun m x->
          let m'=try IntMap.find (drawing_order x) m with Not_found->[] in
          IntMap.add (drawing_order x) (x::m') m
        ) IntMap.empty pages.(page).pageContents
        in
        let comp a b=match a,b with
            Glyph ga,Glyph gb->if ga.glyph_y=gb.glyph_y then compare ga.glyph_x gb.glyph_x
              else compare gb.glyph_y ga.glyph_y
          | Glyph ga,_-> -1
          | _,Glyph gb->1
          | _->0
        in
        let subsort a=match a with
            Link l->Link { l with link_contents=List.sort comp l.link_contents }
          | b->b
        in
        IntMap.fold (fun _ a x->x@a) (IntMap.map (fun l->(List.sort comp (List.map subsort l))) x) []
      in
        List.iter output_contents sorted_pages;
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
            if IntMap.cardinal !pageFonts >0 then (
              fprintf outChan " /Font << ";
              IntMap.iter (fun a b->fprintf outChan "/F%d %d 0 R " b a) !pageFonts;
              fprintf outChan ">> "
            );
            fprintf outChan ">> /Contents %d 0 R " contentObj;

            if !pageLinks <> [] then (
              fprintf outChan "/Annots [ ";
              List.iter (fun l->
                let inf0=match classify_float l.link_x0 with FP_nan | FP_infinite->false
                  | _->true in
                let inf1=match classify_float l.link_x1 with FP_nan | FP_infinite->false
                  | _->true in
                let inf2=match classify_float l.link_y0 with FP_nan | FP_infinite->false
                  | _->true in
                let inf3=match classify_float l.link_y1 with FP_nan | FP_infinite->false
                  | _->true in
                if inf0 && inf1 && inf2 && inf3 then
                  if l.is_internal && l.dest_page>=0 && l.dest_page<Array.length pageObjects then
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
                                          let a=(float_of_int rgb.Images.alpha)/.256. in
                                          let r=(1.-.a)*.255.
                                            +.a*.float_of_int rgb.Images.color.Images.r in
                                          let g=(1.-.a)*.255.
                                            +.a*.float_of_int rgb.Images.color.Images.g in
                                          let b=(1.-.a)*.255.
                                            +.a*.float_of_int rgb.Images.color.Images.b in
                                          Rbuffer.add_char img_buf (char_of_int (round r));
                                          Rbuffer.add_char img_buf (char_of_int (round g));
                                          Rbuffer.add_char img_buf (char_of_int (round b));
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

  (*
    (* Tous les dictionnaires de unicode mapping *)
    let defaultutf8=String.make 1 (char_of_int 0) in
    let glyphutf8 x=
      let n=Fonts.glyphNumber x in
      if n.glyph_utf8="" then defaultutf8 else n.glyph_utf8
    in
    StrMap.iter (fun _ x->
      if x.fontToUnicode>=0 then (
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
            (* On commence par partitionner par premier octet (voir adobe technical note #5411) *)
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
        Rbuffer.add_string buf "endcmap\nCMapName currentdict /CMap defineresource pop\nend end\n";

        resumeObject x.fontToUnicode;
        let filt, data=stream buf in
        let len=Rbuffer.length data in
        fprintf outChan "<< /Length %d %s>>\nstream\n" len filt;
        Rbuffer.output_buffer outChan data;
        fprintf outChan "\nendstream";
        endObject ()
      )
    ) !fonts;

  *)

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
          fprintf outChan "<< /Title <%s> /Parent %d 0 R " (pdf_string str.substructures.(i).name) par;
          if i>0 then fprintf outChan "/Prev %d 0 R " hijosObjs.(i-1);
          if i<Array.length str.substructures-1 then fprintf outChan "/Next %d 0 R " hijosObjs.(i+1);
          if a>0 then
            fprintf outChan "/First %d 0 R /Last %d 0 R /Count %d "
              a b (Array.length str.substructures.(i).substructures);

          if str.substructures.(i).page>=0 && str.substructures.(i).page<Array.length pageObjects then
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
      let a,b=make_outlines { name="";
                              page=0; struct_x=0.; struct_y=0.;
                              substructures=[|structure|];
                              metadata=[];
                              tags=[];
                              displayname=[] } outlines
      in
      (*let a,b=make_outlines structure outlines in*)

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
