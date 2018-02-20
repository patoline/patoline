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
open FTypes
open Extra
open FUtil
open CFF

let extensions = [".otf"; ".ttf"]

let offsetTable=12
let dirSize=16
exception Table_not_found of string

type ttf={
  ttf_version:string;
  ttf_file:string;
  ttf_offset:int;
  ttf_cadratin:float;
}
type cff={
  cff_font:CFF.font;
  cff_offset:int
}
type font =
    CFF of cff
  | TTF of ttf

type ttfglyph={ ttf_font:ttf;
                ttf_glyph_id:glyph_id;
                mutable ttf_width:float;
                mutable ttf_x0:float;
                mutable ttf_x1:float;
                mutable ttf_y0:float;
                mutable ttf_y1:float;
              }


type glyph = CFFGlyph of (cff*CFF.glyph) | TTFGlyph of ttfglyph

let tableLookup table file off=
  let table = Bytes.of_string table in
  seek_in file (off+4);
  let numTables=readInt2 file in
  let tableName = Bytes.make 4 ' ' in
  let rec lookup i j=
    let middle=(i+j) / 2 in
      seek_in file (off+offsetTable+middle*dirSize);
      really_input file tableName 0 4;
      if middle<=i then
        if tableName=table then
          ((seek_in file (off+offsetTable+i*dirSize+8);readInt4_int file),
           (seek_in file (off+offsetTable+i*dirSize+12);readInt4_int file))
        else
          raise (Table_not_found (Bytes.to_string table))
      else
        if compare tableName table <=0 then
          lookup middle j
        else
          lookup i middle
  in
    lookup 0 numTables

let tableList file off=
  seek_in file (off+4);
  let numTables=readInt2 file in
  let rec getTables n l=
    if n=offsetTable then l else
      (seek_in file (off+n);
       let newTable=Bytes.create 4 in
       really_input file newTable 0 4;
       getTables (n-dirSize) ((Bytes.to_string newTable)::l))
  in
    getTables (off+dirSize*(numTables-1)+offsetTable) []

let fontBBox f=
  match f with
    CFF font->
      CFF.fontBBox font.cff_font
    | TTF ttf->(
      let file,offset0=open_in_bin_cached ttf.ttf_file,ttf.ttf_offset in
      let (a,b)=tableLookup "hhea" file offset0 in
      seek_in file (a+4);
      let ascender=sreadInt2 file in
      let descender=sreadInt2 file in
      seek_in file (a+12);
      let lsb=sreadInt2 file in
      (* let rsb=sreadInt2 file in *)
      seek_in file (a+16);
      let xmax=sreadInt2 file in
      (lsb,descender,xmax,ascender)
    )

let cadratin f =
  let res =
    match f with
    | CFF font-> 1000
    | TTF ttf ->
      let file,offset0=open_in_bin_cached ttf.ttf_file,ttf.ttf_offset in
      let (a,b)=tableLookup "head" file offset0 in
      seek_in file (a+18);
      sreadInt2 file
  in
  Printf.printf "cadratin: %d\n%!" res;
  res

let loadFont ?offset:(off=0) ?size:(size=None) file=
  let f = open_in_bin_cached file in
  let typ = Bytes.create 4 in
  seek_in f off;
  really_input f typ 0 4;
  match Bytes.to_string typ with
  | "OTTO"  ->
     let (a,b)=tableLookup "CFF " f off in
     CFF {cff_font=CFF.loadFont file ~offset:(off+a) ~size:(Some b);cff_offset=off}
  | version ->
     let ttf = { ttf_version=version; ttf_offset=off;
                 ttf_file=file; ttf_cadratin= 0.0 }
     in
     let ttf_cadratin =
       let c = cadratin (TTF ttf) in
       if c = 1000 then 1.0 else 1000.0 /. float_of_int c
     in
     TTF { ttf with ttf_cadratin }

let uniqueName font=match font with
    CFF cff->CFF.uniqueName cff.cff_font
  | TTF ttf->ttf.ttf_file

let getNames font off=
  try
    let file=open_in_bin_cached font in
    let (a,b)=tableLookup "name" file off in
    seek_in file a;
    let format=readInt2 file in
    if format=0 then (
      let count=readInt2 file in
      let stringOffset=readInt2 file in
      let rec get_names i m=
        if i>=count then m else (
          seek_in file (a+6+12*i);
          let platformID=readInt2 file in
          let encodingID=readInt2 file in
          let languageID=readInt2 file in
          let nameID=readInt2 file in
          let length=readInt2 file in
          let offset=readInt2 file in
          let str=Bytes.create length in
          seek_in file (a+stringOffset+offset);
          really_input file str 0 length;
          let m'=
            try
              match platformID,encodingID with
                  1,0->(
                    let utf8 = ROMAN.to_utf8 (Bytes.to_string str) in
                    ((languageID,nameID,utf8)::m)
                  )
                | 3,1->(                (* UTF-16 *)
                  let utf8 = UTFConvert.utf16_to_utf8 (Bytes.to_string str) in
                  ((languageID,nameID,utf8)::m)
                )
                | a,b->(
                  Printf.fprintf stderr "encoding : %d %d\n" a b;
                  m
                )
            with
                Not_found->m
          in
          get_names (i+1) m'
        )
      in
      get_names 0 []
    ) else []
  with
      Not_found->[]





let fontName ?index:(idx=0) f =
  match f with
      CFF x->CFF.fontName x.cff_font ~index:idx
    | TTF ttf->
      let name={postscript_name="";full_name="";family_name="";subfamily_name=""} in
      let rec getName l m=match l with
          []->m
        | (lang,nameID,h)::s->(
          let name=try IntMap.find lang m with Not_found->name in
          (match nameID with
              1->name.family_name<-h
            | 2->name.subfamily_name<-h
            | 4->name.full_name<-h
            | 6->name.postscript_name<-h
            | _->());
          getName s (IntMap.add lang name m)
        )
      in
      let lang=getName (getNames ttf.ttf_file ttf.ttf_offset) IntMap.empty in
      snd (IntMap.min_binding lang)


let cardinal f=
  let file,offset0=match f with
      CFF font->
        let file=open_in_bin_cached font.cff_font.file in
        file,font.cff_offset
    | TTF ttf->open_in_bin_cached ttf.ttf_file,ttf.ttf_offset
  in
  let (a,b)=tableLookup "maxp" file offset0 in
  seek_in file (a+4);
  readInt2 file

let italicAngle f=
  let file,offset0=match f with
      CFF font->
        let file=open_in_bin_cached font.cff_font.file in
        file,font.cff_offset
    | TTF ttf->open_in_bin_cached ttf.ttf_file,ttf.ttf_offset
  in
  let (a,b)=tableLookup "post" file offset0 in
  seek_in file (a+4);
  (float_of_int (readInt4_int file))/.65536.

let ascender f=
  let file,offset0=match f with
      CFF font->
        let file=open_in_bin_cached font.cff_font.file in
        file,font.cff_offset
    | TTF ttf->open_in_bin_cached ttf.ttf_file,ttf.ttf_offset
  in
  let (a,b)=tableLookup "OS/2" file offset0 in
  seek_in file (a+68);
  float_of_int (sreadInt2 file)

let descender f=
  let file,offset0=match f with
      CFF font->
        let file=open_in_bin_cached font.cff_font.file in
        file,font.cff_offset
    | TTF ttf->open_in_bin_cached ttf.ttf_file,ttf.ttf_offset
  in
  let (a,b)=tableLookup "OS/2" file offset0 in
  seek_in file (a+70);
  float_of_int (sreadInt2 file)


let glyph_of_uchar font0 char0=
  let file,offset0=match font0 with
      CFF (font)->font.cff_font.file, font.cff_offset
    | TTF ttf->ttf.ttf_file,ttf.ttf_offset
  in
  let file=open_in_bin_cached file in
  let char=UChar.code char0 in
  let (a,b)=tableLookup "cmap" file offset0 in
  seek_in file (a+2);
  let numTables=readInt2 file in

  let rec read_tables table=
    if table>=numTables then 0 else (
      seek_in file (a+8+8*table);
      let offset=a+readInt4_int file in
      seek_in file offset;
      let t=readInt2 file in
      (match t with
          0->if char<256 then (
            seek_in file (offset+6+char);
            let cid=input_byte file in
            if cid<>0 then cid else read_tables (table+1)
          ) else read_tables (table+1)
        | 2->
          (let i=(char lsr 8) land 0xff in
           let j=char land 0xff in
           let k=(seek_in file (offset+6+i*2); readInt2 file) lsr 3 in
           let subHeaders=offset+6+256*2+k*8 in
           if k=0 then
             (seek_in file (subHeaders+6);
              let idRangeOffset=readInt2 file in
              seek_in file (subHeaders+idRangeOffset+i*2);
              let cid=readInt2 file in
              if cid<>0 then cid else read_tables (table+1)
             )
           else
             (let firstCode=seek_in file subHeaders; readInt2 file in
              let entryCount=seek_in file (subHeaders+2); readInt2 file in
              let idDelta=seek_in file (subHeaders+4); readInt file 2 in
              let idRangeOffset=seek_in file (subHeaders+6); readInt2 file in
              if j>=firstCode && j < (firstCode+entryCount) then
                (let p=seek_in file (subHeaders+8+idRangeOffset+j*2); readInt2 file in
                 let cid=if p=0 then p else (p+idDelta) land 0xffff in
                 if cid<>0 then cid else read_tables (table+1))
              else
                read_tables (table+1)
             )
          )
        | 4->(
          let sc2=seek_in file (offset+6); readInt2 file in
          let rec smallestEnd i j=
            if j<=i then i else
              let middle=((i+j) lsr 1) land 0xfffe in
              let end_=seek_in file (offset+14+middle); readInt2 file in
              if char>end_ then
                smallestEnd (middle+2) j
              else
                smallestEnd i middle
          in
          let seg=smallestEnd 0 (sc2-2) in (* 2*numéro de segment *)
          let start=seek_in file (offset+16+sc2+seg); readInt2 file in
          if char>=start then (
            let delta=seek_in file (offset+16+2*sc2+seg); sreadInt2 file in
            let p_idrOffset=offset+16+3*sc2+seg in
            let idrOffset=seek_in file p_idrOffset; readInt2 file in
            let cid=
              if idrOffset=0 then
                (char+delta+0x10000) land 0xffff
              else (
                seek_in file (idrOffset+2*(char-start)+p_idrOffset);
                (readInt2 file+delta+0x10000) land 0xffff
              )
            in
            if cid<>0 then cid else read_tables (table+1)
          ) else read_tables (table+1)
        )
        | 6->
          (seek_in file (offset+6);
           let first=readInt2 file in
           let entryCount=readInt2 file in
           if first<=char && char <first+entryCount then
             (seek_in file (offset+10+(char-first)*2);
              let cid=readInt2 file in
              if cid<>0 then cid else read_tables (table+1))
           else
             read_tables (table+1)
          )
        | _->read_tables (table+1)
      ))
  in
  let cid=read_tables 0 in
  if cid = 0 then
    raise (Glyph_not_found ((fontName font0).full_name, UTF8.init 1 (fun _->char0)))
  else
    cid


let glyph_of_char f c=glyph_of_uchar f (UChar.of_char c)

let read_cmap font=
  let file,offset0=match font with
      CFF (font)->font.cff_font.file, font.cff_offset
    | TTF ttf->ttf.ttf_file,ttf.ttf_offset
  in
  let file=open_in_bin_cached file in
  let (a,b)=tableLookup "cmap" file offset0 in
  Cmap.read_cmap file a


let glyphFont f=match f with
    CFFGlyph (x,_)->CFF x
  | TTFGlyph (g)->TTF g.ttf_font
let loadGlyph f ?index:(idx=0) gl=
  match f with
      CFF (x)->CFFGlyph (x, CFF.loadGlyph x.cff_font ~index:idx gl)
    | TTF ttf->TTFGlyph { ttf_font=ttf;ttf_glyph_id=gl;
                          ttf_width=infinity;
                          ttf_y0=infinity;
                          ttf_y1= -.infinity;
                          ttf_x0=infinity;
                          ttf_x1= -.infinity }


(* Interpréteur truetype *)

#define TT_ON_CURVE 1
#define TT_XSHORT_VECTOR 2
#define TT_YSHORT_VECTOR 4
#define TT_REPEAT 8
#define TT_SAME_X 16
#define TT_SAME_Y 32

#define TT_ARGS_ARE_WORDS                          1
#define TT_ARGS_ARE_XY_VALUES                      2
#define TT_ROUND_XY_TO_GRID                        4
#define TT_WE_HAVE_A_SCALE                         8
#define TT_MORE_COMPONENTS                        32
#define TT_WE_HAVE_AN_X_AND_Y_SCALE               64
#define TT_WE_HAVE_A_TWO_BY_TWO                  128
#define TT_WE_HAVE_INSTRUCTIONS                  256
#define TT_USE_MY_METRICS                        512
#define TT_OVELAP_COMPOUND                      1024
#define TT_SCALED_COMPONENTS_OFFSET             2048
#define TT_UNSCALED_COMPONENTS_OFFSET           4096


let read2dot14 file=
  let x=input_byte file in
  let y=input_byte file in
  let m=(x lsr 6) in
  let mantissa=float_of_int (if m>=0x10 then m-0x100 else m) in
  let frac=(float_of_int (((x land 0x3fff) lsl 8) lor y))/.(float_of_int 0x4000) in
  mantissa+.frac

let outlines ?(orig=true) gl=match gl with
    CFFGlyph (_,x)->CFF.outlines x
  | TTFGlyph (ttfgl)->(
    let file=open_in_bin_cached ttfgl.ttf_font.ttf_file in
    let locformat=
      let (a,b)=tableLookup "head" file ttfgl.ttf_font.ttf_offset in
      seek_in file (a+50);
      readInt2 file
    in

    let (a,b)=tableLookup "loca" file ttfgl.ttf_font.ttf_offset in
    let (c,d)=tableLookup "glyf" file ttfgl.ttf_font.ttf_offset in

    let rec fetch_outlines glyph_index=
      if locformat=0 then seek_in file (a+glyph_index*2) else seek_in file (a+glyph_index*4);
      let off=if locformat=0 then (readInt2 file) lsl 1 else readInt4_int file in
      seek_in file (c+off);
      let numberOfContours=sreadInt2 file in
      if numberOfContours>0 then (        (* simple glyph *)
        seek_in file (c+off+10);
        let rec read_contours i l=
          if i>=numberOfContours then (
            match l with
                h::_->h,List.rev l
              | _-> -1,l
          ) else (
            let j=readInt2 file in
            read_contours (i+1) (j::l)
          )
        in
        let lastPoint,contours=read_contours 0 [] in
        let instr_length=readInt2 file in
        seek_in file (pos_in file+instr_length);
        let flags=Array.make (lastPoint+1) 0 in
        let rec read_flags n=
          if n<=lastPoint then (
            flags.(n)<-input_byte file;
            let rep=if flags.(n) land TT_REPEAT <> 0 then (
              let n_repeat=input_byte file in
              for i=1 to n_repeat do
                flags.(n+i)<-flags.(n)
              done;
              n_repeat
            ) else 0
            in
            read_flags (n+rep+1)
          )
        in
        let x=Array.make (lastPoint+1) 0 in
        let y=Array.make (lastPoint+1) 0 in
        read_flags 0;
        for i=0 to lastPoint do
          x.(i)<-if flags.(i) land TT_XSHORT_VECTOR<>0 then (
            if flags.(i) land TT_SAME_X <>0 then input_byte file else -(input_byte file)
          ) else (
            if flags.(i) land TT_SAME_X <>0 then 0 else sreadInt2 file
          )
        done;
        for i=0 to lastPoint do
          y.(i)<-if flags.(i) land TT_YSHORT_VECTOR<>0 then (
            if flags.(i) land TT_SAME_Y <>0 then input_byte file else -(input_byte file)
          ) else (
            if flags.(i) land TT_SAME_Y <>0 then 0 else sreadInt2 file
          )
        done;
        let finalx=ref IntMap.empty in
        let finaly=ref IntMap.empty in

        let rec build_outlines x0 y0 outlines contours=
          match contours with
              [] | [_]->outlines
            | h::hh::s->(
              let ox0=x0+.float_of_int x.(h) in
              let oy0=y0+.float_of_int y.(h) in
              finalx:=IntMap.add h ox0 !finalx;
              finaly:=IntMap.add h oy0 !finaly;

              (* Courbe qui commence en i *)
              let rec contour lastx lasty x0 y0 i l=
                if i>=hh then lastx,lasty,l else (
                  if i+1>hh || flags.(i+1) land TT_ON_CURVE<>0 then (
                    (* segment de droite *)
                    let x1,y1=if i+1>hh then ox0,oy0 else
                        (lastx+.float_of_int x.(i+1),
                         lasty+.float_of_int y.(i+1))
                    in
                    finalx:=IntMap.add (i+1) x1 !finalx;
                    finaly:=IntMap.add (i+1) y1 !finaly;
                    contour x1 y1 x1 y1 (i+1) (([|x0;x1|],[|y0;y1|])::l)
                  ) else (
                    if i+2>hh then (
                      let x1=lastx+.float_of_int x.(i+1) in
                      let y1=lasty+.float_of_int y.(i+1) in
                    finalx:=IntMap.add (i+1) x1 !finalx;
                    finaly:=IntMap.add (i+1) y1 !finaly;
                      contour x1 y1 ox0 oy0 (i+2) (([|x0;x1;ox0|],[|y0;y1;oy0|])::l)
                    ) else (
                      if flags.(i+2) land TT_ON_CURVE<>0 then (
                        let x1=lastx+.float_of_int x.(i+1) in
                        let y1=lasty+.float_of_int y.(i+1) in
                        finalx:=IntMap.add (i+1) x1 !finalx;
                        finaly:=IntMap.add (i+1) y1 !finaly;
                        let x2=x1+.float_of_int x.(i+2) in
                        let y2=y1+.float_of_int y.(i+2) in
                        finalx:=IntMap.add (i+2) x2 !finalx;
                        finaly:=IntMap.add (i+2) y2 !finaly;
                        contour x2 y2 x2 y2 (i+2) (([|x0;x1;x2|],[|y0;y1;y2|])::l)
                      ) else (
                        let x1=lastx+.float_of_int x.(i+1) in
                        let y1=lasty+.float_of_int y.(i+1) in
                        finalx:=IntMap.add (i+1) x1 !finalx;
                        finaly:=IntMap.add (i+1) y1 !finaly;
                        let x2_=x1+.float_of_int x.(i+2) in
                        let y2_=y1+.float_of_int y.(i+2) in
                        let x2=(x1+.x2_)/.2. in
                        let y2=(y1+.y2_)/.2. in
                        contour x1 y1 x2 y2 (i+1) (([|x0;x1;x2|],[|y0;y1;y2|])::l)
                      )
                    )
                  )
                )
              in
              let x1,y1,l=contour ox0 oy0 ox0 oy0 h [] in
              let l=List.rev (if x1<>ox0 || y1<>oy0 then ([|x1;ox0|],[|y1;oy0|])::l else l) in
              build_outlines x1 y1 (l::outlines) ((hh+1)::s)
            )
        in
        let x=build_outlines 0. 0. [] (0::contours) in
        List.rev x, !finalx, !finaly

      ) else (
        (* Composite *)
        seek_in file (c+off+10);
        let rec make_composite outlines0 mx my=
          let flags=readInt2 file in
          let glyphIndex'=readInt2 file in
          let readArg ()=
            if flags land TT_ARGS_ARE_WORDS<>0 then
              if flags land TT_ARGS_ARE_XY_VALUES <>0 then
                sreadInt2 file
              else
                readInt2 file
            else
              let b=input_byte file in
              if flags land TT_ARGS_ARE_XY_VALUES <>0 then
                if b>=0x80 then b-0x100 else b
              else
                b
          in
          let arg1=readArg () in
          let arg2=readArg () in

          let pos=pos_in file in
          let outlines,finalx,finaly=fetch_outlines glyphIndex' in
          let mx'=IntMap.fold (fun k a m->IntMap.add (IntMap.cardinal m) a m) finalx mx in
          let my'=IntMap.fold (fun k a m->IntMap.add (IntMap.cardinal m) a m) finaly my in
          seek_in file pos;

          let a,b,c,d=
            if flags land TT_WE_HAVE_A_SCALE<>0 then
              let sc=read2dot14 file in (sc,0.,0.,sc)
            else
              if flags land TT_WE_HAVE_AN_X_AND_Y_SCALE<>0 then
                let scx=read2dot14 file in
                let scy=read2dot14 file in
                (scx,0.,0.,scy)
              else
                if flags land TT_WE_HAVE_A_TWO_BY_TWO<>0 then
                  let a=read2dot14 file in
                  let b=read2dot14 file in
                  let c=read2dot14 file in
                  let d=read2dot14 file in
                  (a,b,c,d)
                else
                  (1.,0.,0.,1.)
          in

          let m=max (abs_float a) (abs_float b) in
          let m=if abs_float (abs_float a-.abs_float c) <= 33./.65536. then m*.2. else m in
          let n=max (abs_float c) (abs_float d) in
          let n=if abs_float (abs_float c-.abs_float d) <= 33./.65536. then n*.2. else n in

          let tx,ty=if flags land TT_ARGS_ARE_XY_VALUES<>0 then float_of_int arg1,float_of_int arg2 else
              (IntMap.find arg1 mx -. IntMap.find arg2 finalx,
               IntMap.find arg1 my -. IntMap.find arg2 finaly)
          in
          List.iter (List.iter (fun (x,y)->
            for i=0 to Array.length x-1 do
              x.(i)<-m*.(a*.x.(i)/.m +. c*.y.(i)/.m) +.tx;
              y.(i)<-n*.(b*.x.(i)/.n +. d*.y.(i)/.n) +.ty;
            done
          )) outlines;
          if flags land TT_MORE_COMPONENTS <> 0 then (
            make_composite (outlines@outlines0) mx' my'
          ) else (outlines@outlines0, mx',my')
        in
        make_composite [] IntMap.empty IntMap.empty
      )
    in
    let out,_,_=fetch_outlines ttfgl.ttf_glyph_id.glyph_index in
    if orig then out else
      List.map (List.map (fun (xs,ys) ->
                    let f x = x *. ttfgl.ttf_font.ttf_cadratin in
                    (Array.map f xs, Array.map f ys))) out
  )



let compute_bb gl=
  List.iter (List.iter (fun (x,y)->
    let (a,b)=FBezier.bernstein_extr x in
    let (c,d)=FBezier.bernstein_extr y in
    gl.ttf_x0<-min gl.ttf_x0 a;
    gl.ttf_x1<-max gl.ttf_x1 b;
    gl.ttf_y0<-min gl.ttf_y0 c;
    gl.ttf_y1<-max gl.ttf_y1 d)
  )
    (outlines (TTFGlyph gl))


let glyph_y0 ?(orig=true) gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_y0 x
  | TTFGlyph ttfgl->
    if ttfgl.ttf_y0 = infinity then compute_bb ttfgl;
    if orig then ttfgl.ttf_y0 else ttfgl.ttf_y0 *. ttfgl.ttf_font.ttf_cadratin
let glyph_x0 ?(orig=true) gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_x0 x
  | TTFGlyph ttfgl->
    if ttfgl.ttf_x0 = infinity then compute_bb ttfgl;
    if orig then ttfgl.ttf_x0 else ttfgl.ttf_x0 *. ttfgl.ttf_font.ttf_cadratin
let glyph_y1 ?(orig=true) gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_y1 x
  | TTFGlyph ttfgl->
    if ttfgl.ttf_y1 = infinity then compute_bb ttfgl;
    if orig then ttfgl.ttf_y1 else ttfgl.ttf_y1 *. ttfgl.ttf_font.ttf_cadratin
let glyph_x1 ?(orig=true) gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_x1 x
  | TTFGlyph ttfgl->
    if ttfgl.ttf_x1 = infinity then compute_bb ttfgl;
    if orig then ttfgl.ttf_x1 else ttfgl.ttf_x1 *. ttfgl.ttf_font.ttf_cadratin

let font_glyph_y0 gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_y0 x
  | TTFGlyph (ttfgl)->(
      let file=open_in_bin_cached ttfgl.ttf_font.ttf_file in
      let off_size=
        let (a,b)=tableLookup "head" file ttfgl.ttf_font.ttf_offset in
        seek_in file (a+50);
        if readInt2 file=0 then 2 else 4
      in

      let (a,b)=tableLookup "loca" file ttfgl.ttf_font.ttf_offset in
      seek_in file (a+ttfgl.ttf_glyph_id.glyph_index*off_size);
      let off=
        let off=readInt2 file in
        if off_size=2 then 2*off else off
      in
      let (c,d)=tableLookup "glyf" file ttfgl.ttf_font.ttf_offset in
      seek_in file (c+off+4);
      float_of_int (sreadInt2 file)
  )

let font_glyph_y1 gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_y1 x
  | TTFGlyph (ttfgl)->(
      let file=open_in_bin_cached ttfgl.ttf_font.ttf_file in
      let off_size=
        let (a,b)=tableLookup "head" file ttfgl.ttf_font.ttf_offset in
        seek_in file (a+50);
        if readInt2 file=0 then 2 else 4
      in

      let (a,b)=tableLookup "loca" file ttfgl.ttf_font.ttf_offset in
      seek_in file (a+ttfgl.ttf_glyph_id.glyph_index*off_size);
      let off=
        let off=readInt2 file in
        if off_size=2 then 2*off else off
      in
      let (c,d)=tableLookup "glyf" file ttfgl.ttf_font.ttf_offset in
      seek_in file (c+off+8);
      float_of_int (sreadInt2 file)
  )

let font_glyph_x0 gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_x0 x
  | TTFGlyph (ttfgl)->(
      let file=open_in_bin_cached ttfgl.ttf_font.ttf_file in
      let off_size=
        let (a,b)=tableLookup "head" file ttfgl.ttf_font.ttf_offset in
        seek_in file (a+50);
        if readInt2 file=0 then 2 else 4
      in

      let (a,b)=tableLookup "loca" file ttfgl.ttf_font.ttf_offset in
      seek_in file (a+ttfgl.ttf_glyph_id.glyph_index*off_size);
      let off=
        let off=readInt2 file in
        if off_size=2 then 2*off else off
      in
      let (c,d)=tableLookup "glyf" file ttfgl.ttf_font.ttf_offset in
      seek_in file (c+off+2);
      float_of_int (sreadInt2 file)
  )

let font_glyph_x1 gl=match gl with
    CFFGlyph (_,x)->CFF.glyph_x1 x
  | TTFGlyph (ttfgl)->(
      let file=open_in_bin_cached ttfgl.ttf_font.ttf_file in
      let off_size=
        let (a,b)=tableLookup "head" file ttfgl.ttf_font.ttf_offset in
        seek_in file (a+50);
        if readInt2 file=0 then 2 else 4
      in

      let (a,b)=tableLookup "loca" file ttfgl.ttf_font.ttf_offset in
      seek_in file (a+ttfgl.ttf_glyph_id.glyph_index*off_size);
      let off=
        let off=readInt2 file in
        if off_size=2 then 2*off else off
      in
      let (c,d)=tableLookup "glyf" file ttfgl.ttf_font.ttf_offset in
      seek_in file (c+off+6);
      float_of_int (sreadInt2 file)
  )

let glyphNumber gl=match gl with
    CFFGlyph (_,x)->CFF.glyphNumber x
  | TTFGlyph (ttfgl)->ttfgl.ttf_glyph_id

let glyphContents gl=match gl with
    CFFGlyph (_,x)->CFF.glyphContents x
  | TTFGlyph (ttfgl)->ttfgl.ttf_glyph_id.glyph_utf8

let glyphName gl=
  match gl with
      CFFGlyph (_,x)-> CFF.glyphName x
    | TTFGlyph (gl)->gl.ttf_glyph_id.glyph_utf8

(*
type ttfglyph={ ttf_font:ttf;
                ttf_glyph_id:glyph_id;
                mutable ttf_width:float;
                mutable ttf_y0:float;
                mutable ttf_y1:float }
*)
let glyphWidth ?(orig=true) gl=
  match gl with
      CFFGlyph (_,x)->CFF.glyphWidth x
    | TTFGlyph (ttfgl)->
      if ttfgl.ttf_width<infinity then ttfgl.ttf_width *. ttfgl.ttf_font.ttf_cadratin else (
        let file,offset,x=ttfgl.ttf_font.ttf_file,ttfgl.ttf_font.ttf_offset,ttfgl.ttf_glyph_id in
        let file=open_in_bin_cached file in
        let num=x.glyph_index in
        let (a,_)=tableLookup "hhea" file offset in
        let nh=(seek_in file (a+34); readInt2 file) in
        let (b,_)=tableLookup "hmtx" file offset in
        seek_in file (if num>=nh then (b+4*(nh-1)) else (b+4*num));
        let w=float_of_int (readInt2 file) in
        ttfgl.ttf_width<-w;
        if orig then w else w *. ttfgl.ttf_font.ttf_cadratin
      )

let glyphLSB  gl=
  match gl with
      CFFGlyph (_,x)->CFF.glyphLSB x
    | TTFGlyph (ttfgl)->
        let file,offset,x=ttfgl.ttf_font.ttf_file,ttfgl.ttf_font.ttf_offset,ttfgl.ttf_glyph_id in
        let file=open_in_bin_cached file in
        let num=x.glyph_index in
        let (a,_)=tableLookup "hhea" file offset in
        let nh=(seek_in file (a+34); readInt2 file) in
        let (b,_)=tableLookup "hmtx" file offset in
        seek_in file (if num>=nh then (b+4*(nh-1)+2) else (b+4*num+2));
        float_of_int (readInt2 file)

let otype_file font=match font with
    CFF (font)->font.cff_font.file, font.cff_offset
  | TTF ttf->ttf.ttf_file,ttf.ttf_offset

let coverageIndex file off glyph=
  let format=seek_in file off; readInt2 file in
  let count=readInt2 file in
  let rec format1 x0 x1=
    if x0>=x1 then raise Not_found else
      if x1=x0+1 then
        (let x2=(x0+x1)/2 in
         let current=seek_in file (off+4+2*x2); readInt2 file in
           if current=glyph then x2 else raise Not_found)
      else
        (let x2=(x0+x1)/2 in
         let current=seek_in file (off+4+2*x2); readInt2 file in
           if glyph<current then format1 x0 x2 else format1 x2 x1)
  in
  let rec format2 x0 x1=
    if x0>=x1 then raise Not_found else
      if x1=x0+1 then
        (let start=seek_in file (off+6*x0+4); readInt2 file in
         let final=readInt2 file in
         let cvIdx=readInt2 file in
           if glyph>=start && glyph<=final then
             cvIdx+glyph-start
           else
             raise Not_found)

      else
        (let x2=(x0+x1)/2 in
         let final=seek_in file (off+6*x0+6); readInt2 file in
           if glyph>final then
             format2 x2 x1
           else
             format2 x0 x2)
  in
    if format=1 then format1 0 count else
      if format=2 then format2 0 count else
        (Printf.fprintf stderr "format : %d\n" format; raise Not_found)


let class_def file off glyph=
  let format=seek_in file off; readInt2 file in
    if format=1 then (
      let startGlyph=readInt2 file in
      let glyphCount=readInt2 file in
        if glyph>=startGlyph && glyph<startGlyph+glyphCount then
          (seek_in file (off+6+2*(glyph-startGlyph)); readInt2 file)
        else
          0
    ) else if format=2 then (
      let classRangeCount=readInt2 file in
      let off0=off+4 in
      let rec format2 x0 x1=
        let x2=(x0+x1)/2 in
        let rstart=seek_in file (off0+6*x2); readInt2 file in
        let rend=readInt2 file in
          if glyph<rstart then
            if x1-x0<=1 then 0 else format2 x0 x2
          else
            if glyph>rend then
              if x1-x0<=1 then 0 else format2 x2 x1
            else
              readInt2 file
      in
        format2 0 classRangeCount
    ) else 0

(************* Layout tables : GSUB, GPOS, etc. ***************)

let readCoverageIndex file off=
  let format=seek_in file off; readInt2 file in
  let count=readInt2 file in
  let rec format1 i result=
    if i>=count then result else (
      let c=readInt2 file in
        format1 (i+1) ((c,i)::result)
    )
  in
  let rec format2 i result=

    if i>=count then result else (
      let start=readInt2 file in
      let final=readInt2 file in
      let cvIdx=readInt2 file in
      let rec make_range i result=
        if i>final then result else (
          make_range (i+1) ((i,i+cvIdx-start)::result)
        )
      in
        format2 (i+1) (make_range start result)
    )
  in
    if format=1 then format1 0 [] else
      if format=2 then format2 0 [] else
        []


let readClass file off=
  let format=seek_in file off; readInt2 file in
    if format=1 then (
      let startGlyph=readInt2 file in
      let glyphCount=readInt2 file in
      let rec classValues i result=if i>=glyphCount then List.rev result else
        (classValues (i-1) ((startGlyph+i, readInt2 file)::result))
      in
        classValues 0 []
    ) else if format=2 then (
      let classRangeCount=readInt2 file in
      let rec format2 i result=
        if i>=classRangeCount then result else (
          let startGlyph=readInt2 file in
          let endGlyph=readInt2 file in
          let cl=readInt2 file in
          let rec make_range i r= if i>endGlyph then r else (make_range (i+1) ((i,cl)::r)) in
            format2 (i+1) (make_range startGlyph result)
        )
      in
        format2 0 []
    ) else []
(*********************************)


#define GSUB_SINGLE 1
#define GSUB_MULTIPLE 2
#define GSUB_ALTERNATE 3
#define GSUB_LIGATURE 4
#define GSUB_CONTEXT 5
#define GSUB_CHAINING 6
#define GSUB_EXTENSION 7
#define GSUB_REVERSE 8


let rec readLookup file gsubOff i=
  let subst=ref [] in
  let lookup= seek_in file (gsubOff+8); readInt2 file in
  let offset0=seek_in file (gsubOff+lookup+2+i*2); gsubOff+lookup+(readInt2 file) in
  let lookupType=seek_in file offset0; readInt2 file in
    (* let lookupFlag=seek_in file (offset0+2); readInt2 file in *)
  let subtableCount=seek_in file (offset0+4); readInt2 file in
    for subtable=0 to subtableCount-1 do
      let offset1=seek_in file (offset0+6+subtable*2); offset0+(readInt2 file) in

        match lookupType with
            GSUB_SINGLE->(
              let format=seek_in file offset1;readInt2 file in
              let coverageOff=readInt2 file in
                if format=1 then (
                  let delta=readInt2 file in
                  let cov=readCoverageIndex file (offset1+coverageOff) in
                    List.iter (fun (a,_)->subst:=(Subst { original_glyphs=[|a|]; subst_glyphs=[|a+delta|]})::(!subst)) cov
                ) else if format=2 then (
                  let cov=readCoverageIndex file (offset1+coverageOff) in
                    List.iter (fun (a,b)->
                                 let gl=seek_in file (offset1+6+b*2); readInt2 file in
                                   subst:=(Subst { original_glyphs=[|a|]; subst_glyphs=[|gl|]})::(!subst)) cov
                )
            )
          | GSUB_MULTIPLE->(
              let coverageOff=seek_in file (offset1+2); readInt2 file in
              let cov=readCoverageIndex file (offset1+coverageOff) in
                 List.iter (fun (first_glyph,alternate)->
                             let offset2=seek_in file (offset1+6+alternate*2); offset1+readInt2 file in
                             let glyphCount=seek_in file offset2; readInt2 file in
                             let arr=Array.make glyphCount 0 in
                               for comp=0 to glyphCount-1 do
                                 arr.(comp)<-readInt2 file;
                               done;
                               subst:=(Subst { original_glyphs=[|first_glyph|]; subst_glyphs=arr})::(!subst)
                          ) cov
            )
          | GSUB_ALTERNATE->(
              let coverageOff=seek_in file (offset1+2); readInt2 file in
              let cov=readCoverageIndex file (offset1+coverageOff) in
                List.iter (fun (first_glyph,alternate)->
                             let offset2=seek_in file (offset1+6+alternate*2); offset1+readInt2 file in
                             let glyphCount=seek_in file offset2; readInt2 file in
                             let arr=Array.make (1+glyphCount) first_glyph in
                               for comp=1 to glyphCount do
                                 arr.(comp)<-readInt2 file;
                               done;
                               subst:=(Alternative arr)::(!subst)
                          ) cov
            )
          | GSUB_LIGATURE->(
              let coverageOff=seek_in file (offset1+2); readInt2 file in
              let cov=readCoverageIndex file (offset1+coverageOff) in
                (* let ligSetCount=seek_in file (offset1+4); readInt2 file in *)
                List.iter (fun (first_glyph,ligset)->
                             (* for ligset=0 to ligSetCount-2 do *)
                             let offset2=seek_in file (offset1+6+ligset*2); offset1+readInt2 file in
                             let ligCount=seek_in file offset2; readInt2 file in
                               for lig=0 to ligCount-1 do
                                 let offset3=seek_in file (offset2+2+lig*2); offset2+readInt2 file in
                                 let ligGlyph=seek_in file offset3; readInt2 file in
                                 let compCount=readInt2 file in
                                 let arr=Array.make compCount first_glyph in
                                   for comp=1 to compCount-1 do
                                     arr.(comp)<-readInt2 file
                                   done;
                                   subst:=(Subst { original_glyphs=arr; subst_glyphs=[|ligGlyph|] })::(!subst)
                               done
                                 (* done *)
                          ) cov
            )
          | GSUB_CONTEXT->(
              let format=seek_in file offset1; readInt2 file in
                if format=1 then (
                  let coverageOff=readInt2 file in
                  let cov=readCoverageIndex file (offset1+coverageOff) in
                    List.iter (fun (first_glyph, subruleSet)->
                                 let offset2=offset1+6+subruleSet*2 in
                                 let subruleCount=seek_in file offset2; readInt2 file in
                                   for j=0 to subruleCount-1 do
                                     let subruleOff=seek_in file (offset2+2+j*2); readInt2 file in

                                     let glyphCount=seek_in file (offset2+subruleOff); readInt2 file in
                                     let substCount=readInt2 file in
                                     let arr=Array.make glyphCount (first_glyph,[]) in
                                       for i=1 to glyphCount-1 do
                                         arr.(i)<-(readInt2 file, [])
                                       done;
                                       for i=0 to substCount do
                                         let seqIdx=readInt2 file in
                                         let lookupIdx=readInt2 file in
                                           arr.(seqIdx)<-(fst arr.(i), (readLookup file gsubOff lookupIdx)@(snd arr.(i)))
                                       done;
                                       subst:=(Context arr)::(!subst)
                                   done
                              ) cov
                ) else if format=2 then (

                ) else if format=3 then (

                )
            )
          | GSUB_CHAINING->(
              let format=seek_in file offset1; readInt2 file in
              if format=1 then (

                ) else if format=2 then (

                ) else if format=3 then (
                  let backCount=readInt2 file in
                  let back_arr=Array.make backCount [] in
                    for i=1 to backCount do
                      let covOff=seek_in file (offset1+2+i*2); readInt2 file in
                      let cov=readCoverageIndex file (offset1+covOff) in
                        List.iter (fun (a,_)->back_arr.(backCount-i)<-a::back_arr.(backCount-i)) cov
                    done;
                    let offset2=offset1+4+backCount*2 in
                    let inputCount=seek_in file offset2; readInt2 file in
                    let input_arr=Array.make inputCount [] in
                      for i=0 to inputCount-1 do
                        let covOff=seek_in file (offset2+2+i*2); readInt2 file in
                        let cov=readCoverageIndex file (offset1+covOff) in
                          List.iter (fun (a,_)->input_arr.(i)<-a::input_arr.(i)) cov
                      done;
                      let offset3=offset2+2+inputCount*2 in
                      let aheadCount=seek_in file offset3; readInt2 file in
                      let ahead_arr=Array.make aheadCount [] in
                        for i=0 to aheadCount-1 do
                          let covOff=seek_in file (offset3+2+i*2); readInt2 file in
                          let cov=readCoverageIndex file (offset1+covOff) in
                            List.iter (fun (a,_)->ahead_arr.(i)<-a::ahead_arr.(i)) cov
                        done;
                        subst:=(Chain {before=back_arr; input=input_arr; after=ahead_arr})::(!subst)
                );
            )
          | GSUB_REVERSE->(

          )
          | _->()                       (* Manque les types 7 et 8 *)
    done;
    List.rev !subst


let rec applyLookup file gsubOff i glyphs=
  let lookup= seek_in file (gsubOff+8); readInt2 file in
  let offset0=seek_in file (gsubOff+lookup+2+i*2); gsubOff+lookup+(readInt2 file) in
  let lookupType0=seek_in file offset0; readInt2 file in

  (* let lookupFlag=seek_in file (offset0+2); readInt2 file in *)
  let subtableCount=seek_in file (offset0+4); readInt2 file in
  let ligbuf=Buffer.create 10 in
  (* lookupType et ext sont là juste pour le type "extension", qui redéfinit les offsets *)
  let rec gpos lookupType ext subtable gls=if subtable>=subtableCount then gls else
      begin
        let gls'=
          let offset1=
            if ext>=0 then ext else (
              seek_in file (offset0+6+subtable*2); offset0+(readInt2 file)
            )
          in
          match lookupType with
              GSUB_SINGLE->(
                let format=seek_in file offset1;readInt2 file in
                let coverageOff=readInt2 file in
                let buf=ref [] in
                if format=1 then (
                  let delta=readInt2 file in
                  List.iter (fun g->
                    try
                      let _(* cov *)=coverageIndex file (offset1+coverageOff) g.glyph_index in
                      buf:={ g with glyph_index=g.glyph_index+delta }::(!buf)
                    with
                        Not_found->buf:=g::(!buf)
                  ) gls;
                  List.rev !buf
                ) else if format=2 then (
                  List.iter (fun g->
                    try
                      let cov=coverageIndex file (offset1+coverageOff) g.glyph_index in
                      let gl=seek_in file (offset1+6+cov*2); readInt2 file in
                      buf:={ g with glyph_index=gl }::(!buf)
                    with
                        Not_found->buf:=g::(!buf)
                  ) gls;
                  List.rev !buf
                ) else gls
              )
              | GSUB_MULTIPLE->(
                let coverageOff=seek_in file (offset1+2); readInt2 file in
                let buf=ref [] in
                List.iter (fun g->
                  try
                    let cov=coverageIndex file (offset1+coverageOff) g.glyph_index in
                    let offset2=seek_in file (offset1+6+cov*2); offset1+readInt2 file in
                    let glyphCount=seek_in file offset2; readInt2 file in
                    for comp=0 to glyphCount-1 do
                      if comp=0 then
                        buf:={g with glyph_index=(readInt2 file)}::(!buf)
                      else
                        buf:={glyph_utf8=""; glyph_index=(readInt2 file)}::(!buf)
                    done;
                  with
                      Not_found->buf:=g::(!buf)
                ) gls;
                List.rev !buf
              )
              | GSUB_ALTERNATE->(
              (* C'est compliqué, comment on choisit l'alternative correcte ? *)
              (* À part ça, ça marche comme GSUB_MULTIPLE, sauf qu'on
                 ne choisit qu'un seul glyph au lieu de les prendre tous. *)

              (* let coverageOff=seek_in file (offset1+2); readInt2 file in *)
              (* let cov=readCoverageIndex file (offset1+coverageOff) in *)
              (* List.iter (fun (first_glyph,alternate)-> *)
              (*   let offset2=seek_in file (offset1+6+alternate*2); offset1+readInt2 file in *)
              (*   let glyphCount=seek_in file offset2; readInt2 file in *)
              (*   let arr=Array.make (1+glyphCount) first_glyph in *)
              (*   for comp=1 to glyphCount do *)
              (*     arr.(comp)<-readInt2 file; *)
              (*   done; *)
              (*   subst:=(Alternative arr)::(!subst) *)
              (* ) cov *)
              gls
              )
              | GSUB_LIGATURE->(
                let coverageOff=seek_in file (offset1+2); readInt2 file in
                let buf=ref [] in
                let rec ligature gl=
                  match gl with
                      []->()
                    | h::s->
                      begin
                        try
                          let cov=coverageIndex file (offset1+coverageOff) h.glyph_index in
                          let offset2=seek_in file (offset1+6+cov*2); offset1+readInt2 file in
                          let ligCount=seek_in file offset2; readInt2 file in
                          let rec all_ligs lig=
                            if lig>=ligCount then (
                              (* Aucune ligature n'a été trouvée *)
                              buf:=h::(!buf);
                              s
                            ) else (
                              let offset3=seek_in file (offset2+2+lig*2); offset2+readInt2 file in
                              let ligGlyph=seek_in file offset3; readInt2 file in
                              let compCount=readInt2 file in
                              let rec comp_glyphs l i=
                                if i=0 then true,l else (
                                  let x=readInt2 file in
                                  match l with
                                      []->false,[]
                                    | hg::sg when hg.glyph_index<>x->false,[]
                                    | _::sg->comp_glyphs sg (i-1)
                                )
                              in
                              let applicable,next=comp_glyphs s (compCount-1) in
                              if applicable then (
                                Buffer.clear ligbuf;
                                Buffer.add_string ligbuf h.glyph_utf8;
                                let rec add_glyphs l i=
                                  if i>0 then match l with
                                      []->()
                                    | hg::sg->(
                                      Buffer.add_string ligbuf hg.glyph_utf8;
                                      add_glyphs sg (i-1)
                                    )
                                in
                                add_glyphs s (compCount-1);
                                (* La ligature marche, on s'arrête là. *)
                                { glyph_index=ligGlyph;glyph_utf8=Buffer.contents ligbuf }::next
                              ) else (
                                all_ligs (lig+1)
                              )
                            )
                          in
                          ligature (all_ligs 0)
                        with
                            Not_found->(
                              buf:=h::(!buf);
                              ligature s
                            )
                      end
                in
                ligature gls;
                List.rev !buf
              )
              | GSUB_CONTEXT->gls
              | GSUB_CHAINING->(
                let format=seek_in file offset1; readInt2 file in
                if format=1 then (
                  gls
                ) else if format=2 then (
                  gls
                ) else if format=3 then (

                  let rec split n l r=if n=0 then List.rev l,r else match r with
                      h::s->split (n-1) (h::l) s
                    | []->List.rev l,[]
                  in
                  let rec fold_apply l r=
                    let rec verif off i n g=if i>=n then true else
                        match g with
                            []->n<=0
                          | h::s->(
                            let ok=
                              try
                                let coverageOff=seek_in file (off+i*2); readInt2 file in
                                let _=coverageIndex file (offset1+coverageOff) h.glyph_index in
                                true
                              with
                                  Not_found->false
                            in
                            if ok then verif off (i+1) n s else false
                          )
                    in
                    let backCount=seek_in file (offset1+2);readInt2 file in
                    let offset2=offset1+4+backCount*2 in
                    let inputCount=seek_in file offset2; readInt2 file in
                    let offset3=offset2+2+inputCount*2 in
                    let aheadCount=seek_in file offset3; readInt2 file in

                    let offset4=offset3+2+aheadCount*2 in
                    let substCount=seek_in file offset4; readInt2 file in
                    let rec apply_substs i ginp=if i>=substCount then ginp else
                      let a=readInt2 file in
                      let b=readInt2 file in
                      let u,v=split a [] ginp in
                      apply_substs (i+1) (u@applyLookup file gsubOff b v)
                    in

                    let g0,g1=split inputCount [] r in
                    if verif (offset1+4) 0 backCount l &&
                      verif (offset2+2) 0 inputCount g0 &&
                      verif (offset3+2) 0 aheadCount g1
                    then (
                      seek_in file (offset4+2);
                      let subst=List.rev (apply_substs 0 g0) @ l in
                      match g1 with
                          []->List.rev subst
                        | _->fold_apply subst g1
                    ) else (
                      match r with
                          []->List.rev l
                        | h::s->fold_apply (h::l) s
                    )
                  in
                  fold_apply [] gls
                ) else
                    gls
              )
              | GSUB_EXTENSION->(
                let format=seek_in file offset1;readInt2 file in
                if format=1 then (
                  let lookupType=readInt2 file in
                  let lookupOffset=readInt2 file in
                  gpos lookupType (offset1+lookupOffset) subtable gls
                ) else gls
              )
              | GSUB_REVERSE->(
                let format=seek_in file offset1;readInt2 file in
                if format=1 then (
                  let coverageOff=readInt2 file in
                  let btGlyphCount=readInt2 file in
                  let aheadGlyphCount=seek_in file (offset1+6+2*btGlyphCount);readInt2 file in
                  let buf=ref [] in
                  let rec process gls bt=match gls with
                      []->()
                    | h::s->
                      begin
                        try
                          let cov=coverageIndex file (offset1+coverageOff) h.glyph_index in
                          let rec check_bt l i=
                            if i=0 then true else (
                              match l with
                                  []->raise Not_found
                                | h0::s0->
                                  let _=coverageIndex file (offset1+readInt2 file) h0.glyph_index in
                                  check_bt s0 (i-1)
                            )
                          in
                          let _(* bt_ok *)=seek_in file (offset1+6);check_bt bt btGlyphCount in
                          let _(* ahead_ok *)=seek_in file (offset1+8+2*btGlyphCount);
                            check_bt s aheadGlyphCount
                          in
                          seek_in file (offset1+10+btGlyphCount*2+aheadGlyphCount*2+cov*2);
                          buf:={ h with glyph_index=readInt2 file }::(!buf);
                          process s (h::bt)
                        with
                            Not_found->(
                              buf:=h::(!buf);
                              process s (h::bt)
                            )
                      end
                  in
                  process gls [];
                  List.rev !buf
                ) else gls
              )
              | _->gls
        in
        gpos lookupType (-1) (subtable+1) gls'
      end
  in
  gpos lookupType0 (-1) 0 glyphs


let read_gsub font=
  let (file_,off0)=otype_file font in
  let file=open_in_bin_cached file_ in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let lookup= seek_in file (gsubOff+8); readInt2 file in
  let lookupCount= seek_in file (gsubOff+lookup); readInt2 file in
    (* Iteration sur les lookuptables *)
  let arr=Array.make lookupCount [] in
  for i=0 to lookupCount-1 do
    arr.(i)<-readLookup file gsubOff i
  done;
  arr

let read_lookup font i=
  let (file_,off0)=otype_file font in
  let file=open_in_bin_cached file_ in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let x=readLookup file gsubOff i in
    x


let alternates = "aalt"
let smallCapitals = "c2sc"
let caseSensitiveForms = "case"
let discretionaryLigatures = "dlig"
let denominators = "dnom"
let fractions = "frac"
let standardLigatures = "liga"
let liningFigures = "lnum"
let localizedForms = "locl"
let numerators = "numr"
let oldStyleFigures = "onum"
let ordinals = "odrn"
let ornaments = "ornm"
let proportionalFigures = "pnum"
let stylisticAlternates = "salt"
let scientificInferiors = "sinf"
let subscript = "subs"
let superscript = "sups"
let titling = "titl"
let tabularFigures = "tnum"
let slashedZero = "zero"

type feature_set={gsubOff:int;lookups:int list}

let select_features font (feature_tags : string list) =try
  let (file_,off0)=otype_file font in
  let file=open_in_bin_cached file_ in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let features=seek_in file (gsubOff+6); readInt2 file in
  let featureCount=seek_in file (gsubOff+features);readInt2 file in
  let feature_tag=Bytes.create 4 in
  let rec select i result=
    if i>=featureCount then result else (
        seek_in file (gsubOff+features+2+i*6);
        let _=input file feature_tag 0 4 in
        seek_in file (gsubOff+features+6+i*6);
        let lookupOff=readInt2 file in
        let lookupCount=seek_in file (gsubOff+features+lookupOff+2); readInt2 file in
        let rec read lookup s=
          if lookup>=lookupCount then s else (
            let l=readInt2 file in read (lookup+1) (l::s)
          )
        in
        let feature_tag = Bytes.to_string feature_tag in
          if List.mem feature_tag feature_tags then
            select (i+1) (read 0 result)
          else
            select (i+1) result
    )
  in
  (* let x=List.concat (List.map (fun lookup->readLookup file gsubOff lookup) (select 0 [])) in *)
  {gsubOff=gsubOff;lookups=(select 0 [])}
  with Table_not_found _->{gsubOff=(-1);lookups=[]}


let apply_features font features glyphs=
  let (file_,off0)=otype_file font in
  let file=open_in_bin_cached file_ in
  List.fold_left (fun gls lookup->applyLookup file features.gsubOff lookup gls) glyphs
    features.lookups


let font_features : font -> string list = fun font ->
  try
  let (file_,off0)=otype_file font in
  let file=open_in_bin_cached file_ in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in

  let features=seek_in file (gsubOff+6); readInt2 file in
  let featureCount=seek_in file (gsubOff+features);readInt2 file in
  let buf=Bytes.create 4 in
  let rec make_features i result=
    if i>=featureCount then result else (
      seek_in file (gsubOff+features+2+i*6);
      let _=input file buf 0 4 in
      make_features (i+1) (Bytes.copy buf::result)
    )
  in
  List.map Bytes.to_string (make_features 0 [])
  with Table_not_found _->[]


let read_scripts font=
  let (file,off0)=otype_file font in
  let file=open_in_bin_cached file in
  let (gsubOff,_)=tableLookup "GSUB" file off0 in
  let scripts=seek_in file (gsubOff+4); readInt2 file in
  let scriptCount=seek_in file (gsubOff+scripts); readInt2 file in
    for i=0 to scriptCount-1 do
      let scriptTag=Bytes.create 4 in
        seek_in file (gsubOff+scripts+2+i*6);
        let _=input file scriptTag 0 4 in
        let off=readInt2 file in
        Printf.printf "\n%s\n" (Bytes.to_string scriptTag);
        let offset1=gsubOff+scripts+off in
        let langSysCount=seek_in file (offset1+2); readInt2 file in
        for langSys=0 to langSysCount-1 do
          let langSysTag=Bytes.create 4 in
          seek_in file (offset1+4+langSys*6);
          let _=input file langSysTag 0 4 in
          Printf.printf "lang : %s\n" (Bytes.to_string langSysTag)
        done
    done


#define GPOS_SINGLE 1
#define GPOS_PAIR 2

let rec gpos font glyphs0=
  let (file,off0)=otype_file font in
  let file=open_in_bin_cached file in
  let (gposOff,_)=tableLookup "GPOS" file off0 in
  let lookup= seek_in file (gposOff+8); readInt2 file in
  let lookupCount= seek_in file (gposOff+lookup); readInt2 file in
  let glyphs=ref glyphs0 (* (List.map (fun x->GlyphID x) glyphs0) *) in
    (* Iteration sur les lookuptables *)
    for i=1 to lookupCount do
      let offset=seek_in file (gposOff+lookup+i*2); readInt2 file in

      let lookupType=seek_in file (gposOff+lookup+offset); readInt2 file in
      (* let lookupFlag=seek_in file (gposOff+lookup+offset+2); readInt2 file in *)
      let subtableCount=seek_in file (gposOff+lookup+offset+4); readInt2 file in
      let maxOff=gposOff+lookup+offset + 6+subtableCount*2 in

      let rec lookupSubtables off gl=
        if off>=maxOff then gl else
          let subtableOff=seek_in file off; readInt2 file in
          let offset=gposOff+lookup+offset+subtableOff in


          let rec gpos glyphs=
            (* Printf.printf "lookupType=%d\n" lookupType; *)
            match glyphs with

                id_h::id_h'::s->(
                  let h=glyph_id_cont id_h in
                  let h'=glyph_id_cont id_h' in
                  match lookupType with
                      GPOS_PAIR->(
                        let format=seek_in file offset; readInt2 file in
                          (* Printf.printf "format : %d\n" format; *)
                        let coverageOffset=readInt2 file in
                        let valueFormat1=readInt2 file in
                        let valueFormat2=readInt2 file in

                        let rec compute_size x r=if x=0 then r else compute_size (x lsr 1) (r+(x land 1)) in
                        let size1=compute_size valueFormat1 0 in
                        let size2=compute_size valueFormat2 0 in
                        let readAll format gl=
                          let a=if (format land 0x1) <> 0 then float_of_int (int16 (readInt2 file)) else 0. in
                          let b=if (format land 0x2) <> 0 then float_of_int (int16 (readInt2 file)) else 0. in
                          let c=if (format land 0x4) <> 0 then float_of_int (int16 (readInt2 file)) else 0. in
                          let d=if (format land 0x8) <> 0 then float_of_int (int16 (readInt2 file)) else 0. in
                          { kern_x0=a;
                            kern_y0=b;
                            advance_width=c;
                            advance_height=d;
                            kern_contents=gl }
                        in
                          try
                            let coverage=coverageIndex file (offset+coverageOffset) h in
                              if format=1 then (
                                let rec pairSetTable off0 x0 x1=
                                  let x2=(x0+x1)/2 in
                                  let gl=seek_in file (off0+x2*(1+size1+size2)*2); readInt2 file in
                                    if x1-x0<=1 then
                                      if gl=h' then
                                        let a=readAll valueFormat1 id_h in
                                        let b=readAll valueFormat2 id_h' in
                                        a,b
                                      else raise Not_found
                                    else
                                      if gl>h' then pairSetTable off0 x0 x2 else pairSetTable off0 x2 x1
                                in
                                let pairSetOffset=seek_in file (offset+10+coverage*2); readInt2 file in
                                let count=seek_in file (offset+pairSetOffset); readInt2 file in
                                let a,b=pairSetTable (offset+pairSetOffset+2) 0 count in
                                  (if valueFormat1<>0 then KernID a else id_h)::
                                    (gpos ((if valueFormat2<>0 then KernID b else id_h')::s))
                              ) else if format=2 then (

                                let classdef1=seek_in file (offset+8); class_def file (offset+readInt2 file) h in
                                let classdef2=seek_in file (offset+10); class_def file (offset+readInt2 file) h' in
                                let class1count=seek_in file (offset+12); readInt2 file in
                                let class2count=seek_in file (offset+14); readInt2 file in
                                  if classdef1>class1count || classdef2>class2count then
                                    glyphs
                                  else
                                    (let index=16
                                       + (class2count*2*(size1+size2))*classdef1
                                       + 2*(size1+size2)*classdef2
                                     in
                                     seek_in file (offset+index);
                                     let a=readAll valueFormat1 id_h in
                                     let b=readAll valueFormat2 id_h' in
                                     (if valueFormat1<>0 then KernID a else id_h)::
                                       (gpos ((if valueFormat2<>0 then KernID b else id_h')::s))
                                    )
                              ) else glyphs
                          with
                              Not_found->glyphs
                      )
                    | _->glyphs
                )
              | [h]->[h]
              | [] -> []
          in
            lookupSubtables (off+2) (gpos gl)
      in
        glyphs:=lookupSubtables (gposOff+lookup+offset + 6) !glyphs
    done;
    !glyphs


let positioning font glyphs0=try gpos font glyphs0 with Table_not_found _->glyphs0


(****************************************************************)

type fontInfo=
    { mutable tables:bytes StrMap.t;
      mutable fontType:string;
      mutable names:(int*int*string) list }

let getInt2 str i=(int_of_char str.[i] lsl 8) lor (int_of_char str.[i+1])
let getInt4 str i=
  (((((int_of_char str.[i] lsl 8) lor (int_of_char str.[i+1])) lsl 8) lor (int_of_char str.[i+2])) lsl 8) lor
    (int_of_char str.[i+3])
let sgetInt2 str i=
  let a=(int_of_char str.[i] lsl 8) lor (int_of_char str.[i+1]) in
  if a>=0x8000 then a-0x10000 else a

let fontInfo font=
  let fileName,off=match font with
      CFF (cff)->cff.cff_font.file,cff.cff_offset
    | TTF ttf->ttf.ttf_file,ttf.ttf_offset
  in
  let file=open_in_bin_cached fileName in
  let fontType=Bytes.create 4 in
  seek_in file off;
  really_input file fontType 0 4;
  seek_in file (off+4);
  let numTables=readInt2 file in
  let rec getTables n l=
    if n<offsetTable then l else (
      seek_in file (off+n);
      let newTable=Bytes.create 4 in
      really_input file newTable 0 4;
      (* Printf.fprintf stderr "%S newTable=%S\n" fileName newTable;flush stderr; *)
      let _ (* checkSum *)=readInt4 file in
      let offset=readInt4_int file in
      let length=readInt4_int file in
      seek_in file (off+offset);
      let buf=Bytes.create length in
      really_input file buf 0 length;
      getTables (n-dirSize) (StrMap.add (Bytes.to_string newTable) buf l)
    )
  in
  let tables =
    getTables (off+dirSize*(numTables-1)+offsetTable) StrMap.empty
  in
  let names=
    try
      let buf_name = StrMap.find "name" tables in
      let buf_name = Bytes.to_string buf_name in
      let format = getInt2 buf_name 0 in
      if format=0 then (
        let count=getInt2 buf_name 2 in
        let stringOffset=getInt2 buf_name 4 in
        let rec get_names i m=
          if i>=count then m else (
            let off=6+i*12 in
            let platformID=getInt2 buf_name off in
            let encodingID=getInt2 buf_name (off+2) in
            let languageID=getInt2 buf_name (off+4) in
            let nameID=getInt2 buf_name (off+6) in
            let length=getInt2 buf_name (off+8) in
            let offset=getInt2 buf_name (off+10) in
            let str=String.sub buf_name (stringOffset+offset) length in
            let m'=
              try
                match platformID,encodingID with
                    1,_->(
                      let utf8 = ROMAN.to_utf8 str in
                      ((languageID,nameID,utf8)::m)
                    )
                  | 3,1->(                (* UTF-16 *)
                    let utf8 = UTFConvert.utf16_to_utf8 str in
                    ((languageID,nameID,utf8)::m)
                  )
                  | a,b->(
                    Printf.fprintf stderr "unrecognized encoding, please report (font %s) : %d %d\n" fileName a b;
                    m
                  )
              with
                  _->m
            in
            get_names (i+1) m'
          )
        in
        get_names 0 []
      ) else []
    with
        Not_found->[]
  in

  { tables=tables;
    fontType=Bytes.to_string fontType;
    names=names }



#ifdef INT32

let rec checksum32 x=
  let cs=ref 0l in
  for i=0 to Buffer.length x-1 do
    cs:=Int32.add !cs (Int32.of_int (int_of_char (Buffer.nth x i)))
  done;
  !cs
let rec str_checksum32 x=
  let cs=ref 0l in
  let i=ref 0 in
  while !i<String.length x do
    let a=Int32.of_int (int_of_char x.[!i]) in
    let b=if !i+1<String.length x then Int32.of_int (int_of_char (x.[!i+1])) else 0l in
    let c=if !i+2<String.length x then Int32.of_int (int_of_char (x.[!i+2])) else 0l in
    let d=if !i+3<String.length x then Int32.of_int (int_of_char (x.[!i+3])) else 0l in
    cs:=Int32.add !cs (Int32.logor (Int32.shift_left (Int32.logor (Int32.shift_left (Int32.logor (Int32.shift_left a 8) b) 8) c) 8) d);
    i:= !i+4
  done;
  !cs
let rec buf_checksum32 x=
  let cs=ref 0l in
  let i=ref 0 in
  while !i<Buffer.length x do
    let a=Int32.of_int (int_of_char (Buffer.nth x !i)) in
    let b=if !i+1<Buffer.length x then Int32.of_int (int_of_char (Buffer.nth x (!i+1))) else 0l in
    let c=if !i+2<Buffer.length x then Int32.of_int (int_of_char (Buffer.nth x (!i+2))) else 0l in
    let d=if !i+3<Buffer.length x then Int32.of_int (int_of_char (Buffer.nth x (!i+3))) else 0l in
    cs:=Int32.add !cs (Int32.logor (Int32.shift_left (Int32.logor (Int32.shift_left (Int32.logor (Int32.shift_left a 8) b) 8) c) 8) d);
    i:= !i+4
  done;
  !cs

let total_checksum a b c=
  Int32.sub (-1313820742l) (Int32.add a (Int32.add b c))

#else

let rec checksum32 x=
  let cs=ref 0 in
  for i=0 to Buffer.length x-1 do
    cs:= (!cs+int_of_char (Buffer.nth x i)) land 0xffffffff
  done;
  !cs
let rec str_checksum32 x=
  let cs=ref 0 in
  let i=ref 0 in
  while !i<String.length x do
    let a=int_of_char x.[!i] in
    let b=if !i+1<String.length x then int_of_char (x.[!i+1]) else 0 in
    let c=if !i+2<String.length x then int_of_char (x.[!i+2]) else 0 in
    let d=if !i+3<String.length x then int_of_char (x.[!i+3]) else 0 in
    cs:= (!cs+((((((a lsl 8) lor b) lsl 8) lor c) lsl 8) lor d)) land 0xffffffff;
    i:= !i+4
  done;
  !cs
let rec buf_checksum32 x=
  let cs=ref 0 in
  let i=ref 0 in
  while !i<Buffer.length x do
    let a=int_of_char (Buffer.nth x !i) in
    let b=if !i+1<Buffer.length x then int_of_char (Buffer.nth x (!i+1)) else 0 in
    let c=if !i+2<Buffer.length x then int_of_char (Buffer.nth x (!i+2)) else 0 in
    let d=if !i+3<Buffer.length x then int_of_char (Buffer.nth x (!i+3)) else 0 in
    cs:= (!cs+((((((a lsl 8) lor b) lsl 8) lor c) lsl 8) lor d)) land 0xffffffff;
    i:= !i+4
  done;
  !cs

let total_checksum a b c=
  (-1313820742 - (a+b+c)) land 0xffffffff

#endif

let write_cff fontInfo=

  let buf=Buffer.create 256 in
  Buffer.add_string buf fontInfo.fontType;
  bufInt2 buf (StrMap.cardinal fontInfo.tables);
  let rec searchRange a b k=if a=1 then b lsl 4,k else searchRange (a lsr 1) (b lsl 1) (k+1) in
  let sr,log2=searchRange (StrMap.cardinal fontInfo.tables) 1 0 in
  bufInt2 buf sr;
  bufInt2 buf log2;
  bufInt2 buf ((StrMap.cardinal fontInfo.tables lsl 4) - sr);
  let buf_tables=Buffer.create 256 in
  let buf_headers=Buffer.create 256 in
  let write_tables checksums=
    StrMap.fold (fun k a _->
      (* Printf.fprintf stderr "writing table %S\n" k; *)
      while (Buffer.length buf_tables) land 3 <> 0 do
        Buffer.add_char buf_tables (char_of_int 0)
      done;
      Buffer.add_string buf_headers k;
      let cs=StrMap.find k checksums in
      bufInt4 buf_headers cs;
      bufInt4_int buf_headers (12+16*StrMap.cardinal fontInfo.tables+Buffer.length buf_tables);
      bufInt4_int buf_headers (Bytes.length a);
      Buffer.add_bytes buf_tables a
    ) fontInfo.tables ()
  in
  (try
     let buf_head=StrMap.find "head" fontInfo.tables in
     strInt4_int buf_head 8 0;
     let checksums = StrMap.map (fun a -> str_checksum32 (Bytes.to_string a)) fontInfo.tables in
     write_tables checksums;
     let check=total_checksum
       (buf_checksum32 buf)
       (buf_checksum32 buf_headers)
       (buf_checksum32 buf_tables)
     in
     Buffer.clear buf_tables;
     Buffer.clear buf_headers;
     strInt4 buf_head 8 check;
     (* Printf.fprintf stderr "total checksum=%x %x\n" (total_checksum) (Int32.to_int check); *)
     write_tables checksums
   with
       Not_found->failwith "no head table"
  );
  Buffer.add_buffer buf buf_headers;
  Buffer.add_buffer buf buf_tables;
  while (Buffer.length buf) land 3 <> 0 do
    Buffer.add_char buf (char_of_int 0)
  done;
  buf

let make_tables font fontInfo cmap glyphs_idx=
  let glyphs=Array.map (loadGlyph font) glyphs_idx in

  let fontInfo_tables=fontInfo.tables in
  fontInfo.tables<-
    StrMap.filter (fun k _->
      match k with
          "cmap" | "hmtx" | "hhea" | "head" | "maxp"
        | "OS/2" | "CFF " | "GSUB" | "GPOS" | "name" | "post"
        | "cvt " | "fpgm" | "glyf" | "loca" | "prep" -> true
        | _->(
          (* Printf.fprintf stderr "excluded table : %S\n" k;flush stderr; *)
          false
        )
    ) fontInfo.tables;

  (* Si certains des glyphs sont composites, rajouter les composantes
     (truetype). Il faut faire ça avant maxp, dans le cas de TrueType *)
  let glyphMap=
    let rec make_glyph_map i m0=
      if i>=Array.length glyphs_idx then m0 else (
        let gl=glyphs_idx.(i) in
        make_glyph_map (i+1) (IntMap.add gl.glyph_index i m0)
      )
    in
    let glyphMap=make_glyph_map 0 IntMap.empty in
    try
      let locformat=
        let head = StrMap.find "head" fontInfo_tables in
        getInt2 (Bytes.to_string head) 50
      in
      let buf_loca=StrMap.find "loca" fontInfo_tables in
      let buf_glyf=StrMap.find "glyf" fontInfo_tables in
      (* Complétion de la police quand il manque des composantes de glyphs *)
      let rec make_glyph_map l m0=
        match l with
            []->m0
          | gl::s->(
            (* let gl=glyphs_idx.(i) in *)
            let off0=if locformat=0 then
                2*(getInt2 (Bytes.to_string buf_loca) (2*gl))
              else
                getInt4 (Bytes.to_string buf_loca) (4*gl)
            in
            let off1=if locformat=0 then
                2*(getInt2 (Bytes.to_string buf_loca) (2*gl+2))
              else
                getInt4 (Bytes.to_string buf_loca) (4*gl+4)
            in
            let m1,s1=
              if off1<=off0 then m0,s else (
                let numberOfContours=sgetInt2 (Bytes.to_string buf_glyf) off0 in
                if numberOfContours<0 then (
                  let rec replace_glyphs i m1 s1=
                    let old_component=getInt2 (Bytes.to_string buf_glyf) (i+2) in
                    let m2,s2=
                      if IntMap.mem old_component m1 then m1,s1 else
                        ((IntMap.add old_component (IntMap.cardinal m1) m1),
                         (old_component::s1))
                    in
                    let flags=getInt2 (Bytes.to_string buf_glyf) i in
                    if flags land TT_MORE_COMPONENTS <> 0 then (
                      let off_args=if flags land TT_ARGS_ARE_WORDS <>0 then 4 else 2 in
                      let off_trans=
                        (if flags land TT_WE_HAVE_A_SCALE<>0 then 2 else
                            if flags land TT_WE_HAVE_AN_X_AND_Y_SCALE<>0 then 4 else
                              if flags land TT_WE_HAVE_A_TWO_BY_TWO<>0 then 8 else 0)
                      in
                      replace_glyphs (i+4+off_args+off_trans) m2 s2
                    ) else m2,s2
                  in
                  replace_glyphs (off0+10) m0 s
                ) else m0,s
              )
            in
            make_glyph_map s1 m1
          )
      in
      make_glyph_map (List.map (fun gl->gl.glyph_index) (Array.to_list glyphs_idx)) glyphMap
    with
        Not_found->glyphMap
  in



#ifdef DEBUG_TTF
  Printf.fprintf stderr "cmap\n"; flush stderr;
#endif
  (* cmap *)
  let r_cmap=ref IntMap.empty in
  (try
     r_cmap:=IntMap.filter (fun _ a -> a < Array.length glyphs && a>=0) cmap;
     let buf=Buffer.create 256 in
     Cmap.write_cmap ~formats:[4] !r_cmap buf;
     fontInfo.tables <- StrMap.add "cmap" (Buffer.to_bytes buf) fontInfo.tables
   with
       Not_found->());
  let cmap= !r_cmap in

#ifdef DEBUG_TTF
  Printf.fprintf stderr "hmtx\n"; flush stderr;
#endif
  (* hmtx *)
  let numberOfGlyphs=Array.length glyphs in
  let buf_hmtx=Bytes.create (4*IntMap.cardinal glyphMap) in
  let advanceWidthMax=ref 0.0 in
  for i=0 to numberOfGlyphs-1 do
    let w=glyphWidth glyphs.(i) in
    let lsb = glyphLSB glyphs.(i) in
    (*let lsb = glyph_x0 glyphs.(i) in*)
    advanceWidthMax:=max !advanceWidthMax w;
    strInt2 buf_hmtx (i*4) (int_of_float w);
    strInt2 buf_hmtx (i*4+2) (int_of_float lsb)
  done;
  for i=0 to IntMap.cardinal glyphMap - numberOfGlyphs - 1 do
    strInt2 buf_hmtx (4*numberOfGlyphs + i*4) 0 ;(* lsb to 0 *)
    strInt2 buf_hmtx (4*numberOfGlyphs + i*4+2) 0 (* adv to 0 *)
  done;
  fontInfo.tables<-StrMap.add "hmtx" buf_hmtx fontInfo.tables;

#ifdef DEBUG_TTF
  Printf.fprintf stderr "hhea\n"; flush stderr;
#endif
  (* hhea *)
  let xAvgCharWidth=ref 0. in
  let yMax=ref (-.infinity) in
  let yMin=ref infinity in
  let xMax=ref (-.infinity) in
  let xMin=ref infinity in
  (try
     let minLSB=ref infinity in
     let minRSB=ref infinity in
     let xMaxExtent = ref (-. infinity) in
     for i=0 to numberOfGlyphs-1 do
       let x0=glyph_x0 glyphs.(i) in
       let lsb = glyphLSB glyphs.(i) in
       (*let lsb = x0 in*)
       let x1=glyph_x1 glyphs.(i) in
       let y0=glyph_y0 glyphs.(i) in
       let y1=glyph_y1 glyphs.(i) in
       (*Printf.printf "LSB: %f, x0: %f, x1: %f, y0: %f, y1: %f\n%!" lsb x0 x1 y0 y1;*)
       minLSB:=min !minLSB lsb;
       let aw=glyphWidth glyphs.(i) in
       minRSB:=min !minRSB (aw -. (lsb +. x1 -. x0));
       xMaxExtent := max !xMaxExtent (lsb +. (x1 -. x0));
       (*minRSB:=min !minRSB (aw -. x1);
       xMaxExtent := max !xMaxExtent x1;*)
       xMax:=max !xMax x1;
       xMin:=min !xMin x0;
       yMax:=max !yMax y1;
       yMin:=min !yMin y0;
       xAvgCharWidth:= !xAvgCharWidth+.aw
     done;

     let buf_hhea=StrMap.find "hhea" fontInfo_tables in
#ifdef INT32
     strInt4 buf_hhea 0 0x00010000l;        (* Version *)
#else
     strInt4 buf_hhea 0 0x00010000;        (* Version *)
#endif
     strInt2 buf_hhea 10 (int_of_float !advanceWidthMax);  (* advanceWidthMax (hmtx) *)
     strInt2 buf_hhea 12 (int_of_float !minLSB);           (* minLeftSideBearing *)
     strInt2 buf_hhea 14 (int_of_float !minRSB);           (* minRightSideBearing *)
     strInt2 buf_hhea 16 (int_of_float !xMaxExtent); (* xMaxExtent *)
     strInt2 buf_hhea 34 (IntMap.cardinal glyphMap) (* size of hmtx first array *)
   with
       Not_found -> ());

#ifdef DEBUG_TTF
  Printf.fprintf stderr "head\n"; flush stderr;
#endif
  (* head *)
  (try
     let buf_head=StrMap.find "head" fontInfo_tables in
     (*let old_xMin = sgetInt2 buf_head 36 in
     let old_yMin = sgetInt2 buf_head 38 in
     let old_xMax = sgetInt2 buf_head 40 in
     let old_yMax = sgetInt2 buf_head 42 in
     Printf.printf "%d %d %d %d %f %f %f %f\n%!"
                   old_xMin old_yMin old_xMax old_yMax
                   !xMin !yMin !xMax !yMax;*)
     strInt2 buf_head 36 (int_of_float !xMin);
     strInt2 buf_head 38 (int_of_float !yMin);
     strInt2 buf_head 40 (int_of_float !xMax);
     strInt2 buf_head 42 (int_of_float !yMax)
   with
       Not_found->());

#ifdef DEBUG_TTF
  Printf.fprintf stderr "maxp\n"; flush stderr;
#endif
  (* maxp *)
  (if fontInfo.fontType="OTTO" then (
    let buf_maxp=Bytes.create 6 in
    Bytes.set buf_maxp 0 (char_of_int 0x00);
    Bytes.set buf_maxp 1 (char_of_int 0x00);
    Bytes.set buf_maxp 2 (char_of_int 0x50);
    Bytes.set buf_maxp 3 (char_of_int 0x00);
    strInt2 buf_maxp 4 (IntMap.cardinal glyphMap);
    fontInfo.tables<-StrMap.add "maxp" buf_maxp fontInfo.tables
   ) else (
    let buf_maxp=StrMap.find "maxp" fontInfo.tables in
    strInt2 buf_maxp 4 (IntMap.cardinal glyphMap);
   ));

(* post *)
  (try
     let buf_post=StrMap.find "post" fontInfo_tables in
     let format=getInt4 (Bytes.to_string buf_post) 0 in
     match format with
         0x20000->(
           let rec get_pascal_string i m=
             if i>=String.length (Bytes.to_string buf_post) then m else (
               let len=int_of_char (Bytes.get buf_post i) in
               get_pascal_string (i+1+len)
                 (IntMap.add (IntMap.cardinal m) (i,len) m)
             )
           in
           let strings=get_pascal_string (34+2*Array.length glyphs) IntMap.empty in
           let buf_post'=Buffer.create 256 in
           Buffer.add_bytes buf_post' (Bytes.sub buf_post 0 32);
           bufInt2 buf_post' (Array.length glyphs_idx);
           let strBuf=Buffer.create 256 in
           let strs=ref 0 in
           for i=0 to Array.length glyphs_idx-1 do
             let idx=getInt2 (Bytes.to_string buf_post) (34+2*glyphs_idx.(i).glyph_index) in
             if idx<=257 then bufInt2 buf_post' idx else (
               let off,len=IntMap.find (idx-258) strings in
               bufInt2 buf_post' (258+ !strs);
               for i=off to off+len do
                 Buffer.add_char strBuf (Bytes.get buf_post i)
               done;
               incr strs
             )
           done;
           Buffer.add_buffer buf_post' strBuf;
           fontInfo.tables<-StrMap.add "post" (Buffer.to_bytes buf_post') fontInfo.tables
         )
       | 0x25000->(
          let buf_post'=Buffer.create 256 in
          Buffer.add_bytes buf_post' (Bytes.sub buf_post 0 32);
          bufInt2 buf_post' (Array.length glyphs_idx);
          for i=0 to Array.length glyphs_idx-1 do
            Buffer.add_char buf_post' (Bytes.get buf_post (34+glyphs_idx.(i).glyph_index))
          done;
          fontInfo.tables<-StrMap.add "post" (Buffer.to_bytes buf_post') fontInfo.tables
       )
       | _->()
   with
       Not_found->()
  );

#ifdef DEBUG_TTF
  Printf.fprintf stderr "OS/2\n";flush stderr;
#endif
  (* os/2 *)
  (try
     let buf_os2=StrMap.find "OS/2" fontInfo_tables in
     strInt2 buf_os2 2 ((round (!xAvgCharWidth/.float_of_int (Array.length glyphs))));
#ifdef INT32
     let u1=ref 0l in
     let u2=ref 0l in
     let u3=ref 0l in
     let u4=ref 0l in
#else
     let u1=ref 0 in
     let u2=ref 0 in
     let u3=ref 0 in
     let u4=ref 0 in
#endif
     let _=IntMap.fold (fun k _ _->UnicodeRanges.unicode_range u1 u2 u3 u4 k) cmap () in
     strInt4 buf_os2 42 !u1;
     strInt4 buf_os2 46 !u2;
     strInt4 buf_os2 50 !u3;
     strInt4 buf_os2 54 !u4;

     strInt2 buf_os2 64 (fst (IntMap.min_binding cmap)); (* usFirstCharIndex *)
     strInt2 buf_os2 66 (fst (IntMap.max_binding cmap)); (* usLastCharIndex *)

     ()

     (* (try *)
     (*    let ix=IntMap.find 0x78 cmap in *)
     (*    if ix>0 && ix<Array.length glyphs then *)
     (*      let sxHeight=glyph_y1 glyphs.(ix) in *)
     (*      strInt2 buf_os2 86 (round sxHeight) *)
     (*  with *)
     (*      _->()); *)
     (* (try *)
     (*    let iH=IntMap.find 0x48 cmap in *)
     (*    if iH>0 && iH<Array.length glyphs then *)
     (*      let capHeight=glyph_y1 glyphs.(iH) in *)
     (*      strInt2 buf_os2 88 (round capHeight) *)
     (*  with *)
     (*      _->()); *)
   with
       Not_found->()
  );

#ifdef DEBUG_TTF
  Printf.fprintf stderr "CFF\n";flush stderr;
#endif
  (* CFF  *)
  begin
    match font with
    | CFF (cff)->
        let name0=CFF.fontName cff.cff_font in
        let rec getName l m=match l with
            []->m
          | (lang,nameID,h)::s->(
            let name=try IntMap.find lang m with Not_found->name0 in
            (match nameID with
                1->name.family_name<-h
              | 2->name.subfamily_name<-h
              | 4->name.full_name<-h
              | 6->name.postscript_name<-h
              | _->());
            getName s (IntMap.add lang name m)
          )
        in
        let names=(getName fontInfo.names IntMap.empty) in
        let _,cff_name=IntMap.min_binding names in
        let cff'=(CFF.subset cff.cff_font
                    { CFF.name=cff_name }
                    IntMap.empty
                    glyphs_idx)
        in
        fontInfo.tables<-StrMap.add "CFF " (Buffer.to_bytes cff') fontInfo.tables

  (* truetype *)
    | TTF _->
          try
            let locformat=
              let head=StrMap.find "head" fontInfo_tables in
              getInt2 (Bytes.to_string head) 50
            in
            let buf_loca=StrMap.find "loca" fontInfo_tables in
            let buf_glyf=StrMap.find "glyf" fontInfo_tables in
            let glyf=Buffer.create 256 in
            let loca=Buffer.create 256 in

            let revGlyphMap=Array.make (IntMap.cardinal glyphMap) 0 in
            IntMap.iter (fun old_index gl->revGlyphMap.(gl)<-old_index) glyphMap;
            for i=0 to Array.length revGlyphMap-1 do
              let old_index=revGlyphMap.(i) in
              let off0=
                  if locformat=0 then
                    2*(getInt2 (Bytes.to_string buf_loca) (2*old_index))
                  else
                    getInt4 (Bytes.to_string buf_loca) (4*old_index)
              in
              let off1=
                  if locformat=0 then
                    2*(getInt2 (Bytes.to_string buf_loca) (2*old_index+2))
                  else
                    getInt4 (Bytes.to_string buf_loca) (4*old_index+4)
              in
              if locformat=0 then bufInt2 loca (Buffer.length glyf/2) else bufInt4_int loca (Buffer.length glyf);
              let str=Bytes.sub buf_glyf off0 (off1-off0) in
              if Bytes.length str>0 then (
                let numberOfContours=sgetInt2 (Bytes.to_string str) 0 in
                if numberOfContours<0 then (
                  let rec replace_glyphs i=
                    let old_component=getInt2 (Bytes.to_string str) (i+2) in
                    strInt2 str (i+2) (IntMap.find old_component glyphMap);
                    let flags=getInt2 (Bytes.to_string str) i in
                    if flags land TT_MORE_COMPONENTS <> 0 then (
                      let off_args=if flags land TT_ARGS_ARE_WORDS <>0 then 4 else 2 in
                      let off_trans=
                        (if flags land TT_WE_HAVE_A_SCALE<>0 then 2 else
                            if flags land TT_WE_HAVE_AN_X_AND_Y_SCALE<>0 then 4 else
                              if flags land TT_WE_HAVE_A_TWO_BY_TWO<>0 then 8 else 0)
                      in
                      replace_glyphs (i+4+off_args+off_trans)
                    )
                  in
                  replace_glyphs 10
                ));
              Buffer.add_bytes glyf str
            done;

            if locformat=0 then bufInt2 loca (Buffer.length glyf/2) else bufInt4_int loca (Buffer.length glyf);
            fontInfo.tables<-StrMap.add "loca" (Buffer.to_bytes loca) fontInfo.tables;
            fontInfo.tables<-StrMap.add "glyf" (Buffer.to_bytes glyf) fontInfo.tables
          with
              Not_found->()
  end;

  (* GSUB *)
  begin
  (*   let open Layout in *)
  (*       let scr=StrMap.singleton "latn" StrMap.empty in *)
  (*       let feat=[| {tag="dlig";lookups=[0]} |] in *)
  (*       let lookups= [| make_ligatures [| [[1;2],9] |] |] in *)
  (*       let buf=write_layout scr feat lookups in *)
    fontInfo.tables<-StrMap.remove "GSUB" fontInfo.tables;
    fontInfo.tables<-StrMap.remove "GPOS" fontInfo.tables;
    fontInfo.tables<-StrMap.remove "GDEF" fontInfo.tables
  end;
#ifdef DEBUG_TTF
  Printf.fprintf stderr "names\n";flush stderr;
#endif
  (* names *)
  begin
    try
      let buf=Buffer.create 256 in
      let names=List.fold_left (fun l (language,name,str)->
        let utf16=
          try
            let utf16 = UTFConvert.utf8_to_utf16 str in
            (3,1,language,name,String.sub utf16 2 (String.length utf16-2))::l
          with
              _->(l)
        in
        let mac= utf16 (* FIXME *)
          (*
          try
            (1,0,language,name,ROMAN.from_utf8 str)::utf16
          with
              _->(utf16)
          *)
        in
        mac
      ) [] fontInfo.names
      in
      bufInt2 buf 0;                    (* format *)
      bufInt2 buf (List.length names);
      bufInt2 buf (6+12*(List.length names));
      let buf'=Buffer.create 256 in
      List.iter (fun (a,b,c,d,s)->
        bufInt2 buf a;
        bufInt2 buf b;
        bufInt2 buf c;
        bufInt2 buf d;
        bufInt2 buf (String.length s);
        bufInt2 buf (Buffer.length buf');
        Buffer.add_string buf' s
      ) names;
      Buffer.add_buffer buf buf';
      fontInfo.tables<-StrMap.add "name" (Buffer.to_bytes buf) fontInfo.tables
    with
        Not_found->()
  end

let setName info name=
  info.names<-List.map (fun (lang,id,s)->
    match id with
        1->(lang,id,name.family_name)
      | 2->(lang,id,name.subfamily_name)
      | 4->(lang,id,name.full_name)
      | 6->(lang,id,name.postscript_name)
      | _->(lang,id,s)
  ) info.names

let subset font info cmap glyphs=
  make_tables font info cmap glyphs;
  write_cff info

open Opentype_layout
let add_kerning info kerning_pairs =
  let scr = StrMap.singleton "latn" StrMap.empty in
  let feat = [|{tag="kern";lookups=[0]}|] in
  let lookups=[|make_kerning kerning_pairs|] in
  let buf=write_layout scr feat lookups in
  info.tables<-StrMap.add "GPOS" (Bytes.of_string buf) info.tables

(* now everything must be scaled to 1000.0 *)
let glyph_x0 = glyph_x0 ~orig:false
let glyph_x1 = glyph_x1 ~orig:false
let glyph_y0 = glyph_y0 ~orig:false
let glyph_y1 = glyph_y1 ~orig:false
let glyphWidth = glyphWidth ~orig:false
let outlines = outlines ~orig:false
