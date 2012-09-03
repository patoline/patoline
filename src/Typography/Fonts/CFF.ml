open CamomileLibrary
open Util
open Bezier
open FTypes
let pt_of_mm x=(72.*.x)/.25.4
let mm_of_pt x=(25.4*.x)/.72.


type font= { file:string; offset:int; size:int; offSize:int;
             nameIndex:int array;
             dictIndex:int array;
             stringIndex:int array;
             subrIndex:(string array) array;
             gsubrIndex:string array;
             nominalWidth:float;
             fontMatrix:float list
           }

type glyph= { glyphFont:font; glyphNumber:glyph_id; type2:string;
              glyphContents:UTF8.t;
              mutable glyphWidth:float;
              mutable glyphX0:float; mutable glyphX1:float;
              mutable glyphY0:float; mutable glyphY1:float
            }



let glyphFont gl=gl.glyphFont


exception Index
exception Type2Int of int

type cff_num=CFFFloat of float | CFFInt of int
exception CFFNum of cff_num
let int_of_num=function
    CFFInt x->x
#ifdef DEBUG
  | x ->raise (CFFNum x)
#else
  | CFFFloat f->round f
#endif

let float_of_num=function
    CFFFloat x->x
  | CFFInt x ->float_of_int x

#ifdef DEBUG
let print_num chan=function
    CFFInt x->Printf.fprintf chan "CFFInt %d" x
  | CFFFloat x->Printf.fprintf chan "CFFFloat %f" x
#endif

let readCFFInt f=
  let b0=input_byte f in
    if b0<28 || b0==31 then
      raise (Type2Int b0)
    else
      if b0=30 then
        let first=ref true in
        let next=ref 0 in
        let input_next ()=
          if !first then
            (let a=input_byte f in
               first:=false;
               next:=a land 0xf;
               a lsr 8)
          else
            (first:=true;
             !next)
        in
        let rec parseInt x=
          let a=input_next () in
            if a<=9 then parseInt (x*10+a) else (float_of_int x,a)
        in
        let (a0,b0)=
          let a=input_next () in
            if a = 0xe then let (u,v)=parseInt 0 in (-. u, v) else parseInt a
        in
        let (a1,b1)=
          if b0=0xa then
            let rec makeDecimal x=if (abs_float x)<1. then x else makeDecimal (x /. 10.) in
            let (a1',b1')=parseInt 0 in
              (makeDecimal a1', b1')
          else (0.,b0)
        in
        let (a2,b2)=
          if b1=0xb then parseInt 0 else
            if b1=0xc then
              let (a2',b2')=parseInt 0 in
                (-. a2',b2')
            else
              (0.,b1)
        in
          CFFFloat ((a0+.a1) *. (10. ** a2))
      else
        CFFInt (
          if b0>=32 then
            if b0<=246 then (
              b0-139
            ) else
              let b1=input_byte f in
                if b0<=250 then (
                  let x=(((b0-247) lsl 8) lor b1) + 108 in
                    x
                ) else (
                  let x= - ((((b0-251) lsl 8) lor b1) + 108) in
                    x
                )
          else
            (let b1=input_byte f in
             let b2=input_byte f in
               if b0=28 then (
                 let x=((b1 lsl 8) lor b2) in
                   if x >= 32768 then x - 65536 else x
               ) else (
                 let b3=input_byte f in
                 let b4=input_byte f in
                 let x=(((((b1 lsl 8) lor b2) lsl 8) lor b3) lsl 8) lor b4 in
                   if x>=(1 lsl 31) then x-(1 lsl 31) else x
               )
            )
        )

(* renvoit les offsets dans le fichier des donnees de l'index, à
   partir de la position 0 du fichier *)
let index f idx_off=
  seek_in f idx_off;
  let count=readInt f 2 in
    if count=0 then [|idx_off+2|] else (
      let idx_offSize=input_byte f in
      let idx_arr=Array.make (count+1) 0 in
        for i=0 to count do
          idx_arr.(i) <- idx_off+2+(count+1)*idx_offSize + (readInt f idx_offSize);
        done;
        idx_arr
    )

let strIndex f idx_off=
  let off=index f idx_off in
  let str=Array.create (Array.length off-1) "" in
    for i=0 to Array.length off-2 do
      seek_in f (off.(i));
      str.(i)<-String.create (off.(i+1)-off.(i));
      really_input f (str.(i)) 0 (off.(i+1)-off.(i))
    done;
    str

exception CFF_Not_found of (int*int)
let indexGet f idx_off idx=
  seek_in f idx_off;
  let count=readInt f 2 in
    if idx>=count || idx<0 then raise (CFF_Not_found (idx,count)) else
      (let idx_offSize=input_byte f in
         seek_in f (idx_off+3+idx*idx_offSize);
         let off0=readInt f idx_offSize in
         let off1=readInt f idx_offSize in
         let buf=String.create (off1-off0) in
           seek_in f (idx_off+2+(count+1)*idx_offSize+off0);
           really_input f buf 0 (off1-off0);
           buf
      )
let readDict f a b=
  let rec dict' stack l=
    if pos_in f >= b then l else
      try
        let op=readCFFInt f in
          dict' (op::stack) l
      with
          (Type2Int b0)->
            let op=
              if b0=12 then
                let next=input_byte f in
                  (12 lsl 8) lor next
              else
                b0
            in
              dict' [] ((op,stack)::l)
  in
    seek_in f a;
    dict' [] []

let findDict f a b key=
  let rec dict' stack=
    if pos_in f >= b then raise Not_found else
      try
        let op=readCFFInt f in dict' (op::stack)
      with
          (Type2Int b0)->
            let op=
              if b0=12 then
                let next=input_byte f in (12 lsl 8) lor next
              else
                b0
            in
              if op=key then stack else dict' []
  in
    seek_in f a;
    dict' []


let readEncoding f off=
  seek_in f off;
  let format=input_byte f in
  let enc=if format land 1 =0 then (
    let nCodes=input_byte f in
    let buf=String.create (2+nCodes) in
      seek_in f off;
      really_input f buf 0 (String.length buf);
      buf
  ) else (
    let nCodes=input_byte f in
    let buf=String.create (2+2*nCodes) in
      seek_in f off;
      really_input f buf 0 (String.length buf);
      buf
  )
  in
  let supl=
    let nSups=input_byte f in
    let buf=String.create (1+3*nSups) in
      seek_in f (off+String.length enc);
      really_input f buf 0 (String.length buf);
      buf
  in
    enc,supl


let readCharset f nGlyphs off=
  seek_in f off;
  let format=input_byte f in
    if format =0 then (
      let arr=Array.make nGlyphs 0 in
        for i=1 to nGlyphs-1 do
          arr.(i)<-readInt2 f
        done;
        arr
    ) else (
      let gls=ref 1 in
      let arr=Array.make nGlyphs 0 in
        try
          while !gls<nGlyphs do
            let sid=readInt2 f in
            let nLeft=readInt f format in
              arr.(!gls)<-sid;
              incr gls;
              for i=1 to nLeft-1 do
                arr.(!gls)<-sid+i;
                incr gls
              done;
          done;
          arr
        with
            Invalid_argument _->arr
    )



let loadFont ?offset:(off=0) ?size file=
  let f=open_in_bin_cached file in
  let size=match size with None->in_channel_length f-off | Some s->s in
  seek_in f (off+2);
  let hdrSize=input_byte f in
  let offSize=input_byte f in
  let nameIndex=index f (off+hdrSize) in
  let dictIndex=index f (nameIndex.(Array.length nameIndex-1)) in
  let stringIndex=index f (dictIndex.(Array.length dictIndex-1)) in
  let subrIndex=
    let subrs=Array.create (Array.length dictIndex-1) [||] in
    for idx=0 to Array.length dictIndex-2 do
      try
        let privOffset=findDict f (dictIndex.(idx)) (dictIndex.(idx+1)) 18 in
        match privOffset with
            offset::size::_->(
              try
                let subrsOffset=int_of_num (List.hd (findDict f (off+int_of_num offset) (off+int_of_num offset+int_of_num size) 19)) in
                subrs.(idx) <- strIndex f (off+int_of_num offset+subrsOffset);
              with
                  Not_found -> ()
            )
          | _->()
      with
          Not_found -> ()
    done;
    subrs
  in
  let gsubrIndex=
    let offIndex=index f (stringIndex.(Array.length stringIndex-1)) in
    let gsubr=Array.create (Array.length offIndex-1) "" in
    for i=0 to Array.length gsubr-1 do
      seek_in f (offIndex.(i));
      gsubr.(i)<-String.create (offIndex.(i+1)-offIndex.(i));
      really_input f gsubr.(i) 0 (offIndex.(i+1)-offIndex.(i))
    done;
    gsubr
  in
  let nominalWidth=
    try
      let privOffset=findDict f (dictIndex.(0)) (dictIndex.(1)) 18 in
      match privOffset with
          offset::size::_->float_of_num (
            let w=List.hd (findDict f (off+int_of_num offset) (off+int_of_num offset+int_of_num size) 21) in
            w
          )
        | _->0.
    with
        _->0.
  in
  let fontMatrix=
    try
      List.map (function
      CFFFloat x->x
        | x->raise (CFFNum x))
        (findDict f dictIndex.(0) dictIndex.(1) 3079)
    with
        Not_found | CFFNum _->[0.001;0.;0.;0.001]
  in

    { file=file; offset=off; size=size; offSize=offSize; nameIndex=nameIndex; dictIndex=dictIndex;
      stringIndex=stringIndex; gsubrIndex=gsubrIndex; subrIndex=subrIndex;
      nominalWidth=nominalWidth; fontMatrix=fontMatrix
    }

let glyph_of_uchar _ _=0
let glyph_of_char f c=glyph_of_uchar f (CamomileLibrary.UChar.of_char c)

let loadGlyph font ?index:(idx=0) gl=
  let file=open_in_bin_cached font.file in
  let charStrings=int_of_num (List.hd (findDict file font.dictIndex.(idx) font.dictIndex.(idx+1) 17)) in
  let x=
    { glyphFont=font;
      glyphNumber=gl;
      type2=indexGet file (font.offset+charStrings) gl.glyph_index;
      glyphContents=gl.glyph_utf8;
      glyphWidth=infinity;
      glyphX0=infinity; glyphX1= -.infinity;
      glyphY0=infinity; glyphY1= -.infinity }
  in
    x

let cardinal font=
  let idx=0 in
  let file=open_in_bin_cached font.file in
  let charStrings=int_of_num (List.hd (findDict file font.dictIndex.(idx) font.dictIndex.(idx+1) 17)) in
    seek_in file (font.offset+charStrings);
    readInt2 file


let outlines glyph=Type2.outlines_ glyph.glyphFont.subrIndex.(0) glyph.glyphFont.gsubrIndex glyph.type2 false
let glyphWidth glyph=
  if glyph.glyphWidth=infinity then
    glyph.glyphWidth<-(
      try let _=Type2.outlines_ glyph.glyphFont.subrIndex.(0) glyph.glyphFont.gsubrIndex glyph.type2 true in raise (Type2.Found 0.) with
          Type2.Found x->glyph.glyphFont.nominalWidth+.x
    );
    glyph.glyphWidth

let glyphName glyph=""

let glyphContents gl=gl.glyphContents
let compute_bb gl=
  List.iter (List.iter (fun (x,y)->
                          let (a,b)=Bezier.bernstein_extr x in
                          let (c,d)=Bezier.bernstein_extr y in
                            gl.glyphX0<-min gl.glyphX0 a;
                            gl.glyphX1<-max gl.glyphX1 b;
                            gl.glyphY0<-min gl.glyphY0 c;
                            gl.glyphY1<-max gl.glyphY1 d)
            )
    (outlines gl)


let glyph_y0 gl=
  if gl.glyphY0 = infinity then compute_bb gl;
  gl.glyphY0

let glyph_y1 gl=
  if gl.glyphY1 = -.infinity then compute_bb gl;
  gl.glyphY1

let glyph_x0 gl=
  if gl.glyphX0 = infinity then compute_bb gl;
  gl.glyphX0

let glyph_x1 gl=
  if gl.glyphX1 = -.infinity then compute_bb gl;
  gl.glyphX1


let glyphNumber glyph=glyph.glyphNumber

let fontName ?index:(idx=0) font=
  let buf = String.create (font.nameIndex.(idx+1)-font.nameIndex.(idx)) in
  let f=open_in_bin_cached font.file in
    seek_in f (font.nameIndex.(idx));
    really_input f buf 0 (font.nameIndex.(idx+1)-font.nameIndex.(idx));
    buf

let fontBBox ?index:(idx=0) font=
  let f=open_in_bin_cached font.file in
  try
    match findDict f font.dictIndex.(idx) font.dictIndex.(idx+1) 5 with
        (a::b::c::d::_)->(int_of_num d,int_of_num c,int_of_num b,int_of_num a)
      | _->(0,0,0,0)
  with
      Not_found->(0,0,0,0)


let italicAngle ?index:(idx=0) font=
  let f=open_in_bin_cached font.file in
    try
      match findDict f font.dictIndex.(idx) font.dictIndex.(idx+1) 0x0c02 with
          h::_->float_of_num h
        | _->0.
    with
        Not_found->0.

let font_features _ =[]
let select_features _ _=[]
let positioning _ x=x

(*****************************************************************************)

let writeIndex buf data=
  let dataSize=Array.fold_left (fun s str->s+String.length str) 0 data in
  let rec offSize_ i res=if i=0 then res else offSize_ (i lsr 8) (res+1) in
  let offSize=if Array.length data=0 then 1 else offSize_ dataSize 0 in
  let count0=(Array.length data) lsr 8 in
  let count1=(Array.length data) land 0xff in
  Rbuffer.add_char buf (char_of_int count0);
  Rbuffer.add_char buf (char_of_int count1);
  Rbuffer.add_char buf (char_of_int offSize);
  let dat=Rbuffer.create 100 in
  let offsets=Array.make (1+Array.length data) 1 in
  for i=1 to Array.length offsets-1 do
    Rbuffer.add_string dat data.(i-1);
    offsets.(i)<-offsets.(i-1)+(String.length data.(i-1));
  done;
  for i=0 to Array.length offsets-1 do
    if offSize=1 then
      Rbuffer.add_char buf (char_of_int offsets.(i))
    else if offSize=2 then (
      let a=offsets.(i) in
      let b=a lsr 8 in
      Rbuffer.add_char buf (char_of_int (b land 0xff));
      Rbuffer.add_char buf (char_of_int (a land 0xff))
    ) else if offSize=3 then (
      let a=offsets.(i) in
      let b=a lsr 8 in
      let c=b lsr 8 in
      Rbuffer.add_char buf (char_of_int (c land 0xff));
      Rbuffer.add_char buf (char_of_int (b land 0xff));
      Rbuffer.add_char buf (char_of_int (a land 0xff))
    ) else if offSize=4 then (
      let a=offsets.(i) in
      let b=a lsr 8 in
      let c=b lsr 8 in
      let d=c lsr 8 in
      Rbuffer.add_char buf (char_of_int (d land 0xff));
      Rbuffer.add_char buf (char_of_int (c land 0xff));
      Rbuffer.add_char buf (char_of_int (b land 0xff));
      Rbuffer.add_char buf (char_of_int (a land 0xff))
    )
  done;
  Rbuffer.add_buffer buf dat

let writeCFFInt buf x=
  if x>=(-107) && x<107 then (
    let y=(char_of_int (x+139)) in
      Rbuffer.add_char buf y
  ) else if x>=108 && x<=1131 then (
    let b1=(x-108) in
    let b0=(b1/256)+247 in
      Rbuffer.add_char buf (char_of_int (b0 land 0xff));
      Rbuffer.add_char buf (char_of_int (b1 land 0xff))
  ) else if x>=(-1131) && x<=(-108) then (
    let b1=(-x-108) in
    let b0=(b1/256)+251 in
      Rbuffer.add_char buf (char_of_int (b0 land 0xff));
      Rbuffer.add_char buf (char_of_int (b1 land 0xff))
  ) else if x>=(-32768) && x<=32767 then (
    Rbuffer.add_char buf (char_of_int 28);
    let y=if x>=0 then x else x+65536 in
      Rbuffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
      Rbuffer.add_char buf (char_of_int (y land 0xff));
  ) else if x>=(-(1 lsl 31)) && x<=(1 lsl 31)-1 then (
    Rbuffer.add_char buf (char_of_int 29);
    let y=if x>=0 then x else x+(1 lsl 32) in
      Rbuffer.add_char buf (char_of_int ((y lsr 24) land 0xff));
      Rbuffer.add_char buf (char_of_int ((y lsr 16) land 0xff));
      Rbuffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
      Rbuffer.add_char buf (char_of_int (y land 0xff))
  )


let rec writeDict buf=function
    []->()
  | (op,stack)::s when op>=15 && op<=19->(
        List.iter (fun x->
                     Rbuffer.add_char buf (char_of_int 29);
                     let y=int_of_num x in
                       Rbuffer.add_char buf (char_of_int ((y lsr 24) land 0xff));
                       Rbuffer.add_char buf (char_of_int ((y lsr 16) land 0xff));
                       Rbuffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
                       Rbuffer.add_char buf (char_of_int (y land 0xff)))
          stack;
        Rbuffer.add_char buf (char_of_int (op land 0xff));
        writeDict buf s
    )
  | (op,stack)::s->(
      try
        List.iter (fun x->writeCFFInt buf (int_of_num x)) (List.rev stack);
        if op>0xff then Rbuffer.add_char buf (char_of_int (op lsr 8));
        Rbuffer.add_char buf (char_of_int (op land 0xff));
        writeDict buf s
      with
          _->writeDict buf s
    )

let readIndex f idx_off=
  seek_in f idx_off;
  let count=readInt f 2 in
  let idx_offSize=input_byte f in
  let last_idx=ref 1 in
  let idx_arr=Array.make count "" in
    for i=1 to count do
      seek_in f (idx_off+3+i*idx_offSize);
      let idx=readInt f idx_offSize in
        idx_arr.(i-1)<-String.create (idx - !last_idx);
        seek_in f (idx_off+2+(count+1)*idx_offSize+ !last_idx);
        really_input f (idx_arr.(i-1)) 0 (idx - !last_idx);
        last_idx:=idx;
    done;
    idx_arr

let skipIndex f=
  let idx_off=pos_in f in
  let count=readInt f 2 in
  let idx_offSize=input_byte f in
    seek_in f (idx_off+3+count*idx_offSize);
    let off=readInt f idx_offSize in
      seek_in f (idx_off+2+(count+1)*idx_offSize+off)

let copyIndex f=
  let idx_off=pos_in f in
  let count=readInt f 2 in
    if count=0 then (
      seek_in f idx_off;
      let buf=String.create 2 in
        really_input f buf 0 2;
        buf
    ) else (
      let idx_offSize=input_byte f in
        seek_in f (idx_off+3+count*idx_offSize);
        let off=readInt f idx_offSize in
        let buf=String.create (2+(count+1)*idx_offSize+off) in
          seek_in f idx_off;
          really_input f buf 0 (String.length buf);
          buf
    )

type encoding=
    StdEncoding of int
  | Encoding of string


let subset font gls=
  let buf=Rbuffer.create 100 in
  let f=open_in_bin_cached font.file in
  seek_in f (font.offset+2);
  let headersize=input_byte f in
  seek_in f font.offset;
  Rbuffer.add_channel buf f headersize;
  let name=
    let buf_=String.create (font.nameIndex.(1)-font.nameIndex.(0)) in
    seek_in f (font.offset+headersize);
    really_input f buf_ 0 (String.length buf_);
    buf_
  in
  writeIndex buf [|name|];

  let strings=ref StrMap.empty in
  let topDict=
    readDict f font.dictIndex.(0) font.dictIndex.(1)
  in
  let topDict=List.map (fun (op,st)->
    if (op<=4) || op=0xc00 then (
      let idx=int_of_num (List.hd st) in
      if idx<=390 then (op,st) else (
        let off0=font.stringIndex.(idx-391) in
        let off1=font.stringIndex.(idx-390) in
        let str=String.create (off1-off0) in
        seek_in f off0;
        really_input f str 0 (off1-off0);
        let num=
          try
            StrMap.find str !strings
          with
              Not_found->(
                strings:=StrMap.add str (StrMap.cardinal !strings) !strings;
                StrMap.cardinal !strings-1
              )
        in
        (op,[CFFInt (391+num)])
      )
    ) else (op,st)
  ) topDict
  in

  let topDictBuf_=Rbuffer.create 16 in
  writeDict topDictBuf_ topDict;     (* Premiere tentative pour connaitre la taille *)
  let topDictBuf=Rbuffer.create 16 in
  writeIndex topDictBuf [|Rbuffer.contents topDictBuf_|];

  seek_in f (font.stringIndex.(Array.length font.stringIndex-1));
  let gsubr=copyIndex f in
  let encoding,encodingLength=
    try
      let enc=int_of_num (List.hd (findDict f font.dictIndex.(0) font.dictIndex.(1) 16)) in
      if enc=0 || enc=1 then StdEncoding enc,0 else
        let a,b=readEncoding f enc in
        let encc=a^b in
        Encoding encc, String.length encc
    with
        Not_found -> StdEncoding 0, 0
  in
  let charset=
    try
      let set=int_of_num (List.hd (findDict f font.dictIndex.(0) font.dictIndex.(1) 15)) in
      let charset=readCharset f (cardinal font) (font.offset+set) in
      let str=String.make (2*Array.length gls-1) (char_of_int 0) in
      for i=1 to Array.length gls-1 do
        let idx=charset.(gls.(i)) in
        let num=
          if idx-390>=Array.length font.stringIndex then (
            try
              StrMap.find "" !strings
            with
                Not_found->(
                  strings:=StrMap.add "" (StrMap.cardinal !strings) !strings;
                  StrMap.cardinal !strings-1
                )
          ) else if idx<=390 then idx else (
            let off0=font.stringIndex.(idx-391) in
            let off1=font.stringIndex.(idx-390) in
            let b=String.create (off1-off0) in
            seek_in f off0;
            really_input f b 0 (off1-off0);
            391+(
              try
                StrMap.find b !strings
              with
                  Not_found->(
                    strings:=StrMap.add b (StrMap.cardinal !strings) !strings;
                    StrMap.cardinal !strings-1
                  )
            )
          )
        in
        str.[2*i-1]<-char_of_int (num lsr 8);
        str.[2*i]<-char_of_int (num land 0xff)
      done;
      str
    with
        Not_found -> String.make (2*Array.length gls-1) (char_of_int 0)
  in

  let charStrings=
    let charStrings=int_of_num (List.hd (findDict f font.dictIndex.(0) font.dictIndex.(1) 17)) in
    let progs=Array.map (fun x->indexGet f (font.offset+charStrings) x) gls in
    let buf=Rbuffer.create 100 in
    writeIndex buf progs;
    buf
  in
  let priv,lsubr=
    match findDict f (font.dictIndex.(0)) (font.dictIndex.(1)) 18 with
        offset::size::_->(
          try
            let priv=readDict f (font.offset+int_of_num offset) (font.offset+int_of_num offset+int_of_num size) in
            let subr=try (
              match findDict f (font.offset+int_of_num offset) (font.offset+int_of_num offset+int_of_num size) 19 with
                  offset_::_->(
                    seek_in f (font.offset+int_of_num offset+int_of_num offset_);
                    copyIndex f
                  )
                | _->let bubu=Rbuffer.create 2 in writeIndex bubu [||];Rbuffer.contents bubu
            ) with
                Not_found
              | CFFNum _-> ""
            in
            priv,subr
          with
              Not_found | CFFNum _-> [],""
        )
      | _->[],""
  in

  let strIndex=Rbuffer.create 256 in
  let strArray=Array.make (StrMap.cardinal !strings) "" in
  StrMap.fold (fun k a ()->strArray.(a)<-k) !strings ();
  writeIndex strIndex strArray;

  let privDict=Rbuffer.create 16 in
  let priv0=
    List.rev
      (if List.mem_assoc 19 priv then
          (19, [CFFInt 0])::
            List.remove_assoc 19 priv
       else priv)
  in
  writeDict privDict priv0;
  let privLength=Rbuffer.length privDict in
  Rbuffer.reset privDict;

  writeDict privDict
    (List.rev
       (if List.mem_assoc 19 priv then
           (19, [CFFInt privLength])::
             List.remove_assoc 19 priv
        else priv));
  let topDict0=
    (if List.mem_assoc 17 topDict then
        (17, [CFFInt (Rbuffer.length buf + Rbuffer.length topDictBuf +
                        Rbuffer.length strIndex + String.length gsubr +
                        encodingLength + String.length charset)])::
          List.remove_assoc 17 topDict
     else topDict)
  in
  let topDict1=
    (if List.mem_assoc 18 topDict0 then
        (18, [CFFInt (Rbuffer.length privDict);
              CFFInt (Rbuffer.length buf + Rbuffer.length topDictBuf +
                        Rbuffer.length strIndex + String.length gsubr +
                        encodingLength + String.length charset +
                        Rbuffer.length charStrings)
             ])::
          List.remove_assoc 18 topDict0
     else topDict0)
  in
  let topDict2=                     (* encoding *)
    match encoding with
        Encoding s when List.mem_assoc 16 topDict -> (
          (16, [CFFInt (Rbuffer.length buf + Rbuffer.length topDictBuf +
                          Rbuffer.length strIndex + String.length gsubr)])::
            List.remove_assoc 16 topDict1
        )
      | StdEncoding n when List.mem_assoc 16 topDict ->
        (16, [CFFInt n]) :: List.remove_assoc 16 topDict1
      | _ ->
        topDict1
  in
  let topDict3=                     (* Charset *)
    (15, [CFFInt (Rbuffer.length buf + Rbuffer.length topDictBuf +
                    Rbuffer.length strIndex + String.length gsubr +
                    encodingLength)])::
      List.remove_assoc 15 topDict2
  in
  Rbuffer.reset topDictBuf_;
  writeDict topDictBuf_ (List.rev topDict3);     (* Premiere tentative pour connaitre la taille *)

  writeIndex buf [|Rbuffer.contents topDictBuf_|];
  Rbuffer.add_buffer buf strIndex;
  Rbuffer.add_string buf gsubr;
  (match encoding with
      Encoding enc->Rbuffer.add_string buf enc;
    | StdEncoding s->());
  Rbuffer.add_string buf charset;
  Rbuffer.add_buffer buf charStrings;
  Rbuffer.add_buffer buf privDict;
  Rbuffer.add_string buf lsubr;
  buf
