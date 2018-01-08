open UsualMake
open Util
open FTypes

let pt_of_mm x=(72.*.x)/.25.4
let mm_of_pt x=(25.4*.x)/.72.

let extensions = [".cff"]

type font= { file:string; offset:int; size:int; offSize:int;
             nameIndex:int array;
             dictIndex:int array;
             stringIndex:int array;
             subrIndex:(string array) array;
             gsubrIndex:string array;
             nominalWidth:float;
             defaultWidth:float;
             fontMatrix:float list
           }

type glyph= { glyphFont:font; glyphNumber:glyph_id; type2:string;
              glyphContents:string;
              mutable glyphWidth:float;
              mutable glyphX0:float; mutable glyphX1:float;
              mutable glyphY0:float; mutable glyphY1:float
            }



let glyphFont gl=gl.glyphFont
#define DEBUG

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
let uniqueName f=f.file

let readCFFNum f=
  let b0=input_byte f in
    if b0<28 || b0==31 then
      raise (Type2Int b0)
    else
      if b0=30 then
        let pos=pos_in f in
        let rec compute_len i=
          let cur=input_byte f in
          if cur land 0xf = 0xf || cur lsr 4 = 0xf then
            i
          else
            compute_len (i+1)
        in
        let n=compute_len 1 in
        let arr=Array.make (2*n) 0 in
        seek_in f pos;
        for i=0 to n-1 do
          let x=input_byte f in
          arr.(2*i)<-x lsr 4;
          arr.(2*i+1)<-x land 0xf
        done;
        let sign=if arr.(0)=0xe then -.1. else 1. in
        let rec get_int i x=
          if i>=Array.length arr || arr.(i)>=0xa then x,i else
            get_int (i+1) (x*.10.+.float_of_int arr.(i))
        in
        let intPart,i0=get_int (if arr.(0)=0xe then 1 else 0) 0. in
        let decPart,i1=
          if i0>=Array.length arr then 0.,i0 else
            if arr.(i0)<>0xa then 0.,i0 else (
              let dec,i1=get_int (i0+1) 0. in
              dec/.(10.**(float_of_int (i1-i0-1))), i1
            )
        in
        let exp=
          if i1>=Array.length arr then 1. else
            let sign=if arr.(i1)=0xb then 1. else -.1. in
            let e,_=get_int (i1+1) 0. in
            sign*.(e)
        in
        let x= ((sign*.(intPart+.decPart)) *. (10. ** exp)) in
        CFFFloat x
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
  let str=Array.make (Array.length off-1) "" in
    for i=0 to Array.length off-2 do
      seek_in f (off.(i));
      str.(i)<-Bytes.create (off.(i+1)-off.(i));
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
         let buf=Bytes.create (off1-off0) in
           seek_in f (idx_off+2+(count+1)*idx_offSize+off0);
           really_input f buf 0 (off1-off0);
           buf
      )
let readDict f a b=
  let rec dict' stack l=
    if pos_in f >= b then l else
      try
        let op=readCFFNum f in
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
            dict' [] (IntMap.add op stack l)
  in
    seek_in f a;
    dict' [] IntMap.empty

let findDict f a b key=
  let rec dict' stack=
    if pos_in f >= b then raise Not_found else
      try
        let op=readCFFNum f in dict' (op::stack)
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
    let buf=Bytes.create (2+nCodes) in
      seek_in f off;
      really_input f buf 0 (String.length buf);
      buf
  ) else (
    let nCodes=input_byte f in
    let buf=Bytes.create (2+2*nCodes) in
      seek_in f off;
      really_input f buf 0 (String.length buf);
      buf
  )
  in
  let supl=
    let nSups=input_byte f in
    let buf=Bytes.create (1+3*nSups) in
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
    while !gls<nGlyphs do
      let sid=readInt2 f in
      let nLeft=if format=1 then input_byte f else readInt2 f in
      arr.(!gls)<-sid;
      incr gls;
      for i=1 to nLeft do
        arr.(!gls)<-sid+i;
        incr gls
      done;
    done;
    arr
  )

let useCharset f off glyph=
  seek_in f off;
  let format=input_byte f in
  if glyph=0 then 0 else (
    let glyph=glyph-1 in
    if format =0 then (
      seek_in f (off+1+glyph*2);
      readInt2 f
    ) else (
      let rec find_idx gls=
        let sid=readInt2 f in
        let nLeft=if format=1 then input_byte f else readInt2 f in
        if gls+nLeft < glyph then find_idx (gls+1+nLeft) else (
          sid+(glyph-gls)
        )
      in
      let x=find_idx 0 in
      x
    )
  )



let loadFont ?offset:(off=0) ?size:(size=None) file=
  let f=open_in_bin_cached file in
  let size=match size with None->in_channel_length f-off | Some s->s in
  seek_in f (off+2);
  let hdrSize=input_byte f in
  let offSize=input_byte f in
  let nameIndex=index f (off+hdrSize) in
  let dictIndex=index f (nameIndex.(Array.length nameIndex-1)) in
  let stringIndex=index f (dictIndex.(Array.length dictIndex-1)) in
  let subrIndex=
    let subrs=Array.make (Array.length dictIndex-1) [||] in
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
    let gsubr=Array.make (Array.length offIndex-1) "" in
    for i=0 to Array.length gsubr-1 do
      seek_in f (offIndex.(i));
      gsubr.(i)<-Bytes.create (offIndex.(i+1)-offIndex.(i));
      really_input f gsubr.(i) 0 (offIndex.(i+1)-offIndex.(i))
    done;
    gsubr
  in
  let nominalWidth,defaultWidth=
    try
      let privOffset=findDict f (dictIndex.(0)) (dictIndex.(1)) 18 in
      match privOffset with
          offset::size::_->(
            let w=List.hd (findDict f (off+int_of_num offset) (off+int_of_num offset+int_of_num size) 21) in
            let w'=List.hd (findDict f (off+int_of_num offset) (off+int_of_num offset+int_of_num size) 20) in
            float_of_num w,float_of_num w'
          )
        | _->0.,0.
    with
        _->0.,0.
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
      nominalWidth=nominalWidth; defaultWidth=defaultWidth; fontMatrix=fontMatrix
    }

let glyph_of_uchar _ _=0
let glyph_of_char f c=glyph_of_uchar f (UChar.of_char c)

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
      try let _=Type2.outlines_ glyph.glyphFont.subrIndex.(0) glyph.glyphFont.gsubrIndex glyph.type2 true in raise Not_found with
          Type2.Found x->(
            glyph.glyphFont.nominalWidth+.x
          )
        | Not_found->glyph.glyphFont.defaultWidth
    );
    glyph.glyphWidth



let glyphContents gl=gl.glyphContents
let compute_bb gl=
  List.iter (List.iter (fun (x,y)->
                          let (a,b)=FBezier.bernstein_extr x in
                          let (c,d)=FBezier.bernstein_extr y in
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

let glyphLSB = glyph_x0

let glyphNumber glyph=glyph.glyphNumber


let fontName ?index:(idx=0) font=
  (* postscript name *)
  let buf = Bytes.create (font.nameIndex.(idx+1)-font.nameIndex.(idx)) in
  let f=open_in_bin_cached font.file in
  seek_in f (font.nameIndex.(idx));
  really_input f buf 0 (font.nameIndex.(idx+1)-font.nameIndex.(idx));

  let retrieve_name i=
    let idx=int_of_num (List.hd (findDict f font.dictIndex.(0) font.dictIndex.(1) i)) in
    if idx<=390 then CFFStd.stdStrings.(idx) else (
      let off0=font.stringIndex.(idx-391) in
      let off1=font.stringIndex.(idx-390) in
      let b=Bytes.create (off1-off0) in
      seek_in f off0;
      really_input f b 0 (off1-off0);
      b
    )
  in
  (* family name *)
  let family_name=retrieve_name 3 in
  let full_name=retrieve_name 2 in
  let subfamily_name=retrieve_name 4 in
  {
    postscript_name=buf;
    full_name=full_name;
    family_name=family_name;
    subfamily_name=subfamily_name
  }

let glyphName glyph=
  let font=glyphFont glyph in
  let f=open_in_bin_cached font.file in
  let i=(glyphNumber glyph).glyph_index in
  let set=int_of_num (List.hd (findDict f font.dictIndex.(0) font.dictIndex.(1) 15)) in
  let idx=useCharset f (font.offset+set) i in
  (* Printf.fprintf stderr "glyphName : %S %d -> %d\n" (fontName font).postscript_name i idx; *)
  (* flush stderr; *)
  if idx<=390 then CFFStd.stdStrings.(idx) else (
    let off0=font.stringIndex.(idx-391) in
    let off1=font.stringIndex.(idx-390) in
    let b=Bytes.create (off1-off0) in
    seek_in f off0;
    really_input f b 0 (off1-off0);
    b
  )

let fontBBox ?index:(idx=0) font=
  let f=open_in_bin_cached font.file in
  try
    match findDict f font.dictIndex.(idx) font.dictIndex.(idx+1) 5 with
      (a::b::c::d::_)->
      let lsb = int_of_num d in
      let descender = int_of_num c in
      let xmax = int_of_num b in
      let ascender = int_of_num a in
      (lsb,descender,xmax,ascender)
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

type feature_set=unit
let font_features _ =[]
let select_features _ _=()
let apply_features _ _ x=x
let positioning _ x=x

(*****************************************************************************)

let writeIndex buf data=
  let dataSize=Array.fold_left (fun s str->s+String.length str) 1 data in
  let rec offSize_ i res=if i=0 then res else offSize_ (i lsr 8) (res+1) in
  let offSize=if Array.length data=0 then 1 else offSize_ dataSize 0 in
  let count0=(Array.length data) lsr 8 in
  let count1=(Array.length data) land 0xff in
  Buffer.add_char buf (char_of_int count0);
  Buffer.add_char buf (char_of_int count1);
  Buffer.add_char buf (char_of_int offSize);
  let dat=Buffer.create 100 in
  let offsets=Array.make (1+Array.length data) 1 in
  for i=1 to Array.length offsets-1 do
    Buffer.add_string dat data.(i-1);
    offsets.(i)<-offsets.(i-1)+(String.length data.(i-1));
  done;
  for i=0 to Array.length offsets-1 do
    if offSize=1 then
      Buffer.add_char buf (char_of_int offsets.(i))
    else if offSize=2 then (
      let a=offsets.(i) in
      let b=a lsr 8 in
      Buffer.add_char buf (char_of_int (b land 0xff));
      Buffer.add_char buf (char_of_int (a land 0xff))
    ) else if offSize=3 then (
      let a=offsets.(i) in
      let b=a lsr 8 in
      let c=b lsr 8 in
      Buffer.add_char buf (char_of_int (c land 0xff));
      Buffer.add_char buf (char_of_int (b land 0xff));
      Buffer.add_char buf (char_of_int (a land 0xff))
    ) else if offSize=4 then (
      let a=offsets.(i) in
      let b=a lsr 8 in
      let c=b lsr 8 in
      let d=c lsr 8 in
      Buffer.add_char buf (char_of_int (d land 0xff));
      Buffer.add_char buf (char_of_int (c land 0xff));
      Buffer.add_char buf (char_of_int (b land 0xff));
      Buffer.add_char buf (char_of_int (a land 0xff))
    )
  done;
  Buffer.add_buffer buf dat

let writeCFFInt buf x=
  (* let l0=Buffer.length buf in *)
  if x>=(-107) && x<107 then (
    let y=(char_of_int (x+139)) in
      Buffer.add_char buf y
  ) else if x>=108 && x<=1131 then (
    let v=x-108 in
    let b1=v land 0xff in
    let b0=v lsr 8 + 247 in
      Buffer.add_char buf (char_of_int (b0 land 0xff));
      Buffer.add_char buf (char_of_int (b1 land 0xff))
  ) else if x>=(-1131) && x<=(-108) then (
    let nx= -x - 108 in
    let b1=nx land 0xff in
    let b0=(nx lsr 8) +251 in
    Buffer.add_char buf (char_of_int (b0 land 0xff));
    Buffer.add_char buf (char_of_int (b1 land 0xff))
  ) else if x>=(-32768) && x<=32767 then (
    Buffer.add_char buf (char_of_int 28);
    let y=if x>=0 then x else x+65536 in
      Buffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
      Buffer.add_char buf (char_of_int (y land 0xff));
  ) else if x>=(-(1 lsl 31)) && x<=(1 lsl 31)-1 then (
    Buffer.add_char buf (char_of_int 29);
    let y=if x>=0 then x else x+(1 lsl 32) in
      Buffer.add_char buf (char_of_int ((y lsr 24) land 0xff));
      Buffer.add_char buf (char_of_int ((y lsr 16) land 0xff));
      Buffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
      Buffer.add_char buf (char_of_int (y land 0xff))
  )
      (*
  let l1=Buffer.length buf in
  let s=Buffer.contents buf in
  Printf.fprintf stderr "writeCFFInt %d \"" x;
  for i=l0 to l1-1 do
    Printf.fprintf stderr "%d " (int_of_char s.[i])
  done;
  Printf.fprintf stderr "\"\n";flush stderr
      *)

exception Encoding_problem
let writeCFFFloat buf x=
  let s=Printf.sprintf "%f" x in
  let s=if s.[String.length s-1]='.' then (String.sub s 0 (String.length s-1)) else s in
  let tmp=ref 0 in
  let i=ref 0 in
  let parity=ref 0 in
  Buffer.add_char buf (char_of_int 30);
  (try
     while !i<String.length s do
       let nibble=
         match s.[!i] with
             '0'->0x0 | '1'->0x1 | '2'->0x2 | '3'->0x3 | '4'->0x4
           | '5'->0x5 | '6'->0x6 | '7'->0x7 | '8'->0x8 | '9'->0x9
           | '.'->0xa
           | 'e' when s.[!i+1]='-' -> (incr i; 0xc)
           | 'e'->0xb
           | '-'->0xe
           | _->raise Encoding_problem
       in
       (if !parity mod 2=0 then tmp:=nibble lsl 4 else (
         Buffer.add_char buf (char_of_int (!tmp lor nibble))
        ));
       incr parity;
       incr i
     done;
   with
       Encoding_problem->());
  if !parity mod 2=0 then Buffer.add_char buf (char_of_int 0xff) else
    Buffer.add_char buf (char_of_int (!tmp lor 0xf))


let writeDict buf dict=
  let dict=
    try
      let ros=IntMap.find 0xc1e dict in
      List.iter (fun x->match x with
          CFFInt y->writeCFFInt buf y
        | CFFFloat y->writeCFFFloat buf y
      ) (List.rev ros);
      Buffer.add_char buf (char_of_int 0xc);
      Buffer.add_char buf (char_of_int 0x1e);
      IntMap.remove 0xc1e dict
    with
        Not_found->dict
  in
  let dict=
    try
      let synthetic=IntMap.find 0xc14 dict in
      List.iter (fun x->match x with
          CFFInt y->writeCFFInt buf y
        | CFFFloat y->writeCFFFloat buf y
      ) (List.rev synthetic);
      Buffer.add_char buf (char_of_int 0xc);
      Buffer.add_char buf (char_of_int 0x14);
      IntMap.remove 0xc14 dict
    with
        Not_found->dict
  in
  IntMap.iter (fun op stack->
    (* Printf.fprintf stderr "op=%d\n" op; *)
    if (op>=15 && op<=19) || op=0xc24 || op=0xc25 then (
      List.iter (fun x->
        Buffer.add_char buf (char_of_int 29);
        let y=int_of_num x in
        (* Printf.fprintf stderr "st=%d\n" y; *)
        Buffer.add_char buf (char_of_int ((y lsr 24) land 0xff));
        Buffer.add_char buf (char_of_int ((y lsr 16) land 0xff));
        Buffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
        Buffer.add_char buf (char_of_int (y land 0xff)))
        stack;
      if op>0xff then Buffer.add_char buf (char_of_int (op lsr 8));
      Buffer.add_char buf (char_of_int (op land 0xff))
    ) else (
      List.iter (fun x->match x with
          CFFInt y->(
            (* Printf.fprintf stderr "st=%d\n" y; *)
            writeCFFInt buf y
          )
        | CFFFloat y->(
          (* Printf.fprintf stderr "st=%g\n" y; *)
          writeCFFFloat buf y;
        )
      ) (List.rev stack);
      if op>0xff then Buffer.add_char buf (char_of_int (op lsr 8));
      Buffer.add_char buf (char_of_int (op land 0xff));
    )
  ) dict


let readIndex f idx_off=
  seek_in f idx_off;
  let count=readInt f 2 in
  let idx_offSize=input_byte f in
  let last_idx=ref 1 in
  let idx_arr=Array.make count "" in
    for i=1 to count do
      seek_in f (idx_off+3+i*idx_offSize);
      let idx=readInt f idx_offSize in
        idx_arr.(i-1)<-Bytes.create (idx - !last_idx);
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
      let buf=Bytes.create 2 in
        really_input f buf 0 2;
        buf
    ) else (
      let idx_offSize=input_byte f in
        seek_in f (idx_off+3+count*idx_offSize);
        let off=readInt f idx_offSize in
        let buf=Bytes.create (2+(count+1)*idx_offSize+off) in
          seek_in f idx_off;
          really_input f buf 0 (String.length buf);
          buf
    )

type fontInfo={
  mutable name:FTypes.name
}

let fontInfo f=
  { name=fontName f }

let setName info name=info.name<-name

let rec part n l=
  if n<=0 then [],l else
    (match l with
        []->([],[])
      | h::s->
        let (a,b)=part (n-1) s in
        (h::a,b))

(*

(* Version CID *)

#define CID

let subset font info cmap gls=
  let f=open_in_bin_cached font.file in
  let topDict=readDict f font.dictIndex.(0) font.dictIndex.(1) in
  let strings=ref StrMap.empty in
  let getSid s=
    391+(
      try StrMap.find s !strings with
          Not_found->
            (strings:=StrMap.add s (StrMap.cardinal !strings) !strings;
             (StrMap.cardinal !strings-1))
    )
  in


  let outBuf=Buffer.create 1000 in
  let headersize=4 in
  Buffer.add_char outBuf (char_of_int 1);
  Buffer.add_char outBuf (char_of_int 0);
  Buffer.add_char outBuf (char_of_int headersize);
  Buffer.add_char outBuf (char_of_int 4);
  let f=open_in_bin_cached font.file in

  (* Ecriture du nameIndex *)
  let name=
    let buf_=Bytes.create (font.nameIndex.(1)-font.nameIndex.(0)) in
    seek_in f (font.nameIndex.(0));
    really_input f buf_ 0 (String.length buf_);
    buf_
  in
  writeIndex outBuf [|name|];

  let topDictOffset=Buffer.length outBuf in


  let gsubrBuf=
    seek_in f (font.stringIndex.(Array.length font.stringIndex-1));
    let gsubr=copyIndex f in
    gsubr
  in

  let charStrings=try
    let charStrings=int_of_num (List.hd (findDict f font.dictIndex.(0) font.dictIndex.(1) 17)) in
    let progs=Array.map (fun x->indexGet f (font.offset+charStrings) (x).glyph_index) gls in
    (* let progs=Array.map (Type2.unsubr font.subrIndex.(0) font.gsubrIndex) progs in *)
    let buf=Buffer.create 100 in
    writeIndex buf progs;
    buf
    with
        Not_found->(
          Printf.fprintf stderr "Please report the following to patolist@lists.patoline.org: empty charstrings dict\n";
          flush stderr;
          Buffer.create 0
        )
  in



  let charset=
    let buf=Buffer.create 5 in
    let l=Array.length gls-1 in
    (* Printf.fprintf stderr "charset %d\n" l;flush stderr; *)
    if l<=0xff then (
      Buffer.add_char buf (char_of_int 1);
      Buffer.add_char buf (char_of_int 0);
      Buffer.add_char buf (char_of_int 1);
      Buffer.add_char buf (char_of_int l);
    ) else (
      Buffer.add_char buf (char_of_int 2);
      Buffer.add_char buf (char_of_int 0);
      Buffer.add_char buf (char_of_int 1);
      Buffer.add_char buf (char_of_int (l lsr 8));
      Buffer.add_char buf (char_of_int (l land 0xff));
    );
    buf
  in

  let encoding=
    let size=IntMap.fold (fun k a m->if a<0x100 then m+1 else m) IntMap.empty 0 in
    let enc=String.make (size+2) (char_of_int 0) in
    enc.[0]<-char_of_int 0;
    enc.[1]<-char_of_int size;
    IntMap.iter (fun k a->if a<0x100 then enc.[2+a]<-char_of_int k) cmap;
    enc
  in
#ifdef CID
  let fdselect=
    let buf=Buffer.create 10 in
    let l=Array.length gls in
    (* format *)
    Buffer.add_char buf (char_of_int 3);
    (* nRanges *)
    Buffer.add_char buf (char_of_int 0);
    Buffer.add_char buf (char_of_int 1);
    (* Range 0 *)
    Buffer.add_char buf (char_of_int 0);
    Buffer.add_char buf (char_of_int 0);
    Buffer.add_char buf (char_of_int 0);
    (* Sentinel *)
    Buffer.add_char buf (char_of_int (l lsr 8));
    Buffer.add_char buf (char_of_int (l land 0xff));
    buf
  in
#else
      let fdselect=Buffer.create 10 in
#endif


  (* Privdict est autonome avec ses offsets. Ce truc est prêt à être écrit. *)
  let len_privDict,new_privDict=
    let privDictOff,privDict=
      try
        match IntMap.find 18 topDict with
            off::sz::_->(
              (int_of_num off,
               readDict f (font.offset+int_of_num off) (font.offset+int_of_num off+int_of_num sz))
            )
          | _->(-1,IntMap.empty)
      with
          Not_found->(-1,IntMap.empty)
    in
    (*
    IntMap.iter (fun k a ->
      Printf.fprintf stderr "privDict key : %d\n" k;
      List.iter (fun x->print_num stderr x;Printf.fprintf stderr " ") a;Printf.fprintf stderr "\n";
    ) privDict;flush stderr;
    *)
    let subrs=
      try
        match IntMap.find 19 privDict with
            off::_->readIndex f (font.offset+privDictOff+int_of_num off)
          | _->[||]
      with
          Not_found->[||]
    in
    let privDict=if Array.length subrs<=0 then IntMap.remove 19 privDict else privDict in

    if privDictOff<0 then (0,Buffer.create 0) else (
      let bu=Buffer.create 100 in
      (* Printf.fprintf stderr "privdict\n";flush stderr; *)
      writeDict bu privDict;
      (* Printf.fprintf stderr "/privdict\n";flush stderr; *)
      if Array.length subrs<=0 then (Buffer.length bu,bu) else (
        let privDict=IntMap.add 19 [CFFInt (Buffer.length bu)] privDict in
        Buffer.clear bu;
        writeDict bu privDict;
        let len=Buffer.length bu in
        writeIndex bu subrs;
        (len,bu)
      )
    )
  in


  (*********************)
  (* Ecriture de tout. *)
  (*********************)

  (* Enregistrement des SID dans le strIndex *)
  let topDict=IntMap.mapi (fun k a->
    if (k>=0 && k<=4)
      || k=0xc00
      || (k>=0xc15 && k<=0xc17)
      || k=0xc1e
    then
      let (u,v)=part 2 a in
      (List.map
         (fun h->
           if int_of_num h<=390 then h else
             (try
                let s0=font.stringIndex.(int_of_num h-391) in
                let s1=font.stringIndex.(int_of_num h-391+1) in
                let s=Bytes.create (s1-s0) in
                seek_in f s0;
                really_input f s 0 (s1-s0);
                let sid=getSid s in
                (CFFInt sid)
              with
                  _->(CFFInt 0))
         ) u
      )@v
    else
      a
  ) topDict
  in

  let topDict=IntMap.add 16 [CFFInt 0] topDict in (* Encoding *)
  let topDict=IntMap.add 15 [CFFInt 0] topDict in (* Charset *)
#ifdef CID
  let topDict= (* ROS *)
    let registry=getSid "Adobe" in
    let ordering=getSid "Identity" in
    let supplement=0 in
    IntMap.add 0xc1e [CFFInt supplement;CFFInt ordering;CFFInt registry] topDict
  in
  (*
  let topDict=IntMap.add 0xc1f [CFFInt 0] topDict in (* CIDFontVersion *)
  let topDict=IntMap.add 0xc20 [CFFInt 0] topDict in (* CIDFontRevision *)
  *)
  let topDict=IntMap.add 0xc21 [CFFInt 0] topDict in (* CIDFontType *)
  let topDict=IntMap.add 0xc22 [CFFInt (Array.length gls)] topDict in (* CIDCount *)
  (*
  let topDict=IntMap.add 0xc23 [CFFInt 0] topDict in (* UIDBase *)
  *)
  let topDict=IntMap.add 0xc24 [CFFInt topDictOffset] topDict in (* FDArray *)
  let topDict=IntMap.add 0xc25 [CFFInt 0] topDict in (* FDSelect *)
  (*
  let topDict=IntMap.add 0xc26 [CFFInt 0] topDict in (* FontName *)
  *)
#endif

  (* strings *)
  let strIndex=
    let arr=Array.make (StrMap.cardinal !strings) "" in
    StrMap.iter (fun k a->
      (* Printf.fprintf stderr "%S\n" k; *)
      arr.(a)<-k
    ) !strings;
    let buf=Buffer.create 100 in
    writeIndex buf arr;
    buf
  in
  (* TopDict, strings, gsubr, charset, fdselect, charstrings, Private Dicts *)
  let topDictBuf=
    let topDictBuf=Buffer.create 100 in

    let rec writeTopDict len dict=
      Buffer.clear topDictBuf;
      writeDict topDictBuf topDict;
      let topDictStr=Buffer.contents topDictBuf in
      Buffer.clear topDictBuf;
      writeIndex topDictBuf [|topDictStr|];
      (* encoding *)
      let topDict=IntMap.add 16 [CFFInt(topDictOffset+len
                                        +Buffer.length strIndex
                                        +String.length gsubrBuf)]
        topDict
      in
      (* charset *)
      let topDict=IntMap.add 15 [CFFInt(topDictOffset+len
                                        +Buffer.length strIndex
                                        +String.length gsubrBuf
                                        +String.length encoding)]
        topDict
      in
#ifdef CID
      (* fdarray *)
      let topDict=IntMap.add 0xc24 [CFFInt(topDictOffset(*+len
                                           +Buffer.length strIndex
                                           +String.length gsubrBuf
                                           +Buffer.length charset
                                           +Buffer.length fdselect
                                           +Buffer.length charStrings*))]
        topDict
      in
      (* fdselect *)
      let topDict=IntMap.add 0xc25 [CFFInt(topDictOffset+len
                                           +Buffer.length strIndex
                                           +String.length gsubrBuf
                                           +String.length encoding
                                           +Buffer.length charset)]
        topDict
      in
#endif
      (* charstrings *)
      let topDict=IntMap.add 17 [CFFInt(topDictOffset+len
                                        +Buffer.length strIndex
                                        +String.length gsubrBuf
                                        +String.length encoding
                                        +Buffer.length charset
                                        +Buffer.length fdselect)]
        topDict
      in
      (* private *)
      let topDict=if len_privDict>0 then
          IntMap.add 18 [CFFInt (len_privDict);
                         CFFInt(topDictOffset+len
                                +Buffer.length strIndex
                                +String.length gsubrBuf
                                +String.length encoding
                                +Buffer.length charset
                                +Buffer.length fdselect
                                +Buffer.length charStrings
                                (* +len *)
                         )]
            topDict
        else
          IntMap.remove 18 topDict
      in
      Buffer.clear topDictBuf;
      (* Printf.fprintf stderr "topdict\n";flush stderr; *)
      writeDict topDictBuf topDict;
      (* Printf.fprintf stderr "/topdict\n";flush stderr; *)
      let topDictStr=Buffer.contents topDictBuf in
      Buffer.clear topDictBuf;
      writeIndex topDictBuf [|topDictStr|];

      if Buffer.length topDictBuf <> len then writeTopDict (Buffer.length topDictBuf) topDict else
        topDictBuf
    in
    writeTopDict 0 topDict;
  in

  Buffer.add_buffer outBuf topDictBuf;
  Buffer.add_buffer outBuf strIndex;
  Buffer.add_string outBuf gsubrBuf;
  Buffer.add_string outBuf encoding;
  Buffer.add_buffer outBuf charset;
  Buffer.add_buffer outBuf fdselect;
  Buffer.add_buffer outBuf charStrings;
  (*
  let fdarray=
    let topDict_fd=IntMap.remove 0xc24 topDict in
    let topDict_fd=IntMap.remove 0xc1e topDict_fd in
    let topDict_fd=IntMap.remove 0xc25 topDict_fd in
    let topDict_fd=IntMap.remove 15 topDict_fd in
    let topDict_fd=IntMap.remove 16 topDict_fd in
    let topDict_fd=IntMap.remove 17 topDict_fd in
    let topDict_fd=IntMap.remove 18 topDict_fd in

    let b=Buffer.create 100 in
    writeDict b topDict_fd;
    let s=Buffer.contents b in
    Buffer.clear b;
    writeIndex b [|s|];
    let l0=Buffer.length b in
    for i=l0 to Buffer.length topDictBuf-1 do
      Buffer.add_char b 'x'
    done;
    b
  in
  Buffer.add_buffer outBuf fdarray;
  *)
  Buffer.add_buffer outBuf new_privDict;
  outBuf

*)


let subset(* _encoded *) font info cmap gls=
  let buf=Buffer.create 256 in

  (* Ecriture du header *)
  let headersize=4 in
  Buffer.add_char buf (char_of_int 1);
  Buffer.add_char buf (char_of_int 0);
  Buffer.add_char buf (char_of_int headersize);
  Buffer.add_char buf (char_of_int 4);
  let f=open_in_bin_cached font.file in

  (* Ecriture du nameIndex *)
  let name=
    let buf_=Bytes.create (font.nameIndex.(1)-font.nameIndex.(0)) in
    seek_in f (font.nameIndex.(0));
    really_input f buf_ 0 (String.length buf_);
    buf_
  in
  writeIndex buf [|name|];

  let strings=ref StrMap.empty in
  let getSid s=
    391+(
      try StrMap.find s !strings with
          Not_found->
            (strings:=StrMap.add s (StrMap.cardinal !strings) !strings;
             (StrMap.cardinal !strings-1))
    )
  in


  let topDict=readDict f font.dictIndex.(0) font.dictIndex.(1) in
  (* On commence par écrire tous les offsets pour calculer la taille finale *)
  let topDict=
    let rec make_dict i m=if i>17 then m else make_dict (i+1) (IntMap.add i [CFFInt 0] m) in
    make_dict 15 (IntMap.add 18 [CFFInt 0;CFFInt 0] topDict)
  in
  (* Enregistrement des SID dans le strIndex *)
  let topDict=IntMap.mapi (fun k a->
    if (k>=0 && k<=4)
      || k=0xc00
      || (k>=0xc15 && k<=0xc17)
      || k=0xc1e
    then
      let (u,v)=part 2 a in
      (List.map
         (fun h->
           if int_of_num h<=390 then h else
             (try
                let s0=font.stringIndex.(int_of_num h-391) in
                let s1=font.stringIndex.(int_of_num h-391+1) in
                let s=Bytes.create (s1-s0) in
                seek_in f s0;
                really_input f s 0 (s1-s0);
                let sid=getSid s in
                (CFFInt sid)
              with
                  _->(CFFInt 0))
         ) u
      )@v
    else
      a
  ) topDict
  in


  let topDictBuf_=Buffer.create 200 in
  writeDict topDictBuf_ topDict;     (* Premiere tentative pour connaitre la taille *)
  let topDictBuf=Buffer.create 200 in
  writeIndex topDictBuf [|Buffer.contents topDictBuf_|];

  seek_in f (font.stringIndex.(Array.length font.stringIndex-1));
  let gsubr=copyIndex f in
  let encoding=
    let size=IntMap.fold (fun k a m->
      if a<0x100 then m+1 else m
    ) cmap 0
    in
    let enc=String.make (size+2) (char_of_int 0) in
    Bytes.set enc 0 (char_of_int 0);
    Bytes.set enc 1 (char_of_int size);
    IntMap.iter (fun k a->
      if a<0x100 && a>0 then Bytes.set enc (1+a) (char_of_int k)
    ) cmap;
    enc
  in
  let charset=
    let reverseEncoding=
      IntMap.fold (fun k a m->IntMap.add a k m) cmap IntMap.empty
    in
    try
      (* Les glyphs sont tous renommés avec leur contenu unicode.
         Voir la page
         http://www.adobe.com/devnet/opentype/archives/glyph.html
         pour savoir comment on les nomme. *)
      let str=String.make (2*Array.length gls-1) (char_of_int 0) in
      (*let glbuf=Buffer.create 64 in*)
      let altmap=ref StrMap.empty in
      let names=Array.make (Array.length gls) "" in
      let alternates=Array.make (Array.length gls) 0 in
      for i=1 to Array.length gls-1 do
        (*
        let b=if String.length gls.(i).glyph_utf8 > 0 then
            (* String.sub (gls.(i).glyph_utf8) 0 (UTF8.next gls.(i).glyph_utf8 0) *)
            gls.(i).glyph_utf8
          else
            " "                         (* De toute façon, il y a les .alt%d *)
        in
        let rec make_name i=
          (* Oui, bien sûr, on n'est qu'en 2012, donc il y a des limites de merde sur
             les longueurs des chaînes de caractères :
             http://www.adobe.com/devnet-archive/opentype/archives/glyphnamelimits.html *)
          if UTF8.out_of_range b i || (Buffer.length glbuf+8>31) then Buffer.contents glbuf else
            (let x=UChar.code (UTF8.look b i) in
             if Buffer.length glbuf>0 then Buffer.add_string glbuf "_";
             Buffer.add_string glbuf (Printf.sprintf "uni%04X" x);
             make_name (UTF8.next b i))
        in
        Buffer.clear glbuf;
        let str=make_name 0 in
        *)
        let str=Printf.sprintf "uni%d"
          (try IntMap.find i reverseEncoding with
              Not_found->(
                int_of_char ' '
              ))
        in
        let alt=try StrMap.find str !altmap with _->0 in
        altmap:=StrMap.add str (alt+1) !altmap;

        names.(i)<-if str="" then "space" else str;
        alternates.(i)<-alt
      done;
      for i=1 to Array.length gls-1 do
        let b=
          let alt=try StrMap.find names.(i) !altmap with Not_found->0 in
          if alt<=1 then names.(i) else Printf.sprintf "%s.alt%d" names.(i) alternates.(i)
        in
        let num=
          391+(
            try
              StrMap.find b !strings
            with
                Not_found->(
                  let n=StrMap.cardinal !strings in
                  strings:=StrMap.add b n !strings;
                  n
                )
          )
        in
        Bytes.set str (2*i-1) (char_of_int (num lsr 8));
        Bytes.set str (2*i) (char_of_int (num land 0xff))
      done;
      str
    with
        Not_found -> String.make (2*Array.length gls-1) (char_of_int 0)
  in
  let charStrings=try
    let charStrings=int_of_num (List.hd (findDict f font.dictIndex.(0) font.dictIndex.(1) 17)) in
    let progs=Array.map (fun x->indexGet f (font.offset+charStrings) (x).glyph_index) gls in
    let buf=Buffer.create 100 in
    writeIndex buf progs;
    buf
    with
        Not_found->(
          Printf.fprintf stderr "Please report the following to mltypography@googlegroups.com: empty charstrings dict\n";
          flush stderr;
          Buffer.create 0
        )
  in
  let priv=try
    match findDict f (font.dictIndex.(0)) (font.dictIndex.(1)) 18 with
        offset::size::_->(
          let m=readDict f (font.offset+int_of_num offset)
            (font.offset+int_of_num offset+int_of_num size) in
          m
        )
      | _-> IntMap.empty
    with
        Not_found | CFFNum _-> IntMap.empty
  in
  let subr=try (
    match findDict f (font.dictIndex.(0)) (font.dictIndex.(1)) 18 with
        offset::size::_->(
          let off_subr=List.hd (IntMap.find 19 priv) in
          seek_in f (font.offset+int_of_num offset+int_of_num off_subr);
          copyIndex f
        )
      | _->let bubu=Buffer.create 2 in writeIndex bubu [||];Buffer.contents bubu
  ) with
      Not_found
    | CFFNum _-> ""
  in


  let strIndex=Buffer.create 256 in
  let strArray=Array.make (StrMap.cardinal !strings) "" in
  StrMap.fold (fun k a ()->strArray.(a)<-k) !strings ();
  writeIndex strIndex strArray;

  (* On commence par faire le dictionnaire privé, dont la seule
     information vraiment fondamentale est l'offset vers le tableau de
     subrs, qu'on écrit juste après lui. *)
  let privDict=Buffer.create 16 in
  let priv0=                            (* Premier essai pour voir la taille *)
    (if IntMap.mem 19 priv then
        IntMap.add 19 [CFFInt 0] priv
     else priv)
  in
  writeDict privDict priv0;

  let privLength=Buffer.length privDict in
  Buffer.reset privDict;               (* On l'écrit pour de vrai, maintenant *)
  writeDict privDict
    (if IntMap.mem 19 priv then
        IntMap.add 19 [CFFInt privLength] priv
     else priv);

  (* Premier top-dictionnaire, avec l'offset des charstrings *)
  let topDict=
    (IntMap.add 17
       [CFFInt (Buffer.length buf + (Buffer.length topDictBuf) +
                  Buffer.length strIndex + String.length gsubr +
                  String.length encoding + String.length charset)]
       topDict)
  in
  (* Avec l'adresse et la taille du dictionnaire privé *)
  let topDict=
    IntMap.add 18
      [CFFInt (Buffer.length privDict);
       CFFInt (Buffer.length buf + (Buffer.length topDictBuf) +
                 Buffer.length strIndex + String.length gsubr +
                 String.length encoding + String.length charset +
                 Buffer.length charStrings)]
      topDict
  in
  (* Avec l'encodage *)
  let topDict=
    IntMap.add 16
      [CFFInt (Buffer.length buf + (Buffer.length topDictBuf) +
                 Buffer.length strIndex + String.length gsubr)]
      topDict
  in
  (* Avec le charset *)
  let topDict=
    IntMap.add 15
      [CFFInt (Buffer.length buf + (Buffer.length topDictBuf) +
                 Buffer.length strIndex + String.length gsubr +
                 String.length encoding)]
      topDict
  in

  (* Ecriture finale dans le buffer *)
  Buffer.reset topDictBuf_;
  writeDict topDictBuf_ topDict;
  writeIndex buf [|Buffer.contents topDictBuf_|];

  Buffer.add_buffer buf strIndex;
  Buffer.add_string buf gsubr;
  Buffer.add_string buf encoding;
  Buffer.add_string buf charset;
  Buffer.add_buffer buf charStrings;
  Buffer.add_buffer buf privDict;
  Buffer.add_string buf subr;
  buf



let add_kerning _ _ = ()
