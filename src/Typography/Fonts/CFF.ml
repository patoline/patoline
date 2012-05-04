open CamomileLibrary
open Util
open Bezier
open FTypes
let pt_of_mm x=(72.*.x)/.25.4
let mm_of_pt x=(25.4*.x)/.72.

type font= { file:in_channel; offset:int; size:int; offSize:int; nameIndex:int array;
             dictIndex:int array; stringIndex:int array; subrIndex:(string array) array;
             gsubrIndex:string array
           }

type glyph= { glyphFont:font; glyphNumber:glyph_id; type2:string; matrix:float array; subrs:string array; gsubrs:string array;
              glyphContents:UTF8.t;
              mutable glyphWidth:float;
              mutable glyphX0:float; mutable glyphX1:float;
              mutable glyphY0:float; mutable glyphY1:float
            }

let glyphFont gl=gl.glyphFont

#define HSTEM 1
#define VSTEM 3
#define VMOVETO 4
#define RLINETO 5
#define HLINETO 6
#define VLINETO 7
#define RRCURVETO 8
#define CALLSUBR 10
#define RETURN 11
#define ESCAPE 12
#define ENDCHAR 14
#define HSTEMHM 18
#define HINTMASK 19
#define CNTRMASK 20
#define RMOVETO 21
#define HMOVETO 22
#define VSTEMHM 23
#define RCURVELINE 24
#define RLINECURVE 25
#define VVCURVETO 26
#define HHCURVETO 27
#define SHORTINT 28
#define CALLGSUBR 29
#define VHCURVETO 30
#define HVCURVETO 31
(* Escaped *)
#define AND 3
#define OR 4
#define NOT 5

#define ABS 9
#define ADD 10
#define SUB 11
#define DIV 12
#define NEG 14

#define EQ 15
#define DROP 18
#define PUT 20
#define GET 21
#define IFELSE 22
#define RANDOM 23
#define MUL 24
#define SQRT 26
#define DUP 27
#define EXCH 28
#define INDEX 29
#define ROLL 30

#define HFLEX 34
#define FLEX 35
#define HFLEX1 36
#define FLEX1 37




let showStack st stc=
  print_string "[";
  if stc>0 then
    (for i=0 to stc-2 do
       print_float st.(i);print_string "; "
     done;
     print_float st.(stc-1));
  print_string "]\n"



exception Index
exception Type2Int of int

type cff_num=CFFFloat of float | CFFInt of int
exception CFFNum of cff_num
let int_of_num=function
    CFFInt x->x
  | x ->raise (CFFNum x)
let float_of_num=function
    CFFFloat x->x
  | x ->raise (CFFNum x)

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
              (* Printf.printf "cas 1 : %d\n" (b0-139); *)
              b0-139
            ) else
              let b1=input_byte f in
                if b0<=250 then (
                  let x=(((b0-247) lsl 8) lor b1) + 108 in
                    (* Printf.printf "cas 2 : %d\n" x; *)
                    x
                ) else (
                  let x= - ((((b0-251) lsl 8) lor b1) + 108) in
                    (* Printf.printf "cas 3 : %d\n" x; *)
                    x
                )
          else
            (let b1=input_byte f in
             let b2=input_byte f in
               if b0=28 then (
                 let x=((b1 lsl 8) lor b2) in
                   (* Printf.printf "cas 4 : %d\n" x; *)
                   if x >= 32768 then x - 65536 else x
               ) else (
                 let b3=input_byte f in
                 let b4=input_byte f in
                 let x=(((((b1 lsl 8) lor b2) lsl 8) lor b3) lsl 8) lor b4 in
                   (* Printf.printf "cas 5 : %d\n" x; *)
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
  (* Printf.printf "strIndex : %d\n" idx_off;flush stdout; *)
  let off=index f idx_off in
  let str=Array.create (Array.length off-1) "" in
    for i=0 to Array.length off-2 do
      seek_in f (off.(i));
      str.(i)<-String.create (off.(i+1)-off.(i));
      really_input f (str.(i)) 0 (off.(i+1)-off.(i))
    done;
    str

let indexGet f idx_off idx=
  seek_in f idx_off;
  let count=readInt f 2 in
    if idx>=count || idx<0 then raise Not_found else
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
              Printf.printf "readDict : op : %d\n" op;
              dict' [] ((op,stack)::l)
  in
    seek_in f a;
    dict' [] []

let findDict f a b key=
  (* Printf.printf "findDict : %d %d %d\n" a b (in_channel_length f);flush stdout; *)
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
              (* Printf.printf "op : %d\n" op;flush stdout; *)
              if op=key then stack else dict' []
  in
    seek_in f a;
    dict' []

let loadFont ?offset:(off=0) ?size:(size=0) file=
  let f=open_in_bin file in
    seek_in f (off+2);
    (* Printf.printf "=========================\n";flush stdout; *)
    let hdrSize=input_byte f in
    let offSize=input_byte f in
    let nameIndex=index f (off+hdrSize) in
    let dictIndex=index f (nameIndex.(Array.length nameIndex-1)) in
    let stringIndex=index f (dictIndex.(Array.length dictIndex-1)) in
    let subrIndex=
      let subrs=Array.create (Array.length dictIndex-1) [||] in
        for idx=0 to Array.length dictIndex-2 do
          try
            (* Printf.printf "====privOffset\n";flush stdout; *)
            let privOffset=findDict f (dictIndex.(idx)) (dictIndex.(idx+1)) 18 in
              match privOffset with
                  offset::size::_->(
                    try
                      let _=readDict f (off+int_of_num offset)
                        (off+int_of_num offset+int_of_num size)
                      in

                      let subrsOffset=int_of_num (List.hd (findDict f (off+int_of_num offset) (off+int_of_num offset+int_of_num size) 19)) in
                        Printf.printf "====subrsOffset %d %d %d\n" (int_of_num offset) (int_of_num size) subrsOffset;flush stdout;
                        subrs.(idx) <- strIndex f (off+int_of_num offset+subrsOffset);
                        Printf.printf "subrs : %d\n" (Array.length subrs.(idx));
                    with
                        Not_found -> ()
                  )
                | _->()
          with
              Not_found -> ()
        done;
        subrs
    in
      (* Printf.printf "stringIndex=%d\n" (Array.length stringIndex); *)
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
      (* Printf.printf "read : gsubr=%d\n" (Array.length gsubrIndex); *)
      { file=f; offset=off; size=size; offSize=offSize; nameIndex=nameIndex; dictIndex=dictIndex;
        stringIndex=stringIndex; gsubrIndex=gsubrIndex; subrIndex=subrIndex }

let glyph_of_uchar _ _=0
let glyph_of_char f c=glyph_of_uchar f (CamomileLibrary.UChar.of_char c)

let loadGlyph font ?index:(idx=0) gl=
  let charStrings=int_of_num (List.hd (findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 17)) in
  let fontMatrix=
    try
      List.map (function
                    CFFFloat x->x
                  | x->raise (CFFNum x))
        (findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 3079)
    with
        Not_found | CFFNum _->[0.001;0.;0.;0.001]
  in
    { glyphFont=font;
      glyphNumber=gl;
      type2=indexGet font.file (font.offset+charStrings) gl.glyph_index;
      matrix=Array.of_list fontMatrix;
      subrs=font.subrIndex.(idx);
      gsubrs=font.gsubrIndex;
      glyphContents=gl.glyph_utf8;
      glyphWidth=infinity;
      glyphX0=infinity; glyphX1= -.infinity;
      glyphY0=infinity; glyphY1= -.infinity }

let cardinal font=
  let idx=0 in
  let charStrings=int_of_num (List.hd (findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 17)) in
    seek_in font.file (font.offset+charStrings);
    readInt2 font.file


exception Found of float
let outlines_ gl onlyWidth=
  Random.init (int_of_char gl.type2.[0]);
  let stack=Array.create 48 0. in
  let stackC=ref 0 in
  let pop ()=
    if !stackC<=0 then failwith "CFF.outlines : empty stack" else
      (decr stackC;
       stack.(!stackC)) in
  let heap=Array.create 33 0. in
  let hints=ref 0 in
  let opened=ref false in
  let x=ref 0. in
  let y=ref 0. in
  let x0=ref 0. in
  let y0=ref 0. in
  let resultat=ref [] in
  let lineto x1 y1=
    opened:=true;
    let h,s = match !resultat with []->[],[] | h::s-> h,s in
      resultat:= (([| !x; x1 |],[| !y; y1 |])::h)::s;
      x:=x1;
      y:=y1
  in
  let curveto x1 y1 x2 y2 x3 y3=
    opened:=true;
    let h,s = match !resultat with []->[],[] | h::s-> h,s in

      resultat:=(([| !x;x1;x2;x3 |],
                  [| !y;y1;y2;y3 |])::h)::s;
      x:=x3;
      y:=y3
  in
  let moveto x1 y1=
    if !opened && (!x <> !x0 || !y <> !y0) then lineto !x0 !y0;
    x:=x1; y:=y1;
    x0:=x1; y0:=y1;
    match !resultat with (_::_)::_ ->resultat:=([]::(!resultat)) | _-> ();
      opened:=false;
  in
  let rec hlineto c=
    if c <= !stackC-1 then
      (lineto (!x+.stack.(c)) !y;
       vlineto (c+1))
    else
      stackC:=0
  and vlineto c=
    if c <= !stackC-1 then
      (lineto !x (!y+.stack.(c));
       hlineto (c+1))
    else
      stackC:=0
  in
  let rec hvcurveto c=
    if c <= !stackC-4 then
      (curveto
         (!x +. stack.(c)) !y
         (!x +. stack.(c) +. stack.(c+1)) (!y +. stack.(c+2))
         (!x +. stack.(c) +. stack.(c+1) +. (if !stackC-c = 5 then stack.(c+4) else 0.))
         (!y +. stack.(c+2) +. stack.(c+3));
       vhcurveto (c+4))
    else
      stackC:=0
  and vhcurveto c=
    if c <= !stackC-4 then
      (curveto
         !x (!y +. stack.(c))
         (!x +. stack.(c+1)) (!y +. stack.(c) +. stack.(c+2))
         (!x +. stack.(c+1) +. stack.(c+3))
         (!y +. stack.(c) +. stack.(c+2) +. (if !stackC-c = 5 then stack.(c+4) else 0.));
       hvcurveto (c+4))
    else
      stackC:=0
  in
  let rec execute program=
    let pc=ref 0 in
      while !pc <  (String.length program) do
        (* showStack stack !stackC; *)
        (* Printf.printf "%d > %d\n" !pc (int_of_char (program.[!pc])); *)
        (* flush stdout; *)
        match int_of_char (program.[!pc]) with
            RMOVETO->
              (moveto (!x +. stack.(!stackC-2)) (!y +. stack.(!stackC-1));
               if onlyWidth && !stackC>2 then raise (Found stack.(0));
               stackC:=0;
               incr pc)
          | HMOVETO->
              (moveto (!x +. stack.(!stackC-1)) !y;
               if onlyWidth && !stackC>1 then raise (Found stack.(0));
               stackC:=0;
               incr pc)
          | VMOVETO->
              (moveto !x (!y +. stack.(!stackC-1));
               if onlyWidth && !stackC>1 then raise (Found stack.(0));
               stackC:=0;
               incr pc)
          | RLINETO->
              (let c=ref 0 in
                 while !c <= !stackC-2 do
                   lineto (!x +. stack.(!c)) (!y +. stack.(!c+1));
                   c:= !c+2
                 done;
                 stackC:=0;
                 incr pc)
          | HLINETO->(hlineto 0; incr pc)
          | VLINETO->(vlineto 0; incr pc)
          | RRCURVETO->
              (let c=ref 0 in
                 while !c <= !stackC-6 do
                   let x1=(!x +. stack.(!c)) in
                   let y1=(!y +. stack.(!c+1)) in
                   let x2=x1 +. stack.(!c+2) in
                   let y2=y1 +. stack.(!c+3) in
                   let x3=x2 +. stack.(!c+4) in
                   let y3=y2 +. stack.(!c+5) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+6
                 done;
                 stackC:=0;
                 incr pc)
          | VHCURVETO->(vhcurveto 0; incr pc)
          | HVCURVETO->(hvcurveto 0; incr pc)
          | HHCURVETO->
              (let c=ref 0 in
               let dy1=if (!stackC) land 1 = 1 then (incr c; stack.(0)) else 0. in
                 while !c <= !stackC-4 do
                   let x1= !x +. stack.(!c) in
                   let y1= !y +. (if !c=1 then dy1 else 0.) in
                   let x2=x1 +. stack.(!c+1) in
                   let y2=y1 +. stack.(!c+2) in
                   let x3=x2 +. stack.(!c+3) in
                   let y3=y2 in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+4
                 done;
                 stackC:=0;
                 incr pc)
          | VVCURVETO->
              (let c=ref 0 in
               let dx1=if (!stackC - !c) land 1 = 1 then (incr c; stack.(0)) else 0. in
                 while !c <= !stackC-4 do
                   let x1=(!x +. (if !c=1 then dx1 else 0.)) in
                   let y1=(!y +. stack.(!c)) in
                   let x2=x1 +. stack.(!c+1) in
                   let y2=y1 +. stack.(!c+2) in
                   let x3=x2 in
                   let y3=y2 +. stack.(!c+3) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+4
                 done;
                 stackC:=0;
                 incr pc)
          | RCURVELINE->
              (let c=ref 0 in
                 while !c <= !stackC-8 do
                   let x1=(!x +. stack.(!c)) in
                   let y1=(!y +. stack.(!c+1)) in
                   let x2=x1 +. stack.(!c+2) in
                   let y2=y1 +. stack.(!c+3) in
                   let x3=x2 +. stack.(!c+4) in
                   let y3=y2 +. stack.(!c+5) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+6
                 done;
                 lineto (!x +. stack.(!c)) (!y +. stack.(!c+1));
                 stackC:=0;
                 incr pc)
          | RLINECURVE->
              (let c=ref 0 in
                 while !c <= !stackC-8 do
                   lineto (!x+.stack.(!c)) (!y+.stack.(!c+1));
                   c:= !c+2
                 done;
                 let x1=(!x +. stack.(!c)) in
                 let y1=(!y +. stack.(!c+1)) in
                 let x2=x1 +. stack.(!c+2) in
                 let y2=y1 +. stack.(!c+3) in
                 let x3=x2 +. stack.(!c+4) in
                 let y3=y2 +. stack.(!c+5) in
                   curveto x1 y1 x2 y2 x3 y3;
                   stackC:=0;
                   incr pc)
          | HSTEM | VSTEM | HSTEMHM
          | VSTEMHM->(
              if onlyWidth && (!stackC land 1)=1 then raise (Found stack.(0));
              hints := !hints + !stackC / 2 ;
              stackC:=0 ; incr pc
            )
          | HINTMASK | CNTRMASK ->(
              hints:= !hints+ !stackC/2;
              if onlyWidth && !stackC > 0 then raise (Found stack.(0));
              stackC:=0 ; pc := !pc + 1 + (if !hints land 7=0 then (!hints/8) else (!hints/8+1))
            )
          | ENDCHAR->
              (if !opened && (!x <> !x0 || !y <> !y0) then lineto !x0 !y0;
               if onlyWidth && !stackC>0 then raise (Found stack.(0));
               match !resultat with []::s -> resultat:=s | _->();
               pc:=String.length program)
          | RETURN->pc:=String.length program
          | CALLSUBR->
              (let subrBias=
                 if Array.length gl.subrs < 1240 then 107 else
                   if Array.length gl.subrs < 33900 then 1131 else 32768
               in
               let subr=((subrBias + (int_of_float (pop ())))) in
                 execute gl.subrs.(subr);
                 incr pc)
          | SHORTINT->
              (let a=int_of_char (program.[!pc+1]) in
               let b=int_of_char (program.[!pc+2]) in
                 stack.(!stackC) <- float_of_int ((((a land 0x7f) lsl 8) lor b) - (if a>=128 then 1 lsl 15 else 0));
                 incr stackC;
                 pc:= !pc+3)
          | CALLGSUBR->
              (let gsubrBias=
                 if Array.length gl.gsubrs < 1240 then 107 else
                   if Array.length gl.gsubrs < 33900 then 1131 else 32768
               in
                 execute (gl.gsubrs.(gsubrBias + (int_of_float (pop ()))));
                 incr pc)
          | ESCAPE->
              (match int_of_char (program.[!pc+1]) with
                    FLEX->
                      (if !stackC>=12 then
                         (let x0= !x +. stack.(0) in
                          let y0= !y +. stack.(1) in
                          let x1= x0 +. stack.(2) in
                          let y1= y0 +. stack.(3) in
                          let x2= x1 +. stack.(4) in
                          let y2= y1 +. stack.(5) in
                          let x3=x2 +. stack.(6) in
                          let y3=y2 +. stack.(7) in
                          let x4=x3 +. stack.(8) in
                          let y4=y3 +. stack.(9) in
                          let x5=x4 +. stack.(10) in
                          let y5=y4 +. stack.(11) in
                            curveto x0 y0 x1 y1 x2 y2;
                            curveto x3 y3 x4 y4 x5 y5);
                       stackC:=0;
                       pc:= !pc+2)
                  | FLEX1->
                      (if !stackC>9 then
                         (let x0= !x +. stack.(0) in
                          let y0= !y +. stack.(1) in
                          let x1= x0 +. stack.(2) in
                          let y1= y0 +. stack.(3) in
                          let x2= x1 +. stack.(4) in
                          let y2= y1 +. stack.(5) in
                          let x3=x2 +. stack.(6) in
                          let y3=y2 +. stack.(7) in
                          let x4=x3 +. stack.(8) in
                          let y4=y3 +. stack.(9) in
                          let (x5,y5)=if abs_float (x4 -. !x) > abs_float (y4 -. !y) then
                            (x4+.stack.(10), y4) else
                            (x4, y4+.stack.(10))
                          in
                            curveto x0 y0 x1 y1 x2 y2;
                            curveto x3 y3 x4 y4 x5 y5);
                       stackC:=0;
                       pc:= !pc+2)
                  | HFLEX->
                      (if !stackC>6 then
                         (let x0= !x +. stack.(0) in
                          let x1= x0 +. stack.(1) in
                          let y1= !y +. stack.(2) in
                          let x2= x1 +. stack.(3) in
                          let x3= x2 +. stack.(4) in
                          let x4= x3 +. stack.(5) in
                          let x5= x4 +. stack.(6) in
                            curveto x0 !y x1 y1 x2 y1;
                            curveto x3 y1 x4 !y x5 !y);
                       stackC:=0;
                       pc:= !pc+2)
                  | HFLEX1->
                      (if !stackC>8 then
                         (let x0= !x +. stack.(0) in
                          let y0= !y +. stack.(1) in
                          let x1= x0 +. stack.(2) in
                          let y1= y0 +. stack.(3) in
                          let x2= x1 +. stack.(4) in
                          let x3= x2 +. stack.(5) in
                          let x4= x3 +. stack.(6) in
                          let y4= y1 +. stack.(7) in
                          let x5= x4 +. stack.(8) in
                            curveto x0 y0 x1 y1 x2 y1;
                            curveto x3 y1 x4 y4 x5 !y);
                       stackC:=0;
                       pc:= !pc+2)
                  | ABS->(if !stackC>0 then stack.(!stackC-1) <- abs_float (stack.(!stackC-1)); pc:= !pc+2)
                  | ADD->(if !stackC>1 then stack.(!stackC-2) <- stack.(!stackC-2) +. stack.(!stackC-1); decr stackC; pc:= !pc+2)
                  | SUB->(if !stackC>1 then stack.(!stackC-2) <- stack.(!stackC-2) -. stack.(!stackC-1); decr stackC; pc:= !pc+2)
                  | DIV->(if !stackC>1 then stack.(!stackC-2) <- stack.(!stackC-2) /. stack.(!stackC-1); decr stackC; pc:= !pc+2)
                  | NEG->(if !stackC>0 then stack.(!stackC-1) <- -. (stack.(!stackC-1)); pc:= !pc+2)
                  | RANDOM->(stack.(!stackC) <- Random.float (float_of_int 0xffff); incr stackC; pc:= !pc+2)
                  | MUL->(if !stackC>1 then stack.(!stackC-2) <- stack.(!stackC-2) *. stack.(!stackC-1); decr stackC; pc:= !pc+2)
                  | SQRT->(if !stackC>0 then stack.(!stackC-1) <- sqrt (stack.(!stackC-1)); pc:= !pc+2)
                  | DROP->(stackC := !stackC - (int_of_float (pop ())); pc:= !pc+2)
                  | EXCH->
                      (let tmp=stack.(!stackC-1) in
                         stack.(!stackC-1)<-stack.(!stackC-2);
                         stack.(!stackC-2)<-tmp;
                         pc:= !pc+2)
                  | INDEX->
                      (stack.(!stackC-1) <- stack.(!stackC - 1 - (int_of_float (stack.(!stackC-1))));
                       pc:= !pc+2)
                  | ROLL->
                      (let num=int_of_float (stack.(!stackC-2)) in
                       let j=
                         let j=int_of_float (stack.(!stackC-1)) in
                           if j<0 then (j mod num) + num else j
                       in
                       let stack'=Array.copy stack in
                         for i= !stackC-2-num to !stackC-2 do
                           stack.(i) <- stack'.(!stackC-2-num  +  ((i+j) mod num))
                         done;
                         stackC:= !stackC-2;
                         pc:= !pc+2
                      )
                  | DUP->
                      (stack.(!stackC)<-stack.(!stackC-1);
                       incr stackC;
                       pc:= !pc+2)
                  | PUT->
                      (heap.(int_of_float (stack.(!stackC-1))) <- stack.(!stackC-2);
                       stackC:= !stackC-2;
                       pc:= !pc+2)
                  | GET->
                      (stack.(!stackC-1) <- heap.(int_of_float (stack.(!stackC-1)));
                       pc:= !pc+2)
                  | AND->
                      (stack.(!stackC-2) <- if stack.(!stackC-1) <> 0. && stack.(!stackC-2) <> 0. then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | OR->
                      (stack.(!stackC-2) <- if stack.(!stackC-1) <> 0. || stack.(!stackC-2) <> 0. then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | NOT->
                      (stack.(!stackC-1) <- if stack.(!stackC-1) = 0. then 1. else 0.;
                       pc:= !pc+2)
                  | EQ->
                      (stack.(!stackC-2) <- if stack.(!stackC-1) = stack.(!stackC-2) then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | IFELSE->
                      (if stack.(!stackC-2) <= stack.(!stackC-1) then stack.(!stackC-4) <- stack.(!stackC-3);
                       stackC:= !stackC-3;
                       pc:= !pc+2)
                  | 0 -> incr pc        (* dotsection (deprecated operator) *)
                  | op->(
                      Printf.printf "Type 2 : undefined operator %d\n" op;
                      pc:= !pc+1
                    )
                )
          | op when op>=32 ->
              if op<=246 then (stack.(!stackC)<-float_of_int (op-139); incr stackC; incr pc) else
                (let op1=int_of_char (program.[!pc+1]) in
                   if op<=250 then
                     (stack.(!stackC)<-float_of_int (((op-247) lsl 8) + op1 + 108);
                      incr stackC;
                      pc:= !pc+2)
                   else
                     if op<=254 then
                       (stack.(!stackC)<- -. (float_of_int (((op-251) lsl 8) + op1 + 108));
                        incr stackC;
                        pc:= !pc+2)
                     else
                       (let op2=int_of_char (program.[!pc+2]) in
                        let op3=int_of_char (program.[!pc+3]) in
                        let op4=int_of_char (program.[!pc+4]) in
                          stack.(!stackC) <-
                            ((float_of_int (op1 land 0x7f))*.16777216. +.
                               (float_of_int op2)*.65536. +.
                               (float_of_int op3)*.256. +.
                               (float_of_int op4) -. (if op1 land 0x80 <> 0 then 2147483648. else 0.)) /. (65536.);
                          incr stackC;
                          pc:= !pc+5
                       )
                )
          | op->(
              Printf.printf "Type 2 : undefined operator %d\n" op;
              pc:= !pc+1
            )
      done
  in
    execute gl.type2;
    List.rev !resultat

let outlines glyph=outlines_ glyph false
let glyphWidth glyph=
  if glyph.glyphWidth=infinity then
    glyph.glyphWidth<-(
      try let _=outlines_ glyph true in raise (Found 0.) with
          Found x->
            (try
               let f=glyph.glyphFont.file in
               let off=glyph.glyphFont.offset in
               let privOffset=findDict f (glyph.glyphFont.dictIndex.(0)) (glyph.glyphFont.dictIndex.(1)) 18 in
                 match privOffset with
                     offset::size::_->float_of_num (
                       List.hd (findDict f (off+int_of_num offset) (off+int_of_num offset+int_of_num size) 21)
                     )
                   | _->0.
             with
                 _->0.)+.x
    );
  glyph.glyphWidth


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


let glyphNumber glyph=glyph.glyphNumber

let fontName ?index:(idx=0) font=
  let buf = String.create (font.nameIndex.(idx+1)-font.nameIndex.(idx)) in
    seek_in font.file (font.nameIndex.(idx));
    really_input font.file buf 0 (font.nameIndex.(idx+1)-font.nameIndex.(idx));
    buf

let fontBBox ?index:(idx=0) font=
  try
    match findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 5 with
        (a::b::c::d::_)->(int_of_num d,int_of_num c,int_of_num b,int_of_num a)
      | _->(0,0,0,0)
  with
      Not_found->(0,0,0,0)
let italicAngle ?index:(idx=0) font=
  try
    match findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 0x0c02 with
        h::_->float_of_num h
      | _->0.
  with
      Not_found->0.


let font_features _ =[]
let select_features _ _=[]
let positioning _ x=x

(* type font= { file:in_channel; offset:int; size:int; offSize:int; nameIndex:int array; *)
(*              dictIndex:int array; stringIndex:int array; subrIndex:(string array) array; *)
(*              gsubrIndex:string array *)
(*            } *)

(* Ça ne marche pas du tout, les offsets sont foireux, il faut récrire
   toute la police pour que ça marche *)

let writeIndex buf data=
  let dataSize=Array.fold_left (fun s str->s+String.length str) 0 data in
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
  if x>=(-107) && x<107 then (
    let y=(char_of_int (x+139)) in
      (* Printf.printf ">cas 1 : %d\n" x; *)
      Buffer.add_char buf y
  ) else if x>=108 && x<=1131 then (
    let b1=(x-108) in
    let b0=(b1/256)+247 in
      (* Printf.printf ">cas 2 : %d\n" x; *)
      Buffer.add_char buf (char_of_int (b0 land 0xff));
      Buffer.add_char buf (char_of_int (b1 land 0xff))
  ) else if x>=(-1131) && x<=(-108) then (
      (* Printf.printf ">cas 3 : %d\n" x; *)
    let b1=(-x-108) in
    let b0=(b1/256)+251 in
      Buffer.add_char buf (char_of_int (b0 land 0xff));
      Buffer.add_char buf (char_of_int (b1 land 0xff))
  ) else if x>=(-32768) && x<=32767 then (
      (* Printf.printf ">cas 4 : %d\n" x; *)
    Buffer.add_char buf (char_of_int 28);
    let y=if x>=0 then x else x+65536 in
      Buffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
      Buffer.add_char buf (char_of_int (y land 0xff));
  ) else if x>=(-(1 lsl 31)) && x<=(1 lsl 31)-1 then (
      (* Printf.printf ">cas 5 : %d\n" x; *)
    Buffer.add_char buf (char_of_int 29);
    let y=if x>=0 then x else x+(1 lsl 32) in
      Buffer.add_char buf (char_of_int ((y lsr 24) land 0xff));
      Buffer.add_char buf (char_of_int ((y lsr 16) land 0xff));
      Buffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
      Buffer.add_char buf (char_of_int (y land 0xff))
  )

(* let dict f a b= *)
(*   let rec dict' stack l= *)
(*     if pos_in f >= b then l else *)
(*       try *)
(*         let op=readCFFInt f in *)
(*           dict' (op::stack) l *)
(*       with *)
(*           (Type2Int b0)-> *)
(*             let op= *)
(*               if b0=12 then *)
(*                 let next=input_byte f in *)
(*                   (12 lsl 8) lor next *)
(*               else *)
(*                 b0 *)
(*             in *)
(*               dict' [] ((op,stack)::l) *)
(*   in *)
(*     seek_in f a; *)
(*     dict' [] [] *)


let rec writeDict buf=function
    []->()
  | (op,stack)::s when op=17 || op=18->(
      let l=List.rev stack in
        List.iter (fun x->
                     Buffer.add_char buf (char_of_int 29);
                     let y=int_of_num x in
                       Buffer.add_char buf (char_of_int ((y lsr 24) land 0xff));
                       Buffer.add_char buf (char_of_int ((y lsr 16) land 0xff));
                       Buffer.add_char buf (char_of_int ((y lsr 8) land 0xff));
                       Buffer.add_char buf (char_of_int (y land 0xff)))
          stack;
      Buffer.add_char buf (char_of_int (op land 0xff));
      writeDict buf s
    )
  | (op,stack)::s->(
      try
        List.iter (fun x->writeCFFInt buf (int_of_num x)) (List.rev stack);
        if op>0xff then Buffer.add_char buf (char_of_int (op lsr 8));
        Buffer.add_char buf (char_of_int (op land 0xff));
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

(* type font= { file:in_channel; offset:int; size:int; offSize:int; nameIndex:int array; *)
(*              dictIndex:int array; stringIndex:int array; subrIndex:(string array) array; *)
(*              gsubrIndex:string array *)
(*            } *)

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

let subset font gls=
  let buf=Buffer.create 100 in
  let f=font.file in
    seek_in f (font.offset+2);
    let headersize=input_byte f in
      seek_in f font.offset;
      Buffer.add_channel buf f headersize;
  let name=
    let buf_=String.create (font.nameIndex.(1)-(font.offset+headersize)) in
      seek_in f (font.offset+headersize);
      really_input f buf_ 0 (String.length buf_);
      buf_
  in
    Buffer.add_string buf name;


    let topDict=
      List.remove_assoc 15 (
        List.remove_assoc 16 (
          readDict font.file font.dictIndex.(0) font.dictIndex.(1)
        ))
    in

    let topDictBuf_=Buffer.create 16 in
      writeDict topDictBuf_ topDict;     (* Premiere tentative pour connaitre la taille *)
      let topDictBuf=Buffer.create 16 in
        writeIndex topDictBuf [|Buffer.contents topDictBuf_|];


      seek_in font.file (font.dictIndex.(Array.length font.dictIndex-1));
      let strIndex=copyIndex f in
        seek_in font.file (font.stringIndex.(Array.length font.stringIndex-1));
      let gsubr=copyIndex f in
      let charStrings=
        let charStrings=int_of_num (List.hd (findDict font.file font.dictIndex.(0) font.dictIndex.(1) 17)) in
        let progs=Array.map (fun x->indexGet font.file (font.offset+charStrings) x) gls in
        let buf=Buffer.create 100 in
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
                    | _->let bubu=Buffer.create 2 in writeIndex bubu [||];Buffer.contents bubu
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

      let privDict=Buffer.create 16 in
      let priv0=
        if List.mem_assoc 19 priv then
          (19, [CFFInt 0])::
            List.remove_assoc 19 priv
        else priv
      in
        writeDict privDict priv0;
        let privLength=Buffer.length privDict in
          Buffer.reset privDict;

          writeDict privDict
            (if List.mem_assoc 19 priv then
               (19, [CFFInt privLength])::
                 List.remove_assoc 19 priv
             else priv);

      let topDict0=
        if List.mem_assoc 17 topDict then
          (17, [CFFInt (Buffer.length buf + Buffer.length topDictBuf +
                          String.length strIndex + String.length gsubr)])::
            List.remove_assoc 17 topDict
        else topDict
      in
      let topDict1=
        if List.mem_assoc 18 topDict0 then
          (18, [CFFInt (Buffer.length privDict);
                CFFInt (Buffer.length buf + Buffer.length topDictBuf + String.length strIndex +
                          String.length gsubr + Buffer.length charStrings)
                ])::
            List.remove_assoc 18 topDict0
        else topDict0
      in
        Buffer.reset topDictBuf_;
        writeDict topDictBuf_ topDict1;     (* Premiere tentative pour connaitre la taille *)
        Buffer.reset topDictBuf;
        writeIndex topDictBuf [|Buffer.contents topDictBuf_|];

        Buffer.add_buffer buf topDictBuf;
        Buffer.add_string buf strIndex;
        Buffer.add_string buf gsubr;
        Buffer.add_buffer buf charStrings;
        Buffer.add_buffer buf privDict;
        Buffer.add_string buf lsubr;
        Buffer.contents buf
