open Binary
open Bezier

type font= { file:in_channel; offset:int; size:int; offSize:int; nameIndex:int array;
             dictIndex:int array; stringIndex:int array; subrIndex:(string array) array;
             gsubrIndex:string array
           }

type glyph= { glyphFont:font; glyphNumber:int; type2:string; matrix:float array; subrs:string array; gsubrs:string array }

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
          (a0+.a1) *. (10. ** a2)
            
      else
        float_of_int (
          if b0>=32 then
            if b0<=246 then b0-139 else 
              let b1=input_byte f in
                if b0<=250 then
                  (((b0-247) lsl 8) lor b1) + 108
                else
                  - ((((b0-251) lsl 8) lor b1) + 108)
          else
            (let b1=input_byte f in
             let b2=input_byte f in
               if b0=28 then
                 (b1 lsl 8) lor b2
               else
                 let b3=input_byte f in
                 let b4=input_byte f in
                   (((((b1 lsl 8) lor b2) lsl 8) lor b3) lsl 8) lor b4
            )
        )
          
          
let index f idx_off=
  seek_in f idx_off;
  let count=readInt f 2 in
  let idx_offSize=input_byte f in
  let idx_arr=Array.make (count+1) 0 in
    for i=0 to count do
      idx_arr.(i) <- idx_off+2+(count+1)*idx_offSize + (readInt f idx_offSize);
    done;
    idx_arr
      
let strIndex f idx_off=
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
let dict f a b=
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
      
      
      
    
let loadFont ?offset:(off=0) ?size:(size=0) file=
  let f=open_in file in
    seek_in f (off+2);
    let hdrSize=input_byte f in
    let offSize=input_byte f in
    let nameIndex=index f (off+hdrSize) in
    let dictIndex=index f (nameIndex.(Array.length nameIndex-1)) in
    let stringIndex=index f (dictIndex.(Array.length dictIndex-1)) in
      
    let subrIndex=
      let subrs=Array.create (Array.length dictIndex-1) [||] in
        for idx=0 to Array.length dictIndex-2 do
          let privOffset=findDict f (dictIndex.(idx)) (dictIndex.(idx+1)) 18 in
            match privOffset with
                offset::size::_->
                  let subrsOffset=int_of_float (List.hd (findDict f (off+int_of_float offset) (off+int_of_float (offset+.size)) 19)) in
                    subrs.(idx) <- strIndex f (off+int_of_float offset+subrsOffset)
              | _->()
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
      { file=f; offset=off; size=size; offSize=offSize; nameIndex=nameIndex; dictIndex=dictIndex;
        stringIndex=stringIndex; gsubrIndex=gsubrIndex; subrIndex=subrIndex }
        

let glyph_of_char _ _=0        
        
let loadGlyph font ?index:(idx=0) gl=
  let charStrings=int_of_float (List.hd (findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 17)) in
  let fontMatrix=
    try
      findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 3079
    with
        Not_found->[0.001;0.;0.;0.001]
  in
    { glyphFont=font;
      glyphNumber=gl;
      type2=indexGet font.file (font.offset+charStrings) gl;
      matrix=Array.of_list fontMatrix;
      subrs=font.subrIndex.(idx);
      gsubrs=font.gsubrIndex }

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
    resultat:=([| !x,!y; x1,y1 |])::(!resultat);
    x:=x1;
    y:=y1
  in
  let curveto x1 y1 x2 y2 x3 y3=
    opened:=true;
    resultat:=([| !x,!y; x1,y1; x2,y2; x3,y3 |])::(!resultat);
    x:=x3;
    y:=y3
  in
  let moveto x1 y1=
    if !opened && (!x <> !x0 || !y <> !y0) then lineto !x0 !y0;
    x:=x1; y:=y1;
    x0:=x1; y0:=y1;
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
      while !pc < String.length program do
        (*showStack stack !stackC;
          print_int (int_of_char (program.[!pc]));print_newline();*)
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
                 while !c <= !stackC-4 do
                   let (dy1,off)=if (!stackC - !c) land 1 = 1 then (stack.(!c), 1) else (0.,0) in
                   let x1=(!x +. stack.(!c+off)) in
                   let y1=(!y +. dy1) in
                   let x2=x1 +. stack.(!c+off+1) in
                   let y2=y1 +. stack.(!c+off+2) in
                   let x3=x2 +. stack.(!c+off+3) in
                   let y3=y2 in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+off+4
                 done;
                 stackC:=0;
                 incr pc)
          | VVCURVETO->
              (let c=ref 0 in
                 while !c <= !stackC-4 do
                   let (dx1,off)=if (!stackC - !c) land 1 = 1 then (stack.(!c), 1) else (0.,0) in
                   let x1=(!x +. dx1) in
                   let y1=(!y +. stack.(!c+off)) in
                   let x2=x1 +. stack.(!c+off+1) in
                   let y2=y1 +. stack.(!c+off+2) in
                   let x3=x2 in
                   let y3=y2 +. stack.(!c+off+3) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+off+4
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
              
              hints := !hints + !stackC / 2 ; stackC:=0 ; incr pc
            )
              
          | HINTMASK->((*print_string "hints : ";print_int !hints;print_string ",";print_int !stackC;print_newline();*)
              if onlyWidth && !stackC > 0 then raise (Found stack.(0));
              hints := !hints + !stackC / 2 ;
              stackC:=0 ; pc := !pc + 1 + (int_of_float (ceil ((float_of_int !hints)/.8.))))
              
          | CNTRMASK->((*print_string "hints : ";print_int !hints;print_string ",";print_int !stackC;print_newline();*)
              stackC:=0 ; pc := !pc + 1 + (int_of_float (ceil ((float_of_int !hints)/.8.))))
              
          | ENDCHAR->
              (if !opened && (!x <> !x0 || !y <> !y0) then lineto !x0 !y0;
               if onlyWidth && !stackC>0 then raise (Found stack.(0));
               pc:=String.length program)
          | RETURN->pc:=String.length program
              
          | CALLSUBR->
              (let subrBias=
                 if Array.length gl.subrs < 1240 then 107 else
                   if Array.length gl.subrs < 33900 then 1131 else 32768
               in
                 execute (gl.subrs.(subrBias + (int_of_float (pop ()))));
                 incr pc)
          | SHORTINT->
              (let a=int_of_char (program.[!pc+1]) in
               let b=int_of_char (program.[!pc+2]) in
                 stack.(!stackC) <- (float_of_int ((a lsl 8) lor b)) /. (float_of_int (1 lsl 16));
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
              ((*print_int (int_of_char (program.[!pc+1]));print_newline ();*)
                match int_of_char (program.[!pc+1]) with
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
                          let (x5,y5)=if abs_float (x4 -. !x) > abs_float (y4 -. !y) then (x4+.stack.(10), y4) else (x4, y4+.stack.(10)) in
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
                          let x3= y1 +. stack.(4) in
                            
                          let x4=x2 +. stack.(5) in
                          let x5=y1 +. stack.(6) in
                            curveto x0 !y x1 y1 x2 y1;
                            curveto x3 y1 x4 y1 x5 y1);
                       stackC:=0;
                       pc:= !pc+2)
                  | HFLEX1->
                      (if !stackC>8 then
                         (let x0= !x +. stack.(0) in
                          let y0= !y +. stack.(1) in
                          let x1= x0 +. stack.(2) in
                          let y1= y0 +. stack.(3) in
                          let x2= x1 +. stack.(4) in
                            
                          let x3=x2 +. stack.(5) in
                          let x4=x3 +. stack.(6) in
                          let y4=y1 +. stack.(7) in
                          let x5=x4 +. stack.(8) in
                            curveto x0 y0 x1 y1 x2 y1;
                            curveto x3 y1 x4 y4 x5 y4);
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

                  | op->failwith ("Type 2 : undefined operator 12 "^(string_of_int op))
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
                          stack.(!stackC) <- (float_of_int ((((((op1 lsl 8) lor op2) lsl 8) lor op3) lsl 8 ) lor op4)) /. (float_of_int (1 lsl 16));
                          incr stackC;
                          pc:= !pc+5
                       )
                )
          | op->failwith ("Type 2 : undefined operator "^(string_of_int op))
      done
  in
    execute gl.type2;
    List.rev !resultat

let outlines glyph=outlines_ glyph false
let glyphWidth glyph=
  try let _=outlines_ glyph true in raise (Found 0.) with
      Found x->
        (try
           let f=glyph.glyphFont.file in
           let off=glyph.glyphFont.offset in
           let privOffset=findDict f (glyph.glyphFont.dictIndex.(0)) (glyph.glyphFont.dictIndex.(1)) 18 in
             match privOffset with
                 offset::size::_->List.hd (findDict f (off+int_of_float offset) (off+int_of_float (offset+.size)) 21)
               | _->0.
         with
             _->0.)+.x


let glyphNumber glyph=glyph.glyphNumber

let fontName ?index:(idx=0) font= 
  let buf = String.create (font.nameIndex.(idx+1)-font.nameIndex.(idx)) in
    seek_in font.file (font.nameIndex.(idx));
    really_input font.file buf 0 (font.nameIndex.(idx+1)-font.nameIndex.(idx));
    buf

let fontBBox ?index:(idx=0) font=
  try
    match findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 5 with
        (a::b::c::d::_)->(int_of_float d,int_of_float c,int_of_float b,int_of_float a)
      | _->(0,0,0,0)
  with
      Not_found->(0,0,0,0)
let italicAngle ?index:(idx=0) font=
  try
    match findDict font.file font.dictIndex.(idx) font.dictIndex.(idx+1) 0x0c02 with
        h::_->h
      | _->0.
  with
      Not_found->0.
