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


exception Found of float
let outlines_ subrs gsubrs gl onlyWidth=
(*  Random.init (int_of_char gl.type2.[0]);*)
  let rstack=ref (Array.make 48 0.) in
  let stackC=ref 0 in
  let stWrite c x=
      (if c>= Array.length (!rstack) then
         rstack:=Array.init (Array.length !rstack*2)
           (fun i->if i<Array.length !rstack then (!rstack).(i) else 0.)
      );
      (!rstack).(c)<-x
  in
  let pop ()=
    if !stackC<=0 then failwith "CFF.outlines : empty stack" else
      (decr stackC;
       (!rstack).(!stackC)) in
  let heap=Array.make 33 0. in
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
      (lineto (!x+.(!rstack).(c)) !y;
       vlineto (c+1))
    else
      stackC:=0
  and vlineto c=
    if c <= !stackC-1 then
      (lineto !x (!y+.(!rstack).(c));
       hlineto (c+1))
    else
      stackC:=0
  in
  let rec hvcurveto c=
    if c <= !stackC-4 then
      (curveto
         (!x +. (!rstack).(c)) !y
         (!x +. (!rstack).(c) +. (!rstack).(c+1)) (!y +. (!rstack).(c+2))
         (!x +. (!rstack).(c) +. (!rstack).(c+1) +. (if !stackC-c = 5 then (!rstack).(c+4) else 0.))
         (!y +. (!rstack).(c+2) +. (!rstack).(c+3));
       vhcurveto (c+4))
    else
      stackC:=0
  and vhcurveto c=
    if c <= !stackC-4 then
      (curveto
         !x (!y +. (!rstack).(c))
         (!x +. (!rstack).(c+1)) (!y +. (!rstack).(c) +. (!rstack).(c+2))
         (!x +. (!rstack).(c+1) +. (!rstack).(c+3))
         (!y +. (!rstack).(c) +. (!rstack).(c+2) +. (if !stackC-c = 5 then (!rstack).(c+4) else 0.));
       hvcurveto (c+4))
    else
      stackC:=0
  in
  let rec execute program=
    let pc=ref 0 in
      while !pc <  (String.length program) do
        (* showStack (!rstack) !stackC; *)
        (* Printf.printf "%d > %d\n" !pc (int_of_char (program.[!pc])); *)
        (* flush stdout; *)
        match int_of_char (program.[!pc]) with
            RMOVETO->
              (moveto (!x +. (!rstack).(!stackC-2)) (!y +. (!rstack).(!stackC-1));
               if onlyWidth && !stackC>2 && !stackC land 1=1 then raise (Found (!rstack).(0));
               stackC:=0;
               incr pc)
          | HMOVETO->
              (moveto (!x +. (!rstack).(!stackC-1)) !y;
               if onlyWidth && !stackC>1 && !stackC land 1=0 then raise (Found (!rstack).(0));
               stackC:=0;
               incr pc)
          | VMOVETO->
              (moveto !x (!y +. (!rstack).(!stackC-1));
               if onlyWidth && !stackC>1 && !stackC land 1=0 then raise (Found (!rstack).(0));
               stackC:=0;
               incr pc)
          | RLINETO->
              (let c=ref 0 in
                 while !c <= !stackC-2 do
                   lineto (!x +. (!rstack).(!c)) (!y +. (!rstack).(!c+1));
                   c:= !c+2
                 done;
                 stackC:=0;
                 incr pc)
          | HLINETO->(hlineto 0; incr pc)
          | VLINETO->(vlineto 0; incr pc)
          | RRCURVETO->
              (let c=ref 0 in
                 while !c <= !stackC-6 do
                   let x1=(!x +. (!rstack).(!c)) in
                   let y1=(!y +. (!rstack).(!c+1)) in
                   let x2=x1 +. (!rstack).(!c+2) in
                   let y2=y1 +. (!rstack).(!c+3) in
                   let x3=x2 +. (!rstack).(!c+4) in
                   let y3=y2 +. (!rstack).(!c+5) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+6
                 done;
                 stackC:=0;
                 incr pc)
          | VHCURVETO->(vhcurveto 0; incr pc)
          | HVCURVETO->(hvcurveto 0; incr pc)
          | HHCURVETO->
              (let c=ref 0 in
               let dy1=if (!stackC) land 1 = 1 then (incr c; (!rstack).(0)) else 0. in
                 while !c <= !stackC-4 do
                   let x1= !x +. (!rstack).(!c) in
                   let y1= !y +. (if !c=1 then dy1 else 0.) in
                   let x2=x1 +. (!rstack).(!c+1) in
                   let y2=y1 +. (!rstack).(!c+2) in
                   let x3=x2 +. (!rstack).(!c+3) in
                   let y3=y2 in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+4
                 done;
                 stackC:=0;
                 incr pc)
          | VVCURVETO->
              (let c=ref 0 in
               let dx1=if (!stackC - !c) land 1 = 1 then (incr c; (!rstack).(0)) else 0. in
                 while !c <= !stackC-4 do
                   let x1=(!x +. (if !c=1 then dx1 else 0.)) in
                   let y1=(!y +. (!rstack).(!c)) in
                   let x2=x1 +. (!rstack).(!c+1) in
                   let y2=y1 +. (!rstack).(!c+2) in
                   let x3=x2 in
                   let y3=y2 +. (!rstack).(!c+3) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+4
                 done;
                 stackC:=0;
                 incr pc)
          | RCURVELINE->
              (let c=ref 0 in
                 while !c <= !stackC-8 do
                   let x1=(!x +. (!rstack).(!c)) in
                   let y1=(!y +. (!rstack).(!c+1)) in
                   let x2=x1 +. (!rstack).(!c+2) in
                   let y2=y1 +. (!rstack).(!c+3) in
                   let x3=x2 +. (!rstack).(!c+4) in
                   let y3=y2 +. (!rstack).(!c+5) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+6
                 done;
                 lineto (!x +. (!rstack).(!c)) (!y +. (!rstack).(!c+1));
                 stackC:=0;
                 incr pc)
          | RLINECURVE->
              (let c=ref 0 in
                 while !c <= !stackC-8 do
                   lineto (!x+.(!rstack).(!c)) (!y+.(!rstack).(!c+1));
                   c:= !c+2
                 done;
                 let x1=(!x +. (!rstack).(!c)) in
                 let y1=(!y +. (!rstack).(!c+1)) in
                 let x2=x1 +. (!rstack).(!c+2) in
                 let y2=y1 +. (!rstack).(!c+3) in
                 let x3=x2 +. (!rstack).(!c+4) in
                 let y3=y2 +. (!rstack).(!c+5) in
                   curveto x1 y1 x2 y2 x3 y3;
                   stackC:=0;
                   incr pc)
          | HSTEM | VSTEM | HSTEMHM
          | VSTEMHM->(
              if onlyWidth && (!stackC land 1)=1 then raise (Found (!rstack).(0));
              hints := !hints + !stackC / 2 ;
              stackC:=0 ; incr pc
            )
          | HINTMASK | CNTRMASK ->(
              hints:= !hints+ !stackC/2;
              if onlyWidth && !stackC land 1=1 then raise (Found (!rstack).(0));
              stackC:=0 ; pc := !pc + 1 + (if !hints land 7=0 then (!hints/8) else (!hints/8+1))
            )
          | ENDCHAR->
              (if !opened && (!x <> !x0 || !y <> !y0) then lineto !x0 !y0;
               if onlyWidth && !stackC>0 then raise (Found (!rstack).(0));
               match !resultat with []::s -> resultat:=s | _->();
               pc:=String.length program)
          | RETURN->pc:=String.length program
          | CALLSUBR->
              (let subrBias=
                 if Array.length subrs < 1240 then 107 else
                   if Array.length subrs < 33900 then 1131 else 32768
               in
               let subr=((subrBias + (int_of_float (pop ())))) in
                 execute subrs.(subr);
                 incr pc)
          | SHORTINT->
              (let a=int_of_char (program.[!pc+1]) in
               let b=int_of_char (program.[!pc+2]) in
                 stWrite (!stackC) (float_of_int ((((a land 0x7f) lsl 8) lor b) - (if a>=128 then 1 lsl 15 else 0)));
                 incr stackC;
                 pc:= !pc+3)
          | CALLGSUBR->
              (let gsubrBias=
                 if Array.length gsubrs < 1240 then 107 else
                   if Array.length gsubrs < 33900 then 1131 else 32768
               in
                 execute (gsubrs.(gsubrBias + (int_of_float (pop ()))));
                 incr pc)
          | ESCAPE->
              (match int_of_char (program.[!pc+1]) with
                    FLEX->
                      (if !stackC>=12 then
                         (let x0= !x +. (!rstack).(0) in
                          let y0= !y +. (!rstack).(1) in
                          let x1= x0 +. (!rstack).(2) in
                          let y1= y0 +. (!rstack).(3) in
                          let x2= x1 +. (!rstack).(4) in
                          let y2= y1 +. (!rstack).(5) in
                          let x3=x2 +. (!rstack).(6) in
                          let y3=y2 +. (!rstack).(7) in
                          let x4=x3 +. (!rstack).(8) in
                          let y4=y3 +. (!rstack).(9) in
                          let x5=x4 +. (!rstack).(10) in
                          let y5=y4 +. (!rstack).(11) in
                            curveto x0 y0 x1 y1 x2 y2;
                            curveto x3 y3 x4 y4 x5 y5);
                       stackC:=0;
                       pc:= !pc+2)
                  | FLEX1->
                      (if !stackC>9 then
                         (let x0= !x +. (!rstack).(0) in
                          let y0= !y +. (!rstack).(1) in
                          let x1= x0 +. (!rstack).(2) in
                          let y1= y0 +. (!rstack).(3) in
                          let x2= x1 +. (!rstack).(4) in
                          let y2= y1 +. (!rstack).(5) in
                          let x3=x2 +. (!rstack).(6) in
                          let y3=y2 +. (!rstack).(7) in
                          let x4=x3 +. (!rstack).(8) in
                          let y4=y3 +. (!rstack).(9) in
                          let (x5,y5)=if abs_float (x4 -. !x) > abs_float (y4 -. !y) then
                            (x4+.(!rstack).(10), y4) else
                            (x4, y4+.(!rstack).(10))
                          in
                            curveto x0 y0 x1 y1 x2 y2;
                            curveto x3 y3 x4 y4 x5 y5);
                       stackC:=0;
                       pc:= !pc+2)
                  | HFLEX->
                      (if !stackC>6 then
                         (let x0= !x +. (!rstack).(0) in
                          let x1= x0 +. (!rstack).(1) in
                          let y1= !y +. (!rstack).(2) in
                          let x2= x1 +. (!rstack).(3) in
                          let x3= x2 +. (!rstack).(4) in
                          let x4= x3 +. (!rstack).(5) in
                          let x5= x4 +. (!rstack).(6) in
                            curveto x0 !y x1 y1 x2 y1;
                            curveto x3 y1 x4 !y x5 !y);
                       stackC:=0;
                       pc:= !pc+2)
                  | HFLEX1->
                      (if !stackC>8 then
                         (let x0= !x +. (!rstack).(0) in
                          let y0= !y +. (!rstack).(1) in
                          let x1= x0 +. (!rstack).(2) in
                          let y1= y0 +. (!rstack).(3) in
                          let x2= x1 +. (!rstack).(4) in
                          let x3= x2 +. (!rstack).(5) in
                          let x4= x3 +. (!rstack).(6) in
                          let y4= y1 +. (!rstack).(7) in
                          let x5= x4 +. (!rstack).(8) in
                            curveto x0 y0 x1 y1 x2 y1;
                            curveto x3 y1 x4 y4 x5 !y);
                       stackC:=0;
                       pc:= !pc+2)
                  | ABS->(if !stackC>0 then (!rstack).(!stackC-1) <- abs_float ((!rstack).(!stackC-1)); pc:= !pc+2)
                  | ADD->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) +. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | SUB->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) -. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | DIV->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) /. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | NEG->(if !stackC>0 then (!rstack).(!stackC-1) <- -. ((!rstack).(!stackC-1)); pc:= !pc+2)
                  | RANDOM->(stWrite (!stackC) (Random.float (float_of_int 0xffff)); incr stackC; pc:= !pc+2)
                  | MUL->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) *. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | SQRT->(if !stackC>0 then (!rstack).(!stackC-1) <- sqrt ((!rstack).(!stackC-1)); pc:= !pc+2)
                  | DROP->(stackC := !stackC - (int_of_float (pop ())); pc:= !pc+2)
                  | EXCH->
                      (let tmp=(!rstack).(!stackC-1) in
                         (!rstack).(!stackC-1)<-(!rstack).(!stackC-2);
                         (!rstack).(!stackC-2)<-tmp;
                         pc:= !pc+2)
                  | INDEX->
                      ((!rstack).(!stackC-1) <- (!rstack).(!stackC - 1 - (int_of_float ((!rstack).(!stackC-1))));
                       pc:= !pc+2)
                  | ROLL->
                      (let num=int_of_float ((!rstack).(!stackC-2)) in
                       let j=
                         let j=int_of_float ((!rstack).(!stackC-1)) in
                           if j<0 then (j mod num) + num else j
                       in
                       let stack'=Array.copy !rstack in
                         for i= !stackC-2-num to !stackC-2 do
                           (!rstack).(i) <- stack'.(!stackC-2-num  +  ((i+j) mod num))
                         done;
                         stackC:= !stackC-2;
                         pc:= !pc+2
                      )
                  | DUP->
                      (stWrite (!stackC) ((!rstack).(!stackC-1));
                       incr stackC;
                       pc:= !pc+2)
                  | PUT->
                      (heap.(int_of_float ((!rstack).(!stackC-1))) <- (!rstack).(!stackC-2);
                       stackC:= !stackC-2;
                       pc:= !pc+2)
                  | GET->
                      ((!rstack).(!stackC-1) <- heap.(int_of_float ((!rstack).(!stackC-1)));
                       pc:= !pc+2)
                  | AND->
                      ((!rstack).(!stackC-2) <- if (!rstack).(!stackC-1) <> 0. && (!rstack).(!stackC-2) <> 0. then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | OR->
                      ((!rstack).(!stackC-2) <- if (!rstack).(!stackC-1) <> 0. || (!rstack).(!stackC-2) <> 0. then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | NOT->
                      ((!rstack).(!stackC-1) <- if (!rstack).(!stackC-1) = 0. then 1. else 0.;
                       pc:= !pc+2)
                  | EQ->
                      ((!rstack).(!stackC-2) <- if (!rstack).(!stackC-1) = (!rstack).(!stackC-2) then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | IFELSE->
                      (if (!rstack).(!stackC-2) <= (!rstack).(!stackC-1) then (!rstack).(!stackC-4) <- (!rstack).(!stackC-3);
                       stackC:= !stackC-3;
                       pc:= !pc+2)
                  | 0 -> incr pc        (* dotsection (deprecated operator) *)
                  | op->(
                      Printf.fprintf stderr "Type 2 : undefined operator %d\n" op;
                      pc:= !pc+1
                    )
                )
          | op when op>=32 ->
              if op<=246 then (stWrite (!stackC) (float_of_int (op-139)); incr stackC; incr pc) else
                (let op1=int_of_char (program.[!pc+1]) in
                   if op<=250 then
                     (stWrite (!stackC) (float_of_int (((op-247) lsl 8) + op1 + 108));
                      incr stackC;
                      pc:= !pc+2)
                   else
                     if op<=254 then
                       (stWrite (!stackC) (-. (float_of_int (((op-251) lsl 8) + op1 + 108)));
                        incr stackC;
                        pc:= !pc+2)
                     else
                       (let op2=int_of_char (program.[!pc+2]) in
                        let op3=int_of_char (program.[!pc+3]) in
                        let op4=int_of_char (program.[!pc+4]) in
                          stWrite (!stackC) (
                            ((float_of_int (op1 land 0x7f))*.16777216. +.
                               (float_of_int op2)*.65536. +.
                               (float_of_int op3)*.256. +.
                               (float_of_int op4) -. (if op1 land 0x80 <> 0 then 2147483648. else 0.)) /. (65536.)
                          );
                          incr stackC;
                          pc:= !pc+5
                       )
                )
          | op->(
              Printf.fprintf stderr "Type 2 : undefined operator %d\n" op;
              pc:= !pc+1
            )
      done
  in
    execute gl;
    List.rev_map List.rev !resultat




let rec unsubr subrs gsubrs gl=
  let buf=Buffer.create 1000 in
  let changed=ref false in
  (*  Random.init (int_of_char gl.type2.[0]);*)
  let rstack=ref (Array.make 48 0.) in
  let stackC=ref 0 in
  let stWrite c x=
      (if c>= Array.length (!rstack) then
         rstack:=Array.init (Array.length !rstack*2)
           (fun i->if i<Array.length !rstack then (!rstack).(i) else 0.)
      );
      (!rstack).(c)<-x
  in
  let pop ()=
    if !stackC<=0 then failwith "CFF.outlines : empty stack" else
      (decr stackC;
       (!rstack).(!stackC)) in
  let heap=Array.make 33 0. in
  let hints=ref 0 in
  let opened=ref false in
  let x=ref 0. in
  let y=ref 0. in
  let x0=ref 0. in
  let y0=ref 0. in
  let resultat=ref [] in
  let lineto x1 y1=() in
  let curveto x1 y1 x2 y2 x3 y3=() in
  let moveto x1 y1=() in
  let hlineto c =
    if c <= !stackC-1 then ()
    else stackC:=0
  in
  let vlineto c=
    if c <= !stackC-1 then ()
    else stackC:=0
  in
  let rec hvcurveto c=
    if c <= !stackC-4 then
      (curveto
         (!x +. (!rstack).(c)) !y
         (!x +. (!rstack).(c) +. (!rstack).(c+1)) (!y +. (!rstack).(c+2))
         (!x +. (!rstack).(c) +. (!rstack).(c+1) +. (if !stackC-c = 5 then (!rstack).(c+4) else 0.))
         (!y +. (!rstack).(c+2) +. (!rstack).(c+3));
       vhcurveto (c+4))
    else
      stackC:=0
  and vhcurveto c=
    if c <= !stackC-4 then
      (curveto
         !x (!y +. (!rstack).(c))
         (!x +. (!rstack).(c+1)) (!y +. (!rstack).(c) +. (!rstack).(c+2))
         (!x +. (!rstack).(c+1) +. (!rstack).(c+3))
         (!y +. (!rstack).(c) +. (!rstack).(c+2) +. (if !stackC-c = 5 then (!rstack).(c+4) else 0.));
       hvcurveto (c+4))
    else
      stackC:=0
  in
  let rec execute program=
    let pc=ref 0 in
      while !pc <  (String.length program) do
        (* showStack (!rstack) !stackC; *)
        (* Printf.printf "%d > %d\n" !pc (int_of_char (program.[!pc])); *)
        (* flush stdout; *)
        let op=(program.[!pc]) in
        match int_of_char op with
            RMOVETO->
              (moveto (!x +. (!rstack).(!stackC-2)) (!y +. (!rstack).(!stackC-1));
               stackC:=0;
               Buffer.add_char buf op;
               incr pc)
          | HMOVETO->
              (moveto (!x +. (!rstack).(!stackC-1)) !y;
               stackC:=0;
               Buffer.add_char buf op;
               incr pc)
          | VMOVETO->
              (moveto !x (!y +. (!rstack).(!stackC-1));
               stackC:=0;
               Buffer.add_char buf op;
               incr pc)
          | RLINETO->
              (let c=ref 0 in
                 while !c <= !stackC-2 do
                   lineto (!x +. (!rstack).(!c)) (!y +. (!rstack).(!c+1));
                   c:= !c+2
                 done;
                 stackC:=0;
                 Buffer.add_char buf op;
                 incr pc)
          | HLINETO->(hlineto 0;Buffer.add_char buf op;incr pc)
          | VLINETO->(vlineto 0;Buffer.add_char buf op;incr pc)
          | RRCURVETO->
              (let c=ref 0 in
                 while !c <= !stackC-6 do
                   let x1=(!x +. (!rstack).(!c)) in
                   let y1=(!y +. (!rstack).(!c+1)) in
                   let x2=x1 +. (!rstack).(!c+2) in
                   let y2=y1 +. (!rstack).(!c+3) in
                   let x3=x2 +. (!rstack).(!c+4) in
                   let y3=y2 +. (!rstack).(!c+5) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+6
                 done;
                 stackC:=0;
                 Buffer.add_char buf op;
                 incr pc)
          | VHCURVETO->(vhcurveto 0; Buffer.add_char buf op;incr pc)
          | HVCURVETO->(hvcurveto 0; Buffer.add_char buf op;incr pc)
          | HHCURVETO->
              (let c=ref 0 in
               let dy1=if (!stackC) land 1 = 1 then (incr c; (!rstack).(0)) else 0. in
                 while !c <= !stackC-4 do
                   let x1= !x +. (!rstack).(!c) in
                   let y1= !y +. (if !c=1 then dy1 else 0.) in
                   let x2=x1 +. (!rstack).(!c+1) in
                   let y2=y1 +. (!rstack).(!c+2) in
                   let x3=x2 +. (!rstack).(!c+3) in
                   let y3=y2 in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+4
                 done;
                 stackC:=0;
                 Buffer.add_char buf op;
                 incr pc)
          | VVCURVETO->
              (let c=ref 0 in
               let dx1=if (!stackC - !c) land 1 = 1 then (incr c; (!rstack).(0)) else 0. in
                 while !c <= !stackC-4 do
                   let x1=(!x +. (if !c=1 then dx1 else 0.)) in
                   let y1=(!y +. (!rstack).(!c)) in
                   let x2=x1 +. (!rstack).(!c+1) in
                   let y2=y1 +. (!rstack).(!c+2) in
                   let x3=x2 in
                   let y3=y2 +. (!rstack).(!c+3) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+4
                 done;
                 stackC:=0;
                 Buffer.add_char buf op;
                 incr pc)
          | RCURVELINE->
              (let c=ref 0 in
                 while !c <= !stackC-8 do
                   let x1=(!x +. (!rstack).(!c)) in
                   let y1=(!y +. (!rstack).(!c+1)) in
                   let x2=x1 +. (!rstack).(!c+2) in
                   let y2=y1 +. (!rstack).(!c+3) in
                   let x3=x2 +. (!rstack).(!c+4) in
                   let y3=y2 +. (!rstack).(!c+5) in
                     curveto x1 y1 x2 y2 x3 y3;
                     c:= !c+6
                 done;
                 lineto (!x +. (!rstack).(!c)) (!y +. (!rstack).(!c+1));
                 stackC:=0;
                 Buffer.add_char buf op;
                 incr pc)
          | RLINECURVE->
              (let c=ref 0 in
                 while !c <= !stackC-8 do
                   lineto (!x+.(!rstack).(!c)) (!y+.(!rstack).(!c+1));
                   c:= !c+2
                 done;
                 let x1=(!x +. (!rstack).(!c)) in
                 let y1=(!y +. (!rstack).(!c+1)) in
                 let x2=x1 +. (!rstack).(!c+2) in
                 let y2=y1 +. (!rstack).(!c+3) in
                 let x3=x2 +. (!rstack).(!c+4) in
                 let y3=y2 +. (!rstack).(!c+5) in
                   curveto x1 y1 x2 y2 x3 y3;
                   stackC:=0;
                   Buffer.add_char buf op;
                   incr pc)
          | HSTEM | VSTEM | HSTEMHM
          | VSTEMHM->(
              hints := !hints + !stackC / 2 ;
            Buffer.add_char buf op;
              stackC:=0 ; incr pc
            )
          | HINTMASK | CNTRMASK ->(
              hints:= !hints+ !stackC/2;
            Buffer.add_char buf op;
              stackC:=0 ; pc := !pc + 1 + (if !hints land 7=0 then (!hints/8) else (!hints/8+1))
            )
          | ENDCHAR->
              (if !opened && (!x <> !x0 || !y <> !y0) then lineto !x0 !y0;
               match !resultat with []::s -> resultat:=s | _->();
                 Buffer.add_char buf op;
               pc:=String.length program)
          | RETURN->(Buffer.add_char buf op;pc:=String.length program)
          | CALLSUBR->
              (let subrBias=
                 if Array.length subrs < 1240 then 107 else
                   if Array.length subrs < 33900 then 1131 else 32768
               in
               let subr=((subrBias + (int_of_float (pop ())))) in
               changed:=true;
               Printf.fprintf stderr "unsubring %d %d\n" subr (Array.length subrs);flush stderr;
               Buffer.add_char buf (char_of_int DROP);
               Buffer.add_string buf subrs.(subr);
                 (* execute subrs.(subr); *)
               incr pc)
          | SHORTINT->
              (let a=int_of_char (program.[!pc+1]) in
               let b=int_of_char (program.[!pc+2]) in
                 stWrite (!stackC) (float_of_int ((((a land 0x7f) lsl 8) lor b) - (if a>=128 then 1 lsl 15 else 0)));
               incr stackC;
               Buffer.add_char buf op;
               pc:= !pc+3)
          | CALLGSUBR->
              (let gsubrBias=
                 if Array.length gsubrs < 1240 then 107 else
                   if Array.length gsubrs < 33900 then 1131 else 32768
               in
               execute (gsubrs.(gsubrBias + (int_of_float (pop ()))));
               Buffer.add_char buf op;
               incr pc)
          | ESCAPE->
              (let op2= (program.[!pc+1]) in
               Buffer.add_char buf op;
               Buffer.add_char buf op2;
                 match int_of_char op2 with
                    FLEX->
                      (if !stackC>=12 then
                         (let x0= !x +. (!rstack).(0) in
                          let y0= !y +. (!rstack).(1) in
                          let x1= x0 +. (!rstack).(2) in
                          let y1= y0 +. (!rstack).(3) in
                          let x2= x1 +. (!rstack).(4) in
                          let y2= y1 +. (!rstack).(5) in
                          let x3=x2 +. (!rstack).(6) in
                          let y3=y2 +. (!rstack).(7) in
                          let x4=x3 +. (!rstack).(8) in
                          let y4=y3 +. (!rstack).(9) in
                          let x5=x4 +. (!rstack).(10) in
                          let y5=y4 +. (!rstack).(11) in
                          curveto x0 y0 x1 y1 x2 y2;
                          curveto x3 y3 x4 y4 x5 y5);
                       stackC:=0;
                       pc:= !pc+2)
                  | FLEX1->
                      (if !stackC>9 then
                         (let x0= !x +. (!rstack).(0) in
                          let y0= !y +. (!rstack).(1) in
                          let x1= x0 +. (!rstack).(2) in
                          let y1= y0 +. (!rstack).(3) in
                          let x2= x1 +. (!rstack).(4) in
                          let y2= y1 +. (!rstack).(5) in
                          let x3=x2 +. (!rstack).(6) in
                          let y3=y2 +. (!rstack).(7) in
                          let x4=x3 +. (!rstack).(8) in
                          let y4=y3 +. (!rstack).(9) in
                          let (x5,y5)=if abs_float (x4 -. !x) > abs_float (y4 -. !y) then
                            (x4+.(!rstack).(10), y4) else
                            (x4, y4+.(!rstack).(10))
                          in
                            curveto x0 y0 x1 y1 x2 y2;
                            curveto x3 y3 x4 y4 x5 y5);
                       stackC:=0;
                       pc:= !pc+2)
                  | HFLEX->
                      (if !stackC>6 then
                         (let x0= !x +. (!rstack).(0) in
                          let x1= x0 +. (!rstack).(1) in
                          let y1= !y +. (!rstack).(2) in
                          let x2= x1 +. (!rstack).(3) in
                          let x3= x2 +. (!rstack).(4) in
                          let x4= x3 +. (!rstack).(5) in
                          let x5= x4 +. (!rstack).(6) in
                            curveto x0 !y x1 y1 x2 y1;
                            curveto x3 y1 x4 !y x5 !y);
                       stackC:=0;
                       pc:= !pc+2)
                  | HFLEX1->
                      (if !stackC>8 then
                         (let x0= !x +. (!rstack).(0) in
                          let y0= !y +. (!rstack).(1) in
                          let x1= x0 +. (!rstack).(2) in
                          let y1= y0 +. (!rstack).(3) in
                          let x2= x1 +. (!rstack).(4) in
                          let x3= x2 +. (!rstack).(5) in
                          let x4= x3 +. (!rstack).(6) in
                          let y4= y1 +. (!rstack).(7) in
                          let x5= x4 +. (!rstack).(8) in
                            curveto x0 y0 x1 y1 x2 y1;
                            curveto x3 y1 x4 y4 x5 !y);
                       stackC:=0;
                       pc:= !pc+2)
                  | ABS->(if !stackC>0 then (!rstack).(!stackC-1) <- abs_float ((!rstack).(!stackC-1)); pc:= !pc+2)
                  | ADD->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) +. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | SUB->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) -. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | DIV->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) /. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | NEG->(if !stackC>0 then (!rstack).(!stackC-1) <- -. ((!rstack).(!stackC-1)); pc:= !pc+2)
                  | RANDOM->(stWrite (!stackC) (Random.float (float_of_int 0xffff)); incr stackC; pc:= !pc+2)
                  | MUL->(if !stackC>1 then (!rstack).(!stackC-2) <- (!rstack).(!stackC-2) *. (!rstack).(!stackC-1); decr stackC; pc:= !pc+2)
                  | SQRT->(if !stackC>0 then (!rstack).(!stackC-1) <- sqrt ((!rstack).(!stackC-1)); pc:= !pc+2)
                  | DROP->(stackC := !stackC - (int_of_float (pop ())); pc:= !pc+2)
                  | EXCH->
                      (let tmp=(!rstack).(!stackC-1) in
                         (!rstack).(!stackC-1)<-(!rstack).(!stackC-2);
                         (!rstack).(!stackC-2)<-tmp;
                         pc:= !pc+2)
                  | INDEX->
                      ((!rstack).(!stackC-1) <- (!rstack).(!stackC - 1 - (int_of_float ((!rstack).(!stackC-1))));
                       pc:= !pc+2)
                  | ROLL->
                      (let num=int_of_float ((!rstack).(!stackC-2)) in
                       let j=
                         let j=int_of_float ((!rstack).(!stackC-1)) in
                           if j<0 then (j mod num) + num else j
                       in
                       let stack'=Array.copy !rstack in
                         for i= !stackC-2-num to !stackC-2 do
                           (!rstack).(i) <- stack'.(!stackC-2-num  +  ((i+j) mod num))
                         done;
                         stackC:= !stackC-2;
                         pc:= !pc+2
                      )
                  | DUP->
                      (stWrite (!stackC) ((!rstack).(!stackC-1));
                       incr stackC;
                       pc:= !pc+2)
                  | PUT->
                      (heap.(int_of_float ((!rstack).(!stackC-1))) <- (!rstack).(!stackC-2);
                       stackC:= !stackC-2;
                       pc:= !pc+2)
                  | GET->
                      ((!rstack).(!stackC-1) <- heap.(int_of_float ((!rstack).(!stackC-1)));
                       pc:= !pc+2)
                  | AND->
                      ((!rstack).(!stackC-2) <- if (!rstack).(!stackC-1) <> 0. && (!rstack).(!stackC-2) <> 0. then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | OR->
                      ((!rstack).(!stackC-2) <- if (!rstack).(!stackC-1) <> 0. || (!rstack).(!stackC-2) <> 0. then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | NOT->
                      ((!rstack).(!stackC-1) <- if (!rstack).(!stackC-1) = 0. then 1. else 0.;
                       pc:= !pc+2)
                  | EQ->
                      ((!rstack).(!stackC-2) <- if (!rstack).(!stackC-1) = (!rstack).(!stackC-2) then 1. else 0.;
                       decr stackC;
                       pc:= !pc+2)
                  | IFELSE->
                      (if (!rstack).(!stackC-2) <= (!rstack).(!stackC-1) then (!rstack).(!stackC-4) <- (!rstack).(!stackC-3);
                       stackC:= !stackC-3;
                       pc:= !pc+2)
                  | 0 -> incr pc        (* dotsection (deprecated operator) *)
                  | op->(
                      Printf.fprintf stderr "Type 2 : undefined operator %d\n" op;
                      pc:= !pc+1
                    )
                )
          | op when op>=32 ->
              if op<=246 then (stWrite (!stackC) (float_of_int (op-139)); incr stackC; incr pc) else
                (let op1=int_of_char (program.[!pc+1]) in
                   if op<=250 then
                     (stWrite (!stackC) (float_of_int (((op-247) lsl 8) + op1 + 108));
                      incr stackC;
                      pc:= !pc+2)
                   else
                     if op<=254 then
                       (stWrite (!stackC) (-. (float_of_int (((op-251) lsl 8) + op1 + 108)));
                        incr stackC;
                        pc:= !pc+2)
                     else
                       (let op2=int_of_char (program.[!pc+2]) in
                        let op3=int_of_char (program.[!pc+3]) in
                        let op4=int_of_char (program.[!pc+4]) in
                          stWrite (!stackC) (
                            ((float_of_int (op1 land 0x7f))*.16777216. +.
                               (float_of_int op2)*.65536. +.
                               (float_of_int op3)*.256. +.
                               (float_of_int op4) -. (if op1 land 0x80 <> 0 then 2147483648. else 0.)) /. (65536.)
                          );
                          incr stackC;
                          pc:= !pc+5
                       )
                )
          | op->(
              Printf.fprintf stderr "Type 2 : undefined operator %d\n" op;
              pc:= !pc+1
            )
      done
  in
  execute gl;
  if !changed then unsubr subrs gsubrs (Buffer.contents buf) else
    (Buffer.contents buf)
