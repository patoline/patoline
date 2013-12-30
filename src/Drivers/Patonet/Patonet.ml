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
open OutputCommon
open OutputPaper
open Util
open HtmlFonts

let base64_decode s=
  let buf=Buffer.create (String.length s) in
  let value i=
    let x=s.[i] in
    if x>='A' && x<='Z' then (int_of_char x)-(int_of_char 'A') else
      if x>='a' && x<='z' then 26+(int_of_char x)-(int_of_char 'a') else
        if x>='0' && x<='9' then 52+(int_of_char x)-(int_of_char '0') else
          if x='+' then 62 else
            if x='/' then 63 else if x='=' then 64 else (-1)
  in
  let ii=ref 0 in
  let rec next ()=
    let x=value !ii in
    incr ii;
    if x>=0 then x else next ()
  in
  let rec read_all ()=
    if !ii<String.length s-3 then (
      let a=next() in
      let b=next() in
      let c=next() in
      let d=next() in
      if d=64 then (
        if c=64 then (
          let x=(a lsl 6) lor b in
          Buffer.add_char buf  (char_of_int ((x lsr 4) land 0xff));
        ) else (
          let x=(((a lsl 6) lor b) lsl 6) lor c in
          Buffer.add_char buf (char_of_int ((x lsr 10) land 0xff));
          Buffer.add_char buf (char_of_int ((x lsr 2) land 0xff));
        )
      ) else (
        let x=(((((a lsl 6) lor b) lsl 6) lor c) lsl 6) lor d in
        Buffer.add_char buf (char_of_int ((x lsr 16) land 0xff));
        Buffer.add_char buf (char_of_int ((x lsr 8) land 0xff));
        Buffer.add_char buf (char_of_int (x land 0xff));
      );
      read_all ()
    )
  in
  read_all ();
  Buffer.contents buf

let duck_ico =
"\000\000\001\000\001\000  \000\000\001\000\024\000\168\012\000\000\022\000\000\000(\000\000\000 \000\000\000@\000\000\000\001\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\244\245\245\250\250\250\239\240\241\239\239\239\246\246\246\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255HGF*('PPOHHF\221\221\221\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\253\253\253\254\254\254\254\254\254\249\249\249\247\247\247\247\246\245\177\176\175\127~}``^\248\249\248\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\253\253\253\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\242\243\244\174\176\175\175\174\174\138\138\137\129\130\131\132\134\134\231\232\235\255\255\255\255\255\255lli^][\235\235\234\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\229\230\230\166\167\167{{xWVSfda^[X.-*\030\029\0260-,(% SSPGGEtro\253\253\253\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\253\253\253\179\180\178\130\129~CA;1/(C>90-'fd_MLIGFCGFD986ed`\\ZX\240\241\240\174\173\173\173\173\172\229\229\228\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\246\246\246stq\138\137\134[WTGE@MJETRLOKF\150\148\144\022\024\023:75\026\024\0202/-75.\238\237\237\241\241\241\201\201\201\195\194\194432\154\155\153\227\228\227\255\255\255\255\255\255\227\227\227\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\252\252\252z{w30,PMGCB?bc]ec`>=9JGDVSQ\012\011\011)%##!\031\175\172\172^^Z\217\218\218\144\144\143\152\152\149665\162\162\162\154\153\154\195\195\194\148\150\149\233\234\234xxv\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\199\198\198\018\016\rGD?41-A=9\014\012\n^\\Z\021\021\020\225\224\224\205\205\203\006\005\004\020\019\01641.ZYX)(#]\\Z>>=\192\192\192\129\128\128POOrrr\254\254\254\237\237\237qom\194\195\195\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255igc>;5\245\245\244\017\015\011+)%\207\206\204\170\168\166\n\n\t=<:??>wtr\011\n\t84.<82\153\151\149\225\225\224{zy]]\\\150\149\149fec\144\145\143\255\255\255\142\143\141\158\159\157CBB\187\186\186\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\240\240\239875VTL\027\026\023\201\200\197\"!\031\015\015\rkjg\014\r\011 \030\028TOK\204\206\205\140\141\139\138\137\137322\233\232\232hihyywOMMdecllj\\[Z\160\161\161FECMLM./-ffe\240\240\241\255\255\255\254\254\254\255\255\255\254\254\254\234\234\234\006\005\001e`ZQNI('$\165\165\164\000\000\000sqn\227\227\225~}z\238\239\238\158\159\157\151\150\149\143\143\14310/\172\172\171RQPLLI```ZYXlkiba`676A@?QPN\157\156\154\137\137\135\152\152\150\255\255\255\254\254\254\254\254\254\254\254\254\229\229\228\017\016\011fd\\2/*\205\204\203\231\230\229\152\151\149\150\147\145\192\193\192\173\173\171\197\197\198\156\156\154\164\165\163\159\159\157EEC\160\160\158TSR``_\127}|fecnnl\\\\[xxvjjj\127\127}))'HGF\144\143\141\254\254\254\255\255\255\255\255\255\255\255\255\233\233\232\014\011\007b_W51+\231\229\229\163\164\164\240\240\240\255\255\255\255\255\255\255\255\255\164\164\163\148\149\147\157\159\159\212\214\215\178\179\179\199\200\199zywbcbuwv\187\187\187ihfYYX*('EDAaa_\189\189\187\132\131\128\146\146\144\255\255\255\255\255\255\255\255\255\254\254\254\240\240\240'&#[XQ30*\172\172\170wvu*)&\182\182\183\244\244\243\252\252\252\252\252\252\203\202\200\145\144\142qpo\201\201\202\204\204\203\249\250\250\184\184\183{zxEDC___lkgTTU&$#llj\177\178\175\241\242\242\171\172\169\254\254\254\255\255\255\255\255\255\254\254\254\252\252\251^][^\\VURLuuq\021\019\019\023\022\020\135\136\133kih\176\175\172gggUTU*+*1/._\\W$#\"EDB.,)WSOTNL\\XVc`\\MKH \031\029--*\020\018\017,+*\250\250\250\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\144\145\141'&$YVSFD@CD?\n\t\007\244\244\243\254\254\254\180\180\179776995TPO%##2.,764_][\139\138\136553\018\017\014\028\027\024A@<)(&FD?\027\027\023\b\007\006\199\200\199\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\191\191\188\027\025\022]YTB><\185\185\184HFD\250\251\251\255\255\255\255\255\255\170\170\168\011\011\n?=;('%\027\025\02152010.?=;\141\140\140\017\015\r,+(_^Z%$!\025\023\021\021\021\020\190\191\188\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\024\024\020URLHFB\217\217\216\030\029\028\251\251\251\255\255\255\254\254\254\255\255\255\179\179\178\030\029\028\030\028\026=;9svr\129\129\128KJI#\" WXTutr\022\024\022\007\007\006LLJ\220\219\217\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\143\142\14231,PNI\161\160\157/.+\225\225\225\255\255\255\254\254\254\255\255\255\255\255\255\210\212\210SSQ\025\024\022-+);:7?>?++)=;8WXT]\\[\205\205\202\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\199\200\198\023\022\017mkf\135\135\132\011\t\b\158\158\155\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\221\221\220\177\175\173}}z\145\146\142\187\185\185\211\213\211\253\253\252\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\244\244\243'&\"mke\146\146\143\023\020\019LKJ\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255iieJIC\174\173\172LKHlli\219\220\219\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255cba=;7XXU\160\160\158GFC\134\133\132\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255lkg\158\159\156\242\242\243\255\255\255\255\255\255\255\255\255ded$\"\030\197\196\196\176\175\173\158\157\155KKI\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\151\150\148\169\168\165\176\177\176\132\132\128\158\159\156\145\144\143\132\132\131/.*XVSPNK\241\241\241\249\249\249dec\248\248\248\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\230\230\228\195\195\193\156\154\150\158\158\156\137\137\134\169\168\166\138\134\13021/\237\235\233VTO\253\253\253\254\254\254JIG\245\245\245\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\159\160\158\162\160\1550.,SRMfeb\169\169\165\220\220\218\254\254\254\249\249\248\028\027\024\245\245\245\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\238\239\238[ZXGE?IGA\183\183\180\253\253\253\253\253\253\224\225\224RRN\247\247\246\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\216\216\214\018\015\rgd`xws\210\210\207\211\212\210TSP\164\164\161\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\171\172\170\"!\030QOK[XQ\027\026\022utq\253\253\253\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\216\216\215\154\152\152\147\147\145\216\216\215\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"


let base64_encode s0=
  let m=String.length s0 mod 3 in
  let s=
    if m=1 then (s0^String.make 2 (char_of_int 0)) else
      if m=2 then (s0^String.make 1 (char_of_int 0)) else s0
  in
  let buf=Buffer.create (String.length s*2) in
  let base64 x=
    let y=x land 0x3f in
    if y<26 then (char_of_int (y+int_of_char 'A')) else
      if y<52 then (char_of_int (y-26+int_of_char 'a')) else
        if y<62 then (char_of_int (y-52+int_of_char '0')) else
          if y=62 then '+' else '/'
  in
  let rec encode i=
    if i<=String.length s-3 then (
      let a=int_of_char s.[i]
      and b=int_of_char s.[i+1]
      and c=int_of_char s.[i+2]
      in
      let x=(((a lsl 8) lor b) lsl 8) lor c in
      Buffer.add_char buf (base64 (x lsr 18));
      Buffer.add_char buf (base64 (x lsr 12));
      Buffer.add_char buf (base64 (x lsr 6));
      Buffer.add_char buf (base64 x);
      encode (i+3)
    )
  in
  encode 0;
  let str=Buffer.contents buf in
  if m>=1 then str.[String.length str-1]<-'=';
  if m=1 then str.[String.length str-2]<-'=';
  str


type presentation={ mutable cur_slide:int; mutable cur_state:int; mutable starttime:float;mutable touched:bool }
let present={cur_slide=0;cur_state=0;starttime=0.;touched=false}

let mut=Mutex.create () (* protect the websocket map *)
let mut2=Mutex.create () (* protect the sock_info and alike pointer for connection between patonet/driverGL*)
let mut3=Mutex.create () (*protext Str !!!*)

module AddrMap=Map.Make(struct type 
  t=string let compare=compare end)
let addrs:Unix.file_descr AddrMap.t ref=ref AddrMap.empty

exception Send_error
(* Implémentation partielle, et sans doute naive, des websockets *)
let resp_slave fd data=
  let pos=ref 0 in
  let packet_len=256 in
  let x=Buffer.create (packet_len+10) in
  while !pos<String.length data do
    Buffer.clear x;
    let fin=if String.length data <= !pos+packet_len then 1 else 0 in
    let rsv1=0 and rsv2=0 and rsv3=0 in
    let opcode=if !pos=0 then 0x1 else 0x0 in
    let c0=(fin lsl 7) lor (rsv1 lsl 6) lor (rsv2 lsl 5) lor (rsv3 lsl 4) lor opcode in
    Buffer.add_char x (char_of_int c0);

    let mask=[|0;0;0;0|] in
    (* mask.(0)<-Random.int 0x100; *)
    (* mask.(1)<-Random.int 0x100; *)
    (* mask.(2)<-Random.int 0x100; *)
    (* mask.(3)<-Random.int 0x100; *)

    let payload_len=min (String.length data - !pos) packet_len in
    if payload_len<=125 then (
      Buffer.add_char x (char_of_int (payload_len));
    ) else if payload_len <= 0xffff then (
      Buffer.add_char x (char_of_int 126);
      Buffer.add_char x (char_of_int (payload_len lsr 8));
      Buffer.add_char x (char_of_int (payload_len land 0xff))
    ) else (
      Buffer.add_char x (char_of_int 127);
      Buffer.add_char x (char_of_int ((payload_len lsr 56) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 48) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 40) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 32) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 24) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 16) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 8) land 0xff));
      Buffer.add_char x (char_of_int (payload_len land 0xff))
    );
    (* Buffer.add_char x (char_of_int mask.(0)); *)
    (* Buffer.add_char x (char_of_int mask.(1)); *)
    (* Buffer.add_char x (char_of_int mask.(2)); *)
    (* Buffer.add_char x (char_of_int mask.(3)); *)

    for i= !pos to !pos+payload_len-1 do
      Buffer.add_char x (char_of_int (int_of_char data.[i] lxor mask.((i- !pos) land 3)))
    done;
    (* Buffer.add_substring x data !pos payload_len; *)
    let s=Buffer.contents x in
    Printf.fprintf stderr "select\n";flush stderr;
    let _,x,y=Unix.select [] [fd] [fd] 0. in
    Printf.fprintf stderr "/select %d %d\n" (List.length x) (List.length y);flush stderr;
    if x=[] then (Unix.close fd;raise Send_error) else (
      let cur = ref 0 and len = String.length s in
      while !cur < len do
	let n=Unix.write fd s !cur (len - !cur) in
	cur := !cur + n
      done; 
      pos:= !pos+packet_len
    )
  done

let sock_info = ref None
let sync_sock = ref []
let master_page=ref ""
let port_num = ref 8080
let connect_to = ref ""

let str_addr addr = Unix.(match addr with
    ADDR_UNIX s -> s
  | ADDR_INET(a,p) -> string_of_inet_addr a^":"^string_of_int p)

let pushto ?(change=false) a=
  let time=Unix.time() in
  resp_slave a (
    Printf.sprintf "{ \"slide\":%d, \"state\":%d, \"time\":%g, \"change\":%b }" 
      present.cur_slide present.cur_state 
      (if present.starttime=0. then 0. else (time-.present.starttime))
      change)

let pushfrom ?(change=false) ?from () = (*from avoids to send back to the expeditor*)
  let rec fn acc = function
  [] -> acc
  | (sessid,sock,fo,fi as s)::rest ->
    Printf.fprintf stderr "Sending to sync(%d): %d %d\n%!" (List.length rest) present.cur_slide present.cur_state;
    try 
      if Some sessid <> from then
	Printf.fprintf fo "GET /sync_%d_%d HTTP/1.1\r\n\r\n%!" present.cur_slide present.cur_state;
      fn (s::acc) rest
    with e ->
      Printf.fprintf stderr "not synced (%s)\n%!" (Printexc.to_string e);
      (try Unix.close sock with _ -> ());
      (match !sock_info with
	Some(_,s,_,_) when s = sock -> sock_info := None;
      | _ -> ());
      fn acc rest
  in
  Mutex.lock mut2;
  sync_sock := fn [] !sync_sock;
  Mutex.unlock mut2;
  addrs:=AddrMap.fold (fun k a m->
    try
      if Some k <> from then (
	Printf.fprintf stderr "pushing %s %s\n%!" k (match from with None -> "?" | Some s -> s); 
	pushto ~change a;
	Printf.fprintf stderr "pushed\n%!");
      AddrMap.add k a m
    with
      e->
	Printf.fprintf stderr "not pushed (%s)\n%!" (Printexc.to_string e);
	m
  ) !addrs AddrMap.empty

let svg=Str.regexp "/\\([0-9]*\\)_\\([0-9]*\\)\\.svg"
let css_reg=Str.regexp "/style\\.css"
let pousse=Str.regexp "/pousse_\\([0-9]*\\)_\\([0-9]*\\)"
let click=Str.regexp "/click_\\([0-9]*\\)_\\([0-9]*\\)_"
let tire=Str.regexp "/tire_\\([0-9]*\\)_\\([0-9]*\\)"
let sync=Str.regexp "/sync_\\([0-9]*\\)_\\([0-9]*\\)"
let otf=Str.regexp "/\\([^\\.]*\\.otf\\)"

let get_reg=Str.regexp "GET \\([^ ]*\\) .*"
let header=Str.regexp "\\([^ :]*\\) *: *\\([^\r]*\\)"
let phpsess=Str.regexp "Cookie\\(s?\\):[ \t]*PHPSESSID[ \t]*=[ \t]*\\([0-9a-zA-Z]*\\)[ \t]*;"

exception Websocket

let spec=
  [("--master",Arg.Set_string master_page,"Set the master page");
   ("--port",Arg.Set_int port_num,"Set the port number to listen to");
   ("--connect",Arg.Set_string connect_to,"Connect to another Patonet")]

let websocket is_master w=
  Printf.sprintf "var websocket;var was_error;
function websocket_msg(evt){
     var st=JSON.parse(evt.data);
     loadSlide(st.slide,st.state,st.change);
     current_slide=st.slide;
     current_state=st.state;
};
function websocket_err(evt){
was_error=true;
websocket.close();
start_socket();
};
function websocket_close(evt){};
function start_socket(){
   was_error=false;
   if(websocket){websocket.close();delete websocket.onclose;delete websocket.onmessage;delete websocket.onerror};
   if(location.protocol==\"https:\")
      websocket=new WebSocket(\"wss://\"+location.host+\"/tire\"%s);
   else
      websocket=new WebSocket(\"ws://\"+location.host+\"/tire\"%s);
   websocket.onclose=websocket_close;
   websocket.onmessage = websocket_msg;
   websocket.onerror = websocket_err;
};
window.onbeforeunload = function() {
    websocket.onclose = function () {}; // disable onclose handler first
    websocket.close()
};
"
    (if is_master then "+\"_\"+current_slide+\"_\"+current_state" else "")
    (if is_master then "+\"_\"+current_slide+\"_\"+current_state" else "")


let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let prefix=try Filename.chop_extension fileName with _->fileName in

  let dynCache = Array.map (fun t -> Array.map (fun _ -> Hashtbl.create 13) t) pages in

  let slides,cache,imgs=SVG.buffered_output' ~structure:structure ~dynCache pages prefix in

(*  let imgs = StrMap.add "favicon.ico" duck_ico imgs in*)

  let slave,css=SVG.basic_html
    ~script:(websocket false (fst (pages.(0)).(0).pageFormat))
    ~onload:"start_socket();"
    ~keyboard:""
    cache structure pages prefix
  in

  let master_keyboard=Printf.sprintf "window.onkeydown=function(e){
if(e.keyCode==37 || e.keyCode==38 || e.keyCode==33){
if(current_slide > 0 && (current_state<=0 || e.keyCode==38)) {
  var xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide-1)+\"_\"+(states[current_slide-1]-1),false);
  xhttp.send();
} else if (current_state > 0) {
  var xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide)+\"_\"+(current_state-1),false);
  xhttp.send();
}
} //left
if(e.keyCode==39 || e.keyCode==40 || e.keyCode==34){
if(current_slide < %d && (current_state>=states[current_slide]-1 || e.keyCode==40)) {
  var xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide+1)+\"_0\",false);
  xhttp.send();
} else if (current_state < states.length - 1) {
  var xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide)+\"_\"+(current_state+1),false);
  xhttp.send();
}
} else //right
if(e.keyCode==82){ //r
  var xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide)+\"_\"+(current_state),false);
  xhttp.send();
}
}
function send_click(name,dest) {
  var xhttp=new XMLHttpRequest();
  var message = name;
  for(var i = 0;i<dest.length;i++) { message = message+\"_\"+dest[i] ; }
  xhttp.open(\"GET\",\"click_\"+(current_slide)+\"_\"+(current_state)+\"_\"+message,false);
  xhttp.send();
}
function gotoSlide(n){
  var xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+n+\"_0\",false);
  xhttp.send();
}" (Array.length pages - 1)
  in


  let master,_=SVG.basic_html
    ~script:(websocket true (fst (pages.(0)).(0).pageFormat))
    ~onload:"to=0;start_socket();"
    ~onhashchange:"xhttp=new XMLHttpRequest();xhttp.open(\"GET\",\"pousse_\"+h0+\"_\"+h1,false);xhttp.send();"
    ~keyboard:master_keyboard
    cache structure pages prefix
  in

  let slides = Array.map (Array.map (fun (x,y) -> Rbuffer.contents x, Rbuffer.contents y)) slides in
  let master = Rbuffer.contents master in
  let slave = Rbuffer.contents slave in
  let css = Rbuffer.contents css in
  Printf.fprintf stderr "%s\n" css;

let http_send ?(cookie=true) code ctype datas sessid ouc = 
  Printf.fprintf ouc "HTTP/1.1 %d OK\r\n" code;
  Printf.fprintf ouc "Content-type: %s\r\n" ctype;
  let len = List.fold_left (fun acc s -> acc + String.length s) 0 datas in
  Printf.fprintf ouc "Content-Length: %d\r\n" len;
  if cookie then Printf.fprintf ouc "Set-Cookie: SESSID=%s;\r\n" sessid;
  output_string ouc "\r\n";
  List.iter (output_string ouc) datas;
  output_string ouc "\r\n";
  flush ouc
in

let generate_error sessid ouc =
  let data =
    "<html><head><title>Patoline</title></head><body>Patoline n'a malheureusement pas pu satisfaire votre demande</body></html>"
  in
  Printf.fprintf stderr "sent 404\n%!";
  http_send 404 "text/html" [data] sessid ouc;
in
 
let serve_svg i j sessid ouc =
  if i<Array.length slides && j<Array.length slides.(i) then (
    let prefix,suffix = slides.(i).(j) in
    let dyns = Rbuffer.create 256 in
    Hashtbl.iter (fun k d ->
      Rbuffer.add_string dyns (Printf.sprintf "<g id=\"%s\">%s</g>" d.dyn_label (d.dyn_contents ()))) dynCache.(i).(j);
    let dyns = Rbuffer.contents dyns in
    Printf.fprintf stderr "sent image/svg+xml\n%!";
    http_send 200 "image/svg+xml" [prefix; dyns; suffix] sessid ouc;
  ) else (
    generate_error sessid ouc;
  )
in

let fonts = StrMap.fold (fun key font acc ->
(*  Printf.fprintf stderr "Font: %S\n%!" key;*)
  let key = List.hd (List.rev (Util.split '/' key)) in
  StrMap.add key (Rbuffer.contents font) acc) cache.fontBuffers StrMap.empty
in

let serve_font font sessid ouc=
  try
    Printf.fprintf stderr "Search Font: %S\n%!" font;
    let data= StrMap.find font fonts in
    http_send 200 "font/opentype" [data] sessid ouc;
  with
    Not_found->generate_error sessid ouc
in


let serve_css sessid ouc=
  http_send ~cookie:false 200 "text/css" [css] sessid ouc;
in

let make_sessid () = 
  let str = String.create 32 in
  for i = 0 to 21 do
    let c = Random.int 62 in
    let d = 
      if c < 26 then Char.chr (c + Char.code 'a')
      else if c < 52 then Char.chr (c - 26 + Char.code 'A')
      else Char.chr (c - 52 + Char.code '0')
    in
    str.[i] <- d;
  done;
  str
in

let serve ?sessid num fd=
  Unix.clear_nonblock fd;
  let inc=Unix.in_channel_of_descr fd in
  let ouc=Unix.out_channel_of_descr fd in
  let sessid = ref sessid in
  let rec process_req get hdr reste=
    let x=input_line inc in
    Printf.fprintf stderr "serve %d: %S %S\n%!" num get x;
    Mutex.lock mut3;
    if x.[0]='\r' then (
      let sessid = match !sessid with
	| Some s -> 
	  Printf.fprintf stderr "Reuse sessid: %s\n%!" s;
	  s
	| None -> 
	  let s = make_sessid () in
	  Printf.fprintf stderr "New sessid: %s\n%!" s;
	  s
      in
      if Str.string_match svg get 0 then (
	Printf.fprintf stderr "serve %d: get %S\n%!" num get;
        let i=int_of_string (Str.matched_group 1 get) in
        let j=int_of_string (Str.matched_group 2 get) in
        Mutex.unlock mut3;
        Mutex.lock mut;
	Printf.fprintf stderr "serve %d: get mutex lock\n%!" num;
        let pi=present.cur_slide and pj=present.cur_state in
        Mutex.unlock mut;
	Printf.fprintf stderr "serve %d: get mutex unlock\n%!" num;
        if i<pi || (i=pi && j<=pj) then
          serve_svg i j sessid ouc
        else
          generate_error sessid ouc;
        process_req "" [] reste

      ) else if get= !master_page then (
        Mutex.unlock mut3;
	Printf.fprintf stderr "serve %d: master\n%!" num;	
	http_send 200 "text/html" [master] sessid ouc;
        process_req "" [] reste

      ) else if get="/" then (
        Mutex.unlock mut3;
	Printf.fprintf stderr "serve %d: slave\n%!" num;
	http_send 200 "text/html" [slave] sessid ouc;
        process_req "" [] reste

      ) else if get="/etat" then (
        Mutex.unlock mut3;
	Printf.fprintf stderr "serve %d: etat\n%!" num;
        let data=Buffer.create 1000 in
        Buffer.add_string data "{\"slides\"=[";
        for i=0 to Array.length slides-1 do
          if i>0 then Buffer.add_char data ',';
          Buffer.add_string data (Printf.sprintf "%d" (Array.length slides.(i)));
        done;
        Buffer.add_string data "],";
        Buffer.add_string data (Printf.sprintf "\"slide\"=%d," present.cur_slide);
        Buffer.add_string data (Printf.sprintf "\"state\"=%d," present.cur_state);
        let t=
          let time=Unix.time() in
          if present.starttime=0. then 0. else (time-.present.starttime)
        in
        Buffer.add_string data (Printf.sprintf "\"time\"=%g" t);
        Buffer.add_char data '}';

	http_send 200 "text/plain" [Buffer.contents data] sessid ouc;
        process_req "" [] reste

      ) else if Str.string_match sync get 0 then (
        let asked_slide=max 0 (int_of_string (Str.matched_group 1 get)) in
        let state=max 0 (int_of_string (Str.matched_group 2 get)) in
        Mutex.unlock mut3;
	Printf.fprintf stderr "serve %d: sync\n%!" num;
	Mutex.lock mut2;
	Printf.fprintf stderr "serve %d: sync mutex2 lock\n%!" num;
	if not (List.mem (sessid,fd,ouc,inc) !sync_sock) then
	  sync_sock := (sessid,fd,ouc,inc)::!sync_sock;
	Mutex.unlock mut2;
	Printf.fprintf stderr "serve %d: sync mutex2 unlock\n%!" num;

        let slide=min asked_slide (Array.length slides-1) in
        let state=if asked_slide>slide then
            (Array.length slides.(slide)-1)
          else
            min state (Array.length slides.(slide)-1)
        in
        Mutex.lock mut;
	Printf.fprintf stderr "serve %d: sync mutex lock\n%!" num;
        (try
           if present.cur_slide<>slide || present.cur_state<>state then (
             present.cur_slide<-slide;
             present.cur_state<-state;
             present.touched<-true;
             if present.starttime=0. && (present.cur_slide>0 || present.cur_state>0) then
               present.starttime<-Unix.time();
             pushfrom ~from:sessid ();
           );
         with _->());
        Mutex.unlock mut;
	Printf.fprintf stderr "serve %d: sync mutex unlock\n%!" num;
        process_req "" [] reste
      ) else if Str.string_match tire get 0 || get="/tire" then (
        let asked_slide=max 0 (int_of_string (Str.matched_group 1 get)) in
        let state=max 0 (int_of_string (Str.matched_group 2 get)) in
        Mutex.unlock mut3;

	Printf.fprintf stderr "serve %d: tire\n%!" num;
        try
          Printf.fprintf stderr "pushing\n";flush stderr;
          begin
            let key=
              let websocket_key=List.assoc "Sec-WebSocket-Key" hdr in
              let sha=Cryptokit.Hash.sha1 () in
              sha#add_string websocket_key;
              sha#add_string "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
              base64_encode (sha#result)
            in
            output_string ouc "HTTP/1.1 101 Switching\r\nUpgrade: websocket\r\nConnection: upgrade\r\nSec-WebSocket-Accept: ";
            output_string ouc key;
            output_string ouc "\r\n\r\n";
            flush ouc;
          end;

          Mutex.lock mut;
	  Printf.fprintf stderr "serve %d: tire mutex lock\n%!" num;
          (try
             if get<>"/tire" && not present.touched then (
               let slide=min asked_slide (Array.length slides-1) in
               let state=if asked_slide>slide then
                   (Array.length slides.(slide)-1)
                 else
                   min state (Array.length slides.(slide)-1)
               in
               if present.cur_slide<>slide || present.cur_state<>state then (
                 present.cur_slide<-slide;
                 present.cur_state<-state;
                 present.touched<-true;
                 if present.starttime=0. && (present.cur_slide>0 || present.cur_state>0) then
                   present.starttime<-Unix.time();
               );
             );

             pushto fd;
             addrs:=AddrMap.add sessid fd !addrs;
           with
               _->());
          Mutex.unlock mut;
	  Printf.fprintf stderr "serve %d: tire mutex unlock\n%!" num;
          raise Websocket
        with
            Not_found->(
              Mutex.lock mut;
              Printf.fprintf stderr "pushing\n";flush stderr;
              pushto fd;
              addrs:=AddrMap.add sessid fd !addrs;
	      Mutex.unlock mut;
              raise Websocket
            )
          | Websocket->raise Websocket
          | e->(Printf.fprintf stderr "erreur websocket \"%s\"\n"(Printexc.to_string e);
                flush stderr)
      ) else if Str.string_match click get 0 then (
	Printf.fprintf stderr "serve %d: click\n%!" num;
        let asked_slide=max 0 (int_of_string (Str.matched_group 1 get)) in
        let state=max 0 (int_of_string (Str.matched_group 2 get)) in
	let match_end = Str.match_end () in
        Mutex.unlock mut3;

        let slide=min asked_slide (Array.length slides-1) in
        let state=if asked_slide>slide then
            (Array.length slides.(slide)-1)
          else
            min state (Array.length slides.(slide)-1)
        in
	let rest =String.sub get match_end (String.length get - match_end) in

	Printf.fprintf stderr "click: %d %d %s\n%!" slide state rest;

	let name, dest = match Util.split '_' rest with
	  name::dest -> name,dest
	| _ -> failwith "Bad click"
	in
	let status =
	  List.fold_left (fun acc ds ->
	    try
	      let d = Hashtbl.find dynCache.(slide).(state) ds in
	      let res = d.dyn_react (Click(name)) in
	      max res acc
	    with
	      Not_found -> 
		Printf.fprintf stderr "Warning: dynamic not found: %s\n%!" ds;
		acc) Unchanged dest
	in
        let ok="Ok" in
        Printf.fprintf ouc "HTTP/1.1 200 OK\r\nContent-length: %d\r\n\r\n%s\r\n"
          (String.length ok) ok;
        flush ouc;

	(match status with
	  Unchanged -> Printf.fprintf stderr "Unchanged\n%!"; ()
	| Private -> Printf.fprintf stderr "Private change\n%!"; pushto ~change:true fd
	| Public -> Printf.fprintf stderr "Public change\n%!"; pushfrom ~change:true ());

        process_req "" [] reste

      ) else if Str.string_match pousse get 0 then (
        Printf.fprintf stderr "pousse\n";flush stderr;
        let asked_slide=max 0 (int_of_string (Str.matched_group 1 get)) in
        let state=max 0 (int_of_string (Str.matched_group 2 get)) in
        Mutex.unlock mut3;

        let slide=min asked_slide (Array.length slides-1) in
        let state=if asked_slide>slide then
            (Array.length slides.(slide)-1)
          else
            min state (Array.length slides.(slide)-1)
        in
        Mutex.lock mut;
	Printf.fprintf stderr "serve %d: pousse mutex lock\n%!" num;
        (try
           if present.cur_slide<>slide || present.cur_state<>state then (
             present.cur_slide<-slide;
             present.cur_state<-state;
             present.touched<-true;
             if present.starttime=0. && (present.cur_slide>0 || present.cur_state>0) then
               present.starttime<-Unix.time();
           );
           pushfrom ();
         with _->());
        Mutex.unlock mut;
	Printf.fprintf stderr "serve %d: pousse mutex unlock\n%!" num;
        let ok="Ok" in
        Printf.fprintf ouc "HTTP/1.1 200 OK\r\nContent-length: %d\r\n\r\n%s\r\n"
          (String.length ok) ok;
        flush ouc;

        process_req "" [] reste

      ) else if Str.string_match css_reg get 0 then (
        Mutex.unlock mut3;

	Printf.fprintf stderr "serve %d: css\n%!" num;
        serve_css sessid ouc;
        process_req "" [] reste

      ) else if Str.string_match otf get 0 then (
	let otf = Str.matched_group 1 get in
        Mutex.unlock mut3;

	Printf.fprintf stderr "serve %d: otf\n%!" num;
        serve_font otf sessid ouc;
        process_req "" [] reste

      ) else (
        Mutex.unlock mut3;

	Printf.fprintf stderr "serve %d: img\n%!" num;
        try
          let img=StrMap.find (String.sub get 1 (String.length get-1)) imgs in
          let ext=
            if Filename.check_suffix ".png" get then "image/png" else
              if Filename.check_suffix ".jpeg" get then "image/jpeg" else
                if Filename.check_suffix ".jpg" get then "image/jpg" else
                  if Filename.check_suffix ".gif" get then "image/gif" else
                    if Filename.check_suffix ".ico" get then "image/vnd.microsoft.icon" else
                    "application/octet-stream"
          in
 
	  http_send 200 ext [base64_encode img] sessid ouc;
          process_req "" [] []
        with
            Not_found->(
	      generate_error sessid ouc;
              process_req "" [] reste);
      )

    ) else (

      if hdr=[] && Str.string_match get_reg x 0 then (
	let str = Str.matched_group 1 x in
	Mutex.unlock mut3;
        process_req str hdr reste
      ) else if Str.string_match header x 0 then (
        let a=Str.matched_group 1 x in
        let b=Str.matched_group 2 x in
	Mutex.unlock mut3;
	if a = "Cookie" || a = "Cookies" then
	  let ls = Util.split '=' b in
	  (match ls with
	    [_;s] -> sessid := Some s
	  | _ -> ());
          process_req get hdr reste
	else
          process_req get ((a,b)::hdr) reste
      ) else (
	Mutex.unlock mut3;
        process_req get hdr (x::reste)
      );
    )
  in
  try
    process_req "" [] []
  with
      Websocket-> (Mutex.unlock mut3; Mutex.unlock mut2; Mutex.unlock mut; Printf.fprintf stderr "End process req\n";flush stderr)
    | e->(Mutex.unlock mut3; Mutex.unlock mut2; Mutex.unlock mut; Unix.close fd;
          Printf.fprintf stderr "erreur : \"%s\"\n" (Printexc.to_string e);flush stderr)
in

(*connection as another patonet (at most one is enough to build
any graph) as a follower*)

let reconnect sock_info =
  assert (!sock_info = None);
  match !connect_to with
    "" -> ()
  | server ->
    try
      let ls = Util.split ':' server in
      let server,port = match ls with
	  [s] -> s, 8080
	| [s;p] -> s, int_of_string p
	| _ -> raise Exit
      in
      let addrs = 
	Unix.(getaddrinfo server (string_of_int port) [AI_SOCKTYPE SOCK_STREAM])
      in
      let rec fn = function
      [] -> 
	Printf.fprintf stderr "Failed to connect to Patonet server\n%!";
	raise Exit
	| addr::rest -> Unix.(
	  Printf.fprintf Pervasives.stderr "Trying connect to %s:%d\n%!"
	    (str_addr addr.ai_addr) port;
	  let sock= socket addr.ai_family addr.ai_socktype 0 in
	  try 
	    connect sock addr.ai_addr;
	    addr.ai_addr, sock
	  with _ -> fn rest)
      in
      let addr, sock = fn addrs in
      let fo=Unix.out_channel_of_descr sock in
      let fi=Unix.in_channel_of_descr sock in
      Printf.fprintf stderr "Connected\n%!";
      Mutex.lock mut2;
      let sessid = make_sessid () in
      sock_info := Some (sessid,sock,fo,fi);
      sync_sock := (sessid,sock,fo,fi)::!sync_sock;
      Mutex.unlock mut2;
      let _=Thread.create (fun ()->
	serve ~sessid (-1) sock;
	Printf.fprintf Pervasives.stderr
	  "Reconnect served\n%!";) () in
      ()
    with _ -> ()
in

  Arg.parse spec (fun x->()) "";
  if !master_page="" then (
    Random.self_init ();
    master_page:=Printf.sprintf "/%d" (Random.int (1 lsl 29));
  );
  if !master_page.[0]<>'/' then master_page:="/"^(!master_page);

  ignore (Sys.signal 13 (Sys.Signal_ignore));

  while true do
    Printf.fprintf stderr "upper main loop\n%!";
    try Unix.(
      let port = !port_num in
      let addrs = getaddrinfo "" (string_of_int port) [AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE] in
      let rec fn acc = function
        [] -> if acc = [] then (
	  Printf.fprintf Pervasives.stderr
	    "Failed to listen on any address.\n%!";
	  exit 1)
	  else acc
	| addr::rest ->
	  try
(*	    Printf.fprintf Pervasives.stderr
	      "Trying to listen from %s:%d\n%!" str_addr port;*)
	    let master_sock= socket addr.ai_family addr.ai_socktype 0 in
	    setsockopt master_sock SO_REUSEADDR true;
	    bind master_sock addr.ai_addr;
	    listen master_sock 100;
	    Printf.fprintf Pervasives.stderr
	      "Listening on port %s:%d\n%!" (str_addr addr.ai_addr) port;
	    fn (master_sock::acc) rest
	  with _ -> fn acc rest
      in

      let master_sockets = fn [] addrs in

      Printf.fprintf Pervasives.stderr 
	"Listening from %d addresses -- master: \"%s\"\n%!"
	(List.length master_sockets) !master_page;

      let conn_num = ref 0 in

      while true do
	Printf.fprintf Pervasives.stderr "in main loop\n%!";
	if !connect_to <> "" && !sock_info = None then
	  reconnect sock_info;
	let socks,_,_=Unix.select master_sockets [] [] 10. in
	List.iter (fun master_sock ->
	  try
            let conn_sock, addr = Unix.accept master_sock in
	    Unix.set_nonblock conn_sock;
	    let num = !conn_num in
	    incr conn_num;
            let _=Thread.create (fun ()->
	      Printf.fprintf Pervasives.stderr "Connection started: %d\n%!" num;
	      serve num conn_sock;
	      Printf.fprintf Pervasives.stderr "Connection served: %d\n%!" num;) () in
	    ()
          with _ -> ()) socks
      done)

    with
        e-> Printf.fprintf stderr "main loop: %s\n%!"
	  (Printexc.to_string e)
  done

let output = output_from_prime output'

let _ = 
  Hashtbl.add drivers "Patonet" (module struct let output = output let output' = output' end:Driver)

