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
open Db

let _ = Printexc.record_backtrace true

let duck_ico =
"\000\000\001\000\001\000  \000\000\001\000\024\000\168\012\000\000\022\000\000\000(\000\000\000 \000\000\000@\000\000\000\001\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\244\245\245\250\250\250\239\240\241\239\239\239\246\246\246\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255HGF*('PPOHHF\221\221\221\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\253\253\253\254\254\254\254\254\254\249\249\249\247\247\247\247\246\245\177\176\175\127~}``^\248\249\248\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\253\253\253\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\242\243\244\174\176\175\175\174\174\138\138\137\129\130\131\132\134\134\231\232\235\255\255\255\255\255\255lli^][\235\235\234\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\229\230\230\166\167\167{{xWVSfda^[X.-*\030\029\0260-,(% SSPGGEtro\253\253\253\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\253\253\253\179\180\178\130\129~CA;1/(C>90-'fd_MLIGFCGFD986ed`\\ZX\240\241\240\174\173\173\173\173\172\229\229\228\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\246\246\246stq\138\137\134[WTGE@MJETRLOKF\150\148\144\022\024\023:75\026\024\0202/-75.\238\237\237\241\241\241\201\201\201\195\194\194432\154\155\153\227\228\227\255\255\255\255\255\255\227\227\227\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\252\252\252z{w30,PMGCB?bc]ec`>=9JGDVSQ\012\011\011)%##!\031\175\172\172^^Z\217\218\218\144\144\143\152\152\149665\162\162\162\154\153\154\195\195\194\148\150\149\233\234\234xxv\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\199\198\198\018\016\rGD?41-A=9\014\012\n^\\Z\021\021\020\225\224\224\205\205\203\006\005\004\020\019\01641.ZYX)(#]\\Z>>=\192\192\192\129\128\128POOrrr\254\254\254\237\237\237qom\194\195\195\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255igc>;5\245\245\244\017\015\011+)%\207\206\204\170\168\166\n\n\t=<:??>wtr\011\n\t84.<82\153\151\149\225\225\224{zy]]\\\150\149\149fec\144\145\143\255\255\255\142\143\141\158\159\157CBB\187\186\186\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\240\240\239875VTL\027\026\023\201\200\197\"!\031\015\015\rkjg\014\r\011 \030\028TOK\204\206\205\140\141\139\138\137\137322\233\232\232hihyywOMMdecllj\\[Z\160\161\161FECMLM./-ffe\240\240\241\255\255\255\254\254\254\255\255\255\254\254\254\234\234\234\006\005\001e`ZQNI('$\165\165\164\000\000\000sqn\227\227\225~}z\238\239\238\158\159\157\151\150\149\143\143\14310/\172\172\171RQPLLI```ZYXlkiba`676A@?QPN\157\156\154\137\137\135\152\152\150\255\255\255\254\254\254\254\254\254\254\254\254\229\229\228\017\016\011fd\\2/*\205\204\203\231\230\229\152\151\149\150\147\145\192\193\192\173\173\171\197\197\198\156\156\154\164\165\163\159\159\157EEC\160\160\158TSR``_\127}|fecnnl\\\\[xxvjjj\127\127}))'HGF\144\143\141\254\254\254\255\255\255\255\255\255\255\255\255\233\233\232\014\011\007b_W51+\231\229\229\163\164\164\240\240\240\255\255\255\255\255\255\255\255\255\164\164\163\148\149\147\157\159\159\212\214\215\178\179\179\199\200\199zywbcbuwv\187\187\187ihfYYX*('EDAaa_\189\189\187\132\131\128\146\146\144\255\255\255\255\255\255\255\255\255\254\254\254\240\240\240'&#[XQ30*\172\172\170wvu*)&\182\182\183\244\244\243\252\252\252\252\252\252\203\202\200\145\144\142qpo\201\201\202\204\204\203\249\250\250\184\184\183{zxEDC___lkgTTU&$#llj\177\178\175\241\242\242\171\172\169\254\254\254\255\255\255\255\255\255\254\254\254\252\252\251^][^\\VURLuuq\021\019\019\023\022\020\135\136\133kih\176\175\172gggUTU*+*1/._\\W$#\"EDB.,)WSOTNL\\XVc`\\MKH \031\029--*\020\018\017,+*\250\250\250\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\144\145\141'&$YVSFD@CD?\n\t\007\244\244\243\254\254\254\180\180\179776995TPO%##2.,764_][\139\138\136553\018\017\014\028\027\024A@<)(&FD?\027\027\023\b\007\006\199\200\199\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\191\191\188\027\025\022]YTB><\185\185\184HFD\250\251\251\255\255\255\255\255\255\170\170\168\011\011\n?=;('%\027\025\02152010.?=;\141\140\140\017\015\r,+(_^Z%$!\025\023\021\021\021\020\190\191\188\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\024\024\020URLHFB\217\217\216\030\029\028\251\251\251\255\255\255\254\254\254\255\255\255\179\179\178\030\029\028\030\028\026=;9svr\129\129\128KJI#\" WXTutr\022\024\022\007\007\006LLJ\220\219\217\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\143\142\14231,PNI\161\160\157/.+\225\225\225\255\255\255\254\254\254\255\255\255\255\255\255\210\212\210SSQ\025\024\022-+);:7?>?++)=;8WXT]\\[\205\205\202\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\199\200\198\023\022\017mkf\135\135\132\011\t\b\158\158\155\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\221\221\220\177\175\173}}z\145\146\142\187\185\185\211\213\211\253\253\252\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\244\244\243'&\"mke\146\146\143\023\020\019LKJ\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255iieJIC\174\173\172LKHlli\219\220\219\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255cba=;7XXU\160\160\158GFC\134\133\132\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255lkg\158\159\156\242\242\243\255\255\255\255\255\255\255\255\255ded$\"\030\197\196\196\176\175\173\158\157\155KKI\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\151\150\148\169\168\165\176\177\176\132\132\128\158\159\156\145\144\143\132\132\131/.*XVSPNK\241\241\241\249\249\249dec\248\248\248\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\230\230\228\195\195\193\156\154\150\158\158\156\137\137\134\169\168\166\138\134\13021/\237\235\233VTO\253\253\253\254\254\254JIG\245\245\245\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\159\160\158\162\160\1550.,SRMfeb\169\169\165\220\220\218\254\254\254\249\249\248\028\027\024\245\245\245\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\238\239\238[ZXGE?IGA\183\183\180\253\253\253\253\253\253\224\225\224RRN\247\247\246\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\216\216\214\018\015\rgd`xws\210\210\207\211\212\210TSP\164\164\161\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\171\172\170\"!\030QOK[XQ\027\026\022utq\253\253\253\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\216\216\215\154\152\152\147\147\145\216\216\215\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"


type presentation={ mutable cur_slide:int; mutable cur_state:int; mutable starttime:float;mutable max_slide:int; }
let present={cur_slide=0;cur_state=0;starttime=0.;max_slide=0;}

exception Send_error
(* Implémentation partielle, et sans doute naive, des websockets *)
let resp_slave fd data=
  let chunk_size = 1000000 in
  let pos = ref 0 and len = String.length data in
  while !pos < len do
  let x=Buffer.create 128 in
    let size = min (len - !pos) chunk_size in
    let new_pos = !pos + size in
    let fin= if new_pos = len then 1 else 0 in
    let rsv1=0 and rsv2=0 and rsv3=0 in
    let opcode= if !pos = 0 then 0x1 else 0x0 in
    let c0=(fin lsl 7) lor (rsv1 lsl 6) lor (rsv2 lsl 5) lor (rsv3 lsl 4) lor opcode in
    Buffer.add_char x (char_of_int c0);

    let payload_len= size in
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
    Buffer.output_buffer fd x;
    output fd data !pos size;
    flush fd;
    pos := new_pos;
  done

let decode_slave fd =
  let res = Rbuffer.create 256 in
  let rec fn () =
    Printf.eprintf "decode_slave starts\n%!";
    let c = int_of_char (input_char fd) in
    let fin = 0x80 land c <> 0 in
    let _rsv1 = 0x40 land c <> 0in
    let _rsv2 = 0x20 land c <> 0 in
    let _rsv3 = 0x10 land c <> 0 in
    let _opcode = 0x0f land c in
    Printf.eprintf "First char: '%s' %x %d fin:%b rsv1:%b rsv2:%b rsv3:%b opcode:%d\n%!"
      (Char.escaped (char_of_int c)) c c fin _rsv1 _rsv2 _rsv3 _opcode;
    let c0 = int_of_char (input_char fd) in
    let mask = c0 land 0x80 <> 0 and c0 = c0 land 0x7f in
    Printf.eprintf "Second char: %x %d\n%!" c0 c0;
    let length =
      if c0 <= 125 then c0
      else if c0 = 126 then
	let c0 = int_of_char (input_char fd) in
	let c1 = int_of_char (input_char fd) in
	(c0 lsl 8) lor c1
      else if c0 = 127 then
	let c0 = int_of_char (input_char fd) in
	let c1 = int_of_char (input_char fd) in
	let c2 = int_of_char (input_char fd) in
	let c3 = int_of_char (input_char fd) in
	let c4 = int_of_char (input_char fd) in
	let c5 = int_of_char (input_char fd) in
	let c6 = int_of_char (input_char fd) in
	let c7 = int_of_char (input_char fd) in
	(c0 lsl 56) lor (c1 lsl 48) lor (c2 lsl 40) lor (c3 lsl 32) lor 
	  (c4 lsl 24) lor (c5 lsl 16) lor (c6 lsl 8) lor c7
      else 0
    in
    (*    Printf.eprintf "Length : %d, mask : %b\n%!" length mask;*)
    let mask_array = [|0;0;0;0|] in
    if mask then (
      mask_array.(0) <- int_of_char (input_char fd);
      mask_array.(1) <- int_of_char (input_char fd);
      mask_array.(2) <- int_of_char (input_char fd);
      mask_array.(3) <- int_of_char (input_char fd);
    );
    for i = 0 to length - 1 do
      let c = input_char fd in
      let c = char_of_int (int_of_char c lxor mask_array.(i land 0x3)) in
      Rbuffer.add_char res c;
    done;
    if fin then Rbuffer.contents res else fn ()
  in fn ()

let master_page=ref ""
let port_num = ref 8080

let close_all_other sock = ()
(*
  AddrMap.iter (fun _ (s,n) ->
    if s <> sock then Unix.close s) !addrs
*)

let str_addr addr = Unix.(match addr with
    ADDR_UNIX s -> s
  | ADDR_INET(a,p) -> string_of_inet_addr a^":"^string_of_int p)

(*connection as another patonet (at most one is enough to build
any graph) as a follower*)

type son =
  { 
    fd: in_channel; (* connection to the son process *)
    pid: int;
    num: int; (*internal number, just for printing and debugging*)
    served_sock: Unix.file_descr;
    mutable sessid: string option;
    mutable slide:int;
    mutable state:int
  }

let sonsBySock = ref ([]:(Unix.file_descr * son) list)
    
let kill_son sock =
  sonsBySock:=List.filter (fun (fd,son) ->
    if fd = sock then (
      Printf.eprintf "kill son %d %d\n%!" son.num son.pid;
      (try Unix.close fd with _ -> ());
      (try Unix.close son.served_sock with _ -> ());
      (try Unix.kill son.pid 2 with _ -> ()); 
      false) 
    else true
  ) !sonsBySock

let get_reg=Str.regexp "GET \\([^ ]*\\) .*"
let header=Str.regexp "\\([^ :]*\\) *: *\\([^\r]*\\)"

let rmaster=Str.regexp "\\(/[0-9]+\\)\\(#\\([0-9]*\\)_\\([0-9]*\\)\\)?$"
let slave=Str.regexp "/?\\(#\\([0-9]*\\)_\\([0-9]*\\)\\)?$"
let logged=Str.regexp "/?\\([a-zA-Z0-9]+\\)[?]key=\\([a-z0-9]+\\)\\(&group=\\([-a-zA-Z0-9_]+\\)\\)?\\(#\\([0-9]*\\)_\\([0-9]*\\)\\)?$"
let svg=Str.regexp "/\\([0-9]*\\)_\\([0-9]*\\)\\.svg"
let css_reg=Str.regexp "/style\\.css"
let tire=Str.regexp "/tire_\\([0-9]*\\)_\\([0-9]*\\)"
let otf=Str.regexp "/\\([^\\.]*\\.otf\\)"

let move=Str.regexp "move_\\([0-9]*\\)_\\([0-9]*\\)"
let click=Str.regexp "click_\\([0-9]*\\)_\\([0-9]*\\) "
let edit=Str.regexp "edit_\\([0-9]*\\)_\\([0-9]*\\) "
let drag=Str.regexp "drag_\\([0-9]*\\)_\\([0-9]*\\)_\\(-?[0-9.]*\\)_\\(-?[0-9.]*\\) "


let spec=
  [("--master",Arg.Set_string master_page,"Set the master page");
   ("--port",Arg.Set_int port_num,"Set the port number to listen to")]

let websocket  =
  Printf.sprintf "var websocket;
setInterval(function () {
  if (to_refresh) loadSlide(current_slide,current_state,true);
}, 5000);
function websocket_msg(evt){
     var st=JSON.parse(evt.data);
     var ch = st.change;
     if (ch == 'Slide') {
       to_refresh = false;
       var svg = Base64.decode(st.change_list);
       loadSlideString(st.slide,st.state,svg);
     } else if (ch == 'Refresh') {
       if (current_slide == st.slide && current_state == st.state) {
         to_refresh = true;
       }
     } else if (ch && current_slide == st.slide && current_state == st.state) {
     }  
};
function websocket_err(evt){
};
function websocket_close(evt){
};
function start_socket(){
   if(websocket){delete websocket.onclose;delete websocket.onmessage;delete websocket.onerror;websocket.close();};
   if(location.protocol==\"https:\")
      websocket=new WebSocket(\"wss://\"+location.host+\"/tire\"+\"_\"+current_slide+\"_\"+current_state);
   else
      websocket=new WebSocket(\"ws://\"+location.host+\"/tire\"+\"_\"+current_slide+\"_\"+current_state);
   websocket.onclose=websocket_close;
   websocket.onmessage = websocket_msg;
   websocket.onerror = websocket_err;
};
window.onbeforeunload = function() {
    websocket.onclose = function () {}; // disable onclose handler first
    websocket.close()
};
function websocket_send(data){
  if (!websocket) start_socket();
  websocket.send(data);
}
"

type change =
  Nothing
| Slide of int * int
| Refresh of int * int
| Dynamics of string Typography.OutputCommon.dynamic list

let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let dynCache = Array.map (fun t -> Array.map (fun _ -> Hashtbl.create 13) t) pages in

  let slides,cache,imgs=SVG.buffered_output' ~structure:structure ~dynCache pages "" in
  
  let dynTable = Hashtbl.create 101 in

  Array.iteri (fun slide tbl ->
    Array.iteri (fun state h ->
      Hashtbl.iter (fun label dyn ->
	try
	  let old = Hashtbl.find dynTable label in
	  Hashtbl.replace dynTable label (((slide,state),dyn)::old)
	with
	  Not_found ->
	    Hashtbl.add dynTable label [(slide,state),dyn])
	h)
      tbl)
    dynCache;

  let imgs = StrMap.fold (fun k filename m ->
    Printf.eprintf "image: %s %s\n%!" k filename;
    let buf =
	Printf.eprintf "encoding %s\n%!" filename;
	let ch = open_in k in
	let len = in_channel_length ch in
	let buf = String.create len in
	really_input ch buf 0 len;
	buf
    in
    StrMap.add filename buf m) imgs StrMap.empty
  in
  
  let imgs = StrMap.add "favicon.ico" duck_ico imgs in

  let read_slide_state get =
    let asked_slide=max 0 (int_of_string (Str.matched_group 1 get)) in
    let state=max 0 (int_of_string (Str.matched_group 2 get)) in
    
    let slide=min asked_slide (Array.length slides-1) in
    let state=if asked_slide>slide then
	(Array.length slides.(slide)-1)
      else
	min state (Array.length slides.(slide)-1)
    in
    slide, state
  in
  
  let mouse_script=
    let w,h = pages.(0).(0).pageFormat in (* FIXME: assume same format for all pages *)
    Printf.sprintf 
"
function send_click(name,dest,ev) {
  ev = ev || window.event;
  var message = name+' '+dest;
  websocket_send(\"click_\"+(current_slide)+\"_\"+(current_state)+\" \"+message);
}


function draggable(anchor,obj)
{
    anchor.onmousedown = function(e){
      var objX0 = parseInt(obj.style.left) - e.pageX;
      var objY0 = parseInt(obj.style.top) - e.pageY;

      window.onmousemove = function(e){
        var x = e.pageX + objX0;
        var y = e.pageY + objY0;
         obj.style.left=x+'px';
         obj.style.top=y+'px';
      };	  

      window.onmouseup = function(e){
        window.onmousemove = null;
        window.onmouseup = null;
      };
    };
}
 
function start_edit(name,dest,ev) {
  ev = ev || window.event;
  var elt =  document.getElementById(name);
  window.onkeydown = null;
  elt.onclick = null;

  var contents = elt.getAttribute('contents')
  var div = document.createElement('div');
  div.className='editor';
  div.style.left = '50px';
  div.style.top = '50px';
  div.innerHTML = \"<table><tr id='editorTitleBar'><td>Editor</td><td align='right'><button type='button' id='reset_button_\"+name+\"' >Reset</button><button type='button' id='cancel_button_\"+name+\"' >Cancel</button><button type='button' id='edit_button_\"+name+\"' >Save</button></td></tr><tr><td colspan='2'><textarea cols='80'; rows='30'; id='edit_\"+name+\"'>\"+contents+\"</textarea></td></tr></table>\";
  function restart_edit(name,dest) {
    return(function (e) { start_edit(name,dest,e); });
  }
  function reset_edit(name,dest) {
    return function () {
      var icontents = elt.getAttribute('initial');
      var textArea = document.getElementById('edit_'+name);
      if (icontents.replace(/\\r\\n/g,'\\n') != textArea.value.replace(/\\r\\n/g,'\\n')) {
        if (confirm('This will discard your edit and return to the initial text, are you sure ?')) { 
          elt.setAttribute('contents', icontents);
          textArea.value = icontents;
          return false;
        }
      }
    }
  }
  function stop_edit(name,dest,div) {
    return function () {
      var message = name+' '+dest;
      var textArea = document.getElementById('edit_'+name);
      var elt =  document.getElementById(name);
      elt.setAttribute('contents', textArea.value);
      var contents = textArea.value.replace('&#13;&#10;','\\n').replace('&gt;','>').replace('&lt;','<').replace('&apos',\"'\").replace('&quot;','\"').replace('&amp;','&');
      websocket_send('edit_'+(current_slide)+'_'+(current_state)+' '+message+' '+Base64.encode(contents));
      window.onkeydown = manageKey;
      div.parentNode.removeChild(div);
      elt.onclick = restart_edit(name,dest);
      return false;
    }
  }
  function cancel_edit(name,dest,div) {
    return function () {
      var textArea = document.getElementById('edit_'+name);
      var elt =  document.getElementById(name);
      var contents = elt.getAttribute('contents');

      if (textArea.value.replace(/\\r\\n/g,'\\n') == contents.replace(/\\r\\n/g,'\\n') ||
        confirm('This will discard your edit, are you sure ?')) { 
          window.onkeydown = manageKey;
          div.parentNode.removeChild(div);
          elt.onclick = restart_edit(name,dest);
          return false;
      }
    }
  }
  document.body.appendChild(div);
  var title = document.getElementById('editorTitleBar');
  draggable(title,div);
  window.onkeydown = null;
  var button = document.getElementById('edit_button_'+name);
  button.onclick = stop_edit(name,dest,div);
  var button2 = document.getElementById('reset_button_'+name);
  button2.onclick = reset_edit(name,dest);
  var button3 = document.getElementById('cancel_button_'+name);
  button3.onclick = cancel_edit(name,dest,div);
}

function start_drag(name,dest,ev) {
  ev = ev || window.event;
  var svg_rect = document.getElementById('svg_div');
  var w = svg_rect.offsetWidth;
  var h = svg_rect.offsetHeight;
  var scale = Math.max(%g / w, %g / h);
  var message = name+' '+dest;
  var x0 =ev.pageX;
  var y0 =ev.pageY;
  var x  = x0;
  var y  = y0;
  function do_drag() {
    var dx = scale * (x - x0); var dy = scale * (y0 - y);
    if (dx != 0 || dy != 0) {
      websocket_send('drag_'+(current_slide)+'_'+(current_state)+'_'+dx+'_'+dy+' '+message);
      x0 = x; y0 = y;
  }}
  var timer = setInterval(do_drag,200);
  function stop_drag(e) {
    clearInterval(timer);
    x = e.pageX; y = e.pageY;
    do_drag();
    window.onmousemove = null;
    window.onmouseup = null;
  }
  window.onmouseup = stop_drag;
  window.onmousemove = function(e) {
    x = e.pageX;
    y = e.pageY;
  };
  return false;
}
/*
Copyright (c) 2008 Fred Palmer fred.palmer_at_gmail.com

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*/
function StringBuffer()
{ 
    this.buffer = []; 
} 

StringBuffer.prototype.append = function append(string)
{ 
    this.buffer.push(string); 
    return this; 
}; 

StringBuffer.prototype.toString = function toString()
{ 
    return this.buffer.join(\"\"); 
}; 

var Base64 =
{
    codex : \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=\",

    encode : function (input)
    {
        var output = new StringBuffer();

        var enumerator = new Utf8EncodeEnumerator(input);
        while (enumerator.moveNext())
        {
            var chr1 = enumerator.current;

            enumerator.moveNext();
            var chr2 = enumerator.current;

            enumerator.moveNext();
            var chr3 = enumerator.current;

            var enc1 = chr1 >> 2;
            var enc2 = ((chr1 & 3) << 4) | (chr2 >> 4);
            var enc3 = ((chr2 & 15) << 2) | (chr3 >> 6);
            var enc4 = chr3 & 63;

            if (isNaN(chr2))
            {
                enc3 = enc4 = 64;
            }
            else if (isNaN(chr3))
            {
                enc4 = 64;
            }

            output.append(this.codex.charAt(enc1) + this.codex.charAt(enc2) + this.codex.charAt(enc3) + this.codex.charAt(enc4));
        }

        return output.toString();
    },

    decode : function (input)
    {
        var output = new StringBuffer();

        var enumerator = new Base64DecodeEnumerator(input);
        while (enumerator.moveNext())
        {
            var charCode = enumerator.current;

            if (charCode < 128)
                output.append(String.fromCharCode(charCode));
            else if ((charCode > 191) && (charCode < 224))
            {
                enumerator.moveNext();
                var charCode2 = enumerator.current;

                output.append(String.fromCharCode(((charCode & 31) << 6) | (charCode2 & 63)));
            }
            else
            {
                enumerator.moveNext();
                var charCode2 = enumerator.current;

                enumerator.moveNext();
                var charCode3 = enumerator.current;

                output.append(String.fromCharCode(((charCode & 15) << 12) | ((charCode2 & 63) << 6) | (charCode3 & 63)));
            }
        }

        return output.toString();
    }
}


function Utf8EncodeEnumerator(input)
{
    this._input = input;
    this._index = -1;
    this._buffer = [];
}

Utf8EncodeEnumerator.prototype =
{
    current: Number.NaN,

    moveNext: function()
    {
        if (this._buffer.length > 0)
        {
            this.current = this._buffer.shift();
            return true;
        }
        else if (this._index >= (this._input.length - 1))
        {
            this.current = Number.NaN;
            return false;
        }
        else
        {
            var charCode = this._input.charCodeAt(++this._index);

            // \"\\r\\n\" -> \"\\n\"
            //
            if ((charCode == 13) && (this._input.charCodeAt(this._index + 1) == 10))
            {
                charCode = 10;
                this._index += 2;
            }

            if (charCode < 128)
            {
                this.current = charCode;
            }
            else if ((charCode > 127) && (charCode < 2048))
            {
                this.current = (charCode >> 6) | 192;
                this._buffer.push((charCode & 63) | 128);
            }
            else
            {
                this.current = (charCode >> 12) | 224;
                this._buffer.push(((charCode >> 6) & 63) | 128);
                this._buffer.push((charCode & 63) | 128);
            }

            return true;
        }
    }
}

function Base64DecodeEnumerator(input)
{
    this._input = input;
    this._index = -1;
    this._buffer = [];
}

Base64DecodeEnumerator.prototype =
{
    current: 64,

    moveNext: function()
    {
        if (this._buffer.length > 0)
        {
            this.current = this._buffer.shift();
            return true;
        }
        else if (this._index >= (this._input.length - 1))
        {
            this.current = 64;
            return false;
        }
        else
        {
            var enc1 = Base64.codex.indexOf(this._input.charAt(++this._index));
            var enc2 = Base64.codex.indexOf(this._input.charAt(++this._index));
            var enc3 = Base64.codex.indexOf(this._input.charAt(++this._index));
            var enc4 = Base64.codex.indexOf(this._input.charAt(++this._index));

            var chr1 = (enc1 << 2) | (enc2 >> 4);
            var chr2 = ((enc2 & 15) << 4) | (enc3 >> 2);
            var chr3 = ((enc3 & 3) << 6) | enc4;

            this.current = chr1;

            if (enc3 != 64)
                this._buffer.push(chr2);

            if (enc4 != 64)
                this._buffer.push(chr3);

            return true;
        }
    }
};
" w h
  in
  
  let keyboard=Printf.sprintf 
"function manageKey(e){
if(e.keyCode==37 || e.keyCode==38 || e.keyCode==33){
if(current_slide > 0 && (current_state<=0 || e.keyCode==38)) {
  websocket.send(\"move_\"+(current_slide-1)+\"_\"+(states[current_slide-1]-1));
} else if (current_state > 0) {
  websocket.send(\"move_\"+(current_slide)+\"_\"+(current_state-1));
}
} //left
if(e.keyCode==39 || e.keyCode==40 || e.keyCode==34){
if(current_slide < %d && (current_state>=states[current_slide]-1 || e.keyCode==40)) {
  websocket.send(\"move_\"+(current_slide+1)+\"_0\");
} else if (current_state < states.length - 1) {
  websocket.send(\"move_\"+(current_slide)+\"_\"+(current_state+1));
}
} else //right
if(e.keyCode==82){ //r
  websocket.send(\"move_\"+(current_slide)+\"_\"+(current_state));
}
}
window.onkeydown=manageKey;
function gotoSlide(n){
  websocket.send(\"move_\"+n+\"_0\");
}
%s" (Array.length pages - 1) mouse_script
  in

  let page,css=SVG.basic_html
    ~script:websocket
    ~onload:"start_socket();loadSlide(h0,h1);"
    ~keyboard:keyboard
    cache structure pages ""
  in

  let slides = Array.map (Array.map (fun (x,y) -> Rbuffer.contents x, Rbuffer.contents y)) slides in
  let page = Rbuffer.contents page in
  let css = Rbuffer.contents css in

  let output_cache out i j =
    Hashtbl.iter (fun k (d, ptr) ->
      try 
	let c = match !ptr with
	    Some c -> Printf.eprintf "From cache\n%!";  c 
	  | None -> let c = d.dyn_contents () in ptr := Some c; c
	in
	Rbuffer.add_string out (Printf.sprintf "<g id=\"%s\">%s</g>" d.dyn_label c) 
      with e -> 
	let e = Printexc.to_string e in
	Printf.eprintf "uncaught exception %s from dyn_contents %s\n%!" e d.dyn_label;
	Printexc.print_backtrace stderr;
	Rbuffer.add_string out (Printf.sprintf "<g id=\"%s\">%s</g>" d.dyn_label e)
    ) dynCache.(i).(j)
  in

  let build_svg i j =
    let prefix,suffix = slides.(i).(j) in
    let buf = Rbuffer.create 256 in
    Rbuffer.add_string buf prefix;
    output_cache buf i j;
    Rbuffer.add_string buf suffix;
    Rbuffer.contents buf
  in

  let pushto ?(change=Nothing) a fd =
    let a = Unix.out_channel_of_descr a in
    try
      let slide, state, change, change_list = match change with
          Nothing -> present.cur_slide, present.cur_state, "\"Nothing\"", "[]"
	| Slide(i,j) -> 
	  let svg = build_svg i j in
	  i, j, "\"Slide\"", Printf.sprintf "\"%s\"" (base64_encode svg)
	| Refresh(i,j) -> 
	  i, j, "\"Refresh\"", "[]"
	| Dynamics l ->
	  let l = List.map (fun d -> 
	    Printf.sprintf "{\"dyn_label:\"%s\",dyn_contents:\"%s\"}" d.dyn_label (base64_encode (d.dyn_contents ()))) l in
	  let full = "[" ^ String.concat "," l ^"]" in 
	  0, 0, "\"Dynamics\"", full
      in
      resp_slave a (
	Printf.sprintf "{ \"slide\":%d, \"state\":%d, \"change\":%s, \"change_list\":%s }" 
	  slide state 
	  change change_list
      );
    with
      e->
	kill_son fd
  in

  let affected slide state dest =
    List.fold_left (fun acc label ->
      try
	let l = Hashtbl.find dynTable label in
	List.iter (fun ((i,j),(d,ptr)) -> ptr := None) l;
	acc || List.mem_assoc (slide,state) l
      with Not_found -> acc) false dest
  in

  let push from dest = (*from avoids to send back to the expeditor*)
    List.iter (fun (fd,son) ->
      if fd <> from && affected son.slide son.state dest then
	pushto ~change:(Refresh(son.slide,son.state)) son.served_sock fd
    ) !sonsBySock
  in

  let http_send ?sessid code ctype datas ouc = 
    Printf.fprintf ouc "HTTP/1.1 %d OK\r\n" code;
    Printf.fprintf ouc "Content-type: %s\r\n" ctype;
    let len = List.fold_left (fun acc s -> acc + String.length s) 0 datas in
    Printf.fprintf ouc "Content-Length: %d\r\n" len;
    (match sessid with
      Some (sessid, groupid) -> 
	Printf.eprintf "Set-Cookie: SESSID=%s; GROUPID=%s;\r\n" sessid groupid;
	Printf.fprintf ouc "Set-Cookie: SESSID=%s; GROUPID=%s;\r\n" sessid groupid;
    | None -> ());
    output_string ouc "\r\n";
    List.iter (output_string ouc) datas;
    output_string ouc "\r\n";
    flush ouc
  in

  let generate_error ouc =
    let data =
      "Not found"
    in
    Printf.eprintf "sent 404\n%!";
    http_send 404 "text/plain" [data] ouc;
  in
(*
  let generate_ok sessid ouc =
    let data =
      "Ok"
    in
    Printf.eprintf "sent 200 Ok\n%!";
    http_send 200 "text/plain" [data] sessid ouc;
  in
*)
  let serve_svg i j num (sessid,groupid) ouc =
    if i<Array.length slides && j<Array.length slides.(i) then (
      Printf.eprintf "building slide %d_%d for %d\n%!" i j num;
      try
	let prefix,suffix = slides.(i).(j) in
	let dyns = Rbuffer.create 256 in
	output_cache dyns i j;
	let dyns = Rbuffer.contents dyns in
	Printf.eprintf "start sent image/svg+xml %d %s\n%!" num sessid;
	http_send 200 "image/svg+xml" [prefix; dyns; suffix] ouc;
      Printf.eprintf "sent image/svg+xml %d %s\n%!" num sessid;
      with e -> Printf.eprintf "error building or sending slide\n%!"; raise e
    ) else (
      generate_error ouc;
    )
  in
  
  let fonts = StrMap.fold (fun key font acc ->
  (*  Printf.eprintf "Font: %S\n%!" key;*)
    let key = List.hd (List.rev (Util.split '/' key)) in
    StrMap.add key (Rbuffer.contents font) acc) cache.fontBuffers StrMap.empty
  in

  let serve_font font ouc=
    try
      Printf.eprintf "Search Font: %S\n%!" font;
      let data= StrMap.find font fonts in
      http_send 200 "font/opentype" [data] ouc;
    with
      Not_found->generate_error ouc
  in


  let serve_css ouc=
    http_send 200 "text/css" [css] ouc;
  in
  
  let serve ?sessid fdfather num fd =

    Unix.clear_nonblock fd;
    let websocket = ref false in
    let inc=Unix.in_channel_of_descr fd in
    let ouc=Unix.out_channel_of_descr fd in
    let fouc=Unix.out_channel_of_descr fdfather in
    let sessid = Db.sessid in
    Random.self_init ();
    let read_sessid () = match !sessid with
      | Some (s,g) -> 
	Printf.eprintf "Reuse sessid: %s from %s\n%!" s g;
	s, g
      | None -> 
	let s = make_sessid () in
	Printf.eprintf "New sessid: %s as guest\n%!" s;
	s, "guest"
    in

    let update slide state ev dest =
      let priv, pub =
	List.fold_left (fun (priv,pub as acc) ds ->
	  try
	    let l = Hashtbl.find dynTable ds in
	    List.fold_left  (fun (priv,pub as acc) (_,(d,_)) ->
	      let res = d.dyn_react ev in
	      match res with
		Unchanged -> acc
	      | Private -> (ds::priv), pub
	      | Public ->  (ds::priv), (ds::pub))
	      acc l
	  with
	    Not_found -> 
	      Printf.eprintf "Warning: dynamic not found: %s\n%!" ds;
	      acc) ([], []) dest
      in
      Printf.eprintf "Private change\n%!"; 
      if affected slide state priv then pushto ~change:(Slide(slide,state)) fd fdfather;
      Printf.eprintf "Public change\n%!"; 
      Printf.fprintf fouc "change %s\n" (String.concat " " pub);
      flush fouc 
    in

    let rec process_req master get hdr reste=
      
      if !websocket then (
	Printf.eprintf "Reading web socket message\n%!";
	let get = decode_slave inc in
	Printf.eprintf "Web socket message:%s\n%!" get;
	
	if Str.string_match move get 0 then (
          Printf.eprintf "move\n";flush stderr;
	  let slide, state = read_slide_state get in
	  
          pushto ~change:(Slide(slide,state)) fd fdfather;
	  Printf.eprintf "Sending to father ...\n";
	  Printf.fprintf fouc "move %s %d %d\n"
	    (fst (read_sessid ())) slide state;
	  flush fouc;
	  Printf.eprintf "Sending to father done\n";

          process_req master "" [] reste)
	else if Str.string_match click get 0 then (
	  Printf.eprintf "serve %d: click\n%!" num;
	  let match_end = Str.match_end () in
	  let slide, state = read_slide_state get in
	  let rest =String.sub get match_end (String.length get - match_end) in
	  
	  Printf.eprintf "click: %d %d %s\n%!" slide state rest;
	  
	  let name, dest = match Util.split ' ' rest with
	      name::dest -> name,dest
	    | _ -> failwith "Bad click"
	  in
	  update slide state (Click(name)) dest;
          process_req master "" [] reste)
	    
	else if Str.string_match edit get 0 then (
	  Printf.eprintf "serve %d: edit\n%!" num;
	  let match_end = Str.match_end () in
	  let slide, state = read_slide_state get in
	  let rest =String.sub get match_end (String.length get - match_end) in

	  Printf.eprintf "edit: %d %d %s\n%!" slide state rest;

	  let name, dest, contents = 
	    try 
	      match Util.split ' ' rest with
		name::dest -> 
		  let dest = List.rev dest in
		  let contents = List.hd dest in
		  let dest = List.tl dest in
		  name,dest, base64_decode contents
	      | _ -> raise Not_found
	    with _ -> failwith "Bad edit" 
	  in
	  Printf.eprintf "edited text: %S\n%!" contents;

	  update slide state (Edit(name,contents)) dest;
          process_req master "" [] reste
	    
	) else if Str.string_match drag get 0 then (
	  Printf.eprintf "serve %d: drag\n%!" num;
	  let match_end = Str.match_end () in
	  let dx = float_of_string (Str.matched_group 3 get) 
	  and dy = float_of_string (Str.matched_group 4 get) in
	  let slide, state = read_slide_state get in
	  let rest =String.sub get match_end (String.length get - match_end) in
	  
	  Printf.eprintf "drag: %d %d %g %g %s\n%!" slide state dx dy rest;
	  
	  let name, dest = match Util.split ' ' rest with
	      name::dest -> name,dest
	    | _ -> failwith "Bad click"
	  in
	  update slide state (Drag(name,(dx,dy))) dest;
          process_req master "" [] reste
	) else	  
	    process_req master "" [] reste
      ) else 
	let x=input_line inc in
	Printf.eprintf "serve %d: %S %S\n%!" num get x;
	if x.[0]='\r' then (
	  if Str.string_match svg get 0 then (
	    Printf.eprintf "serve %d: get %S\n%!" num get;
            let i=int_of_string (Str.matched_group 1 get) in
            let j=int_of_string (Str.matched_group 2 get) in
            serve_svg i j num (read_sessid ()) ouc;
            process_req master "" [] reste

	  ) else if Str.string_match rmaster get 0 && Str.matched_group 1 get = !master_page then (
	    Printf.eprintf "serve %d: master\n%!" num;	
	    http_send ~sessid:(read_sessid ()) 200 "text/html" [page] ouc;
            process_req true "" [] reste
	      
	  ) else if Str.string_match logged get 0 then (
	    let login = Str.matched_group 1 get in
            let md5 = Str.matched_group 2 get in
	    let groupid = try Str.matched_group 4 get with Not_found -> "guest" in
	    let md5' = Digest.to_hex(Digest.string(login ^ "+" ^ groupid ^ !secret)) in
	    Printf.eprintf "serve %d: logged %s from %s %s %s\n%!" num login groupid md5 md5';
	    if md5 = md5' then (
	      sessid := Some (login, groupid);
  	      http_send ~sessid:(login,groupid) 200 "text/html" [page] ouc;
              process_req false "" [] reste
            ) else (
	      generate_error ouc
            )

	  ) else if Str.string_match slave get 0 then (
	    Printf.eprintf "serve %d: slave (%s)\n%!" num get;
	    http_send ~sessid:(read_sessid ()) 200 "text/html" [page]  ouc;
            process_req false "" [] reste

	  ) else if get="/etat" then (
	    Printf.eprintf "serve %d: etat\n%!" num;
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
            Buffer.add_string data (Printf.sprintf "\"time\"=%g," t);
	    let son_descr = List.map (fun (fd,son) ->
	      Printf.sprintf "  { \"num\" = %d, \"pid\" = %d, \"slide\" = %d, \"state\" = %d }"
		son.num son.pid son.slide son.state) !sonsBySock in
	    let son_descr = String.concat ",\n" son_descr in
            Buffer.add_string data (Printf.sprintf "\"sons\"=[\n%s\n]" son_descr);
            Buffer.add_char data '}';
	    
	    http_send 200 "text/plain" [Buffer.contents data] ouc;
            process_req master "" [] reste
	      
	  ) else if Str.string_match tire get 0 || get="/tire" then (
            let slide, state =if get = "/tire" then -1, -1 else
		read_slide_state get 
	    in

	    Printf.eprintf "serve %d: tire\n%!" num;
            try
              Printf.eprintf "pushing\n";flush stderr;
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
	      
	      Printf.eprintf "Sending to father ...\n";
	      Printf.fprintf fouc "move %s %d %d\n"
		(fst (read_sessid ())) slide state;
	      flush fouc;
	      Printf.eprintf "Sending to father done\n";
	      websocket := true;
	      
	      process_req master "" [] reste
		
            with
            | e-> Printf.eprintf "erreur %d websocket \"%s\"\n%!" num (Printexc.to_string e);
	      
	  ) else if Str.string_match css_reg get 0 then (
	    
	    Printf.eprintf "serve %d: css\n%!" num;
            serve_css ouc;
            process_req master "" [] reste
	      
	  ) else if Str.string_match otf get 0 then (
	    let otf = Str.matched_group 1 get in
	    
	    Printf.eprintf "serve %d: otf\n%!" num;
            serve_font otf ouc;
            process_req master "" [] reste

	  ) else (
	    
        try
	  let name = String.sub get 1 (String.length get-1) in
	  Printf.eprintf "serve %d: image: %s\n%!" num name;
          let img=StrMap.find name imgs in
          let ext=
            if Filename.check_suffix ".png" get then "image/png" else
              if Filename.check_suffix ".jpeg" get then "image/jpeg" else
                if Filename.check_suffix ".jpg" get then "image/jpg" else
                  if Filename.check_suffix ".gif" get then "image/gif" else
                    if Filename.check_suffix ".ico" get then "image/vnd.microsoft.icon" else
                    "application/octet-stream"
          in
 
	  http_send 200 ext [img] ouc;
          process_req master "" [] []
        with
            Not_found->(
	      generate_error ouc;
              process_req master "" [] reste);
      )

    ) else (

      if hdr=[] && Str.string_match get_reg x 0 then (
	let str = Str.matched_group 1 x in
        process_req master str hdr reste
      ) else if Str.string_match header x 0 then (
        let a=Str.matched_group 1 x in
        let b=Str.matched_group 2 x in
	if a = "Cookie" || a = "Cookies" then (
	  let ls = Str.split (Str.regexp ";[ \t]+") b in
	  let ls = List.fold_left (fun acc s -> 
	    match Util.split '=' s with
	      [key;v] -> (key,v)::acc
	    | _ -> acc) [] ls
	  in
	  (try sessid := Some (List.assoc "SESSID" ls, List.assoc "GROUPID" ls) with Not_found -> ());
          process_req master get hdr reste)
	else
          process_req master get ((a,b)::hdr) reste
      ) else (
        process_req master get hdr (x::reste)
      );
    )
  in
  try
    process_req false "" [] []
  with
    | e-> 
      (match !sessid with
	None ->
	  Printf.fprintf fouc "quit ? %d\n" (Unix.getpid ());
	  flush fouc
      | Some sessid ->
	Printf.fprintf fouc "quit %s %d\n" (fst sessid) (Unix.getpid ());
	flush fouc);
      Printf.eprintf "erreur %d : \"%s\"\n%!" num (Printexc.to_string e);
      exit 0;
in

  Arg.parse spec (fun x->()) "";

  if !master_page="" then (
    master_page:=Printf.sprintf "/%d" (Random.int (1 lsl 29));
  );
  if !master_page.[0] <> '/' then master_page := "/" ^ !master_page;

  ignore (Sys.signal 13 (Sys.Signal_ignore));

  while true do
    Printf.eprintf "upper main loop\n%!";
    try Unix.(
      let port = !port_num in
      let poss_addrs = getaddrinfo "" (string_of_int port) [AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE] in
      let rec fn acc = function
        [] -> if acc = [] then (
	  Printf.eprintf
	    "Failed to listen on any address.\n%!";
	  exit 1)
	  else acc
	| addr::rest ->
	  try
(*	    Printf.eprintf
	      "Trying to listen from %s:%d\n%!" str_addr port;*)
	    let master_sock= socket addr.ai_family addr.ai_socktype 0 in
	    setsockopt master_sock SO_REUSEADDR true;
	    bind master_sock addr.ai_addr;
	    listen master_sock 100;
	    Printf.eprintf
	      "Listening on port %s:%d\n%!" (str_addr addr.ai_addr) port;
	    fn (master_sock::acc) rest
	  with _ -> fn acc rest
      in

      let master_sockets = fn [] poss_addrs in

      Printf.eprintf 
	"Listening from %d addresses -- master: \"%s\"\n%!"
	(List.length master_sockets) !master_page;

      let conn_num = ref 0 in

      while true do
	Printf.eprintf "in main loop (%d addresses, %d sons)\n%!" (List.length master_sockets) (List.length !sonsBySock);
	(try while (fst (Unix.waitpid [WNOHANG] (-1)) <> 0) do () done with _ -> ());
	let socks,_,errors=Unix.select (master_sockets@List.map fst !sonsBySock) [] (List.map fst !sonsBySock) 30. in
	Printf.eprintf "select returns %d read and %d errors.\n%!" (List.length socks) (List.length errors);
	List.iter (fun sock ->
	  Printf.eprintf "Remove a son on error\n%!";
	  kill_son sock) errors;
	List.iter (fun sock -> try
	  if not (List.mem sock errors) then (
	  if not (List.mem sock master_sockets) then (
	    Printf.eprintf "Serving a son\n%!";
	    let son (*ic, pid, num, fd, sessid_ptr*) = try List.assoc sock !sonsBySock with _ -> assert false in
	    let cmd = Util.split ' ' (input_line son.fd) in
	    Printf.eprintf "received from %d %d: %s\n%!" son.num son.pid (String.concat " " cmd);
	    match cmd with
	      ["move";sessid;slide;state] -> (
		let slide = int_of_string slide and state = int_of_string state in
		son.sessid <- Some sessid;
		son.slide <- slide;
		son.state <- state;
		if son.sessid <> None then (
		  try
		    List.iter (fun (fd,son') ->
		      if son'.sessid = son.sessid && son' != son then (
			Printf.eprintf "Killing old son: %d %d\n%!"
			  son'.num son'.pid;
			kill_son fd)) !sonsBySock
		  with
		    Not_found -> ())
	      )
	    | "change"::dest ->
	      push sock dest;

	    | ["quit";sessid;pid] ->
	      kill_son sock;
	    | _ ->
	      Printf.eprintf "Bad message from son\n%!";
	  ) else (
	    Printf.eprintf "Accepting a connection\n";
            let conn_sock, addr = Unix.accept sock in
	    Unix.set_nonblock conn_sock;
	    let num = !conn_num in
	    incr conn_num;
	    let fd2,fd1 =  Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
	    List.iter (fun f -> f ()) !interaction_start_hook;
	    let pid = Unix.fork () in
	    if pid = 0 then (
	      try
		Util.close_in_cache ();
		Unix.close fd2;
		close_all_other conn_sock;
		Printf.eprintf "Connection started: %d\n%!" num;
		serve fd1 num conn_sock;
	      with _ -> exit 0);
	    Unix.close fd1;
	    sonsBySock := (fd2,{ fd = in_channel_of_descr fd2;
			   pid = pid; num = num;
			   served_sock = conn_sock;
			   sessid = None; slide = 0; state = 0})::!sonsBySock))
          with e -> 
	    kill_son sock;
	    Printf.eprintf "main loop (reading): %s\n%!"
	    (Printexc.to_string e)) socks
      done)

    with e-> Printf.eprintf "main loop (before read): %s\n%!"
	  (Printexc.to_string e)
  done

let output = output_from_prime output'

let _ = 
  Hashtbl.add drivers "Patonet" (module struct let output = output let output' = output' end:Driver)

