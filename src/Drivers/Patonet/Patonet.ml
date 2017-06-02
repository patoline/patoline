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

open Db
open RawContent
open Driver
open Util
open UsualMake
open HtmlFonts

let _ = Printexc.record_backtrace true

let duck_ico =
"\000\000\001\000\001\000  \000\000\001\000\024\000\168\012\000\000\022\000\000\000(\000\000\000 \000\000\000@\000\000\000\001\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\244\245\245\250\250\250\239\240\241\239\239\239\246\246\246\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255HGF*('PPOHHF\221\221\221\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\253\253\253\254\254\254\254\254\254\249\249\249\247\247\247\247\246\245\177\176\175\127~}``^\248\249\248\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\253\253\253\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\242\243\244\174\176\175\175\174\174\138\138\137\129\130\131\132\134\134\231\232\235\255\255\255\255\255\255lli^][\235\235\234\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\229\230\230\166\167\167{{xWVSfda^[X.-*\030\029\0260-,(% SSPGGEtro\253\253\253\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\253\253\253\179\180\178\130\129~CA;1/(C>90-'fd_MLIGFCGFD986ed`\\ZX\240\241\240\174\173\173\173\173\172\229\229\228\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\246\246\246stq\138\137\134[WTGE@MJETRLOKF\150\148\144\022\024\023:75\026\024\0202/-75.\238\237\237\241\241\241\201\201\201\195\194\194432\154\155\153\227\228\227\255\255\255\255\255\255\227\227\227\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\252\252\252z{w30,PMGCB?bc]ec`>=9JGDVSQ\012\011\011)%##!\031\175\172\172^^Z\217\218\218\144\144\143\152\152\149665\162\162\162\154\153\154\195\195\194\148\150\149\233\234\234xxv\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\199\198\198\018\016\rGD?41-A=9\014\012\n^\\Z\021\021\020\225\224\224\205\205\203\006\005\004\020\019\01641.ZYX)(#]\\Z>>=\192\192\192\129\128\128POOrrr\254\254\254\237\237\237qom\194\195\195\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255igc>;5\245\245\244\017\015\011+)%\207\206\204\170\168\166\n\n\t=<:??>wtr\011\n\t84.<82\153\151\149\225\225\224{zy]]\\\150\149\149fec\144\145\143\255\255\255\142\143\141\158\159\157CBB\187\186\186\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\240\240\239875VTL\027\026\023\201\200\197\"!\031\015\015\rkjg\014\r\011 \030\028TOK\204\206\205\140\141\139\138\137\137322\233\232\232hihyywOMMdecllj\\[Z\160\161\161FECMLM./-ffe\240\240\241\255\255\255\254\254\254\255\255\255\254\254\254\234\234\234\006\005\001e`ZQNI('$\165\165\164\000\000\000sqn\227\227\225~}z\238\239\238\158\159\157\151\150\149\143\143\14310/\172\172\171RQPLLI```ZYXlkiba`676A@?QPN\157\156\154\137\137\135\152\152\150\255\255\255\254\254\254\254\254\254\254\254\254\229\229\228\017\016\011fd\\2/*\205\204\203\231\230\229\152\151\149\150\147\145\192\193\192\173\173\171\197\197\198\156\156\154\164\165\163\159\159\157EEC\160\160\158TSR``_\127}|fecnnl\\\\[xxvjjj\127\127}))'HGF\144\143\141\254\254\254\255\255\255\255\255\255\255\255\255\233\233\232\014\011\007b_W51+\231\229\229\163\164\164\240\240\240\255\255\255\255\255\255\255\255\255\164\164\163\148\149\147\157\159\159\212\214\215\178\179\179\199\200\199zywbcbuwv\187\187\187ihfYYX*('EDAaa_\189\189\187\132\131\128\146\146\144\255\255\255\255\255\255\255\255\255\254\254\254\240\240\240'&#[XQ30*\172\172\170wvu*)&\182\182\183\244\244\243\252\252\252\252\252\252\203\202\200\145\144\142qpo\201\201\202\204\204\203\249\250\250\184\184\183{zxEDC___lkgTTU&$#llj\177\178\175\241\242\242\171\172\169\254\254\254\255\255\255\255\255\255\254\254\254\252\252\251^][^\\VURLuuq\021\019\019\023\022\020\135\136\133kih\176\175\172gggUTU*+*1/._\\W$#\"EDB.,)WSOTNL\\XVc`\\MKH \031\029--*\020\018\017,+*\250\250\250\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\144\145\141'&$YVSFD@CD?\n\t\007\244\244\243\254\254\254\180\180\179776995TPO%##2.,764_][\139\138\136553\018\017\014\028\027\024A@<)(&FD?\027\027\023\b\007\006\199\200\199\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\191\191\188\027\025\022]YTB><\185\185\184HFD\250\251\251\255\255\255\255\255\255\170\170\168\011\011\n?=;('%\027\025\02152010.?=;\141\140\140\017\015\r,+(_^Z%$!\025\023\021\021\021\020\190\191\188\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\024\024\020URLHFB\217\217\216\030\029\028\251\251\251\255\255\255\254\254\254\255\255\255\179\179\178\030\029\028\030\028\026=;9svr\129\129\128KJI#\" WXTutr\022\024\022\007\007\006LLJ\220\219\217\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\143\142\14231,PNI\161\160\157/.+\225\225\225\255\255\255\254\254\254\255\255\255\255\255\255\210\212\210SSQ\025\024\022-+);:7?>?++)=;8WXT]\\[\205\205\202\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\199\200\198\023\022\017mkf\135\135\132\011\t\b\158\158\155\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\221\221\220\177\175\173}}z\145\146\142\187\185\185\211\213\211\253\253\252\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\244\244\243'&\"mke\146\146\143\023\020\019LKJ\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255iieJIC\174\173\172LKHlli\219\220\219\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\254\254\254\255\255\255cba=;7XXU\160\160\158GFC\134\133\132\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255lkg\158\159\156\242\242\243\255\255\255\255\255\255\255\255\255ded$\"\030\197\196\196\176\175\173\158\157\155KKI\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\151\150\148\169\168\165\176\177\176\132\132\128\158\159\156\145\144\143\132\132\131/.*XVSPNK\241\241\241\249\249\249dec\248\248\248\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\230\230\228\195\195\193\156\154\150\158\158\156\137\137\134\169\168\166\138\134\13021/\237\235\233VTO\253\253\253\254\254\254JIG\245\245\245\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\159\160\158\162\160\1550.,SRMfeb\169\169\165\220\220\218\254\254\254\249\249\248\028\027\024\245\245\245\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\254\254\254\238\239\238[ZXGE?IGA\183\183\180\253\253\253\253\253\253\224\225\224RRN\247\247\246\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\216\216\214\018\015\rgd`xws\210\210\207\211\212\210TSP\164\164\161\254\254\254\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\254\254\254\254\254\254\254\254\254\254\254\254\171\172\170\"!\030QOK[XQ\027\026\022utq\253\253\253\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\216\216\215\154\152\152\147\147\145\216\216\215\255\255\255\254\254\254\255\255\255\254\254\254\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"


type presentation={ mutable cur_slide:int; mutable cur_state:int; mutable starttime:float;mutable max_slide:int; }
let present={cur_slide=0;cur_state=0;starttime=0.;max_slide=0;}

let type_from_extension name =
  if Filename.check_suffix name ".js" then "text/javascript" else
  if Filename.check_suffix name ".png" then "image/png" else
  if Filename.check_suffix name ".jpeg" then "image/jpeg" else
  if Filename.check_suffix name ".jpg" then "image/jpg" else
  if Filename.check_suffix name ".gif" then "image/gif" else
  if Filename.check_suffix name ".html" then "text/html" else
  if Filename.check_suffix name ".css" then "text/css" else
  if Filename.check_suffix name ".ico" then "image/vnd.microsoft.icon" else
    "application/octet-stream"

exception Send_error

(* Implémentation partielle, et sans doute naive, des websockets *)
(* Encoding for websocket *)
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

(* Decoding for websocket *)
let decode_slave fd =
  let res = Rbuffer.create 256 in
  let rec fn () =
    let c = int_of_char (input_char fd) in
    let fin = 0x80 land c <> 0 in
    let _rsv1 = 0x40 land c <> 0 in
    let _rsv2 = 0x20 land c <> 0 in
    let _rsv3 = 0x10 land c <> 0 in
    let _opcode = 0x0f land c in
    let c0 = int_of_char (input_char fd) in
    let mask = c0 land 0x80 <> 0 and c0 = c0 land 0x7f in
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

let str_sessid = function
  | None -> ""
  | Some(sessid,groupid) -> sessid ^":"^ groupid
(*connection as another patonet (at most one is enough to build
any graph) as a follower*)

type son =
  {
    fd: in_channel; (* connection to the son process *)
    df: out_channel;
    pid: int;
    num: int; (*internal number, just for printing and debugging*)
    served_sock: Unix.file_descr;
    mutable addr: Unix.sockaddr;
    mutable sessid: (string * string) option;
    mutable slide:int;
    mutable state:int
  }

type event =
  | EvClick
  | EvDrag of float * float * bool
  | EvEdit of string
  | EvMenu of int

let log_father msg =
  Printf.eprintf ("[father] " ^^ msg ^^ "\n%!")

let log_son num msg =
  Printf.eprintf ("[son%d] " ^^ msg ^^ "\n%!") num

let sonsBySock = ref ([]:(Unix.file_descr * son) list)

let kill_son sock =
  sonsBySock:=List.filter (fun (fd,son) ->
    if fd = sock then (
      log_father "kill [son%d] %d" son.num son.pid;
      (try Unix.close fd with _ -> ());
      (try Unix.close son.served_sock with _ -> ());
      (try Unix.kill son.pid Sys.sigterm with _ -> ());
      false)
    else true
  ) !sonsBySock

let get_reg=Str.regexp "GET \\([^ ]*\\) .*"
let header=Str.regexp "\\([^ :]*\\) *: *\\([^\r]*\\)"

let prefix_url = ref ""

let rmaster=Str.regexp "\\(/[0-9]+\\)\\(#\\([0-9]*\\)_\\([0-9]*\\)\\)?$"
let slave=Str.regexp "/?\\(#\\([0-9]*\\)_\\([0-9]*\\)\\)?$"
let logged=Str.regexp "/?\\([a-zA-Z0-9]+\\)[?]key=\\([a-z0-9]+\\)\\(&group=\\([-a-zA-Z0-9_]+\\)\\(&friends=\\([-a-zA-Z0-9_+,]+\\)\\)?\\)?\\(#\\([0-9]*\\)_\\([0-9]*\\)\\)?$"
let svg=Str.regexp "/\\([0-9]*\\)_\\([0-9]*\\)\\.svg"
let css_reg=Str.regexp "/style\\.css"
let static_reg=Str.regexp "/static/\\([^\n\r]+\\)"
let tire=Str.regexp "/tire_\\([0-9]*\\)_\\([0-9]*\\)"
let otf=Str.regexp "/\\([^\\.]*\\.otf\\)"

let move=Str.regexp "move_\\([0-9]*\\)_\\([0-9]*\\)"
let refresh=Str.regexp "refresh_\\([0-9]*\\)_\\([0-9]*\\)"
let click=Str.regexp "click_\\([0-9]*\\)_\\([0-9]*\\) "
let edit=Str.regexp "edit_\\([0-9]*\\)_\\([0-9]*\\) "
let menu=Str.regexp "menu_\\([0-9]*\\)_\\([0-9]*\\) \\([a-zA-Z_0-9]*\\) \\([a-zA-Z_0-9]*\\)"
let drag=Str.regexp "drag_\\([0-9]*\\)_\\([0-9]*\\)_\\(-?[0-9.]*\\)_\\(-?[0-9.]*\\)\\(_release\\)? "
let ping=Str.regexp "ping"

let filter_options argv = argv
let static_folder = ref ""

let set_prefix_url str =
  let l = String.length str in
  if l > 0 && str.[l - 1] <> '/' then
      prefix_url := str ^"/"
    else
      prefix_url := str

let no_guest = ref false

let driver_options =
  SVG.driver_options @
  [("--master"       , Arg.Set_string master_page   ,"Set the master page");
   ("--port"         , Arg.Set_int port_num         ,"Set the port number to listen to");
   ("--static-folder", Arg.Set_string static_folder ,"Set the folder containing static css, html, etc.");
   ("--url-prefix"   , Arg.String set_prefix_url    ,"Set a prefix added to a few url. Usefull when redirecting using the prefix of the url.");
   ("--no-guest"     , Arg.Set no_guest             ,"Only logged user can access the presentation.");
  ]

let websocket () =
  Printf.sprintf
"var websocket;
setInterval(function () {
  websocket_send('ping');
}, 50000);
function websocket_msg(evt){
     console.log('websocket message');
     var st=JSON.parse(evt.data);
     var ch = st.change;
     if (ch == 'Ping') {
       console.log('answer to ping');
     } else if (ch == 'Slide') {
       var svg = Base64.decode(st.change_list);
       loadSlideString(st.slide,st.state,svg);
     } else if (ch == 'Dynamics' && current_slide == st.slide && current_state == st.state) {
/*     var svg = Base64.decode(st.change_list);*/
       var parser=new DOMParser();
       for(var change in st.change_list) {
         var label = st.change_list[change].dyn_label;
         var contents = Base64.decode(st.change_list[change].dyn_contents);
         var newSvg=parser.parseFromString('<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">'+contents+'</svg>',\"text/xml\");
         newSvg=document.importNode(newSvg.documentElement,true);
         var oldSvg=document.getElementById(label);
         while (oldSvg.hasChildNodes()) {
           oldSvg.removeChild(oldSvg.lastChild);
         }
         childs = newSvg.childNodes
         while (newSvg.hasChildNodes()) {
           oldSvg.appendChild(newSvg.lastChild);
         }
         setReaction(oldSvg)
       }
     }
};
function websocket_err(evt){
  console.log('websocket error');
  websocket=null;
};
function websocket_close(evt){
  console.log('websocket close');
  websocket=null;
};
function start_socket(){
   if (typeof WebSocket === \"undefined\") {
     alert('Your Browser does not seem to support websockets, this page will not work.');
     return false;
   }
   if(websocket){delete websocket.onclose;delete websocket.onmessage;delete websocket.onerror;websocket.close();};
   if(location.protocol==\"https:\")
      websocket=new WebSocket(\"wss://\"+location.host+\"/%stire\"+\"_\"+current_slide+\"_\"+current_state);
   else
      websocket=new WebSocket(\"ws://\"+location.host+\"/%stire\"+\"_\"+current_slide+\"_\"+current_state);
   websocket.onclose=websocket_close;
   websocket.onmessage = websocket_msg;
   websocket.onerror = websocket_err;
};
window.onbeforeunload = function() {
    if (websocket) {
     websocket.onclose = function () {}; // disable onclose handler first
      websocket.close()
    }
};
function websocket_send(data){
  if (websocket) {
    console.log(\"websocket state:\", websocket.readyState);
  }
  else {
    console.log(\"websocket unitialized\");
  }
  if (!websocket || websocket.readyState != 1) {
    if (!websocket || websocket.readyState != 0) start_socket();
    function do_send(interval, tries) {
      if (tries > 0) {
        var timer = setInterval(function () {
          clearInterval(timer);
          if (websocket && websocket.readyState == 1) {
             console.log(\"sending\",tries,\"==>\",data);
             websocket.send(data);
          } else if (websocket && websocket.readyState == 0) {
             do_send(interval * 2, tries - 1);
          }
        }, interval)
      }
    };
    do_send(200,7);
  } else {
    console.log(\"sending in else\",\"==>\",data);
    websocket.send(data);
  }
}
" !prefix_url !prefix_url

type change =
  Ping
| Slide of int * int
| Dynamics of int * int * string list(*Typography.OutputCommon.dynamic list*)

let output' ?(structure:structure={name="";raw_name=[];metadata=[];tags=[];
                                  page= -1;struct_x=0.;struct_y=0.;children=[||]})
    pages fileName=

  let dynCache =
    Array.map (fun t ->
        Array.map (fun _ -> (Hashtbl.create 13,
                             Hashtbl.create 13)) t) pages in
  let graph = Hashtbl.create 1001 in

  let slides,cache,imgs=
    SVG.buffered_output' ~structure:structure ~dynCache:(dynCache,graph,Db.record_read) pages ""
  in

  let imgs = StrMap.fold (fun k filename m ->
    let buf =
        let ch = open_in k in
        let len = in_channel_length ch in
        let buf = Bytes.create len in
        really_input ch buf 0 len;
        buf
    in
    StrMap.add filename buf m) imgs StrMap.empty
  in

  let imgs = StrMap.add "favicon.ico" duck_ico imgs in

  let codemirror = Filename.concat !static_folder "codemirror/lib" in
  let use_codemirror = !static_folder <> "" &&
    Sys.file_exists codemirror && Sys.is_directory codemirror
  in
  let extraheader =
    if use_codemirror then
      "<link rel=\"stylesheet\" href=\"static/codemirror/lib/codemirror.css\">
       <script src=\"static/codemirror/lib/codemirror.js\"></script>
       <script src=\"static/codemirror/addon/edit/matchbrackets.js\"></script>
       <script src=\"static/codemirror/addon/mode/loadmode.js\"></script>
       <script src=\"static/codemirror/mode/meta.js\"></script>
    " else ""
  in

  let editor_init =
    if use_codemirror then "
      CodeMirror.modeURL = \"static/codemirror/mode/%N/%N.js\";
      var textArea = document.getElementById('edit_'+name);
      var editor = CodeMirror.fromTextArea(textArea,
         { lineNumbers: true, matchBrackets: true,
           smartIndent: true, indentWithTabs: false,
           viewportMargin: Infinity
         });
      var extension = name.split('_').pop();
      var info = CodeMirror.findModeByExtension(extension);
      if (!info) info = CodeMirror.findModeByExtension('ml');
      var mode = info.mode;
      var spec = info.mime;
      editor.setOption(\"mode\", spec);
      editor.setSize(640, 400);
      CodeMirror.autoLoadMode(editor, mode);
      editor.textContent = spec;
      function finaliser()  { editor.save(); }
      function reseter(v) { editor.getDoc().setValue(v); }
    "
    else "function finaliser() {}
          function reseter(v) {}
         "
  in

  let mouse_script=
    let w,h = pages.(0).(0).size in (* FIXME: assume same format for all pages *)
    Printf.sprintf
"
function send_click(name,ev) {
  ev = ev || window.event;
  websocket_send(\"click_\"+(current_slide)+\"_\"+(current_state)+\" \"+name);
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

function start_edit(name,ev) {
  ev = ev || window.event;
  var elt =  document.getElementById(name);
  window.onkeydown = null;
  elt.onclick = null;

  var contents = elt.getAttribute('contents')
  var div = document.createElement('div');
  div.className='editor';
  div.style.left = '50px';
  div.style.top = '50px';
  div.innerHTML = \"<table><tr id='editorTitleBar'><td>Editor</td><td align='right'><button type='button' id='reset_button_\"+name+\"' >Reset</button><button type='button' id='cancel_button_\"+name+\"' >Cancel</button><button type='button' id='edit_button_\"+name+\"' >Save</button></td></tr><tr><td colspan='2'><textarea cols='80'; rows='40'; id='edit_\"+name+\"'>\"+contents+\"</textarea></td></tr></table>\";
  function restart_edit(name) {
    return(function (e) { start_edit(name,e); });
  }
  function reset_edit(name,reseter) {
    return function () {
      var icontents = elt.getAttribute('initial');
      var textArea = document.getElementById('edit_'+name);
      if (confirm('This will discard your edit and return to the initial text, are you sure ?')) {
          textArea.value = icontents;
          reseter(icontents);
          return false;
      }
    }
  }
  function stop_edit(name,div,extra) {
    return function () {
      extra();
      var textArea = document.getElementById('edit_'+name);
      var elt =  document.getElementById(name);
      elt.setAttribute('contents', textArea.value);
      var contents = textArea.value.replace('&#13;&#10;','\\n').replace('&gt;','>').replace('&lt;','<').replace('&apos',\"'\").replace('&quot;','\"').replace('&amp;','&');
      websocket_send('edit_'+(current_slide)+'_'+(current_state)+' '+name+' '+Base64.encode(contents));
      window.onkeydown = manageKey;
      div.parentNode.removeChild(div);
      elt.onclick = restart_edit(name);
      return false;
    }
  }
  function cancel_edit(namediv) {
    return function () {
      var textArea = document.getElementById('edit_'+name);
      var elt =  document.getElementById(name);
      var contents = elt.getAttribute('contents');

      if (textArea.value.replace(/\\r\\n/g,'\\n') == contents.replace(/\\r\\n/g,'\\n') ||
        confirm('This will discard your edit, are you sure ?')) {
          window.onkeydown = manageKey;
          div.parentNode.removeChild(div);
          elt.onclick = restart_edit(name);
          return false;
      }
    }
  }
  document.body.appendChild(div);
  window.onkeydown = null;
  %s
  var button = document.getElementById('edit_button_'+name);
  button.onclick = stop_edit(name,div,finaliser);
  var button2 = document.getElementById('reset_button_'+name);
  button2.onclick = reset_edit(name,reseter);
  var button3 = document.getElementById('cancel_button_'+name);
  button3.onclick = cancel_edit(name,div);
  var title = document.getElementById('editorTitleBar');
  draggable(title,div);
}

function start_drag(name,ev,touch) {
  ev = ev || window.event;
  var svg_rect = document.getElementById('svg_div');
  var w = svg_rect.offsetWidth;
  var h = svg_rect.offsetHeight;
  var scale = Math.max(%g / w, %g / h);
  var x0 =ev.pageX;
  var y0 =ev.pageY;
  var x  = x0;
  var y  = y0;
  function do_drag(release) {
    var dx = scale * (x - x0); var dy = scale * (y0 - y);
    if (dx != 0 || dy != 0 || release) {
      if (release) rmsg = '_release'; else rmsg = '';
      websocket_send('drag_'+(current_slide)+'_'+(current_state)+'_'+dx+'_'+dy+rmsg+' '+name);
      x0 = x; y0 = y;
  }}
  var timer = setInterval(function () {do_drag(false);},200);
  function touch_move(e) {
      x = e.targetTouches[0].pageX,
      y = e.targetTouches[0].pageY;
  }
  function stop_drag(e) {
    clearInterval(timer);
    if (touch) {
      touch.removeEventListener('touchmove', touch_move , false);
      touch.removeEventListener('touchend', stop_drag, false);
      touch.removeEventListener('touchcancel', stop_drag, false);
      touch_move(e);
    } else {
      x = e.pageX; y = e.pageY;
      window.onmousemove = null;
      window.onmouseup = null;
    }
    do_drag(true);
  }
  if (touch) {
    touch.addEventListener('touchmove', touch_move , false);
    touch.addEventListener('touchend', stop_drag, false);
    touch.addEventListener('touchcancel', stop_drag, false);
  } else {
    window.onmouseup = stop_drag;
    window.onmousemove = function(e) {
      x = e.pageX;
      y = e.pageY;
    };
  }
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
" editor_init w h
  in

  let keyboard=Printf.sprintf
"
function previousPage(page) {
  if(current_slide > 0 && (current_state<=0 || page)) {
    websocket_send(\"move_\"+(current_slide-1)+\"_\"+(states[current_slide-1]-1));
  } else if (current_state > 0) {
    websocket_send(\"move_\"+(current_slide)+\"_\"+(current_state-1));
  }
}
function nextPage(page) {
  if(current_slide < %d && (current_state>=states[current_slide]-1 || page)) {
    websocket_send(\"move_\"+(current_slide+1)+\"_0\");
  } else if (current_state < states[current_slide] - 1) {
    websocket_send(\"move_\"+(current_slide)+\"_\"+(current_state+1));
  }
}
function manageKey(e){
  if(e.keyCode==37 || e.keyCode==38 || e.keyCode==33) previousPage(e.keyCode==38); //left
  else if(e.keyCode==39 || e.keyCode==40 || e.keyCode==34) nextPage(e.keyCode==40); //right
  else if(e.keyCode==82) //r
    websocket_send(\"move_\"+(current_slide)+\"_\"+(current_state));
}
window.onkeydown=manageKey;
function gotoSlide(n){
  websocket_send(\"move_\"+n+\"_0\");
}
%s" (Array.length pages - 1) mouse_script
  in

  let onload =
"start_socket();
websocket_send(\"refresh_\"+h0+\"_\"+h1);
"
  in

  let extrabody = "
  <div id=\"leftpanel\" style=\"left: 0px; top: 0px; position: absolute; z-index: 10;\"><button onclick=\"previousPage(false);\" style=\"min_height: 5%; min_width: 5%;\"><<</button></div>
  <div id=\"rightpanel\" style=\"right: 0px; top: 0px; position: absolute;  z-index: 10;\"><button onclick=\"nextPage(false);\">>></button></div>"
  in


  let page,css=SVG.basic_html
    ~extraheader
    ~extrabody
    ~script:(websocket ())
    ~onload
    ~keyboard:keyboard
    cache structure pages ""
  in

  let slides = Array.map (Array.map (fun (x,y) -> Rbuffer.contents x, Rbuffer.contents y)) slides in
  let page = Rbuffer.contents page in
  let css = Rbuffer.contents css in

  let output_cache out i j =
    Rbuffer.add_string out "<defs id=\"svg_defs\">";
    Hashtbl.iter (fun k (d, ptr, _, _) ->
      try
        let c = match !ptr with
            Some c -> (*Printf.eprintf "From cache\n%!";*)  c
          | None -> let c = try d.dyn_contents () with _ -> "" in ptr := Some c; c
        in
        Rbuffer.add_string out (Printf.sprintf "<g id=\"@%s\">%s</g>" d.dyn_label c)
      with e ->
        let e = Printexc.to_string e in
        Printf.eprintf "[ERROR]uncaught exception %s from dyn_contents %s\n%!" e d.dyn_label;
        Printexc.print_backtrace stderr;
        Rbuffer.add_string out (Printf.sprintf "<g id=\"%s\">%s</g>" d.dyn_label e)
    ) (fst dynCache.(i).(j));
    Rbuffer.add_string out "</defs>";
  in

  let reset_cache () =
    Array.iter (fun tbl ->
      Array.iter (fun (tbl,_) ->
        Hashtbl.iter (fun k (d, ptr,_,_) -> ptr := None) tbl
      ) tbl
    ) dynCache
  in

  let build_svg i j =
    let prefix,suffix = slides.(i).(j) in
    let buf = Rbuffer.create 256 in
    Rbuffer.add_string buf prefix;
    output_cache buf i j;
    Rbuffer.add_string buf suffix;
    Rbuffer.contents buf;
  in

  let pushto ?(change=Ping) num a =
    let a = Unix.out_channel_of_descr a in
    try
      let slide, state, change, change_list = match change with
          Ping -> present.cur_slide, present.cur_state, "\"Ping\"", "[]"
        | Slide(i,j) ->
          let svg = build_svg i j in
          i, j, "\"Slide\"", Printf.sprintf "\"%s\"" (base64_encode svg)
        | Dynamics(i,j,l) ->
           let l = List.fold_left
                     (fun acc label ->
                       (*log_son num "searching %s,%d,%d" label i j;*)
                       let (d,ptr,_,_) = Hashtbl.find (fst dynCache.(i).(j)) label in
                       (*log_son num "found";*)
                       let c = match !ptr with
                         | None ->
                            let c = d.dyn_contents () in
                            ptr := Some c; c
                         | Some c -> c
                       in
                       Printf.sprintf "{\"dyn_label\":\"%s\", \"dyn_contents\":\"%s\"}"
                                      d.dyn_label (base64_encode c)::acc)
                     [] l
           in
           if l = [] then raise Exit;
           let full = "[" ^ String.concat "," l ^"]" in
           i, j, "\"Dynamics\"", full
      in
      resp_slave a (
        Printf.sprintf "{ \"slide\":%d, \"state\":%d, \"change\":%s, \"change_list\":%s }"
          slide state
          change change_list
      );
    with
    | Exit -> ()
    | e-> log_son num "unexpected exception in pushto: %s" (Printexc.to_string e)
  in

  let in_group son g = match son.sessid with
      None -> false
    | Some(_,g') -> g = g'
  in

  let push from s groupid dest = (*from avoids to send back to the expeditor*)
    sessid := Some(s,groupid,[]); (* dirty ... very hard to fix *)
    List.iter (fun (fd,son) ->
        if fd <> from && in_group son groupid then (
          log_father "to [son%d] change" son.num;
          output_value son.df dest;
          flush son.df
      )) !sonsBySock

  in


  let http_send ?sessid code ctype datas ouc =
    Printf.fprintf ouc "HTTP/1.1 %d OK\r\n" code;
    Printf.fprintf ouc "Content-type: %s\r\n" ctype;
    let len = List.fold_left (fun acc s -> acc + String.length s) 0 datas in
    Printf.fprintf ouc "Content-Length: %d\r\n" len;
    (match sessid with
      Some (sessid, groupid, friends) ->
        Printf.fprintf ouc "Set-Cookie: SESSID=%s;\r\n" sessid;
        Printf.fprintf ouc "Set-Cookie: GROUPID=%s;\r\n" groupid;
        let str = Db.friends_to_string friends in
        Printf.fprintf ouc "Set-Cookie: FRIENDS=%s;\r\n" str;
    | None -> ());
    output_string ouc "\r\n";
    List.iter (output_string ouc) datas;
    output_string ouc "\r\n";
    flush ouc
  in

  let generate_error ?(message="Erreur") ouc =
    Printf.eprintf "sent 404\n%!";
    http_send 404 "text/plain" [message] ouc;
  in

  let http_send_file ?sessid code ctype path ouc =
    let ch = open_in path in
    let len = in_channel_length ch in
    Printf.fprintf ouc "HTTP/1.1 %d OK\r\n" code;
    Printf.fprintf ouc "Content-type: %s\r\n" ctype;
    Printf.fprintf ouc "Content-Length: %d\r\n" len;
    (match sessid with
      Some (sessid, groupid, friends) ->
        Printf.fprintf ouc "Set-Cookie: SESSID=%s;\r\n" sessid;
        Printf.fprintf ouc "Set-Cookie: GROUPID=%s;\r\n" groupid;
        let str = Db.friends_to_string friends in
        Printf.fprintf ouc "Set-Cookie: FRIENDS=%s;\r\n" str;
    | None -> ());
    output_string ouc "\r\n";
    let buf = Buffer.create 4096 in
    let remain = ref len in
    while !remain > 0 do
      Buffer.add_channel buf ch (min 4096 !remain);
      Buffer.output_buffer ouc buf;
      Buffer.clear buf;
      remain := !remain - 4096;
     done;
    output_string ouc "\r\n";
    flush ouc
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
      generate_error ~message:"serve svg failed" ouc;
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
      Not_found->generate_error ~message:"serve font failed" ouc
  in


  let serve_css ouc=
    http_send 200 "text/css" [css] ouc;
  in

  let serve_static_file folder filename ouc =
    try
      let path = Filename.concat folder filename in
      Printf.eprintf "path: %S\n%!" path;
      let ext=type_from_extension filename in
      Printf.eprintf "type: %S\n%!" ext;
      http_send_file 200 ext path ouc;
    with _ ->
      generate_error ouc
  in

  let serve fdfather num fd =

    Unix.clear_nonblock fd;
    let websocket = ref false in
    let inc=Unix.in_channel_of_descr fd in
    let finc=Unix.in_channel_of_descr fdfather in
    let ouc=Unix.out_channel_of_descr fd in
    let fouc=Unix.out_channel_of_descr fdfather in
    let sessid = Db.sessid in
    sessid := None;
    Random.self_init ();
    reset_cache ();
    let cur_slide = ref 0 and cur_state = ref 0 in

    let read_slide_state get =
      let asked_slide=max 0 (int_of_string (Str.matched_group 1 get)) in
      let state=max 0 (int_of_string (Str.matched_group 2 get)) in

      let slide=min asked_slide (Array.length slides-1) in
      let state=if asked_slide>slide then
                  (Array.length slides.(slide)-1)
                else
                  min state (Array.length slides.(slide)-1)
      in
      cur_slide := slide; cur_state := state;
      slide, state
  in

    let set_sessid (login,group,friends) =
      let friends = Db.friends_from_string friends in
      match !sessid with
      | Some(s,g,fs) when s = login && g = group && fs = friends->
         log_son num "reuse sessid: %s from %s" s g;
         fs
      | None ->
         log_son num "Set sessid: %s from %s" login group;
         sessid := Some(login,group,friends);
         friends
      | Some(s,g,_) ->
         log_son num "cancel sessid: %s from %s" s g;
         log_son num "set sessid: %s from %s" login group;
         sessid := Some(login,group,friends);
         friends
    in

    let reject_unlogged () =
      if !no_guest then begin
        match !sessid with
        | Some (s,g,_) when g = "guest" -> raise Exit
        | None -> raise Exit
        | _ -> ()
      end
    in

    let check_guest () =
      match !sessid with
      | Some (s,g,_) when g != "guest" ->
         log_son num "cancel sessid: %s from %s" s g;
         let s = make_sessid () in
         sessid := Some(s, "guest",[]);
         Printf.eprintf "New sessid: %s as guest" s;
      | None ->
         let s = make_sessid () in
         sessid := Some(s, "guest",[]);
         log_son num "new sessid: %s as guest" s;
      | _ -> ()
    in

    let read_sessid () = match !sessid with
      | Some (s,g,_) -> s, g
      | _ ->
         Printf.eprintf "Session not set\n%!";
         exit 1;
    in

    let read_sessid_fs () = match !sessid with
      | Some (s,g,fs) -> s, g, fs
      | _ ->
         Printf.eprintf "Session not set\n%!";
         exit 1;
    in


    let update slide state name ev =
      try
        let (_,buttons) = dynCache.(slide).(state) in
        let button = try Hashtbl.find buttons name
                     with Not_found ->
                       log_son num "unknown button %S" name;
                       raise Exit
        in
        let affected = match ev, button with
          | EvClick, Click(act) -> act ()
          | EvDrag(x,y,r), Drag(act) -> act (x, y) r
          | EvEdit(contents), Edit(_,_,act) -> act contents
          | EvMenu(id), Menu(items) -> fst (List.nth items id) ()
          | _ ->  log_son num "button %S do not match" name;
                  assert false
        in
        let to_push = ref [] in
        let priv, pub =
          List.fold_left (fun (priv,pub) (_,vis as key) ->
              let ds = try Hashtbl.find graph key
                       with Not_found -> []
              in
              let priv =
                List.fold_left (fun acc (d,ptr,i,j) ->
                    ptr := None;
                    if i = slide && j = state
                       && not (List.mem d.dyn_label !to_push) then
                      to_push := d.dyn_label :: !to_push;
                    if List.mem d.dyn_label acc then acc
                    else d.dyn_label::acc) priv ds
              in
              let ds =
                if snd key = Private then
                  (try Hashtbl.find graph (fst key, Group) (* Group differently from public *)
                  with Not_found -> [])
                  @
                  (try Hashtbl.find graph (fst key, Public)
                  with Not_found -> [])                else ds
              in
              let pub =
                List.fold_left (fun acc (d,ptr,i,j) ->
                    let t = (d.dyn_label,i,j) in
                    ptr := None;
                    if i = slide && j = state
                       && not (List.mem d.dyn_label !to_push) then
                      to_push := d.dyn_label :: !to_push;
                    if List.mem t acc then acc else t::acc) pub ds
              in
              (priv,pub)) ([], []) affected
        in
        if !to_push <> [] then
          begin
            log_son num "private change (%d,%d)" slide state;
            pushto ~change:(Dynamics(slide,state,!to_push)) num fd;
          end;
        let s, g = read_sessid () in
        if pub <> [] then
          begin
            let pub = String.concat " " (List.map (fun (d,i,j) ->
                                             Printf.sprintf "%s,%d,%d" d i j) pub) in
            log_son num "public change coucou %s %s %s" s g pub;
            Printf.fprintf fouc "change %s %s %s\n%!" s g pub;
          end
      with Not_found -> ()
    in

    let rec process_req master get hdr reste =
      if !websocket then (

      let socks, _, _ = Unix.select [fd;fdfather] [] [] 300. in
      match socks with
      | [] -> log_son num "quit because no ping"; exit 0
      | ch::_ when ch == fdfather ->
         begin
           log_son num "from father change";
           let dest = input_value finc in
           let dest =
             List.fold_left
               (fun acc (dest,i,j) ->
                 let (d,ptr,_,_) = Hashtbl.find (fst dynCache.(i).(j)) dest in
                 ptr := None;
                 if i = !cur_slide && j = !cur_state && not (List.mem dest acc) then
                   dest::acc
                 else
                   acc) [] dest
           in
           pushto ~change:(Dynamics(!cur_slide,!cur_state,dest)) num fd;
           process_req master get hdr reste
         end
      | ch::_ when ch != fd -> assert false
      | ch::_ ->
        let get = decode_slave inc in

        if Str.string_match move get 0 then (
          let slide, state = read_slide_state get in
          log_son num "websocket move (%d,%d) (send to websocket and father)" slide state;
          pushto ~change:(Slide(slide,state)) num fd;
          let s, g = read_sessid () in
          Printf.fprintf fouc "move %s %s %d %d\n%!" s g slide state;
          process_req master "" [] reste)
        else if Str.string_match refresh get 0 then (
          let slide, state = read_slide_state get in
          log_son num "websocket refresh (%d,%d) (send to websocket and father)" slide state;
          Hashtbl.iter (fun label (dyn,ptr,_,_) -> ptr := None) (fst dynCache.(slide).(state)) ;
          pushto ~change:(Slide(slide,state)) num fd;
          let s, g = read_sessid () in
          Printf.fprintf fouc "move %s %s %d %d\n%!" s g slide state;
          process_req master "" [] reste)
        else if Str.string_match click get 0 then (
          let match_end = Str.match_end () in
          let slide, state = read_slide_state get in
          let name =String.sub get match_end (String.length get - match_end) in
          log_son num "websocket click (%d,%d) %s" slide state name;
          update slide state name EvClick;
          process_req master "" [] reste)
        else if Str.string_match menu get 0 then (
          let slide, state = read_slide_state get in
          let name = Str.matched_group 3 get in
          let id = int_of_string (Str.matched_group 4 get) in
          log_son num "websocket menu (%d, %d) %s %d" slide state name id;
          update slide state name (EvMenu id);
          process_req master "" [] reste)
        else if Str.string_match edit get 0 then (
          let match_end = Str.match_end () in
          let slide, state = read_slide_state get in
          let rest =String.sub get match_end (String.length get - match_end) in
          log_son num "websocket edit (%d, %d) %s" slide state rest;

          let name, contents =
            try
              match Util.split ' ' rest with
                [name;contents] ->
                  name, base64_decode contents
              | _ -> raise Not_found
            with _ -> failwith "Bad edit"
          in
          update slide state name (EvEdit(contents));
          process_req master "" [] reste

        ) else if Str.string_match drag get 0 then (
          let match_end = Str.match_end () in
          let dx = float_of_string (Str.matched_group 3 get)
          and dy = float_of_string (Str.matched_group 4 get) in
          let release = try ignore (Str.matched_group 5 get); true with _ -> false in
          let slide, state = read_slide_state get in
          let name =String.sub get match_end (String.length get - match_end) in

          log_son num "websocket drag (%d,%d) %g %g %s" slide state dx dy name;

          update slide state name (EvDrag(dx,dy,release));
          process_req master "" [] reste
        ) else if Str.string_match ping get 0 then (
          log_son num "websocket ping";
          pushto ~change:Ping num fd;
          process_req master "" [] reste
        ) else
            process_req master "" [] reste
      ) else
        let x=input_line inc in
        if x.[0]='\r' then (
          if Str.string_match svg get 0 then (
            let _ = reject_unlogged () in
            log_son num "client get svg %S" get;
            let i=int_of_string (Str.matched_group 1 get) in
            let j=int_of_string (Str.matched_group 2 get) in
            serve_svg i j num (read_sessid ()) ouc;
            process_req master "" [] reste

          ) else if Str.string_match rmaster get 0 && Str.matched_group 1 get = !master_page then (
            log_son num "master";
            let _ = check_guest in
            http_send ~sessid:(read_sessid_fs ()) 200 "text/html" [page] ouc;
            process_req true "" [] reste

          ) else if Str.string_match logged get 0 then (
            let login = Str.matched_group 1 get in
            let md5 = Str.matched_group 2 get in
            let groupid = try Str.matched_group 4 get with Not_found -> "guest" in
            let friends = try Str.matched_group 6 get with Not_found -> "" in
            let key = login ^ "+" ^ groupid ^ friends ^ !secret in
            let md5' = Digest.to_hex(Digest.string key) in
            log_son num "logged '%s' from '%s' friends '%s' (%s = %s) %s"
              login groupid friends md5 md5' key;
            if md5 = md5' then (
              let friends = set_sessid (login, groupid, friends) in
              http_send ~sessid:(login,groupid, friends) 200 "text/html" [page] ouc;
              process_req false "" [] reste
            ) else (
              sessid := None;
              generate_error ~message:"md5 do not match" ouc;
            )

          ) else if Str.string_match slave get 0 then (
            log_son num "slave (%s)" get;
            let _ = reject_unlogged () in
            let _ = check_guest in
            http_send ~sessid:(read_sessid_fs ()) 200 "text/html" [page]  ouc;
            process_req false "" [] reste

          ) else if get="/etat" then (
            log_son num "etat";
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
              Printf.sprintf
                "  { \"sessid\" = %s, \"addr\" = %S, \"num\" = %d, \"pid\" = %d, \"slide\" = %d, \"state\" = %d }"
                (str_sessid son.sessid) (str_addr son.addr) son.num son.pid son.slide son.state) !sonsBySock in
            let son_descr = String.concat ",\n" son_descr in
            Buffer.add_string data (Printf.sprintf "\"sons\"=[\n%s\n]" son_descr);
            Buffer.add_char data '}';

            http_send 200 "text/plain" [Buffer.contents data] ouc;
            close_out ouc; (* /etat: must close to get accurate repporting *)
            close_out fouc; exit 0
          ) else if Str.string_match tire get 0 || get="/tire" then (
            let _ = reject_unlogged () in
            let slide, state =if get = "/tire" then -1, -1 else
                read_slide_state get
            in

            log_son num "tire (%d,%d)" slide state;
            try
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
                flush ouc
              end;

              let s, g = read_sessid () in
              Printf.fprintf fouc "move %s %s %d %d\n%!" s g slide state;
              websocket := true;

              process_req master "" [] reste

            with
            | e-> log_son num "erreur websocket %S" (Printexc.to_string e);

          ) else if Str.string_match css_reg get 0 then (
            let _ = reject_unlogged () in
            log_son num "css";
            serve_css ouc;
            process_req master "" [] reste

          ) else if Str.string_match otf get 0 then (
            let _ = reject_unlogged () in
            let otf = Str.matched_group 1 get in

            log_son num "otf %S" otf;
            serve_font otf ouc;
            process_req master "" [] reste

          ) else if !static_folder <> "" && Str.string_match static_reg get 0 then (
            let _ = reject_unlogged () in

            let filename = Str.matched_group 1 get in
            log_son num "static: %S" filename;

            serve_static_file !static_folder filename ouc;
            process_req master "" [] reste
          ) else (
            let _ = reject_unlogged () in
            let name = String.sub get 1 (String.length get-1) in

            (try
              log_son num "image or js: %S" name;
              let img=StrMap.find name imgs in
              let ext=type_from_extension name in
              http_send 200 ext [img] ouc;
              fun () -> process_req master "" [] []
             with
               Not_found->
               generate_error ~message:("serve file '"^name^"'failed") ouc;
               fun () -> process_req master "" [] reste) ()
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
            | _ -> acc) [] (List.rev ls)
          in
          (try ignore(set_sessid (List.assoc "SESSID" ls,
                                  List.assoc "GROUPID" ls,
                                  List.assoc "FRIENDS" ls))
           with Not_found -> ());
          process_req master get hdr reste)
        else if a = "X-Real-IP" || a = "X-Forwarded-For" then (
          Printf.fprintf fouc "addr %s\n%!" b;
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
    | e ->
       (match !sessid with
          None ->
          Printf.fprintf fouc "quit ? %d\n%!" (Unix.getpid ());
        | Some(sessid,_,_) ->
           Printf.fprintf fouc "quit %s %d\n%!" sessid (Unix.getpid ()));
       Printf.eprintf "erreur %d : \"%s\"\n%!" num (Printexc.to_string e);
       exit 0;
  in

  if !master_page="" then (
    master_page:=Printf.sprintf "/%d" (Random.int (1 lsl 29));
  );
  if !master_page.[0] <> '/' then master_page := "/" ^ !master_page;

  ignore (Sys.signal 13 (Sys.Signal_ignore));

  while true do
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
(*          Printf.eprintf
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

      log_father "Listening from %d addresses -- master: \"%s\""
        (List.length master_sockets) !master_page;

      let conn_num = ref 0 in

      Sys.(set_signal sigterm (Signal_handle (fun _ ->
        List.iter (fun (_,son) ->
          Unix.kill son.pid Sys.sigterm) !sonsBySock)));

      while true do
        log_father "in main loop (%d addresses, %d sons)"
                   (List.length master_sockets) (List.length !sonsBySock);
        (try while (fst (Unix.waitpid [WNOHANG] (-1)) <> 0) do () done with _ -> ());
        let socks,_,errors=Unix.select (master_sockets@List.map fst !sonsBySock) [] (List.map fst !sonsBySock) 30. in
        List.iter (fun sock -> kill_son sock) errors;
        List.iter (fun sock ->
          if not (List.mem sock errors) then (
          if not (List.mem sock master_sockets) then (
          let son = try List.assoc sock !sonsBySock with _ -> assert false in
          try
            let cmd = Util.split ' ' (input_line son.fd) in
            match cmd with
              ["move";sessid;groupid;slide;state] -> (
                let slide = int_of_string slide and state = int_of_string state in
                log_father "from [son%d] move (%d,%d)" son.num slide state;
                son.sessid <- Some (sessid, groupid);
                son.slide <- slide;
                son.state <- state;
                if son.sessid <> None then (
                  try
                    List.iter (fun (fd,son') ->
                      if son'.sessid = son.sessid && son.addr = son'.addr && son' != son then (
                        kill_son fd)) !sonsBySock
                  with
                    Not_found -> ())
              )
            | ["addr"; addr] ->
               let port = match son.addr with
                 | Unix.ADDR_UNIX _ -> 0
                 | Unix.ADDR_INET(_,p) -> p
               in
               log_father "from [son%d] addr %d" son.num port;
               son.addr <- Unix.ADDR_INET(inet_addr_of_string addr, port)
            | "change"::sessid::groupid::dest ->
               log_father "from [son%d] change" son.num;
               let dest = List.map (fun s ->
                              log_father ">>> dest: %S" s;
                              try
                                match Util.split ',' s with
                                | [d;i;j] -> (d, int_of_string i, int_of_string j)
                                | _ -> raise Exit
                              with _ ->
                                 log_father "malformed dest: %S" s;
                                 assert false) dest
               in
               push sock sessid groupid dest;

            | ["quit";sessid;pid] ->
               log_father "from [son%d] quit" son.num;
               kill_son sock;
            | _ ->
               log_father "from [son%d] BAD MESSAGE" son.num;
          with e ->
            log_father "from [son%d] exception: %S" son.num (Printexc.to_string e);
            kill_son sock
          ) else (
            log_father "Accepting a connection";
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
                Sys.(set_signal sigterm Signal_default);
                close_all_other conn_sock;
                log_son num "connection started";
                serve fd1 num conn_sock;
                assert false;
              with _ -> exit 0);
            Unix.close fd1;
            sonsBySock := (fd2,{ fd = in_channel_of_descr fd2;
                                 df = out_channel_of_descr fd2;
                           pid = pid; num = num; addr;
                           served_sock = conn_sock;
                           sessid = None; slide = 0; state = 0})::!sonsBySock))
          ) socks
      done)

    with e-> log_father "EXCEPTION %S" (Printexc.to_string e)
  done

let output = output_from_prime output'

let _ =
  Hashtbl.add DynDriver.drivers "Patonet" (module struct let output = output let output' = output' end:OutputDriver)
