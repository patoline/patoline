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
open Printf

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

let base64_encode s0=
  let m=String.length s0 mod 3 in
  let s=s0^(if m=1 then (String.make 2 (char_of_int 0)) else
      if m=2 then (String.make 1 (char_of_int 0)) else "")
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

let generate resp =
  let h=new Netmime.basic_mime_header [ "Content-type", "text/html" ] in
  let data=page in
  resp # send (`Resp_status_line (200, "OK"));
  resp # send (`Resp_header h);
  resp # send (`Resp_body (data, 0, String.length data));
  resp # send `Resp_end


type presentation={ mutable cur_slide:int; mutable cur_state:int; mutable starttime:float }
let present={cur_slide=0;cur_state=0;starttime=0.}
let mut=Mutex.create ()

module AddrMap=Map.Make(struct type t=Unix.sockaddr let compare=compare end)
let addrs:Unix.file_descr AddrMap.t ref=ref AddrMap.empty


let send_state resp=
  let h=new Netmime.basic_mime_header [ "Content-type", "application/json" ] in
  let time=Unix.time() in
  let data=sprintf "{ \"slide\": %d, \"state\": %d, \"time\": %g }"
    present.cur_slide present.cur_state
    (if present.starttime=0. then -1. else (time-.present.starttime))
  in
  resp # send (`Resp_status_line (200, "OK"));
  resp # send (`Resp_header h);
  resp # send (`Resp_body (data, 0, String.length data));
  resp # send `Resp_end

let send_state_xml resp=
  let h=new Netmime.basic_mime_header [ "Content-type", "text/xml" ] in
  let time=Unix.time() in
  let data=sprintf "<?xml version=\"1.0\"?>\n<presentation><slide>%d</slide><state>%d</state><time>%g</time></presentation>\n"
    present.cur_slide present.cur_state
    (if present.starttime=0. then -1. else (time-.present.starttime))
  in
  resp # send (`Resp_status_line (200, "OK"));
  resp # send (`Resp_header h);
  resp # send (`Resp_body (data, 0, String.length data));
  resp # send `Resp_end



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

    let mask=0 in
    let payload_len=min (String.length data - !pos) packet_len in
    if payload_len<=125 then (
      Buffer.add_char x (char_of_int ((mask lsl 7) lor payload_len));
    ) else if payload_len <= 0xffff then (
      Buffer.add_char x (char_of_int ((mask lsl 7) lor 126));
      Buffer.add_char x (char_of_int (payload_len lsr 8));
      Buffer.add_char x (char_of_int (payload_len land 0xff))
    ) else (
      Buffer.add_char x (char_of_int ((mask lsl 7) lor 126));
      Buffer.add_char x (char_of_int ((payload_len lsr 56) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 48) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 40) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 32) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 24) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 16) land 0xff));
      Buffer.add_char x (char_of_int ((payload_len lsr 8) land 0xff));
      Buffer.add_char x (char_of_int (payload_len land 0xff))
    );
    Buffer.add_substring x data !pos payload_len;
    let s=Buffer.contents x in
    let _=Unix.write fd s 0 (String.length s) in
    pos:= !pos+packet_len
  done

let push ()=
  addrs:=AddrMap.fold (fun k a m->
    try
      let time=Unix.time() in
      resp_slave a (sprintf "{ \"slide\":%d, \"state\":%d, \"time\":%g }" present.cur_slide present.cur_state (if present.starttime=0. then -1. else (time-.present.starttime)));
      AddrMap.add k a m
    with
        _->m
  ) !addrs AddrMap.empty




let generate_error resp =
  let h=new Netmime.basic_mime_header [ "Content-type", "text/html" ] in
  let data =
    "<html><head><title>Patoline</title></head><body>Patoline n'a malheureusement pas pu satisfaire votre demande</body></html>"
  in
  resp # send (`Resp_status_line (400, "Bad Request"));
  resp # send (`Resp_header h);
  resp # send (`Resp_body (data, 0, String.length data));
  resp # send `Resp_end


(* let serve_svg i j resp= *)
(*   let h = *)
(*     new Netmime.basic_mime_header *)
(*       [ "Content-type", "image/svg+xml" ] in *)
(*   if i<Array.length slides && j<Array.length slides.(i) then ( *)
(*     let data=slides.(i).(j) in *)
(*     resp # send (`Resp_status_line (200, "OK")); *)
(*     resp # send (`Resp_header h); *)
(*     resp # send (`Resp_body (data, 0, String.length data)); *)
(*     resp # send `Resp_end; *)
(*   ) else ( *)
(*     generate_error resp *)
(*   ) *)
(* ;; *)

let serve_svg i j resp=
  let h =
    new Netmime.basic_mime_header
      [ "Content-type", "application/json" ] in
  let time=Unix.time() in
  if i<Array.length slides && j<Array.length slides.(i) then (
    let data=Printf.sprintf "{\"svg\":%S, \"time\":%g}" slides.(i).(j)
      (if present.starttime=0. then -1. else (time-.present.starttime))
    in
    resp # send (`Resp_status_line (200, "OK"));
    resp # send (`Resp_header h);
    resp # send (`Resp_body (data, 0, String.length data));
    resp # send `Resp_end;
  ) else (
    let data=Printf.sprintf "{\"svg\":\"\", \"time\":%g}"
      (if present.starttime=0. then -1. else (time-.present.starttime))
    in
    resp # send (`Resp_status_line (400, "Invalid request"));
    resp # send (`Resp_header h);
    resp # send (`Resp_body (data, 0, String.length data));
    resp # send `Resp_end;
  )
;;

let serve_font font resp=
  let h =
    new Netmime.basic_mime_header
      [ "Content-type", "font/opentype" ] in
  try
    let data=List.assoc font fonts in
    resp # send (`Resp_status_line (200, "OK"));
    resp # send (`Resp_header h);
    resp # send (`Resp_body (data, 0, String.length data));
    resp # send `Resp_end;
  with
      Not_found->generate_error resp
;;


let serve addr fd =
  let config = Nethttpd_kernel.default_http_protocol_config in
  let proto = new Nethttpd_kernel.http_protocol config fd in

  let rec next_token times=
    if times>=max_int then `Timeout else (
      if proto # recv_queue_len = 0 then (
        proto # cycle ~block:1. ();
        next_token (times+1)
      ) else
        proto # receive()
    )
  in
  let cur_resp = ref None in
  let uri=ref "" in
  let websocket_key=ref None in
  let websocket_ok=ref false in
  let rec process_tok cur_tok=
    if cur_tok<>`Eof then (
      (match cur_tok with
        | `Req_header  (((meth, uri_), v), hdr, resp) ->
          (try
             websocket_key:=Some (hdr#field "Sec-Websocket-Key")
           with
               Not_found->());
          uri:=Netencoding.Url.decode uri_;
          Printf.fprintf stderr "uri : %S\n" !uri;flush stderr;
	  cur_resp := Some resp

        | `Req_expect_100_continue ->
	  ( match !cur_resp with
	    | Some resp -> resp # send Nethttpd_kernel.resp_100_continue
	    | None -> assert false
	  )
        | `Req_end ->
          let svg=Str.regexp "/[^_/]*_\\([0-9]*\\)_\\([0-9]*\\)\\.svg" in
          let pousse=Str.regexp "/pousse/\\([0-9]*\\)/\\([0-9]*\\)" in
          let otf=Str.regexp "/\\([^/]*\\.otf\\)" in
          if Str.string_match svg !uri 0 then (
            let i=int_of_string (Str.matched_group 1 !uri) in
            let j=int_of_string (Str.matched_group 2 !uri) in
            match !cur_resp with
	      | Some resp -> (
                Mutex.lock mut;
                let pi=present.cur_slide and pj=present.cur_state in
                Mutex.unlock mut;
                if i<pi || (i=pi && j<=pj) then
                  serve_svg i j resp
                else
                  generate_error resp
              )
	      | None -> assert false
          ) else if Str.string_match otf !uri 0 then (
            match !cur_resp with
	      | Some resp -> serve_font (Str.matched_group 1 !uri) resp
	      | None -> assert false
          ) else if Str.string_match pousse !uri 0 then (
            match !cur_resp with
	      | Some resp ->(
                let slide=max 0 (int_of_string (Str.matched_group 1 !uri)) in
                let slide=min slide (Array.length slides-1) in
                let state=max 0 (int_of_string (Str.matched_group 2 !uri)) in
                let state=min state (Array.length slides.(slide)-1) in
                Mutex.lock mut;
                if present.cur_slide<>slide || present.cur_state<>state then (
                  present.cur_slide<-slide;
                  present.cur_state<-state;
                  if present.starttime=0. && (present.cur_slide>0 || present.cur_state>0) then
                    present.starttime<-Unix.time();
                  push ();
                );
                Mutex.unlock mut;
                serve_svg slide state resp;
              )
	      | None -> assert false
          ) else if !uri="/current" then (
            match !cur_resp with
	      | Some resp ->send_state resp
	      | None -> assert false
          ) else if !uri="/curxml" then (
            match !cur_resp with
	      | Some resp ->send_state_xml resp
	      | None -> assert false
          ) else if !uri="/ajax" then (
            match !cur_resp with
	      | Some resp ->(
                let h =
                  new Netmime.basic_mime_header
                    [ "Content-type", "text/html" ] in
                resp # send (`Resp_status_line (200, "OK"));
                resp # send (`Resp_header h);
                resp # send (`Resp_body (master, 0, String.length master));
                resp # send `Resp_end;
              )
	      | None -> assert false
          ) else if !uri="/tire" then (
            match !cur_resp with
	      | Some resp ->(
                match !websocket_key with
                    None->()
                  | Some k->(
                    Mutex.lock mut;
                    addrs:=AddrMap.add addr fd !addrs;
                    Mutex.unlock mut;
                    websocket_ok:=true;
                    let sha=Cryptokit.Hash.sha1 () in
                    sha#add_string k;
                    sha#add_string "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
                    let key'=sha#result in
                    let h =
                      new Netmime.basic_mime_header
                        [ "Content-type", "text/html";
                          "Upgrade","websocket";
                          "Connection","upgrade";
                          "Sec-Websocket-Accept", (base64_encode key')
                        ]
                    in
                    resp # send (`Resp_info_line ((101,"switching"),h));
                  )
              )
	      | None -> assert false
          ) else if !uri="/next" || !uri="/prev" then (
            match !cur_resp with
	      | Some resp ->(
                Mutex.lock mut;
                let slide,state=
                  if !uri="/prev" then
                    if present.cur_state-1>=0 then
                      present.cur_slide, present.cur_state-1
                    else
                      if present.cur_slide-1>=0 then
                        present.cur_slide-1, (Array.length slides.(present.cur_slide-1)-1)
                      else
                        present.cur_slide,present.cur_state
                  else
                    if present.cur_state+1<Array.length slides.(present.cur_slide) then
                      present.cur_slide, present.cur_state+1
                    else
                      if present.cur_slide+1<Array.length slides then
                        present.cur_slide+1, 0
                      else
                        present.cur_slide,present.cur_state
                in
                Printf.fprintf stderr "next : %d %d\n" slide state;flush stderr;
                if present.cur_slide<>slide || present.cur_state<>state then (
                  present.cur_slide<-slide;
                  present.cur_state<-state;
                  if present.starttime=0. && (present.cur_slide>0 || present.cur_state>0) then
                    present.starttime<-Unix.time();
                  push ();
                );
                Mutex.unlock mut;
                send_state_xml resp
              )
	      | None -> assert false
          ) else (
            match !cur_resp with
	      | Some resp ->generate resp
	      | None -> assert false
          );
	  cur_resp := None

        | `Fatal_error e ->(
	  let name = Nethttpd_kernel.string_of_fatal_error e in
	  printf "Fatal_error: %s\n" name;
	  flush stdout;
        )
        | `Bad_request_error (e, resp) ->(
	  let name = Nethttpd_kernel.string_of_bad_request_error e in
	  printf "Bad_request_error: %s\n" name;
	  flush stdout;
	  generate_error resp
        )
        | `Timeout ->()
        | _ ->());
      if cur_tok<>`Timeout && not !websocket_ok then process_tok (next_token 0)
    )
  in
  process_tok (next_token 0);

  while proto # resp_queue_len > 0 do
    proto # cycle ~block:(-1.0) ();
  done;
  if not !websocket_ok then (
    proto # shutdown();
    (if proto # need_linger then (
      let lc = new Nethttpd_kernel.lingering_close fd in
      while lc # lingering do
        lc # cycle ~block:true ()
      done
     )
     else
        Unix.close fd
    )
  );
  Thread.exit ()


let start() =
  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  let port=8080 in
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, port));
  Unix.listen master_sock 100;
  printf "Listening on port %d\n" port;
  flush stdout;
  let accept_connections ()=
    while true do
      try
        let conn_sock, addr = Unix.accept master_sock in
      (* (match addr with *)
      (*     Unix.ADDR_INET (s,p)->Printf.fprintf stderr "serve %s %d\n" (Unix.string_of_inet_addr s) p *)
      (*   | _->assert false); *)
      (* flush stderr; *)
        Unix.set_nonblock conn_sock;
        let _=Thread.create (fun ()->serve addr conn_sock) () in
        ()
      with
	  Unix.Unix_error(Unix.EINTR,_,_) -> ()
    done
  in
  accept_connections ()

let conf_debug() =
  (* Set the environment variable DEBUG to either:
       - a list of Netlog module names
       - the keyword "ALL" to output all messages
       - the keyword "LIST" to output a list of modules
     By setting DEBUG_WIN32 additional debugging for Win32 is enabled.
   *)
  let debug = try Sys.getenv "DEBUG" with Not_found -> "" in
  if debug = "ALL" then
    Netlog.Debug.enable_all()
  else if debug = "LIST" then (
    List.iter print_endline (Netlog.Debug.names());
    exit 0
  )
  else (
    let l = Netstring_str.split (Netstring_str.regexp "[ \t\r\n]+") debug in
    List.iter
      (fun m -> Netlog.Debug.enable_module m)
      l
  );
  if (try ignore(Sys.getenv "DEBUG_WIN32"); true with Not_found -> false) then
    Netsys_win32.Debug.debug_c_wrapper true
;;

let _=
  Netsys_signal.init();
  conf_debug();
  start()
