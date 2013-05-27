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


type presentation={ mutable cur_slide:int; mutable cur_state:int; mutable starttime:float }
let present={cur_slide=0;cur_state=0;starttime=0.}
let mut=Mutex.create ()

module AddrMap=Map.Make(struct type t=Unix.sockaddr let compare=compare end)
let addrs:Unix.file_descr AddrMap.t ref=ref AddrMap.empty


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
    let _=Unix.write fd s 0 (String.length s) in
    pos:= !pos+packet_len
  done

let pushto a=
  let time=Unix.time() in
  resp_slave a (Printf.sprintf "{ \"slide\":%d, \"state\":%d, \"time\":%g }" present.cur_slide present.cur_state (if present.starttime=0. then 0. else (time-.present.starttime)))

let push ()=
  addrs:=AddrMap.fold (fun k a m->
    try
      pushto a;
      AddrMap.add k a m
    with
        _->m
  ) !addrs AddrMap.empty




let serve_svg i j ouc=
  if i<Array.length slides && j<Array.length slides.(i) then (
    let data=Printf.sprintf "%s" slides.(i).(j) in
    output_string ouc "HTTP/1.1 200 OK\r\n";
    output_string ouc "Content-type: image/svg+xml\r\n";
    Printf.fprintf ouc "Content-Length: %d\r\n" (String.length data);
    output_string ouc "\r\n";
    output_string ouc data;
    output_string ouc "\r\n";
    flush ouc

  ) else (
    let data="400 Invalid request" in
    output_string ouc "HTTP/1.1 400 Invalid request\r\n";
    Printf.fprintf ouc "Content-Length: %d\r\n" (String.length data);
    output_string ouc "\r\n";
    output_string ouc data;
    output_string ouc "\r\n";
    flush ouc
  )


let generate_error ouc=
  let data =
    "<html><head><title>Patoline</title></head><body>Patoline n'a malheureusement pas pu satisfaire votre demande</body></html>"
  in
  Printf.fprintf ouc "HTTP/1.1 404 Not found\r\n";
  Printf.fprintf ouc "Content-type: text/html\r\n";
  Printf.fprintf ouc "Content-Length: %d\r\n" (String.length data);
  Printf.fprintf ouc "\r\n";
  output_string ouc data;
  Printf.fprintf ouc "\r\n";
  flush ouc

let serve_font font ouc=
  try
    let data=List.assoc ("slides/"^font) fonts in
    Printf.fprintf ouc "HTTP/1.1 200 OK\r\n";
    Printf.fprintf ouc "Content-type: font/opentype\r\n";
    Printf.fprintf ouc "Content-Length: %d\r\n" (String.length data);
    Printf.fprintf ouc "\r\n";
    output_string ouc data;
    Printf.fprintf ouc "\r\n";
    flush ouc
  with
      Not_found->generate_error ouc
;;

let serve_css ouc=
  output_string ouc "HTTP/1.1 200 OK\r\n";
  output_string ouc "Content-type: text/css\r\n";
  Printf.fprintf ouc "Content-Length: %d\r\n" (String.length css);
  output_string ouc "\r\n";
  output_string ouc css;
  output_string ouc "\r\n";
  flush ouc

let master_page=ref ""

let svg=Str.regexp "/\\([0-9]*\\)_\\([0-9]*\\)\\.svg"
let css=Str.regexp "/style\\.css"
let pousse=Str.regexp "/pousse_\\([0-9]*\\)_\\([0-9]*\\)"
let otf=Str.regexp "/\\([^\\.]*\\.otf\\)"


let get_reg=Str.regexp "GET \\([^ ]*\\) .*"
let header=Str.regexp "\\([^ :]*\\) *: *\\([^\r]*\\)"


let serve addr fd=
  Unix.clear_nonblock fd;
  let inc=Unix.in_channel_of_descr fd in
  let ouc=Unix.out_channel_of_descr fd in
  let rec process_req get hdr reste=
    let x=input_line inc in
    if x.[0]='\r' then (

      if Str.string_match svg get 0 then (
        let i=int_of_string (Str.matched_group 1 get) in
        let j=int_of_string (Str.matched_group 2 get) in
        Mutex.lock mut;
        let pi=present.cur_slide and pj=present.cur_state in
        Mutex.unlock mut;
        if i<pi || (i=pi && j<=pj) then
          serve_svg i j  ouc
        else
          generate_error ouc;
        process_req "" [] []

      ) else if get= !master_page then (
        output_string ouc "HTTP/1.1 200 OK\r\n";
        output_string ouc "Content-type: text/html\r\n";
        Printf.fprintf ouc "Content-Length: %d\r\n" (String.length master);
        output_string ouc "\r\n";
        output_string ouc master;
        output_string ouc "\r\n";
        flush ouc;
        process_req "" [] []

      ) else if get="/" then (
        output_string ouc "HTTP/1.1 200 OK\r\n";
        output_string ouc "Content-type: text/html\r\n";
        Printf.fprintf ouc "Content-Length: %d\r\n" (String.length page);
        output_string ouc "\r\n";
        output_string ouc page;
        output_string ouc "\r\n";
        flush ouc;
        process_req "" [] []

      ) else if get="/etat" then (
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

        output_string ouc "HTTP/1.1 200 OK\r\n";
        output_string ouc "Content-type: text/plain\r\n";
        Printf.fprintf ouc "Content-Length: %d\r\n" (Buffer.length data);
        output_string ouc "\r\n";
        Buffer.output_buffer ouc data;
        output_string ouc "\r\n";
        flush ouc;
        process_req "" [] []

      ) else if get="/tire" then (
        try
          begin
            try
              let key=
                let websocket_key=List.assoc "Sec-WebSocket-Key" hdr in
                let sha=Cryptokit.Hash.sha1 () in
                sha#add_string websocket_key;
                sha#add_string "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
                base64_encode (sha#result)
              in
              output_string ouc "HTTP/1.1 101 Switching\r\nUpgrade: websocket\r\nConnection: upgrade\r\nSec-Websocket-Accept: ";
              output_string ouc key;
              output_string ouc "\r\n\r\n";
              flush ouc;
            with
              (* Version Apple *)
                Not_found->(
                  let key=
                    let wk1=List.assoc "Sec-WebSocket-Key1" hdr in
                    let wk2=List.assoc "Sec-WebSocket-Key2" hdr in
                    let wk1_dig=Buffer.create (String.length wk1) in
                    let wk2_dig=Buffer.create (String.length wk1) in
                    let wk1_sp=ref 0 in
                    let wk2_sp=ref 0 in
                    for i=0 to String.length wk1-1 do
                      if wk1.[i]>='0' && wk1.[i]<='9' then Buffer.add_char wk1_dig wk1.[i]
                      else if wk1.[i]=' ' then incr wk1_sp
                    done;
                    for i=0 to String.length wk2-1 do
                      if wk2.[i]>='0' && wk2.[i]<='9' then Buffer.add_char wk2_dig wk2.[i]
                      else if wk2.[i]=' ' then incr wk2_sp
                    done;
                    let wk1=int_of_string (Buffer.contents wk1_dig)/(min 1 !wk1_sp) in
                    let wk2=int_of_string (Buffer.contents wk1_dig)/(min 1 !wk2_sp) in
                    let k=String.create 16 in
                    k.[0]<-(char_of_int ((wk1 lsr 24) land 0xff));
                    k.[1]<-(char_of_int ((wk1 lsr 16) land 0xff));
                    k.[2]<-(char_of_int ((wk1 lsr 8) land 0xff));
                    k.[3]<-(char_of_int (wk1 land 0xff));

                    k.[4]<-(char_of_int ((wk2 lsr 24) land 0xff));
                    k.[5]<-(char_of_int ((wk2 lsr 16) land 0xff));
                    k.[6]<-(char_of_int ((wk2 lsr 8) land 0xff));
                    k.[7]<-(char_of_int (wk2 land 0xff));
                    let _=input inc k 8 8 in
                    let md5=Cryptokit.Hash.md5 () in
                    md5#add_string k;
                    md5#result
                  in
                  let orig=List.assoc "Origin" hdr in

                  output_string ouc "HTTP/1.1 101 WebSocket Protocol Handshake";
                  output_string ouc "\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nSec-WebSocket-Origin: ";
                  let ws=
                    try
                      let i=String.index orig '/' in
                      String.sub orig (i+2) (String.length orig-i-2)
                    with
                        Not_found->orig
                  in
                  output_string ouc orig;
                  output_string ouc "\r\nSec-WebSocket-Location: ws://";
                  output_string ouc ws;
                  output_string ouc "/tire\r\n\r\n";
                  output_string ouc key;
                  flush ouc;
                );
          end;
          pushto fd;
          Mutex.lock mut;
          addrs:=AddrMap.add addr fd !addrs;
          Mutex.unlock mut;

        with
            Not_found->(
              Mutex.lock mut;
              addrs:=AddrMap.add addr fd !addrs;
              Mutex.unlock mut;
            )

      ) else if Str.string_match pousse get 0 then (

        let slide=max 0 (int_of_string (Str.matched_group 1 get)) in
        let slide=min slide (Array.length slides-1) in
        let state=max 0 (int_of_string (Str.matched_group 2 get)) in
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
        let notfound="Ok" in
        Printf.fprintf ouc "HTTP/1.1 200 OK\r\nContent-length: %d\r\n\r\n%s\r\n"
        (String.length notfound) notfound;
        flush ouc;

        process_req "" [] reste

      ) else if Str.string_match css get 0 then (
        serve_css ouc;
        process_req "" [] reste

      ) else if Str.string_match otf get 0 then (
        serve_font (Str.matched_group 1 get) ouc;
        process_req "" [] reste

      ) else (

        try
          let img=List.assoc (String.sub get 1 (String.length get-1)) imgs in
          let ext=
            if Filename.check_suffix ".png" get then "image/png" else
            if Filename.check_suffix ".jpeg" get then "image/jpeg" else
            if Filename.check_suffix ".jpg" get then "image/jpg" else
            if Filename.check_suffix ".gif" get then "image/gif" else
              "application/octet-stream"
          in
          output_string ouc "HTTP/1.1 200 OK\r\n";
          output_string ouc "Content-type: ";
          output_string ouc ext;
          Printf.fprintf ouc "\r\nContent-Length: %d\r\n" (String.length img);
          output_string ouc "\r\n";
          output_string ouc img;
          output_string ouc "\r\n";
          flush ouc;
          process_req "" [] []
        with
            Not_found->(
              let notfound="Not found" in
              Printf.fprintf ouc "HTTP/1.1 404 Not_found\r\nContent-length: %d\r\n\r\n%s\r\n"
                (String.length notfound) notfound;
              flush ouc;
              process_req "" [] reste
            );
      )

    ) else (
      if hdr=[] && Str.string_match get_reg x 0 then (
        process_req (Str.matched_group 1 x) hdr reste
      ) else if Str.string_match header x 0 then (
        let a=Str.matched_group 1 x in
        let b=Str.matched_group 2 x in
        process_req get ((a,b)::hdr) reste
      ) else (
        process_req get hdr (x::reste)
      );
    )
  in
  try
    process_req "" [] []
  with
      _->(Unix.close fd)

let spec=
  [("-master",Arg.Set_string master_page,"Set the master page")]

let _=
  Arg.parse spec (fun x->()) "";
  if !master_page="" then (
    Random.self_init ();
    master_page:=Printf.sprintf "/%d" (Random.int (1 lsl 29));
  );
  if !master_page.[0]<>'/' then master_page:="/"^(!master_page);

  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  let port=8080 in
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, port));
  Unix.listen master_sock 100;


  Printf.printf "Listening on port %d -- master: \"%s\"\n" port !master_page;
  flush stdout;
  let accept_connections ()=
    while true do
      try
        let conn_sock, addr = Unix.accept master_sock in
        Unix.set_nonblock conn_sock;
        let _=Thread.create (fun ()->serve addr conn_sock) () in
        ()
      with
	  Unix.Unix_error(Unix.EINTR,_,_) -> ()
    done
  in
  accept_connections ()
