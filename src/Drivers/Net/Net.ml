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

open Typography.OutputCommon
open Typography.OutputPaper
open Typography.Util
open Typography.ConfigUtil
open HtmlFonts


let websocket is_master w=
  Printf.sprintf "var websocket;var was_error;
function websocket_msg(evt){
     var st=JSON.parse(evt.data);
     if(st.slide==current_slide || !first_displayed) {
         loadSlide(st.slide,st.state);
     } else if(st.slide<current_slide) {
         loadSlide(st.slide,st.state,function(a,b){slide(%g,a,b)})
     } else {
         loadSlide(st.slide,st.state,function(a,b){slide(%g,a,b)})
     }
     current_slide=st.slide;
     current_state=st.state;
     setTimeout(tout,to);
};
function websocket_err(evt){
was_error=true;
websocket.close();
};
function websocket_close(evt){if(!was_error){/*setTimeout(start_socket,1000)*/}};
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
    (-.w)
    w
    (if is_master then "+\"_\"+current_slide+\"_\"+current_state" else "")
    (if is_master then "+\"_\"+current_slide+\"_\"+current_state" else "")

let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages filename=
  let prefix=try Filename.chop_extension filename with _->filename in
  let svg_files,cache,imgs=SVG.buffered_output' ~structure:structure pages prefix in
  let html,style=SVG.basic_html
    ~script:(websocket false (fst (pages.(0)).(0).pageFormat))
    ~onload:"start_socket();"
    ~keyboard:""
    cache structure pages prefix
  in

  let master_keyboard=Printf.sprintf "window.onkeydown=function(e){
if(e.keyCode==37 || e.keyCode==38 || e.keyCode==33){
if(current_state<=0 || e.keyCode==38) {
  xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide-1)+\"_\"+(states[current_slide-1]-1),false);
  xhttp.send();
} else {
  xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide)+\"_\"+(current_state-1),false);
  xhttp.send();
}
} //left
if(e.keyCode==39 || e.keyCode==40 || e.keyCode==34){
if(current_state>=states[current_slide]-1 || e.keyCode==40) {
  xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide+1)+\"_0\",false);
  xhttp.send();
} else {
  xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide)+\"_\"+(current_state+1),false);
  xhttp.send();
}
} else //right
if(e.keyCode==82){ //r
  xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+(current_slide)+\"_\"+(current_state),false);
  xhttp.send();
}
}
function gotoSlide(n){
  xhttp=new XMLHttpRequest();
  xhttp.open(\"GET\",\"pousse_\"+n+\"_0\",false);
  xhttp.send();
  setTimeout(tout,to);
}"
  in


  let master,_=SVG.basic_html
    ~script:(websocket true (fst (pages.(0)).(0).pageFormat))
    ~onload:"to=0;start_socket();"
    ~onhashchange:"xhttp=new XMLHttpRequest();xhttp.open(\"GET\",\"pousse_\"+h0+\"_\"+h1,false);xhttp.send();"
    ~keyboard:master_keyboard
    cache structure pages prefix
  in

  let o=open_out (prefix^"_server.c") in
  Printf.fprintf o "char* page=%S;\n" (Rbuffer.contents html);
  Printf.fprintf o "char* master=%S;\n" (Rbuffer.contents master);
  Printf.fprintf o "char* css=%S;\n" (Rbuffer.contents style);

  let print_double_arr pref arr bin=
    let arr_len=(Array.map (Array.length) arr) in
    Printf.fprintf o "int n_%s=%d;" pref (Array.length arr);
    Printf.fprintf o "int n_%s_[]={%s};" pref
      (String.concat "," (List.map string_of_int (Array.to_list arr_len)));
    for i=0 to Array.length arr-1 do
      Printf.fprintf o "int n_%s_%d[]={%s};\n" pref i
        (String.concat "," (List.map string_of_int
                              (Array.to_list (Array.map Rbuffer.length arr.(i)))));
      Printf.fprintf o "char* %s%d[]={" pref i;
      for j=0 to Array.length arr.(i)-1 do
        if j>0 then Printf.fprintf o ",";
        if bin then (
          Printf.fprintf o "\"";
          for k=0 to Rbuffer.length arr.(i).(j)-1 do
            Printf.fprintf o "\\x%02x" (int_of_char (Rbuffer.nth arr.(i).(j) k));
          done;
          Printf.fprintf o "\"";
        ) else
          Printf.fprintf o "%S" (Rbuffer.contents arr.(i).(j));
      done;
      Printf.fprintf o "};\n";
    done;
    Printf.fprintf o "char** %s[]={" pref;
    for i=0 to Array.length arr-1 do
      if i>0 then Printf.fprintf o ",";
      Printf.fprintf o "%s%d" pref i;
    done;
    Printf.fprintf o "};\n";
    Printf.fprintf o "int* strlen_%s[]={" pref;
    for i=0 to Array.length arr-1 do
      if i>0 then Printf.fprintf o ",";
      Printf.fprintf o "n_%s_%d" pref i;
    done;
    Printf.fprintf o "};\n";
  in

  print_double_arr "slides" svg_files false;

  let bin=
    (List.map (fun (a,b)->
      let buf=Rbuffer.create (String.length a) in
      Rbuffer.add_string buf a;
      [|buf;b|]
     ) (StrMap.bindings cache.fontBuffers))
    @
    (List.map (fun (a,b)->
      let buf=Rbuffer.create 10000 in
      let i=open_in a in
      Rbuffer.add_channel buf i (in_channel_length i);
      close_in i;
      let buf'=Rbuffer.create (String.length b) in
      Rbuffer.add_string buf b;
      [|buf';buf|]
     ) (StrMap.bindings imgs))
  in
  print_double_arr "bin"
    (Array.of_list (List.sort (fun a b->compare a.(0) b.(0)) bin))
    true;

(*
  let buf=Rbuffer.create 2000 in
  Printf.fprintf o "let imgs=[";
  first_f:=true;
  StrMap.iter (fun img k->
    if not !first_f then Printf.fprintf o ";";
    first_f:=false;

    Rbuffer.clear buf;
    let i=open_in img in
    Rbuffer.add_channel buf i (in_channel_length i);
    close_in i;

    Printf.fprintf o "(%S,%S)" k (Rbuffer.contents buf)
  ) imgs;
  Printf.fprintf o "]\n";
*)
  let patonet=
    let pato=Typography.FindPath.findPath "patonet.c" ((!Typography.Config.pluginspath)@["."]) in
    let patof=open_in pato in
    let s=String.create (in_channel_length patof) in
    really_input patof s 0 (String.length s);
    close_in patof;
    s
  in
  Printf.fprintf o "# 1 \"patonet.c\"\n%s\n" patonet;
  close_out o;
  Printf.fprintf stdout "\nServer %s.c written. Compile with:\n\tgcc $(pkg-config --libs gnutls) -lgcrypt -o %s_server %s_server.c\n\n" prefix prefix prefix
