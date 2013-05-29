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


let websocket w=
  Printf.sprintf "var websocket;
function start_socket(){
   if(websocket) websocket.close();
   websocket=new WebSocket(\"ws://\"+location.host+\"/tire\");
   websocket.onclose=function(evt){};
   websocket.onmessage = function(evt) {
     var st=JSON.parse(evt.data);
     if(st.slide==current_slide || current_slide==(-1)) {
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
   websocket.onerror = function(evt) { };
};
window.onbeforeunload = function() {
    websocket.onclose = function () {}; // disable onclose handler first
    websocket.close()
};
"
    (-.w)
    w

let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages filename=
  let prefix=try Filename.chop_extension filename with _->filename in
  let svg_files,cache,imgs=SVG.buffered_output' ~structure:structure pages prefix in
  let html,style=SVG.basic_html
    ~script:(websocket (fst (pages.(0)).(0).pageFormat))
    ~onload:"start_socket();"
    ~keyboard:""
    cache structure pages prefix
  in

  let w,h=if Array.length pages>0 then (pages.(0)).(0).pageFormat else 0.,0. in
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
    ~script:(websocket (fst (pages.(0)).(0).pageFormat))
    ~onload:"to=0;start_socket();websocket.onopen=function(){xhttp=new XMLHttpRequest();xhttp.open(\"GET\",\"pousse_\"+h0+\"_\"+h1,false);xhttp.send()};"
    ~onhashchange:"xhttp=new XMLHttpRequest();xhttp.open(\"GET\",\"pousse_\"+h0+\"_\"+h1,false);xhttp.send();"
    ~keyboard:master_keyboard
    cache structure pages prefix
  in

  let o=open_out (prefix^"_server.ml") in
  Printf.fprintf o "(* #PACKAGES netstring,netsys,unix,nethttpd,netcgi2,cryptokit *)\n";
  Printf.fprintf o "let page=%S\n" (Rbuffer.contents html);
  Printf.fprintf o "let master=%S\n" (Rbuffer.contents master);
  Printf.fprintf o "let css=%S\n" (Rbuffer.contents style);
  Printf.fprintf o "let slides=[|";
  let first_x=ref true in
  Array.iter (fun x->
    if not !first_x then Printf.fprintf o ";";
    first_x:=false;
    Printf.fprintf o "[|";
    let first_y=ref true in
    Array.iter (fun y->
      if not !first_y then Printf.fprintf o ";";
      first_y:=false;
      Printf.fprintf o "%S" (Rbuffer.contents y);
    ) x;
    Printf.fprintf o "|]";
  ) svg_files;
  Printf.fprintf o "|]\n";

  Printf.fprintf o "let fonts=[";
  let first_f=ref true in
  StrMap.iter (fun font buf->
    if not !first_f then Printf.fprintf o ";";
    first_f:=false;
    Printf.fprintf o "(%S,%S)" font (Rbuffer.contents buf)
  ) cache.fontBuffers;
  Printf.fprintf o "]\n";

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

  let patonet=
    let pato=findPath "patonet.ml" ((!Typography.Config.pluginspath)@["."]) in
    let patof=open_in pato in
    let s=String.create (in_channel_length patof) in
    really_input patof s 0 (String.length s);
    close_in patof;
    s
  in
  Printf.fprintf o "# 1 \"patonet.ml\"\n%s\n" patonet;
  close_out o;
  Printf.fprintf stdout "\nOCaml file %s.ml written. Compile with :\n\t" prefix;
  Printf.fprintf stdout "ocamlfind ocamlopt -o %s_server -package \"cryptokit,str\" -thread -linkpkg %s_server.ml\n\n" prefix prefix
