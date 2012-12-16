(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

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
open HtmlFonts



let add_header title w h html=
  Rbuffer.add_string html
    "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>";
  Rbuffer.add_string html title;
  Rbuffer.add_string html "</title>\n";
  Rbuffer.add_string html (Printf.sprintf "<script>
var current_slide=0;
var current_state=0;
var server_current_slide=0;
var server_current_state=0;
resize=function(){
sizex=(window.innerWidth)/%g;
sizey=(window.innerHeight)/%g;
size=sizex>sizey ? sizey : sizex;
svg=document.getElementById(\"svg\");
svg.style.width=(%g*size)+'px';
svg.style.height=(%g*size)+'px';
};
" w h (w-.10.) (h-.10.))


let add_slide html=
  Rbuffer.add_string html "function slide(width,g0,g1){
  var svg=document.getElementsByTagName(\"svg\")[0];
  g0.setAttribute(\"transform\",\"translate(\"+width+\" 0)\");
  svg.appendChild(g0);

  var i=0;
  var slideTimer;
  var n=60;
  var do_slide=function(){
    if(i<=n){
      g0.setAttribute(\"transform\",\"translate(\"+width*(n-i)/n+\" 0)\");
      if(g1) g1.setAttribute(\"transform\",\"translate(\"+width*(-i)/n+\" 0)\");
      i++;
    } else {
      clearInterval(slideTimer);
      if(g1) svg.removeChild(g1);
    }
  }
  slideTimer=setInterval(do_slide,0.0005);
}
"

let add_loadslide prefix w h html=
  Rbuffer.add_string html (
    Printf.sprintf "function loadSlide(n,state,effect){
    var svg_xhttp=new XMLHttpRequest();
    svg_xhttp.open(\"GET\",\"%s_\"+n+\"_\"+state+\".svg\",false);
    svg_xhttp.send();
    if(svg_xhttp.status==200){
    var json=JSON.parse(svg_xhttp.responseText);
    var parser=new DOMParser();
    var newSvg=parser.parseFromString(json.svg,\"image/svg+xml\");

    var svg=document.getElementsByTagName(\"svg\")[0];

    newSvg=document.importNode(newSvg.rootElement,true);
    var g=document.createElementNS(\"http://www.w3.org/2000/svg\",\"g\");

    //suppression des artefacts de webkit
    var rect=document.createElementNS(\"http://www.w3.org/2000/svg\",\"rect\");
    rect.setAttribute(\"x\",\"0\");
    rect.setAttribute(\"y\",\"0\");
    rect.setAttribute(\"width\",\"%g\");
    rect.setAttribute(\"height\",\"%g\");
    rect.setAttribute(\"fill\",\"#ffffff\");
    rect.setAttribute(\"stroke\",\"none\");
    g.appendChild(rect);
    g.setAttribute(\"id\",\"g\"+n+\"_\"+state);

    while(newSvg.firstChild) {
        if(newSvg.firstChild.nodeType==document.ELEMENT_NODE)
        g.appendChild(newSvg.firstChild);
        else
        newSvg.removeChild(newSvg.firstChild);
    }
    var cur_g=document.getElementById(\"g\"+current_slide+\"_\"+current_state);
    if(effect) { effect(g,cur_g); } else {
      if(cur_g) svg.removeChild(cur_g);
      svg.appendChild(g);
    }
    current_slide=n;
    current_state=state;
}}
"
      prefix
      w
      h
  )

let add_websocket w html=
  Rbuffer.add_string html (Printf.sprintf "var output;var websocket;
function start_socket(){
   if(websocket) websocket.close();
   websocket=new WebSocket(\"ws://\"+location.host+\"/tire\");
   websocket.onclose=function(evt){};
   websocket.onmessage = function(evt) {
     var st=JSON.parse(evt.data);
     if(current_slide==server_current_slide && current_state==server_current_state) {
       if(st.slide==current_slide) {
         loadSlide(st.slide,st.state);
       } else if(st.slide<current_slide) {
         loadSlide(st.slide,st.state,function(a,b){slide(%g,a,b)})
       } else {
         loadSlide(st.slide,st.state,function(a,b){slide(%g,a,b)})
       }
     }
     server_current_slide=st.slide;
     server_current_state=st.state;
   };
   websocket.onerror = function(evt) { };
};
"
    (-.w)
    w)

let add_client_onload w html=
  Rbuffer.add_string html (
    Printf.sprintf "window.onload=function(){
start_socket();
var xmlhttp=new XMLHttpRequest();
xmlhttp.open(\"GET\",\"/current\",false);
xmlhttp.send(null);
resize();
var json=JSON.parse(xmlhttp.responseText);
loadSlide(json.slide,json.state)
};
window.onkeydown=function(e){
if(e.keyCode==37){
if(current_state<=0) {
  loadSlide(current_slide-1,states[current_slide-1]-1,function(a,b){slide(%g,a,b)})
} else {
  loadSlide(current_slide,current_state-1)
}
} //left
if(e.keyCode==39){
if(current_state>=states[current_slide]-1) {
  loadSlide(current_slide+1,0,function(a,b){slide(%g,a,b)})
} else {
  loadSlide(current_slide,current_state+1)
}
} //right
}
"
      (-.w)
      w)

let add_unload_ws html=
  Rbuffer.add_string html "
window.onbeforeunload = function() {
    websocket.onclose = function () {}; // disable onclose handler first
    websocket.close()
};
"


let slave_html cache structure pages prefix=
  let html=Rbuffer.create 10000 in
  let w,h=if Array.length pages>0 then (pages.(0)).(0).pageFormat else 0.,0. in
  add_header structure.name w h html;
  let states=Rbuffer.create 10000 in
  for i=0 to Array.length pages-1 do
    if Rbuffer.length states>0 then Rbuffer.add_string states ",";
    Rbuffer.add_string states (string_of_int (Array.length pages.(i)))
  done;
  Rbuffer.add_string html "var states=[";
  Rbuffer.add_buffer html states;
  Rbuffer.add_string html "];";

  add_slide html;
  add_loadslide prefix w h html;
  add_websocket w html;

  add_client_onload w html;
  add_unload_ws html;


  Rbuffer.add_string html "</script><title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title></head><body style=\"margin:0;padding:0;\"><div id=\"svg\" style=\"margin-top:auto;margin-bottom:auto;margin-left:auto;margin-right:auto;\">";
  Rbuffer.add_string html (Printf.sprintf "<svg viewBox=\"0 0 %d %d\" overflow=\"hidden\">" (round (w)) (round (h)));

  let style=SVG.make_defs "" cache in
  Rbuffer.add_string html "<defs><style type=\"text/css\">\n<![CDATA[\n";
  Rbuffer.add_buffer html style;
  Rbuffer.add_string html "]]>\n</style></defs><title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title></svg></div></body></html>";
  html


let add_master_onload html=
  Rbuffer.add_string html "
function changeSlide(n,state){
    var svg_xhttp=new XMLHttpRequest();
    svg_xhttp.open(\"GET\",\"/pousse/\"+n+\"/\"+state,false);
    svg_xhttp.send();
}

var timediv;
function updatetime(){
if(time>=0){
var heure=Math.floor(time/3600);
var minute=\" \"+(Math.floor(time/60))%60;
var seconde=\" \"+(Math.floor(time)%60);
if(minute.length>2) minute=minute.substring(1,3);
if(seconde.length>2) seconde=seconde.substring(1,3);
timediv.innerHTML=(heure?heure+\":\":\"\")+minute+\":\"+seconde
time++;
}
}

window.onload=function(){
start_socket();
var xmlhttp=new XMLHttpRequest();
xmlhttp.open(\"GET\",\"/current\",false);
xmlhttp.send(null);
var json=JSON.parse(xmlhttp.responseText);
time=json.time;
timediv=document.getElementById(\"temps\");
updatetime();
slideTimer=setInterval(updatetime,1000);
loadSlide(json.slide,json.state)
};

window.onkeydown=function(e){
if(e.keyCode==37){
if(current_state<=0) {
console.log(current_slide,states,states[current_slide-1]);
  changeSlide(current_slide-1,states[current_slide-1]-1);
} else {
  changeSlide(current_slide,current_state-1)
}
} //left
if(e.keyCode==39){
if(current_state>=states[current_slide]-1) {
  changeSlide(current_slide+1,0)
} else {
  changeSlide(current_slide,current_state+1)
}
} //right
}
"
let master_html cache structure pages prefix=
  let html=Rbuffer.create 10000 in
  let w,h=if Array.length pages>0 then (pages.(0)).(0).pageFormat else 0.,0. in
  add_header structure.name w h html;
  let states=Rbuffer.create 10000 in
  for i=0 to Array.length pages-1 do
    if Rbuffer.length states>0 then Rbuffer.add_string states ",";
    Rbuffer.add_string states (string_of_int (Array.length (pages.(i))))
  done;
  Rbuffer.add_string html "var states=[";
  Rbuffer.add_buffer html states;
  Rbuffer.add_string html "];";

  add_slide html;
  add_loadslide prefix w h html;
  add_websocket w html;

  add_master_onload html;
  add_unload_ws html;

  let with_time=false in

  Rbuffer.add_string html "</script><title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title></head><body style=\"margin:0;padding:0;\">";
  Rbuffer.add_string html (Printf.sprintf "
<table style=\"width:100%%;height:100%%;position:absolute;bottom:0;top:0;border-spacing:0;\">
<tbody>
<tr style=\"margin:0;padding:0;\">
<td style=\"width:%s;padding:0;\">" (if with_time then "50%%" else "100%%"));
  Rbuffer.add_string html (Printf.sprintf "<svg viewBox=\"0 0 %d %d\" overflow=\"hidden\">" (round (w)) (round (h)));

  let style=SVG.make_defs "" cache in
  Rbuffer.add_string html "<defs><style type=\"text/css\">\n<![CDATA[\n";
  Rbuffer.add_buffer html style;
  Rbuffer.add_string html "]]>\n</style></defs><title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title></svg>";
  Rbuffer.add_string html "</td>";
  if with_time then (
    Rbuffer.add_string html "
<td style=\"font-family:helvetica;\">
<table style=\"width:100%%;\">
<tbody>
<tr>
<td id=\"temps\" style=\"font-family:monospace;font-size:40px;text-align:center;\">
</td>
</tr>
</tbody>
</table>

</td>
");
  Rbuffer.add_string html "</tr></tbody></table>";
  html


let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages filename=
  let prefix=try Filename.chop_extension filename with _->filename in
  let svg_files,cache=SVG.buffered_output' ~structure:structure pages prefix in
  let html=slave_html cache structure pages prefix in
  let master=master_html cache structure pages prefix in

  let o=open_out (prefix^".ml") in
  Printf.fprintf o "(* #PACKAGES netstring,netsys,unix,nethttpd,netcgi2,cryptokit *)\n";
  Printf.fprintf o "let page=%S\n" (Rbuffer.contents html);
  Printf.fprintf o "let master=%S\n" (Rbuffer.contents master);
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
  Printf.fprintf stdout "ocamlfind ocamlopt -o %s -package \"netstring,netsys,unix,nethttpd,netcgi2,cryptokit\" -thread -linkpkg %s.ml\n\n" prefix prefix
