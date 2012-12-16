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
open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util
open HtmlFonts
open Box
exception Bezier_degree



let assemble style title svg=
  let svg_buf=Rbuffer.create 256 in
  Rbuffer.add_string svg_buf "<defs>";
  Rbuffer.add_string svg_buf "<style type=\"text/css\">\n<![CDATA[\n";
  Rbuffer.add_buffer svg_buf style;
  Rbuffer.add_string svg_buf "]]>\n</style>\n";
  Rbuffer.add_string svg_buf "</defs>";
  Rbuffer.add_string svg_buf "<title>";
  Rbuffer.add_string svg_buf title;
  Rbuffer.add_string svg_buf "</title>";
  Rbuffer.add_buffer svg_buf svg;
  svg_buf

let standalone w h style title svg=
  let svg_buf=Rbuffer.create 256 in
  Rbuffer.add_string svg_buf (Printf.sprintf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
<svg width=\"%d\" height=\"%d\" version=\"1.1\"
     xmlns:xlink=\"http://www.w3.org/1999/xlink\"
     xmlns=\"http://www.w3.org/2000/svg\" " (round w) (round h));
  Rbuffer.add_string svg_buf (Printf.sprintf "viewBox=\"0 0 %d %d\" >" (round (w)) (round (h)));
  Rbuffer.add_buffer svg_buf (assemble style title svg);
  Rbuffer.add_string svg_buf "</svg>\n";
  svg_buf


let make_defs prefix fontCache=
  let def_buf=Rbuffer.create 256 in
  StrMap.iter (fun full class_name->
    Rbuffer.add_string def_buf
      (Printf.sprintf "@font-face { font-family:f%d; src:url(\"" class_name);
    Rbuffer.add_string def_buf full;
    Rbuffer.add_string def_buf ".otf\") format(\"opentype\"); }\n"
  ) fontCache.fontFamilies;
  ClassMap.iter (fun (fam,size,col) k->
    Rbuffer.add_string def_buf (
      Printf.sprintf ".c%d { font-family:f%d;font-size:%gpx;" k fam size
    );
    (match col with
        RGB fc ->
          Rbuffer.add_string def_buf
            (Printf.sprintf "fill:#%02x%02x%02x; "
               (round (255.*.fc.red))
               (round (255.*.fc.green))
               (round (255.*.fc.blue)))
    );
    Rbuffer.add_string def_buf "}\n";
  ) fontCache.classes;
  def_buf


let draw ?fontCache prefix w h contents=
  let svg_buf=Rbuffer.create 256 in

  let fontCache=match fontCache with
      None->build_font_cache prefix [|contents|]
    | Some x->x
  in
  (* Une petite burocratie pour gérer les particularités d'html/svg/etc *)
  let escapes=
    IntMap.add (int_of_char '<') "&lt;"
      (IntMap.add (int_of_char '>') "&gt;" IntMap.empty)
  in
  let esc_buf=Rbuffer.create 2 in
  let html_escape x=
    Rbuffer.clear esc_buf;
    for i=0 to String.length x-1 do
      try
        Rbuffer.add_string esc_buf
          (IntMap.find (int_of_char x.[i]) escapes)
      with
          Not_found -> Rbuffer.add_char esc_buf x.[i]
    done;
    Rbuffer.contents esc_buf
  in
  (****)


  (* Écriture du contenu à proprement parler *)

  let cur_x=ref 0. in
  let cur_y=ref 0. in
  let cur_cls=ref (-1) in
  let opened_text=ref false in
  let opened_tspan=ref false in

  let rec output_contents cont=match cont with
      Glyph x->(
        if not !opened_text then (
          Rbuffer.add_string svg_buf "<text>\n";
          opened_text:=true;
          opened_tspan:=false
        );

        let cls=className fontCache x in
        let size=x.glyph_size in
        if cls<> !cur_cls
          || !cur_x<>x.glyph_x || !cur_y <>x.glyph_y
          || not !opened_tspan
        then (
          if !opened_tspan then (
            Rbuffer.add_string svg_buf "</tspan>";
          );

          Rbuffer.add_string svg_buf (Printf.sprintf "<tspan x=\"%g\" y=\"%g\" class=\"c%d\">"
                                        (x.glyph_x) ((h-.x.glyph_y)) cls);
          cur_x:=x.glyph_x;
          cur_y:=x.glyph_y;
          cur_cls:=cls;
          opened_tspan:=true;
        );
        let utf8=(Fonts.glyphNumber x.glyph).glyph_utf8 in
        Rbuffer.add_string svg_buf (html_escape (UTF8.init 1 (fun _->UTF8.look utf8 0)));
        cur_x:= !cur_x +. (Fonts.glyphWidth x.glyph)*.x.glyph_size/.1000.;
      )
    | Path (args, l)->(
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      let buf=Rbuffer.create 100000 in
      List.iter
        (fun a->
          if Array.length a>0 then (
            let x0,y0=a.(0) in
            Rbuffer.add_string buf (Printf.sprintf "M%g %g" (x0.(0)) ( (h-.y0.(0))));
            Array.iter
              (fun (x,y)->
                if Array.length x=2 then Rbuffer.add_string buf "L" else
                  if Array.length x=3 then Rbuffer.add_string buf "Q" else
                    if Array.length x=4 then Rbuffer.add_string buf "C" else
                      raise Bezier_degree;
                for j=1 to Array.length x-1 do
                  Rbuffer.add_string buf (Printf.sprintf "%g %g " (x.(j)) ( (h-.y.(j))));
                done
              ) a;
            if args.close then Rbuffer.add_string buf "Z"
          )
        ) l;
      Rbuffer.add_string svg_buf "<path ";
      (match args.fillColor with
          Some (RGB fc) ->
            Rbuffer.add_string svg_buf (
              Printf.sprintf "fill=\"#%02X%02X%02X\" "
                (round (255.*.fc.red))
                (round (255.*.fc.green))
                (round (255.*.fc.blue))
            );
        | None->Rbuffer.add_string svg_buf "fill=\"none\" ");
      (match args.strokingColor with
          Some (RGB fc) ->
            Rbuffer.add_string svg_buf (
              Printf.sprintf "stroke=\"#%02X%02X%02X\" stroke-width=\"%f\" "
                (round (255.*.fc.red))
                (round (255.*.fc.green))
                (round (255.*.fc.blue))
                (args.lineWidth)
            );
        | None->
          Rbuffer.add_string svg_buf "stroke=\"none\" "
      );
      Rbuffer.add_string svg_buf "d=\"";
      Rbuffer.add_buffer svg_buf buf;
      Rbuffer.add_string svg_buf "\" />\n";
    )
    | Image i->(
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      let f_=Filename.basename i.image_file in
      let f=try Filename.chop_extension f_ with _->f_ in
      let ext=String.sub i.image_file (String.length f)
        (String.length i.image_file-String.length f)
      in
      let rec nonexistent i=
        let name=Printf.sprintf "%s%d%s" f i ext in
        if Sys.file_exists (Filename.concat prefix name) then nonexistent (i+1) else name
      in
      let name=nonexistent 0 in
      copy_file i.image_file (Filename.concat prefix name);
      Rbuffer.add_string svg_buf
        (Printf.sprintf "<image x=\"%g\" y=\"%g\" width=\"%gpx\" height=\"%gpx\" xlink:href=\"%s\"/>\n"
           i.image_x (h-.i.image_y-.i.image_height) i.image_width i.image_height name)
    )
    | States s->List.iter output_contents s.states_contents
    | Link l->(
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );

      if l.is_internal then (
        Rbuffer.add_string svg_buf
          (Printf.sprintf "<a xlink:href=\"#%d_%d\">"
             l.dest_page 0
          );
      ) else (
        Rbuffer.add_string svg_buf "<a xlink:href=\"";
        Rbuffer.add_string svg_buf l.uri;
        Rbuffer.add_string svg_buf "\">"
      );

      List.iter output_contents (l.link_contents);

      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      Rbuffer.add_string svg_buf "</a>";
    )
    | _->()
  in
  List.iter output_contents contents;
  if !opened_tspan then (
    Rbuffer.add_string svg_buf "</tspan>\n";
  );
  if !opened_text then (
    Rbuffer.add_string svg_buf "</text>\n";
  );

  svg_buf



let buffered_output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages prefix=

  let total=Array.fold_left (fun m x->m+Array.length x) 0 pages in
  let all_pages=Array.make total OutputPaper.defaultPage in
  let _=Array.fold_left (fun m0 x->
    Array.fold_left (fun m x->
      all_pages.(m)<-x;
      m+1
    ) m0 x
  ) 0 pages
  in
  let cache=build_font_cache prefix (Array.map (fun x->x.pageContents) all_pages) in

  let svg_files=Array.map (fun pi->
    Array.map (fun page->
      let file=Rbuffer.create 10000 in
        (* Printf.sprintf "%s_%d_%d.svg" chop_file i j *)
      let w,h=page.pageFormat in
      Rbuffer.add_string file (Printf.sprintf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet href=\"style.css\" type=\"text/css\"?>
<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 %d %d\">"
                                 (round (w)) (round (h)));
      let sorted_pages=
        let x=List.fold_left (fun m x->
          let m'=try IntMap.find (drawing_order x) m with Not_found->[] in
          IntMap.add (drawing_order x) (x::m') m
        ) IntMap.empty page.pageContents
        in
        let comp a b=match a,b with
            Glyph ga,Glyph gb->if ga.glyph_y=gb.glyph_y then compare ga.glyph_x gb.glyph_x
              else compare gb.glyph_y ga.glyph_y
          | Glyph ga,_-> -1
          | _,Glyph gb->1
          | _->0
        in
        let subsort a=match a with
            Link l->Link { l with link_contents=List.sort comp l.link_contents }
          | b->b
        in
        IntMap.fold (fun _ a x->x@a) (IntMap.map (fun l->(List.sort comp (List.map subsort l))) x) []
      in
      let svg=draw ~fontCache:cache prefix w h sorted_pages in
      Rbuffer.add_buffer file svg;
      Rbuffer.add_string file "</svg>\n";
      file
    ) pi
  ) pages
  in
  svg_files,cache

let basic_html cache structure pages prefix=
  let html=Rbuffer.create 10000 in
  let w,h=if Array.length pages>0 then (pages.(0)).(0).pageFormat else 0.,0. in
  Rbuffer.add_string html
    "<!DOCTYPE html>
<html lang=\"en\">
<head>
<link href=\"style.css\" rel=\"stylesheet\" type=\"text/css\">
<meta charset=\"utf-8\">
<title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title>\n";
  Rbuffer.add_string html (Printf.sprintf "<script>
var current_slide= -1;
var current_state= -1;
resize=function(){
sizex=(window.innerWidth)/%g;
sizey=(window.innerHeight)/%g;
size=sizex>sizey ? sizey : sizex;
svg=document.getElementById(\"svg\");
svg.style.width=(%g*size)+'px';
svg.style.height=(%g*size)+'px';
};
" w h (w-.10.) (h-.10.));

  Rbuffer.add_string html "function slide(width,g0,g1){
  var svg=document.getElementsByTagName(\"svg\")[0];
  g0.setAttribute(\"transform\",\"translate(\"+width+\" 0)\");
  svg.appendChild(g0);

  var i=0;
  var slideTimer;
  var n=10;
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
  slideTimer=setInterval(do_slide,20);
}";

  let states=Rbuffer.create 10000 in
  for i=0 to Array.length pages-1 do
    if Rbuffer.length states>0 then Rbuffer.add_string states ",";
    Rbuffer.add_string states (string_of_int (Array.length (pages.(i))))
  done;
  Rbuffer.add_string html "var states=[";
  Rbuffer.add_buffer html states;
  Rbuffer.add_string html "];";

  Rbuffer.add_string html (
    Printf.sprintf "function loadSlide(n,state,effect){
if(n>=0 && n<%d && state>=0 && state<states[n] && (n!=current_slide || state!=current_state)) {
    xhttp=new XMLHttpRequest();
    xhttp.open(\"GET\",n+\"_\"+state+\".svg\",false);
    xhttp.send();
    var parser=new DOMParser();
    var newSvg=parser.parseFromString(xhttp.responseText,\"image/svg+xml\");

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
    location.hash=n+\"_\"+state;
}}"
      (Array.length pages)
      w
      h
  );

  Rbuffer.add_string html (
    Printf.sprintf "window.onload=function(){
var h0=0,h1=0;
if(location.hash){
var i=location.hash.indexOf(\"_\");
h0=location.hash?parseInt(location.hash.substring(1,i)):0;
h1=location.hash?parseInt(location.hash.substring(i+1)):0;
}
resize();
console.log(h0,h1);
loadSlide(h0,h1)
};

window.onhashchange=function(){
var h0=0,h1=0;
if(location.hash){
var i=location.hash.indexOf(\"_\");
h0=location.hash?parseInt(location.hash.substring(1,i)):0;
h1=location.hash?parseInt(location.hash.substring(i+1)):0;
}
loadSlide(h0,h1)
};

window.onkeydown=function(e){
//console.log(e);
if(e.keyCode==37 || e.keyCode==38 || e.keyCode==33){
if(current_state<=0) {
  loadSlide(current_slide-1,states[current_slide-1]-1,function(a,b){slide(%g,a,b)})
} else {
  loadSlide(current_slide,current_state-1)
}
} //left
if(e.keyCode==39 || e.keyCode==40 || e.keyCode==34){
if(current_state>=states[current_slide]-1) {
  loadSlide(current_slide+1,0,function(a,b){slide(%g,a,b)})
} else {
  loadSlide(current_slide,current_state+1)
}
} //right
if(e.charCode==82){ //r
loadSlide(current_slide,current_state);
}
}

function gotoSlide(n){
if(n>current_slide)
  loadSlide(n,0,function(a,b){slide(%g,a,b)})
else if(n<current_slide)
  loadSlide(n,0,function(a,b){slide(%g,a,b)})
}

</script>"
      (-.w)
      w
      w
      (-.w)
  );

  Rbuffer.add_string html "<title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title></head><body style=\"margin:0;padding:0;\"><div id=\"svg\" style=\"margin-top:auto;margin-bottom:auto;margin-left:auto;margin-right:auto;\">";
  Rbuffer.add_string html (Printf.sprintf "<svg xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 %d %d\" overflow=\"hidden\">" (round (w)) (round (h)));

  let style=make_defs "" cache in
  let ststr=(Filename.concat prefix "style.css") in
  let o=open_out (Filename.concat prefix "style.css") in
  Rbuffer.output_buffer o style;
  close_out o;
  Rbuffer.add_string html "<defs><style type=\"text/css\" src=\"";
  Rbuffer.add_string html ststr;
  Rbuffer.add_string html "\"/>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</svg></div></body></html>";
  html



let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages filename=
  let prefix=try Filename.chop_extension filename with _->filename in
  let rec unlink_rec dir=
    if Sys.file_exists dir then (
      if Sys.is_directory dir then (
        Array.iter (fun x->unlink_rec (Filename.concat dir x)) (Sys.readdir dir);
        Unix.rmdir dir
      ) else (
        Unix.unlink dir
      )
    );
  in
  (try
     unlink_rec prefix;
     Unix.mkdir prefix 0o755;
   with
       _->());

  let svg_files,cache=buffered_output' ~structure:structure pages prefix in
  let html=basic_html cache structure pages prefix in
  let o=open_out (Filename.concat (Filename.basename prefix) "index.html") in
  Rbuffer.output_buffer o html;
  close_out o;

  Array.iteri (fun i->
    Array.iteri (fun j x->
      let o=open_out (Filename.concat (Filename.basename prefix) (Printf.sprintf "%d_%d.svg" i j)) in
      Rbuffer.output_buffer o x;
      close_out o
    )
  ) svg_files;
  output_fonts cache

let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages filename=
  output' ~structure (Array.map (fun x->[|x|]) pages) filename





let images prefix env conts=

  (if Sys.file_exists prefix && not (Sys.is_directory prefix) then
    Unix.unlink prefix;
  );
  (if not (Sys.file_exists prefix) then
      Unix.mkdir prefix 0o755);

  let conts_box=Array.map (fun x->Document.boxify_scoped env x) conts in
  let raws=Array.map (fun cont->Document.draw_boxes env cont) conts_box in
  let cache=build_font_cache prefix raws in
  let css_file=Filename.concat prefix "style.css" in


  let r=Rbuffer.create 1000 in
  let imgs=Array.mapi (fun i x->
    Rbuffer.clear r;
    let _,w,_=boxes_interval (Array.of_list conts_box.(i)) in
    let x0,y0,x1,y1=bounding_box_full raws.(i) in
    let h=y1-.y0 in
    Rbuffer.add_string r (Printf.sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" overflow=\"visible\" width=\"%gmm\" height=\"%gmm\" viewBox=\"%g %g %g %g\" style=\"margin-bottom:%gmm;\">"
                            w (y1-.y0)
                            (floor x0) (h-.y1) (ceil (x1-.x0)) (h)
                            y0);

    let dr=draw ~fontCache:cache prefix w (y1 -. y0) raws.(i) in
    HtmlFonts.output_fonts cache;

    (* Rbuffer.add_string r (Printf.sprintf "<defs><style type=\"text/css\" src=\"%s\"/></defs>" css_file); *)
    Rbuffer.add_string r "<title></title>";
    Rbuffer.add_buffer r dr;
    Rbuffer.add_string r "</svg>\n";
    Rbuffer.contents r;
  ) conts
  in
  let style=make_defs prefix cache in
  let css=open_out css_file in
  Rbuffer.output_buffer css style;
  close_out css;
  imgs
