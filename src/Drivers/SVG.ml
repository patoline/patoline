open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util
open HtmlFonts

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


let make_defs fontCache=
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


let draw ?fontCache w h contents=
  let svg_buf=Rbuffer.create 256 in

  let fontCache=match fontCache with
      None->build_font_cache [|contents|]
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
      Rbuffer.add_string svg_buf
        (Printf.sprintf "<image x=\"%g\" y=\"%g\" width=\"%gpx\" height=\"%gpx\" xlink:href=\"%s\"/>\n"
           i.image_x (h-.i.image_y-.i.image_height) i.image_width i.image_height i.image_file)
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


let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let cache=build_font_cache (Array.map (fun x->x.pageContents) pages) in
  HtmlFonts.output_fonts cache;
  let chop=Filename.chop_extension fileName in
  let i=ref 0 in
  while !i<Array.length pages do
    let html_name=Printf.sprintf "%s%d.html" chop !i in
    let html=open_out html_name in
    Printf.fprintf html
      "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>%s</title>"      structure.name;
    Printf.fprintf html "</head><body style=\"margin:0;padding:0;\">";
    Printf.fprintf html "<div id=\"svg\" style=\"margin-top:auto;margin-bottom:auto;margin-left:auto;margin-right:auto;width:100%%;\">";
    for j= !i to min (!i+10) (Array.length pages-1) do
      let w,h=pages.(j).pageFormat in
       Printf.fprintf html "<svg viewBox=\"0 0 %d %d\" style=\"width:100%%;\">"
        (round (w)) (round ( h));

      let sorted_pages=

        let x=List.fold_left (fun m x->
          let m'=try IntMap.find (drawing_order x) m with Not_found->[] in
          IntMap.add (drawing_order x) (x::m') m
        ) IntMap.empty pages.(j).pageContents
        in
        let comp a b=match a,b with
            Glyph ga,Glyph gb->if ga.glyph_y=gb.glyph_y then compare ga.glyph_x gb.glyph_x
              else compare gb.glyph_y ga.glyph_y
          | Glyph ga,_-> -1
          | _,Glyph gb->1
          | _->0
        in
        IntMap.fold (fun _ a x->x@a) (IntMap.map (fun l->(List.sort comp l)) x) []
      in
      let svg=draw ~fontCache:cache w h sorted_pages in
      let defs=make_defs cache in
      Rbuffer.output_buffer html (assemble defs "" svg);
      Printf.fprintf html "</svg>\n";
    done;
    Printf.fprintf html "</div></body></html>";
    close_out html;
    i:= !i+10
  done;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr



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
  let cache=build_font_cache (Array.map (fun x->x.pageContents) all_pages) in

  let svg_files=Array.map (fun pi->
    Array.map (fun page->
      let file=Rbuffer.create 10000 in
        (* Printf.sprintf "%s_%d_%d.svg" chop_file i j *)
      let w,h=page.pageFormat in
      Rbuffer.add_string file (Printf.sprintf "<?xml version=\"1.0\" encoding=\"UTF-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 %d %d\">"
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
        IntMap.fold (fun _ a x->x@a) (IntMap.map (fun l->(List.sort comp l)) x) []
      in
      let svg=draw ~fontCache:cache w h sorted_pages in
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
<meta charset=\"utf-8\">
<title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title>\n";
  Rbuffer.add_string html (Printf.sprintf "<script>
var current_slide=0;
var current_state=0;
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
if(n>=0 && n<%d && state>=0 && state<states[n]) {
    location.hash=n+\"_\"+state;
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
resize();loadSlide(h0,h1)
};
window.onhashchange=function(){window.onload()};
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
console.log(\"gotoSlide\",n);
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

  let style=make_defs cache in
  Rbuffer.add_string html "<defs><style type=\"text/css\">\n<![CDATA[\n";
  Rbuffer.add_buffer html style;
  Rbuffer.add_string html "]]>\n</style></defs>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</svg></div></body></html>";
  html



let onepage_html cache structure pages svg_files=
  let html=Rbuffer.create 10000 in
  Rbuffer.add_string html
    "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title></head><body style=\"margin:0;padding:0;\">";

  Array.iteri (fun i->
    Array.iteri (fun j x->
      Rbuffer.add_buffer html x;
    )
  ) svg_files;

  Rbuffer.add_string html "</div></body></html>";
  html



let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages filename=
  let prefix=try Filename.chop_extension filename with _->filename in
  let svg_files,cache=buffered_output' ~structure:structure pages prefix in
  let html=basic_html cache structure pages prefix in
  let rec unlink_rec dir=
    if Sys.file_exists dir then (
      if Sys.is_directory dir then (
        Array.iter (fun x->unlink_rec (Filename.concat dir x)) (Sys.readdir dir);
        Unix.rmdir dir
      ) else
        Unix.unlink dir
    );
  in
  unlink_rec prefix;
  Unix.mkdir prefix 0o755;
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
