open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util
open HtmlFonts

exception Bezier_degree

let filename x= try (Filename.chop_extension x)^".html" with _ -> x^".html"


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
  Rbuffer.add_string svg_buf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1 Tiny//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd\">
<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" id=\"svg-root\"";
  Rbuffer.add_string svg_buf (Printf.sprintf "viewBox=\"0 0 %d %d\"" (round (w)) (round (h)));
  Rbuffer.add_string svg_buf "version=\"1.1\" baseProfile=\"tiny\">";
  Rbuffer.add_buffer svg_buf (assemble style title svg);
  svg_buf


let make_defs fontCache=
  let def_buf=Rbuffer.create 256 in
  StrMap.iter (fun full class_name->
    Rbuffer.add_string def_buf "@font-face { font-family:";
    Rbuffer.add_string def_buf class_name;
    Rbuffer.add_string def_buf "; src:url(\"";
    Rbuffer.add_string def_buf full;
    Rbuffer.add_string def_buf ".otf\") format(\"opentype\"); }\n"
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


  let buf=Rbuffer.create 100 in


  (* Écriture du contenu à proprement parler *)

  let cur_x=ref 0. in
  let cur_y=ref 0. in
  let cur_family=ref "" in
  let cur_size=ref 0. in
  let cur_color=ref (RGB {red=0.;green=0.;blue=0.}) in
  let opened_text=ref false in
  let opened_tspan=ref false in

  let rec output_contents cont=match cont with
      Glyph x->(
        if not !opened_text then (
          Rbuffer.add_string svg_buf "<text>\n";
          opened_text:=true
        );

        let _,fontName=className fontCache x.glyph in
        let size=x.glyph_size in
        if !cur_x<>x.glyph_x || !cur_y<>x.glyph_y || !cur_family<>fontName
          || !cur_size<>size || !cur_color<>x.glyph_color
        then (
          if !opened_tspan then (
            Rbuffer.add_string svg_buf "</tspan>";
          );
          Rbuffer.add_string svg_buf (Printf.sprintf "<tspan x=\"%g\" y=\"%g\" font-family=\"%s\" font-size=\"%gpx\" "
                                        (x.glyph_x) ( (h-.x.glyph_y))
                                        fontName
                                        (size));
          (match x.glyph_color with
              RGB fc ->
                Rbuffer.add_string svg_buf
                  (Printf.sprintf "fill=\"#%02x%02x%02x\" "
                     (round (255.*.fc.red))
                     (round (255.*.fc.green))
                     (round (255.*.fc.blue)))
        (* | _->() *)
          );
          Rbuffer.add_string svg_buf "stroke=\"none\">";
          cur_x:=x.glyph_x;
          cur_y:=x.glyph_y;
          cur_family:=fontName;
          cur_size:=size;
          cur_color:=x.glyph_color;
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
    | States (a,b)->List.iter output_contents a
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



let output ?(structure:structure={name="";displayname=[];metadata=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let fileName = filename fileName in
  let cache=build_font_cache (Array.map (fun x->x.pageContents) pages) in

  for i=0 to Array.length pages-1 do
    let chop=Filename.chop_extension fileName in
    let chop_file=Filename.basename chop in
    let html_name=Printf.sprintf "%s%d.html" chop i in
    let w,h=pages.(i).pageFormat in
    let html=open_out html_name in
    let noscript=true in
    Printf.fprintf html
      "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>%s</title>"      structure.name;
    if not noscript then
      Printf.fprintf html "<script>
resize=function(){
sizex=(window.innerWidth)/%g;
sizey=(window.innerHeight)/%g;
size=sizex>sizey ? sizey : sizex;
svg=document.getElementById(\"svg\");
svg.style.width=(%g*size)+'px';
svg.style.height=(%g*size)+'px';
console.log(svg.style.width,svg.style.height);
};
//window.onresize=function(e){resize()};
window.onload=function(){resize()};
window.onkeydown=function(e){
%s
%s
}
</script>"
      w h (w-.10.) (h-.10.)
        (if i>0 then
            Printf.sprintf "if(e.keyCode==37){document.location.href=\"%s%d.html\"} // left" chop_file (i-1)
         else "")
        (if i<Array.length pages-1 then
            Printf.sprintf "if(e.keyCode==39){document.location.href=\"%s%d.html\"} //right" chop_file (i+1)
         else "");

    Printf.fprintf html "</head><body style=\"margin:0;padding:0;\">";
    if noscript then (
      Printf.fprintf html "<div style=\"margin:0;padding:0;width:100%%;\">%s%s%s</div>"
        (if i=0 then "" else
            Printf.sprintf "<a href=\"%s\">Précédent</a>"
              (Printf.sprintf "%s%d.html" chop_file (i-1)))
        (if i<>0 && i<>Array.length pages-1 then " " else "")
        (if i=Array.length pages-1 then "" else
            Printf.sprintf "<a href=\"%s\">Suivant</a>"
              (Printf.sprintf "%s%d.html" chop_file (i+1)));
    );
    Printf.fprintf html "<div id=\"svg\" style=\"margin-top:auto;margin-bottom:auto;margin-left:auto;margin-right:auto;width:100%%;\">";
    Printf.fprintf html "<svg viewBox=\"0 0 %d %d\" style=\"width:100%%;\">"
      (round (w)) (round ( h));
    let svg=draw ~fontCache:cache w h pages.(i).pageContents in
    let defs=make_defs cache in
    Rbuffer.output_buffer html (assemble defs "" svg);
    Printf.fprintf html "</svg>\n";
    Printf.fprintf html "</div></body></html>";
    close_out html
  done;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr




let output' ?(structure:structure={name="";displayname=[];metadata=[];
				   page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let fileName = filename fileName in
  let total=Array.fold_left (fun m x->m+Array.length (snd x)) 0 pages in
  let all_pages=Array.make total ((snd pages.(0)).(0)) in
  let _=Array.fold_left (fun m0 (_,x)->
    Array.fold_left (fun m x->
      all_pages.(m)<-x;
      m+1
    ) m0 x
  ) 0 pages
  in

  let cache=build_font_cache (Array.map (fun x->x.pageContents) all_pages) in

  let chop=Filename.chop_extension fileName in
  let chop_file=Filename.basename chop in

  let defs=make_defs cache in

  for i=0 to Array.length pages-1 do
    let env,p=pages.(i) in
    for j=0 to Array.length p-1 do
      let page=(snd pages.(i)).(j) in
      let file=open_out (Printf.sprintf "%s_%d_%d.svg" chop_file i j) in
      let w,h=page.pageFormat in
      Printf.fprintf file "<?xml version=\"1.0\" encoding=\"UTF-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\">"
        (round (w)) (round (h));
      let svg=draw ~fontCache:cache w h page.pageContents in
      Rbuffer.output_buffer file svg;
      Printf.fprintf file "</svg>\n";
      close_out file
    done
  done;
  let html_name=Printf.sprintf "%s.html" chop in
  let html=open_out html_name in
  let w,h=if Array.length pages>0 then (snd pages.(0)).(0).pageFormat else 0.,0. in
  Printf.fprintf html
      "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"utf-8\">
<title>%s</title>"      structure.name;
  let states=Rbuffer.create 10000 in
  for i=0 to Array.length pages-1 do
    if Rbuffer.length states>0 then Rbuffer.add_string states ",";
    Rbuffer.add_string states (string_of_int (Array.length (snd pages.(i))))
  done;
  Printf.fprintf html "<script>
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
//window.onresize=function(e){resize()};

function slide(width,g0,g1){
  var svg=document.getElementsByTagName(\"svg\")[0];
  g0.setAttribute(\"transform\",\"translate(\"+width+\" 0)\");
  svg.appendChild(g0);

  var i=0;
  var slideTimer;
  var n=40;
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
  slideTimer=setInterval(do_slide,0.1);
}

var states=[%s];
function loadSlide(n,state,effect){
if(n>=0 && n<%d && state>=0 && state<states[n]) {
    xhttp=new XMLHttpRequest();
    xhttp.open(\"GET\",\"%s_\"+n+\"_\"+state+\".svg\",false);
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

    for(i=0;i<newSvg.childNodes.length;i++) {
        if(newSvg.childNodes[i].nodeType==document.ELEMENT_NODE)
        g.appendChild(newSvg.childNodes[i]);
        else
        newSvg.removeChild(newSvg.childNodes[i]);
    }
    var cur_g=document.getElementById(\"g\"+current_slide+\"_\"+current_state);
    if(effect) { effect(g,cur_g); } else {
      if(cur_g) svg.removeChild(cur_g);
      svg.appendChild(g);
    }
    current_slide=n;
    current_state=state;
}
}

window.onload=function(){
resize();loadSlide(0,0)
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
</script>"
    w h (w-.10.) (h-.10.)
    (Rbuffer.contents states)
    (Array.length pages)
    chop_file
    w
    h
    (-.w)
    w
  ;

    Printf.fprintf html "<title>%s</title></head><body style=\"margin:0;padding:0;\">" structure.name;
    Printf.fprintf html "<div id=\"svg\" style=\"margin-top:auto;margin-bottom:auto;margin-left:auto;margin-right:auto;\">";
    Printf.fprintf html "<svg viewBox=\"0 0 %d %d\" overflow=\"hidden\">" (round (w)) (round (h));

    let style=make_defs cache in
    Printf.fprintf html "<defs>";
    Printf.fprintf html "<style type=\"text/css\">\n<![CDATA[\n";
    Rbuffer.output_buffer html style;
    Printf.fprintf html "]]>\n</style>\n";
    Printf.fprintf html "</defs><title>%s</title>" structure.name;

    Printf.fprintf html "</svg>\n";
    Printf.fprintf html "</div></body></html>";
    close_out html;

    Printf.fprintf stderr "File %s written.\n" fileName;
    flush stderr
