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
open FTypes
open Box
open Util
open UsualMake
open HtmlFonts
open Driver
open RawContent
open Color

exception Bezier_degree of int

let font_filter = ref ""

let filter_options argv = argv
let driver_options =
  [("--font-filter",Arg.Set_string font_filter,"Set a command to filter otf fonts");]

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

let htmlColor col =
  let (r,g,b) = to_rgb col in
  if r<>0. || g<>0. || b<>0. then
    let r = min 1. (max 0. r) in
    let g = min 1. (max 0. g) in
    let b = min 1. (max 0. b) in
    (Printf.sprintf "fill:#%02x%02x%02x; "
                    (round (255.*.r))
                    (round (255.*.g))
                    (round (255.*.b)))
  else ""

let output_fontCache def_buf fontCache units class_prefix =
  StrMap.iter (fun _ (full, class_name) ->
      Rbuffer.add_string def_buf
        (Printf.sprintf "@font-face { font-family:f%d; src:url(\"" class_name);
      Rbuffer.add_string def_buf full;
      Rbuffer.add_string def_buf ".otf\") format(\"opentype\"); }\n"
    ) fontCache.fontFamilies;
  ClassMap.iter (fun (fam,size,col) k->
    let col = htmlColor col in
    Rbuffer.add_string def_buf (
       Printf.sprintf ".%s%d { font-family:f%d;font-size:%g%s; %s}\n"
                      class_prefix k fam size units col);
    ) fontCache.classes

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

let make_defs ?(output_fonts=true) ?(units="px") ?(class_prefix="c") prefix fontCache=
  let def_buf=Rbuffer.create 256 in
  Rbuffer.add_string def_buf
"
a:hover{opacity: 0.75;cursor:crosshair;}
g.button:hover{opacity: 0.75;cursor:crosshair;}
g.button{z-index:10;unselectable:'on';onselectstart:'return false;'; -webkit-touch-callout: none;-webkit-user-select: none;
  -khtml-user-select: none;-moz-user-select: none;-ms-user-select: none;user-select: none;user-select: none;}
g.dragable:hover{opacity: 0.75;cursor:move;}
g.dragable{z-index:10;unselectable:'on';onselectstart:'return false;'; -webkit-touch-callout: none;-webkit-user-select: none;
  -khtml-user-select: none;-moz-user-select: none;-ms-user-select: none;user-select: none;user-select: none;}
g.editable:hover{opacity: 0.75;cursor:text;}
g.editable{z-index:10;unselectable:'on';onselectstart:'return false;'; -webkit-touch-callout: none;-webkit-user-select: none;
  -khtml-user-select: none;-moz-user-select: none;-ms-user-select: none;user-select: none;user-select: none;}
div.editor{position:fixed;border:2px solid blue;background:#000080;background:rgba(0,0,255,0.5);margin:0;}
svg text{pointer-events:none;}
.menu {background-color:#b0ffb0; opacity:0.80}
g.svg_item{opacity:0;pointer-events: all;}
td.svg_item:hover {background-color:#b0b0ff; opacity:1.0 }
svg path{pointer-events:none;}
svg rect{pointer-events:none;}
svg a text{pointer-events:all;}
svg[id='svg_svg'] { width:100%; height:auto; }
svg .button text{pointer-events:all;}
svg .editable text{pointer-events:all;}
svg .dragable text{pointer-events:all;}
svg a path{pointer-events:all;}
svg .button path{pointer-events:all;}
svg .editable path{pointer-events:all;}
svg .dragable path{pointer-events:all;}
svg a rect{pointer-events:all;}
svg .button rect{pointer-events:all;}
svg .editable rect{pointer-events:all;}
svg .dragable rect{pointer-events:all;}
 ";
  if output_fonts then output_fontCache def_buf fontCache units class_prefix;
  def_buf


let draw ?fontCache ?dynCache prefix w h contents=
  let dynCache, buttonCache = match dynCache with
    | None -> None, None
    | Some((d,b),g,sl,st,f) -> Some (d,g,sl,st,f), Some b
  in
  let svg_buf=Rbuffer.create 256 in

  let fontCache=match fontCache with
      None->build_font_cache prefix [|contents|]
    | Some x->x
  in
  (* Une petite burocratie pour gérer les particularités d'html/svg/etc *)
  let escapes=
    IntMap.add (int_of_char '&') "&amp;"
      (IntMap.add (int_of_char '<') "&lt;"
        (IntMap.add (int_of_char '>') "&gt;" IntMap.empty))
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

  let rec output_contents ?(offset=h) ?(create_new_class=true) ~svg_buf contents=
    let outcont ?(offset=offset) = output_contents ~offset ~create_new_class in
    let imgs=ref StrMap.empty in
    let cur_x=ref 0. in
    let cur_y=ref 0. in
    let cur_cls=ref (-1) in
    let opened_text=ref false in
    let opened_tspan=ref false in
    let animate_count = ref 0 in
    let rec output_contents_aux ?(offset=offset) cont=match cont with
      Glyph x->(
        if not !opened_text then (
          Rbuffer.add_string svg_buf "<text>\n";
          opened_text:=true;
          opened_tspan:=false
        );
	try
          let style,cls =
            try
              let cls = className ~create_new_class fontCache x in
              Printf.sprintf "class=\"c%d\"" cls, cls
            with
              Style((fam,size,col),cls) ->
              let col = htmlColor col in
              Printf.sprintf "style=\"font-family:f%d; font-size:%gpx; %s\"" fam size col, cls
          in
        (* let size=x.glyph_size in *)
          if cls <> !cur_cls
            || !cur_x<>x.glyph_x || !cur_y <>x.glyph_y
            || not !opened_tspan
          then (
            if !opened_tspan then (
              Rbuffer.add_string svg_buf "</tspan>";
            );
            Rbuffer.add_string svg_buf (Printf.sprintf "<tspan x=\"%g\" y=\"%g\" %s>"
                                          (x.glyph_x) ((offset-.x.glyph_y)) style);
            cur_x:=x.glyph_x;
            cur_y:=x.glyph_y;
            cur_cls:=cls;
            opened_tspan:=true;
          );
          let utf8=(Fonts.glyphNumber x.glyph).glyph_utf8 in
          Rbuffer.add_string svg_buf (html_escape (UTF8.init 1 (fun _->UTF8.look utf8 0)));
          cur_x:= !cur_x +. (Fonts.glyphWidth x.glyph)*.x.glyph_size/.1000.;
	with Not_found ->
	  Printf.fprintf stderr "Missing glyph: %s\n%!" (Fonts.glyphNumber x.glyph).glyph_utf8
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
            Rbuffer.add_string buf (Printf.sprintf "M%g %g" (x0.(0)) ( (offset-.y0.(0))));
            Array.iter
              (fun (x,y)->
                if Array.length x=2 then Rbuffer.add_string buf "L" else
                  if Array.length x=3 then Rbuffer.add_string buf "Q" else
                    if Array.length x=4 then Rbuffer.add_string buf "C" else
                      raise (Bezier_degree (Array.length x));
                for j=1 to Array.length x-1 do
                  Rbuffer.add_string buf (Printf.sprintf "%g %g " (x.(j)) ( (offset-.y.(j))));
                done
              ) a;
            if args.close then Rbuffer.add_string buf "Z"
          )
        ) l;
      Rbuffer.add_string svg_buf "<path ";
      (match args.fillColor with
        | Some col ->
            let (r,g,b,a) = to_rgba col in
            Rbuffer.add_string svg_buf (
              Printf.sprintf "fill=\"#%02X%02X%02X\" fill-opacity=\"%f\" "
                (round (255.0 *. r)) (round (255.0 *. g)) (round (255.0 *. b))
                a
            );
        | None->Rbuffer.add_string svg_buf "fill=\"none\" ");
      (match args.strokingColor with
          Some col ->
            let (r,g,b,a) = to_rgba col in
            Rbuffer.add_string svg_buf (
              Printf.sprintf "stroke=\"#%02X%02X%02X\" stroke-width=\"%f\" stroke-opacity=\"%f\" "
                (round (255.0 *. r))
                (round (255.0 *. g))
                (round (255.0 *. b))
                (args.lineWidth)
                a
                               );
        | None->
          Rbuffer.add_string svg_buf "stroke=\"none\" "
      );

      (match args.dashPattern with
       | [] -> ()
       | l ->
          Rbuffer.add_string svg_buf "stroke-dasharray=\"";
          (* do not use string_of_float, 1. is not legal in SVG *)
          Rbuffer.add_string svg_buf (String.concat "," (List.map (Printf.sprintf "%f") l));
          Rbuffer.add_string svg_buf "\" ");

      (match args.lineCap with
       | Butt_cap -> ()
       | Round_cap -> Rbuffer.add_string svg_buf "stroke-linecap=\"round\" "
       | Proj_square_cap -> Rbuffer.add_string svg_buf "stroke-linecap=\"square\" ");

      (match args.lineJoin with
       | Miter_join -> ()
       | Round_join -> Rbuffer.add_string svg_buf "stroke-linejoin=\"round\" "
       | Bevel_join -> Rbuffer.add_string svg_buf "stroke-linejoin=\"bevel\" ");

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
      let ext=String.sub f_ (String.length f)
        (String.length f_-String.length f)
      in
      let rec nonexistent i=
	if prefix <> "" then (
          let name=Printf.sprintf "%s%d%s" f i ext in
          if Sys.file_exists (Filename.concat prefix name) then nonexistent (i+1) else name )
	else f_
      in
      let name=nonexistent 0 in
      imgs:=StrMap.add i.image_file name !imgs;
      Rbuffer.add_string svg_buf
        (Printf.sprintf "<image x=\"%g\" y=\"%g\" width=\"%gpx\" height=\"%gpx\" xlink:href=\"%s\"/>\n"
           i.image_x (offset-.i.image_y-.i.image_height) i.image_width i.image_height name)
    )
    | Video i->(
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      let f_=Filename.basename i.video_file in
      let f=try Filename.chop_extension f_ with _->f_ in
      let ext=String.sub f_ (String.length f)
        (String.length f_-String.length f)
      in
      let rec nonexistent i=
        let name=Printf.sprintf "%s%d%s" f i ext in
        if Sys.file_exists (Filename.concat prefix name) then nonexistent (i+1) else name
      in
      let name=nonexistent 0 in
      imgs:=StrMap.add i.video_file name !imgs;
      Rbuffer.add_string svg_buf
        (Printf.sprintf "<g transform=\"translate(%g,%g) scale(%g,%g)\"><foreignObject x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"xMinYMin slice\" href=\"%s\">
<body xmlns=\"http://www.w3.org/1999/xhtml\" style=\"margin: 0; padding: 0\">
<video id=\"sampleMovie\" autoplay=\"true\" style=\"display: block; margin: auto;\" src=\"%s\"></video>
</body>
</foreignObject></g>"
           i.video_x (offset-.i.video_y-.i.video_height)
           (i.video_width/.(float_of_int i.video_pixel_width))
           (i.video_height/.(float_of_int i.video_pixel_height))
           i.video_pixel_width i.video_pixel_height name
           name)
    )
    | States s->List.iter output_contents_aux s.states_contents
    | Link l->(
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );

      (match l.link_kind with
	Intern(label,dest_page,dest_x,dest_y) ->
          Rbuffer.add_string svg_buf
            (Printf.sprintf "<a xlink:href=\"#%d_%d\">"
               dest_page 0
            );
      | Extern uri ->
        Rbuffer.add_string svg_buf "<a xlink:href=\"";
        Rbuffer.add_string svg_buf uri;
        Rbuffer.add_string svg_buf "\">"
      | Button(Menu(items) as btype, name) ->
         begin
           match buttonCache with
           | None -> ()
           | Some c -> Hashtbl.replace c name btype
         end;
         List.iteri (fun i (w,c) ->
             let (x0,y0,x1,y1) = RawContent.bounding_box_full c in
             let x0 = min x0 0.0 in
             let (x0',_,_,_) = RawContent.bounding_box_kerning c in
             let h = y1 -. y0 in
             let w = x1 -. x0 in
             Rbuffer.add_string svg_buf
                (Printf.sprintf "<g class='svg_item' name='%s' id='%d'
                                    box_w='%f' box_h='%f' box_x='%f' box_y='%f'
                                    transform='translate(%f,0)'>"
                                name i w h x0 (-.y1) (x0' -. x0));
             (* NOTE: the translate is here because a transform-orgin of 50%
                is there in user agent css and can not be disables ... *)
             ignore (outcont ~offset:0. ~svg_buf c);
             Rbuffer.add_string svg_buf "</g>") items;
         Rbuffer.add_string svg_buf
             (Printf.sprintf "<a onclick='make_menu(evt,\"%s\");'>" name)
      | Button(btype,name) ->
         begin
           match buttonCache with
           | None -> ()
           | Some c -> Hashtbl.replace c name btype
         end;
	 let ty = match btype with
          | Menu _ -> assert false
	  | Click _ -> "button"
	  | Drag  _ -> "dragable"
	  | Edit(s, init, _) -> "editable' contents='" ^
	    Str.(global_replace (regexp "\r?\n") "&#13;&#10;"
		   (global_replace (regexp_string ">") "&gt;"
		      (global_replace (regexp_string "<") "&lt;"
			 (global_replace (regexp_string "\'") "&apos;"
			    (global_replace (regexp_string "\"") "&quot;"
			       (global_replace (regexp_string "&") "&amp;" s)))))) ^
	    "' initial='" ^
	    Str.(global_replace (regexp "\r?\n") "&#13;&#10;"
		   (global_replace (regexp_string ">") "&gt;"
		      (global_replace (regexp_string "<") "&lt;"
			 (global_replace (regexp_string "\'") "&apos;"
			    (global_replace (regexp_string "\"") "&quot;"
			                    (global_replace (regexp_string "&") "&amp;" init))))))
	in
        Rbuffer.add_string svg_buf (
                             Printf.sprintf "<g class='%s' id='%s'>" ty name
	);
      );

      ignore (outcont ~svg_buf l.link_contents);

      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      (match l.link_kind with
      | Button(Menu _, _) ->
	Rbuffer.add_string svg_buf "</a>"
      | Button(_,_) ->
	Rbuffer.add_string svg_buf "</g>"
      | _ ->
	Rbuffer.add_string svg_buf "</a>")
    )
    | Dynamic d ->
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      Rbuffer.add_string svg_buf (Printf.sprintf "<g id=\"%s\">" d.dyn_label);
      (match dynCache with
	 None ->
	 List.iter output_contents_aux (d.dyn_contents ());
       | Some (ds,gs,slide,state,record) ->
	  (* <use> and <defs> would be much better ... but click inside
             defs does not work with firefox (bug reported for more
             than one year *)
	  let tmp = Rbuffer.create 256 in
	  ignore (outcont ~svg_buf:tmp (d.dyn_contents ()));
	  ignore (outcont ~svg_buf:tmp (d.dyn_sample));
          let ptr = ref None in
 	  let rec contents () =
	    let buf = Rbuffer.create 256 in
            let contents, reads = record d.dyn_contents () in
            List.iter (fun key ->
                let old = try Hashtbl.find gs key with Not_found -> [] in
                if not (List.memq d0 old) then
	          Hashtbl.add gs key (d0::old)) reads;
	    ignore (output_contents ~create_new_class:false ~svg_buf:buf contents);
	    Rbuffer.contents buf
	  and d0 = ({ d with dyn_contents = contents; dyn_sample = "" }, ptr,
                    slide, state)
          in
	  Hashtbl.add ds d.dyn_label d0
      );
      Rbuffer.add_string svg_buf "</g>";

    | Affine a->(
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );

      Rbuffer.add_string svg_buf (
        Printf.sprintf "<g transform=\"matrix(%f,%f,%f,%f,%f,%f)\">"
          a.affine_matrix.(0).(0)
          (-.a.affine_matrix.(1).(0))
          (-.a.affine_matrix.(0).(1))
          (a.affine_matrix.(1).(1))
          (a.affine_matrix.(0).(2))
          (offset-.a.affine_matrix.(1).(2))
        ;
      );
      List.iter (output_contents_aux ~offset:0.) a.affine_contents;

      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      Rbuffer.add_string svg_buf "</g>";
    )
    | Animation a ->
      if !opened_tspan then (
        Rbuffer.add_string svg_buf "</tspan>\n";
        opened_tspan:=false
      );
      if !opened_text then (
        Rbuffer.add_string svg_buf "</text>\n";
        opened_text:=false
      );
      let prefix = !animate_count in
      incr animate_count;
      Rbuffer.add_string svg_buf (Printf.sprintf "<g class=\"animation\" nbframes=\"%d\" step=\"%d\" mirror=\"%d\" id=\"%d\">"
				    (Array.length a.anim_contents) (truncate (a.anim_step *. 1000.))
				    (if a.anim_mirror then 1 else 0) prefix );

      Array.iteri (fun i c ->
	Rbuffer.add_string svg_buf (
	  Printf.sprintf "<g id=\"Animation_%d_%d\" visibility=\"%s\">\n"
	    prefix i
	    (if i = a.anim_default then "inherit" else "hidden"));
        opened_tspan:=false;
        opened_text:=false;
	List.iter output_contents_aux (a.anim_contents.(i));
	if !opened_tspan then (
          Rbuffer.add_string svg_buf "</tspan>\n";
          opened_tspan:=false
	);
	if !opened_text then (
          Rbuffer.add_string svg_buf "</text>\n";
          opened_text:=false
	);
	Rbuffer.add_string svg_buf "</g>\n") a.anim_contents;
      Rbuffer.add_string svg_buf "</g>\n"
  in
  let raws=
    let x=List.fold_left (fun m x->
      let m'=try IntMap.find (drawing_order x) m with Not_found->[] in
      IntMap.add (drawing_order x) (x::m') m
    ) IntMap.empty (drawing_sort contents)
    in
    let comp a b=match a,b with
        Glyph ga,Glyph gb->if ga.glyph_y=gb.glyph_y then compare ga.glyph_x gb.glyph_x
          else compare gb.glyph_y ga.glyph_y
      | Glyph ga,_-> -1
      | _,Glyph gb->1
      | _->0
    in
    let subsort a=match a with
      | Link l->
         let l = match l.link_kind with
           | Button(Menu(items), name) ->
              let items = List.map (fun (f,c) -> (f, List.sort comp c)) items in
              let link_kind = Button(Menu(items), name) in
              { l with link_kind }
           | _ -> l
         in
         Link { l with link_contents=List.sort comp l.link_contents }
      | b->b
    in
    IntMap.fold (fun _ a x->x@a) (IntMap.map (fun l->(List.sort comp (List.map subsort l))) x) []
  in
  List.iter output_contents_aux raws;
  if !opened_tspan then (
    Rbuffer.add_string svg_buf "</tspan>\n";
  );
  if !opened_text then (
    Rbuffer.add_string svg_buf "</text>\n";
  );
  !imgs
  in
  let imgs = output_contents ~svg_buf contents in
  svg_buf,imgs




let buffered_output' ?dynCache ?(structure:structure=empty_structure) pages prefix=

  let total=Array.fold_left (fun m x->m+Array.length x) 0 pages in
  let all_pages=Array.make total (empty_page (0.0,0.0)) in
  let _=Array.fold_left (fun m0 x->
    Array.fold_left (fun m x->
      all_pages.(m)<-x;
      m+1
    ) m0 x
  ) 0 pages
  in
  let cache=build_font_cache prefix (Array.map (fun x->x.contents) all_pages) in
  let imgs=ref StrMap.empty in
  let svg_files=Array.mapi (fun slide pi->
    Array.mapi (fun state page ->
      let file0=Rbuffer.create 10000 in
      let file=Rbuffer.create 10000 in
        (* Printf.sprintf "%s_%d_%d.svg" chop_file i j *)
      let w,h=page.size in
      Rbuffer.add_string file0 (Printf.sprintf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet href=\"style.css\" type=\"text/css\"?>
<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 %d %d\"><g>"
                                 (round (w)) (round (h)));

      let sorted_pages = sort_raw page.contents in
      let dynCache = match dynCache with
	  None -> None
	| Some (c,g,f) -> Some (c.(slide).(state), g, slide, state, f)
      in
      let svg,imgs0=draw ~fontCache:cache ?dynCache prefix w h sorted_pages in
      imgs:=StrMap.fold StrMap.add imgs0 !imgs;
      Rbuffer.add_buffer file svg;
      Rbuffer.add_string file "</g></svg>\n";
      file0, file
    ) pi
  ) pages
  in
  if !font_filter <> "" then filter_fonts !font_filter cache;
  svg_files,cache,!imgs

let default_script = ""

let basic_html ?(extraheader="") ?(extrabody="") ?script:(script=default_script) ?onload:(onload="") ?onhashchange:(onhashchange="")
    ?keyboard
    cache structure pages prefix=
  let html=Rbuffer.create 10000 in
  let w,h=if Array.length pages>0 then (pages.(0)).(0).size else 0.,0. in
  let keyboard=match keyboard with
      None->Printf.sprintf "window.onkeydown=function(e){
if(e.keyCode==37 || e.keyCode==38 || e.keyCode==33){
if(current_slide > 0 && (current_state<=0 || e.keyCode==38)) {
  loadSlide(current_slide-1,states[current_slide-1]-1)
} else if (current_state > 0) {
  loadSlide(current_slide,current_state-1)
}
} //left
if(e.keyCode==39 || e.keyCode==40 || e.keyCode==34){
if(current_slide < %d && (current_state>=states[current_slide]-1 || e.keyCode==40)) {
  loadSlide(current_slide+1,0)
} else if (current_state < states[current_slide] - 1) {
  loadSlide(current_slide,current_state+1)
}
} else //right
if(e.keyCode==82){ //r
loadSlide(current_slide,current_state);
} else if(e.key==\"f\" || e.key==\"F\" || e.keyCode==70) {
if(document.mozCancelFullScreen){
if(document.mozFullScreenElement){
document.mozCancelFullScreen();
}else{
document.getElementById(\"svg_svg\").mozRequestFullScreen();
}
} else if(document.webkitCancelFullScreen){
if(document.webkitFullScreenElement){
document.webkitCancelFullScreen();
}else{
document.getElementById(\"svg_svg\").requestFullScreen();
}}}};

function gotoSlide(n){
if(n>current_slide)
  loadSlide(n,0);
else if(n<current_slide)
  loadSlide(n,0);
}" (Array.length pages - 1)
    | Some x->x
  in


  Rbuffer.add_string html
    "<!DOCTYPE html>
<html lang=\"en\">
<head>
<link href=\"style.css\" rel=\"stylesheet\" type=\"text/css\">
<meta charset=\"utf-8\">
<title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title>\n";
  Rbuffer.add_string html extraheader;
  Rbuffer.add_string html "<script>\n";
  Rbuffer.add_string html script;

  let states=Rbuffer.create 10000 in
  for i=0 to Array.length pages-1 do
    if Rbuffer.length states>0 then Rbuffer.add_string states ",";
    Rbuffer.add_string states (string_of_int (Array.length (pages.(i))))
  done;
  Rbuffer.add_string html "var states=[";
  Rbuffer.add_buffer html states;
  Rbuffer.add_string html "];";

  Rbuffer.add_string html (
    Printf.sprintf "
var current_slide=0;
var current_state=0;
var first_displayed=false;
var cur_child = new Array();
var animations = new Array();
var cur_anim=0;

function Animate(name,nbframes,mirror,step) {
    var i = 0; var d = 1;
    var cur = document.getElementById('Animation_'+name+'_'+i.toString());
    animations.push(setInterval(function () {
      i = i + d;
      if (i >= nbframes) {
        if (mirror) { i = i - 2; d = -1; } else { i = 0; }
      }
      else if (i < 0) { i = 2; d = 1; }
      var next = document.getElementById('Animation_'+name+'_'+i.toString());
      next.setAttribute('visibility','inherit');
      cur.setAttribute('visibility','hidden');
      cur = next;
    }, step));
  }


function setReaction(svg) {

    /* move definitions (can not use <use> and <defs>
                         because of a firefox bug) */
    var defs_elt = document.getElementById('svg_defs')
    if (defs_elt) {
      var defs = defs_elt.childNodes;
      for (var i = 0; i < defs.length; i++) {
        var id = defs[i].id;
        id=id.substring(1,id.length); // remove leading @
	var dest = document.getElementById(id);
	while (defs[i].hasChildNodes()) {
	  dest.appendChild(defs[i].lastChild);
	}
      }
      defs_elt.parentNode.removeChild(defs_elt);
    }

    var anim2=svg.getElementsByClassName('animation');

    for (var a=0;a<anim2.length;a++) {
        Animate(anim2[a].getAttribute('id'),
                anim2[a].getAttribute('nbframes'),
                anim2[a].getAttribute('mirror'),
                anim2[a].getAttribute('step')); }

    var buttons=svg.getElementsByClassName('button');
    for (var a=0;a<buttons.length;a++) {
        function closure(name) {
          return(function (e) { send_click(name,e); });
        }
        var elt = buttons[a];
        elt.style.pointerEvents = 'all';
        elt.onclick=closure(elt.getAttribute('id'));
        elt.onmouseover=(function () { document.body.style.cursor = 'crosshair'; });
        elt.onmouseout=(function () { document.body.style.cursor = 'default'; });
    }

    var dragable=svg.getElementsByClassName('dragable');
    for (var a=0;a<dragable.length;a++) {
        var elt = dragable[a];
        elt.style.pointerEvents = 'all';
        var name = elt.getAttribute('id');
        function closure2(elt,name,touch) {
          return(function (e) {
            if (touch) e.preventDefault();
            start_drag(elt,name,e,touch); });
        }
        elt.onmousedown=closure2(elt,name,false);
        elt.addEventListener('touchstart', closure2(elt,name, true));
        elt.onmouseover=(function () { document.body.style.cursor = 'move'; });
        elt.onmouseout=(function () { document.onselectstart = null; document.body.style.cursor = 'default'; });
    }

    var editable=svg.getElementsByClassName('editable');
    for (var a=0;a<editable.length;a++) {
        var elt = editable[a];
        elt.style.pointerEvents = 'all';
        var name = elt.getAttribute('id');
        function closure3(name) {
          return(function (e) { start_edit(name,e); });
        }
        elt.onclick=closure3(name);
        elt.onmouseover=(function () { document.body.style.cursor = 'text'; });
        elt.onmouseout=(function () { document.body.style.cursor = 'default'; });
    }

    var videos=document.getElementsByTagName(\"video\");
    for(var i=0;i<videos.length;i++) videos[i].controls=true;
}

function loadSlideString(slide,state,str){
    var old_anim=cur_anim;
    if(slide!=current_slide)
       cur_anim=(cur_anim+1)%%2;
    var svg=document.getElementById(\"svg_container\"+cur_anim);
    while (animations.length > 0) {
       clearInterval(animations.shift());
    }

    var parser=new DOMParser();
    var newSvg=parser.parseFromString(str,\"text/xml\");
    newSvg=document.importNode(newSvg.documentElement,true).firstChild;

    cur_outer=document.getElementById('svg_outer'+cur_anim);
    try {
      svg_container=document.getElementById('svg_container'+cur_anim);
      cur_outer.removeChild(svg_container);
    } catch (e) {}
    cur_outer.appendChild(newSvg);
    newSvg.id ='svg_container'+cur_anim;

    if (current_slide < slide) {
      var animO=document.getElementById(\"animation\"+old_anim);
      animO.setAttribute(\"values\",\"0;%d\");
      animO.beginElement();
      var animT=document.getElementById(\"animation\"+cur_anim);
      animT.setAttribute(\"values\",\"%d;0\");
      animT.beginElement();
    }
    else if (current_slide > slide) {
      var animO=document.getElementById(\"animation\"+old_anim);
      animO.setAttribute(\"values\",\"0;%d\");
      animO.beginElement();
      var animT=document.getElementById(\"animation\"+cur_anim);
      animT.setAttribute(\"values\",\"%d;0\");
      animT.beginElement();
    }

    if (current_slide != slide) {
      old_outer=document.getElementById('svg_outer'+old_anim);
      try {
        svg_container=document.getElementById('svg_container'+old_anim);
        old_outer.removeChild(svg_container);
      } catch (e) {}
    }
    current_slide=slide;
    current_state=state;
    first_displayed=true;
    location.hash=slide+\"_\"+state;

    setReaction(newSvg);
}

function close_menu(n,i) {
   var id = i;
   var name = n;
   return (function () {
     var sel = 'div[name=\"' + name + '\"]';
     var div =document.body.querySelectorAll(sel)[0];
     table=div.firstChild;
     var svg_svg = document.getElementById('svg_svg');
     while (table.hasChildNodes()) {
       var row = table.firstChild;
       var td = row.firstChild;
       var svg = td.firstChild;
       var g = svg.firstChild;
       g.style.opacity = 0.0;
       document.body.appendChild(svg_svg);
       table.removeChild(row);
     }
     document.body.removeChild(div);
     if (typeof websocket_send === 'function') {
       websocket_send('menu_'+(current_slide)+'_'+(current_state)+' '+n+' '+id);
     }
     //console.log('menu: ', n, id)
  });
}

function make_menu(evt,name) {
   var sel = 'div[name=\"' + name + '\"]';
   var div =document.body.querySelectorAll(sel);
   if (div.length != 0) return; //menu already open
   var svg_svg = document.getElementById('svg_svg');
   var box = svg_svg.getBBox();
   var wsvg = svg_svg.getBoundingClientRect();
   var ratio = wsvg.width / box.width;
   var div = document.createElement('div');
   div.style.position = 'absolute';
   div.style.left = evt.pageX.toString() + 'px';
   div.style.top = evt.pageY.toString() + 'px';
   div.style.zIndex = '20';
   div.setAttribute('name',name);
   var table = document.createElement('table');
   table.setAttribute('class','menu');
   div.appendChild(table);

   var sel = 'svg g[name=\"' + name + '\"]';
   var items =[].slice.call(document.body.querySelectorAll(sel));
   items.sort(function (x, y) {
     var ix = x.getAttribute('id');
     var iy = y.getAttribute('id');
     return(ix - iy)});
   // sometime duplicate append. How is this possible ?
   for(var i=0;i<items.length-1;i++) {
     var i1 = items[i].getAttribute('id');
     var i2 = items[i+1].getAttribute('id');
     if (i1 == i2) items.splice(i,1);
   }
   for(var i=0;i<items.length;i++) {
      var row = document.createElement('tr');
      table.appendChild(row);
      var td  = document.createElement('td');
      td.setAttribute('class', 'svg_item');
      row.appendChild(td);
      var svg = document.createElementNS('http://www.w3.org/2000/svg','svg');
      td.appendChild(svg);
      items[i].style.opacity = 1.0;
      svg.appendChild(items[i]);
      var w = items[i].getAttribute('box_w');
      var h = items[i].getAttribute('box_h');
      var x = items[i].getAttribute('box_x');
      var y = items[i].getAttribute('box_y');
      svg.setAttribute('viewBox',
        String(x) + ' ' + String(y) + ' ' + String(w) + ' ' + String(h));
      var name = items[i].getAttribute('name');
      var id = items[i].getAttribute('id');
      td.onclick = close_menu(name,id);
      svg.style.width = String(w * ratio) + 'px';
      svg.style.height = String(h * ratio) + 'px';
   }
   document.body.appendChild(div);
}

function loadSlide(n,state,force){
  if(n>=0 && n<%d && state>=0 && state<states[n] && (force || n!=current_slide || state!=current_state || !first_displayed)) {

    document.body.style.cursor = 'wait';
    var xhttp=new XMLHttpRequest();
    xhttp.open(\"GET\",n+\"_\"+state+\".svg\",false);
    xhttp.send();
    if(xhttp.status==200 || xhttp.status==0){
      loadSlideString(n,state,xhttp.responseText);
    }
    document.body.style.cursor = 'default';
  }
}"
      (-round w)
      (round w)
      (round w)
      (-round w)
      (Array.length pages)
  );

  Rbuffer.add_string html (
    Printf.sprintf "
window.onload=function(){
var h0=0,h1=0;
if(location.hash){
var i=location.hash.indexOf(\"_\");
h0=location.hash?parseInt(location.hash.substring(1,i)):0;
h1=location.hash?parseInt(location.hash.substring(i+1)):0;
}
%s
};

window.onhashchange=function(){
var h0=0,h1=0;
if(location.hash){
var i=location.hash.indexOf(\"_\");
h0=location.hash?parseInt(location.hash.substring(1,i)):0;
h1=location.hash?parseInt(location.hash.substring(i+1)):0;
}
if(h0!=current_slide || h1!=current_state){
%s
}
};
%s
</script>"
      (if onload="" then "loadSlide(h0,h1);" else onload)
      (if onhashchange="" then "loadSlide(h0,h1);" else onhashchange)
      keyboard
  );

  Rbuffer.add_string html "<title>";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</title></head><body style=\"margin:0;padding:0;\"><div id=\"svg_div\" style=\"margin-top:auto;margin-bottom:auto;margin-left:auto;margin-right:auto;\">";
  Rbuffer.add_string html extrabody;
  Rbuffer.add_string html (Printf.sprintf "<svg id='svg_svg' xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 %d %d\"><g id='svg_outer0'>"
                             (round (w)) (round (h)));
  Rbuffer.add_string html
    (Printf.sprintf "<animateTransform attributeName=\"transform\" attributeType=\"XML\" id=\"animation0\" type=\"translate\" calcMode=\"spline\" fill=\"freeze\" keySplines=\"0.2 0 0.1 1\" values=\"0\" dur=\"0.7s\"/><g id=\"svg_container0\">"
    );
  Rbuffer.add_string html (Printf.sprintf "</g></g><g transform=\"translate(%d,0)\" id='svg_outer1'><animateTransform attributeName=\"transform\" attributeType=\"XML\" id=\"animation1\" type=\"translate\" calcMode=\"spline\" fill=\"freeze\" keySplines=\"0.2 0 0.1 1\" values=\"0\" dur=\"0.7s\"/><g id=\"svg_container1\">\n" (round w));

  Rbuffer.add_string html (Printf.sprintf "</g></g>\n");

  let style=make_defs prefix cache in
  Rbuffer.add_string html "<defs><style type=\"text/css\" src=\"style.css\"/></defs>\n";
  Rbuffer.add_string html structure.name;
  Rbuffer.add_string html "</svg></div></body></html>";
  html,style



let output' ?(structure:structure=empty_structure) pages filename=
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

  let svg_files,cache,imgs=buffered_output' ~structure:structure pages prefix in

  StrMap.fold (fun k a _->
    copy_file k (Filename.concat prefix a)
  ) imgs ();

  let html,style=basic_html cache structure pages prefix in
  let o=open_out (Filename.concat (Filename.basename prefix) "index.html") in
  Rbuffer.output_buffer o html;
  close_out o;
  let o=open_out (Filename.concat prefix "style.css") in
  Rbuffer.output_buffer o style;
  close_out o;

  Array.iteri (fun i->
    Array.iteri (fun j (x,y) ->
      let o=open_out (Filename.concat (Filename.basename prefix) (Printf.sprintf "%d_%d.svg" i j)) in
      Rbuffer.output_buffer o x;
      Rbuffer.output_buffer o y;
      close_out o
    )
	      ) svg_files;
  output_fonts cache

let output ?(structure:structure=empty_structure) pages filename=
  output' ~structure (Array.map (fun x->[|x|]) pages) filename





let images_of_boxes ?cache ?(css="style.css") ?(output_font_defs=true) prefix env conts_box=

  let raws=Array.map (fun cont->Document.draw_boxes env cont) conts_box in

  (if Sys.file_exists prefix && not (Sys.is_directory prefix) then
    Unix.unlink prefix;
  );
  (if not (Sys.file_exists prefix) then
      Unix.mkdir prefix 0o755);

  let raws=
    Array.map (fun cont->
      let x=List.fold_left (fun m x->
        let m'=try IntMap.find (drawing_order x) m with Not_found->[] in
        IntMap.add (drawing_order x) (x::m') m
      ) IntMap.empty (drawing_sort cont)
      in
      let comp a b=match a,b with
          Glyph ga,Glyph gb->if ga.glyph_y=gb.glyph_y then compare ga.glyph_x gb.glyph_x
            else compare gb.glyph_y ga.glyph_y
        | Glyph ga,_-> -1
        | _,Glyph gb->1
        | _->0
      in
      let subsort a=match a with
        | Link l->
           let l = match l.link_kind with
             | Button(Menu(items), name) ->
                let items = List.map (fun (f,c) -> (f, List.sort comp c)) items in
                let link_kind = Button(Menu(items), name) in
                { l with link_kind }
             | _ -> l
           in
           Link { l with link_contents=List.sort comp l.link_contents }
        | b->b
      in
      IntMap.fold (fun _ a x->x@a) (IntMap.map (fun l->(List.sort comp (List.map subsort l))) x) []

    ) raws
  in
  let cache=match cache with
      None->build_font_cache prefix raws
    | Some c->c
  in
  let css_file=if String.length css>0 then Filename.concat prefix css else "" in


  let r=Rbuffer.create 1000 in
  let imgs=Array.mapi (fun i x->
    Rbuffer.clear r;
    let _,w,_=boxes_interval (Array.of_list conts_box.(i)) in
    let x0,y0,x1,y1=bounding_box_full raws.(i) in
    let normal x=match classify_float x with
        FP_infinite | FP_nan->false | _->true
    in
    if normal x0 && normal y1 && normal x1 && normal y1 then (
      let y0=y0-.0.2 in
      let y1=y1+.0.2 in
      let h=(y1-.y0) in
      Rbuffer.add_string r (Printf.sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" overflow=\"visible\" width=\"%gpx\" height=\"%gmm\" viewBox=\"%g %g %g %g\" style=\"position:relative;bottom:%gmm;\">"
                              (ceil (x1-.floor x0))
                              (y1-.y0)
                              (floor x0) (h-.y1) (ceil (x1-.floor x0)) (y1-.y0)
                              (y0));

      let dr,imgs=draw ~fontCache:cache prefix w (y1 -. y0) raws.(i) in
      StrMap.fold (fun k a _->
        copy_file k (Filename.concat prefix a)
      ) imgs ();
      HtmlFonts.output_fonts cache;

    (* Rbuffer.add_string r (Printf.sprintf "<defs><style type=\"text/css\" src=\"%s\"/></defs>" css_file); *)
      Rbuffer.add_string r "<title></title>";
      Rbuffer.add_buffer r dr;
      Rbuffer.add_string r "</svg>\n";
    );
    Rbuffer.contents r;
  ) conts_box
  in
  let style=make_defs prefix cache in
  if String.length css_file>0 then (
    let css=open_out css_file in
    Rbuffer.output_buffer css style;
    close_out css
  );
  imgs

let images ?cache ?(css="style.css") prefix env conts=
  let conts_box=Array.map (fun x->Document.boxify_scoped env x) conts in
  match cache with
      Some a->images_of_boxes ~cache:a ~css:css prefix env conts_box
    | None->images_of_boxes ~css:css prefix env conts_box

let _ =
  Hashtbl.add DynDriver.drivers "SVG" (
    module struct
      let output  = output
      let output' = output'
    end : OutputDriver)
