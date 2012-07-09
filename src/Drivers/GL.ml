open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util

let cur_page = ref 0

let init_gl width height =
(*    GlDraw.shade_model `smooth;*)
    GlClear.color (1.0, 1.0, 1.0);
    GlClear.depth 1.0;
    GlClear.clear [`color; `depth];
    Gl.enable `depth_test;
    Gl.enable `polygon_smooth;
    Gl.enable `line_smooth;
    GlMisc.hint `polygon_smooth `nicest;
    GlFunc.depth_func `lequal

let filename x=""

let zoom = ref 1.0
let dx = ref 0.0
let dy = ref 0.0

let glyphCache = Hashtbl.create 1001
let imageCache = Hashtbl.create 1001

let output ?(structure:structure={name="";displayname=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let num_pages = Array.length pages in

  let links = 
    Array.mapi
      (fun i page ->
	let l = ref [] in
	List.iter
	  (function  
            Link(l') -> 
	      l := (l', i)::!l
	  | _ -> ())
	  page.pageContents;
	!l)
      pages;
  in

  let pixel_width = ref 0.0 in
  let pixel_height = ref 0.0 in

(* Handle window reshape events *)
  let reshape_cb ~w ~h =
    let ratio = (float_of_int w) /. (float_of_int h) in
    let page = !cur_page in
    let pw,ph=pages.(page).pageFormat in
    let cx = pw /. 2.0 +. !dx in
    let cy = ph /. 2.0 +. !dy in
    let rx = (ph *. ratio) /. 2.0 *. !zoom in
    let ry = ph /. 2.0 *. !zoom in
    pixel_width := rx *. 2.0 /. (float_of_int h);
    pixel_height := ry *. 2.0 /. (float_of_int w);

    let set_proj () =
      GlDraw.viewport 0 0 w h;
      GlMat.mode `projection;
      GlMat.load_identity ();
      GlMat.ortho (cx -. rx, cx +. rx) (cy -. ry, cy +. ry) (-1., 1.);
      GlMat.mode `modelview;
      GlMat.load_identity ()
    in

(*
    Glut.useLayer(Glut.OVERLAY);
    set_proj ();
    Glut.useLayer(Glut.NORMAL);
*)
    set_proj ()
  in

  let overlay_rect (r,g,b) (x,y,x',y') =
    GlMat.load_identity ();
    GlDraw.color (r,g,b);
    GlDraw.begins `line_loop;
    GlDraw.vertex2 (x,y');
    GlDraw.vertex2 (x',y');
    GlDraw.vertex2 (x',y);
    GlDraw.vertex2 (x,y);
    GlDraw.ends ();
  in
    
  let inverse_coord x y =
    let w = float (Glut.get Glut.WINDOW_WIDTH)
    and h = float (Glut.get Glut.WINDOW_HEIGHT) in
    let x = float x and y = h -. float y in
    let ratio = w /. h in
    let page = !cur_page in
    let pw,ph=pages.(page).pageFormat in
    let cx = pw /. 2.0 +. !dx in
    let cy = ph /. 2.0 +. !dy in
    let dx = (ph *. ratio) *. !zoom in
    let dy = ph *. !zoom in
    let x = (x -. w /. 2.0) /. w *. dx +. cx in
    let y = (y -. h /. 2.0) /. h *. dy +. cy in
    (x, y)
  in

  let find_link x y =
    let x,y = inverse_coord x y in
    List.filter (fun (l, i) ->
      l.link_x0 <= x && x <= l.link_x1 &&
      l.link_y0 <= y && y <= l.link_y1) links.(!cur_page)
  in

  let draw_gl_scene () =
    GlClear.clear [`color; `depth];
    GlMat.load_identity ();
    let page = !cur_page in

    List.iter (function
      Glyph g ->
	let x = g.glyph_x in
	let y = g.glyph_y  in
	let size = g.glyph_size in
	let s = 1.   /. 1000. *. size in
(*	let w = Fonts.glyphWidth g.glyph *. s in	*)
(*	Printf.fprintf stderr "x = %f, y = %f, dx = %f, dy = %f, s = %f\n"
	  x y dx dy size;*)

	let draw_glyph () = 
	  try
	    GlList.call (Hashtbl.find glyphCache g.glyph)
	  with
	    Not_found ->
	      let beziers =  Fonts.outlines g.glyph in
	      let lines = 
		List.map (fun bs -> 
		  let bs = List.flatten (List.map (Bezier.subdivise 1e-1) bs) in
		  List.map (fun (xs, ys) -> (xs.(0),ys.(0),0.0)) bs
		) beziers 
	      in
	      let l = GlList.create `compile_and_execute in
	      GluTess.tesselate  lines;
	      GlList.ends ();
	      Hashtbl.add glyphCache g.glyph l
	in
(*	
	List.iter (fun bs ->
	  List.iter (fun (x,y,z) -> 
	    Printf.fprintf stderr "(%f,%f,%f) " x y z) bs;

	  Printf.fprintf stderr "\n") lines; *)
	let r,g,b = match g.glyph_color with
	    RGB{red = r; green=g; blue=b;} -> r,g,b
	in
	flush stderr;
	GlDraw.color (r,g,b);
	GlMat.load_identity ();
	GlMat.translate3 (x +. !pixel_width/.4., y +. !pixel_height/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();
	GlMat.load_identity ();
	GlMat.translate3 (x -. !pixel_width/.4., y +. !pixel_height/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();
	GlMat.load_identity ();
	GlMat.translate3 (x +. !pixel_width/.4., y -. !pixel_height/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();
	GlMat.load_identity ();
	GlMat.translate3 (x -. !pixel_width/.4., y -. !pixel_height/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();

    | Path(param, beziers) ->
	let lines = List.map (fun line ->
	  let line = Array.to_list line in
	  let line = List.flatten (List.map (Bezier.subdivise  (1e-2 *. !zoom)) line) in
	  List.map
	    (fun (xa,ya) -> (xa.(0), ya.(0), 0.0),
	      (xa.(Array.length xa -1), ya.(Array.length ya - 1), 0.0)) line
	) beziers in
      (match param.fillColor with
	None -> ()
      | Some RGB{red = r; green=g; blue=b;} ->
	GlDraw.color (r,g,b);
	GlMat.load_identity ();
	GlMat.translate3 (!pixel_width/.4., !pixel_height/.4., 0.0);
	let l = GlList.create `compile_and_execute in
	List.iter (fun l -> GluTess.tesselate [List.map fst l]) lines;
	GlList.ends ();
	GlMat.load_identity ();
	GlMat.translate3 (-. !pixel_width/.4., !pixel_height/.4., 0.0);
	GlList.call l;
	GlMat.load_identity ();
	GlMat.translate3 (!pixel_width/.4., -. !pixel_height/.4., 0.0);
	GlList.call l;
	GlMat.load_identity ();
	GlMat.translate3 (-. !pixel_width/.4., -. !pixel_height/.4., 0.0);
	GlList.call l;
	GlList.delete l
      );
      (match param.strokingColor with
	None -> ()
      | Some RGB{red = r; green=g; blue=b;} ->
	GlDraw.color (r,g,b);
	List.iter (fun line ->
	  GlMat.load_identity ();
	  GlDraw.begins (if param.close then `line_loop else `line_strip);
	  GlDraw.line_width (param.lineWidth /. !pixel_width);
	  let first = ref true in
	  List.iter
	    (fun (a,b) ->
	      (*	      if !first then Printf.fprintf stderr "(%f, %f) " xa.(0) ya.(0);*)
	      if !first then (GlDraw.vertex3 a; first := false);
		(*	      Printf.fprintf stderr "(%f, %f) " xa.(Array.length xa -1) *)
              	(*	ya.(Array.length ya - 1);*)
		GlDraw.vertex3 b) line;
	      (*	Printf.fprintf stderr "\n"; flush stderr;*)
	  GlDraw.ends ();
	) lines);

    | Link(link) -> ()

    | Image i -> 
      GlMat.load_identity ();
      Gl.enable `texture_2d;
      begin     
	try 	  
	  GlTex.bind_texture `texture_2d (Hashtbl.find imageCache i)
	with Not_found ->
	  let image = Images.load i.image_file [] in
	  let w,h=Images.size image in
	  let image32 = match image with
	      Images.Rgba32 i -> i
	    | Images.Rgb24 i -> Rgb24.to_rgba32 i
	    | _ -> failwith "Unsupported"
	  in 
	  let raw = Raw.create `ubyte ~len:(4*w*h) in
	  for j=0 to h-1 do
            for i=0 to w-1 do
	      let rgba = Rgba32.get image32 i j in
	      Raw.set raw ((j * w + i) * 4 + 0) rgba.Color.color.Color.r;
	      Raw.set raw ((j * w + i) * 4 + 1) rgba.Color.color.Color.g;
	      Raw.set raw ((j * w + i) * 4 + 2) rgba.Color.color.Color.b;
	      Raw.set raw ((j * w + i) * 4 + 3) rgba.Color.alpha;
	    done
	  done;
	  let texture = GlPix.of_raw raw `rgba w h in  
	  let tid = GlTex.gen_texture () in
	  GlTex.bind_texture `texture_2d tid;
	  GlTex.image2d texture ~border:false;
	  GlTex.env (`mode `modulate);
	  GlTex.env (`color (1.0, 1.0, 1.0, 1.0));
	  GlTex.parameter ~target:`texture_2d (`min_filter `nearest);
	  GlTex.parameter ~target:`texture_2d (`mag_filter `nearest);    
	  Hashtbl.add imageCache i tid
      end;
      GlDraw.color (1.0,1.0,1.0);
      GlDraw.begins `quads;
      GlTex.coord2 (0., 1.);
      GlDraw.vertex2 (i.image_x, i.image_y);
      GlTex.coord2 (1., 1.);
      GlDraw.vertex2 (i.image_x +. i.image_width, i.image_y);
      GlTex.coord2 (1., 0.);
      GlDraw.vertex2 (i.image_x +. i.image_width, i.image_y +. i.image_height);
      GlTex.coord2 (0., 0.);
      GlDraw.vertex2 (i.image_x, i.image_y +. i.image_height);
      GlDraw.ends ();
      Gl.disable `texture_2d;
    ) pages.(page).pageContents
  in

  let redraw () =
    Glut.reshapeWindow (Glut.get Glut.WINDOW_WIDTH)  (Glut.get Glut.WINDOW_HEIGHT);
    Glut.postRedisplay ()
  in

  let keyboard_cb ~key ~x ~y =
    match key with
    | 27 (* ESC *) -> exit 0
    | 110 | 32 -> if !cur_page < num_pages - 1 then incr cur_page; redraw ();
    | 112 | 8 -> if !cur_page > 0 then decr cur_page; redraw ();
    | 43 -> zoom := !zoom /. 1.1; redraw ();
    | 45 -> zoom := !zoom *. 1.1; redraw ();
    | n -> Printf.fprintf stderr "Unbound key: %d (%s)\n" n (Char.escaped (Char.chr n)); flush stderr      
  in

  let special_cb ~key ~x ~y =
    match key with
    | Glut.KEY_DOWN -> dy := !dy -. 5.; redraw ();
    | Glut.KEY_UP -> dy := !dy +. 5.; redraw ();
    | Glut.KEY_LEFT -> dx := !dx -. 5.; redraw ();
    | Glut.KEY_RIGHT -> dx := !dx +. 5.; redraw ();
    | b -> Printf.fprintf stderr "Unbound special: %s\n" (Glut.string_of_special b); flush stderr;
  in

  let motion_ref = ref None in

  let motion_cb ~x ~y =
    match !motion_ref with
      None -> ()
    | Some (x', y') ->
      let mx = float (x - x') and my = float (y - y') in
      motion_ref := Some (x, y);
      dx := !dx -. mx *. !pixel_width;
      dy := !dy +. my *. !pixel_height;
      redraw ();
  in

  let previous_links = ref [] in

  let passive_motion_cb ~x ~y =
    let l = find_link x y in
    if l <> !previous_links then (
      draw_gl_scene ();
      previous_links := l;
      if l = [] then Glut.setCursor Glut.CURSOR_INHERIT;
      List.iter (fun (l, _) ->
	let color = 
	  if String.length(l.uri) >= 5 &&
	    String.sub l.uri 0 5 = "edit:" then (
	    Glut.setCursor Glut.CURSOR_TEXT;
	      (1.0,0.0,0.0)
	    )
	  else (
	    Glut.setCursor Glut.CURSOR_INFO;
	    if l.uri <> "" then (0.0,0.0,1.0) else (0.0,1.0,0.0)
	  )
	in
	overlay_rect color (l.link_x0,l.link_y0,l.link_x1,l.link_y1);
      ) l;
      Glut.swapBuffers ();
    )
  in
      
  let mouse_cb ~button ~state ~x ~y =
    match button, state with
    | Glut.RIGHT_BUTTON, Glut.DOWN ->
      motion_ref := Some (x, y);

    | Glut.OTHER_BUTTON(3), Glut.UP -> zoom := !zoom /. 1.1; redraw ();
    | Glut.OTHER_BUTTON(4), Glut.UP -> zoom := !zoom *. 1.1; redraw ();

    | Glut.RIGHT_BUTTON, Glut.UP ->
      motion_ref := None;

    | Glut.LEFT_BUTTON, Glut.UP ->
	List.iter 
	  (fun (l, i) ->
	    Printf.fprintf stderr 
	      "link cliqued: uri = %s, dest_page = %d, dest_x = %f, dest_y = %f\n"
	      l.uri l.dest_page l.dest_x l.dest_y;
	    flush stderr;
	    if l.uri = "" then
	      begin
		cur_page := l.dest_page;
		redraw ();
	      end
	    else
	      begin
		try
		  let browser = Sys.getenv "BROWSER" in
		  ignore (Sys.command (Printf.sprintf "%s \"%s\"" browser l.uri));
		with
		  Not_found -> 
		    Printf.fprintf stderr "%s: BROWSER environment variable undefined" Sys.argv.(0)
	      end
	  )
	  (find_link x y)
    | b, Glut.UP -> 
      Printf.fprintf stderr "Unbound button: %s\n" (Glut.string_of_button b);
      flush stderr
    | _ -> ()
  in

  let display_cb () = 
    draw_gl_scene ();
    Glut.swapBuffers ()
  in

  let main () =
    let 
	width = 640 and
	height = 480 
    in
    ignore (Glut.init Sys.argv);
    Glut.initDisplayString "rgba double samples>=32";
    Glut.initWindowSize width height;
    ignore (Glut.createWindow "Patoline OpenGL Driver");
    Printf.fprintf stderr "Number of samples: %d\n" (Glut.get Glut.WINDOW_NUM_SAMPLES);
    flush stderr;
    Glut.displayFunc display_cb;
    Glut.keyboardFunc keyboard_cb;
    Glut.specialFunc special_cb;
    Glut.reshapeFunc reshape_cb;
    Glut.mouseFunc mouse_cb;
    Glut.motionFunc motion_cb;
    Glut.passiveMotionFunc passive_motion_cb;
    init_gl width height;
    Glut.mainLoop ()
  in

  main ()
