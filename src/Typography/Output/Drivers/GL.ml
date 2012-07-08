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

(* Handle window reshape events *)
  let reshape_cb ~w ~h =
    let ratio = (float_of_int w) /. (float_of_int h) in
    let page = !cur_page in
    let pw,ph=pages.(page).pageFormat in
    GlDraw.viewport 0 0 w h;
    GlMat.mode `projection;
    GlMat.load_identity ();
    let cx = pw /. 2.0 +. !dx in
    let cy = ph /. 2.0 +. !dy in
    let rx = (ph *. ratio) /. 2.0 *. !zoom in
    let ry = ph /. 2.0 *. !zoom in
    pixel_width := rx *. 2.0 /. (float_of_int h);
    GlMat.ortho (cx -. rx, cx +. rx) (cy -. ry, cy +. ry) (-1., 1.);
    GlMat.mode `modelview;
    GlMat.load_identity ()
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
	let w = Fonts.glyphWidth g.glyph *. s in
	
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
	GlMat.translate3 (x +. !pixel_width/.4., y +. !pixel_width/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();
	GlMat.load_identity ();
	GlMat.translate3 (x -. !pixel_width/.4., y +. !pixel_width/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();
	GlMat.load_identity ();
	GlMat.translate3 (x +. !pixel_width/.4., y -. !pixel_width/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();
	GlMat.load_identity ();
	GlMat.translate3 (x -. !pixel_width/.4., y -. !pixel_width/.4., 0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph ();
    | Path(param, beziers) ->
	let lines = List.map (fun line ->
	  let line = List.rev (Array.to_list line) in
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
	GlMat.translate3 (!pixel_width/.4., !pixel_width/.4., 0.0);
	let l = GlList.create `compile_and_execute in
	List.map (fun l -> GluTess.tesselate [List.map fst l]) lines;
	GlList.ends ();
	GlMat.load_identity ();
	GlMat.translate3 (-. !pixel_width/.4., !pixel_width/.4., 0.0);
	GlList.call l;
	GlMat.load_identity ();
	GlMat.translate3 (!pixel_width/.4., -. !pixel_width/.4., 0.0);
	GlList.call l;
	GlMat.load_identity ();
	GlMat.translate3 (-. !pixel_width/.4., -. !pixel_width/.4., 0.0);
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
    | _ -> ())
      pages.(page).pageContents;

    Glut.swapBuffers ()
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
    | n -> Printf.fprintf stderr "Unbound key: %d\n" n; flush stderr      
  in

  let special_cb ~key ~x ~y =
    match key with
    | Glut.KEY_DOWN -> dy := !dy -. 5.; redraw ();
    | Glut.KEY_UP -> dy := !dy +. 5.; redraw ();
    | Glut.KEY_LEFT -> dx := !dx -. 5.; redraw ();
    | Glut.KEY_RIGHT -> dx := !dx +. 5.; redraw ();
    | _ -> ();
  in

  let mouse_cb ~button ~state ~x ~y =
    match button, state with
      Glut.LEFT_BUTTON, Glut.UP ->
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
    | _ -> ()
  in

  let main () =
    let 
	width = 640 and
	height = 480 
    in
    ignore (Glut.init Sys.argv);
    Glut.initDisplayMode ~multisample:true ~alpha:true ~depth:true ~double_buffer:true ();
    Glut.initWindowSize width height;
    ignore (Glut.createWindow "Patoline OpenGL Driver");
    Printf.fprintf stderr "Number of samples: %d\n" (Glut.get Glut.WINDOW_NUM_SAMPLES);
    flush stderr;
    Glut.displayFunc draw_gl_scene;
    Glut.keyboardFunc keyboard_cb;
    Glut.specialFunc special_cb;
    Glut.reshapeFunc reshape_cb;
    Glut.mouseFunc mouse_cb;
    init_gl width height;
    Glut.mainLoop ()
  in

  main ()
