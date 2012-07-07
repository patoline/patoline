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
    GlMat.ortho (cx -. rx, cx +. rx) (cy -. ry, cy +. ry) (-1., 1.);
    GlMat.mode `modelview;
    GlMat.load_identity ()
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
	flush stderr;
	GlMat.load_identity ();
	GlMat.translate3 (x, y, 0.0);
	GlMat.scale3 (s, s, s);
	GlDraw.color (0.0, 0.0, 0.0);
	draw_glyph ()
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
    | 110 | 32 -> if !cur_page < num_pages - 1 then incr cur_page; Glut.postRedisplay ()
    | 112 | 8 -> if !cur_page > 0 then decr cur_page; Glut.postRedisplay ()
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

  let main () =
    let 
	width = 640 and
	height = 480 
    in
    ignore (Glut.init Sys.argv);
    Glut.initDisplayMode ~multisample:true ~alpha:true ~depth:true ~double_buffer:true ();
    Glut.initWindowSize width height;
    ignore (Glut.createWindow "O'Caml OpenGL Lesson 2");
    Glut.displayFunc draw_gl_scene;
    Glut.keyboardFunc keyboard_cb;
    Glut.specialFunc special_cb;
    Glut.reshapeFunc reshape_cb;
    init_gl width height;
    Glut.mainLoop ()
  in

  main ()
