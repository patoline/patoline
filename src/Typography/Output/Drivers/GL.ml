open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util

let cur_page = ref 0

let init_gl width height =
    GlDraw.shade_model `smooth;
    GlClear.color (1.0, 1.0, 1.0);
    GlClear.depth 1.0;
    GlClear.clear [`color; `depth];
    Gl.enable `depth_test;
    GlFunc.depth_func `lequal;
    GlMisc.hint `perspective_correction `nicest

let filename x=""


let output ?(structure:structure={name="";displayname=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let coord x=x in

  let num_pages = Array.length pages in

(* Handle window reshape events *)
  let reshape_cb ~w ~h =
    let ratio = (float_of_int w) /. (float_of_int h) in
    let page = !cur_page in
    let pw,ph=pages.(page).pageFormat in
    GlDraw.viewport 0 0 w h;
    GlMat.mode `projection;
    GlMat.load_identity ();
    GlMat.ortho (0.,ph*.ratio) (0.,ph) (-1.,1.);
    GlMat.mode `modelview;
    GlMat.load_identity ()
  in

  let draw_gl_scene () =
    GlClear.clear [`color; `depth];
    GlMat.load_identity ();
    let page = !cur_page in

    List.iter (function
      Glyph g ->
	let x = coord g.glyph_x in
	let y = coord g.glyph_y  in
	let s = coord g.glyph_size in
	let dx = coord (Fonts.glyphWidth g.glyph) /. 1000. *. s in
	let dy = coord (Fonts.glyph_y1 g.glyph) /. 1000. *. s in

(*	Printf.fprintf stderr "x = %f, y = %f, dx = %f, dy = %f, s = %f\n"
	  x y dx dy s;*)
	GlMat.load_identity ();
	GlMat.translate3 (x, y, 0.0);
	GlDraw.color (0.0, 0.0, 0.0);
	GlDraw.begins `quads;
	GlDraw.vertex3 (0.0,  0.0, 0.0);
	GlDraw.vertex3 (dx,  0.0, 0.0);
	GlDraw.vertex3 (dx,  dy, 0.0);
	GlDraw.vertex3 (0.0, dy, 0.0);
	GlDraw.ends ();
    | _ -> ())
      pages.(page).pageContents;

    Glut.swapBuffers ()
  in

  let keyboard_cb ~key ~x ~y =
    match key with
    | 27 (* ESC *) -> exit 0
    | 110 | 32 -> if !cur_page < num_pages - 1 then incr cur_page; draw_gl_scene ()
    | 112 | 8 -> if !cur_page > 0 then decr cur_page; draw_gl_scene ()
    | n -> Printf.fprintf stderr "Unbound key: %d\n" n; flush stderr      
  in

  let main () =
    let 
	width = 640 and
	height = 480 
    in
    ignore (Glut.init Sys.argv);
    Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
    Glut.initWindowSize width height;
    ignore (Glut.createWindow "O'Caml OpenGL Lesson 2");
    Glut.displayFunc draw_gl_scene;
    Glut.keyboardFunc keyboard_cb;
    Glut.reshapeFunc reshape_cb;
    init_gl width height;
    Glut.mainLoop ()
  in

  main ()
