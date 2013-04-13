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
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util
open GLNet

type subpixel_anti_aliasing =
  No_SAA | RGB_SAA | BGR_SAA | VRGB_SAA | VBGR_SAA

type init_zoom = 
  FitWidth  | FitHeight | FitPage

type prefs = {
  subpixel_anti_aliasing : subpixel_anti_aliasing;
  graisse : float;
  tesselation_factor : float;
  init_zoom : init_zoom;
  rotation : float option;
  batch_cmd : 'a.((int -> int option -> int option -> subpixel_anti_aliasing -> (([< `ubyte] as 'a) Raw.t * int * int) array array) -> unit) option;
  server_port : int option;
}

let prefs = ref {
  subpixel_anti_aliasing = RGB_SAA;
  graisse = 0.1;
  tesselation_factor = 1.0/.4.0;
  init_zoom = FitPage;
  rotation = None;
  batch_cmd = None;
  server_port = None;
}

let cur_page = ref 0

let init_gl () =
    (*GlDraw.shade_model `smooth;*)
    GlFunc.color_mask ~red:true ~green:true ~blue:true ~alpha:true ();
    Gl.enable `depth_test;
    (*Gl.enable `polygon_smooth;
    GlMisc.hint `polygon_smooth `nicest;*)
    Gl.disable `line_smooth;
    GlFunc.depth_func `lequal;
    GlFBO.init_shader ()

let filename x=""

let zoom = ref 1.0
let dx = ref 0.0
let dy = ref 0.0
let subpixel saa =
  match saa with
    No_SAA -> None
  | RGB_SAA -> Some(1.0 /. 3.0, 0.0, -. 1.0 /. 3.0, 0.0)
  | BGR_SAA -> Some(-. 1.0 /. 3.0, 0.0, 1.0 /. 3.0, 0.0)
  | VRGB_SAA -> Some(0.0, 1.0 /. 3.0, 0.0, -. 1.0 /. 3.0)
  | VBGR_SAA -> Some(0.0, -. 1.0 /. 3.0, 0.0, 1.0 /. 3.0)

let flou_x () =
  match !prefs.subpixel_anti_aliasing with
    RGB_SAA | BGR_SAA -> 1.0 /. 2.0 | _ -> 1.0 /. 2.0


let flou_y () =
  match !prefs.subpixel_anti_aliasing with
    VRGB_SAA | VBGR_SAA -> 1.0 /. 2.0 | _ -> 1.0 /. 2.0


let glyphCache = Hashtbl.create 1001
#ifdef CAMLIMAGES
let imageCache = Hashtbl.create 1001
#endif

let rec last = function
[] -> assert false
  | [x] -> x
  | _::l -> last l
    
type page_mode = Single | Double

let pixel_width = ref 0.0
let pixel_height = ref 0.0
let cx = ref 0.0
let cy = ref 0.0
let rx = ref 0.0
let ry = ref 0.0

(* Handle window reshape events *)

let overlay_rect (r,g,b) (x,y,x',y') =
  Gl.disable `depth_test; (* FIXME: should not be necessary ? *)
  GlMat.load_identity ();
  GlDraw.color (r,g,b);
  GlDraw.begins `line_loop;
  GlDraw.vertex2 (x,y');
  GlDraw.vertex2 (x',y');
  GlDraw.vertex2 (x',y);
  GlDraw.vertex2 (x,y);
  GlDraw.ends ();
  Gl.enable `depth_test

let normalize (a,b) = 
  let n = 1. /. sqrt(a*.a +. b*.b) in
  let n = if classify_float n <> FP_normal then (
      print_string "Normalizing null vector!"; print_newline (); 1.0) else n in
  a *. n, b *. n

let dot_prod (a,b) (a',b') =
  a*.a' +. b*.b'

let combine orientation ratio (a,b) (a',b') =
  let ratio = if orientation then ratio else -. ratio in
  let c = a *. a' +. b *. b' in
  let s = a *. b' -. a' *. b in

  let (a0,b0),sgn = 
    if c > sqrt(2.0) /. 2.0 then
      normalize (a +. a', b +. b'), 1.0
    else 
      normalize (-.b +. b', a -. a'), -1.0
  in

  if s >= 0.0 then begin
    let l = (ratio *. a, ratio *. b) and r = (ratio *. a', ratio *. b') in
    (ratio*.a0,ratio*.b0),Some(l,r)
  end else begin
    let l = sgn *. ratio /. cos(acos(c) /. 2.0) in
    let l = if classify_float l <> FP_normal then ratio else l in
    (l*.a0,l*.b0),None
  end


let single ratio (a, b) =
  (ratio *. a, ratio *. b), None

let add_normals closed ratio beziers =

    let tesselation_factor = !prefs.tesselation_factor  in
  
    if closed then
      let beziers = Bezier.oriente beziers in
      List.split (List.map (fun (bs, orientation) -> 
	let bs = List.flatten (List.map (Bezier.subdivise (tesselation_factor *. ratio)) bs) in
	let prev, bs = 
	  let xe, ye = last bs in
	  let xs, ys = List.hd bs in
	  let prev = ref ( normalize (Bezier.derivee_approx ye, -. Bezier.derivee_approx xe)) in
	  let ex = xe.(Array.length xe - 1) and ey = ye.(Array.length ye - 1) in
	  let sx = xs.(0) and sy = ys.(0) in
	  if abs_float (ex -. sx) > 1e-6 || abs_float (ey -. sy) > 1e-6 then (
 	    prev, ([| ex; sx |], [| ey; sy |]) :: bs)
	  else
	    prev, bs
	in
	List.split (List.map (fun (xs, ys) -> 
	  let cur = normalize (Bezier.derivee_approx ys, -. Bezier.derivee_approx xs) in
	  let n = combine orientation ratio !prev cur in
	  prev :=  cur;
	  (xs.(0),ys.(0),0.0),n) bs)
      ) beziers) 
    else
      List.split (List.map (fun bs -> 
	let bs = List.flatten (List.map (Bezier.subdivise (tesselation_factor *. ratio)) bs) in
	let prev = ref None in
	let ln = 
	  List.map (fun (xs, ys) -> 
	    let n =
	      match !prev with
		None -> 
		  single ratio (normalize (Bezier.derivee_start ys, -. Bezier.derivee_start xs))
	      | Some(_,_,v) -> 
		  combine true ratio v (normalize (Bezier.derivee_start ys, -. Bezier.derivee_start xs))
	    in
	    prev :=  Some (xs, ys, normalize (Bezier.derivee_end ys, -. Bezier.derivee_end xs));
	    (xs.(0),ys.(0),0.0),n) bs
	in
	let last = 
	  match !prev with
	    None -> assert false
	  | Some(xs,ys,v) -> 
	    let s = Array.length xs in
	    let n = single ratio v in
	    (xs.(s - 1),ys.(s - 1),0.0),n
	in
	List.split (ln @ [last])
    ) beziers)   


let image_cache = Hashtbl.create 101
  
let win = ref None

let output' ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let pages = ref pages in
  let structure = ref structure in
  let num_pages = ref (Array.length !pages) in
  let num_states = ref (Array.length !pages.(!cur_page)) in
  let links = ref [||] in
  let cur_state = ref 0 in

  let tmp_image_dir = 
    if !prefs.server_port <> None then begin
      let dir = Filename.temp_file "patoline" "Image" in
      Sys.remove dir;
      Unix.mkdir dir 0o700;
      dir
    end else ""
  in

  let read_links () = 
    links := Array.mapi
      (fun i page ->
	Array.mapi (fun j state ->
	  let l = ref [] in
	  List.iter
	    (function  
            | Link(l') -> 
	      l := l'::!l
	    | _ -> ())
	    (drawing_sort state.pageContents);
	  !l) page)
      !pages;
  in

  let _ = read_links () in

  let inverse_coord x y =
    let w = float (Glut.get Glut.WINDOW_WIDTH)
    and h = float (Glut.get Glut.WINDOW_HEIGHT) in
    let x = float x and y = h -. float y in
    let ratio = w /. h in
    let page = !cur_page in
    let state = !cur_state in
    let pw,ph = !pages.(page).(state).pageFormat in
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
    List.filter (fun l ->
      l.link_x0 <= x && x <= l.link_x1 &&
      l.link_y0 <= y && y <= l.link_y1) !links.(!cur_page).(!cur_state)
  in

  let init_zoom = ref true in

  let set_proj xb yb =
    let w = Glut.get Glut.WINDOW_WIDTH in
    let h = Glut.get Glut.WINDOW_HEIGHT in
    GlDraw.viewport 0 0 w h;
    GlMat.mode `projection;
    GlMat.load_identity ();
    GlMat.translate3 (xb /. float w , yb /. float h , 0.0);
    GlMat.frustum ((!cx -. !rx)/.1000., (!cx +. !rx)/.1000.) 
      ((!cy -. !ry)/.1000., (!cy +. !ry)/.1000.) (1., 10000.);
    GlMat.translate3 (0., 0., -1000.);
    GlMat.mode `modelview;
    GlMat.load_identity ()
  in

  let reshape_cb ~w ~h =
    let ratio = (float_of_int w) /. (float_of_int h) in
    let page = !cur_page in
    let state = !cur_state in
    let pw,ph = !pages.(page).(state).pageFormat in
    if !init_zoom then begin
      init_zoom := false; dx := 0.0; dy := 0.0;
      if (pw /. ph < ratio && !prefs.init_zoom != FitWidth) || !prefs.init_zoom = FitHeight  then
	zoom := 1.0
      else
	zoom := (pw /. ph) /. ratio
    end;

    cx := pw /. 2.0 +. !dx;
    cy := ph /. 2.0 +. !dy;
    rx := (ph *. ratio) /. 2.0 *. !zoom;
    ry := ph /. 2.0 *. !zoom;
    pixel_width := !rx *. 2.0 /. (float_of_int w);
    pixel_height := !ry *. 2.0 /. (float_of_int h);
  in

  let is_edit uri =
    String.length(uri) >= 5 && String.sub uri 0 5 = "edit:"
  in

  let fps = ref 0.0 and cfps = ref 0 in

  let saved_rectangle = ref None in
  let to_revert = ref false in

  let old_menu = ref [] in
  let menu_item = Hashtbl.create 13 in

  let menu_cb ~value:i =
    try
      let (a,i) = Hashtbl.find menu_item i in
      Printf.printf "menu: %d of %d\n" i (Array.length a);
      flush stdout;
      let s = a.(i) in
      cur_page:= s.page;
      Glut.swapBuffers ()
    with Not_found -> ()
  in

  let create_menu structure = 
    if structure.substructures <> [||] then begin
      let c = ref 0 in
      List.iter (fun menu -> Glut.destroyMenu ~menu) !old_menu;
      let menu = Glut.createMenu menu_cb in
      old_menu := [menu];
      Glut.setMenu menu;
      let rec fn menu a i s = 
	Glut.addMenuEntry s.name !c;
	Hashtbl.add menu_item !c (a, i);
	incr c;
	if s.substructures <> [||] then
	  begin
	    let menu' = Glut.createMenu menu_cb in
	    old_menu := menu' :: !old_menu;
	    Glut.setMenu menu';
	    Array.iteri (fn menu' s.substructures) s.substructures;
	    Glut.setMenu menu;
	    Glut.addSubMenu "  ==>" menu';
	  end
      in
      Array.iteri (fn menu structure.substructures)  structure.substructures;
      Glut.attachMenu Glut.RIGHT_BUTTON
    end
  in

  let revert () = 
    let ch = open_in fileName in
    to_revert := false;
    let prime = input_value ch in
    structure := input_value ch;
    if prime then
      pages := input_value ch
    else 
      pages := Array.map (fun x -> [|x|]) (input_value ch);
    Printf.printf "Structure:\n";
    create_menu !structure;
    close_in ch;
    num_pages := Array.length !pages;
    if !cur_page > !num_pages - 1 then (cur_page := !num_pages - 1; cur_state := 0);
    num_states := Array.length (!pages.(!cur_page));
    if !cur_state > !num_states - 1 then (cur_state := !num_states - 1);
    read_links ();
    (* Not clearing the caches slows down a lot after redraw *)
#ifdef CAMLIMAGES
    Hashtbl.iter (fun _ t -> GlTex.delete_texture t) imageCache;
    Hashtbl.clear imageCache;
#endif
    Hashtbl.iter (fun _ l -> GlList.delete l) glyphCache;
    Hashtbl.clear glyphCache;
    Gc.compact ();
  in

  let start_page_time = ref (Unix.gettimeofday ()) in
  let cur_time = ref (Unix.gettimeofday ()) in
  let do_animation = ref false in

  let mode = ref Single in

  let draw_blank page state =
    let pw,ph = !pages.(page).(state).pageFormat in
    GlDraw.color ~alpha:0.0 (1.0, 1.0, 1.0);
    GlDraw.begins `quads;
    GlDraw.vertex2 (0., 0.);
    GlDraw.vertex2 (pw, 0.);
    GlDraw.vertex2 (pw, ph);
    GlDraw.vertex2 (0., ph);
    GlDraw.ends ();
  in
  
  let other_items = Hashtbl.create 13 in

    let draw_page page state =
      let graisse =  !prefs.graisse in
      let flou_x = flou_x () and flou_y = flou_y () in
      let graisse_x' = flou_x -. graisse and graisse_y' = flou_y -. graisse in
      let graisse_x = 3.0 *. flou_x +. graisse and graisse_y = 3.0 *. flou_y +. graisse in

      GlFunc.depth_mask true;
      GlFBO.merge_blend2 ();
      draw_blank page state;

      let fill_bezier color lines normals =
	let lines = List.map2 (fun l n -> List.map2 (fun (x,y,_) ((xn,yn),_) -> 
	  (x -. xn *. graisse_x', y -. yn *. graisse_y' , 0.0)) l n) lines normals in
	GlDraw.color color;
	GluTess.tesselate (*~tolerance:(scale/.5.)*) lines;
	
	let fn alpha ends (x,y,_) ((xn,yn),quality) =
	  match quality with 
	  | None ->
	    GlDraw.color color;
	    GlDraw.vertex2 (x,y);
	    GlDraw.color ~alpha color;
	    GlDraw.vertex2 (x+. graisse_x *. xn,y+. graisse_y *. yn)
	  | Some((a,b),(a',b')) ->
	    GlDraw.color color;
	    GlDraw.vertex2 (x,y);
	    GlDraw.color ~alpha color;
	    GlDraw.vertex2 (x+. graisse_x *. a,y+. graisse_y *. b);
	    if not ends then begin
	      GlDraw.color color;
	      GlDraw.vertex2 (x,y);
	      GlDraw.color ~alpha color;
	      GlDraw.vertex2 (x+. graisse_x *. xn,y+. graisse_y *. yn);
	      GlDraw.color color;
	      GlDraw.vertex2 (x,y);
	      GlDraw.color ~alpha color;
	      GlDraw.vertex2 (x+. graisse_x *. a',y+. graisse_y *. b')
	    end
	in
	      (*
		GlDraw.polygon_mode `both `line;
		List.iter2 (fun l n ->
		GlDraw.begins `quad_strip;
		List.iter2 (fn 1.0 false) l n;
		fn 1.0 true (List.hd l) (List.hd n);
		GlDraw.ends ();
		)	lines normals;
		GlDraw.polygon_mode `both `fill;
	      *)
	GlFBO.use_shader ();
	List.iter2 (fun l n ->
	  GlDraw.begins `quad_strip;
	  List.iter2 (fn 0.0 false) l n;
	  fn 0.0 true (List.hd l) (List.hd n);
	  GlDraw.ends ();
	) lines normals;
	GlFBO.no_shader ();
	
      in

      let rec fn = function
	| Animation(_,_,ft,fr) ->
	  do_animation := true;
	  let t = !cur_time -. !start_page_time in
	  let r = fr (ft t) in
	  List.iter fn (drawing_sort r);	  

	| Glyph g ->
        let x = g.glyph_x in
        let y = g.glyph_y  in
	let size = g.glyph_size in
	let s = size   /. 1000. in
	let ratio = !pixel_width /. s (* *. 10.0 *) in
(*
	Printf.fprintf stderr "x = %f, y = %f, s = %f, pw = %f, ph = %f, ratio = %f\n"
	  x y s !pixel_width !pixel_height ratio ;
*)
	  
	let draw_glyph color = 
	  try
	    GlList.call (Hashtbl.find glyphCache (g.glyph, ratio, color))
	  with
	    Not_found ->
	      let beziers =  Fonts.outlines g.glyph in
	      let lines, normals = add_normals true ratio beziers in
	      let l = GlList.create `compile_and_execute in
	      fill_bezier color lines normals;
	      GlList.ends ();

	      Hashtbl.add glyphCache (g.glyph, ratio, color) l

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
(*
	let x = (floor (x /. !pixel_width +. 1.0 /. 3.0) -. 0.5 /. 3.0) *. !pixel_width in
	let y = (floor (y /. !pixel_height +. 1.0) -. 0.5) *. !pixel_height in
*)
	GlMat.push ();
	GlMat.translate3 (x,y,0.0);
	GlMat.scale3 (s, s, s);
	draw_glyph (r,g,b);
	GlMat.pop ();

    | Path(param, beziers) ->
      let beziers = List.map Array.to_list beziers in
      let lines, normals = add_normals param.close !pixel_width beziers in

      (match param.fillColor with
	None -> ()
      | Some RGB{red = r; green=g; blue=b;} ->
	let color = (r,g,b) in
	fill_bezier color lines normals);

      (match param.strokingColor with
	None -> ()
      | Some RGB{red = r; green=g; blue=b;} ->
	let lw = param.lineWidth /. 2.0 in
	let graisse_x' = lw -. (!pixel_width *. (flou_x -. graisse)) 
	  and graisse_y' = lw -. (!pixel_width *. (flou_y -. graisse)) in
	let graisse_x =  lw +. (!pixel_width *. (3.0 *. flou_x +. graisse))
	  and graisse_y = lw +. (!pixel_width *. (3.0 *. flou_y +. graisse)) in

	let color = (r,g,b) in
	let lines, normals = 
	  if param.close then
	    List.map (function (x::_ as l) -> l @ [x] | [] -> []) lines,
	    List.map (function (x::_ as l) -> l @ [x] | [] -> []) normals
	  else
	    lines, normals
	in
	List.iter2 (fun l n ->
	  GlDraw.begins `quad_strip; 
	  GlDraw.color color; 
	  List.iter2
	    (fun (x,y,_) ((vx, vy),_) ->
	      let norm = sqrt (vx*.vx +. vy*.vy) in
	      let nx = vx /. norm *. graisse_x' in
	      let ny = vy /. norm *. graisse_y' in
	      GlDraw.vertex2 (x +. nx, y +. ny);
	      GlDraw.vertex2 (x -. nx, y -. ny)
	    ) l n;
	  GlDraw.ends ();

  	  GlFBO.use_shader ();

	  GlDraw.begins `quad_strip;
	  List.iter2
	    (fun (x,y,_) ((vx, vy),_) ->
	      let norm = sqrt (vx*.vx +. vy*.vy) in
	      let nx' = vx /. norm *. graisse_x' in
	      let ny' = vy /. norm *. graisse_y' in
	      let nx = vx /. norm *. graisse_x in
	      let ny = vy /. norm *. graisse_y in
	      GlDraw.color color;
	      GlDraw.vertex2 (x +. nx', y +. ny');
	      GlDraw.color ~alpha:0.0 color;
	      GlDraw.vertex2 (x +. nx, y +. ny);
	    ) l n;
	  GlDraw.ends ();

	  GlDraw.begins `quad_strip;
	  List.iter2
	    (fun (x,y,_) ((vx, vy),_) ->
	      let norm = sqrt (vx*.vx +. vy*.vy) in
	      let nx' = vx /. norm *. graisse_x' in
	      let ny' = vy /. norm *. graisse_y' in
	      let nx = vx /. norm *. graisse_x in
	      let ny = vy /. norm *. graisse_y in
	      GlDraw.color color;
	      GlDraw.vertex2 (x -. nx', y -. ny');
	      GlDraw.color ~alpha:0.0 color;
	      GlDraw.vertex2 (x -. nx, y -. ny);
	    ) l n;
	  GlDraw.ends ();
	  GlFBO.no_shader ();

	) lines normals);

    | Link(link) -> (
      List.iter fn link.link_contents
    )

    | States (s) -> assert false
	
    | Image i -> 
      Gl.enable `texture_2d;
#ifdef CAMLIMAGES
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
#endif
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
      in

      Gl.enable `blend;
      GlFBO.merge_blend ();
      GlFunc.depth_mask false;
      List.iter fn (drawing_sort !pages.(page).(state).pageContents);

      
      GlFunc.depth_mask true;
      Gl.disable `blend;
      Hashtbl.iter (fun name f ->
	try 
	  f () ;
	with e -> Printf.fprintf stderr "other: exception %s\n" name; flush stderr; ) other_items;

  
    in


    let do_draw () =
      match !mode with
	Single ->
	  draw_page !cur_page !cur_state
      | Double -> () (*
	let page = (!cur_page / 2) * 2 in
	let pw,ph = !pages.(!cur_page).(!cur_state).pageFormat in
	if page - 1 >= 0 && page - 1 < !num_pages then draw_page (page - 1) else draw_blank !cur_page;
	GlMat.push ();
	GlMat.translate3 (pw +. 1.0, 0.0, 0.0);
	if page >= 0 && page < !num_pages then draw_page page else draw_blank !cur_page;
	GlMat.pop ();*)

    in

   let draw_saa do_draw set_proj saa =
     match subpixel saa with 
	None -> 
	  set_proj 0.0 0.0;
	  do_draw ();
      | Some (xr,yr, xb,yb) ->
	set_proj xb yb;
	GlFunc.color_mask ~red:false ~green:false ~blue:true ~alpha:true ();
	do_draw ();
	set_proj xr yr;
	GlFunc.color_mask ~red:true ~green:false ~blue:false ~alpha:true ();
	GlClear.clear [`depth];
	do_draw ();
	set_proj 0.0 0.0;
	GlFunc.color_mask ~red:false ~green:true ~blue:false ~alpha:true ();
	GlClear.clear [`depth];
	do_draw ();
	GlFunc.color_mask ~red:true ~green:true ~blue:true ~alpha:true ();
   in 

    let draw_fbo fbo w h pw ph page state saa =
      GlDraw.viewport 0 0 w h;
      GlClear.clear [`color;`depth];
      GlMat.load_identity ();
      let set_proj xb yb = 
	GlMat.mode `projection;
	GlMat.load_identity ();
	GlMat.translate3 (xb /. float w , yb /. float h , 0.0);
	GlMat.ortho (0., pw) (0., ph) (-1., 1.);
	GlMat.mode `modelview;
      in
      draw_saa (fun () -> draw_page page state) set_proj saa;
    in

  let draw_off_screen samples page state =
    let pw,ph = !pages.(page).(state).pageFormat in
    let w, h = int_of_float (pw /. !pixel_width),  int_of_float (ph /. !pixel_width) in
    let fbo = GlFBO.create_fbo_texture (samples*w) (samples*h) true in
    GlFBO.bind_fbo fbo;
    draw_fbo fbo (samples*w) (samples*h) (float samples*.pw) (float samples*.ph) page state No_SAA;
    GlFBO.unbind_fbo fbo;
    fbo
  in
 
  let get_pix samples w h saa page state =
    let pw,ph = !pages.(page).(state).pageFormat in
    let w, h = match w, h with
	None, None -> 400, int_of_float (ph /. pw *. float 400)
      | Some w, None -> w, int_of_float (ph /. pw *. float w)
      | None, Some h -> int_of_float (pw /. ph *. float h), h
      | Some w, Some h -> w, h
    in
    pixel_width := pw /. float (samples * w);
    pixel_height := ph /. float (samples * h);
    let fbo = GlFBO.create_fbo_texture (samples*w) (samples*h) false in
    GlFBO.bind_fbo fbo;
    draw_fbo fbo (samples*w) (samples*h) (float samples*.pw) (float samples*.ph) page state saa;
    Gl.finish ();
(*
    let raw = Raw.create `ubyte ~len:(4*samples*w*samples*h) in
    GlFBO.read_fbo fbo raw;
*)
    let raw = GlPix.to_raw (GlPix.read ~x:0 ~y:0 ~width:(samples*w) ~height:(samples*h)
	~format:`rgba ~kind:`ubyte)
    in
    GlFBO.unbind_fbo fbo;
    (raw,w,h)
  in

  let get_pixes samples w h saa =
    Array.mapi (fun i states ->
      Array.mapi (fun j _ ->
	get_pix samples w h saa i j) states) !pages
  in

   let draw_texture fbo page state =
     let pw,ph = !pages.(page).(state).pageFormat in
     Gl.enable `texture_2d;
     GlFBO.bind_texture fbo;
     GlDraw.color (1.0, 1.0, 1.0);
     GlDraw.begins `quads;
     GlTex.coord2 (0., 0.);
     GlDraw.vertex2 (0., 0.);
     GlTex.coord2 (1., 0.);
     GlDraw.vertex2 (pw, 0.);
     GlTex.coord2 (1., 1.);
     GlDraw.vertex2 (pw, ph);
     GlTex.coord2 (0., 1.);
     GlDraw.vertex2 (0., ph);
     GlDraw.ends ();
     Gl.disable `texture_2d;
   in
  
  let draw_gl_scene () =
    let time = Sys.time () in
    cur_time := Unix.gettimeofday ();
    saved_rectangle := None;
    if !to_revert then revert ();
    GlFunc.color_mask ~red:true ~green:true ~blue:true ~alpha:true ();
    GlClear.color ~alpha:0.0 (0.0, 0.0, 0.0);
    GlClear.depth 1.0;
    GlClear.clear [`color;`depth];
    GlMat.load_identity ();
    draw_saa do_draw set_proj !prefs.subpixel_anti_aliasing;

    let delta = Sys.time () -. time in
    fps := !fps +. delta;
    incr cfps;
    if !cfps = 50 then (
      Printf.fprintf stderr "fps: %f\n" (float !cfps /. !fps);
      flush stderr;
      cfps := 0; fps := 0.0
    )
  in

  let rotate_page (i,j) (i',j') =
(*    Gc.set {(Gc.get ()) with Gc.verbose = 255 };*)
    match !prefs.rotation, i <> i' with
      (None, _) | (Some _, false) -> ()
    | Some duration, _ -> 
      let dir = i < i' in
      saved_rectangle := None;
      if !to_revert then revert ();
 
      let fbo1 = draw_off_screen 1 i j in
      let fbo2 = draw_off_screen 1 i' j' in
 
      let nb = ref 0 in
      let time = Unix.gettimeofday () in
      let angle = ref (if dir then 0.0 else 90.0) in
      let axe = (0.0,-1.0,0.0) in
      while !angle <= 90.0 && !angle >= 0.0 do
	GlClear.clear [`color;`depth];
	draw_saa (fun () ->
	GlMat.load_identity ();
	GlMat.rotate3 !angle axe;
	draw_texture fbo1 i j;
	if !angle > 15.0 then 
	  begin
	    GlMat.load_identity ();      
	    draw_texture fbo2 i' j';
	  end
	) set_proj !prefs.subpixel_anti_aliasing;
	Glut.swapBuffers ();
	incr nb;
	let delta = Unix.gettimeofday () -. time in
	angle := if dir then delta /. duration *. 90.0 else (1.0 -. delta /. duration) *. 90.0;

      done;
      
      Printf.printf "rotation: %f fps.\n" (float !nb /. duration);
      flush stdout
  in

    

  let redraw () =
    reshape_cb ~w:(Glut.get Glut.WINDOW_WIDTH)  ~h:(Glut.get Glut.WINDOW_HEIGHT);
    Glut.postRedisplay ()
  in


  let draw_show x y w () =
    let w = float w in
    let pw,ph = !pages.(!cur_page).(!cur_state).pageFormat in
    let x = float x /. w *. pw and y = ph -. float y /. w *. pw in
    let alpha = 0.0 in
    let (x1,y1,z1) as v1 = (x +. 2.0 *. pw, y -. 2.0 *. ph, 0.5 *. min pw ph) in
    let (x0,y0,z0) as v2 = (x +. alpha *. 2.0 *. pw, y -. alpha *. 2.0 *. ph, alpha *. 0.5 *. min pw ph) in
    Printf.fprintf stderr "Drawing: (%f,%f,%f) (%f,%f,%f)\n" x0 y0 z0 x1 y1 z1;
    flush stderr;
    let axe = Vec3.sub v2 v1 in
    let (ax,ay,az) as r1 = Vec3.normalize (y0,(-.x0),0.0) in
    let (bx,by,bz) as r2 = Vec3.normalize (Vec3.vecp axe r1) in
    let prec = 32 in
    let r = 1.0 in
    GlDraw.color (0.8, 0.3, 0.3);
    GlDraw.begins `quad_strip;
    for i = 0 to prec do
      let a = float(i) *. 2.0 *. Vec3.pi /. float(prec) in
      let c = r *. cos(a) and s = r *. sin(a) in
      GlDraw.vertex3 (Vec3.add v1 (Vec3.add (Vec3.mul c r1) (Vec3.mul s r2)));
      GlDraw.vertex3 (Vec3.add v2 (Vec3.add (Vec3.mul c r1) (Vec3.mul s r2)));
    done;
    GlDraw.ends ()
  in

  let show (cgi:Netcgi.cgi_activation) _ = 
    try
      let out = cgi # output # output_string in
      let x = int_of_string ((cgi # argument "show_x") # value) in
      let y = int_of_string ((cgi # argument "show_y") # value) in
      let w = int_of_string ((cgi # argument "show_w") # value) in
      Printf.fprintf stderr "show %d %d %d\n" x y w;
      flush stderr;
      Hashtbl.replace  other_items "show" (draw_show x y w);
      redraw()
    with
      Not_found | Failure _ -> 
	Printf.fprintf stderr "Bad show request"; 
	flush stderr
  in

  let show_end _ _ =
    Hashtbl.remove  other_items "show";
    redraw ();
  in

  GLNet.cbs := ("show_x", show)::("show_end", show_end)::!cbs;

  let dest = ref 0 in

  let incr_page () =
    Hashtbl.clear other_items;
    let i, j = !cur_page, !cur_state in
    if !cur_page + 1 < !num_pages then (
      start_page_time := Unix.gettimeofday ();
      incr cur_page;
      cur_state := 0;
      num_states := Array.length !pages.(!cur_page);
      rotate_page (i,j) (!cur_page, !cur_state);
      redraw ())
  in

  let decr_page reset_state =
    Hashtbl.clear other_items;
    let i, j = !cur_page, !cur_state in
    if !cur_page > 0 then (
      start_page_time := Unix.gettimeofday ();
      decr cur_page;
      num_states := Array.length !pages.(!cur_page);
      cur_state := if reset_state then 0 else !num_states - 1;
      rotate_page (i,j) (!cur_page, !cur_state);
      redraw ())
  in

  let incr_state () =
    let i, j = !cur_page, !cur_state in
    if !cur_state + 1 >= !num_states then incr_page () else (
      incr cur_state;
      rotate_page (i,j) (i, !cur_state);
      redraw())
  in

  let decr_state () =
    let i, j = !cur_page, !cur_state in
    if !cur_state <= 0 then decr_page false else (
      decr cur_state;
      rotate_page (i,j) (i, !cur_state);
      redraw())
  in
      
  let keyboard_cb ~key ~x ~y =
    if key >= 48 && key < 58 then
      dest := !dest * 10 + (key - 48)
    else begin
      (match key with
      | 27 | 120 | 113 (* ESC *) -> raise Exit
      | 110 -> incr_page ()
      | 32 -> incr_state ()
      | 112 -> decr_page true
      | 8 -> decr_state ()
      | 103 -> cur_page := min (max 0 !dest) (!num_pages - 1); cur_state := 0; redraw ();
      | 43 -> 
	if Glut.getModifiers () = Glut.active_shift then (
	  Hashtbl.clear glyphCache;
	  prefs := { !prefs with graisse = !prefs.graisse +. 0.05 };
	  Printf.printf "Graisse : %f\n" !prefs.graisse; flush stdout;
	  redraw ())
	else if Glut.getModifiers () = Glut.active_ctrl then (
	  Hashtbl.clear glyphCache;
	  prefs := { !prefs with tesselation_factor = !prefs.tesselation_factor *. 2.0 };
	  Printf.printf "Tesselation factor : %f\n" !prefs.tesselation_factor; flush stdout;
	  redraw ())
	else
	  zoom := !zoom /. 1.1; redraw ();
      | 45 ->
	if Glut.getModifiers () = Glut.active_shift then (
	  Hashtbl.clear glyphCache;
	  prefs := { !prefs with graisse = !prefs.graisse -. 0.05 };
	  Printf.printf "Graisse : %f\n" !prefs.graisse; flush stdout;
	  redraw ())
	else if Glut.getModifiers () = Glut.active_ctrl then (
	  Hashtbl.clear glyphCache;
	  prefs := { !prefs with tesselation_factor = !prefs.tesselation_factor /. 2.0 };
	  Printf.printf "Tesselation factor : %f\n" !prefs.tesselation_factor; flush stdout;
	  redraw ())
	else
	  zoom := !zoom *. 1.1; redraw ();
      | 119 ->
	init_zoom:=true;prefs:={ !prefs with init_zoom = FitWidth }; redraw ()
      | 104 ->
	init_zoom:=true;prefs:={ !prefs with init_zoom = FitHeight }; redraw ()
      | n -> Printf.fprintf stderr "Unbound key: %d (%s)\n" n (Char.escaped (Char.chr n)); flush stderr);
      dest := 0;
    end
  in

  let special_cb ~key ~x ~y =
    match key with
    | Glut.KEY_DOWN -> dy := !dy -. 5.; redraw ();
    | Glut.KEY_UP -> dy := !dy +. 5.; redraw ();
    | Glut.KEY_LEFT -> dx := !dx -. 5.; redraw ();
    | Glut.KEY_RIGHT -> dx := !dx +. 5.; redraw ();
    | Glut.KEY_PAGE_DOWN -> incr_page ()
    | Glut.KEY_PAGE_UP -> decr_page true
    | Glut.KEY_HOME -> init_zoom:=true; redraw ()
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
  let next_links = ref [] in

  let show_links () =
    let l = !next_links in
    if l <> !previous_links then (
      previous_links := l;
      (match !saved_rectangle with
	None -> ()
      | Some (r,r') -> Printf.printf "redraw\n"; flush stdout;       GlClear.clear [`color;`depth]; GlPix.draw r; GlPix.draw r');
      (if l = [] then Glut.setCursor Glut.CURSOR_INHERIT
       else if !saved_rectangle = None then
	 (Printf.printf "saving\n"; flush stdout;
	 saved_rectangle := 
	   Some (GlPix.read ~x:0 ~y:0 
		   ~width:(Glut.get Glut.WINDOW_WIDTH)  ~height:(Glut.get Glut.WINDOW_HEIGHT) 
		   ~format:`rgba ~kind:`ubyte
		,GlPix.read ~x:0 ~y:0 
		   ~width:(Glut.get Glut.WINDOW_WIDTH)  ~height:(Glut.get Glut.WINDOW_HEIGHT) 
		   ~format:`depth_component ~kind:`float)));

      List.iter (fun l ->
	let color = 
	  if is_edit l.uri then (
	    Glut.setCursor Glut.CURSOR_TEXT;
	      (1.0,0.0,0.0)
	    )
	  else (
	    Glut.setCursor Glut.CURSOR_INFO;
	    if l.uri <> "" then (0.0,0.0,1.0) else (0.0,1.0,0.0)
	  )
	in
	Printf.printf "link: x0 = %f, y0 = %f, x1 = %f, y1 = %f\n" l.link_x0 l.link_y0 l.link_x1 l.link_y1;
	flush stdout;
	overlay_rect color (l.link_x0,l.link_y0,l.link_x1,l.link_y1);
      ) l;
      Glut.swapBuffers ();
    )
  in

  let passive_motion_cb ~x ~y =
    let l = find_link x y in
    flush stderr;
    next_links := l
  in
  
  let goto_link l0 c0 = 
    let res = 
      Array.fold_left(fun (acc, (i,j)) linkss ->
	Array.fold_left(fun (acc, (i,j)) links ->
	  let acc =
	    List.fold_left (fun (bl, bc, res as acc) link ->
	      if Str.string_match (Str.regexp "^edit:[^@]*@\\([0-9]+\\)@\\([0-9]+\\)")
		link.uri 0 then
		let l = int_of_string (Str.matched_group 1 link.uri) in
		let c = int_of_string (Str.matched_group 2 link.uri) in
		if (l0 > l or (l = l0 && c0 >= c)) && 
		  (l0 - l < bl or (l0 - l = bl && c0 - c < bc)) then
		  l0 - l, c0 - c, Some(link, i-1, j)
		else
		  acc
	      else acc) acc links
	  in
	  (acc, (i, j + 1))) (acc, (i+1,0)) linkss) ((max_int, 0, None), (0, 0)) !links
    in

    match fst res with
      (_,_,None) -> Printf.fprintf stderr "Edit position not found: line <= %d, col <= %d.\n"
	l0 c0; flush stderr
    | (_,_,Some(l,i,j)) -> 
      cur_page:= i;
      cur_state := j;
      draw_gl_scene ();
      overlay_rect (1.0,0.0,0.0) (l.link_x0,l.link_y0,l.link_x1,l.link_y1);
      Glut.swapBuffers ()
  in

  let handle_one = 
    (match !prefs.server_port with
      Some port -> GLNet.handle_one port
    | None -> fun () -> ())
  in

  let rec idle_cb ~value:() =
    Glut.timerFunc ~ms:30 ~cb:idle_cb ~value:();
    if !do_animation then (draw_gl_scene (); Glut.swapBuffers ());
    show_links ();
    handle_one ();
    try
      let i,_,_ = Unix.select [Unix.stdin] [] [] 0.0 in
      match i with
	[] -> ()
      | i ->
	let cmd = input_line stdin in
	if cmd <> "" then begin
	  try
	    match cmd.[0] with
	      'r' -> to_revert := true; Glut.postRedisplay ()
	    | 'e' -> (
	      match Str.split (Str.regexp_string " ") cmd with
		[_;l;c] ->
		  goto_link (int_of_string l) (int_of_string c)
	      | _ -> raise Exit)
	    | _ -> raise Exit
	  with
	    Exit ->
	      Printf.fprintf stderr "Illegal cmd: %s\n" cmd; flush stderr
	end
    with 
      Unix.Unix_error(nb, _, _) as e -> 
      Printf.fprintf stderr "Error in select: %s (%s)\n"
	(Printexc.to_string e)
	(Unix.error_message nb);
      flush stderr;

  in

  let display_cb () = 
    draw_gl_scene ();
    Glut.swapBuffers ();
    idle_cb ();
  in
    
  let page_counter = Random.self_init (); ref (Random.int 1000000000)
  in

  let make_content out id cmd fmt width height format =
    Printf.fprintf stderr "Make content\n"; flush stderr;
    if id = !page_counter then
      begin match cmd with
	Next_page -> incr_page ()
      | Prev_page -> decr_page true
      | Next_state -> incr_state ()
      | Prev_state -> decr_state ()
      | Goto(page, state) -> 
	cur_page := min (max 0 page) (!num_pages - 1);
	num_states := Array.length !pages.(!cur_page);
	cur_state := min (max 0 state) (!num_states - 1);
	redraw ()
      | Refresh -> ()
      end;
    let args =
      (match format with None -> "" | Some n -> Printf.sprintf "&format=%s" n)
    in
    let page, state = !cur_page, !cur_state in

    let format = match format with
	None -> "png"
      | Some f -> f
    in 
 
    let imgname,w,h = try
      Hashtbl.find image_cache (page,state,width,height,format)
    with
      Not_found ->
	let (raw,w,h) = get_pix 1 width height No_SAA page state in
	let image = Rgba32.create w h in 
	for j=0 to h-1 do	  
	  for i=0 to w-1 do
	    let r = Raw.get raw ((j * w + i) * 4 + 0) in
	    let g = Raw.get raw ((j * w + i) * 4 + 1) in
	    let b = Raw.get raw ((j * w + i) * 4 + 2) in
	    let a = Raw.get raw ((j * w + i) * 4 + 3) in
	    let c = { Color.color = { Color.r = r; Color.g = g; Color.b = b };
		      Color.alpha = a } in
	(*	    Printf.printf "%d %d %d %d\n" r g b a;*)
	    Rgba32.set image i (h-1-j) c
	  done
	done;
	let fname = Filename.temp_file ~temp_dir:tmp_image_dir "page" ("."^format) in
	
	Printf.fprintf stderr "Wrinting %s\n" fname;
	Images.save fname None [] (Images.Rgba32 image);
	Hashtbl.add image_cache (page,state,width,height,format) (fname,w,h);
	fname,w,h
    in

    incr page_counter;
    let id = !page_counter in

    out "
<script language=\"JavaScript\">
function httpSend(theUrl) {
    var xmlHttp = null;
    xmlHttp = new XMLHttpRequest();
    xmlHttp.open( \"GET\", theUrl, false );
    xmlHttp.send( null );}

function show_end() {
    httpSend(\"?show_end=\");}

function init() {
    next = document.getElementById(\"next\");
    if (next) next.href = next.href + \"&width=\" + (document.getElementById(\"image\").offsetWidth - 30).toString();
    prev = document.getElementById(\"prev\");
    if (prev) prev.href = prev.href + \"&width=\" + (document.getElementById(\"image\").offsetWidth - 30).toString();
}
window.onload=init;

function show(event){
	pos_x = event.offsetX?(event.offsetX):event.pageX-document.getElementById(\"slide\").offsetLeft;
	pos_y = event.offsetY?(event.offsetY):event.pageY-document.getElementById(\"slide\").offsetTop;
	pos_w  = document.getElementById(\"slide\").offsetWidth;
        httpSend(\"?show_x=\"+pos_x.toString()+\"&show_y=\"+pos_y.toString()+\"&show_w=\"+pos_w.toString());}
</script>";

    let button s = Printf.sprintf "<div class=\"button\">%s</div>" s in
    out ("<div id=\"leftbar\">");
    out (button (if !cur_page = 0 then "<a>PREV</a>" else
	       Printf.sprintf "<a id=\"prev\" href=\"id%d?prev=%s\">PREV</a>" id args));
    out ("</div>");

    out ("<div id=\"rightbar\">");
    out (button (if !cur_page >= !num_pages - 1 then "<a>NEXT</a>" else
	       Printf.sprintf "<a id=\"next\" href=\"id%d?next=%s\">NEXT</a>" id args));
    out (button "<a onClick=\"show_end()\">NO SHOW</a>");

    out ("</div>");
(*
     out "<span style=\"position:fixed;top:60px;left:20px;text-align:center;background-color:#FBFBFF;border:1px solid #000000;color:#666666;z-index: 1\"><a href=\"id%d?zoom_in\">ZOOM IN</a></span>";

     out "<span style=\"position:fixed;top:100px;left:20px;text-align:center;background-color:#FBFBFF;border:1px solid #000000;color:#666666;z-index: 1\"><a href=\"id%d?zoom_in\">ZOOM OUT</a></span>";

     out "<span style=\"position:fixed;top:140px;left:20px;text-align:center;background-color:#FBFBFF;border:1px solid #000000;color:#666666;z-index: 1\"><a href=\"id%d?zoom_in\">ZOOM END</a></span>";

     out "<span style=\"position:fixed;top:60px;right:20px;text-align:center;background-color:#FBFBFF;border:1px solid #000000;color:#666666;z-index: 1\"><a href=\"id%d?zoom_in\">SHOW</a></span>";

     out "<span style=\"position:fixed;top:100px;right:20px;text-align:center;background-color:#FBFBFF;border:1px solid #000000;color:#666666;z-index: 1\" onClick=\"show_end()q\">SHOW END</span>";
*)
     out ("<div id=\"image\">");
     out (Printf.sprintf "<IMG id=\"slide\" src=\"?file=%s\" width=%d height=%d align=\"center\"onClick=\"show(event)\">" imgname w h);
     out ("</div>");

 in

  GLNet.make_content := make_content;

  let mouse_cb ~button ~state ~x ~y =
    match button, state with

    | Glut.OTHER_BUTTON(3), Glut.UP -> zoom := !zoom /. 1.1; redraw ();
    | Glut.OTHER_BUTTON(4), Glut.UP -> zoom := !zoom *. 1.1; redraw ();

    | Glut.LEFT_BUTTON, Glut.DOWN ->
      motion_ref := Some (x, y);

    | Glut.LEFT_BUTTON, Glut.UP ->
      let clic = match !motion_ref with
	  None -> false
	| Some(x',y') ->
	  let dx = x - x' and dy = y - y' in
	  (dx * dx + dy * dy < 9)
      in
      motion_ref := None;
      if clic then List.iter 
	  (fun l ->
	    Printf.fprintf stderr 
	      "link cliqued: uri = %s, dest_page = %d, dest_x = %f, dest_y = %f\n"
	      l.uri l.dest_page l.dest_x l.dest_y;
	    flush stderr;
	    if l.uri = "" then
	      begin
		cur_page := l.dest_page;
		cur_state := 0;
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

  let main () =
    let win0 = match !win with
      None ->
	Sys.catch_break true;
	Printf.fprintf stderr "Start patoline GL.\n"; flush stderr;    
	ignore (Glut.init Sys.argv);
	Glut.initDisplayString "rgba>=8 alpha>=16 depth>=16 double";
	Printf.fprintf stderr "Glut init finished, creating window\n"; flush stderr;
	let w =  Glut.createWindow "Patoline OpenGL Driver" in
	win := Some w;
	Printf.fprintf stderr "Window created, number of samples: %d\n" 
	  (Glut.get Glut.WINDOW_NUM_SAMPLES);
	flush stderr;
	init_gl ();
	w
    | Some w -> w
    in
    match !prefs.batch_cmd with
      None ->
	Glut.displayFunc display_cb;
	Glut.keyboardFunc keyboard_cb;
	Glut.specialFunc special_cb;
	Glut.reshapeFunc reshape_cb;
	Glut.mouseFunc mouse_cb;
	Glut.timerFunc ~ms:30 ~cb:idle_cb ~value:();
	Glut.motionFunc motion_cb;
	Glut.passiveMotionFunc passive_motion_cb;
	
	Sys.set_signal Sys.sighup
	  (Sys.Signal_handle
	     (fun s ->  to_revert := true; Glut.postRedisplay ()));
	Printf.fprintf stderr "GL setup finished, starting loop\n";
	flush stderr;
	(try Glut.mainLoop ()
	with e -> 
	  Hashtbl.iter (fun _ l -> GlList.delete l) glyphCache;
	  Gl.flush ();
	  Glut.destroyWindow win0; if e <> Exit then raise e)
    | Some f -> 
      f get_pixes
  in

  main()

let output = output_from_prime output'