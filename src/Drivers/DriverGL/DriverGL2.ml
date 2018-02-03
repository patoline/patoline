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
open Fonts.FTypes
open Raw
open RawContent
open Color
open Driver

let cur_page = ref 0

let initGL () =
(*    GlDraw.shade_model `smooth;*)
    GlClear.color (0.5, 0.5, 0.5);
    GlClear.depth 1.0;
    GlClear.clear [`color; `depth];
    Gl.enable `depth_test;
    Gl.disable `polygon_smooth;
(*    GlMisc.hint `polygon_smooth `nicest;*)
    Gl.disable `line_smooth;
    Gl.enable `blend;
    GlFunc.blend_func `src_alpha `one_minus_src_alpha;
    GlFunc.depth_func `lequal

let filename x=""

let zoom = ref 1.0
let dx = ref 0.0
let dy = ref 0.0
let subpixel = ref (Some(1.0 /. 3.0, 0.0, -. 1.0 /. 3.0, 0.0))

let glyphCache = Hashtbl.create 1001
let imageCache = Hashtbl.create 1001

let rec last = function
[] -> assert false
  | [x] -> x
  | _::l -> last l
    
type page_mode = Single | Double

let output ?(structure:structure={name="";raw_name=[]; metadata = []; tags=[];
                                  page= -1;struct_x=0.;struct_y=0.;children=[||]})
    pages fileName=

  let killGLWindow () =
    () (* do nothing *)
  in


  let pages = ref pages in
  let structure = ref structure in
  let num_pages = ref (Array.length !pages) in
  let links = ref [||] in

  let read_links () = 
    links := Array.mapi
      (fun i page ->
        let l = ref [] in
        List.iter
          (function  
            Link(l') -> 
              l := l'::!l
          | _ -> ())
          page.contents;
        !l)
      !pages;
  in

  let _ = read_links () in

  let pixel_width = ref 0.0 in
  let pixel_height = ref 0.0 in

(* Handle window reshape events *)
  let reshape_cb ~width:w ~height:h =
    let ratio = (float_of_int w) /. (float_of_int h) in
    let page = !cur_page in
    let pw,ph = !pages.(page).size in
    let cx = pw /. 2.0 +. !dx in
    let cy = ph /. 2.0 +. !dy in
    let rx = (ph *. ratio) /. 2.0 *. !zoom in
    let ry = ph /. 2.0 *. !zoom in
    pixel_width := rx *. 2.0 /. (float_of_int w);
    pixel_height := ry *. 2.0 /. (float_of_int h);

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
  (*  
  let inverse_coord x y =
    let w = float (Glut.get Glut.WINDOW_WIDTH)
    and h = float (Glut.get Glut.WINDOW_HEIGHT) in
    let x = float x and y = h -. float y in
    let ratio = w /. h in
    let page = !cur_page in
    let pw,ph = !pages.(page).size in
    let cx = pw /. 2.0 +. !dx in
    let cy = ph /. 2.0 +. !dy in
    let dx = (ph *. ratio) *. !zoom in
    let dy = ph *. !zoom in
    let x = (x -. w /. 2.0) /. w *. dx +. cx in
    let y = (y -. h /. 2.0) /. h *. dy +. cy in
    (x, y)
  in
  *)

  let find_link x y = []
(*
    let x,y = inverse_coord x y in
    List.filter (fun l ->
      l.link_x0 <= x && x <= l.link_x1 &&
      l.link_y0 <= y && y <= l.link_y1) !links.(!cur_page)
*)
  in

  let is_edit uri =
    String.length(uri) >= 5 && String.sub uri 0 5 = "edit:"
  in

  let fps = ref 0.0 and cfps = ref 0 in

  let saved_rectangle = ref None in
  let to_revert = ref true in


  let old_menu = ref [] in
  let menu_item = Hashtbl.create 13 in

(*
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
    if structure.children <> [||] then begin
      let c = ref 0 in
      List.iter (fun menu -> Glut.destroyMenu ~menu) !old_menu;
      let menu = Glut.createMenu menu_cb in
      old_menu := [menu];
      Glut.setMenu menu;
      let rec fn menu a i s = 
        Glut.addMenuEntry s.name !c;
        Hashtbl.add menu_item !c (a, i);
        incr c;
        if s.children <> [||] then
          begin
            let menu' = Glut.createMenu menu_cb in
            old_menu := menu' :: !old_menu;
            Glut.setMenu menu';
            Array.iteri (fn menu' s.children) s.children;
            Glut.setMenu menu;
            Glut.addSubMenu "  ==>" menu';
          end
      in
      Array.iteri (fn menu structure.children)  structure.children;
      Glut.attachMenu Glut.RIGHT_BUTTON
    end
  in
*)

  let revert () = 
    let ch = open_in fileName in
    to_revert := false;
    pages := input_value ch;
    structure := input_value ch;
    Printf.printf "Structure:\n";
    (*    create_menu !structure;*)
    close_in ch;
    num_pages := Array.length !pages;
    read_links ();
    (* Not clearing the caches slows down a lot after redraw *)
    Hashtbl.iter (fun _ t -> GlTex.delete_texture t) imageCache;
    Hashtbl.clear imageCache;
    Hashtbl.iter (fun _ l -> GlList.delete l) glyphCache;
    Hashtbl.clear glyphCache;
    Gc.compact ();
  in

  let flou_x = 1.0 /. 6.0 and flou_y = 1.0 /. 2.0 (* 1.0 /. 3.0 et 1.0 plus logique ?*) in
  let graisse = ref 0.0 (* entre -1 et 1 pour rester raisonnable *) in
 
  let tesselation_factor = 0.25 in

  let add_normals closed ratio beziers =
    if closed then
      let beziers = Bezier.oriente beziers in
      List.split (List.map (fun (bs, orientation) -> 
        let bs = List.flatten (List.map (Bezier.subdivise (tesselation_factor *. ratio)) bs) in
        (* FIXME: close the curve if it is not closed !!! *)
        let prev = ref (let xs, ys = last bs in (Bezier.derivee_end ys), -. (Bezier.derivee_end xs)) in
        List.split (List.map (fun (xs, ys) -> 
          let a, b = !prev in
          let xp = Bezier.derivee_start ys +. a in
          let yp =  -. (Bezier.derivee_start xs) +. b in
          let n = ratio /. sqrt (xp*.xp +. yp*.yp) in
          let n = if classify_float n <> FP_normal then 0.0 else n in
          let n = if orientation then n else -. n in
          prev :=  (Bezier.derivee_end ys), -. (Bezier.derivee_end xs);
          (xs.(0),ys.(0),0.0),(xp*.n, yp*.n,0.0)) bs)
      ) beziers) 
    else
      List.split (List.map (fun bs -> 
        let bs = List.flatten (List.map (Bezier.subdivise (tesselation_factor *. ratio)) bs) in
        let prev = ref None in
        let ln = 
          List.map (fun (xs, ys) -> 
            let xp, yp =
              match !prev with
                None -> 
                  Bezier.derivee_start ys, -. (Bezier.derivee_start xs)
              | Some(_,_,a,b) -> 
                Bezier.derivee_start ys +. a, -. (Bezier.derivee_start xs) +. b 
            in
            let n = ratio /. sqrt (xp*.xp +. yp*.yp) in
            prev :=  Some (xs, ys, (Bezier.derivee_end ys), -. (Bezier.derivee_end xs));
            (xs.(0),ys.(0),0.0),(xp*.n, yp*.n,0.0)) bs
        in
        let last = 
          match !prev with
            None -> assert false
          | Some(xs,ys,xp,yp) -> 
            let n = ratio /. sqrt (xp*.xp +. yp*.yp) in
            let s = Array.length xs in
            (xs.(s - 1),ys.(s - 1),0.0),(xp*.n, yp*.n,0.0)
        in
        List.split (ln @ [last])
    ) beziers) 
  in

  let mode = ref Single in

    let draw_blank page =
      let pw,ph = !pages.(page).size in
      GlDraw.color (1.0, 1.0, 1.0);
      GlDraw.begins `quads;
      GlDraw.vertex2 (0., 0.);
      GlDraw.vertex2 (pw, 0.);
      GlDraw.vertex2 (pw, ph);
      GlDraw.vertex2 (0., ph);
      GlDraw.ends ();
    in

    let draw_page page =
      let graisse = !graisse in
      let graisse_x = flou_x +. graisse and graisse_y = flou_y +. graisse in
      

      draw_blank page;

      List.iter (function
      Glyph g ->
        let x = g.glyph_x in
        let y = g.glyph_y  in
        let size = g.glyph_size in
        let s = size   /. 1000. in
        let ratio = !pixel_width /. s in
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

              GlDraw.color color;
              let lines = List.map2 (fun l n -> List.map2 (fun (x,y,_) (xn,yn,_) -> 
                (x +. xn *. graisse, y +. yn *. graisse , 0.0)) l n) lines normals in
              GluTess.tesselate (*~tolerance:(scale/.5.)*) lines;
                            
              List.iter2 (fun l n ->
                GlDraw.begins `quad_strip;
                List.iter2 (fun (x,y,_) (xn,yn,_) ->
                  GlDraw.color color;
                  GlDraw.vertex2 (x,y);
                  GlDraw.color ~alpha:0.0 color;
                  GlDraw.vertex2 (x+. graisse_x *. xn,y+. graisse_y *. yn)) l n;
                let (x,y,_) = List.hd l in
                let (xn,yn,_) = List.hd n in
                GlDraw.color color;
                GlDraw.vertex2 (x,y);
                GlDraw.color ~alpha:0.0 color;
                GlDraw.vertex2 (x+. graisse_x *. xn,y+. graisse_y *. yn);
              GlDraw.ends ();
              )        lines normals;

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
        GlDraw.color color;

        let lines = List.map2 (fun l n -> List.map2 (fun (x,y,_) (xn,yn,_) -> 
          (x +. xn *. graisse, y +. yn *. graisse, 0.0)) l n) lines normals in
        GluTess.tesselate (*~tolerance:(2e-3 *. !zoom)*) lines;

        List.iter2 (fun l n ->
          if param.strokingColor = None then begin
            GlDraw.begins `quad_strip;
            List.iter2 (fun (x,y,_) (xn,yn,_) ->
              GlDraw.color color;
              GlDraw.vertex2 (x,y);
              GlDraw.color ~alpha:0.0 color;
              GlDraw.vertex2 (x+.graisse_x*.xn,y+.graisse_y*.yn)) l n;
            let (x,y,_) = List.hd l in
            let (xn,yn,_) = List.hd n in
            GlDraw.color color;
            GlDraw.vertex2 (x,y);
            GlDraw.color ~alpha:0.0 color;
            GlDraw.vertex2 (x+.graisse_x*.xn,y+.graisse_y*.yn);
            GlDraw.ends ()
          end) lines normals;

      );
      (match param.strokingColor with
        None -> ()
      | Some RGB{red = r; green=g; blue=b;} ->
        let color = (r,g,b) in
        List.iter2 (fun l n ->
          GlDraw.begins `quad_strip; 
          GlDraw.color color; 
          let lw = param.lineWidth in
          List.iter2
            (fun (x,y,_) (vx, vy,_) ->
              let norm = sqrt (vx*.vx +. vy*.vy) in
              let nx = vx /. norm *. lw /. 2.0 in
              let ny = vy /. norm *. lw /. 2.0 in
              GlDraw.vertex2 (x +. nx, y +. ny);
              GlDraw.vertex2 (x -. nx, y -. ny)
            ) l n;
          GlDraw.ends ();
(*
          GlDraw.begins `quad_strip;
          let lw = param.lineWidth in
          List.iter2
            (fun (x,y,_) (vx, vy,_) ->
              let norm = sqrt (vx*.vx +. vy*.vy) in
              let nx = (vx /. norm *. lw) /. 2.0 in
              let ny = (vy /. norm *. lw) /. 2.0 in
              GlDraw.color color;
              GlDraw.vertex2 (x +. nx, y +. ny);
              GlDraw.color ~alpha:0.0 color;
              GlDraw.vertex2 (x +. nx +. vx, y +. ny +. vy);
            ) l n;
          GlDraw.ends ();
          GlDraw.begins `quad_strip;
          let lw = param.lineWidth in
          List.iter2
            (fun (x,y,_) (vx, vy,_) ->
              let norm = sqrt (vx*.vx +. vy*.vy) in
              let nx = (vx /. norm *. lw) /. 2.0 in
              let ny = (vy /. norm *. lw) /. 2.0 in
              GlDraw.color color;
              GlDraw.vertex2 (x -. nx, y -. ny);
              GlDraw.color ~alpha:0.0 color;
              GlDraw.vertex2 (x -. nx -. vx, y -. ny -. vy);
            ) l n;
          GlDraw.ends ();
*)
        ) lines normals);

    | Link(link) -> ()

    | Image i -> 
      Gl.enable `texture_2d;
      begin     
        try           
          GlTex.bind_texture `texture_2d (Hashtbl.find imageCache i)
        with Not_found ->
           let image = ReadImg.openfile i.image_file in
           let w = Image.(image.width) in
           let h = Image.(image.height) in
           let raw = Raw.create `ubyte ~len:(4*w*h) in
           for j=0 to h-1 do
             for i=0 to w-1 do
               Image.(read_rgba_pixel image i j (fun ~r ~g ~b ~a ->
                 let r,g,b,a =
                   if image.max_val <> 255 then
                     ( (r * 255 * 2 + 1) / (2 * image.max_val)
                     , (g * 255 * 2 + 1) / (2 * image.max_val)
                     , (b * 255 * 2 + 1) / (2 * image.max_val)
                     , (a * 255 * 2 + 1) / (2 * image.max_val) )
                   else r,g,b,a
                 in
                 Raw.set raw ((j * w + i) * 4 + 0) r;
                 Raw.set raw ((j * w + i) * 4 + 1) g;
                 Raw.set raw ((j * w + i) * 4 + 2) b;
                 Raw.set raw ((j * w + i) * 4 + 3) a))
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
    ) !pages.(page).contents
    in

    let do_draw () =
      match !mode with
        Single ->
          draw_page !cur_page
      | Double ->
        let page = (!cur_page / 2) * 2 in
        let pw,ph = !pages.(!cur_page).size in
        if page - 1 >= 0 && page - 1 < !num_pages then draw_page (page - 1) else draw_blank !cur_page;
        GlMat.push ();
        GlMat.translate3 (pw +. 1.0, 0.0, 0.0);
        if page >= 0 && page < !num_pages then draw_page page else draw_blank !cur_page;
        GlMat.pop ();

    in

  let draw_gl_scene () =
    let time = Sys.time () in
    saved_rectangle := None;
    if !to_revert then revert ();
    GlClear.clear [`color;`depth];
    GlMat.load_identity ();


    begin
      match !subpixel with 
        None -> do_draw ();
      | Some (xr,yr, xb,yb) ->
        GlFunc.color_mask ~red:false ~green:true ~blue:false ();
        do_draw ();
        GlMat.push ();
        GlMat.translate3 (!pixel_width *. xb , !pixel_width *. yb , 0.0);
        GlFunc.color_mask ~red:false ~green:false ~blue:true ();
        do_draw ();
        GlMat.pop ();
        GlMat.push ();
        GlMat.translate3 (!pixel_width *. xr , !pixel_width *. yr, 0.0);
        GlFunc.color_mask ~red:true ~green:false ~blue:false ();
        do_draw ();
        GlMat.pop ();
        GlFunc.color_mask ~red:true ~green:true ~blue:true ();
    end;

    let delta = Sys.time () -. time in
    fps := !fps +. delta;
    incr cfps;
    if !cfps = 50 then (
      Printf.fprintf stderr "fps: %f\n" (float !cfps /. !fps);
      flush stderr;
      cfps := 0; fps := 0.0
    )
  in

(* Ne marche pas 
  let rotate_page i =
    let time = Sys.time () in
    let angle = ref 0.0 in
    saved_rectangle := None;
    if !to_revert then revert ();

    while !angle < 1.57 do
      let delta = Sys.time () -. time in
      angle := delta /. 2.0 *. 1.57;
      Printf.printf "angle: %f\n" !angle;
      flush stdout;
      GlClear.clear [`color;`depth];
      GlMat.load_identity ();
      GlMat.rotate3 !angle (0.0,-1.0,0.0);
      Gl.disable `blend;
      draw_blank (!cur_page - i);
      GlMat.load_identity ();      
      draw_page (!cur_page);
      Glut.swapBuffers ();
      Gl.enable `blend;
    done;
  in
*)
    
(*
  let redraw () =
    reshape_cb ~w:(Glut.get Glut.WINDOW_WIDTH)  ~h:(Glut.get Glut.WINDOW_HEIGHT);
    Glut.postRedisplay ()
  in
*)
  let dest = ref 0 in
(*
  let keyboard_cb ~key ~x ~y =
    if key >= 48 && key < 58 then
      dest := !dest * 10 + (key - 48)
    else begin
      (match key with
      | 27 | 120 | 113 (* ESC *) -> raise Exit
      | 110 | 32 -> if !cur_page < !num_pages - 1 then (incr cur_page; redraw ());
      | 112 | 8 -> if !cur_page > 0 then (decr cur_page; redraw ());
      | 103 -> cur_page := min (max 0 !dest) (!num_pages - 1); redraw ();
      | 43 -> 
        if Glut.getModifiers () = Glut.active_shift then (
          Hashtbl.clear glyphCache;
          graisse := !graisse +. 0.05;
          Printf.printf "Graisse : %f\n" !graisse; flush stdout;
          redraw ())
        else
          zoom := !zoom /. 1.1; redraw ();
      | 45 ->
        if Glut.getModifiers () = Glut.active_shift then (
          Hashtbl.clear glyphCache;
          graisse := !graisse -. 0.05;
          Printf.printf "Graisse : %f\n" !graisse; flush stdout;
          redraw ())
        else
          zoom := !zoom *. 1.1; redraw ();
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
    | Glut.KEY_PAGE_DOWN -> if !cur_page < !num_pages - 1 then (incr cur_page; redraw ());
    | Glut.KEY_PAGE_UP -> if !cur_page > 0 then (decr cur_page; redraw ());
(*    | Glut.KEY_HOME -> 
    | Glut.KEY_END ->
*)
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

  let passive_motion_cb ~x ~y =
    let l = find_link x y in
    next_links := l
  in
  
  let show_links () =
    let l = !next_links in
    if l <> !previous_links then (
(*      draw_gl_scene ();*)
      previous_links := l;
      (match !saved_rectangle with
        None -> ()
      | Some r -> GlPix.draw r);
      (if l = [] then Glut.setCursor Glut.CURSOR_INHERIT
       else if !saved_rectangle = None then
         saved_rectangle := 
           Some (GlPix.read ~x:0 ~y:0 
                   ~width:(Glut.get Glut.WINDOW_WIDTH)  ~height:(Glut.get Glut.WINDOW_HEIGHT) 
                   ~format:`rgba ~kind:`ubyte));
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
        overlay_rect color (l.link_x0,l.link_y0,l.link_x1,l.link_y1);
      ) l;
      Glut.swapBuffers ();
    )
  in

  let goto_link l0 c0 = 
    let res = Array.fold_left(fun (acc, i) links ->
      let acc =
        List.fold_left (fun (bl, bc, res as acc) link ->
          if Str.string_match (Str.regexp "^edit:[^@]*@\\([0-9]+\\)@\\([0-9]+\\)")
            link.uri 0 then
            let l = int_of_string (Str.matched_group 1 link.uri) in
            let c = int_of_string (Str.matched_group 2 link.uri) in
            if (l0 > l or (l = l0 && c0 >= c)) && 
              (l0 - l < bl or (l0 - l = bl && c0 - c < bc)) then
              l0 - l, c0 - c, Some(link, i)
            else
              acc
          else acc) acc links
      in
      (acc, i + 1))
      ((max_int, 0, None), 0) !links
    in

    match fst res with
      (_,_,None) -> Printf.fprintf stderr "Edit position not found: line <= %d, col <= %d.\n"
        l0 c0; flush stderr
    | (_,_,Some(l,i)) -> 
      cur_page:= i;
      draw_gl_scene ();
      overlay_rect (1.0,0.0,0.0) (l.link_x0,l.link_y0,l.link_x1,l.link_y1);
      Glut.swapBuffers ()
  in

  let rec idle_cb ~value:() =
    Glut.timerFunc ~ms:250 ~cb:idle_cb ~value:();
    show_links ();
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
*)
  let display_cb area () = 
(*    idle_cb ();*)
    draw_gl_scene ();
    area#swap_buffers ()
  in
      
(*
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
*)
  let createGLWindow title width height bits fullscreen =
    let w = GWindow.window ~title:title () in
    w#connect#destroy ~callback:(fun () -> GMain.Main.quit (); exit 0);
    w#set_resize_mode `IMMEDIATE;
    let area = GlGtk.area [`DOUBLEBUFFER;`RGBA;`DEPTH_SIZE 16;`BUFFER_SIZE bits]
      ~width:width ~height:height~packing:w#add () in
    area#event#add [`KEY_PRESS];
    
    w#event#connect#key_press ~callback:
      begin fun ev ->
        let key = GdkEvent.Key.keyval ev in
        if key = GdkKeysyms._Escape then w#destroy ();
        true
      end;
    
(*
    GMain.Timeout.add ~ms:20 ~callback:
      begin fun () ->
        draw_gl_scene area ; true
      end;
*)  
    area#connect#display ~callback:(display_cb area);
    area#connect#reshape ~callback:reshape_cb;
    
    area#connect#realize ~callback:
      begin fun () ->
        initGL ();
        reshape_cb ~width ~height
      end;
    w#show ();
    
    w
  in

  let main () =
    let w = createGLWindow "Tutorial 5" 640 480 16 false in
    GMain.Main.main ()
  in

  let _ = Printexc.print main ()
  in

  main ()
