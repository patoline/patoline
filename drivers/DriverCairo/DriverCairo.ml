(****************************************************************************)
(*                    The Patoline Typesetting System                       *)
(*           The Patoline development team - Copyright 2012-2018            *)
(*                                                                          *)
(* Patoline is free software: you can redistribute it and/or modify it      *)
(* under the terms of the GNU General Public License as published by the    *)
(* Free Software Foundation, either version 3 of the License, or (at your   *)
(* option) any later version.                                               *)
(*                                                                          *)
(* Patoline is distributed in the hope that it will be useful, but WITHOUT  *)
(* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or    *)
(* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for *)
(* more details.                                                            *)
(*                                                                          *)
(* You should have received a copy of the GNU General Public License along  *)
(* with Patoline  If not, see <http://www.gnu.org/licenses/>.               *)
(****************************************************************************)

open Patoraw
open Patfonts
open Driver
open Color

let pixels_per_mm = ref 10.0

let driver_options = []
let filter_options argv = argv

let draw_page (width, height) surface contents =
  (* Initialize the drawing context. *)
  let ctx = Cairo.create surface in
  (* Draw the paper. *)
  Cairo.move_to ctx 0.0   0.0   ;
  Cairo.line_to ctx width 0.0   ;
  Cairo.line_to ctx width height;
  Cairo.line_to ctx 0.0   height;
  Cairo.Path.close ctx;
  Cairo.set_source_rgb ctx 1.0 1.0 1.0;
  Cairo.fill ctx;
  (* Function for drawing a path. *)
  let draw_path param path =
    let open RawContent in
    (* Set the line width. *)
    let line_width = !pixels_per_mm *. param.lineWidth in
    Cairo.set_line_width ctx line_width;
    (* Build the path by iterating over the pieces. *)
    let draw_piece piece =
      (* Move to the beginning of the piece. *)
      let (x0,y0) = piece.(0) in
      Cairo.move_to ctx
        (         !pixels_per_mm*.x0.(0))
        (height-. !pixels_per_mm*.y0.(0));
      (* Build the piece. *)
      for i = 0 to Array.length piece-1 do
        let (xi,yi) = piece.(i) in
        if Array.length xi = 4 then
          Cairo.curve_to ctx
            (!pixels_per_mm*.xi.(1)) (height-. !pixels_per_mm*.yi.(1))
            (!pixels_per_mm*.xi.(2)) (height-. !pixels_per_mm*.yi.(2))
            (!pixels_per_mm*.xi.(3)) (height-. !pixels_per_mm*.yi.(3))
        else
          Cairo.line_to ctx
            (         !pixels_per_mm*.xi.(Array.length xi-1))
            (height-. !pixels_per_mm*.yi.(Array.length yi-1))
      done;
      (* Close the piece of path if appropriate. *)
      if param.close then Cairo.Path.close ctx;
    in
    List.iter draw_piece path;
    (* Actually draw (fill / stroke) the piece of path. *)
    match (param.strokingColor, param.fillColor) with
    | (Some cs, Some cf) ->
        let (rs,gs,bs) = to_rgb cs in
        let (rf,gf,bf) = to_rgb cf in
        Cairo.set_source_rgb ctx rf gf bf;
        Cairo.fill_preserve ctx ;
        Cairo.set_source_rgb ctx rs gs bs;
        Cairo.stroke ctx
    | (Some cs, None   ) ->
        let (rs,gs,bs) = to_rgb cs in
        Cairo.set_source_rgb ctx rs gs bs;
        Cairo.stroke ctx
    | (None   , Some cf) ->
        let (rf,gf,bf) = to_rgb cf in
        Cairo.set_source_rgb ctx rf gf bf;
        Cairo.fill ctx
    | (None   , None   ) -> ()
  in
  (* Actual drawing function. *)
  let rec draw : RawContent.raw list -> unit = fun l ->
    let open RawContent in
    let warn msg = Printf.eprintf "[DriverCairo] %s\n%!" msg in
    match l with
    | []        -> ()
    | e :: rest ->
    match e with
    (* Unsupported contents type. *)
    | Video(_)     -> warn "Video not supported."; draw rest
    | States(_)    -> warn "State not supported."; draw rest
    | Image(_)     -> warn "Image not supported."; draw rest
    (* Easy cases, just propagate. *)
    | Link(l)      -> draw (l.link_contents @ rest)
    | Animation(a) -> draw (a.anim_contents.(a.anim_default) @ rest)
    | Dynamic(d)   -> draw (d.dyn_contents () @ rest)
    (* Draw the path and continue. *)
    | Path(cfg,p)  -> draw_path cfg p; draw rest
    (* Draw the outlines of the glyph. *)
    | Glyph(g)     ->
        let outlines = Fonts.outlines g.glyph in
        let fn piece =
          let gn (x,y) =
            ( Array.map (fun xx->(xx*.g.glyph_size/.1000.+.g.glyph_x)) x
            , Array.map (fun xx->(xx*.g.glyph_size/.1000.+.g.glyph_y)) y )
          in
          Array.of_list (List.map gn piece)
        in
        let cfg =
          let fillColor = Some(g.glyph_color) in
          let strokingColor = None in
          {default_path_param with close = true; fillColor; strokingColor}
        in
        draw_path cfg (List.map fn outlines); draw rest
    (* Apply the transformation and continue. *)
    | Affine(a)    ->
        let (x0, y0) = (0.0, -.height) in
        let x1 = a.affine_matrix.(0).(0)*.x0 +. a.affine_matrix.(0).(1)*.y0 in
        let y1 = a.affine_matrix.(1).(0)*.x0 +. a.affine_matrix.(1).(1)*.y0 in
        (* Apply the transformation and draw the corresponding contents. *)
        Cairo.(transform ctx
          { xx =   a.affine_matrix.(0).(0)
          ; yx = -.a.affine_matrix.(1).(0)
          ; xy = -.a.affine_matrix.(0).(1)
          ; yy =   a.affine_matrix.(1).(1)
          ; x0 =           a.affine_matrix.(0).(2) *. !pixels_per_mm -. x1
          ; y0 = height -. a.affine_matrix.(1).(2) *. !pixels_per_mm +. y1 });
        draw a.affine_contents;
        (* Revert the transformation and draw the rest. *)
        Cairo.identity_matrix ctx;
        draw rest
  in
  draw contents

let output ?structure:(_=empty_structure) pages fname =
  (* Create the directory that will hold the generated files. *)
  let dir = (try Filename.chop_extension fname with _ -> fname) ^ "_cairo" in
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  (* Iterate over the pages to generate images. *)
  let write_page i page =
    (* Obtain the size of the page as floating-point number. *)
    let width  = fst page.size *. !pixels_per_mm in
    let height = snd page.size *. !pixels_per_mm in
    (* Create the Cairo surface for drawing. *)
    let surface =
      let w = int_of_float width  in
      let h = int_of_float height in
      Cairo.Image.(create ARGB32 ~w ~h)
    in
    (* Draw the page, sorting the elements for the drawing order. *)
    let contents = RawContent.drawing_sort page.contents in
    draw_page (width, height) surface contents;
    (* Write the surface to the corresponding file. *)
    let fname =
      match Array.length pages with
      | n when n <= 10   -> Printf.sprintf "page_%1d.png" i
      | n when n <= 100  -> Printf.sprintf "page_%2d.png" i
      | n when n <= 1000 -> Printf.sprintf "page_%3d.png" i
      | _                -> Printf.sprintf "page_%6d.png" i
    in
    Cairo.PNG.write surface (Filename.concat dir fname)
  in
  Array.iteri write_page pages

let output' = output_to_prime output

let _ =
  Hashtbl.add DynDriver.drivers "DriverCairo"
    (module struct
      let output  = output
      let output' = output'
     end:OutputDriver)
