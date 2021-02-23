open Patfonts
open Patutil

(* A first atomic type of contents: a glyph. *)
type glyph =
  { glyph_x     : float
  ; glyph_kx    : float
  ; glyph_y     : float
  ; glyph_ky    : float
  ; glyph_order : int
  ; glyph_color : Color.color
  ; glyph_size  : float
  ; glyph       : Fonts.glyph }

(* Another atomic type of contents: a path following a bezier curve. *)
type lineCap = Butt_cap | Round_cap | Proj_square_cap
type lineJoin = Miter_join | Round_join | Bevel_join
type path_param =
  { path_order    : int
  ; close         : bool
  ; strokingColor : Color.color option
  ; fillColor     : Color.color option
  ; lineCap       : lineCap
  ; lineJoin      : lineJoin
  ; lineWidth     : float
  ; dashPattern   : float list }
type path = path_param * Bezier.curve array list

let default_path_param =
  { path_order    = 0
  ; close         = false
  ; strokingColor = Some Color.black
  ; fillColor     = None
  ; lineCap       = Butt_cap
  ; lineJoin      = Miter_join
  ; lineWidth     = 0.1
  ; dashPattern   = [] }

(* Two more types of atomic contents : image and video. *)
type image =
  { image_file   : string
  ; image_x      : float
  ; image_y      : float
  ; image_order  : int
  ; image_width  : float
  ; image_height : float
  ; image_pixel_width  : int
  ; image_pixel_height : int }

let image filename =
  let (w,h) = ImageLib_unix.size filename in
  { image_file   = filename
  ; image_x      = 0.0
  ; image_y      = 0.0
  ; image_order  = 0
  ; image_width  = float_of_int w /. 1.0
  ; image_height = float_of_int h /. 1.0
  ; image_pixel_width  = w
  ; image_pixel_height = h }

type video =
  { video_file   : string
  ; video_x      : float
  ; video_y      : float
  ; video_order  : int
  ; video_height : float
  ; video_width  : float
  ; video_pixel_width  : int
  ; video_pixel_height : int }

(* The lowest level of contents type. Drivers need to be able to typeset
elements of this type to produce output. *)
type raw =
  (* Atomic contents. *)
  | Glyph     of glyph
  | Path      of path
  | Image     of image
  | Video     of video
  (* Contents transformers. *)
  | Link      of link      (* Contents with a link. *)
  | Affine    of affine    (* Afine transformation on a list of contents. *)
  | States    of states    (* ??? *)
  | Animation of animation (* ??? *)
  | Dynamic   of raw list dynamic (* ??? *)

 (* Link on some contents. *)
and write = (string * Util.visibility) list (* the written data *)
and button_kind =
  | Click of (unit -> write)
  | Drag of ((float * float) -> bool (* true: release button *) -> write)
  | Edit of string * string * (string  -> write)
      (* snd elt is init value to reset contents *)
  | Menu of ((unit -> write) * raw list) list

and link_kind =
  | Extern of string                             (* URI *)
  | Intern of string * int * float * float       (* (label, page, x, y) *)
  | Button of button_kind * string
and link =
  { mutable link_x0       : float
  ; mutable link_y0       : float
  ; mutable link_x1       : float
  ; mutable link_y1       : float
  ; mutable link_closed   : bool
  ; link_order            : int
  ; link_kind             : link_kind
  ; mutable link_contents : raw list }

(* Afine transformation on contents. *)
and affine =
  { affine_matrix   : float array array
  ; affine_contents : raw list
  ; affine_order    : int }

(* States. *)
and states =
  { states_contents : raw list
  ; states_states   : int list
  ; states_order    : int }

(* Animation. *)
and animation =
  { anim_contents : raw list array
  ; anim_default  : int
  ; anim_step     : float (* in seconds *)
  ; anim_duration : float (* in seconds, stop to save cpu consumption *)
  ; anim_mirror   : bool
  ; anim_order    : int }

(* Dynamic. *)
and 'a dynamic =
  { dyn_label    : string
  ; dyn_contents : unit -> 'a
  ; dyn_sample   : 'a
  ; dyn_order    : int }

(* Shortcut function to build a state raw element. *)
let states states_states states_contents =
  States { states_contents ; states_states ; states_order = 0 }

(* Shortcut functionto build an affine transformation of raw elements. *)
let affine matrix l =
  Affine { affine_matrix = matrix ; affine_contents = l ; affine_order = 0 }

(* Rotation of raw elements using an affine transformation. *)
let rotation_matrix th =
  let costh = cos th in
  let sinth = sin th in
  [| [| costh ; -.sinth ; 0.0 |]
  ;  [| sinth ; costh   ; 0.0 |]
  ;  [| 0.0   ; 0.0     ; 1.0 |] |]

let rotate a = affine (rotation_matrix a)

(* Set the drawing order of a raw element. *)
let in_order i = function
  | Glyph gl    -> Glyph { gl with glyph_order = i }
  | Path (pm,p) -> Path ({ pm with path_order = i },p)
  | Link lk     -> Link { lk with link_order = i }
  | Image img   -> Image { img with image_order = i }
  | Video vid   -> Video { vid with video_order = i }
  | States st   -> States { st with states_order = i }
  | Animation a -> Animation { a with anim_order = i }
  | Dynamic dyn -> Dynamic { dyn with dyn_order = i }
  | Affine aff  -> Affine { aff with affine_order = i }

(* Obtain the drawing order of a raw element. *)
let drawing_order = function
  | Glyph gl    -> gl.glyph_order
  | Path (pm,_) -> pm.path_order
  | Link lk     -> lk.link_order
  | Image img   -> img.image_order
  | Video vid   -> vid.video_order
  | States st   -> st.states_order
  | Animation a -> a.anim_order
  | Dynamic dyn -> dyn.dyn_order
  | Affine aff  -> aff.affine_order

(* Select the contents that is in state [s] in the list of raw [l]. *)
let rec in_state s l =
  let f acc e =
    match e with
    | Glyph _     -> e :: acc
    | Path _      -> e :: acc
    | Image _     -> e :: acc
    | Video _     -> e :: acc
    | Affine aff  ->
        let affine_contents = in_state s aff.affine_contents in
        Affine { aff with affine_contents } :: acc
    | Link lk     ->
        let link_contents = in_state s lk.link_contents in
        Link { lk with link_contents } :: acc
    | States st   ->
        if st.states_states = [] || List.mem s st.states_states then
          let states_contents = in_state s st.states_contents in
          States { st with states_contents } :: acc
        else acc
    | Animation a ->
        let anim_contents = Array.map (in_state s) a.anim_contents in
        Animation { a with anim_contents } :: acc
    | Dynamic dyn ->
        let dyn_contents () = in_state s (dyn.dyn_contents ()) in
        Dynamic { dyn with dyn_contents } :: acc
  in List.fold_left f [] l

(* Translates a raw contents element. *)
let rec translate x y = function
  | Glyph gl -> Glyph
      { gl with glyph_x = gl.glyph_x +. x ; glyph_y = gl.glyph_y +. y
      ; glyph_kx = gl.glyph_kx +. x ; glyph_ky = gl.glyph_ky +. y }
  | Path (pm,p) ->
      let f (xs,ys) =
        (Array.map (fun x0 -> x0 +. x) xs, Array.map (fun y0 -> y0 +. y) ys)
      in Path (pm, List.map (Array.map f) p)
  | Link lk -> Link
      { lk with link_x0 = lk.link_x0 +. x ; link_y0 = lk.link_y0 +. y
      ; link_x1 = lk.link_x1 +. x ; link_y1 = lk.link_y1 +. y
      ; link_contents = List.map (translate x y) lk.link_contents }
  | Image img -> Image
      { img with image_x = img.image_x +. x ; image_y = img.image_y +. y }
  | Video vid -> Video
      { vid with video_x = vid.video_x +. x ; video_y = vid.video_y +. y }
  | States s -> States
      { s with states_contents = List.map (translate x y) s.states_contents }
  | Animation a ->
      let trs = Array.map (List.map (translate x y)) in
      Animation { a with anim_contents = trs a.anim_contents }
  | Dynamic d ->
      let trs = List.map (translate x y) in
      Dynamic { d with dyn_contents = fun e -> trs (d.dyn_contents e) }
  | Affine a ->
      begin
        let m = Array.map (Array.copy) a.affine_matrix in
        m.(0).(2) <- m.(0).(2) +. x;
        m.(1).(2) <- m.(1).(2) +. y;
        Affine { a with affine_matrix = m }
      end

(* Resize (scale) a raw contents element. *)
let rec resize al = function
  | Glyph gl -> Glyph
      { gl with glyph_x = gl.glyph_x *. al ; glyph_y = gl.glyph_y *. al
      ; glyph_kx = gl.glyph_kx *. al ; glyph_ky = gl.glyph_ky *. al
      ; glyph_size = gl.glyph_size *. al }
  | Path (pm,p) ->
      let pm =
        { pm with lineWidth = pm.lineWidth *. al
        ; dashPattern = List.map (fun x -> x *. al) pm.dashPattern }
      in
      let f (xs,ys) =
        (Array.map (fun x0 -> x0 *. al) xs, Array.map (fun y0 -> y0 *. al) ys)
      in Path (pm, List.map (Array.map f) p)
  | Link lk -> Link
      { lk with link_x0 = lk.link_x0 *. al ; link_y0 = lk.link_y0 *. al
      ; link_x1 = lk.link_x1 *. al ; link_y1 = lk.link_y1 *. al
      ; link_contents = List.map (resize al) lk.link_contents }
  | Image img -> Image
      { img with image_width = img.image_width *. al
      ; image_height = img.image_height *. al }
  | Video vid -> Video
      { vid with video_width = vid.video_width *. al
      ; video_height = vid.video_height *. al }
  | States s-> States
      { s with states_contents = List.map (resize al) s.states_contents }
  | Animation a ->
      let rsz = Array.map (List.map (resize al)) in
      Animation { a with anim_contents = rsz a.anim_contents }
  | Dynamic d ->
      let rsz = List.map (resize al) in
      Dynamic { d with dyn_contents = fun e -> rsz (d.dyn_contents e) }
  | Affine a ->
      let m = Array.map (Array.map (fun x -> x *. al)) a.affine_matrix in
      Affine { a with affine_matrix = m }

(* Transform a path given a matrix. *)
let affine_path_transform m =
  let f (u,v) =
    let f1 i x = m.(0).(0) *. x +. m.(0).(1) *. v.(i) +. m.(0).(2) in
    let f2 i x = m.(1).(0) *. x +. m.(1).(1) *. v.(i) +. m.(1).(2) in
    (Array.mapi f1 u, Array.mapi f2 u)
  in List.map (Array.map f)

(* Rectangular path given two points. *)
let rectangle (xa,ya) (xb,yb) =
  [| ([|xa;xa|],[|ya;yb|])
  ;  ([|xa;xb|],[|yb;yb|])
  ;  ([|xb;xb|],[|yb;ya|])
  ;  ([|xb;xa|],[|ya;ya|]) |]

(* Line path given two points. *)
let line (xa,ya) (xb,yb) = [|xa;xb|],[|ya;yb|]

(* Approximate a circle using paths given the radius. *)
let circle r =
  let lambda = r *. 4.0 *. (sqrt 2.0 -. 1.0) /. 3.0 in
  [| ([|-.r;-.r;-.lambda;0.0|],[|0.0;lambda;r;r|])
  ;  ([|0.0;lambda;r;r|], [|r;r;lambda;0.0|])
  ;  ([|r;r;lambda;0.0|], [|0.0;-.lambda;-.r;-.r|])
  ;  ([|0.0;-.lambda;-.r;-.r|], [|-.r;-.r;-.lambda;0.0|]) |]

(* Transform a raw element into a list of paths. *)
let rec toPaths = function
  | Path (_,p) -> p
  | Glyph gl   ->
      let f (xs,ys) =
        let sc v = v *. gl.glyph_size /. 1000.0 in
        (Array.map sc xs, Array.map sc ys)
      in
      let outlines = Fonts.outlines gl.glyph in
      List.map (fun l -> Array.of_list (List.map f l)) outlines
  | Image img ->
      let x = img.image_x and y = img.image_y in
      let w = img.image_width and h = img.image_height in
      [rectangle (x,y) (x +. w, y +. h)]
  | Video vid ->
      let x = vid.video_x and y = vid.video_y in
      let w = vid.video_width and h = vid.video_height in
      [rectangle (x,y) (x +. w, y +. h)]
  | States s ->
      List.concat (List.map toPaths s.states_contents)
  | Link lk ->
      List.concat (List.map toPaths lk.link_contents)
  | Animation a ->
      let c = List.concat (Array.to_list a.anim_contents) in
      List.concat (List.map toPaths c)
  | Dynamic d -> List.concat (List.map toPaths (d.dyn_contents ()))
  | Affine a ->
      affine_path_transform a.affine_matrix
      (List.concat (List.map toPaths a.affine_contents))

type bounding_box_opt =
  { ignore_negative_abcisse : bool
  ; ignore_after_glyphWidth : bool
  ; ignore_under_base_line  : bool }

(* Compute the minimal bounding box including the rectangle (x0,y0) (x1,y1)
and the given paths. *)
let paths_bounding_box x0 y0 x1 y1 ps =
  let x0' = ref x0 and y0' = ref y0 in
  let x1' = ref x1 and y1' = ref y1 in
  let f p =
    for i=0 to Array.length p-1 do
      let ((xa,ya),(xb,yb)) = Bezier.bounding_box p.(i) in
      x0' := min !x0' xa; y0' := min !y0' ya;
      x1' := max !x1' xb; y1' := max !y1' yb;
    done
  in
  List.iter f ps; (!x0', !y0', !x1', !y1')

(* Compute the bounding box of a glyph. *)
let glyph_bounding_box opt gl =
  let x0 =
    if opt.ignore_negative_abcisse then gl.glyph_kx
    else gl.glyph_x +. Fonts.glyph_x0 gl.glyph *. gl.glyph_size /. 1000.0
  in
  let w =
    if opt.ignore_after_glyphWidth then Fonts.glyphWidth gl.glyph
    else Fonts.glyph_x1 gl.glyph
  in
  let x1 = gl.glyph_x +. w *. gl.glyph_size /. 1000.0 in
  let y0 =
    if opt.ignore_under_base_line then gl.glyph_ky
    else gl.glyph_y +. Fonts.glyph_y0 gl.glyph *. gl.glyph_size /. 1000.0
  in
  let y1 = gl.glyph_y +. Fonts.glyph_y1 gl.glyph *. gl.glyph_size /. 1000.0
  in (x0,y0,x1,y1)

(* Compute the minimal bounding box containing every element of the raw
contents list. *)
let bounding_box_opt opt l =
  let rec bbox x0 y0 x1 y1 = function
    | [] -> (x0, y0, x1, y1)
    | Glyph gl :: l ->
        let (x0', y0', x1', y1') = glyph_bounding_box opt gl in
        bbox (min x0 x0') (min y0 y0') (max x1 x1') (max y1 y1') l
    | Path (_,ps) :: l ->
        let (x0',y0',x1',y1') = paths_bounding_box x0 y0 x1 y1 ps in
        bbox x0' y0' x1' y1' l
    | Image img :: l ->
        let x0' = img.image_x in
        let y0' = img.image_y in
        let x1' = x0' +. img.image_width in
        let y1' = y0' +. img.image_height in
        bbox (min x0 x0') (min y0 y0') (max x1 x1') (max y1 y1') l
    | Video vid :: l ->
        let x0' = vid.video_x in
        let y0' = vid.video_y in
        let x1' = x0' +. vid.video_width in
        let y1' = y0' +. vid.video_height in
        bbox (min x0 x0') (min y0 y0') (max x1 x1') (max y1 y1') l
    | States s :: l ->
        let (x0',y0',x1',y1') = bbox x0 y0 x1 y1 s.states_contents in
        bbox x0' y0' x1' y1' l
    | Link lk :: l ->
        let (x0',y0',x1',y1') = bbox x0 y0 x1 y1 lk.link_contents in
        bbox x0' y0' x1' y1' l
    | Animation a :: l ->
        let contents = List.concat (Array.to_list a.anim_contents) in
        let (x0',y0',x1',y1') = bbox x0 y0 x1 y1 contents in
        bbox x0' y0' x1' y1' l
    | Dynamic d :: l ->
        let (x0',y0',x1',y1') = bbox x0 y0 x1 y1 (d.dyn_contents ()) in
        bbox x0' y0' x1' y1' l
    | Affine a :: l ->
        let paths = List.concat (List.map toPaths a.affine_contents) in
        let paths = affine_path_transform a.affine_matrix paths in
        let (x0',y0',x1',y1') = paths_bounding_box x0 y0 x1 y1 paths in
        bbox x0' y0' x1' y1' l
  in bbox infinity infinity (-.infinity) (-.infinity) l

(* Bounding box functions with different settings *)
let bounding_box = bounding_box_opt
  { ignore_negative_abcisse = true
  ; ignore_after_glyphWidth = true
  ; ignore_under_base_line  = false }

let bounding_box_kerning = bounding_box_opt
  { ignore_negative_abcisse = true
  ; ignore_after_glyphWidth = true
  ; ignore_under_base_line  = true }

let bounding_box_full = bounding_box_opt
  { ignore_negative_abcisse = false
  ; ignore_after_glyphWidth = false
  ; ignore_under_base_line  = false }

(* Sorting a list of raw contents according to its drawing order. *)
let drawing_sort l =
  let open Extra in
  let rec make_list acc = function
    | []            -> acc
    | States s :: l ->
        let f m x =
          let l = try IntMap.find (drawing_order x) m with Not_found -> [] in
          IntMap.add (drawing_order x) (x::l) m
        in
        let m = List.fold_left f IntMap.empty s.states_contents in
        let f k a l =
          States { s with states_order = k ; states_contents = a }::l
        in
        make_list (IntMap.fold f m acc) l
    | e :: l        -> make_list (e :: acc) l
  in
  let cmp a b = compare (drawing_order a) (drawing_order b) in
  List.sort cmp (make_list [] l)

(* ??? *)
let sort_raw l =
  let open Extra in
  let f m x =
    let m' = try IntMap.find (drawing_order x) m with Not_found -> [] in
    IntMap.add (drawing_order x) (x :: m') m
  in
  let x = List.fold_left f IntMap.empty l in
  let comp a b =
    match (a,b) with
    | (Glyph ga, Glyph gb) ->
        if ga.glyph_y = gb.glyph_y then compare ga.glyph_x gb.glyph_x
        else compare gb.glyph_y ga.glyph_y
    | (Glyph _ , _       ) -> -1
    | (_       , Glyph _ ) -> 1
    | _                    -> 0
  in
  let subsort = function
    | Link l -> Link { l with link_contents = List.sort comp l.link_contents }
    | e      -> e
  in
  let m = IntMap.map (fun l -> List.sort comp (List.map subsort l)) x in
  IntMap.fold (fun _ a x -> x@a) m []



(* Print a raw content, deguging function. *)
let rec print_raw ch = function
  | Glyph gl ->
      let glyph = (Fonts.glyphNumber gl.glyph).FTypes.glyph_utf8 in
      let x = gl.glyph_x and y = gl.glyph_y and sz = gl.glyph_size in
      Printf.fprintf ch "Glyph %s (%f,%f) size %f\n" glyph x y sz
  | Path (_,ps) ->
      let f p =
        let f (x,y) =
          let sx = List.map string_of_float (Array.to_list x) in
          let sy = List.map string_of_float (Array.to_list y) in
          "[|"^ (String.concat ";" sx) ^"|],[|"^ (String.concat ";" sy) ^"|]"
        in
        let a = Array.map f p in
        "[" ^ (String.concat ";" (Array.to_list a)) ^"]"
      in
      let l = List.map f ps in
      Printf.fprintf ch "Path [%s]\n" (String.concat "," l)
  | Image img ->
      let x0 = img.image_x and y0 = img.image_y in
      let x1 = x0 +. img.image_width and y1 = y0 +. img.image_height in
      Printf.fprintf ch "Image (%f,%f) (%f,%f)\n" x0 y0 x1 y1
  | Video vid ->
      let x0 = vid.video_x and y0 = vid.video_y in
      let x1 = x0 +. vid.video_width and y1 = y0 +. vid.video_height in
      Printf.fprintf ch "Video (%f,%f) (%f,%f)\n" x0 y0 x1 y1
  | States s ->
      List.iter (print_raw ch) s.states_contents
  | Link lk ->
      let f ch = List.iter (print_raw ch) in
      Printf.fprintf ch "Link [ %a ]\n" f lk.link_contents
  | Animation a ->
      let f ch =
        let f ch = List.iter (print_raw ch) in
        Array.iter (Printf.fprintf ch "[%a]" f)
      in
      Printf.fprintf ch "Animation [ %a ] " f a.anim_contents;
      Printf.fprintf ch "(default=%d,mirror=%b,step=%f,duration=%f)"
        a.anim_default a.anim_mirror a.anim_step a.anim_duration
  | Dynamic d ->
      Printf.fprintf stderr "Dynamic %s [ %a ]\n" d.dyn_label
       (fun ch -> List.iter (print_raw ch)) (d.dyn_contents ())
  | Affine a ->
      Printf.fprintf stderr "Affine [ %g %g %g %g %g %g ] [%a]\n"
       a.affine_matrix.(0).(0) a.affine_matrix.(0).(1) a.affine_matrix.(0).(2)
       a.affine_matrix.(1).(0) a.affine_matrix.(1).(1) a.affine_matrix.(1).(2)
       (fun ch -> List.iter (print_raw ch)) a.affine_contents
