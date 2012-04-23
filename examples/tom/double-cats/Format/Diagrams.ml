open Typography
open Document
module Drivers = OutputCommon
open OutputCommon

let pi = 3.14159
let one_third = 1. /. 3.
let half_pi = pi /. 2.
let rec list_last = function
  [] -> assert false
  | [x] -> x
  | x :: l -> list_last l

let list_split_last_rev l = 
  let rec list_split_last_rec res = function
    | [] -> assert false
    | [x] -> res, x
    | x :: l -> list_split_last_rec (x :: res) l
  in list_split_last_rec [] l

module Point = struct 
  type t = float * float
  type point = t
  let proj (x,y) = x
  let proj' (x,y) = y
  let middle (x0,y0) (x1,y1) = (0.5 *. (x0 +. x1), 0.5 *. (y0 +. y1))
  let distance (x0,y0) (x1,y1) = sqrt ((x1 -. x0)**2. +. (y1 -. y0)**2.)
  let (+) (x0,y0) (x1,y1) = (x0 +. x1), (y0 +. y1)
  let (-) (x0,y0) (x1,y1) = (x0 -. x1), (y0 -. y1)
  let (/) (x0,y0) r = (x0 /. r, y0 /.r)
end


module Vector = struct
  type t = Point.t 
  let of_points p q = ((Point.proj q -. Point.proj p),
		       (Point.proj' q -. Point.proj' p))
  let scal_mul r (x,y) = (x *. r, y *. r)
  let (+) (x,y) (x',y') = (x +. x', y +. y')
  let (-) (x,y) (x',y') = (x -. x', y -. y')
  let minus (x,y) = (-. x,-. y)
  let translate p vec = (p + vec)
  let rotate angle (x,y) = (* Angle en radians *)
    (x *. cos angle -. y *. sin angle,
     x *. sin angle +. y *. cos angle)
  let norm (x,y) = sqrt (x ** 2. +. y ** 2.)
  let normalise ?norm:(norm'=1.0) vec = 
    let l = norm vec in
    if l == 0. then failwith "Can't normalise the 0 vector." else
    scal_mul (norm' /. l) vec
  let turn_left (x,y) = (-. y,x)
  let turn_right (x,y) = (y, -. x)
end

module Curve = struct
  type t =  Bezier.curve list
  let bezier_of_point_list l = 
    let xs, ys = List.split l in
    (Array.of_list xs, Array.of_list ys)
  let nb_beziers curve = List.length curve
  let of_point_lists l = List.map bezier_of_point_list l
  let of_contents = function
    | OutputCommon.Path (_, beziers) -> List.map Array.to_list beziers
    | _ -> failwith "Attempt to convert a non-path value of type OutputCommon.content to Curve.t."
  let draw
    ?parameters:(parameters=OutputCommon.default)
    (curve:t) =
    if curve = [] then [] else
      [OutputCommon.Path (parameters, [Array.of_list curve])]

  let translate (x,y) curves = List.map (fun (xs,ys) ->
    (Array.map ((+.) x) xs,
     Array.map ((+.) y) ys))
    curves

  let global_time curve (i,t) = 
    let n = List.length curve in
    (t +. (float_of_int i)) /. (float_of_int n)

  let local_time curve a =
    let n = List.length curve in
    let a' = a *. (float_of_int n) in
    let i_float = floor a' in
    let i = int_of_float i_float in
    let t = a' -. i_float in
    (i, t)

  let intersections bezier1 beziers2 = 
    let res, _ = List.split(List.flatten (List.map (Bezier.intersect bezier1) beziers2)) in
    List.map (fun t -> (t,bezier1)) res

  let intersections beziers1 beziers2 = 
    let _, res = 
      List.fold_left
      (fun (i, res) bezier1 -> 
	((succ i),
	 (res @ (List.map (fun (t1,t2) -> (i,t1)) (intersections bezier1 beziers2)))))
      (0,[])
      beziers1
    in res

  let latest_intersection beziers1 beziers2 =
    let inters = (intersections beziers1 beziers2) in
    let latest = List.fold_left 
      (fun yet ((i,t) as candidate) -> match yet with
	| None -> Some candidate
	| Some (_,_) -> Some (i,t))
      None
      inters
    in latest


  let earliest_intersection beziers1 beziers2 =
    match (intersections beziers1 beziers2) with
      | [] -> None
      | (i,t) :: _ -> Some (i,t)

  let bezier_evaluate (xs,ys) t = 
    (Bezier.eval xs t, Bezier.eval ys t)

  let eval beziers t =
    let t = t *. (float_of_int (List.length beziers)) in
    let rec eval_rec beziers t = 
      match beziers with 
	| [] -> Printf.fprintf stderr ("Warning: evaluating an empty curve. Using (0.,0.)") ; (0.,0.)
	| bezier :: reste -> 
	  if t <= 1. then bezier_evaluate bezier t
	  else eval_rec reste (t -. 1.)
    in eval_rec beziers t

  let internal_restrict curve (i,t) (j,t') = 

    let rec restrict_right res curve (j,t') = match curve with
	[] -> Printf.fprintf stderr ("Warning: attempt to restrict an empty curve.\n") ; []
      | (xs,ys) as bezier :: rest -> if j = 0 then
	  let xs' = Bezier.restrict xs 0. t' in
	  let ys' = Bezier.restrict ys 0. t' in
	  List.rev ((xs',ys') :: res)
	else restrict_right (bezier :: res) rest (j-1,t')
    in

    let rec restrict_left curve (i,t) (j,t') = match curve with
	[] -> Printf.fprintf stderr ("Warning: attempt to restrict an empty curve.\n") ; []
      | (xs,ys) :: rest -> if i = 0 then begin
	  if i = j then 
	  let xs' = Bezier.restrict xs t t' in
	  let ys' = Bezier.restrict ys t t' in
	  [(xs',ys')]
	  else
	    let xs' = Bezier.restrict xs t 1. in
	    let ys' = Bezier.restrict ys t 1. in
	    restrict_right [] ((xs',ys') :: rest) (j,t')
      end
	else
	  restrict_left rest (i-1,t) (j-1,t')
    in
    
    if i <= j then 
    restrict_left curve (i,t) (j,t')
    else 
      (Printf.fprintf stderr ("Warning: restriction to an empty curve.\n") ;
       [])

  let restrict curve a b = 
    internal_restrict curve (local_time curve a) (local_time curve b)

  let split2 curve a b =
    let (i,t) = local_time curve a in
    let (j,t') = local_time curve b in

    let rec split2_right curve1 res curve (j,t') = match curve with
	[] -> Printf.fprintf stderr ("Warning: attempt to split2 an empty curve.\n") ; [],[],[]
      | (xs,ys) as bezier :: rest -> if j = 0 then
	  let xs1, xs', xs2 = Bezier.split2 xs 0. t' in
	  let ys1, ys', ys2 = Bezier.split2 ys 0. t' in
	  (curve1, 
	   (List.rev ((xs',ys') :: res)), 
	   (xs2, ys2) :: rest)
	else split2_right curve1 (bezier :: res) rest (j-1,t')
    in

    let rec split2_left curve curve1 (i,t) (j,t') = match curve with
	[] -> Printf.fprintf stderr ("Warning: attempt to split2 an empty curve.\n") ; [],[],[]
      | (xs,ys) :: rest -> if i = 0 then begin
	if i = j then 
	  let xs1, xs', xs2 = Bezier.split2 xs t t' in
	  let ys1, ys', ys2 = Bezier.split2 ys t t' in
	  (List.rev ((xs1,ys1) :: curve1)),
	  [(xs',ys')],
	  ((xs2,ys2) :: rest)
	else
	  (* Aaaaah il faut splitter en deux juste, c'est pas super efficace... *)
	  let xs1, xs', xs2 = Bezier.split2 xs t 1. in
	  let ys1, ys', ys2 = Bezier.split2 ys t 1. in
	  split2_right (List.rev ((xs1,ys1) :: curve1)) [] ((xs',ys') :: rest) (j,t')
      end
	else
	  split2_left rest ((xs,ys) :: curve1) (i-1,t) (j-1,t')
    in
    
    if i <= j then 
    split2_left curve [] (i,t) (j,t')
    else 
      (Printf.fprintf stderr ("Warning: split2ion to an empty curve.\n") ;
       [],[],[])


(*   let curviligne xs ys abs = *)
(*     let significant, exponent = frexp t in  *)
(*     let beziers_x = divide xs (2 ** exponent) in *)
(*     let beziers_y = divide ys (2 ** exponent) in *)
(*     let epsilon = 2 ** exponent in *)
(*     let rec scan t_yet abs_yet xs' ys' = *)
(*       let length =  *)
(* 	if abs_float (abs -. abs_yet) < epsilon then *)
(* 	  t_yet *)
(* ) *)
(*     (0.,0.) *)
(*     (List.combine beziers_x beziers_y) *)


end

module Rectangle = struct
  type t = { bottom_left : Point.t ; top_right : Point.t }
  let curve : t -> Curve.t = fun r ->
    let x1,y1 = r.bottom_left in
    let x3,y3 = r.top_right in
    let x2 = x3 in
    let y2 = y1 in
    let x4 = x1 in
    let y4 = y3 in
    Curve.of_point_lists [
      [(x1,y1);(x2,y2)];
      [(x2,y2);(x3,y3)];
      [(x3,y3);(x4,y4)];
      [(x4,y4);(x1,y1)]
    ]
  (* let draw ?parameters:(parameters=OutputCommon.default) r:t =  *)
  (*   Curve.draw ~parameters:parameters (curve r) *)
  let make x1 y1 x3 y3 = { bottom_left = (x1,y1) ;
			   top_right = (x3, y3) }
  let points: t -> Point.t list = fun r ->
    let x1,y1 = r.bottom_left in
    let x3,y3 = r.top_right in
    let x2 = x3 in
    let y2 = y1 in
    let x4 = x1 in
    let y4 = y3 in
    [(x1,y1);(x2,y2);(x3,y3);(x4,y4)]
end

module ArrowTip = struct
  type t = { parameters : OutputCommon.path_parameters ; 
	     curve : Curve.t }

  let none = { parameters = OutputCommon.default ; curve = Curve.of_point_lists [] }

  let simple ?size:(size=3.0)
      ?angle:(angle=0.4)
      ?bend:(bend=0.5)
      ?parameters:(parameters=OutputCommon.default) 
      curve =
	let a, b = list_last curve in
	let da = Bezier.eval (Bezier.derivee a) 1. in
	let db = Bezier.eval (Bezier.derivee b) 1. in
	let x = Bezier.eval a 1. in
	let y = Bezier.eval b 1. in
	let p = (x,y) in
	let vec = Vector.normalise ~norm:size (da,db) in
	let left_vec = Vector.rotate (-. angle) vec in
	let right_vec = Vector.rotate angle vec in
	let (xl,yl) as l = Vector.translate (x,y) (Vector.minus left_vec) in
	let (xr,yr) as r = Vector.translate (x,y) (Vector.minus right_vec) in
	let ml = Vector.translate (Point.middle p l) (Vector.normalise
							~norm:bend
							(Vector.turn_right (Vector.of_points l p)))
	in
	let mr = Vector.translate (Point.middle p r) (Vector.normalise
							~norm:bend
							(Vector.turn_left (Vector.of_points r p)))
	in 
	{ parameters = parameters ;
	  curve = (Curve.of_point_lists [
	  [r;mr;p] ;
	  [p;ml;l]
	]) }

  let draw tip = Curve.draw ~parameters:tip.parameters tip.curve

end


module Anchor = struct
  type t = Vector.t
  type spec = Vec of Vector.t | South | Center | East | West | North
	      | Add of spec * Vector.t  (* To be completed *)
	      | Angle of float 		(* An angle in radians *)
  exception Undefined of spec
end

module NodeShape = struct
  type bb = float * float * float * float
  type t = { parameters : OutputCommon.path_parameters ; 
	     make_curve : bb -> Curve.t ;
	     make_anchor : bb -> Anchor.spec -> Anchor.t }

  let default = { OutputCommon.default with OutputCommon.close = true ; OutputCommon.lineWidth=0.5 } 
  let default_inner_sep = 0.5

  let rectangle ?inner_sep:(inner_sep = default_inner_sep) 
      ?parameters:(parameters=default) () = 
    let extract_points (x0,y0,x1,y1) =
      let x0 = x0 -. inner_sep in
      let y0 = y0 -. inner_sep in
      let x1 = x1 +. inner_sep in
      let y1 = y1 +. inner_sep in
      (* let x_center, y_center = middle (x0,y0)( x1,y1) in *)
      (* let radius = 0.5 *. (distance (x0,y0) (x1,y1)) in *)
      let p1 = (x0,y0) in
      let p2 = (x1,y0) in
      let p3 = (x1,y1) in
      let p4 = (x0,y1) in
      (p1,p2,p3,p4)
    in
    let make_curve bb =
      let (p1,p2,p3,p4) = extract_points bb in
      Curve.of_point_lists [
	[p1;p2] ;	(* The bottom curve *)
	[p2;p3] ;	(* The right-hand curve *)
	[p3;p4] ;	(* The top curve *)
	[p4;p1] 	(* The left-hand curve *)
      ]
    in
    let make_anchor bb = 
      let (p1,p2,p3,p4) = extract_points bb in
      (* let (x0,y0,x1,y1) = bb in *)
      (* let _ = Printf.fprintf stderr ("Rectangle: (%f,%f), (%f,%f)") x0 y0 x1 y1 in *)
      function
	| Anchor.Vec v -> v
	| Anchor.Center -> Point.middle p1 p3
	| Anchor.East -> Point.(+) (Point.middle p2 p3)  (0.5 *. parameters.OutputCommon.lineWidth, 0.0)
	| Anchor.West -> Point.(-) (Point.middle p1 p4)  (0.5 *. parameters.OutputCommon.lineWidth, 0.0)
	| Anchor.North -> Point.(+) (Point.middle p3 p4)  (0.0, 0.5 *. parameters.OutputCommon.lineWidth)
	| Anchor.South -> Point.(-) (Point.middle p1 p2)  (0.0, 0.5 *. parameters.OutputCommon.lineWidth)
	| a -> raise (Anchor.Undefined a)
    in
    { parameters = parameters ;
      make_curve = make_curve ;
      make_anchor = make_anchor
    }


  let circle ?inner_sep:(inner_sep = default_inner_sep) 
      ?parameters:(parameters=default) () = 
    let extract_points (x0,y0,x1,y1) =
      let x0 = x0 -. inner_sep in
      let y0 = y0 -. inner_sep in
      let x1 = x1 +. inner_sep in
      let y1 = y1 +. inner_sep in
      (* let x_center, y_center = Point.middle (x0,y0)( x1,y1) in *)
      (* let radius = 0.5 *. (distance (x0,y0) (x1,y1)) in *)
      match Rectangle.points (Rectangle.make x0 y0 x1 y1) with
	| ((x1,y1) as p1) :: ((x2,y2) as p2) :: ((x3,y3) as p3) :: ((x4,y4) as p4) :: [] ->
	  (p1,p2,p3,p4)
	| _ -> assert false
    in
    let make_curve bb =
      let (p1,p2,p3,p4) = extract_points bb in
      let control p1 p2 =
	Vector.translate
	  (Point.middle p1 p2)
	  (Vector.scal_mul 0.5 (Vector.rotate (-. half_pi) (Vector.of_points p1 p2)));
      in
      Curve.of_point_lists [
	[p1; control p1 p2; p2] ;	(* The bottom curve *)
	[p2; control p2 p3; p3] ;	(* The right-hand curve *)
	[p3; control p3 p4; p4] ;	(* The top curve *)
	[p4; control p4 p1; p1] (* The left-hand curve *)
      ] 
    in
    let make_anchor bb = 
      let (p1,p2,p3,p4) = extract_points bb in
      function
	| Anchor.Vec v -> v
	| Anchor.South -> Point.middle p1 p2
	| Anchor.West -> Point.middle p1 p4
	| Anchor.East -> Point.middle p2 p3
	| Anchor.North -> Point.middle p3 p4
	| Anchor.Center -> Point.middle p1 p3
	| a -> raise (Anchor.Undefined a)
    in
    { parameters = parameters ;
      make_curve = make_curve ;
      make_anchor = make_anchor
    }

  let flower  ?inner_sep:(inner_sep = default_inner_sep) ?amplitude:(amplitude=1.0) 
      ?parameters:(parameters=default) () = 
    let extract_points (x0,y0,x1,y1) =
      let x0 = x0 -. inner_sep in
      let y0 = y0 -. inner_sep in
      let x1 = x1 +. inner_sep in
      let y1 = y1 +. inner_sep in
      (* let x_center, y_center = Point.middle (x0,y0)( x1,y1) in *)
      (* let radius = 0.5 *. (distance (x0,y0) (x1,y1)) in *)
      let p1 = (x0,y0) in
      let p2 = (x1,y0) in
      let p3 = (x1,y1) in
      let p4 = (x0,y1) in
      (p1,p2,p3,p4)
    in
    let make_curve bb =
      let (p1,p2,p3,p4) = extract_points bb in
	  let control p1 p2 =
	    Vector.translate
	      (Point.middle p1 p2)
	      (Vector.scal_mul amplitude (Vector.rotate (-. half_pi) (Vector.of_points p1 p2)));
	  in
	  Curve.of_point_lists [
	    [p1; control p1 p2; p2] ;	(* The bottom curve *)
	    [p2; control p2 p3; p3] ;	(* The right-hand curve *)
	    [p3; control p3 p4; p4] ;	(* The top curve *)
	    [p4; control p4 p1; p1] (* The left-hand curve *)
	  ] 
    in
    let make_anchor bb = 
      let (p1,p2,p3,p4) = extract_points bb in
      function
	| Anchor.Vec v -> v
	| Anchor.South -> begin match make_curve bb with
	    | [ south_bezier ; east_bezier ; top_bezier ; west_bezier ] -> 
	      Curve.bezier_evaluate south_bezier 0.5
	    | _ -> assert false
	end 
	| Anchor.West -> begin match make_curve bb with
	    | [ _ ; east_bezier ; top_bezier ; west_bezier ] -> 
	      Curve.bezier_evaluate west_bezier 0.5
	    | _ -> assert false
	end 
	| Anchor.East -> begin match make_curve bb with
	    | [ _ ; east_bezier ; top_bezier ; _ ] -> 
	      Curve.bezier_evaluate east_bezier 0.5
	    | _ -> assert false
	end 
	| Anchor.North -> begin match make_curve bb with
	    | [ _ ; _ ; top_bezier ; _ ] -> 
	      Curve.bezier_evaluate top_bezier 0.5
	    | _ -> assert false
	end 
	| Anchor.Center -> Point.middle p1 p3
	| a -> raise (Anchor.Undefined a)
    in
    { parameters = parameters ;
      make_curve = make_curve ;
      make_anchor = make_anchor
    }

end

module Node = struct
  type t = { curve : Curve.t ;
	     parameters : OutputCommon.path_parameters ;
	     make_anchor : Anchor.spec -> Point.t ;
	     contents : OutputCommon.contents list }

  let center node = node.make_anchor Anchor.Center

  type spec = { at : Point.t ;
		contents_spec : (Document.user Document.content) list ;
		shape : NodeShape.t ;
		anchor: Anchor.spec }
  let default = { 
    at = (0.,0.) ;
    contents_spec = [] ;
    shape = NodeShape.rectangle ~inner_sep:NodeShape.default_inner_sep 
      ~parameters:NodeShape.default () ;
      anchor = Anchor.Center }

  let make env spec =
    let shape = spec.shape in
    let anchor = spec.anchor in
    let at = spec.at in
    (* Compute the contents a first time to get a bounding box of the right size *)
    let contents = (Boxes.draw_boxes (boxify_scoped env spec.contents_spec)) in
    let bb_boot =  OutputCommon.bounding_box contents in
    (* Use this to compute the real coordinates, by translating "at" according to anchor. *)
    (* But computing anchor needs make_anchor bb.  *)
    let x,y = Vector.translate (Vector.minus (shape.NodeShape.make_anchor bb_boot anchor)) at in
    (* Now translate the contents by x,y and compute the real bounding box, curve, etc *)
    let contents = List.map (OutputCommon.translate x y) contents in
    let bb =  OutputCommon.bounding_box contents in
    let curve = shape.NodeShape.make_curve bb in
    { curve = curve ; 
      parameters = shape.NodeShape.parameters ;
      contents =  contents ; 
      make_anchor = fun anchor_spec -> (shape.NodeShape.make_anchor bb anchor_spec) }

  (* En fait il faudra vite passer aussi curve a make_anchor... *)


  let draw : t -> OutputCommon.contents list = fun node -> 
    let shape_curve = 
      Curve.draw ~parameters:node.parameters node.curve
    in 
    node.contents @ shape_curve

  let make_draw env l spec =
    let node = make env spec in
    node, (l @ (draw node))
end	    

module Edge = struct

  type label_spec = {
    pos : float ;			(* A float between 0 and 1 *)
    node_spec : Node.spec
  }

  type transfo_spec = 
    | BendLeft of float
    | BendRight of float
    | Squiggle of int * float
    | SquiggleFromTo of int * float * float * float
    | Fore of float
    | ShortenS of float			(* A float between 0 and 1 *)
    | ShortenE of float			(* Idem *)
    | Double of float * float		(* Space between the two lines, and their common width *)

  type t = { curves : (OutputCommon.path_parameters * Curve.t) list ;
	     head : ArrowTip.t ;
	     tail : ArrowTip.t ;
	     labels : Node.t list ;
	   }

  type parameters = { parameters_spec : OutputCommon.path_parameters ;
		controls : Point.t list ;
		head_spec : ?parameters:OutputCommon.path_parameters -> Curve.t -> ArrowTip.t ;
		tail_spec : ?parameters:OutputCommon.path_parameters -> Curve.t -> ArrowTip.t ;
		label_specs : label_spec list ;
		transfo_specs : transfo_spec list }

  let default = {
    parameters_spec = OutputCommon.default ;
    controls = [] ;
    head_spec = (fun ?parameters:(parameters = OutputCommon.default) _ -> ArrowTip.none) ;
    tail_spec = (fun ?parameters:(parameters = OutputCommon.default) _ -> ArrowTip.none) ;
    label_specs = [] ;
    transfo_specs = []
  }

  let label_of_spec env curve { pos = pos ; node_spec = spec } =
    Node.make env { spec with Node.at = Curve.eval curve pos }

  let bend ?angle:(angle=30.) node1 node2 = 
    let angle = (angle) *. pi /. 180. in
    let vec = Vector.scal_mul (0.5 /. (cos angle)) (Vector.of_points node1 node2) in
    let vec = Vector.rotate (angle) vec in
    Vector.translate node1 vec

  let squiggle freq angle (xs,ys) = 
    let beziersx' = Bezier.divide xs (2 * freq) in 
    let beziersy' = Bezier.divide ys (2 * freq) in 
    let make_handles left_or_right (xs,ys) = 
      (* Forget the intermediate points, then add two intermediate points, to the left or to the right *)
      let x0 = xs.(0) in
      let x1 = xs.(Array.length xs - 1) in
      let y0 = ys.(0) in
      let y1 = ys.(Array.length ys - 1) in
      let p0 = (x0,y0) in
      let p1 = (x1,y1) in
      let cosangle = cos angle in
      let length_factor = if cosangle = 0.0 then one_third else 0.25 /. cosangle
      in
      let angle = if left_or_right then angle else (-. angle) in
      let hx1,hy1 = Vector.translate p0
	(Vector.scal_mul length_factor
	   (Vector.rotate angle 
	      (Vector.of_points p0 p1)))
      in
      let hx2,hy2 = Vector.translate p1
	(Vector.scal_mul length_factor
	   (Vector.rotate (-. angle)
	      (Vector.of_points p1 p0)))
      in
      [|x0;hx1;hx2;x1|],
      [|y0;hy1;hy2;y1|]
    in
    let res,_ = 
      List.fold_left 
	(fun (res,left_or_right) bezier ->
	  ((make_handles left_or_right bezier) :: res,
	   not left_or_right))
	([],true)
	(List.combine beziersx' beziersy')
    in List.rev res


  let rec pre_of_transfo_spec = function
    | BendLeft(angle) -> begin fun paths -> 
      List.map (fun (params,curve) -> if Curve.nb_beziers curve = 1 then
	  begin match curve with | [] -> assert false | (xs,ys) :: _ -> 
	    if Array.length xs = 2 then
	      let x,y = bend ~angle:angle (xs.(0),ys.(0)) (xs.(1),ys.(1)) in
	      (params, [ [| xs.(0) ; x ; xs.(1) |], 
			 [| ys.(0) ; y ; ys.(1) |] ])
	    else
	      (params, curve)
	  end
	else (params, curve))
	paths end
    | BendRight(angle) -> begin fun paths -> 
      List.map (fun (params,curve) -> if Curve.nb_beziers curve = 1 then
	  begin match curve with | [] -> assert false | (xs,ys) :: _ -> 
	    if Array.length xs = 2 then
	      let x,y = bend ~angle:(-. angle) (xs.(0),ys.(0)) (xs.(1),ys.(1)) in
	      (params, [ [| xs.(0) ; x ; xs.(1) |], 
			 [| ys.(0) ; y ; ys.(1) |] ])
	    else
	      (params, curve)
	  end
	else (params, curve))
	paths end
    | Squiggle (freq, amplitude) -> begin fun paths -> 
      List.map (fun (params, curve) -> 
      (params, List.flatten (List.map (squiggle freq amplitude) curve)))
	paths
    end
    | _ -> (fun x -> x)

  let rec post_of_transfo_spec = function
    | Fore margin -> begin fun paths -> 
      let white_paths = List.map (fun (params, curve) -> 
	{ params with 
	  Drivers.strokingColor=Some (Drivers.RGB { Drivers.red=1.;Drivers.green=1.;Drivers.blue=1. }); 
	  Drivers.lineWidth=params.Drivers.lineWidth +. 2. *. margin },
	 curve)
	paths
      in
      let white_paths = post_of_transfo_spec (ShortenS 0.1) white_paths in
      let white_paths = post_of_transfo_spec (ShortenE 0.1) white_paths in
      white_paths @ paths
      end
    | Double (margin,linewidth) -> begin fun paths -> 
      let black_paths = List.map (fun (params, curve) -> 
	{ params with 
	  Drivers.lineWidth = margin +. 2.0 *. linewidth },
	 curve)
	paths
      in
      let white_paths = List.map (fun (params, curve) -> 
	{ params with 
	  Drivers.strokingColor = Some (Drivers.RGB { Drivers.red=1.;Drivers.green=1.;Drivers.blue=1. }); 
	  Drivers.lineWidth = margin },
	 curve)
	paths
      in
      let delta = 0.02 in
      let white_paths = post_of_transfo_spec (ShortenS delta) white_paths in
      let white_paths = post_of_transfo_spec (ShortenE delta) white_paths in
      let black_paths = post_of_transfo_spec (ShortenS delta) black_paths in
      let black_paths = post_of_transfo_spec (ShortenE delta) black_paths in
      black_paths @ white_paths
      end
    | ShortenS a -> begin fun paths -> match paths with
	| [] -> []
	| (params, curve) :: rest -> (params, Curve.internal_restrict 
	  curve (0,a) (Curve.nb_beziers curve - 1, 1.)) :: rest
    end
    | ShortenE a -> begin fun paths -> 
      let rev_paths, (params, curve) = list_split_last_rev paths in
      List.rev ((params, Curve.internal_restrict curve (0,0.) (Curve.nb_beziers curve - 1, 1. -. a)) :: rev_paths)
    end
    | SquiggleFromTo (freq,amplitude,a,b) -> begin fun paths -> 
      List.map (fun (params, curve) -> 
	let curve1,curve,curve2 = Curve.split2 curve a b in
	(params, curve1 @ (List.flatten (List.map (squiggle freq amplitude) curve)) @ curve2))
	paths
    end
    | _ -> (fun x -> x)

  let clip curve node1 node2 = 
    let start = begin
      match Curve.latest_intersection curve node1.Node.curve with
	| None -> begin
	  Printf.fprintf stderr
	    "I can't find any intersection of your edge with the start node shape.\nI'm taking the center as a start node instead.\n" ;
	  (0,0.)
	end
	| Some (i, t1) -> (i, t1)
    end in
    let (j,t') as finish = begin 
      match Curve.earliest_intersection curve node2.Node.curve with
	| None -> begin
	  Printf.fprintf stderr
	    "I can't find any intersection of your edge with the end node shape.\nI'm taking the center as a end node instead.\n" ;
	  ((Curve.nb_beziers curve) - 1,1.)
	end
	| Some (j, t2) -> (j, t2)
    end in
    Curve.internal_restrict curve start finish 

  let make env spec node1 node2 =
    let parameters = spec.parameters_spec in
    let controls = spec.controls in
    let head = spec.head_spec in
    let tail = spec.tail_spec in
    let labels = spec.label_specs in
    let underlying_curve = (Curve.of_point_lists [(Node.center node1) :: (controls @ [Node.center node2])]) in
    let curves = List.fold_left
      (fun curves transfo_spec -> (pre_of_transfo_spec transfo_spec curves))
      [parameters, underlying_curve]
      spec.transfo_specs
    in    
    let curves = List.map (fun (params, curve) -> (params, clip curve node1 node2)) curves in
    let curves = List.fold_left 
      (fun curves transfo_spec -> (post_of_transfo_spec transfo_spec curves))
      curves
      spec.transfo_specs
    in
    match curves with
      | [] -> Printf.fprintf stderr
	("Warning: one of your edge transformers has deleted all curves.\n") ;
	{ curves = [parameters, underlying_curve] ;
	  head = head ~parameters:parameters underlying_curve ; 
	  tail = tail ~parameters:parameters underlying_curve ;
	  labels = (List.map (label_of_spec env underlying_curve) labels) 
	}
      | (params, curve) :: _ ->
	{ curves = curves ;
	  head = head ~parameters:params curve ; 
	  tail = tail ~parameters:params curve ;
	  labels = (List.map (label_of_spec env curve) labels) 
	}

  let draw edge =
    (List.flatten (List.map Node.draw edge.labels))
    @ ArrowTip.draw edge.head 
    @ ArrowTip.draw edge.tail 
    @ (List.flatten
       (List.map 
	  (fun (params, curve) -> Curve.draw ~parameters:params curve) edge.curves)) 


    let make_draw env l spec node1 node2 =
      let edge = make env spec node1 node2 
      in 
      edge, (l @ (draw edge))

end

module Diagram = struct

  (* Types for constructed figures *)
  type figure = contents list		(* The figure ultimately is just a list of contents *)

  (* A figure is constructed by constructing graphical entities
     (type gentity), which, as a side effect, produces contents. *)
  (* During construction of the figure, we need to be able to place
     new gentities relatively to old ones. So we keep track of the
     created gentities in the following form. *)
  type anchor = [ `Angle of float 	(* In degrees *)
	       | `North
	       | `South
	       | `NorthEast
	       | `SouthEast
	       | `NorthWest
	       | `SouthWest
	       | `West
	       | `East
	       | `Center
	       | `Vec of Vector.t
	       | `Curvilinear of float	(* Between 0. and 1. (for paths) *)
	       | `Temporal of float	(* Between 0. and 1. (for paths) *)
	       ]
  type gentity = { curve : Curve.t ;	(* The curve is used to determine the start and end of edges *)
		   anchor : anchor -> Point.t } (* Anchors are used for relative placement *)

  (* Casting points into gentities *)
  let coord (x : Point.t) = 
    { curve = Curve.of_point_lists [[x]] ;
      anchor = fun a -> x }

  (* Two important ways of constructing gentities are nodes and edges between them.  *)

  (* Types for style instructions passed as arguments for constructing nodes *)
  type node_shape = [ `Rectangle ]
  type style = [ `At of Point.t
  	       | `Anchor of anchor
  	       | `InnerSep of float
  	       | `OuterSep of float
  	       | `TextDepth of float
  	       | `TextHeight of float
  	       | `TextWidth of float
  	       | `Draw 
  	       | `Fill
  	       | `DrawOf of path_parameters
  	       | `Shape of node_shape
  	       ]
  type style_instructions = style list

  module Style = struct
    (* Find a coordinate specification in a list of style instructions *)
    let rec at_rec res style_instructions = match style_instructions with
      | [] -> res
      | `At (x,y) :: rest -> at_rec (x,y) rest
      | _ :: rest -> at_rec res rest
    and at style_instructions = at_rec (0.,0.) style_instructions

  (* Find a shape specification in a list of style instructions *)
    let rec shape_rec res style_instructions = match style_instructions with
      | [] -> res
      | `Rectangle :: rest -> shape_rec `Rectangle rest
      | _ :: rest -> shape_rec res rest
    and shape style_instructions = shape_rec `Rectangle style_instructions

  (* Find path parameters specification in a list of style instructions *)
    let rec parameters_rec res style_instructions = match style_instructions with
      | [] -> res
      | `Draw :: rest -> parameters_rec { res with strokingColor = Some black } rest
      | `Fill :: rest -> parameters_rec { res with fillColor = Some black } rest
      | `DrawOf params :: rest -> parameters_rec params rest
      | `Dashed :: rest -> parameters_rec { res with dashPattern = [0.1;0.2] } rest
      | _ :: rest -> parameters_rec res rest
    and parameters style_instructions = parameters_rec 
      { default with close = true ; strokingColor=None ; lineWidth = 0.1 } 
      style_instructions
    and path_parameters style_instructions = parameters_rec 
      { default with close = false ; strokingColor=None ; lineWidth = 0.1 } 
      style_instructions

  (* Find outer_sep specification in a list of style instructions *)
    let rec outer_sep_rec res style_instructions = match style_instructions with
      | [] -> res
      | `OuterSep h :: rest -> outer_sep_rec h rest
      | _ :: rest -> outer_sep_rec res rest
    and outer_sep style_instructions = outer_sep_rec 0. style_instructions

  (* Find inner_sep specification in a list of style instructions *)
    let rec inner_sep_rec res style_instructions = match style_instructions with
      | [] -> res
      | `InnerSep h :: rest -> inner_sep_rec h rest
      | _ :: rest -> inner_sep_rec res rest
    and inner_sep style_instructions = inner_sep_rec 0.5 style_instructions

  (* Find line width specification in a list of style instructions *)
    let line_width style_instructions = 
      let params = parameters style_instructions in
      params.lineWidth

  (* Find anchor specification in a list of style instructions *)
    let rec anchor_rec res style_instructions = match style_instructions with
      | [] -> res
      | `Anchor h :: rest -> anchor_rec h rest
      | _ :: rest -> anchor_rec res rest
    and anchor style_instructions = anchor_rec `Center style_instructions

  (* Find arrow head specification in a list of style instructions *)
    let rec head_rec res style_instructions = match style_instructions with
      | [] -> res
      | `Head h :: rest -> head_rec h rest
      | _ :: rest -> head_rec res rest
    and head style_instructions = head_rec `None style_instructions
      

  (* Find matrix placement specification in a list of style instructions *)
    let rec matrix_placement_rec res style_instructions = match style_instructions with
      | [] -> res
      | (`Centers h) as pl :: rest -> matrix_placement_rec pl rest
      | _ :: rest -> matrix_placement_rec res rest
    and matrix_placement style_instructions = matrix_placement_rec (`Centers 1.) style_instructions
  end

    module BB = struct 

      let points  (x0,y0,x1,y1) =
	let p1 = (x0,y0) in
	let p2 = (x1,y0) in
	let p3 = (x1,y1) in
	let p4 = (x0,y1) in
	(p1,p2,p3,p4)
	
      let outer_points style (x0,y0,x1,y1) =
	let inner_sep = Style.inner_sep style in
	let outer_sep = Style.outer_sep style in
	let line_width = Style.line_width style in
	let sep = inner_sep +. outer_sep +. line_width in
	let x0 = x0 -. sep in
	let y0 = y0 -. sep in
	let x1 = x1 +. sep in
	let y1 = y1 +. sep in
	points (x0,y0,x1,y1)

      let mid_points style (x0,y0,x1,y1) =
	let inner_sep = Style.inner_sep style in
	let line_width = Style.line_width style in
	let sep = inner_sep +. 0.5 *. line_width in
	let x0 = x0 -. sep in
	let y0 = y0 -. sep in
	let x1 = x1 +. sep in
	let y1 = y1 +. sep in
	points (x0,y0,x1,y1)

      let center (x0,y0,x1,y1) =
	let x1' = (x1 -. x0) *. 0.5 in
	let x0' = -. x1' in
	let y0' = (y0 -. y1) *. 0.5 in
	let y1' = -. y0' in
	(x0',y0',x1',y1') 

    end

    let anchor style bb = 	
      let (p1,p2,p3,p4) = BB.outer_points style bb in
      match Style.shape style with
	| `Rectangle -> begin function
	    | `Vec v -> Vector.(+) (Point.middle p1 p3) v
	    | `Center -> Point.middle p1 p3
	    | `East -> Point.middle p2 p3  
	    | `West -> Point.middle p1 p4  
	    | `North -> Point.middle p3 p4
	    | `South -> Point.middle p1 p2
	    | `SouthWest -> p1
	    | `SouthEast -> p2
	    | `NorthEast -> p3
	    | `NorthWest -> p4
	    | _ -> Printf.fprintf stderr "Anchor undefined for a rectangle. Returning the center instead.\n" ; 
	      Point.middle p1 p3
	end
	| `Circle -> begin function
	    | _ -> Printf.fprintf stderr "Anchor undefined for a circle. Returning the center instead.\n" ; 
	      Point.middle p1 p3
	end 

    type mid_or_out = Outer | Mid 
	
    let curve mid_or_out style bb = 	
      let (p1,p2,p3,p4) = match mid_or_out with
	| Mid -> BB.mid_points style bb 
	| Outer -> BB.outer_points style bb 
      in
      match Style.shape style with
	| `Rectangle -> begin
	  Curve.of_point_lists [
	    [p1;p2] ;	(* The bottom curve *)
	    [p2;p3] ;	(* The right-hand curve *)
	    [p3;p4] ;	(* The top curve *)
	    [p4;p1] 	(* The left-hand curve *)
	  ]
	end
	| `Circle -> begin 
	  let control p1 p2 =
	    Vector.translate
	      (Point.middle p1 p2)
	      (Vector.scal_mul 0.5 (Vector.rotate (-. half_pi) (Vector.of_points p1 p2)));
	  in
	  Curve.of_point_lists [
	    [p1; control p1 p2; p2] ;	(* The bottom curve *)
	    [p2; control p2 p3; p3] ;	(* The right-hand curve *)
	    [p3; control p3 p4; p4] ;	(* The top curve *)
	    [p4; control p4 p1; p1] (* The left-hand curve *)
	  ] 
	end 

    module Edge = struct

      let rec transfo curves = function
	| `Squiggle (freq,amplitude,a,b) -> 
	  List.map (fun (params, curve) -> 
	    let curve1,curve,curve2 = Curve.split2 curve a b in
	    (params, curve1 @ (List.flatten (List.map (Edge.squiggle freq amplitude) curve)) @ curve2))
	    curves
	| `BendLeft(angle) -> 
	  List.map (fun (params,curve) -> if Curve.nb_beziers curve = 1 then
	      begin match curve with | [] -> assert false | (xs,ys) :: _ -> 
		if Array.length xs = 2 then
		  let x,y = Edge.bend ~angle:angle (xs.(0),ys.(0)) (xs.(1),ys.(1)) in
		  (params, [ [| xs.(0) ; x ; xs.(1) |], 
			     [| ys.(0) ; y ; ys.(1) |] ])
		else
		  (params, curve)
	      end
	    else (params, curve))
	    curves
	| `BendRight(angle) -> 
	  List.map (fun (params,curve) -> if Curve.nb_beziers curve = 1 then
	      begin match curve with | [] -> assert false | (xs,ys) :: _ -> 
		if Array.length xs = 2 then
		  let x,y = Edge.bend ~angle:(-. angle) (xs.(0),ys.(0)) (xs.(1),ys.(1)) in
		  (params, [ [| xs.(0) ; x ; xs.(1) |], 
			     [| ys.(0) ; y ; ys.(1) |] ])
		else
		  (params, curve)
	      end
	    else (params, curve))
	    curves
	| `Fore margin -> 
	  let white_paths = List.map (fun (params, curve) -> 
	    { params with 
	      Drivers.strokingColor=Some (Drivers.RGB { Drivers.red=1.;Drivers.green=1.;Drivers.blue=1. }); 
	      Drivers.lineWidth=params.Drivers.lineWidth +. 2. *. margin },
	    curve)
	    curves
	  in
	  let white_paths = transfo white_paths (`Shorten (0.1,0.1))  in
	  white_paths @ curves
	| `Double margin -> 
	  let black_paths = List.map (fun (params, curve) -> 
	    (* begin match params.strokingColor with *)
	    (*   | None -> Printf.fprintf stderr "Couleur: none.\n"  *)
	    (*   | Some (RGB color) -> Printf.fprintf stderr "Couleur r = %f, g = %f, b = %f.\n"  *)
	    (* 	color.red *)
	    (* 	color.green *)
	    (* 	color.blue end ; *)
	    (* Printf.fprintf stderr "Largeur: %f.\n" params.lineWidth ; *)
	    { params with 
	      (* strokingColor = Some (RGB { red=0.;green=1.;blue=0. });  *)
	      lineWidth = margin +. 2.0 *. params.lineWidth },
	    curve)
	    curves
	  in
	  let white_paths = List.map (fun (params, curve) -> 
	    { params with 
	      strokingColor = Some (RGB { red=1.;green=1.;blue=1. }); 
	      lineWidth = margin },
	    curve)
	    curves
	  in
	  (* let delta = 0.02 in *)
	  (* let white_paths = transfo white_paths (`Shorten (delta,delta)) in *)
	  (* let black_paths = transfo black_paths (`Shorten (delta,delta)) in *)
	  black_paths @ white_paths
	| `ShortenS a -> transfo curves (`Sorten (a,1.)) 
	| `ShortenE b -> transfo curves (`Sorten (0.,b)) 
	| `Shorten (a,b) -> begin match curves with
	    | [] -> []
	    | (params, curve) :: rest -> 
	      (params, Curve.restrict curve a b) :: rest
	end
	| _ -> curves

      let draw_curve style curve = 
	let curve, head_curves = 
	  match Style.head style with
	  | `To -> 
	    curve, [Style.path_parameters style, (ArrowTip.simple curve).ArrowTip.curve]
	  | `None -> curve, []
	  | _ -> Printf.fprintf stderr "Arrow head not yet implemented.\n" ;
	    curve, []
	in
	let curves = List.fold_left
	transfo [(Style.path_parameters style, curve)] style
	in
	head_curves @ curves

      let outer_curve style curve = curve
    end

    (* Place contents according to style, assuming its anchor zero is currently at (0,0). *)
    (* Returns the translation vector and the translated contents *)
    let place style contents zero = 
      let v = Style.at style in
      let bb_boot =  BB.center (OutputCommon.bounding_box contents) in
      let a = (Style.anchor style) in
      let x,y = 
	Vector.(+) v
	  (Vector.of_points
	  (anchor style bb_boot a)
	  (anchor style bb_boot zero)) 
      in
      (x,y), List.map (OutputCommon.translate x y) contents 

    let node env style contents =
    let at = Style.at style in
    let a = Style.anchor style in
    let parameters = Style.parameters style in
    (* Compute the contents a first time to get a bounding box of the right size *)
    let contents = (Boxes.draw_boxes (boxify_scoped env contents)) in
    let v, contents = place style contents `SouthWest in
    (* let bb_boot =  BB.center (OutputCommon.bounding_box contents) in *)
    (* (\* Use this to compute the real coordinates, by translating "at" according to anchor. *\) *)
    (* let x,y = Vector.(-) (Vector.(+) at (anchor style bb_boot `SouthWest)) (anchor style bb_boot a) in *)
    (* (\* let x,y = Vector.translate (anchor style bb_boot a) at in *\) *)
    (* (\* (\\* Now translate the contents by x,y and compute the real bounding box, curve, etc *\\) *\) *)
    (* (\* let x',y' = Vector.(-) (x,y) (anchor style bb_boot `SouthWest) in *\) *)
    (* let contents = List.map (OutputCommon.translate x y) contents in *)
    let bb =  OutputCommon.bounding_box contents in 
    let anchor a = (anchor style bb a) in
    let outer_curve = curve Outer style bb in
    let mid_curve = curve Mid style bb in
    ({ anchor = anchor ;
      curve = outer_curve },
     contents @ (Curve.draw ~parameters:parameters mid_curve))

  type continue_path_spec = Point.t list 
  and path_spec = continue_path_spec list 

  let point_lists_of_path_spec s continues =
    let rec point_lists_of_path_spec_rec res s continues = 
      match continues with
	| [] -> List.rev res
	| e :: rest -> point_lists_of_path_spec_rec ((s :: e) :: res) (list_last e) rest
    in
    point_lists_of_path_spec_rec [] s continues

  let path style s continues = 		
    let curve = (Curve.of_point_lists (point_lists_of_path_spec s continues)) in
    let parameters = Style.path_parameters style in
    let anchor = function
      | `Temporal pos -> Curve.eval curve pos
      | `Center -> Curve.eval curve 0.5
      | _ -> Printf.fprintf stderr "Anchor undefined for a path. Returning the center instead.\n" ; 
	Curve.eval curve 0.5
    in
    ({ anchor = anchor ; curve = curve },
     Curve.draw ~parameters:parameters curve)

  type controls = continue_path_spec * Point.t list (* The last list is treated differently: *)
  (* its endpoint should be provided as a node, see edge below. *)

  let point_lists_of_edge_spec s continues e =
    let rec point_lists_of_edge_spec_rec res s e continues = 
      match continues with
	| [] -> [[s;e]]
	| [ controls ] -> List.rev (((s :: controls) @ [e]) :: res)
	| controls :: rest -> point_lists_of_edge_spec_rec ((s :: controls) :: res) (list_last controls) e rest
    in
    point_lists_of_edge_spec_rec [] (s.anchor `Center) (e.anchor `Center) continues

  let clip curve node1 node2 = 
    let start = begin
      match Curve.latest_intersection curve node1.curve with
	| None -> begin
	  Printf.fprintf stderr
	    "I can't find any intersection of your edge with the start node shape.\nI'm taking the center as a start node instead.\n" ;
	  (0,0.)
	end
	| Some (i, t1) -> (i, t1)
    end in
    let (j,t') as finish = begin 
      match Curve.earliest_intersection curve node2.curve with
	| None -> begin
	  Printf.fprintf stderr
	    "I can't find any intersection of your edge with the end node shape.\nI'm taking the center as a end node instead.\n" ;
	  ((Curve.nb_beziers curve) - 1,1.)
	end
	| Some (j, t2) -> (j, t2)
    end in
    Curve.internal_restrict curve start finish 

  let edge style s controls e =
    let underlying_curve = (Curve.of_point_lists 
			      (point_lists_of_edge_spec s controls e)) in
    let curve = clip underlying_curve s e in
    let outer_curve = Edge.outer_curve style curve in
    let draw_curve = Edge.draw_curve style curve in
    let anchor = function
      | `Temporal pos -> Curve.eval curve pos
      | `Center -> Curve.eval curve 0.5
      | _ -> Printf.fprintf stderr "Anchor undefined for a path. Returning the center instead.\n" ; 
	Curve.eval curve 0.5
    in
    ({ anchor = anchor ; curve = outer_curve },
     List.flatten (List.map (fun (params,curve) -> (Curve.draw ~parameters:params curve))
		     draw_curve))

  let translate v node =
    { curve = Curve.translate v node.curve ;
      anchor = fun a -> Vector.(+) (node.anchor a) v }
      
  let matrix env style lines =
    let width = List.fold_left
      (fun res line -> max res (List.length line))
      0
      lines
    in
    let height = List.length lines in
    (* let bboxes = List.map (fun line -> List.map (fun contents -> OutputCommon.bounding_box contents)) in *)
    (* On commence par placer les noeuds de sorte que le noeud (0,0) ait son centre en (0,0) *)
    let placement = match Style.matrix_placement style with
      | `Centers dist -> begin fun i j -> 
	  let (x,y) as translation = (float_of_int j *. 10.), -. (float_of_int i *. 10.) in 
	  Printf.fprintf stderr "Translation %d,%d: %f, %f.\n" i j x y ;
	[`At (translation) ; `Anchor `Center] end
      | _ -> Printf.fprintf stderr "Matrix placement specification not yet implemented. Using `Centers 10.\n";
	begin fun i j -> 
	  let (x,y) as translation = (float_of_int j *. 10.), -. (float_of_int i *. 10.) in 
	  Printf.fprintf stderr "Translation %d,%d: %f, %f.\n" i j x y ;
	  [`At (translation) ; `Anchor (`Center)] end
    in
    let res = Array.make_matrix height width (fst (node env [] [])) in
    let i = ref 0 in
    let j = ref 0 in
    let state = ref [] in
    List.iter (fun line -> 
      List.iter (fun (style', contents) -> 
	let mij,stateij = node env (style @ (placement !i !j) @ style') contents in
	res.(!i).(!j) <- mij ;
	Printf.fprintf stderr "Indices: %d, %d.\n" !i !j ;	
	state := stateij @ !state ;
	incr j)
	line ;
      incr i ; j := 0)
      lines ;
    (* Now we have an array res of nodes and a list states of pdf instructions to draw them. 
       We translate this to match the constraint placement of the matrix. *)
    let contents = !state in
    let x0,y0,x1,y1 = OutputCommon.bounding_box contents in
    let zero = `Vec (Vector.of_points (Point.middle (x0,y0) (x1,y1)) (0.,0.)) in
    let (x,y) as v, contents = place style contents zero in
    Printf.fprintf stderr "Translation: %f, %f.\n" x y ;
    let res' = Array.map (Array.map (translate v)) res in
    res', contents

end


  (* class type style = object *)
  (*   method at : Point.t *)
  (*   method anchor : anchor *)
  (*   method innersep : float *)
  (*   method outersep : float *)
  (*   method textdepth : float *)
  (*   method textheight : float *)
  (*   method textwidth : float *)
  (*   method draw : bool *)
  (*   method drawof : path_parameters *)
  (*   method fillof : path_parameters *)
  (*   method shape : node_shape *)
  (* end *)

  (* class type node = object  *)
  (*   method curve : Curve.t  *)
  (*   method center : Point.t *)
  (*   method base : Point.t *)
  (*   method north : Point.t *)
  (*   method south : Point.t *)
  (*   method west : Point.t *)
  (*   method east : Point.t *)
  (*   method angle : float -> Point.t *)
  (* end *)

  (* class rectangle env ?style:(style_instructions=[]) contents = *)
  (*   let at = style_instructions#at in *)
  (*   (\* let shape = style_instructions#shape in *\) *)
  (*   (\* Compute the contents a first time to get a bounding box of the right size *\) *)
  (*   let contents = (Util.draw_boxes (boxify_scoped env contents)) in *)
  (*   let bb_boot =  OutputCommon.bounding_box contents in *)
  (*   (\* Use this to compute the real coordinates, by translating "at" according to anchor. *\) *)
  (*   (\* But computing anchor needs make_anchor bb.  *\) *)
  (*   let anchor_to_vect = function *)
  (*   let x,y = Vector.translate (Vector.minus (shape.NodeShape.make_anchor bb_boot anchor)) at in *)
  (*   (\* Now translate the contents by x,y and compute the real bounding box, curve, etc *\) *)
  (*   let contents = List.map (OutputCommon.translate x y) contents in *)
  (*   let bb =  OutputCommon.bounding_box contents in () *)
