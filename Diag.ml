open Typography

let pi = 3.14159
let half_pi = pi /. 2.
let rec list_last = function
  [] -> assert false
  | [x] -> x
  | x :: l -> list_last l

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

module Curve = struct
  type t =  Bezier.curve list
  let bezier_of_point_list l = 
    let xs, ys = List.split l in
    (Array.of_list xs, Array.of_list ys)
  let nb_beziers curve = List.length curve
  let of_point_lists l = List.map bezier_of_point_list l
  let of_contents = function
    | Drivers.Path (_, beziers) -> List.map Array.to_list beziers
    | _ -> failwith "Attempt to convert a non-path value of type Drivers.content to Curve.t."
  let draw
    ?parameters:(parameters=Drivers.default)
    (curve:t) =
    if curve = [] then [] else
      [Drivers.Path (parameters, [Array.of_list curve])]

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
	| [] -> Printf.printf ("Warning: evaluating an empty curve. Using (0.,0.)") ; (0.,0.)
	| bezier :: reste -> 
	  if t <= 1. then bezier_evaluate bezier t
	  else eval_rec reste (t -. 1.)
    in eval_rec beziers t

  let restrict curve (i,t) (j,t') = 

    let rec restrict_right res curve (j,t') = match  curve with
	[] -> Printf.printf ("Warning: attempt to restrict an empty curve.\n") ; []
      | (xs,ys) as bezier :: rest -> if j = 0 then
	  let xs' = Bezier.restrict xs 0. t' in
	  let ys' = Bezier.restrict ys 0. t' in
	  List.rev ((xs',ys') :: res)
	else restrict_right (bezier :: res) rest (j-1,t')
    in

    let rec restrict_left curve (i,t) (j,t') = match curve with
	[] -> Printf.printf ("Warning: attempt to restrict an empty curve.\n") ; []
      | (xs,ys) :: rest -> if i = 0 then
	  let xs' = Bezier.restrict xs t 1. in
	  let ys' = Bezier.restrict ys t 1. in
	  restrict_right [] ((xs',ys') :: rest) (j,t')
	else
	  restrict_left rest (i-1,t) (j-1,t')
    in
    restrict_left curve (i,t) (j,t')

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
  (* let draw ?parameters:(parameters=Drivers.default) r:t =  *)
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
  type t = { parameters : Drivers.path_parameters ; 
	     curve : Curve.t }

  let none = { parameters = Drivers.default ; curve = Curve.of_point_lists [] }

  let simple ?size:(size=6.0)
      ?angle:(angle=0.4)
      ?bend:(bend=2.)
      ?parameters:(parameters=Drivers.default) 
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
  type t = { parameters : Drivers.path_parameters ; 
	     make_curve : bb -> Curve.t ;
	     make_anchor : bb -> Anchor.spec -> Anchor.t }

  let default = { Drivers.default with Drivers.close = true ; Drivers.lineWidth=0.5 } 
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
      match Rectangle.points (Rectangle.make x0 y0 x1 y1) with
	| ((x1,y1) as p1) :: ((x2,y2) as p2) :: ((x3,y3) as p3) :: ((x4,y4) as p4) :: [] ->
	  (p1,p2,p3,p4)
	| _ -> assert false
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
      (* let _ = Printf.printf ("Rectangle: (%f,%f), (%f,%f)") x0 y0 x1 y1 in *)
      function
	| Anchor.Vec v -> v
	| Anchor.Center -> Point.middle p1 p3
	| Anchor.East -> Point.(+) (Point.middle p2 p3)  (0.5 *. parameters.Drivers.lineWidth, 0.0)
	| Anchor.West -> Point.(-) (Point.middle p1 p4)  (0.5 *. parameters.Drivers.lineWidth, 0.0)
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

end

module Node = struct
  type t = { curve : Curve.t ;
	     parameters : Drivers.path_parameters ;
	     make_anchor : Anchor.spec -> Point.t ;
	     contents : Drivers.contents list }

  let center node = node.make_anchor Anchor.Center

  type spec = { at : Point.t ;
		contents_spec : (Typography.user Typography.content) list ;
		shape : NodeShape.t ;
		anchor: Anchor.spec }
  let default = { 
    at = (0.,0.) ;
    contents_spec = [] ;
    shape = NodeShape.rectangle ~inner_sep:NodeShape.default_inner_sep 
      ~parameters:NodeShape.default () ;
      anchor = Anchor.Center }

  let make spec =
    let shape = spec.shape in
    let anchor = spec.anchor in
    let at = spec.at in
    (* Compute the contents a first time to get a bounding box of the right size *)
    let contents = (Util.draw_boxes (boxify { defaultEnv with size = 4. } spec.contents_spec)) in
    let bb_boot =  Drivers.bounding_box contents in
    (* Use this to compute the real coordinates, by translating "at" according to anchor. *)
    (* But computing anchor needs make_anchor bb.  *)
    let x,y = Vector.translate (Vector.minus (shape.NodeShape.make_anchor bb_boot anchor)) at in
    (* Now translate the contents by x,y and compute the real bounding box, curve, etc *)
    let contents = List.map (Drivers.translate x y) contents in
    let bb =  Drivers.bounding_box contents in
    let curve = shape.NodeShape.make_curve bb in
    { curve = curve ; 
      parameters = shape.NodeShape.parameters ;
      contents =  contents ; 
      make_anchor = fun anchor_spec -> (shape.NodeShape.make_anchor bb anchor_spec) }

  (* En fait il faudra vite passer aussi curve a make_anchor... *)


  let draw : t -> Drivers.contents list = fun node -> 
    let shape_curve = 
      Curve.draw ~parameters:node.parameters node.curve
    in 
    node.contents @ shape_curve

  let make_draw l spec =
    let node = make spec in
    node, (l @ (draw node))
end	    

module Edge = struct


  type label_spec = {
    pos : float ;			(* A float between 0 and 1 *)
    node_spec : Node.spec
  }
  type t = { curve : Curve.t ;
	     head : ArrowTip.t ;
	     tail : ArrowTip.t ;
	     labels : Node.t list ;
	     parameters : Drivers.path_parameters
	   }

  type parameters = { parameters_spec : Drivers.path_parameters ;
		controls : Point.t list ;
		head_spec : ?parameters:Drivers.path_parameters -> Curve.t -> ArrowTip.t ;
		tail_spec : ?parameters:Drivers.path_parameters -> Curve.t -> ArrowTip.t ;
		label_specs : label_spec list }

  let default = {
    parameters_spec = Drivers.default ;
    controls = [] ;
    head_spec = (fun ?parameters:(parameters = Drivers.default) _ -> ArrowTip.none) ;
    tail_spec = (fun ?parameters:(parameters = Drivers.default) _ -> ArrowTip.none) ;
    label_specs = [] ;
  }

  let label_of_spec curve { pos = pos ; node_spec = spec } =
    Node.make { spec with Node.at = Curve.eval curve pos }

  let make spec node1 node2 =
    let parameters = spec.parameters_spec in
    let controls = spec.controls in
    let head = spec.head_spec in
    let tail = spec.tail_spec in
    let labels = spec.label_specs in
    let underlying_curve = (Curve.of_point_lists [(Node.center node1) :: (controls @ [Node.center node2])]) in
    let start = begin
      match Curve.latest_intersection underlying_curve node1.Node.curve with
	| None -> begin
	  Printf.printf
	    "I can't find any intersection of your edge with the start node shape.\nI'm taking the center as a start node instead.\n" ;
	  (0,0.)
	end
	| Some (i, t1) -> (i, t1)
    end in
    let finish = begin 
      match Curve.earliest_intersection underlying_curve node2.Node.curve with
	| None -> begin
	  Printf.printf
	    "I can't find any intersection of your edge with the end node shape.\nI'm taking the center as a end node instead.\n" ;
	  ((Curve.nb_beziers underlying_curve) - 1,1.)
	end
	| Some (j, t2) -> (j, t2)
    end in
    let curve = Curve.restrict underlying_curve start finish in
    { curve = curve ;
      head = head ~parameters:parameters curve ; 
      tail = tail ~parameters:parameters curve ;
      labels = (List.map (label_of_spec curve) labels) ;
      parameters = parameters }

  let draw edge =
    Curve.draw ~parameters:edge.parameters edge.curve @
      ArrowTip.draw edge.head @
      ArrowTip.draw edge.tail @
      (List.flatten (List.map Node.draw edge.labels))

    let make_draw l spec node1 node2 =
      let edge = make spec node1 node2 
      in 
      edge, (l @ (draw edge))



  let bend_left ?angle:(angle=30.) node1 node2 = 
    let node1 = Node.center node1 in
    let node2 = Node.center node2 in
    let angle = (-. angle) *. pi /. 180. in
    let vec = Vector.scal_mul (0.5 /. (cos angle)) (Vector.of_points node1 node2) in
    let vec = Vector.rotate (angle) vec in
    [Vector.translate node1 vec]
  let bend_right ?angle:(angle=30.) node1 node2 = 
    let node1 = Node.center node1 in
    let node2 = Node.center node2 in
    let angle = angle *. pi /. 180. in
    let vec = Vector.scal_mul (0.5 /. (cos angle)) (Vector.of_points node1 node2) in
    let vec = Vector.rotate (angle) vec in
    [Vector.translate node1 vec]

end
