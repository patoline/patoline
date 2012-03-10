
let pi = 3.14159
let half_pi = pi /. 2.
let rec list_last = function
  [] -> assert false
  | [x] -> x
  | x :: l -> list_last l

module Point = struct 
  type t = float * float
  let proj (x,y) = x
  let proj' (x,y) = y
  let middle (x0,y0) (x1,y1) = (0.5 *. (x0 +. x1), 0.5 *. (y0 +. y1))
  let distance (x0,y0) (x1,y1) = sqrt ((x1 -. x0)**2. +. (y1 -. y0)**2.)
  let (+) (x0,y0) (x1,y1) = (x0 +. x1), (y0 +. y1)
  let (-) (x0,y0) (x1,y1) = (x0 -. x1), (y0 -. y1)
  let (/) (x0,y0) r = (x0 /. r, y0 /.r)
end

open Point

module Curve = struct
  type t =  Bezier.curve list
  let bezier_of_point_list l = 
    let xs, ys = List.split l in
    (Array.of_list xs, Array.of_list ys)
  let of_point_lists l = List.map bezier_of_point_list l
  let of_contents = function
    | Drivers.Path (_, beziers) -> List.map Array.to_list beziers
    | _ -> failwith "Attempt to convert a non-path value of type Drivers.content to Curve.t."
  let draw
    ?parameters:(parameters=Drivers.default)
    (curve:t) =
      Drivers.Path (parameters, [Array.of_list curve])
  let intersections bezier1 beziers2 = 
    let res, _ = List.split(List.flatten (List.map (Bezier.intersect bezier1) beziers2)) in
    List.map (fun t -> (t,bezier1)) res
  let intersections beziers1 beziers2 = 
    List.flatten (List.map (fun bezier1 -> intersections bezier1 beziers2) beziers1)
  let latest_intersection beziers1 beziers2 =
    let inters = (intersections beziers1 beziers2) in
    let latest = List.fold_left 
      (fun yet ((t,bezier) as candidate) -> match yet with
	| None -> Some candidate
	| Some (t',bezier') -> Some (t,bezier))
      None
      inters
    in latest
  let earliest_intersection beziers1 beziers2 =
    match (intersections beziers1 beziers2) with
      | [] -> None
      | (t,bezier) :: _ -> Some (t,bezier)
  let bezier_evaluate (xs,ys) t = 
    (Bezier.eval xs t, Bezier.eval ys t)
end

module Vector = struct
  type t = Point.t 
  let of_points p q = ((proj q -. proj p),
		       (proj' q -. proj' p))
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

  let simple path ?size:(size=6.0) ?angle:(angle=0.4) ?bend:(bend=2.) () =
    match path with
      | Drivers.Path (params, curve::_) -> 
	let curve = Array.to_list curve in 
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
	Curve.draw ~parameters:params (Curve.of_point_lists [
	  [r;mr;p] ;
	  [p;ml;l]
	])
      | _ -> assert false    
end

module NodeShape = struct
  type t = { parameters : Drivers.path_parameters ; 
	     curve : Curve.t }

  let default = { Drivers.default with Drivers.close = true } 
  let default_inner_sep = 0.2

  let rectangle ?inner_sep:(inner_sep = default_inner_sep) 
      ?parameters:(parameters={ Drivers.default with Drivers.close = true}) contents = 
    let (x0,y0,x1,y1) = Drivers.bounding_box contents in
    let x0 = x0 -. inner_sep in
    let y0 = y0 -. inner_sep in
    let x1 = x1 +. inner_sep in
    let y1 = y1 +. inner_sep in
    (* let x_center, y_center = middle (x0,y0)( x1,y1) in *)
    (* let radius = 0.5 *. (distance (x0,y0) (x1,y1)) in *)
    match Rectangle.points (Rectangle.make x0 y0 x1 y1) with
      | ((x1,y1) as p1) :: ((x2,y2) as p2) :: ((x3,y3) as p3) :: ((x4,y4) as p4) :: [] ->
	{ parameters = parameters ;
	  curve = Curve.of_point_lists [
	  [p1;p2] ;	(* The bottom curve *)
	  [p2;p3] ;	(* The right-hand curve *)
	  [p3;p4] ;	(* The top curve *)
	  [p4;p1] 	(* The left-hand curve *)
	] }
      | _ -> assert false

  let circle  ?inner_sep:(inner_sep = default_inner_sep) 
      ?parameters:(parameters={ Drivers.default with Drivers.close = true}) contents = 
    let (x0,y0,x1,y1) = Drivers.bounding_box contents in
    let x0 = x0 -. inner_sep in
    let y0 = y0 -. inner_sep in
    let x1 = x1 +. inner_sep in
    let y1 = y1 +. inner_sep in
    (* let x_center, y_center = middle (x0,y0)( x1,y1) in *)
    (* let radius = 0.5 *. (distance (x0,y0) (x1,y1)) in *)
    match Rectangle.points (Rectangle.make x0 y0 x1 y1) with
      | ((x1,y1) as p1) :: ((x2,y2) as p2) :: ((x3,y3) as p3) :: ((x4,y4) as p4) :: [] ->
	let control p1 p2 =
	   Vector.translate
	     (Point.middle p1 p2)
	     (Vector.scal_mul 0.5 (Vector.rotate (-. half_pi) (Vector.of_points p1 p2)));
	in
	{ parameters = parameters ;
	  curve = Curve.of_point_lists [
	  [p1; control p1 p2; p2] ;	(* The bottom curve *)
	  [p2; control p2 p3; p3] ;	(* The right-hand curve *)
	  [p3; control p3 p4; p4] ;	(* The top curve *)
	  [p4; control p4 p1; p1] (* The left-hand curve *)
	] }
      | _ -> assert false

  let flower  ?inner_sep:(inner_sep = default_inner_sep) ?amplitude:(amplitude=1.0) 
      ?parameters:(parameters={ Drivers.default with Drivers.close = true}) contents = 
    let (x0,y0,x1,y1) = Drivers.bounding_box contents in
    let x0 = x0 -. inner_sep in
    let y0 = y0 -. inner_sep in
    let x1 = x1 +. inner_sep in
    let y1 = y1 +. inner_sep in
    (* let x_center, y_center = middle (x0,y0)( x1,y1) in *)
    (* let radius = 0.5 *. (distance (x0,y0) (x1,y1)) in *)
    match Rectangle.points (Rectangle.make x0 y0 x1 y1) with
      | ((x1,y1) as p1) :: ((x2,y2) as p2) :: ((x3,y3) as p3) :: ((x4,y4) as p4) :: [] ->
	let control p1 p2 =
	   Vector.translate
	     (Point.middle p1 p2)
	     (Vector.scal_mul amplitude (Vector.rotate (-. half_pi) (Vector.of_points p1 p2)));
	in
	{ parameters = parameters ; 
	  curve = Curve.of_point_lists [
	  [p1; control p1 p2; p2] ;	(* The bottom curve *)
	  [p2; control p2 p3; p3] ;	(* The right-hand curve *)
	  [p3; control p3 p4; p4] ;	(* The top curve *)
	  [p4; control p4 p1; p1] (* The left-hand curve *)
	] }
      | _ -> assert false

end

module Node = struct
  type t = { shape : NodeShape.t ;
	     contents : Drivers.contents list ;
	     center : Point.t }

  let make
      ?contents:(contents = []) 
      ?center:(center = (
	let x0,y0,x1,y1 = Drivers.bounding_box contents in 
	middle (x0,y0) (x1,y1))) 
      ?shape:(shape = fun contents -> NodeShape.rectangle 
	~inner_sep:NodeShape.default_inner_sep 
	~parameters:{ Drivers.default with Drivers.close = true} 
	contents)
      ()
      =
    { shape = shape contents ; 
      contents = contents ; 
      center = center }
  let draw : t -> Drivers.contents list = fun node -> 
    node.contents @ [Curve.draw ~parameters:node.shape.NodeShape.parameters node.shape.NodeShape.curve]
  let edge
      ?parameters:(parameters = Drivers.default)
      node1
      ?controls:(controls = [])
      node2 = 
    let underlying_curve = (Curve.of_point_lists [node1.center :: (controls @ [node2.center])]) in
    let start = begin
      match Curve.latest_intersection underlying_curve node1.shape.NodeShape.curve with
	| None -> begin
	  Printf.printf
	    "I can't find any intersection of your edge with the start node shape.\nI'm taking the center as a start node instead.\n" ;
	  node1.center
	end
	| Some (t1, bezier1) -> Curve.bezier_evaluate bezier1 t1 
    end in
    let finish = begin 
      match Curve.earliest_intersection underlying_curve node2.shape.NodeShape.curve with
	| None -> begin
	  Printf.printf
	    "I can't find any intersection of your edge with the end node shape.\nI'm taking the center as a end node instead.\n" ;
	  node2.center
	end
	| Some (t2, bezier2) -> Curve.bezier_evaluate bezier2 t2 
    end in
    Curve.draw ~parameters:parameters (Curve.of_point_lists [start :: (controls @ [finish])])

  let bend_left ?angle:(angle=30.) node1 node2 = 
    let node1 = node1.center in
    let node2 = node2.center in
    let angle = (-. angle) *. pi /. 180. in
    let vec = Vector.scal_mul (0.5 /. (cos angle)) (Vector.of_points node1 node2) in
    let vec = Vector.rotate (angle) vec in
    [Vector.translate node1 vec]
  let bend_right ?angle:(angle=30.) node1 node2 = 
    let node1 = node1.center in
    let node2 = node2.center in
    let angle = angle *. pi /. 180. in
    let vec = Vector.scal_mul (0.5 /. (cos angle)) (Vector.of_points node1 node2) in
    let vec = Vector.rotate (angle) vec in
    [Vector.translate node1 vec]
  let make_draw l
      ?contents:(contents = []) 
      ?shape:(shape = fun contents -> NodeShape.rectangle 
	~inner_sep:NodeShape.default_inner_sep 
	~parameters:{ Drivers.default with Drivers.close = true} 
	contents)
      ?center:(center = (
	let x0,y0,x1,y1 = Drivers.bounding_box contents in 
	middle (x0,y0) (x1,y1))) 
      () =
    let node = make ~contents:contents ~shape:shape ~center:center () in
    node, (l @ (draw node))
end	    

