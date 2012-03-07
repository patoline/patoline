
type point = float * float
let proj (x,y) = x
let proj' (x,y) = y
let middle (x0,y0) (x1,y1) = (0.5 *. (x0 +. x1), 0.5 *. (y0 +. y1))
let distance (x0,y0) (x1,y1) = sqrt ((x1 -. x0)**2. +. (y1 -. y0)**2.)

module Curve = struct
  type t =  Bezier.curve list
  let bezier_of_point_list l = 
    let xs, ys = List.split l in
    (Array.of_list xs, Array.of_list ys)
  let of_point_lists l = List.map bezier_of_point_list l
  let draw
    ?parameters:(parameters=Drivers.default)
    (curve:t) =
      Drivers.Path (parameters, Array.of_list curve)
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

module Rectangle = struct
  type t = { bottom_left : point ; top_right : point }
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
  let points: t -> point list = fun r ->
    let x1,y1 = r.bottom_left in
    let x3,y3 = r.top_right in
    let x2 = x3 in
    let y2 = y1 in
    let x4 = x1 in
    let y4 = y3 in
    [(x1,y1);(x2,y2);(x3,y3);(x4,y4)]
end



module NodeShape = struct
  type t = ?contents:Rectangle.t -> Curve.t
  let circle contents = 
    let (x0,y0,x1,y1) = Drivers.bounding_box contents in
    (* let x_center, y_center = middle (x0,y0)( x1,y1) in *)
    (* let radius = 0.5 *. (distance (x0,y0) (x1,y1)) in *)
    match Rectangle.points (Rectangle.make x0 y0 x1 y1) with
      | ((x1,y1) as p1) :: ((x2,y2) as p2) :: ((x3,y3) as p3) :: ((x4,y4) as p4) :: [] ->
	let h = (x1 -. x0) /. 10. in
	let v = (y1 -. y0) /. 10. in
	let advance_h (x,y) h =  (x +. h, y) in
	let advance_v (x,y) v =  (x, y +. h) in
	Curve.of_point_lists [
	  [p1;(advance_v p1 (-. v));(advance_v p2 (-. v));p2] ;	(* The bottom curve *)
	  [p2;(advance_h p2 h);(advance_h p3 h);p3] ;	(* The right-hand curve *)
	  [p3;(advance_v p3 v);(advance_v p4 v);p4] ;	(* The top curve *)
	  [p4;(advance_h p4 (-. h));(advance_h p1 (-. h));p1] 	(* The left-hand curve *)
	]
      | _ -> assert false
end

module Node = struct
  type t = { shape : Curve.t ;
	     parameters : Drivers.path_parameters ;
	     contents : Drivers.contents list ;
	     center : point }
  let make
      ?contents:(contents = []) 
      ?parameters:(parameters = Drivers.default)
      ?center:(center = (
	let x0,y0,x1,y1 = Drivers.bounding_box contents in 
	middle (x0,y0) (x1,y1))) 
      ?shape:(shape = NodeShape.circle)
      ()
      =
    { shape = shape contents ; 
      parameters = parameters ; 
      contents = contents ; 
      center = center }
  let draw : t -> Drivers.contents list = fun node -> 
    node.contents @ [(Curve.draw ~parameters:node.parameters node.shape)]
  let edge
      ?parameters:(parameters = Drivers.default)
      node1
      ?controls:(controls = [])
      node2 = 
    let underlying_curve = (Curve.of_point_lists [node1.center :: (controls @ [node2.center])]) in
    match Curve.latest_intersection underlying_curve node1.shape with
      | None -> failwith "The shape of start node should be a closed curve."
      | Some (t1, bezier1) ->
    	let start = Curve.bezier_evaluate bezier1 t1 in
    	match Curve.earliest_intersection underlying_curve node2.shape with
	  | None -> 
	    failwith "The shape of end node should be a closed curve."
	  | Some (t2, bezier2) ->
    	    let finish = Curve.bezier_evaluate bezier2 t2 in
    	    Curve.draw ~parameters:parameters (Curve.of_point_lists [start :: (controls @ [finish])])
  let make_draw l
      ?contents:(contents = []) 
      ?parameters:(parameters = Drivers.default)
      ?shape:(shape = NodeShape.circle) 
      ?center:(center = (
	let x0,y0,x1,y1 = Drivers.bounding_box contents in 
	middle (x0,y0) (x1,y1))) 
      () =
    let node = make ~contents:contents ~parameters:parameters ~shape:shape ~center:center () in
    node, (l @ (draw node))
end	    

