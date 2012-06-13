open Document
module Drivers = OutputCommon
open OutputCommon

let swap (x,y) = (y,x)
let pi = 3.14159
let one_third = 1. /. 3.
let half_pi = pi /. 2.
let to_deg angle = angle *. 180. /. pi
let to_rad angle = angle *. pi /. 180.
let default_line_width = ref 0.2
let ex env = 
  let l = Maths.draw [ env ] ([Maths.Ordinary (Maths.noad ((Maths.glyphs "x"))) ]) in
  let x = List.find
    (function Box.GlyphBox x -> true | _ -> false)
    l
  in (Box.upper_y x 0. -. Box.lower_y x 0.) /. 2.

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
  let (<>) (x,y) (x',y') = x *. x' +. y *. y'
  let minus (x,y) = (-. x,-. y)
  let translate p vec = (p + vec)
  let rotate angle (x,y) = (* Angle en degres *)
    let angle = to_rad angle in
    (x *. cos angle -. y *. sin angle,
     x *. sin angle +. y *. cos angle)
  let norm (x,y) = sqrt (x ** 2. +. y ** 2.)
  let normalise ?norm:(norm'=1.0) vec = 
    let l = norm vec in
    if l == 0. then failwith "Can't normalise the 0 vector." else
    scal_mul (norm' /. l) vec
  let turn_left (x,y) = (-. y,x)
  let turn_right (x,y) = (y, -. x)
  let unit angle = rotate angle (1.,0.) 
  let angle v = 
    let x,y = normalise v in
    to_deg (atan2 y x)
  let rec pipi angle = 
    if angle <= 180. then
      if angle >= -. 180. then angle 
      else pipi (angle +. 360.) 
    else pipi (angle -. 360.) 

  let rec zeropi angle = 
    if angle <= 360. then
      if angle >= 0. then angle 
      else pipi (angle +. 360.) 
    else pipi (angle -. 360.) 


  let sector v v' = zeropi (angle v' -. angle v)

end

module Affine = struct

  type droite = float * float * float

  let of_vect_point (x,y) p0 = 
    let (a,b) = Vector.rotate 90. (x,y) in
    let c = Vector.(<>) (a,b) p0 in
    (a,b, -. c)

  let of_points p q = 
    let v = Vector.of_points p q in
    of_vect_point v p

  let intersect (a,b,c) (a',b',c') = 
    let det = a *. b' -. a' *. b in
    if det = 0.0 then
      let _ = Printf.fprintf stderr "Non existent intersection. Picking (0,0) instead.\n" in
      (0.,0.)
    else
      let x = (c' *. b -. c *. b') /. det in
      let y = (c *. a' -. c' *. a) /. det in
      (x,y)

  let mediatrice p q = 
    let (a,b) as v = Vector.of_points p q in
    let m = Point.middle p q in
    let c = Vector.(<>) v m in
    (a,b, -. c)

  let normal e (a,b,c) =
    let v' = Vector.turn_left (a,b) in
    of_vect_point v' e

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


      let make_quadratic e l ll =
	(* begin Printf.fprintf stderr "Entering make_quadratic with e = (%f,%f), l = (%f,%f), ll = (%f,%f).\n" *)
	(*     (fst e) (snd e) *)
	(*     (fst l) (snd l) *)
	(*     (fst ll) (snd ll) ; *)
	(*   flush stderr *)
	(* end;  *)
      	let alpha = Vector.sector (Vector.of_points l ll) (Vector.of_points l e) in
	let do_make_quadratic alpha e l ll = 
	  let beta = 0.5 *. (180. -. alpha) in
	  let le = Vector.of_points l e in
	  let lll = Vector.of_points l ll in
	  let vl = Vector.normalise
	    ~norm:((Vector.norm le) /. (2. *. (cos (to_rad beta)))) 
	    (Vector.rotate beta le)
	  in
	  let xl = Vector.(+) l vl in
	  let vr = Vector.normalise
	    ~norm:((Vector.norm lll) /. (2. *. (cos (to_rad beta)))) 
	    (Vector.rotate (-. beta) lll)
	  in
	  let xr = Vector.(+) l vr in
	  xl, xr
	in
	let xl,xr = 
	  if alpha <= 180. then 
	    do_make_quadratic alpha e l ll 
	  else swap (do_make_quadratic (360. -. alpha) ll l e)
	in
	(* begin Printf.fprintf stderr "Returning [[e;xl;l];[l;xr;ll]] = " ; *)
	(*   Printf.fprintf stderr "[[(%f,%f);(%f,%f);(%f,%f)];[(%f,%f);(%f,%f);(%f,%f)]].\n"  *)
	(*     (fst e) (snd e) *)
	(*     (fst xl) (snd xl) *)
	(*     (fst l) (snd l) *)
	(*     (fst l) (snd l) *)
	(*     (fst xr) (snd xr) *)
	(*     (fst ll) (snd ll) *)
	(* end ; *)
	[[e;xl;l];[l;xr;ll]]	

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

  let gradient curve =
    List.map (fun (xs,ys) -> (Bezier.derivee xs, Bezier.derivee ys)) curve

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

  let bezier_linear_length bezier = 
    let s = bezier_evaluate bezier 0. in
    let t = bezier_evaluate bezier 1. in
    Point.distance s t 

  let linear_length = List.fold_left
    (fun res ((xs,ys) as bezier) -> res +. bezier_linear_length bezier)
    0.

  let length n curve = 
    let beziers = List.concat
      (List.map (fun (xs, ys) -> 
	let beziers_x = Bezier.divide xs n in
	let beziers_y = Bezier.divide ys n in
	List.combine beziers_x beziers_y)
	 curve)
    in
    linear_length beziers

  let curvilinear curve z =
    (* Printf.fprintf stderr "Entering curvilinear %f.\n" z ; *)
    let n = 1000 in
    let time_unit = 1. /. (float_of_int n) in
    let beziers = List.concat
      (List.map (fun (xs, ys) -> 
	let beziers_x = Bezier.divide xs n in
	let beziers_y = Bezier.divide ys n in
	List.combine beziers_x beziers_y)
	 curve)
    in
    let rec scan z n_yet t_yet z_yet l = 
      (* let _ = Printf.fprintf stderr "Scanning n_yet = %d, t_yet = %f, z_yet = %f.\n" n_yet t_yet z_yet in *)
      match l with
      | [] -> let _ = 
		Printf.fprintf stderr 
		  "Couldn't find curvilinear coordinate. Returning 0.\n" 
	      in 0.
      | (xs', ys') as bezier :: l' -> 
	let length = bezier_linear_length bezier in
	let z_restant = z -. z_yet in
	if z_restant > length then
	  scan z (n_yet + 1) (t_yet +. time_unit) (z_yet +. length) l'
	else time_unit *. ((float_of_int n_yet) +. (z_restant /. length))
    in
    let rec backwards_scan z n_yet t_yet z_yet l = 
      (* let _ = Printf.fprintf stderr "Scanning backwards n_yet = %d, t_yet = %f, z_yet = %f.\n" n_yet t_yet z_yet in *)
      match l with
      | [] -> let _ = 
		Printf.fprintf stderr 
		  "Couldn't find curvilinear coordinate. Returning 0.\n" 
	      in 0.
      | (xs', ys') as bezier :: l' -> 
	let length = bezier_linear_length bezier in
	let z_restant = z -. z_yet in
	if z_restant > length then
	  backwards_scan z (n_yet + 1) (t_yet +. time_unit) (z_yet +. length) l'
	else 1. -. time_unit *. ((float_of_int n_yet) +. (z_restant /. length))
    in
    if z < 0. then
      backwards_scan (-. z) 0 0. 0. (List.rev beziers)
    else 
      scan z 0 0. 0. beziers
	
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
      ?angle:(angle=20.)
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
	  (Vector.scal_mul 0.5 (Vector.rotate (-. 90.) (Vector.of_points p1 p2)));
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
	      (Vector.scal_mul amplitude (Vector.rotate (-. 90.) (Vector.of_points p1 p2)));
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
    let contents = (Box.draw_boxes (boxify_scoped env spec.contents_spec)) in
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
      let cosangle = cos (to_rad angle) in
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
      match node1.Node.curve with
	| [a] -> (0,0.)
	| curve1 ->
	  match Curve.latest_intersection curve curve1 with
	    | None -> begin
	      (* Printf.fprintf stderr *)
	      (* 	"I can't find any intersection of your edge with the start node shape.\nI'm taking the center as a start node instead.\n" ; *)
	      (0,0.)
	    end
	    | Some (i, t1) -> (i, t1)
    end in
    let (j,t') as finish = begin 
      match node2.Node.curve with
	| [b] -> ((Curve.nb_beziers curve) - 1,1.)
	| curve2 ->
	  match Curve.earliest_intersection curve curve2 with
	    | None -> begin
	      (* Printf.fprintf stderr *)
	      (* 	"I can't find any intersection of your edge with the end node shape.\nI'm taking the center as a end node instead.\n" ; *)
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
	       | `Main			(* The anchor used to draw edges between gentities by default; 
					   Will be `Center by default. *)
	       | `Base
	       | `BaseWest
	       | `BaseEast
	       | `Line
	       | `LineWest
	       | `LineEast
	       | `Vec of Vector.t
	       | `Pdf		       (* The origin when typesetting the contents *)
	       | `Curvilinear of float	(* Between 0. and 1. (for paths) *)
	       | `CurvilinearFromStart of float	(* Between 0. and 1. (for paths) *)
	       | `Temporal of float	(* Between 0. and 1. (for paths) *)
	       | `Start
	       | `End
	       ]
  type gentity = { curve : Curve.t ;	(* The curve is used to determine the start and end of edges *)
		   anchor : anchor -> Point.t ; (* Anchors are used for relative placement *)
		   contents : OutputCommon.contents list (* What's needed to actually draw the node *)
		 } 

  (* Casting points into gentities *)
  let coord (x : Point.t) = 
    { curve = Curve.of_point_lists [[x]] ;
      anchor = (fun a -> x) ;
      contents = [] }

  (* Two important ways of constructing gentities are nodes and edges between them.  *)

  (* Types for style instructions passed as arguments for constructing nodes *)
  type style = [ 			
  (* Placement of nodes *)
  | `At of Point.t
  | `Anchor of anchor 			(* Anchor used for placement *)
  | `MainAnchor of anchor		(* Anchor used by default for drawing edges between gentities *)
  (* Contents of nodes *)
  | `InnerSep of float
  | `OuterSep of float
  | `TextDepth of float
  | `TextHeight of float
  | `TextWidth of float
  (* Node shapes *)
  | `Rectangle
  | `Circle
  | `Coordinate
  (* Edge modifiers *)
  | `Dashed of float list
  | `Dotted
  | `Bend of float
  | `BendRight of float
  | `BendLeft of float
  | `Squiggle of int * float * float * float 
  | `Fore of float
  | `Double of float 
  | `ShortenS of float
  | `ShortenE of float
  | `Shorten of float * float
  | `Head of arrow_head
  (* Node or edge modifiers *)
  | `Draw 
  | `Fill
  | `DrawOf of path_parameters
  | `LineWidth of float 	
  | `Rotate 
  | `Color of OutputCommon.color
  (* Matrix specification *)
  | `Centers of float * float
  | `Placement of (gentity array) array -> int -> int -> Point.t
  | `AllNodes of style_instructions
  | `ToOf of tip_info -> path_parameters -> (float * float * float * float * float)
  | `To
  | `ModTo
  | `ModToOf of float * float
  | `Moustache
  ]
  and arrow_head = [ `To | `Moustache | `None ]
  and style_instructions = style list
  and tip_info = { tip_line_width : float ; is_double : bool }


  let between_centers _ disty distx i j =
    (float_of_int j *. distx), -. (float_of_int i *. disty) 

  module Style = struct

    let memoise f = 
      let module M = Map.Make (struct 
	type t = style_instructions
	let compare x y = if (x == y) then 0 else 1 
      end) 
      in
      let m = ref M.empty in
      fun style -> 
	try M.find style !m with
	  | Not_found -> 
	    let res = f style in
	    let _ = m := M.add style res !m in
	    res	    

    let make f default = memoise (fun style -> 
      List.fold_right f style default)

    let at = make
      (fun instr res -> match instr with `At (x,y) -> (x,y) | _ -> res)
      (0.,0.)

    let shape = make 
      (fun instr res -> match instr with 
      | `Rectangle -> `Rectangle 
      | `Coordinate -> `Coordinate 
      | `Circle -> `Circle
      | _ -> res)
      `Rectangle

  (* Find path parameters specification in a list of style instructions *)
    let parameters_rec = 
      (fun instr res -> match instr with
         | `Draw when res.strokingColor=None-> { res with strokingColor = Some black }
         | `Draw -> res
         | `Fill -> { res with close = true ; fillColor = Some black }
      | `LineWidth w -> { res with lineWidth = w }
      | `Color c -> { res with strokingColor=Some c }
      | `DrawOf params -> params 
      | `Dashed pattern -> { res with dashPattern = pattern }
      | _ -> res)
    let parameters = make parameters_rec { default with close = true ; 
      strokingColor=None ; 
      lineWidth = !default_line_width } 
    let path_parameters = make parameters_rec { default with close = false ; strokingColor=None ; 
      lineWidth = !default_line_width } 

  (* Find outer_sep specification in a list of style instructions *)
    let outer_sep = make
      (fun instr res -> match instr with `OuterSep h -> h | _ -> res)
      0.

  (* Find inner_sep specification in a list of style instructions *)
    let inner_sep = make
      (fun instr res -> match instr with `InnerSep h -> h | _ -> res)
      0.5

  (* Find text_depth specification in a list of style instructions *)
    let text_depth = make
      (fun instr res -> match instr with `TextDepth h -> h | _ -> res)
      0.

  (* Find main anchor specification in a list of style instructions *)
    let main_anchor = make
      (fun instr res -> match instr with `MainAnchor a -> a | _ -> res)
      `Center

  (* Find line width specification in a list of style instructions *)
    let line_width = make
      (fun instr res -> match instr with
      | `DrawOf params -> params.lineWidth
      | `LineWidth w -> w
      | _ -> res)
      !default_line_width

  (* Find anchor specification in a list of style instructions *)
    let anchor = make
      (fun instr res -> match instr with `Anchor h -> h | _ -> res)
      `Center

  (* Find arrow head specification in a list of style instructions *)
    let head = make 
      (fun instr res -> match instr with `Head h -> h | _ -> res)
      `None

  (* Find arrow style specification in a list of style instructions *)
    let double = make 
      (fun instr res -> match instr with `Double margin -> Some margin | _ -> res)
      None
      
  (* Find matrix placement specification in a list of style instructions *)
    let matrix_placement matrix = make 
      (fun instr res -> match instr with 
	  `Centers (distx,disty) -> (between_centers matrix distx disty) 
	| `Placement f -> f matrix
	| _ -> res)
      (between_centers matrix 10. 10.) 

    (* Search 'style' for instruction on 'all nodes' of a matrix *)
    let pop style = 
      List.flatten (List.map (function 
	| `AllNodes style' -> style' 
	| _ -> []) style)

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
	(* let line_width = Style.line_width style in *)
	let sep = inner_sep +. outer_sep (* +. line_width *) in
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

    let rec anchor env style outer_curve mid_curve bb pdf_anchor = 	
      let (p1,p2,p3,p4) = BB.outer_points style bb in
      let text_depth = Style.text_depth style in 
      let inner_sep = Style.inner_sep style in 
      let outer_sep = Style.outer_sep style in 
      (* let line_width = Style.line_width style in *)
      let south = Point.middle p1 p2 in
      let ex = ex env in
      let base = Vector.(+) south (0.,inner_sep +. outer_sep +. ex +. text_depth) in
      let main = begin let main = Style.main_anchor style 
		       in if main = `Main then Printf.fprintf stderr "Please do not choose `Main as a main anchor; 
`Main is used when drawing edges; 
it is `Base by default and you may change it, e.g., to `Center, using `MainAnchor `Center.\n" ; `Base 
      end
      in
      (* Printf.fprintf stderr "Calcul de anchor: ex = %f \n" ex ; *)
      match Style.shape style with
	| `Coordinate -> begin       let at = Style.at style in
				     function _ -> at end
	| `Rectangle -> begin function
	    | `Vec v -> Vector.(+) (Point.middle p1 p3) v
	    | `Center -> Point.middle p1 p3
	    | `Main -> anchor env style outer_curve mid_curve bb pdf_anchor main
	    | `Base -> base
	    | `BaseEast -> (fst (Point.middle p2 p3),snd base)
	    | `BaseWest -> (fst (Point.middle p1 p4),snd base)
	    | `Line -> (fst base, (snd base -. ex))
	    | `LineEast -> (fst (Point.middle p2 p3),(snd (Vector.(+) (Point.middle p1 p3) pdf_anchor)))
	    | `LineWest -> (fst (Point.middle p1 p4), snd (Vector.(+) (Point.middle p1 p3) pdf_anchor))
	    | `East -> Point.middle p2 p3  
	    | `West -> Point.middle p1 p4  
	    | `North -> Point.middle p3 p4
	    | `South -> south
	    | `SouthWest -> p1
	    | `SouthEast -> p2
	    | `NorthEast -> p3
	    | `NorthWest -> p4
	    | `Pdf -> anchor env style outer_curve mid_curve bb pdf_anchor (`Vec pdf_anchor)
	    | _ -> Printf.fprintf stderr "Anchor undefined for a rectangle. Returning the center instead.\n" ; 
	      Point.middle p1 p3
	end
	| `Circle -> let center = Point.middle p1 p3 in
		     let radius = Point.distance center p1 in
		     let anchor_of_angle angle = 			 
		       Vector.(+) center (Vector.normalise ~norm:radius (Vector.unit angle))
		     in
		     begin function
		       | `Vec v -> Vector.(+) center v
		       | `Center -> center
		       | `Base -> base
		       | `Main -> anchor env style outer_curve mid_curve bb pdf_anchor main
		       | `East -> anchor_of_angle 0.
		       | `West -> anchor_of_angle 180.
		       | `North -> anchor_of_angle 90.
		       | `South -> anchor_of_angle (-. 90.) 
		       | `SouthWest -> p1
		       | `SouthEast -> p2
		       | `NorthEast -> p3
		       | `NorthWest -> p4
		       | `Angle angle -> anchor_of_angle angle
		       | `Pdf -> anchor env style outer_curve mid_curve bb pdf_anchor (`Vec pdf_anchor)
		       | _ -> Printf.fprintf stderr "Anchor undefined for a circle. Returning the center instead.\n" ; 
			 center
		     end 

    type mid_or_out = Outer | Mid 
	
    let kappa = 0.5522847498

    let curve mid_or_out style bb = 	
      let (p1,p2,p3,p4) = match mid_or_out with
	| Mid -> BB.mid_points style bb 
	| Outer -> BB.outer_points style bb 
      in
      match Style.shape style with
	| `Coordinate -> Curve.of_point_lists [[Point.middle p1 p3]]
	| `Rectangle -> begin
	  Curve.of_point_lists [
	    [p1;p2] ;	(* The bottom curve *)
	    [p2;p3] ;	(* The right-hand curve *)
	    [p3;p4] ;	(* The top curve *)
	    [p4;p1] 	(* The left-hand curve *)
	  ]
	end
	| `Circle -> begin 
	  let center = Point.middle p1 p3 in
	  let radius = Point.distance center p1 in
	  let anchor_of_angle angle = 			 
	    Vector.(+) center (Vector.normalise ~norm:radius (Vector.unit angle))
	  in
	  let q1 = anchor_of_angle 0. in
	  let q2 = anchor_of_angle 90. in
	  let q3 = anchor_of_angle 180. in
	  let q4 = anchor_of_angle (-. 90.) in
	  let quadrant a b =
	    let tangent_a = Vector.normalise ~norm:(radius *. kappa) (Vector.of_points center b) in
	    let tangent_b = Vector.normalise ~norm:(radius *. kappa) (Vector.of_points center a) in
	    [a ;
	     Vector.(+) a tangent_a ;
	     Vector.(+) b tangent_b ;
	     b ]
	  in
	  Curve.of_point_lists [
	    quadrant q1 q2 ;
	    quadrant q2 q3 ;
	    quadrant q3 q4 ;
	    quadrant q4 q1 
	  ]
	end 

    module Edge = struct

      let default_info = { tip_line_width = !default_line_width ; is_double = false }

      let head_moustache info params = 
       (* let _ = begin  *)
       (* 	 Printf.fprintf stderr "Entering head: lineWidth = %f, true lineWidth = %f \n" *)
       (* 	   params.lineWidth info.tip_line_width ; *)
       (* 	 flush stderr *)
       (* end in *)
       if info.is_double then
	 let short = max (params.lineWidth /. 2.) 0.6 in
	 let thickness = params.lineWidth in
       let height = max (1.6 *. short) 0.8 in
       let width = max info.tip_line_width 1. in
       (short, thickness, height, width, 0.01)

       else

	let short = max (0.6 *. params.lineWidth) 0.2 in
	let thickness = max (0.7 *. params.lineWidth) 0.2 in
	let height = max (2. *. short) 0.8 in
	let width = max (1.3 *. info.tip_line_width) 1. in
	(short, thickness, height, width, 0.01)

      let rec transfo style (info, params, underlying_curve, curves) = match style with
	| `Squiggle (freq,amplitude,a,b) -> 
	  let squiggle (params, curve) =
	    let curve1,curve,curve2 = Curve.split2 curve a b in
	    (params, curve1 @ (List.flatten (List.map (Edge.squiggle freq amplitude) curve)) @ curve2)
	  in
	  let params', u_curve = squiggle (params, underlying_curve) in
	  (info, params', u_curve, List.map squiggle curves)
	| `Fore margin -> 
	  let white_paths = List.map (fun (params, curve) -> 
	    { params with 
	      Drivers.strokingColor=Some (Drivers.RGB { Drivers.red=1.;Drivers.green=1.;Drivers.blue=1. }); 
	      Drivers.lineWidth=params.Drivers.lineWidth +. 2. *. margin },
	    curve)
	    curves
	  in
	  let _, _, _, white_paths = 
	    transfo (`Shorten (0.1,0.1)) (info, params, underlying_curve, white_paths)   in
	  info, params, underlying_curve, (white_paths @ curves)
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
	      lineWidth = margin +. 2. *. params.lineWidth },
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
	  (* let white_paths = transfo info white_paths (`Shorten (delta,delta)) in *)
	  (* let black_paths = transfo info black_paths (`Shorten (delta,delta)) in *)
	  { info with tip_line_width = margin +. 2.0 *. params.lineWidth }, 
	  params,
	  underlying_curve,
	  (black_paths @ white_paths)
	| `ShortenS a -> transfo (`Shorten (a,1.)) (info, params, underlying_curve, curves) 
	| `ShortenE b -> transfo (`Shorten (0.,b)) (info, params, underlying_curve, curves) 
	| `Shorten (a,b) -> 
	  let shorten (params',curve') = 
	    let ta = Curve.curvilinear curve' a in
	    let tb = Curve.curvilinear curve' (-. b) in
	    (* let _ = Printf.fprintf stderr "Restricting to %f,%f.\n" ta tb ; flush stderr in *)
	    params', Curve.restrict curve' ta tb
	  in
	  let params', u_curve = shorten (params, underlying_curve) in
	  info, params', u_curve, List.map shorten curves
	| _ -> (info, params, underlying_curve, curves)

      let rec posttransfo style (info, params, underlying_curve, curves) = match style with
	| (`Head `To | `To) -> posttransfo (`ToOf head_moustache) (info, params, underlying_curve, curves)
	| `ModTo -> posttransfo (`ModToOf (0.5, 1.)) (info, params, underlying_curve, curves)
	| (`ModToOf (time,width)) -> 
	  let info, params, underlying_curve, curves = 
	    posttransfo (`ToOf head_moustache) (info, params, underlying_curve, curves)
	  in
	  let middle = Curve.eval underlying_curve time in
	  let (da,db) as grad = Curve.eval (Curve.gradient underlying_curve) time in
	  let (la,lb) as l = (-. db, da) in
	  let (ra,rb) as r = (db, -. da) in
	  let (la,lb) as l = Vector.normalise ~norm:width l in
	  let (ra,rb) as r = Vector.normalise ~norm:width r in
	  let dash = Curve.of_point_lists [[(Vector.(+) l middle);(Vector.(+) r middle)]] in 
	  (info,params, underlying_curve, curves @ [params, dash])
	| ((`Head `Moustache) | `Moustache) -> 
	  posttransfo (`ToOf head_moustache) (info, params, underlying_curve, curves)
	| `ToOf head_params -> 
	  let (da,db) as grad = Curve.eval (Curve.gradient underlying_curve) 1. in
	  let short, thickness, height, width, lw = head_params info params in
	  let thickness' = thickness -. thickness *. info.tip_line_width /. 2. /. width in

	  (* Control points on the curve *)
	  let (xe,ye) as e = Curve.eval underlying_curve 1. in
	  (* let _ = Printf.fprintf stderr "Shortening by %f.\n" short ; flush stderr in *)
	  let _, _, curve0, curves = 
	    transfo (`Shorten (0.,short)) (info, params, underlying_curve, curves) in
	  (* let _ = Printf.fprintf stderr "Done shortening.\n" ; flush stderr in *)
	  let e0 = Curve.eval curve0 1. in
	  let ee0 = Vector.of_points e e0 in
	  let e1 = Vector.(+) e (Vector.normalise ~norm:thickness ee0) in
	  let e2 = Vector.(+) e (Vector.normalise ~norm:height ee0) in

	  let lnormale = Vector.rotate 90. (Vector.normalise grad) in
	  let rnormale = Vector.rotate (-. 90.) (Vector.normalise grad) in

	  (* Left control points *)
	  let l = Vector.(+) e0 (Vector.normalise ~norm:(info.tip_line_width /. 2.) lnormale) in
	  let ll = Vector.(+) e2 (Vector.normalise ~norm:(width) lnormale) in
	  let l' = Vector.(+) l (Vector.normalise ~norm:thickness' ee0) in

	  (* Right control points *)
	  let r = Vector.(+) e0 (Vector.normalise ~norm:(info.tip_line_width /. 2.) rnormale) in
	  let rr = Vector.(+) e2 (Vector.normalise ~norm:(width) rnormale) in
	  let r' = Vector.(+) r (Vector.normalise ~norm:thickness' ee0) in

	  (* Put everything together *)
	  let tip = Curve.of_point_lists ((Curve.make_quadratic e l ll) 
					  @ (Curve.make_quadratic ll l' e1) 
					  @ (Curve.make_quadratic e1 r' rr) 
					  @ (Curve.make_quadratic rr r e)) 
	  in
	  (info, params, underlying_curve, 
	   curves @ [({ params with 
	     close = true ; 
	     fillColor = params.strokingColor ; 
	     lineWidth = lw }, tip)])

	| _ -> (info, params, underlying_curve, curves)

      let rec pretransfo style curve = match style with
	| `Bend (angle) -> if Curve.nb_beziers curve = 1 then
	    begin match curve with | [] -> assert false | (xs,ys) :: _ -> 
	      if Array.length xs = 2 then
		let s = (xs.(0),ys.(0)) in
		let e = (xs.(1),ys.(1)) in
		let vec = Vector.scal_mul (0.5 /. (cos (to_rad angle))) (Vector.of_points s e) in
		let vec = Vector.rotate angle vec in
		let x,y = Vector.translate s vec in
		[ [| xs.(0) ; x ; xs.(1) |], 
		  [| ys.(0) ; y ; ys.(1) |] ]
	      else
		curve
	    end
	  else curve
	| `BendLeft angle -> pretransfo (`Bend angle) curve
	| `BendRight angle -> pretransfo (`Bend (-. angle)) curve
	| _ -> curve

      let pretransfos styles curve = List.fold_right
	pretransfo styles curve

      let draw_curve style curve = 
	let info =
	  match Style.double style with
	    | Some margin -> { tip_line_width = margin +. 2. *. Style.line_width style ;
			       is_double = true }
	    | None -> { tip_line_width = Style.line_width style ;
			is_double = false }
	in
	let info', params', u_curve, curves = List.fold_right
	  transfo style (info, (Style.path_parameters style), curve, [(Style.path_parameters style, curve)]) 
	in
	let _, params', u_curve, curves = List.fold_right
	  posttransfo style (info', (Style.path_parameters style), u_curve, curves)
	in
	curves

      let outer_curve style curve = curve
    end
      
    (* Translate a gentity node by v *)
    let translate v node =
      { curve = Curve.translate v node.curve ;
	anchor = (fun a -> Vector.(+) (node.anchor a) v) ;
	contents = let x,y = v in List.map (OutputCommon.translate x y) node.contents }

    (* Returns the translation vector needed for placing 'node' according to 'style' *)
    let translation style node =
      let v = Style.at style in
      let a = (Style.anchor style) in
      let x,y = 
	Vector.(+) v
	  (Vector.of_points
	  (node.anchor a)
	  (0.,0.)) 
      in (x,y)

    (* Creates a node from boxified contents. Recenters. *)
    let node_boxified ?dump_contents:(dump_contents=false) env style contents =
      let parameters = Style.parameters style in
      (* Compute the gentity a first time, centered *)
      let (x0,y0,x1,y1) as bb = match contents with
	| [] -> (0.,0.,0.,0.)
	| _ -> OutputCommon.bounding_box contents 
      in
      let text_depth = -. y0 in
      let text_height = y1 in
      let style = style @ [ `TextDepth text_depth ; `TextHeight text_height ] in
      (* Keep the coordinate of (0,0) relative to the center *)
      let pdf_anchor = Vector.minus (Point.middle (x0,y0) (x1,y1)) in 
      let (x0',y0',_,_) as bb_boot = BB.center bb in
      let xt,yt = Vector.of_points (x0,y0) (x0',y0') in
      let contents = List.map (OutputCommon.translate xt yt) contents in (* Center the contents *)
      let outer_curve = curve Outer style bb_boot in (* The curve for calculating anchors *)
      let mid_curve = curve Mid style bb_boot in     (* The drawn curve *)
      let anchor = anchor env style outer_curve mid_curve bb_boot pdf_anchor in
      let centered_res = { curve = mid_curve ; 
			   anchor = anchor ; 
			   contents = (if dump_contents then [] else contents)
			   @ (Curve.draw ~parameters:parameters mid_curve) } 
      in
      (* Compute the translation *)
      let v = translation style centered_res in
      (translate v centered_res), (Vector.(+) v (xt,yt))
 		(* Return the translated gentity, and the translation *)
      
    let node env style contents = fst (node_boxified env style
					 (Box.draw_boxes (boxify_scoped env contents)))      

    let coordinate env (x,y) = node env [`Coordinate; `At (x,y)] []

  type continue_path_spec = Point.t list 
  and path_spec = continue_path_spec list 

  let point_lists_of_path_spec s continues =
    let rec point_lists_of_path_spec_rec res s continues = 
      match continues with
	| [] -> List.rev res
	| e :: rest -> point_lists_of_path_spec_rec ((s :: e) :: res) (list_last e) rest
    in
    point_lists_of_path_spec_rec [] s continues

  let path_of_curve style curve = 
    let parameters = Style.path_parameters style in
    let anchor = function
      | `Temporal pos -> Curve.eval curve pos
      | `Curvilinear pos -> Curve.eval curve (Curve.curvilinear curve pos)
      | `Center -> Curve.eval curve 0.5
      | _ -> Printf.fprintf stderr "Anchor undefined for a path. Returning the center instead.\n" ; 
	Curve.eval curve 0.5
    in
    { anchor = anchor ; curve = curve ; contents = Curve.draw ~parameters:parameters curve}

  let path style s continues = 		
    let curve = (Curve.of_point_lists (point_lists_of_path_spec s continues)) in
    path_of_curve style curve

  type controls = continue_path_spec * Point.t list (* The last list is treated differently: *)
  (* its endpoint should be provided as a node, see edge below. *)

  let point_lists_of_edge_spec s continues e =
    let rec point_lists_of_edge_spec_rec res s e continues = 
      match continues with
	| [] -> [[s;e]]
	| [ controls ] -> List.rev (((s :: controls) @ [e]) :: res)
	| controls :: rest -> point_lists_of_edge_spec_rec ((s :: controls) :: res) (list_last controls) e rest
    in
    point_lists_of_edge_spec_rec [] (s.anchor `Main) (e.anchor `Main) continues

  let clip curve node1 node2 = 
    let start = begin
      match node1.curve with
	| [xs,ys] when Array.length xs <= 1 -> (0,0.)
	| curve1 ->
	  match Curve.latest_intersection curve curve1 with
	    | None -> begin
	      (* Printf.fprintf stderr *)
	      (* 	"I can't find any intersection of your edge with the start node shape.\nI'm taking the center as a start node instead.\n" ; *)
	      (0,0.)
	    end
	    | Some (i, t1) -> (i, t1)
    end in
    let (j,t') as finish = begin 
      match node2.curve with
	| [xs,ys] when Array.length xs <= 1 -> ((Curve.nb_beziers curve) - 1,1.)
	| curve2 ->
	  match Curve.earliest_intersection curve curve2 with
	    | None -> begin
	      (* Printf.fprintf stderr *)
	      (* 	"I can't find any intersection of your edge with the end node shape.\nI'm taking the center as a end node instead.\n" ; *)
	      ((Curve.nb_beziers curve) - 1,1.)
	    end
	    | Some (j, t2) -> (j, t2)
    end in
    Curve.internal_restrict curve start finish 

  let edge_of_curve style curve = 
    let curve = Edge.pretransfos style curve in
    let underlying_curve = curve in
    let outer_curve = Edge.outer_curve style curve in
    let draw_curve = Edge.draw_curve style curve in
    let rec anchor = function
      | `Temporal pos -> Curve.eval curve pos
      | `Curvilinear pos -> Curve.eval curve (Curve.curvilinear curve pos)
      | `CurvilinearFromStart pos -> Curve.eval underlying_curve (Curve.curvilinear underlying_curve pos)
      | `Start -> anchor (`Temporal 0.)
      | `End -> anchor (`Temporal 1.)
      | `Center -> Curve.eval curve 0.5
      | _ -> Printf.fprintf stderr "Anchor undefined for an edge. Returning the center instead.\n" ; 
	Curve.eval curve 0.5
    in
    { anchor = anchor ; curve = outer_curve ; contents = 
	List.flatten (List.map (fun (params,curve) -> (Curve.draw ~parameters:params curve))
			draw_curve) }

  let edge style s controls e =
    let point_lists = point_lists_of_edge_spec s controls e in
    (* List.iter (fun curve ->  *)
    (*   List.iter (fun (x,y) -> Printf.fprintf stderr "(%f,%f) .. " x y) curve ; *)
    (*   Printf.fprintf stderr "\n") *)
    (*   point_lists; *)
    let underlying_curve = Curve.of_point_lists point_lists in
    let underlying_curve = Edge.pretransfos style underlying_curve in
    let curve = clip underlying_curve s e in
    let outer_curve = Edge.outer_curve style curve in
    let draw_curve = Edge.draw_curve style curve in
    let anchor = function
      | `Temporal pos -> Curve.eval curve pos
      | `Curvilinear pos -> Curve.eval curve (Curve.curvilinear curve pos)
      | `CurvilinearFromStart pos -> Curve.eval underlying_curve (Curve.curvilinear underlying_curve pos)
      | `Start -> s.anchor `Main
      | `End -> e.anchor `Main
      | `Center -> Curve.eval curve 0.5
      | _ -> Printf.fprintf stderr "Anchor undefined for an edge. Returning the center instead.\n" ; 
	Curve.eval curve 0.5
    in
    { anchor = anchor ; curve = outer_curve ; contents = 
	List.flatten (List.map (fun (params,curve) -> (Curve.draw ~parameters:params curve))
			draw_curve) }

  let edges style edge_list = 
    List.map (fun (style',s,controls,e) -> edge (style' @ style) s controls e) edge_list

  let matrix env style lines =
    let width = List.fold_left
      (fun res line -> max res (List.length line))
      0
      lines
    in
    let height = List.length lines in
    (* let bboxes = List.map (fun line -> List.map (fun contents -> OutputCommon.bounding_box contents)) in *)
    (* On commence par placer les noeuds de sorte que le noeud (0,0) ait son centre en (0,0) *)
    let res = Array.make_matrix height width (node env [] []) in
    let i = ref 0 in
    let j = ref 0 in
    List.iter (fun line -> 
      List.iter (fun (style', contents) -> 
	let mij = node env (style' @ (Style.pop style) @ [`Anchor `Base;`MainAnchor `Base]) contents in
	res.(!i).(!j) <- mij ;
	(* state := mij.contents @ !state ; *)
	incr j)
	line ;
      incr i ; j := 0)
      lines ;
    (* Now we have an array res of a priori centered nodes *) 
    (* We translate this to match the constraint placement of the matrix. *)
    let state = ref [] in		(* Create a reference for collecting the contents of all nodes *)
    let placement = Style.matrix_placement res style in
    let res' = 
      Array.mapi (fun i line -> 
	Array.mapi (fun j gentity -> 
	  let gentity' = translate (placement i j) gentity in
	  let _ = state := gentity'.contents @ !state in (* Collect the contents in 'state' *)
	  gentity') 
	  line) 
	res 
    in
    let res_node, v = node_boxified ~dump_contents:true env style !state in
    let res_matrix = Array.map (Array.map (translate v)) res' in
    (* Printf.fprintf stderr "Translation : (%f, %f)\n" (fst v) (snd v) ; *)
    res_node (* with contents = [] *), res_matrix

end

module Env_Diagram (Args : sig val arg1 : string end)(Args' : sig val env : user environment end) = struct
  open Diagram
    let stack = ref []
    let env = Args'.env
    let offset = ref 0.

    let node style contents = 
      let a = node env style contents in
      stack := a :: !stack ;
      a

    let coordinate p = 
      let a = coordinate env p in
      let _ = stack := a :: !stack in
      a

    let edge style a controls b =
      let e = edge style a controls b in
      stack := e :: !stack ;
      e

    let path style s continues =
      let e = path style s continues in
      stack := e :: !stack ;
      e


    let edges style edge_list = 
      let l = edges style edge_list in
      stack := l @ !stack ;
      l

    let matrix style lines = 
      let node, m = matrix env style lines in
      stack := node :: ((List.flatten (Array.to_list (Array.map Array.to_list m))) @ !stack) ;
      node, m

    let make () = 
      let fig = Box.drawing ~offset:(!offset) 
	(List.fold_left (fun res gentity -> List.rev_append gentity.contents res)
	   []
	   !stack)
      in
      stack := [] ; fig

      let label e style pos contents = 
	node  ((`At (e.anchor (`Temporal pos))) :: style) 
	  ([Scoped ((fun env -> { env with mathStyle = Mathematical.Script }), contents)])

      let label_anchor e anchor pos contents = 
	label e [`Anchor anchor] pos contents

     let labela ?style:(style=[]) e contents = label e (style @ [`Anchor `South]) 0.5 contents
     let labelb ?style:(style=[]) e contents = label e (style @ [`Anchor `North]) 0.5 contents
     let labell ?style:(style=[]) e contents = label e (style @ [`Anchor `East]) 0.5 contents
     let labelr ?style:(style=[]) e contents = label e (style @ [`Anchor `West]) 0.5 contents
     let labelbr ?style:(style=[]) e contents = label e (style @ [`Anchor `NorthWest]) 0.5 contents
     let labelbl ?style:(style=[]) e contents = label e (style @ [`Anchor `NorthEast]) 0.5 contents
     let labelar ?style:(style=[]) e contents = label e (style @ [`Anchor `SouthWest]) 0.5 contents
     let labelal ?style:(style=[]) e contents = label e (style @ [`Anchor `SouthEast]) 0.5 contents
     let labelc ?style:(style=[]) e contents = label e (style @ [`Anchor `Main]) 0.5 contents

     let edge_anchor a b style anchor pos contents = 
       let e = edge (`To :: `Draw :: style) a [] b in
       let l = label_anchor e anchor pos contents in
       e,l

     let edges_anchor l = 
       List.map (fun (s,e,style,anchor,pos,contents) -> edge_anchor s e style anchor pos contents) l
       
     let edges_anchor_mid l = 
       List.map (fun (s,e,style,anchor,contents) -> edge_anchor s e style anchor 0.5 contents) l

     let math_matrix style l = 
       matrix style (List.map (fun line ->
	 (List.map (fun (style, math_list) -> 
	   (style, [B (fun env -> Maths.draw [env] math_list)])) 
	    line))
		       l)


     module Arr = struct

	   let fun_max ?max:(max=max) f n = 
    let rec fun_max_rec res f n = 
      if n = 1 then res else
	(fun_max_rec (max res (f (n-1))) f (n - 1))
    in
    if n <= 1 then f 0
    else fun_max_rec (f 0) f n

  let node_width node = 
    fst (node.anchor `East)
    -. fst (node.anchor `West)

  let base_east style node = 
    let anchor = Style.anchor style in
    fst (node.anchor `East)
    -. fst (node.anchor anchor)

  let west_base style node = 
    let anchor = Style.anchor style in
    fst (node.anchor anchor)
    -. fst (node.anchor `West)

  let rec between_borders style vertically horizontally matrix =
    let width = Array.length matrix.(0) in
    let height = Array.length matrix in
    let widths = Array.make width 0. in
    let heights = Array.make height 0. in
    let _ = for j = 1 to width - 1 do 
	widths.(j) <- 	widths.(j-1)
	+. (horizontally j)
	+. (fun_max (fun i -> base_east (style i (j-1)) (matrix.(i).(j-1))) height)
	+. (fun_max (fun i -> west_base (style i j) (matrix.(i).(j))) height)
      done ;
    in
    let _ = 
      for i = 1 to height - 1 do 
	heights.(i) <- begin
	  (* The height of the previous row *)
	  heights.(i-1)
	  (* plus the given vertical space *)
	  -. (vertically i)
	  (* plus the least y0 of the previous row *)
	  +. (fun_max ~max:(fun y y' -> min y y') (fun j -> snd (matrix.(i-1).(j).anchor `South)) width)
	  (*  minus the greatest y1 of the present row (because we go downwards) *)
	  -. (fun_max (fun j -> snd (matrix.(i).(j).anchor `North)) width)
	end ;
      done ;
    in fun i j -> (widths.(j), heights.(i))

  let array anchors ?vertical_padding:(vpad=fun _ -> 1.) ?horizontal_padding:(hpad=fun _ -> 1.)
(* Mettre la valeur par defaut en ex *)
      lines =
    let style i j = [`Anchor (List.nth anchors j)] in
    let lines_contents = List.rev (snd (List.fold_left
					  (fun (i, reslines) line ->
					    succ i, 
					    (let _,_,resline =
					       (List.fold_left (
						 fun (i,j,resline) math -> 
						   let cell = (style i j, math) in
						   (i, succ j, cell :: resline))
						  (i,0,[])
						  line)
					     in List.rev resline) :: reslines)
					  (0, [])
					  lines))
    in    
    math_matrix [`Placement (between_borders style vpad hpad);`At (0.,0.);`Anchor `SouthWest] 
      lines_contents 


end

     let array = Arr.array

  end


open Diagram
open Box
  let xto ?margin:(margin=2.) a =
    [Maths.Binary { Maths.bin_priority = 0 ; Maths.bin_drawing = Maths.Normal 
	(true, 
        (Maths.noad
           (fun env st->
             let dr=Box.draw_boxes (Maths.draw [{env with mathStyle = Mathematical.Script}] a) in
	     let (x0,y0,x1,y1)=OutputCommon.bounding_box dr in
	     let m,ms = matrix env
	       [`Centers (1.,(x1 -. x0 +. 2. *. margin));
		`Anchor `Pdf;
		`InnerSep 0. ; `OuterSep 0. ;
		`At (-. 0.75 *. env.size *. (ex env),0.)] [[
	       ([`InnerSep 0.;`OuterSep 0.], []) ; 
	       ([`InnerSep 0.;`OuterSep 0.], [])
	     ]]
	     in
	     let e = edge [`Draw;`LineWidth 0.1;`To (* Of head *)] ms.(0).(0) [] ms.(0).(1) in
	     let l,_ = node_boxified env
	       [`OuterSep 0.2 ; `InnerSep 0.; `Anchor `South; `At (e.anchor (`Temporal 0.5))] dr 
	     in
	     let drawn = 
	       drawing_inline ~offset:(ex env) 
		 (List.fold_left (fun res gentity -> List.rev_append gentity.contents res)
		    []
		    (l :: e :: m :: (List.flatten (Array.to_list (Array.map Array.to_list ms)))))
	     in 
	     let width = drawn.drawing_min_width  -. 1.5 *. env.size *. (ex env) in
	     let drawn = { drawn with
		   drawing_min_width = width ;
	       drawing_nominal_width = width ;
		   drawing_max_width = width }
	     in
	     [Box.Drawing drawn])),
		   true);
		    Maths.bin_left = [] ; 
		    Maths.bin_right = [] }
     ]


  let xot ?margin:(margin=2.) a =
    [Maths.Binary { Maths.bin_priority = 0 ; Maths.bin_drawing = Maths.Normal 
	(true, 
        (Maths.noad
           (fun env st->
             let dr=Box.draw_boxes (Maths.draw [{env with mathStyle = Mathematical.Script}] a) in
	     let (x0,y0,x1,y1)=OutputCommon.bounding_box dr in
	     let m,ms = matrix env
	       [`Centers (1.,(x1 -. x0 +. 2. *. margin));
		`Anchor `Pdf;
		`InnerSep 0. ; `OuterSep 0. ;
		`At (-. 0.75 *. env.size *. (ex env),0.)] [[
	       ([`InnerSep 0.;`OuterSep 0.], []) ; 
	       ([`InnerSep 0.;`OuterSep 0.], [])
	     ]]
	     in
	     let e = edge [`Draw;`LineWidth 0.1;`To (* Of head *)] ms.(0).(1) [] ms.(0).(0) in
	     let l,_ = node_boxified env
	       [`OuterSep 0.2 ; `InnerSep 0.; `Anchor `South; `At (e.anchor (`Temporal 0.5))] dr 
	     in
	     let drawn = 
	       drawing_inline ~offset:(ex env) 
		 (List.fold_left (fun res gentity -> List.rev_append gentity.contents res)
		    []
		    (l :: e :: m :: (List.flatten (Array.to_list (Array.map Array.to_list ms)))))
	     in 
	     let width = drawn.drawing_min_width  -. 1.5 *. env.size *. (ex env) in
	     let drawn = { drawn with
		   drawing_min_width = width ;
	       drawing_nominal_width = width ;
		   drawing_max_width = width }
	     in
	     [Box.Drawing drawn])),
		   true);
		    Maths.bin_left = [] ; 
		    Maths.bin_right = [] }
     ]
