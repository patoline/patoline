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

let array_last xs = 
  let n = Array.length xs in 
  xs.(n-1)

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

module Curve = struct
  type t =  Bezier.curve list

  let first = function [] -> (Printf.fprintf stderr "Patofig warning: empty curve.\n" ; (0.,0.))
    | (xs,ys) :: rest -> (xs.(0),ys.(0))
  let last = function [] -> (Printf.fprintf stderr "Patofig warning: empty curve.\n" ; (0.,0.))
    | l -> let (xs,ys) = list_last l in
	   (array_last xs, array_last ys)

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


    module Graph (X : Set.OrderedType) = struct
	
	type color = Visited | Visiting | Virgin
	    
	type node = { id : X.t ; mutable sons : node list ; mutable color : color }
	    
	type graph = node list

	open Stack

	let total_order graph = 
	  let rec visit res stack node = 
	    if node.color = Visited 
	    then 
	      continue res stack
	    else
	      if (node.color = Visiting) or (List.for_all (fun son -> son.color = Visited) node.sons)
	      then let _ = node.color <- Visited in continue (node :: res) stack
	      else 
		let _ = node.color <- Visiting in
		let _ = push node stack in
		match node.sons with
		  | [] -> assert false	(* Because we've checked that not all sons are already visited *)
		  | x :: sons -> 
		    let _ = List.iter (fun son -> push son stack) sons in
		    visit res stack x
	  and continue res stack = 
	    if is_empty stack
	    then res
	    else 
	      visit res stack (pop stack) 
	  in
	  let stack = create () in
	  let _ = List.iter
	    (fun node -> push node stack)
	    graph
	  in 
	  let res = continue [] stack in
	  List.map (fun node -> node.id) res



    end


      module Transfo (X : Set.OrderedType) = struct

      module rec Style : sig

	(* module type T = sig type arg *)
	(* 		    module pet : Pet.T with type arg = arg  *)
	(* 		    val arg : arg *)
	(* end *)
	(* type t = module T *)

	  type t = { pet : Pet.t ; transfo : transfo }
	  and transfo = transfos -> X.t -> X.t
	  and transfos = t list Pet.Map.t

      end
	= struct 

	  type t = { pet : Pet.t ; transfo : transfo }
	  and transfo = transfos -> X.t -> X.t
	  and transfos = t list Pet.Map.t

	end
      and Pet : sig 
	type t 
	val name : t -> string
	val register : ?depends:t list -> ?codepends: t list -> string -> (t -> 'a) -> ('a * t)
	module Map : Map.S with type key = t
      end = struct

	open Style

	(* module type T = sig type arg val transfo : arg -> transfo end *)
	(* type t = module PT *)

	type t = { uid : int ; name : string }
	type u = t

	let name x = x.name

	module UPet = struct type t = u let compare x y = compare x.uid y.uid end

	module PETGraph = Graph (UPet)
	open PETGraph

      (* Our global state consists of 

	 - a graph ref, i.e., a node list ref, 

	 - a ref to a list of parameterised edge transfos (pet's),

         - a ref to a (bijective) map from nodes to pets,

         - a ref to its inverse map.

	 The main data structure is the graph: the rest is just for using it with just pets.
	 
      *)

      (* Here is the graph ref *)
	let graph : graph ref = ref []   

	let add_node x = let node = { id = x ; sons = [] ; color = Virgin } in
			 let _ = (graph := node :: !graph) in 
			 node

	let add_edge node1 node2 = let _ = node1.sons <- node2 :: node1.sons in ()

	let add_edges = List.iter (fun (node1,node2) -> add_edge node1 node2)

      (* Here is the ref to list of pets *)
	let pets : t list ref = ref []

	let compare_node node1 node2 = compare node1.id node2.id

	module NodeMap = Map.Make (struct type t = node let compare = compare_node end)
	module PetMap = Map.Make (UPet)

      (* Here are the maps *)
	let node_map : t NodeMap.t ref = ref NodeMap.empty
	let pet_map : node PetMap.t ref = ref PetMap.empty

	let node_of_pet pet = PetMap.find pet !pet_map
	let pet_of_node node = NodeMap.find node !node_map

	let count = ref 0
	let gensym () = 
	  let res = !count in 
	  incr count ;
	  res

      (* We now define the API to register new edge transformations *)
	let register ?depends:(depends=[]) ?codepends:(codepends=[]) name (f : t -> 'a) = 
	  let uid = gensym () in
	  let pet = {uid = uid;name=name} in
	  let node = add_node pet in
	  let _ = graph := node :: !graph in
	  let _ = pets := pet :: !pets in
	  let _ = node_map := NodeMap.add node pet !node_map in
	  let _ = pet_map := PetMap.add pet node !pet_map in
	  let _ = List.iter (fun dep -> add_edge (node_of_pet dep) node) depends in
	  let _ = List.iter (fun codep -> add_edge node (node_of_pet codep)) codepends in
	  (f pet), pet

      (* We now define the type of style specifications and the way they are applied *)

      (* From a list of style specifications, we will construct a map pets -> style list,
       but this time using the ordering induced by the graph for pets *)
	let poset_of_list l = 
	  let nodes = List.map 
	    (fun x -> { id = x ; sons = [] ; color = Virgin })
	    l
	  in
	  let (node_of_pet_t,pet_of_node_t) = 
	    List.fold_left2
	      (fun (node_of_pet_t, pet_of_node_t) pet node ->
		(PetMap.add pet node node_of_pet_t,
		 NodeMap.add node pet pet_of_node_t))
	      (PetMap.empty, NodeMap.empty)
	      l
	      nodes
	  in
	  let _ = match nodes with [] -> [] | _ -> begin
	    List.fold_left
	      (fun successors node -> match successors with [] -> [] | _ ->
		let _ = node.sons <- successors in List.tl successors)
	      (List.tl nodes)
	      nodes
	  end
	  in
	  (nodes, node_of_pet_t, pet_of_node_t)

	let print_pet out pet = 
	  output_string out (Printf.sprintf "{ id = %d, name = %s }" (pet.uid) (pet.name))
	let string_of_pet pet = Printf.sprintf "{ id = %d, name = %s }" (pet.uid) (pet.name)
	let print_node out node = print_pet out node.id
	let string_of_node node = string_of_pet node.id

	let print_graph g = 
	  List.iter (fun node -> Printf.fprintf stdout "%a has sons %a \n"
	    print_node node
	    (fun out sons -> output_string out (String.concat " ; " 
	      (List.map (fun node -> string_of_node node) sons)))
	    node.sons)
	    g

      let compare = 
	let memo = ref [] in
	let memo_res : 
	    (t -> t -> int) ref 
	    = ref (fun x y -> 0)
	in
	fun () ->
	  if !memo = !graph then !memo_res
	  else
	    let _ = memo := !graph in
	    (* Printf.fprintf stdout "Starting with the graph:\n" ; *)
	    (* let _ = print_graph !graph in  *)
	    let pets = total_order !graph in
	    (* Printf.fprintf stdout "Sorting, obtaining:\n" ;   *)
	    (* List.iter (fun pet -> print_pet stdout pet ; print_newline ()) pets ; *)
	    let nodes,node_of_pet_t,_ = poset_of_list pets in
	    (* Printf.fprintf stdout "Obtaining the poset:\n" ; *)
	    (* let _ = print_graph nodes in *)
	    (* let _ = flush stdout in *)
	    let fres x y = 
	      if x == y 
	      then 0 
	      else 
		let x' = PetMap.find x node_of_pet_t in 
		let y' = PetMap.find y node_of_pet_t in 
		(* Printf.fprintf stdout "Comparing:\n" ; *)
		(* let _ = print_graph [x';y'] in *)
		(* let _ = flush stdout in *)
		if List.mem y' x'.sons 
		then -1
		else
		  if List.mem x' y'.sons 
		  then 1
		  else begin
		    Printf.fprintf stderr 
		      "Patofig: there is probably a bug. A poset which should be linear isn't.\n" ;
		    Printf.fprintf stderr 
		      "%s and %s appear to be incomparable.\n Here is the list: " x.name y.name ;
		    List.iter (fun pet ->
		      Printf.fprintf stderr 
			"%s ;" pet.name) pets ;
		    flush stderr ;
		    1
		  end
	    in
	    let _ = memo_res := fres in
	    fres

      module Map = Map.Make (struct type t = u
				     let compare x y = compare () x y end)

      end

      open Style

      let transform styles default = 

	let make_transfos styles = 
	  List.fold_left 
	    (fun map style -> 
	      let pet = style.pet in 
	      let styles = if Pet.Map.mem pet map then Pet.Map.find pet map else [] in 
	      Pet.Map.add pet (styles @ [style]) map)
	    Pet.Map.empty
	    styles
	in

	let transfos = make_transfos styles in
	let x' = 
	  Pet.Map.fold (fun pet styles x' -> 
	    (List.fold_left (fun  x'' style -> 
	      style.transfo transfos x'')
	       x'
	       styles))
	    transfos
	    default
	in
	x'

    end

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


    (* Translate a gentity by v *)
    let translate v node =
      { curve = Curve.translate v node.curve ;
	anchor = (fun a -> Vector.(+) (node.anchor a) v) ;
	contents = let x,y = v in List.map (OutputCommon.translate x y) node.contents }


  (* Two important ways of constructing gentities are nodes and edges between them.  *)
  (* We start with nodes. *)

  module Node = struct

    type node_info = { 
      params : path_parameters ;
      mainAnchor : anchor ;

      innerSep : float ; 
      outerSep : float ;

      bb : float * float * float * float ;
      center : Point.t ;
      pdfAnchor: Point.t ;
      node_contents : OutputCommon.contents list ;

      (* textDepth : float ; *)
      (* textHeight : float ; *)
      (* textWidth : float ; *)

      innerCurve : Curve.t ;
      midCurve : Curve.t ;
      outerCurve : Curve.t ;
      anchors : anchor -> Point.t ;

      node_anchor : anchor ;

      at : Point.t ;
    }

    let default_params = { OutputCommon.default with close = false ; strokingColor=None ; 
      lineWidth = !default_line_width } 

    let default = { at = (0.,0.) ; node_anchor = `Pdf ;
		    mainAnchor = `Center ; 
		    center = (0.,0.) ;
		    pdfAnchor = (0.,0.) ;
		    innerSep = 1.; outerSep = 0. ;
		    innerCurve = [] ;
		    midCurve = [] ;
		    outerCurve = [] ;
		    bb = (0.,0.,0.,0.) ;
		    anchors = (fun _ -> (0.,0.)) ;
		    params = default_params ;
		    node_contents = [] ;
		    (* textDepth = 0. ; *)
		    (* textHeight = 0. ; *)
		  }

    let to_gentity info = 
      { curve = info.midCurve ; 
	anchor = info.anchors ;
	contents = info.node_contents @ (Curve.draw ~parameters:info.params info.midCurve)
      }

    module Transfo = Transfo (struct type t = node_info let compare = compare end)

    open Transfo
    open Style 

    let transfo ?dump_contents:(dump_contents=false) transfos info =
      let info = Transfo.transform transfos info in 
      { info with node_contents = (if dump_contents then [] else info.node_contents) }

    let params,params_pet = 
      Pet.register "node params" (fun pet params -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with params = params })})

    let lineWidth w = 
      { pet = params_pet ; 
	transfo = (fun transfos info -> 
	  { info with params = { info.params with lineWidth = w ;
	    strokingColor = Some black } } ) }

    let dashed,dashed_pet = 
      Pet.register "node dashed" (fun pet pattern -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with params = { info.params with
	    strokingColor = Some black ;
	    dashPattern = pattern } } ) })

    let draw,draw_pet = 
      Pet.register "node draw" (fun pet -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with params = { info.params with
	    strokingColor = Some black } } ) })

    let fill,fill_pet = 
      Pet.register "node fill" (fun pet color -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with params = { info.params with
	    fillColor = Some color } } ) })

    let color,color_pet = 
      Pet.register "node draw" ~depends:[dashed_pet;draw_pet;params_pet] 
	(fun pet color -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with params = { info.params with
	    strokingColor = Some color } } ) })

    let contents_outputcommon,contents_pet = 
      Pet.register "node contents" 
	(fun pet contents -> 
	  { pet = pet ; transfo = (fun transfos info -> 
	    (* Printf.fprintf stderr "contents\n" ;  *)
	    let (x0,y0,x1,y1) as bb = match contents with
	      | [] -> (0.,0.,0.,0.)
	      | _ -> OutputCommon.bounding_box contents 
	    in
	    (* let text_depth = -. y0 in *)
	    (* let text_height = y1 in *)
	    let center = Point.middle (x0,y0) (x1,y1) in 
	      { info with 
		center = center ;
		bb = bb ;
		(* textDepth = text_depth ; *)
		(* textHeight = text_height ; *)
		node_contents = contents }
	  )})

    let contents_box boxes = contents_outputcommon (Box.draw_boxes boxes)
    let contents env contents = contents_box (boxify_scoped env contents)

    let innerSep,inner_sep_pet = 
      Pet.register "inner sep" (fun pet sep -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with innerSep = sep })})

    let outerSep,outer_sep_pet = 
      Pet.register "outer sep" (fun pet sep -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with outerSep = sep })})

    let mainAnchor,main_anchor_pet = 
      Pet.register "node main anchor" (fun pet anchor -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "main anchor" ; flush stderr ; *)
	  (* let _ =  *)
	  (*   if anchor = `Base  *)
	  (*   then (Printf.fprintf stderr " (main anchor: `Base)\n" ; flush stderr) *)
	  (*   else if anchor = `Center then (Printf.fprintf stderr " (main anchor: `Center)\n" ; flush stderr)  *)
	  (* in *)
	  { info with mainAnchor = anchor })})

    module BB = struct 

      let translate (x,y) (x0,y0,x1,y1) =
	(x0 +. x, y0 +. y, x1 +. x, y1 +. y)

      let points  (x0,y0,x1,y1) =
	let p1 = (x0,y0) in
	let p2 = (x1,y0) in
	let p3 = (x1,y1) in
	let p4 = (x0,y1) in
	(p1,p2,p3,p4)
	  
      let outer_points style (x0,y0,x1,y1) =
	let inner_sep = style.innerSep in
	let outer_sep = style.outerSep in
	(* let line_width = Style.line_width style in *)
	let sep = inner_sep +. outer_sep in
	let x0 = x0 -. sep in
	let y0 = y0 -. sep in
	let x1 = x1 +. sep in
	let y1 = y1 +. sep in
	points (x0,y0,x1,y1)

      let mid_points style (x0,y0,x1,y1) =
	let inner_sep = style.innerSep in
	(* let line_width = style.lineWidth in *)
	let sep = inner_sep in
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

    let boundingBox,bounding_box_pet = 
      Pet.register "bounding box" ~depends:[contents_pet] (fun pet f -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with bb = f (info.bb) })})

    (* let textWidth,text_width_pet =  *)
    (*   Pet.register "text width" ~depends:[contents_pet] (fun pet w ->  *)
    (* 	{ pet = pet ; transfo = (fun transfos info ->  *)
    (* 	  let (x0,y0,x1,y1) = info.bb in *)
    (* 	  let bb' = (x0,y0,w, y1) in *)
    (* 	  { info with bb = bb' })}) *)

    (* let textHeight,text_height_pet =  *)
    (*   Pet.register "text height" ~depends:[contents_pet] (fun pet h ->  *)
    (* 	{ pet = pet ; transfo = (fun transfos info ->  *)
    (* 	  let (x0,y0,x1,y1) = info.bb in *)
    (* 	  let bb' = (x0,y0,x1,h) in *)
    (* 	  { info with bb = bb' })}) *)

    let (rectangle : user Document.environment -> Transfo.Style.t),
      shape_pet = 
      Pet.register "node shape" 
	~depends:[inner_sep_pet;outer_sep_pet;
		  (* text_height_pet;text_width_pet; *)
		  contents_pet;main_anchor_pet] 
	(fun pet env -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "node shape\n" ; flush stderr ; *)
	  let (x0,y0,x1,y1) as bb_boot = info.bb in
	  let rectangle (p1,p2,p3,p4) = 
	    Curve.of_point_lists [
	      [p1;p2] ;	(* The bottom curve *)
	      [p2;p3] ;	(* The right-hand curve *)
	      [p3;p4] ;	(* The top curve *)
	      [p4;p1] 	(* The left-hand curve *)
	    ]
	  in
	  let inner_curve = rectangle (BB.points bb_boot) in
	  let mid_curve = rectangle  (BB.mid_points info bb_boot) in
	  let outer_curve = rectangle (BB.outer_points info bb_boot) in	  

	  let (p1,p2,p3,p4) = BB.outer_points info bb_boot in
	  let text_depth = -. y0 in 
	  let inner_sep = info.innerSep  in 
	  let outer_sep = info.outerSep  in 
	  let south = Point.middle p1 p2 in
	  let ex = ex env in
	  let base = Vector.(+) south (0.,inner_sep +. outer_sep +. ex +. text_depth) in
	  let main = begin let main = info.mainAnchor 
			   in (if main = `Main then 
			       let _ = (Printf.fprintf stderr "Please do not choose `Main as a main anchor; 
`Main is used when drawing edges; 
it is `Base by default and you may change it, e.g., to `Center, using `MainAnchor `Center.\n" ; 
					flush stderr)
			       in
			       `Center
			     else main)
	  end
	  in
	  (* let _ =  *)
	  (*   if main = `Base  *)
	  (*   then (Printf.fprintf stderr " (main anchor: `Base)\n" ; flush stderr) *)
	  (*   else if main = `Center then (Printf.fprintf stderr " (main anchor: `Center)\n" ; flush stderr)  *)
	  (* in *)
	  let rec anchors = function
	    | `Vec v -> Vector.(+) (Point.middle p1 p3) v
	    | `Center -> info.center
	    | `Main -> anchors main
	    | `Base -> base
	    | `BaseEast -> (fst (Point.middle p2 p3),snd base)
	    | `BaseWest -> (fst (Point.middle p1 p4),snd base)
	    | `Line -> (fst base, (snd base -. ex))
	    | `LineEast -> (fst (Point.middle p2 p3),(snd (Vector.(+) (Point.middle p1 p3) info.pdfAnchor)))
	    | `LineWest -> (fst (Point.middle p1 p4), snd (Vector.(+) (Point.middle p1 p3) info.pdfAnchor))
	    | `East -> Point.middle p2 p3  
	    | `West -> Point.middle p1 p4  
	    | `North -> Point.middle p3 p4
	    | `South -> south
	    | `SouthWest -> p1
	    | `SouthEast -> p2
	    | `NorthEast -> p3
	    | `NorthWest -> p4
	    | `Pdf -> info.pdfAnchor
	    | _ -> Printf.fprintf stderr "Anchor undefined for a rectangle. Returning the center instead.\n" ; 
	      Point.middle p1 p3
	  in
	  { info with 
	    innerCurve = inner_curve ;
	    midCurve = mid_curve ;
	    outerCurve = outer_curve ;
	    anchors = anchors
	  })})

    let (default_shape : user Document.environment -> Transfo.Style.t),
      default_shape_pet = 
      Pet.register "node default shape" 
	~depends:[shape_pet] 
	(fun pet env -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "default node shape\n" ;  *)
	  if info.midCurve = [] then (rectangle env).transfo transfos info else info) })

    let kappa = 0.5522847498

    let circle =
	{ pet = shape_pet ; transfo = (fun transfos info -> 
	  let (_,y0,_,_) as bb = info.bb in
	  let center = info.center in
	  let circle (p1,p2,p3,p4) = 

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

	  in

	  let inner_curve = circle (BB.points bb) in
	  let mid_curve = circle  (BB.mid_points info bb) in
	  let outer_curve = circle (BB.outer_points info bb) in	  

	  let (p1,p2,p3,p4) = BB.outer_points info bb in
	  let text_depth = -. y0 in 
	  let inner_sep = info.innerSep  in 
	  let outer_sep = info.outerSep  in 
	  let south = Point.middle p1 p2 in
	  let base = Vector.(+) south (0.,inner_sep +. outer_sep +. text_depth) in
	  let main = begin let main = info.mainAnchor 
			   in (if main = `Main then 
			       let _ = (Printf.fprintf stderr "Please do not choose `Main as a main anchor; 
`Main is used when drawing edges; 
it is `Base by default and you may change it, e.g., to `Center, using `MainAnchor `Center.\n" ; 
					flush stderr)
			       in
			       `Center
			     else main)
	  end
	  in

	  let rec anchors = function
	    | `Vec v -> Vector.(+) (Point.middle p1 p3) v
	    | `Center -> Point.middle p1 p3
	    | `Main -> anchors main
	    | `Base -> base
	    | `BaseEast -> (fst (Point.middle p2 p3),snd base)
	    | `BaseWest -> (fst (Point.middle p1 p4),snd base)
	    | `Line -> (fst base, (snd base -. ex))
	    | `LineEast -> (fst (Point.middle p2 p3),(snd (Vector.(+) (Point.middle p1 p3) info.pdfAnchor)))
	    | `LineWest -> (fst (Point.middle p1 p4), snd (Vector.(+) (Point.middle p1 p3) info.pdfAnchor))
	    | `East -> Point.middle p2 p3  
	    | `West -> Point.middle p1 p4  
	    | `North -> Point.middle p3 p4
	    | `South -> south
	    | `SouthWest -> p1
	    | `SouthEast -> p2
	    | `NorthEast -> p3
	    | `NorthWest -> p4
	    | `Pdf -> info.pdfAnchor
	    | _ -> Printf.fprintf stderr "Anchor undefined for a rectangle. Returning the center instead.\n" ; 
	      Point.middle p1 p3
	  in
	  { info with 
	    innerCurve = inner_curve ;
	    midCurve = mid_curve ;
	    outerCurve = outer_curve ;
	    anchors = anchors
	  })}

    let translate ((xt,yt) as v) info = 
	  let bb = BB.translate v info.bb in
	  let center = Vector.(+) info.center v in
	  let pdfAnchor = Vector.(+) info.pdfAnchor v in
	  let contents = List.map (OutputCommon.translate xt yt) info.node_contents in (* Center the contents *)
	  let innerCurve = Curve.translate v info.innerCurve in
	  let midCurve = Curve.translate v info.midCurve in
	  let outerCurve = Curve.translate v info.outerCurve in
	  let anchors anchor = Vector.(+) v (info.anchors anchor) in
	  let at = Vector.(+) v info.at in
	  { info with 
	    bb = bb ;
	    center = center ;
	    pdfAnchor = pdfAnchor ;
	    node_contents = contents ;
	    innerCurve = innerCurve ;
	    outerCurve = outerCurve ;
	    midCurve = midCurve ;
	    anchors = anchors ;
	    at = at }

    let anchor,anchor_pet = 
      Pet.register "node anchor" ~depends:[shape_pet] (fun pet anchor -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "anchor\n" ; flush stderr ; *)
	  let (xt,yt) as v = Vector.of_points (info.anchors anchor) (info.anchors info.node_anchor) in
	  let info' = translate v info in
	  { info' with
	    node_anchor = anchor })})

    let at,at_pet = 
      Pet.register "node at" ~depends:[anchor_pet] (fun pet point -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "node at\n" ; flush stderr ; *)
	  translate point info)})

    let make_output styles cont =
      let info = Transfo.transform
	((contents_outputcommon cont) :: anchor `Center :: mainAnchor `Center :: styles) 
	default 
      in
      to_gentity info
    let make_boxified styles cont = make_output styles (Box.draw_boxes cont)
    let make env styles cont = make_boxified styles (boxify_scoped env cont)

end


    module Matrix = struct
      type 'a matrix = 'a array array
      let map f = Array.map (Array.map f) 
      let mapi f = Array.mapi (fun i line -> Array.mapi (f i) line)

      type info = 
	  { mainNode : Node.node_info ;
	    common : Node.Transfo.Style.t list ;
	    nodes : Node.node_info matrix ;
	    placement : info -> int -> int -> Point.t }

      module Transfo = Transfo (struct type t = info let compare = compare end)
      module T = Transfo
      module S = T.Style
      open Node
      open T
      open S

      let default_matrix_node env = Node.([mainAnchor `Base ; anchor `Base ; rectangle env])

      let between_centers disty distx _ i j =
	(float_of_int j *. distx), -. (float_of_int i *. disty) 

      let default env = { mainNode = Node.default ; common = default_matrix_node env ; nodes = [||] ;
			placement = (between_centers 20. 20.)}

      let to_gentities { mainNode = main ; nodes = nodes } = (Node.to_gentity main, map Node.to_gentity nodes)

      let nodes_contents info = 
	List.flatten 
	  (Array.to_list 
	     (Array.map 
		(fun line -> 
		  List.flatten 
		    (Array.to_list 
		       (Array.map 
			  (fun node_info -> (Node.to_gentity node_info).contents) 
			  line)))
		info.nodes))

      let to_contents info = (nodes_contents info) @ (Node.to_gentity info.mainNode).contents

      let (allNodes : Node.Transfo.Style.t list -> S.t),
	all_nodes_pet = 
      Pet.register "matrix all nodes" (fun pet node_transfos -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "allNodes \n" ; flush stderr ; *)
	  { info with 
	    common =  info.common @ node_transfos })})

      let makeNodes,make_nodes_pet = 
      Pet.register "matrix make nodes" ~depends:[all_nodes_pet] (fun pet lines -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "makeNodes \n" ; flush stderr ; *)
	  let width = List.fold_left
	    (fun res line -> max res (List.length line))
	    0
	    lines
	  in
	  let height = List.length lines in
	  (* On commence par placer les noeuds de sorte que le noeud (0,0) ait son centre en (0,0) *)
	  let res = Array.make_matrix height width Node.default in
	  let i = ref 0 in
	  let j = ref 0 in
	  let _ = List.iter (fun line -> 
	    List.iter (fun node_transfo -> 
	      let info_ij = Node.Transfo.transform (info.common @ node_transfo) res.(!i).(!j)
	      in
	      res.(!i).(!j) <- info_ij ;
	      incr j)
	      line ;
	    incr i ; j := 0)
	    lines 
	  in
	  { info with nodes = res })})

      let contents_output lines =
	let lines' = 
	  List.map (List.map (fun (style,contents) -> ((Node.contents_outputcommon contents)::style))) lines
	in
	makeNodes lines'
      let contents_box lines =
	let lines' = 
	  List.map 
	    (List.map 
	       (fun (style,contents) -> 
		 ((Node.contents_outputcommon (Box.draw_boxes contents))::style)))
	    lines
	in
	makeNodes lines'

      let contents env lines =
	let lines' = 
	  List.map 
	    (List.map 
	       (fun (style,contents) -> 
		 ((Node.contents_outputcommon (Box.draw_boxes (boxify_scoped env contents)))::style)))
	    lines
	in
	makeNodes lines'

      let placement,placement_pet = 
	Pet.register "matrix placement" ~depends:[all_nodes_pet;make_nodes_pet] (fun pet placement -> 
	  { pet = pet ; transfo = (fun transfos info -> 
	    (* Printf.fprintf stderr "matrix placement \n" ; flush stderr ; *)
	    { info with placement = placement })})

      let mainNodeContents,main_node_contents_pet =
	Pet.register "matrix main node contents" ~depends:[placement_pet] (fun pet -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  let nodes_contents = nodes_contents info in
	  let node_info = Node.Transfo.transform
	    [Node.contents_outputcommon nodes_contents]
	    info.mainNode 
	  in
	  { info with mainNode = node_info })})
	  
      let mainNode,main_node_pet = 
      Pet.register "matrix main node" ~depends:[placement_pet] (fun pet node_transfos -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  let node_info = Node.Transfo.transform node_transfos info.mainNode in
	  { info with mainNode = node_info })})

      let wrap,wrap_pet = 
	Pet.register "matrix wrap" ~depends:[main_node_contents_pet; main_node_pet] (fun pet ->
	  { pet = pet ; 
	    transfo = (fun transfos info -> 
	      let info' = 
		{ info with nodes = mapi (fun i j node_info -> 
		  Node.translate (info.placement info i j) node_info) info.nodes } 
	      in 
	      let node_info' = { info.mainNode with node_contents = [] } in
	      let pdf_start = info'.nodes.(0).(0).anchors `Pdf in
	      let pdf_end = node_info'.pdfAnchor in
	      let v = Vector.of_points pdf_start pdf_end in
	      let nodes = map (Node.translate v) info'.nodes in
	      { info' with mainNode = node_info' ;
		nodes = nodes })})

      let centers y x = placement (between_centers y x)

      let make env style lines =
	let info = T.transform 
	  (contents env lines :: 
	     mainNode [rectangle env] :: 
	     mainNodeContents ::
	     allNodes [] ::
	     wrap ::
	     style) (default env) in
	let m,ms = to_gentities info in
	m, ms

    end

    module Edge = struct

      type tip_info = { tip_line_width : float ; is_double : bool }

      type edge_info = { tip_info : tip_info ; 
			 start : gentity ;
			 finish : gentity ;
			 params : path_parameters ; 
			 underlying_curve : Curve.t ;
			 curves : (path_parameters * Curve.t) list ;
			 decorations : (path_parameters * Curve.t) list }
      type edge_transfo = edge_info -> edge_info

      (* module rec PTDef : sig *)
      (* 	module type ParameterisedEdgeTransfo = sig *)
      (* 	  type t  *)
      (* 	  val name : string  *)
      (* 	  val transfo : t -> edge_transfo *)
      (* 	end *)
      (* 	type parameterised_edge_transfo = (module PTDef.ParameterisedEdgeTransfo) *)
      (* end = PTDef	     *)


      module Transfo = Transfo (struct type t = edge_info let compare = compare end)
      open Transfo 
      open Style

      let default_tip_info = { tip_line_width = !default_line_width ; is_double = false }

      let default_params = { OutputCommon.default with 
	strokingColor = Some black }

      let default_edge_info s e underlying_curve = 
	(* let tip_info = *)
	(*   match Style.double styles with *)
	(*     | Some margin -> { tip_line_width = margin +. 2. *. Style.line_width styles ; *)
	(* 		       is_double = true } *)
	(*     | None -> { tip_line_width = Style.line_width styles ; *)
	(* 		is_double = false } *)
	(* in *)
	{ tip_info = default_tip_info ;
	  start = s ; finish = e ;
	  params = default_params ;
	  underlying_curve = underlying_curve ;
	  curves = [] ; 
	  decorations = [] }

      let transform styles s e underlying_curve = 
	let edge_info = Transfo.transform styles (default_edge_info s e underlying_curve)  in
	edge_info

      let outer_curve styles curve = curve

      (* We now define ways of constructing edges *)

      type continue_path_spec = Point.t list 
      and path_spec = continue_path_spec list 

      let point_lists_of_path_spec s continues =
	let rec point_lists_of_path_spec_rec res s continues = 
	  match continues with
	    | [] -> List.rev res
	    | e :: rest -> point_lists_of_path_spec_rec ((s :: e) :: res) (list_last e) rest
	in
	point_lists_of_path_spec_rec [] s continues

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

      let raw_edge style s e underlying_curve =
	let edge_info = transform style s e underlying_curve in
	let curve = edge_info.underlying_curve in
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
	{ anchor = anchor ; curve = curve ; 
	  contents = 
	    (List.flatten (List.map (fun (params,curve) -> (Curve.draw ~parameters:params curve)) 
			     edge_info.curves))
	  @ (List.flatten (List.map (fun (params,curve) -> (Curve.draw ~parameters:params curve)) 
			     edge_info.decorations))
	}


      let path_of_curve style curve = 
	let s = coord (Curve.first curve) in 
	let e = coord (Curve.last curve) in 
	raw_edge style s e curve

      let path style s continues = 		
	let curve = (Curve.of_point_lists (point_lists_of_path_spec s continues)) in
	path_of_curve style curve

      (* ******************************************************** *)
      (* We start defining new edge transfos and registering them *)
      (* ******************************************************** *)

      let do_clip curve node1 node2 = 
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

      let clip, clip_pet = 
	Pet.register "clip" (fun pet ->
	  { pet = pet ; transfo = (fun transfos info -> 
	    { info with underlying_curve = do_clip info.underlying_curve info.start info.finish }) })

      let draw, draw_pet =
	Pet.register ~depends:[clip_pet] "draw edge" (fun pet ->
	  { pet = pet ;
	    transfo = (fun transfos edge_info -> 
	      { edge_info with curves = [ edge_info.params, edge_info.underlying_curve ] })
	  })

      let make style s ?controls:(controls=[]) e = 
	let point_lists = point_lists_of_edge_spec s controls e in
	let underlying_curve = Curve.of_point_lists point_lists in
	raw_edge (clip :: style) s e underlying_curve

      let makes style edge_list = 
	List.map (fun (style',s,controls,e) -> make (style' @ style) s ~controls:controls e) edge_list

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

      let do_squiggle freq angle (xs,ys) = 
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

      let squiggle, squiggle_pet =
	Pet.register ~depends:[draw_pet] "squiggle" 
	  (fun pet freq amplitude a b ->
	    { pet = pet ;
	      transfo = 
		(fun transfos edge_info -> 
		  let squiggle (params, curve) =
		    let curve1,curve,curve2 = Curve.split2 curve a b in
		    (params, curve1 @ (List.flatten (List.map (do_squiggle freq amplitude) curve)) @ curve2)
		  in
		  let params', u_curve = squiggle (edge_info.params, edge_info.underlying_curve) in
		  { edge_info with params = params' ; underlying_curve = u_curve ;
		    curves = List.map squiggle edge_info.curves }
		)})

      let shorten, shorten_pet = Pet.register ~depends:[clip_pet] "shorten" (fun pet a b ->
	{ pet = pet ; transfo = (fun transfos info ->
	  let shorten (params',curve') = 
	    let ta = Curve.curvilinear curve' a in
	    let tb = Curve.curvilinear curve' (-. b) in
	    params', Curve.restrict curve' ta tb
	  in
	  let _, u_curve = shorten (info.params, info.underlying_curve) in
	  { info with underlying_curve = u_curve ; curves = List.map shorten info.curves })})

      let shortenS a = shorten a 0.
      let shortenE b = shorten 0. b

      let foreground, foreground_pet = 
	Pet.register ~depends:[draw_pet;shorten_pet] "foreground" (fun pet margin ->
	  { pet = pet ; transfo = (fun transfos info -> 
	    let white_paths = List.map (fun (params, curve) -> 
	      { info.params with 
		Drivers.strokingColor=Some (Drivers.RGB { Drivers.red=1.;Drivers.green=1.;Drivers.blue=1. }); 
		Drivers.lineWidth=params.Drivers.lineWidth +. 2. *. margin },
	      curve)
	      info.curves
	    in
	    let edge_info' = 
	      Transfo.transform [shorten 0.1 0.1] { info with curves = white_paths }   in
	    { info with curves = (edge_info'.curves @ info.curves) }) })

      let double, double_pet = Pet.register ~depends:[draw_pet] "double" (fun pet margin -> 
	{ pet = pet ; transfo = (fun transfos info ->
	  let black_paths = List.map (fun (params, curve) -> 
	    { params with 
	      lineWidth = margin +. 2. *. params.lineWidth },
	    curve)
	    info.curves
	  in
	  let white_paths = List.map (fun (params, curve) -> 
	    { params with 
	      strokingColor = Some (RGB { red=1.;green=1.;blue=1. }); 
	      lineWidth = margin },
	    curve)
	    info.curves
	  in
	  let delta = 0.02 in
	  let info_white = Transfo.transform [shorten delta delta] { info with curves = white_paths } in
	  let info_black = Transfo.transform [shorten delta delta] { info with curves = black_paths } in
	  { info_black with 
	    tip_info = { info.tip_info with tip_line_width = margin +. 2.0 *. info.params.lineWidth };
	    curves = (info_black.curves @ info_white.curves) }) })

      let arrowOf, arrow_head_pet = 
	Pet.register ~depends:[double_pet;shorten_pet] "arrow head" (fun pet head_params -> 
	  { pet = pet ; transfo = (fun transfos edge_info -> 
	    let info = edge_info.tip_info in
	    let params = edge_info.params in
	    let underlying_curve = edge_info.underlying_curve in
	    let (da,db) as grad = Curve.eval (Curve.gradient underlying_curve) 1. in
	    let short, thickness, height, width, lw = head_params info params in
	    let thickness' = thickness -. thickness *. info.tip_line_width /. 2. /. width in

	    (* Control points on the curve *)
	    let (xe,ye) as e = Curve.eval underlying_curve 1. in
	    (* let _ = Printf.fprintf stderr "Shortening by %f.\n" short ; flush stderr in *)
	    let edge_info' = Transfo.transform [shortenE short] edge_info in
	    let curve0 = edge_info'.underlying_curve in
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
	    { edge_info with decorations = edge_info.decorations @
		[({ params with 
		  close = true ; 
		  fillColor = params.strokingColor ; 
		  lineWidth = lw }, tip)]})})

      let arrow = arrowOf head_moustache

      let modToOf,mod_to_of_pet = Pet.register ~depends:[draw_pet] "mod to"
	(fun pet time width ->
	  { pet = pet ; transfo = (fun transfos edge_info ->
	    let edge_info' = Transfo.transform [arrow] edge_info in
	    let underlying_curve = edge_info'.underlying_curve in
	    let middle = Curve.eval underlying_curve time in
	    let (da,db) = Curve.eval (Curve.gradient underlying_curve) time in
	    let (la,lb) as l = (-. db, da) in
	    let (ra,rb) as r = (db, -. da) in
	    let (la,lb) as l = Vector.normalise ~norm:width l in
	    let (ra,rb) as r = Vector.normalise ~norm:width r in
	    let dash = Curve.of_point_lists [[(Vector.(+) l middle);(Vector.(+) r middle)]] in 
	    { edge_info' with decorations = edge_info'.decorations @ [edge_info'.params, dash] }
	  )})

      let modTo = modToOf 0.5 1.

      let bendOf,bend_pet = Pet.register ~codepends:[clip_pet] "bend"
	(fun pet angle ->
	  { pet = pet ; transfo = (fun transfos edge_info ->
	    let curve = edge_info.underlying_curve in
	    let curve' = begin
	      if Curve.nb_beziers curve = 1 then
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
	    end
	    in { edge_info with underlying_curve = curve' } ) } )

      let bendLeft = bendOf
      let bendRight angle = bendOf (-. angle)

      let (paramsOf : user Document.environment -> Transfo.Style.t),
	params_pet = Pet.register ~codepends:[clip_pet] "params"
	(fun pet params ->
	  { pet = pet ; transfo = (fun transfos edge_info ->
	    { edge_info with params = edge_info.params })})

      let dashed pattern =
	{ pet = params_pet ; transfo = (fun transfos edge_info ->
	  { edge_info with params = { edge_info.params with dashPattern = pattern }})}

      let fill = 
	{ pet = params_pet ; transfo = (fun transfos edge_info ->
	  { edge_info with params = { edge_info.params with close = true ; fillColor = Some black }})}

      let color c = 
	{ pet = params_pet ; transfo = (fun transfos edge_info ->
	  { edge_info with params = { edge_info.params with strokingColor = Some c }})}

      let black = color black 

      let lineWidth w = 
	{ pet = params_pet ; transfo = (fun transfos edge_info ->
	  { edge_info with params = { edge_info.params  with lineWidth = w }})}



    end
      



    module Env_Diagram (Args : sig val arg1 : string end)(Args' : sig val env : user environment end) = struct
      let stack = ref []
      let env = Args'.env
      let offset = ref 0.


      let node style contents = 
	let a = Node.(make env (default_shape env :: style)) contents in
	stack := a :: !stack ;
	a

      let coordinate p = 
	let a = coord p in
	let _ = stack := a :: !stack in
	a

      let edge style a ?controls:(controls=[]) b =
	let e = Edge.make style a ~controls:controls b in
	stack := e :: !stack ;
	e

      let path style s continues =
	let e = Edge.path style s continues in
	stack := e :: !stack ;
	e

      let edges style edge_list = 
	let l = Edge.makes style edge_list in
	stack := l @ !stack ;
	l

      let matrix style lines = 
	let node, m = Matrix.make env style lines in
	stack := node :: ((List.flatten (Array.to_list (Array.map Array.to_list m))) @ !stack) ;
	node, m

      let make () = 
	let fig = Box.drawing ~offset:(!offset) 
	  (List.fold_left (fun res gentity -> List.rev_append gentity.contents res)
	     []
	     !stack)
	in
	stack := [] ; fig

      open Node
      open Edge

      let label e style pos contents = 
	node  ((Node.at (e.anchor (`Temporal pos))) :: style) 
	  ([Scoped ((fun env -> { env with mathStyle = Mathematical.Script }), contents)])

      let label_anchor e anchor pos contents = 
	label e [Node.anchor anchor] pos contents

      let labela ?style:(style=[]) e contents = label e (anchor `South :: style) 0.5 contents
      let labelb ?style:(style=[]) e contents = label e (anchor `North :: style) 0.5 contents
      let labell ?style:(style=[]) e contents = label e (anchor `East :: style) 0.5 contents
      let labelr ?style:(style=[]) e contents = label e (anchor `West :: style) 0.5 contents
      let labelbr ?style:(style=[]) e contents = label e (anchor `NorthWest :: style) 0.5 contents
      let labelbl ?style:(style=[]) e contents = label e (anchor `NorthEast :: style) 0.5 contents
      let labelar ?style:(style=[]) e contents = label e (anchor `SouthWest :: style) 0.5 contents
      let labelal ?style:(style=[]) e contents = label e (anchor `SouthEast :: style) 0.5 contents
      let labelc ?style:(style=[]) e contents = label e (anchor `Main :: style) 0.5 contents

      let edge_anchor a b style anchor pos contents = 
	let e = edge (arrow :: draw :: style) a b in
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

	let between_borders vpad hpad styles info = 
	  let nodes = info.Matrix.nodes in 
	  let matrix = Matrix.mapi 
	    (fun i j node_info -> Node.Transfo.transform (styles i j) node_info) nodes 
	  in
	  let width = Array.length matrix.(0) in
	  let height = Array.length matrix in
	  let widths = Array.make width 0. in
	  let heights = Array.make height 0. in
	  let _ = for j = 1 to width - 1 do 
	      widths.(j) <- 	widths.(j-1)
	      +. (hpad j)
	      +. (fun_max (fun i -> fst (matrix.(i).(j-1).anchors `East)(* base_east (matrix.(i).(j-1)) *)) height)
	      -. (fun_max (fun i -> fst (matrix.(i).(j).anchors `West) (* west_base (matrix.(i).(j)) *)) height)
	    done ;
	  in
	  let _ = 
	    for i = 1 to height - 1 do 
	      heights.(i) <- begin
		(* The height of the previous row *)
		heights.(i-1)
		(* plus the given vertical space *)
		-. (vpad i)
		(* plus the least y0 of the previous row *)
		+. (fun_max ~max:(fun y y' -> min y y') (fun j -> snd (matrix.(i-1).(j).anchors `South)) width)
		(*  minus the greatest y1 of the present row (because we go downwards) *)
		-. (fun_max (fun j -> snd (matrix.(i).(j).anchors `North)) width)
	      end ;
	    done ;
	  in fun i j -> (widths.(j), heights.(i))

	let array anchors ?vertical_padding:(vpad=fun _ -> 1.) ?horizontal_padding:(hpad=fun _ -> 1.)
	    (* Mettre la valeur par defaut en ex *)
	    lines =
	  let style i j = [Node.anchor (List.nth anchors j)] in
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
	  math_matrix [Matrix.placement (between_borders vpad hpad style);
		       Matrix.mainNode [at (0.,0.);anchor `SouthWest]] 
	    lines_contents 


      end

      let array = Arr.array

    end


    open Box
    let xto ?margin:(margin=2.) a =
      [Maths.Binary { Maths.bin_priority = 0 ; Maths.bin_drawing = Maths.Normal 
	  (true, 
           (Maths.noad
              (fun env st->
		let dr=Box.draw_boxes (Maths.draw [{env with mathStyle = Mathematical.Script}] a) in
		let (x0,y0,x1,y1)=OutputCommon.bounding_box dr in
		let _ = Printf.fprintf stderr "Bb: %f,%f,%f,%f\n" x0 y0 x1 y1 ; flush stderr in
		let m,ms = Matrix.(make env
		  [placement (between_centers 1. (x1 -. x0 +. 2. *. margin));
		   mainNode Node.([
		   anchor `Pdf;
		   innerSep 0. ; outerSep 0. ;
		   at (-. 0.75 *. env.size *. (ex env), 0.)])] 
		  Node.([[
		     ([innerSep 0.;outerSep 0.], []) ; 
		     ([innerSep 0.;outerSep 0.], [])
		   ]]))
		in
		let e = Edge.(make [draw;lineWidth 0.1;arrow]) ms.(0).(0) ms.(0).(1) in
		let l = Node.(make_output
		  [outerSep 0.2 ; innerSep 0.; anchor `South; 
		   default_shape env ; at (e.anchor (`Temporal 0.5))] dr)
		in
		let drawn = 
		  drawing_inline
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
		let m,ms = Matrix.(make env
		  [placement (between_centers 1. (x1 -. x0 +. 2. *. margin));
		   mainNode Node.([
		     anchor `Pdf;
		     innerSep 0. ; outerSep 0. ;
		     at (-. 0.75 *. env.size *. (ex env),0.)])]\
		     Node.([[
		     ([innerSep 0.;outerSep 0.], []) ; 
		     ([innerSep 0.;outerSep 0.], [])
		   ]]))
		in
		let e = Edge.(make [draw;lineWidth 0.1;arrow] ms.(0).(1) ms.(0).(0)) in
		let l = Node.(make_output
		  [outerSep 0.2 ; innerSep 0.; default_shape env ; 
		   anchor `South; at (e.anchor (`Temporal 0.5))] dr) 
		in
		let drawn = 
		  drawing_inline 
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
