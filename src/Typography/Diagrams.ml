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
open Document
module Drivers = OutputCommon
open OutputCommon
open Proj3d
open Geometry

let swap (x,y) = (y,x)
let pi = 4. *. atan 1.
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
  in (Box.upper_y x -. Box.lower_y x) /. 2.

let rec mem_compare cmp x l = match l with 
  | [] -> false 
  | y :: l' -> if cmp x y = 0 then true else mem_compare cmp x l'

let rec list_last = function
  [] -> assert false
  | [x] -> x
  | x :: l -> list_last l

let only_last l x = [x]

let array_last xs = 
  let n = Array.length xs in 
  xs.(n-1)

let list_split_last_rev l = 
  let rec list_split_last_rec res = function
    | [] -> assert false
    | [x] -> res, x
    | x :: l -> list_split_last_rec (x :: res) l
  in list_split_last_rec [] l

let app_default f x y default = 
  match f x y with
    | Some res -> res
    | None -> 
      let _ = Printf.fprintf stderr "Warning: empty intersection list, returning default anchor.\n" ; 
	flush stderr 
      in
      default

let max_list f z l = List.fold_left 
  (fun res x -> max res (f x))
  z
  l

let max_list_list f z ls = 
  List.fold_left 
    (fun res l -> max res (max_list f z l))
    z
    ls

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

  let sector v v' = 
    let x = mod_float (angle v' -. angle v) 360. in
    if x < 0.0 then x +. 360. else x (* mod_float keeps the sign of the first argument *)

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
    if a = 1. then 
      (n-1,1.)
    else
    let a' = a *. (float_of_int n) in
    let i_float = floor a' in
    let i = int_of_float i_float in
    let t = a' -. i_float in
    (i, t)

  let compare_lt (i,t) (j,u) = 
    let k = compare i j in
    if k <> 0 then k else 
      compare t u

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

  let intersections_aux bezier1 e1 beziers2 = 
    let _,inters = List.fold_left (fun (i,inters) (bezier2_i, e2i) -> 
      let inters_i = Bezier.intersect' bezier1 e1 bezier2_i e2i in	
      (succ i, inters @ (List.map (fun (t1,t2) -> (t1,(i,t2,bezier2_i))) inters_i)))
      (0,[])
      beziers2
    in inters

  let intersections beziers1 beziers2 = 
    let _, res = 
      List.fold_left
	(fun (i, res) (bezier1, e1) -> 
	  ((succ i),
	   (res @ (List.map (fun (t1,gt2) -> ((i,t1,bezier1),gt2)) (intersections_aux bezier1 e1 beziers2)))))
	(0,[])
	beziers1
    in res

  let print_point_lists points = 	
    let _ = Printf.fprintf stderr "Points: \n"  in
    let _ = List.iter (fun (xs,ys) ->  
      let _ = Array.iter (fun x -> Printf.fprintf stderr "%f ; " x) xs in
      Printf.fprintf stderr "\n" ;
      let _ = Array.iter (fun x -> Printf.fprintf stderr "%f ; " x) ys in
      Printf.fprintf stderr "\n")
      points in
    let _ = flush stderr in

    ()


  let latest_intersection beziers1 beziers2 =
    (* Printf.fprintf stderr "Yoho!\n" ;  *)
    (* print_point_lists beziers1 ;  *)
    (* print_point_lists beziers2 ;  *)
    (* Printf.fprintf stderr "Tchow!\n" ; flush stderr ; *)
    let beziers1 = List.map (fun b  -> b, Bezier.extremity b) beziers1 in
    let beziers2 = List.map (fun b  -> b, Bezier.extremity b) beziers2 in
    let inters = (intersections beziers1 beziers2) in
    let latest = List.fold_left 
      (fun yet ((i,t,_),_) -> match yet with
	| None -> Some (i,t)
	| Some (_,_) -> Some (i,t))
      None
      inters
    in latest


  let earliest_intersection beziers1 beziers2 =
    let beziers1 = List.map (fun b  -> b, Bezier.extremity b) beziers1 in
    let beziers2 = List.map (fun b  -> b, Bezier.extremity b) beziers2 in
    match (intersections beziers1 beziers2) with
      | [] -> None
      | ((i,t,_),_) :: _ -> Some (i,t)

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

  let eval_local beziers (i,t) = (* eval beziers (global_time beziers lt) *)
    try
      let bezier = List.nth beziers i in
      bezier_evaluate bezier t
    with _ -> begin
      Printf.fprintf stderr ("Warning: attempt to evaluate an empty curve. Returning (0,0).\n") ;
      0.,0.
    end



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
	if z_restant >= length then
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
	if z_restant >= length then
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
	      if (node.color = Visiting) || (List.for_all (fun son -> son.color = Visited) node.sons)
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
	val append : t -> Style.t list -> Style.t -> Style.t list
	val (=) : t -> t -> bool
	val register : ?depends:t list -> ?codepends: t list 
	  -> ?append:(Style.t list -> Style.t -> Style.t list) 
	  -> string -> (t -> 'a) -> ('a * t)
	module Map : Map.S with type key = t
      end = struct

	open Style

	(* module type T = sig type arg val transfo : arg -> transfo end *)
	(* type t = module PT *)

	type t = { uid : int ; name : string ;  append : Style.t list -> Style.t -> Style.t list }
	type u = t

	let name x = x.name
	let append pet = pet.append
	let (=) x y = (x.uid = y.uid)

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

	let compare_node node1 node2 = UPet.compare node1.id node2.id

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
	let register ?depends:(depends=[]) ?codepends:(codepends=[]) 
	    ?append:(append=(fun l x -> l @ [x])) name (f : t -> 'a) = 
	  let uid = gensym () in
	  let pet = {uid = uid;name=name;append=append} in
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
	  if !memo == !graph then !memo_res
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
		if mem_compare compare_node y' x'.sons 
		then -1
		else
		  if mem_compare compare_node x' y'.sons 
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

	      (* Here, it is not obvious what to do. Should styles
		 override each other, or should they be applied
		 iteratively? *)
	      (* The initial behaviour was the latter, which is useful, e.g., to
		 accumulate styles for the main node of a matrix *)
	      (* However, it is sometimes undesirable, e.g., for
		 edges, the "arrow" style would draw two arrow heads,
		 which, when combined with the "double" style, could
		 be quite distant from each other because each
		 shortens the involved edge. *)
	      (* Hence, pets carry their own "append" function, which leaves the choice open *)

	      Pet.Map.add pet (Pet.append pet styles style) map)

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

	       | `Vertex of int
	       | `Apex
	       | `Edge of int

	       | `A | `B | `C | `D
	       ]

  module Gentity = struct
  type t = { curve : Curve.t ;	(* The curve is used to determine the start and end of edges *)
	     anchor : anchor -> Point.t ; (* Anchors are used for relative placement *)
	     contents : OutputCommon.raw list (* What's needed to actually draw the node *)
	   } 
  end
  type gentity = Gentity.t
  open Gentity

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

    type info = { 
      params : path_parameters ;
      mainAnchor : anchor ;

      innerSep : float ; 
      outerSep : float ;

      bb : float * float * float * float ;
      center : Point.t ;
      pdfAnchor: Point.t ;
      node_contents : OutputCommon.raw list ;
      
      button : (string * string list) option;
      (* textDepth : float ; *)
      (* textHeight : float ; *)
      (* textWidth : float ; *)

      innerCurve : Curve.t ;
      midCurve : Curve.t ;
      outerCurve : Curve.t ;
      anchor : anchor -> Point.t ;

      decorations : decoration list; 
      node_anchor : anchor ;

      at : Point.t ;
      z : float
    }

    and decoration = Curve of path_parameters * Curve.t
		       | Node of info

    type t = info

    let default_params = { OutputCommon.default with close = false ; strokingColor=None ; 
      lineWidth = !default_line_width } 

    let default = { at = (0.,0.) ; 
		    z = 0. ;
		    node_anchor = `Pdf ;
		    mainAnchor = `Center ; 
		    center = (0.,0.) ;
		    pdfAnchor = (0.,0.) ;
		    innerSep = 1.; outerSep = 0. ;
		    innerCurve = [] ;
		    midCurve = [] ;
		    outerCurve = [] ;
		    decorations = [];
		    button = None;
		    bb = (0.,0.,0.,0.) ;
		    anchor = (fun _ -> (0.,0.)) ;
		    params = default_params ;
		    node_contents = [] ;
		    (* textDepth = 0. ; *)
		    (* textHeight = 0. ; *)
		  }

    let coord ((x,y) as p : Point.t) = 
      { default with 
	at = p ;
	center = p ;
	pdfAnchor = p ;
	innerSep = 0. ;
	innerCurve = Curve.of_point_lists [[p]] ;
	outerCurve = Curve.of_point_lists [[p]] ;
	bb = (x,y,x,y) ;
	anchor = (fun _ -> p) }

    let rec decoration_to_contents edge_info = function
      | Curve (params, curve) -> Curve.draw ~parameters:params curve
      | Node node -> to_contents node

    and to_contents info =
      let c = (Curve.draw ~parameters:info.params info.midCurve) @ info.node_contents in
      let c = match info.button with
	None -> c
      | Some(n,d) -> 
	let (x0,y0,x1,y1) = bounding_box c in
	[Link { link_x0 = x0; link_y0 = y0; link_x1 = x1; link_y1 = y1;
		link_closed = true; link_order = 0; link_kind = Button(n,d);
		link_contents = c }]
      in
      c @ (List.flatten (List.map (decoration_to_contents info) info.decorations))

    let to_gentity info =
      { Gentity.curve = info.midCurve ;
    	Gentity.anchor = info.anchor ;
    	Gentity.contents = (Curve.draw ~parameters:info.params info.midCurve) @ info.node_contents
      }

    module Transfo = Transfo (struct type t = info let compare = compare end)

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
	    (* path_order = -1 ; *)
	    fillColor = Some color } } ) })

    let color,color_pet = 
      Pet.register "node draw" ~depends:[dashed_pet;draw_pet;params_pet] 
	(fun pet color -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with params = { info.params with
	    strokingColor = Some color } } ) })

    let contents_outputcommon,contents_pet = 
      Pet.register "node contents" ~depends:[fill_pet]
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

    let contents_box env boxes = contents_outputcommon (Document.draw_boxes env boxes)
    let contents env contents = contents_box env (boxify_scoped env contents)

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


    let getMainAnchor info = 
      let main = info.mainAnchor 
      in (if main = `Main then 
	  let _ = (Printf.fprintf stderr "Please do not choose `Main as a main anchor; 
`Main is used when drawing edges; 
it is `Base by default and you may change it, e.g., to `Center, using `MainAnchor `Center.\n" ; 
		   flush stderr)
	  in
	  `Center
	else main)

    let (rectangle : Document.environment -> Transfo.Style.t),
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
	  let (p1,p2,p3,p4) as outer_bb = BB.outer_points info bb_boot in
	  let inner_curve = rectangle (BB.points bb_boot) in
	  let mid_curve = rectangle  (BB.mid_points info bb_boot) in
	  let outer_curve = rectangle outer_bb in

	  let text_depth = -. y0 in 
	  let inner_sep = info.innerSep  in 
	  let outer_sep = info.outerSep  in 
	  let south = Point.middle p1 p2 in
	  let ex = ex env in
	  let base = Vector.(+) south (0.,inner_sep +. outer_sep +. ex +. text_depth) in
	  (* let _ =  *)
	  (*   if main = `Base  *)
	  (*   then (Printf.fprintf stderr " (main anchor: `Base)\n" ; flush stderr) *)
	  (*   else if main = `Center then (Printf.fprintf stderr " (main anchor: `Center)\n" ; flush stderr)  *)
	  (* in *)
	  let main = getMainAnchor info in
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
	    | `Angle angle ->		(* angle en degres *)
	      let (x,y) as main = anchors main in
	      let angle = to_rad angle in
	      let direction = (cos angle, sin angle) in
	      let (vx,vy) as direction' = Vector.normalise ~norm:300. direction in 
	      let rayon = Curve.of_point_lists [[main;Vector.(+) main direction']] in
	      let inter = 
		app_default Curve.latest_intersection rayon outer_curve (0,0.)
	      in
	      Curve.eval_local rayon inter
	    | `Pdf -> info.pdfAnchor
	    | _ -> Printf.fprintf stderr "Anchor undefined for a rectangle. Returning the center instead.\n" ; 
	      Point.middle p1 p3
	  in
	  { info with 
	    innerCurve = inner_curve ;
	    midCurve = mid_curve ;
	    outerCurve = outer_curve ;
	    anchor = anchors
	  })})

    let button,button_pet = 
      Pet.register "node draw" ~depends:[] 
	(fun pet (name, destinations) -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with button = Some(name,destinations);
	  } ) })

    let (default_shape : Document.environment -> Transfo.Style.t) = fun env -> rectangle env

    let default_rectangle env = Transfo.transform [rectangle env] default

    (* let (default_shape : user Document.environment -> Transfo.Style.t), *)
    (*   default_shape_pet = *)
    (*   Pet.register "node default shape" *)
    (* 	~depends:[shape_pet] *)
    (* 	(fun pet env -> *)
    (* 	{ pet = pet ; transfo = (fun transfos info -> *)
    (* 	  (\* Printf.fprintf stderr "default node shape\n" ;  *\) *)
    (* 	  if info.midCurve = [] then (rectangle env).transfo transfos info else info) }) *)

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

	  let rec anchors = function
	    | `Vec v -> Vector.(+) (Point.middle p1 p3) v
	    | `Center -> Point.middle p1 p3
	    | `Main -> anchors (getMainAnchor info)
	    | `Base -> base
	    | `BaseEast -> (fst (Point.middle p2 p3),snd base)
	    | `BaseWest -> (fst (Point.middle p1 p4),snd base)
	    | `Line -> (fst base, (snd base))
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
	    anchor = anchors
	  })}


  let float_div a b = 
    let n = truncate (a /. b) in
    let x = a -. (float_of_int n) *. b in
    if x < 0. then (n-1, x +. b)
    else (n, x)

  let pmod_float a b = snd (float_div a b)
    

  let nat_mod a b = 
    let x = a mod b in
    if x < 0 then x + b else x

  let scal_prod (x,y) (x',y') = x *. x' +. y *. y'

    (* An isoceles triangle with apex angle [apex_angle] oriented towards [orientation] *)
    let triangle ?orientation:(orientation=90.) ?apex_angle:(apex_angle=60.)  env =
	{ Transfo.Style.pet = shape_pet ; Transfo.Style.transfo = (fun transfos info -> 

	  let n,orient = float_div orientation 90. in
	  
	  let angle = (float_of_int n) *. 90. in
	  let info' = Transfo.transform [rectangle env] info in

	  if apex_angle = 0. then 
	    begin
	      Printf.fprintf stderr "Warning: angle shape with zero apex angle: don't know how to do this, sorry.\n 
Doing a rectangle.\n" ;
	      flush stderr ; info'
	    end else

	    let make_triangle ne_orig  nw_orig  sw_orig  se_orig =

	      let coins = [| ne_orig ; nw_orig ; sw_orig ; se_orig |] in
	      let shift = nat_mod n 4 in
	      let _ = begin Printf.fprintf stderr "angle = %f.\n" angle ; flush stderr end in 
	      let nouveaux_coins = Array.mapi
		(fun i _ ->  coins.((i + shift) mod 4))
		coins
	      in
	      let center = Point.middle ne_orig sw_orig in

	      let aller v = Vector.rotate (-. angle) (Vector.translate (Vector.scal_mul (-. 1.) center) v) in
	      let retour v = Vector.translate center (Vector.rotate angle v) in
	      (* let aller = Vector.rotate (-. angle) in *)
	      (* let retour = Vector.rotate angle in *)
	      let center = aller center in
	      let _ = begin Printf.fprintf stderr "center = %f,%f.\n" (fst center) (snd center) ; flush stderr end in 


	      let ne = aller nouveaux_coins.(0) in
	      let nw = aller nouveaux_coins.(1) in
	      let sw = aller nouveaux_coins.(2) in
	      let se = aller nouveaux_coins.(3) in
(*	      let south = Point.middle sw se in*)

	      let _ = begin Printf.fprintf stderr "ne = %f,%f.\n" (fst ne) (snd ne) ; flush stderr end in 
	      let _ = begin Printf.fprintf stderr "sw = %f,%f.\n" (fst sw) (snd sw) ; flush stderr end in 


(*	      let (u1,u2) as u = Vector.normalise (1., tan (to_rad orient)) in*)

	      let inter_droites (a,b,c) (a',b',c') = 
		let det = a *. b' -. a' *. b in
		if det = 0. then 		  
		      (* Any point on the first line will do *)
		    if a = 0. && b = 0. && c = 0.  then 
			(* The "line" is all the plane; the origin will do *)
		      0.,0.
		    else if a <> 0. 
		         then (-. c /. a, 0.)
		         else 
		           if b <> 0. 
			   then (0., -. c /. b)
			   else	   (* a = b = 0. but c <> 0. *)
		           (* hopeless again *)
			     begin Printf.fprintf stderr "Warning: attempt to find point in an empty intersection of \"lines\". Returning the origin.\n" ; flush stderr ; (0.,0.)
			     end
		  else 			(* non-degenerate case *)
		      ((c' *. b -. c *. b')/. (a *. b' -. a' *. b)),
		      ((c' *. a -. c *. a')/. (a' *. b -. a *. b'))
	      in
			
	      let droite angle predicate point1 point2 =
		let x0,y0 = if predicate angle then point1 else point2 in
		if pmod_float angle 180. = 0. then (0., 1., -. y0)
		else if pmod_float angle 180. = 90. then (1.,0.,-. x0)
		else
		  let pente = tan (to_rad angle) in
		  let oo = y0 -. pente *. x0 in 
		  (pente, -.1., oo)
	      in		

	      let _ = begin Printf.fprintf stderr "orient mod 180 = %f.\n" (pmod_float orient 180.) ; flush stderr end in 


	      let ga = droite (orient -. apex_angle /. 2.) (fun angle -> pmod_float angle 180. <= 90.)
		nw ne
	      in
	      let da = droite (orient +. apex_angle /. 2.) (fun angle -> pmod_float angle 180. <= 90.)
		se ne
	      in
	      let gd = droite (pmod_float (orient +. 90.) 180.) (fun angle -> pmod_float angle 180. <= 90.)
		sw sw
	      in
	      
	      let apex = inter_droites ga da in
	      let droit = inter_droites gd da in
	      let gauche = inter_droites gd ga in
	      
	      let droit_final = retour droit in 
	      let gauche_final = retour gauche in 
	      let apex_final = retour apex in 

	      let curve =
		Curve.of_point_lists [
		  [gauche_final;droit_final] ;
		  [droit_final;apex_final] ;  
		  [apex_final;gauche_final] 
		]
	      in
	      ((droit_final, gauche_final,apex_final),curve)
	    in
	    
	    let (x0,y0,x1,y1) as bb_boot = info'.bb in
	    let (p1,p2,p3,p4) as outer_bb = BB.outer_points info' bb_boot in
	    let (p1',p2',p3',p4') = BB.mid_points info' bb_boot in

	    let ((droit_final, gauche_final,apex_final),outer_curve) = make_triangle p3 p4 p1 p2 in
	    let (_,mid_curve) = make_triangle p3' p4' p1' p2' in

	    let inter point angle =
	      let angle = to_rad angle in
	      let direction = (cos angle, sin angle) in
	      let (vx,vy) as direction' = Vector.normalise ~norm:300. direction in 
	      let rayon = Curve.of_point_lists [[point;Vector.(+) point direction']] in
	      let inter = 
		app_default Curve.latest_intersection rayon outer_curve (0,0.)
	      in
	      Curve.eval_local rayon inter
	    in

	    let base = info'.anchor `Base in
	    let line = fst base, (snd base -. ex env) in
	    let center = info'.anchor `Center in
	    let south = inter center (-90.) in 
	    let north = inter center 90. in 

	    let rec anchors = function
	      | `Main -> anchors (getMainAnchor info)
	      | `BaseEast -> inter base 0.
	      | `BaseWest -> inter base 180.
	      | `Line -> line
	      | `LineEast -> inter line 0.
	      | `LineWest -> inter line 180.
	      | `East -> inter center 0.
	      | `West -> inter center 180.
	      | `North -> north
	      | `South -> south
	      | `SouthWest -> inter south 180.
	      | `SouthEast -> inter south 0.
	      | `NorthEast -> inter north 0.
	      | `NorthWest -> inter north 180.
	      | `A -> apex_final
	      | `B -> droit_final
	      | `C -> gauche_final
	      | x -> info'.anchor x
	    in
	    { info' with 
	      outerCurve = outer_curve ;
	      midCurve = mid_curve ;
	      anchor = anchors ;

	    })}


    let translate ((xt,yt) as v) info = 
	  let bb = BB.translate v info.bb in
	  let center = Vector.(+) info.center v in
	  let pdfAnchor = Vector.(+) info.pdfAnchor v in
	  let contents = List.map (OutputCommon.translate xt yt) info.node_contents in (* Center the contents *)
	  let innerCurve = Curve.translate v info.innerCurve in
	  let midCurve = Curve.translate v info.midCurve in
	  let outerCurve = Curve.translate v info.outerCurve in
	  let anchors anchor = Vector.(+) v (info.anchor anchor) in
	  let at = Vector.(+) v info.at in
	  { info with 
	    bb = bb ;
	    center = center ;
	    pdfAnchor = pdfAnchor ;
	    node_contents = contents ;
	    innerCurve = innerCurve ;
	    outerCurve = outerCurve ;
	    midCurve = midCurve ;
	    anchor = anchors ;
	    at = at }

    let anchor,anchor_pet = 
      Pet.register "node anchor" ~depends:[shape_pet] (fun pet anchor -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "anchor\n" ; flush stderr ; *)
	  let (xt,yt) as v = Vector.of_points (info.anchor anchor) (info.anchor info.node_anchor) in
	  let info' = translate v info in
	  { info' with
	    node_anchor = anchor })})

    let at,at_pet = 
      Pet.register "node at" ~depends:[anchor_pet] (fun pet point -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "node at\n" ; flush stderr ; *)
	  translate point info)})

    let at3d,at3d_pet = 
      Pet.register "node at ... in 3d" ~depends:[anchor_pet] (fun pet projection point3d -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "node at\n" ; flush stderr ; *)
	  let (x,y,z) = Proj3d.project projection point3d in
	  { (translate (x,y) info)
	    with z = z })})

    let make_output styles cont =
      let info = Transfo.transform
	((contents_outputcommon cont) :: anchor `Center :: mainAnchor `Center :: styles) 
	default 
      in info
      (* to_gentity info *)
    let make_boxified env styles cont = make_output styles (Document.draw_boxes env cont)
    let make env styles cont = make_boxified env styles (boxify_scoped env cont)

    let label, label_pet = 
      Pet.register ~depends:[draw_pet;params_pet;fill_pet] "label" 
	(fun pet env ?pos:(pos=(`North : anchor)) ?style:(style=[rectangle env]) cont ->
	  { pet = pet ; transfo = (fun transfos info -> 
	    let node = make env (at (info.anchor pos) :: style) cont in
	    {info with decorations = Node node::info.decorations }) })

    let inter a b=
      let curvesa=ref [] in
      let curvesb=ref [] in
      List.iter
        (function
             Path (_,xx)->List.iter (Array.iter (fun x->curvesa:=x::(!curvesa))) xx
           | _->()) a.contents;
      List.iter
        (function
             Path (_,xx)->List.iter (Array.iter (fun x->curvesb:=x::(!curvesb))) xx
           | _->()) b.contents;
      let inters=ref [] in
      List.iter (fun xa->
                   List.iter (fun xb->
                                List.iter (fun (t,_)->
                                             let x=Bezier.eval (fst xa) t in
                                             let y=Bezier.eval (snd xa) t in
                                             inters:=(x,y)::(!inters)
                                          ) (Bezier.intersect xa xb)
                             ) !curvesb
                ) !curvesa;
      !inters
  end


    module Matrix = struct
      type 'a matrix = 'a array array
      let map f = Array.map (Array.map f) 
      let mapi f = Array.mapi (fun i line -> Array.mapi (f i) line)

      type info = 
	  { mainNode : Node.info ;
	    common : Node.Transfo.Style.t list ;
	    nodes : Node.info matrix ;
	    placement : info -> int -> int -> Point.t }

      type t = info

      module Transfo = Transfo (struct type t = info let compare = compare end)
      module T = Transfo
      module S = T.Style
      open T
      open S


      let default_matrix_node_anchor = `Base
      let default_matrix_node_style env = 
	Node.([mainAnchor `Center ; anchor default_matrix_node_anchor ; rectangle env])
      let default_main_node_style env = 
	Node.([mainAnchor `Center ; anchor `Pdf ; rectangle env])

      let between_centers disty distx _ i j =
	(float_of_int j *. distx), -. (float_of_int i *. disty) 

      let default env = { mainNode = Node.default ; common = default_matrix_node_style env ; nodes = [||] ;
			placement = (between_centers 20. 20.)}

      (* let to_gentities { mainNode = main ; nodes = nodes } = (Node.to_gentity main, map Node.to_gentity nodes) *)

      let nodes_contents info = 
	List.flatten 
	  (Array.to_list 
	     (Array.map 
		(fun line -> 
		  List.flatten 
		    (Array.to_list 
		       (Array.map 
			  (fun node_info -> (Node.to_contents node_info)) 
			  line)))
		info.nodes))

      let to_contents info = (nodes_contents info) @ (Node.to_contents info.mainNode)

      let (allNodes : Node.Transfo.Style.t list -> S.t),
	all_nodes_pet = 
      Pet.register "matrix all nodes" (fun pet node_transfos -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "allNodes \n" ; flush stderr ; *)
	  { info with 
	    common =  info.common @ node_transfos })})

      let make_node_array style height width lines = 
	  (* On commence par placer les noeuds de sorte que le noeud (0,0) ait son 
	     default_matrix_node_anchor en (0,0) *)
	let res = Array.make_matrix height width Node.default in
	let i = ref 0 in
	let j = ref 0 in
	let _ = List.iter (fun line -> 
	  List.iter (fun node_transfo -> 
	    let info_ij = Node.Transfo.transform (style @ node_transfo) Node.default
	    in
	    res.(!i).(!j) <- info_ij ;
	    incr j)
	    line ;
	  incr i ; j := 0)
	  lines 
	in
	res

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
	  let res = make_node_array info.common height width lines in
	  { info with nodes = res })})

      let contents_output lines =
	let lines' = 
	  List.map (List.map (fun (style,contents) -> ((Node.contents_outputcommon contents)::style))) lines
	in
	makeNodes lines'
      let contents_box env lines =
	let lines' = 
	  List.map 
	    (List.map 
	       (fun (style,contents) -> 
		 ((Node.contents_outputcommon (Document.draw_boxes env contents))::style)))
	    lines
	in
	makeNodes lines'

      let contents env lines =
	let lines' = 
	  List.map 
	    (List.map 
	       (fun (style,contents) -> 
		 ((Node.contents_outputcommon (Document.draw_boxes env (boxify_scoped env contents)))::style)))
	    lines
	in
	makeNodes lines'

      let placement, placement_pet = 
	Pet.register "prepare matrix placement" ~depends:[all_nodes_pet;make_nodes_pet] (fun pet placement -> 
	  { pet = pet ; transfo = (fun transfos info -> { info with placement = placement }) })

      let makePlacement,make_placement_pet = 
	Pet.register "matrix placement" ~depends:[placement_pet;all_nodes_pet;make_nodes_pet] (fun pet -> 
	  { pet = pet ; transfo = (fun transfos info -> 
	    (* Printf.fprintf stderr "matrix placement \n" ; flush stderr ; *)
	      let info' = 
		{ info with nodes = mapi (fun i j node_info -> 
		  Node.translate (info.placement info i j) node_info) info.nodes } 
	      in 
	      let nodes_contents = nodes_contents info' in
	      let node_info = Node.Transfo.transform
		[Node.contents_outputcommon nodes_contents]
		info'.mainNode 
	      in
	      { info' with mainNode = node_info })})

      let setZ,set_z_pet = 
	Pet.register "set matrix z coordinate" ~depends:[placement_pet;all_nodes_pet;make_nodes_pet] 
	  (fun pet z_placement -> 
	  { pet = pet ; transfo = (fun transfos info -> 
	      let info' = 
		{ info with nodes = mapi 
		    (fun i j node_info -> { node_info with Node.z = z_placement node_info i j })
		    info.nodes } 
	      in info' ) })

      (* let mainNodeContents,main_node_contents_pet = *)
      (* 	Pet.register "matrix main node contents" ~depends:[placement_pet] (fun pet ->  *)
      (* 	{ pet = pet ; transfo = (fun transfos info ->  *)
      (* 	  let nodes_contents = nodes_contents info in *)
      (* 	  let node_info = Node.Transfo.transform *)
      (* 	    [Node.contents_outputcommon nodes_contents] *)
      (* 	    info.mainNode  *)
      (* 	  in *)
      (* 	  { info with mainNode = node_info })}) *)
	  
      let mainNode,main_node_pet = 
      Pet.register "matrix main node" ~depends:[make_placement_pet] 
	(fun pet node_transfos -> 
	  { pet = pet ; transfo = (fun transfos info -> 
	    let pdf_start = 0.,0. in
	    let node_info = Node.Transfo.transform node_transfos info.mainNode in
	    let pdf_end = node_info.Node.anchor `Pdf in
	    let v = Vector.of_points pdf_start pdf_end in
	    let nodes = map (Node.translate v) info.nodes in
	    { info with 
	      mainNode = { node_info with Node.node_contents = [] }; 
	      nodes = nodes })})
	
      (* let wrap,wrap_pet =  *)
      (* 	Pet.register "matrix wrap" ~depends:[main_node_contents_pet; main_node_pet] (fun pet -> *)
      (* 	  { pet = pet ;  *)
      (* 	    transfo = (fun transfos info ->  *)
      (* 	      let info' =  *)
      (* 		{ info with nodes = mapi (fun i j node_info ->  *)
      (* 		  Node.translate (info.placement info i j) node_info) info.nodes }  *)
      (* 	      in  *)
      (* 	      let node_info' = { info.mainNode with node_contents = [] } in *)
      (* 	      let pdf_start = info'.nodes.(0).(0).anchor `Pdf in *)
      (* 	      let pdf_end = node_info'.pdfAnchor in *)
      (* 	      let v = Vector.of_points pdf_start pdf_end in *)
      (* 	      let nodes = map (Node.translate v) info'.nodes in *)
      (* 	      { info' with mainNode = node_info' ; *)
      (* 		nodes = nodes })}) *)

      let centers y x = placement (between_centers y x)

      let transform_matrix env style lines info =
	T.transform 
	  (contents env lines :: 
	     mainNode [Node.rectangle env] :: 
	     (* mainNodeContents :: *)
	     makePlacement ::
	     allNodes [] ::
	     (* wrap :: *)
	     style) info

      let make env style lines = transform_matrix env style lines (default env) 
	
      let make_simple env style lines = 	
	let info = make env style lines in 
	info.mainNode,info.nodes

      let translate v matrix_info = 
	{ matrix_info with
	  mainNode = Node.translate v matrix_info.mainNode ;
	  nodes = map (Node.translate v) matrix_info.nodes
	}

    end

    module Matrix3d = struct

      type info = {
	placement : info -> int -> int -> int -> (Point.t * float) ;
	planes : Matrix.info array ;
	common : Node.Transfo.Style.t list ;
	mainNode : Node.info ;
	mainNodes : Node.Transfo.Style.t list 
      }
	
      type t = info

      module Transfo = Transfo (struct type t = info let compare = compare end)
      module T = Transfo
      module S = T.Style
      open T
      open S

      let between_centers ?projection:(projection=Proj3d.cavaliere45bg) disty distx distz _ i j k = 
	let z = -. (float_of_int k *. distz) in
	let x,y,z = Proj3d.project projection 
	   ((float_of_int j *. distx), 
	    -. (float_of_int i *. disty),
	    z)
	in (x,y),z


      let default env = {
	mainNode = Node.(default_rectangle env) ;
	mainNodes = [Node.rectangle env] ;
	common = Matrix.default_matrix_node_style env ;
	planes = [|Matrix.default env|] ;
	placement = (between_centers 20. 20. 20.) }

      (* let to_gentities m3d = *)
      (* 	(Node.to_gentity m3d.mainNode, *)
      (* 	 Array.map (fun plane -> snd (Matrix.to_gentities plane)) m3d.planes) *)

      let nodes_contents info = 
	List.flatten (Array.to_list (Array.map Matrix.nodes_contents info.planes))

      let to_contents info = (nodes_contents info) @ (Node.to_contents info.mainNode)

      let (allNodes : Node.Transfo.Style.t list -> S.t),
	all_nodes_pet = 
      Pet.register "matrix 3d all nodes" (fun pet node_transfos -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  { info with 
	    common =  info.common @ node_transfos })})


      let makeNodes,make_nodes_pet = 
      Pet.register "matrix 3d make nodes" ~depends:[all_nodes_pet] (fun pet planes -> 
	{ pet = pet ; transfo = (fun transfos info -> 
	  (* Printf.fprintf stderr "makeNodes \n" ; flush stderr ; *)
	  let width = max_list_list List.length 0 planes in
	  let depth = List.length planes in
	  let height = max_list List.length 0 planes in
	  let arr = Array.make depth (info.planes.(0)) in
	  Printf.fprintf stderr "makeNodes: passed info.planes.(0) without damage. \n" ; flush stderr ;
	  (* On commence par placer les noeuds de sorte que le noeud (0,0) ait son centre en (0,0) *)
	  let _ =
	    List.fold_left
	      (fun k plane -> let res = Matrix.make_node_array info.common height width plane in 
			      let _ =
				arr.(k) <- { info.planes.(0) with Matrix.nodes = res } 
			      in
			      succ k)
	      0
	      planes
	  in { info with planes = arr })})

      let contents_output lines =
	let lines' = 
	  List.map (List.map (List.map 
				(fun (style,contents) -> ((Node.contents_outputcommon contents)::style)))) 
	    lines
	in
	makeNodes lines'
      let contents_box env lines =
	let lines' = 
	  List.map (List.map (List.map 
	       (fun (style,contents) -> 
		 ((Node.contents_outputcommon (Document.draw_boxes env contents))::style))))
	    lines
	in
	makeNodes lines'

      let contents env lines =
	let lines' = 
	  List.map 
	    (List.map 
	       (List.map 
		  (fun (style,contents) -> 
		    ((Node.contents_outputcommon (Document.draw_boxes env (boxify_scoped env contents)))::style))))
	    lines
	in
	makeNodes lines'


      let placement, placement_pet = 
	Pet.register "prepare 3d matrix placement" ~depends:[all_nodes_pet;make_nodes_pet] (fun pet placement -> 
	  { pet = pet ; transfo = (fun transfos info -> { info with placement = placement }) })

      let transmit, transmit_pet = 
      Pet.register "make 3d matrix " ~depends:[all_nodes_pet;make_nodes_pet;placement_pet] (fun pet sty -> 
	{ pet = pet ; transfo = (fun transfos 
	  ({ mainNode = main ;
	    mainNodes = mains ;
	    common = style ;
	    planes = planes ;
	    placement = placement } as info)
	-> 
	  let planes' = Array.mapi
	    (fun k matrix_info -> 
	      let placement_k node_info i j = let (x,y),_ = placement info i j k in x,y in
	      let z_placement_k node_info i j = let _,z = placement info i j k in z in
	      let open Matrix in
	      let m = Matrix.Transfo.transform
		([(allNodes style); (placement (placement_k)); makePlacement ;
			(setZ z_placement_k); (mainNode mains)] @ sty)
		planes.(k) 
	      in m)
	    planes
	  in
	  { info with planes = planes' }
	)})

      let mainNode,main_node_pet = 
      Pet.register "matrix 3d main node" ~depends:[transmit_pet]
	(fun pet node_transfos -> 
	  { pet = pet ; transfo = (fun transfos info -> 
	    let nodes_contents = nodes_contents info in
	    let pdf_start = 0.,0. in
	    let node_info = Node.Transfo.transform 
	      ((Node.contents_outputcommon nodes_contents) :: node_transfos) info.mainNode 
	    in
	    let pdf_end = node_info.Node.anchor `Pdf in
	    let v = Vector.of_points pdf_start pdf_end in
	    let planes' = Array.map (Matrix.translate v) info.planes in
	    { info with 
	      mainNode = { node_info with Node.node_contents = [] }; 
	      planes = planes' })})

      let transform_matrix env style planes info = 
	T.transform
	  (contents env planes ::
	     mainNode [Node.rectangle env] :: 
	     transmit [] ::
	     allNodes [] ::
	     style)
	  info

      let make env style planes = 
	let info = transform_matrix env style planes (default env) in
	info

    end

    module Edge = struct

      type tip_info = { tip_line_width : float ; is_double : bool }

      type info = { tip_info : tip_info ; 
			 start : Gentity.t ;
			 finish : Gentity.t ;
			 params : path_parameters ; 
			 given_curve : Curve.t ;
			 underlying_curve : Curve.t ;
			 curves : (path_parameters * Curve.t) list ;
			 decorations : decoration list ;
			 z_curve : float array list ;
			 anchor : anchor -> Point.t
		  }
      and decoration = Curve of path_parameters * Curve.t
		       | Node of Node.t

      let evaluate_z info (i,t) = match info.z_curve with
	  [] -> 0.0
	| l -> 
	  try 
	    let curve = (List.nth l i) in Bezier.eval curve t
	  with _ -> begin
	    Printf.fprintf stderr ("Warning: attempt to evaluate the z of an empty curve. Returning 0.\n") ;
	    0.0
	  end

      type t = info


      type edge_transfo = info -> info

      (* module rec PTDef : sig *)
      (* 	module type ParameterisedEdgeTransfo = sig *)
      (* 	  type t  *)
      (* 	  val name : string  *)
      (* 	  val transfo : t -> edge_transfo *)
      (* 	end *)
      (* 	type parameterised_edge_transfo = (module PTDef.ParameterisedEdgeTransfo) *)
      (* end = PTDef	     *)


      module Transfo = Transfo (struct type t = info let compare = compare end)
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
	  given_curve = underlying_curve ;
	  underlying_curve = underlying_curve ;
	  curves = [] ; 
	  decorations = [] ;
	  z_curve = [] ;
	  anchor = (fun _ -> (0.,0.)) }

      let empty = 
	{ (default_edge_info (coord (0.,0.)) (coord (0.,0.)) (Curve.of_point_lists [[(0.,0.)]]))
	  with params = { OutputCommon.default with strokingColor = None } }

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

      let rec point_lists_of_edge_spec_rec res s e continues = 
	match continues with
	| [] -> [[s;e]]
	| [ controls ] -> List.rev (((s :: controls) @ [e]) :: res)
	| controls :: rest -> point_lists_of_edge_spec_rec ((s :: controls) :: res) (list_last controls) e rest

      let point_lists_of_edge_spec s continues e =
	point_lists_of_edge_spec_rec [] (s.Gentity.anchor `Main) (e.Gentity.anchor `Main) continues

      let decoration_to_contents edge_info = function
	| Curve (params, curve) -> Curve.draw ~parameters:params curve
	| Node node -> Node.to_contents node

      let to_contents edge_info = 
	(List.flatten (List.map (fun (params,curve) -> (Curve.draw ~parameters:params curve)) 
			 edge_info.curves))
	@ (List.flatten (List.map (decoration_to_contents edge_info) edge_info.decorations))

      let to_gentity info = 
	{ Gentity.curve = info.underlying_curve ;
	  Gentity.anchor = info.anchor ;
	  Gentity.contents = to_contents info }

      (* ******************************************************** *)
      (* We start defining new edge transfos and registering them *)
      (* ******************************************************** *)

      let do_clip curve node1 node2 = 
	let start = begin
	  match node1.curve with
	    | [xs,ys] when Array.length xs <= 1 -> (0,0.)
	    | curve1 ->
	      Curve.(app_default latest_intersection curve curve1 (0,0.))
	(* let _ = Printf.fprintf stderr "Points: \n"  in *)
	(* let _ = List.iter (fun l ->   *)
	(*   let _ = List.iter (fun (x,y) -> Printf.fprintf stderr "(%f,%f) ; " x y) l in *)
	(*   Printf.fprintf stderr "\n") *)
	(*   point_lists in *)
	(* let _ = flush stderr in *)

	      (* match Curve.latest_intersection curve curve1 with *)
	      (* 	| None -> begin *)
	      (* 	  (\* Printf.fprintf stderr *\) *)
	      (* 	  (\* 	"I can't find any intersection of your edge with the start node shape.\nI'm taking the center as a start node instead.\n" ; *\) *)
	      (* 	  (0,0.) *)
	      (* 	end *)
	      (* 	| Some (i, t1) -> (i, t1) *)
	end in
	let (j,t') as finish = begin 
	  match node2.curve with
	    | [xs,ys] when Array.length xs <= 1 -> ((Curve.nb_beziers curve) - 1,1.)
	    | curve2 ->
	      Curve.(app_default earliest_intersection curve curve2 ((Curve.nb_beziers curve) - 1,1.))
	      (* match Curve.earliest_intersection curve curve2 with *)
	      (* 	| None -> begin *)
	      (* 	  (\* Printf.fprintf stderr *\) *)
	      (* 	  (\* 	"I can't find any intersection of your edge with the end node shape.\nI'm taking the center as a end node instead.\n" ; *\) *)
	      (* 	  ((Curve.nb_beziers curve) - 1,1.) *)
	      (* 	end *)
	      (* 	| Some (j, t2) -> (j, t2) *)
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



      let head_moustache info params = 
	(* let _ = begin  *)
	(* 	 Printf.fprintf stderr "Entering head: lineWidth = %f, true lineWidth = %f \n" *)
	(* 	   params.lineWidth info.tip_line_width ; *)
	(* 	 flush stderr *)
	(* end in *)
	if info.is_double then
	  let short = max (params.lineWidth *. 0.66) 0.6 in
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
	    let tb = if b = 0. then 1. else Curve.curvilinear curve' (-. b) in
	    params', Curve.restrict curve' ta tb
	  in
	  let _, u_curve = shorten (info.params, info.underlying_curve) in
	  { info with underlying_curve = u_curve ; curves = List.map shorten info.curves })})

      let shortenS a = shorten a 0.
      let shortenE b = shorten 0. b

      let zs, zs_pet =
	Pet.register "provide a z curve (mainly for intersections)" (fun pet zs ->
	  { pet = pet ;
	    transfo = (fun transfos edge_info -> 
	      { edge_info with z_curve = zs })
	  })




      let double, double_pet = Pet.register ~depends:[draw_pet] ~append:only_last "double" (fun pet margin -> 
	{ pet = pet ; transfo = (fun transfos info ->
	  let black_paths = List.map (fun (params, curve) -> 
	    { params with 
	      lineWidth = margin +. 2. *. params.lineWidth },
	    curve)
	    info.curves
	  in
	  let white_paths = List.map (fun (params, curve) -> 
	    { params with 
	      (* path_order = (-1) ; *)
	      strokingColor = Some (RGB { red=1.;green=1.;blue=1. }); 
	      lineWidth = margin },
	    curve)
	    info.curves
	  in
	  let delta = 0.02 in
	  let info_white = Transfo.transform [shorten delta delta] { info with curves = white_paths } in
	  let info_black = Transfo.transform [shorten (delta +. 0.1) (delta +. 0.1)] 
	    { info with curves = black_paths } in
	  { info_black with 
	    tip_info = { tip_line_width = margin +. 2.0 *. info.params.lineWidth ;
			 is_double = true };
	    curves = (info_black.curves @ info_white.curves) }) })

      let base_arrow ?head_or_tail:(head_or_tail=true) head_params transfos edge_info=
	let info = edge_info.tip_info in
	let params = edge_info.params in
	let underlying_curve = edge_info.underlying_curve in
	let time = if head_or_tail then 1. else 0. in
	let (da,db) as grad = Vector.scal_mul (if head_or_tail then 1. else -1.) (Curve.eval (Curve.gradient underlying_curve) time) in
	let short, thickness, height, width, lw = head_params info params in
	let thickness' = thickness -. thickness *. info.tip_line_width /. 2. /. width in

	(* Control points on the curve *)
	let (xe,ye) as e = Curve.eval underlying_curve time in
	(*let _ = Printf.fprintf stderr "Shortening by %f.\n" short ; flush stderr in*)
	let edge_info' = Transfo.transform [(if head_or_tail then shortenE else shortenS) short] edge_info in
	let curve0 = edge_info'.underlying_curve in
	(*let _ = Printf.fprintf stderr "Done shortening.\n" ; flush stderr in*)
	let e0 = Curve.eval curve0 time in
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
	{ edge_info' with decorations = edge_info'.decorations @
	    [Curve ({ params with 
		  close = true ; 
		  fillColor = params.strokingColor ; 
		  lineWidth = lw }, tip)]}

      let arrowOf, arrow_head_pet = 
        Pet.register ~depends:[double_pet;shorten_pet] ~append:only_last "arrow head"
          (fun pet head_params -> 
	     { pet = pet ; transfo = base_arrow head_params })

      let backArrowOf, backArrow_head_pet = 
        Pet.register ~depends:[double_pet;shorten_pet] ~append:only_last "arrow tail"
          (fun pet head_params -> 
	     { pet = pet ; transfo = base_arrow ~head_or_tail:false head_params })

      let arrow = arrowOf head_moustache
      let arrow' = backArrowOf head_moustache

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
	    { edge_info' with decorations = edge_info'.decorations @ [Curve (edge_info'.params, dash)] }
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

      let paramsOf,params_pet = 
	Pet.register ~codepends:[clip_pet] "params"
	(fun pet params ->
	  { pet = pet ; transfo = (fun transfos edge_info ->
	    { edge_info with params = params })})

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


      let foreground, foreground_pet = 
	Pet.register ~depends:[draw_pet;shorten_pet;params_pet] "foreground" 
	  (fun pet ?shortens:(shortens=3.) ?shortene:(shortene=3.) ?color:(color=Drivers.white) margin ->
	  { pet = pet ; transfo = (fun transfos info -> 
	    let white_paths = List.map (fun (params, curve) -> 
	      { info.params with 
		Drivers.dashPattern = [] ;
		Drivers.strokingColor=Some color; 
		Drivers.lineWidth=params.Drivers.lineWidth +. 2. *. margin },
	      curve)
	      info.curves
	    in
	    let edge_info' = 
	      Transfo.transform [shorten shortens shortene] { info with curves = white_paths }   in
	    { info with curves = (edge_info'.curves @ info.curves) }) })

      let make_anchors, make_anchors_pet = 
	Pet.register 
	  ~depends:[foreground_pet;clip_pet;bend_pet;arrow_head_pet;double_pet;
		    params_pet;draw_pet;shorten_pet;squiggle_pet;clip_pet] 
	  "make anchors" 
	  (fun pet ->
	  { pet = pet ; transfo = (fun transfos edge_info -> 
      	    let curve = edge_info.underlying_curve in
      	    let given_curve = edge_info.given_curve in
      	    let anchor = function
      	      | `Temporal pos -> Curve.eval curve pos
      	      | `Curvilinear pos -> Curve.eval curve (Curve.curvilinear curve pos)
      	      | `CurvilinearFromStart pos -> Curve.eval given_curve (Curve.curvilinear given_curve pos)
      	      | `Start -> edge_info.start.Gentity.anchor `Main
      	      | `End -> edge_info.finish.Gentity.anchor `Main
      	      | `Center -> Curve.eval curve 0.5
      	      | _ -> Printf.fprintf stderr "Anchor undefined for an edge. Returning the center instead.\n" ;
      		Curve.eval curve 0.5
      	    in
	    { edge_info with anchor = anchor })})




      let label, label_pet = 
	Pet.register ~depends:[draw_pet;shorten_pet;params_pet;double_pet;foreground_pet;make_anchors_pet] "label" 
	  (fun pet env ?pos:(pos=(`Temporal 0.5 : anchor)) ?style:(style=[Node.rectangle env]) cont ->
	    { pet = pet ; transfo = (fun transfos info -> 
	      let node = Node.make env (Node.at (info.anchor pos) :: style) cont in
	      { info with decorations = info.decorations @ [Node node] }) })



      let raw_edge style s e underlying_curve =
	let edge_info = transform (make_anchors :: style) s e underlying_curve in
	edge_info

      let path_of_curve style curve = 
	let s = coord (Curve.first curve) in 
	let e = coord (Curve.last curve) in 
	raw_edge style s e curve

      let path style s continues = 		
	let curve = (Curve.of_point_lists (point_lists_of_path_spec s continues)) in
	path_of_curve style curve


      let of_gentities style s ?controls:(controls=[]) e = 
	let point_lists = point_lists_of_edge_spec s controls e in
	let underlying_curve = Curve.of_point_lists point_lists in
	raw_edge (clip :: style) s e underlying_curve

      let make style s ?controls:(controls=[]) e = 
	of_gentities style (Node.to_gentity s) ~controls:controls (Node.to_gentity e)

      let makes style edge_list = 
	List.map (fun (style',s,controls,e) -> make (style' @ style) s ~controls:controls e) edge_list

      let make_3d style start ?projection:(projection=Proj3d.cavaliere45bg)
	  ?controls:(controls=[]) ?controls3d:(controls3d=[]) finish = 
	let xycontrols,zcontrols = 
	  if controls3d = [] then
	    controls,[]
	  else
	    let associate l = (List.map
		     (fun point -> 
		       let (x,y,z) = Proj3d.project projection point in (x,y),z) l)
	    in
	    let split ls = List.map List.split (List.map associate ls) in
	    (List.split (split controls3d))
	in
	let zstart = start.Node.z in
	let zfinish = finish.Node.z in	
	let s = Node.to_gentity start in
	let e = Node.to_gentity finish in
	let point_lists = point_lists_of_edge_spec s xycontrols e in
	let z_curve = List.map
	  Array.of_list 
	  (point_lists_of_edge_spec_rec [] zstart  zfinish zcontrols) 
	in
	let underlying_curve = Curve.of_point_lists point_lists in
	raw_edge (clip :: (zs z_curve) :: style) s e underlying_curve


      let makes_3d style  ?projection:(projection=Proj3d.cavaliere45bg)
	  edge_list = 
	List.map (fun (style',s,controls,controls3d,e) -> 
	  make_3d (style' @ style) s ~controls:controls ~controls3d:controls3d e) edge_list


      let restrict info lt1 lt2 =
	if Curve.compare_lt lt1 lt2 > 0 then begin
	  Printf.fprintf stderr "Patofig warning: incoherent restriction of an edge; returning empty edge.\n" ; 
	  empty 
	end
	else
	{ info with
	  start = coord (Curve.eval_local info.underlying_curve lt1) ;
	  finish = coord (Curve.eval_local info.underlying_curve lt2) ;
	  underlying_curve = Curve.internal_restrict info.underlying_curve lt1 lt2 ;
	  curves = List.map (fun (params,curve) -> (params,Curve.internal_restrict curve lt1 lt2)) info.curves ;
	  decorations = []
	}

      let put_forth info lt ?color:(color=OutputCommon.white) epsilon margin = 
	let gt = Curve.global_time info.underlying_curve lt  in
	let cut x = min (max x 0.) 1. in
	let gt1 = cut (gt -. epsilon) in 
	let gt2 = cut (gt +. epsilon) in 
	if epsilon >= 0. then
	  let (i,t) as lt1 = Curve.local_time info.underlying_curve gt1 in
	  let (j,u) as lt2 = Curve.local_time info.underlying_curve gt2 in
	  let info' = restrict info lt1 lt2 in
      	  { info' with curves =
      	      ({info'.params with
      		Drivers.dashPattern = [] ;
      		Drivers.strokingColor=Some color;
      		Drivers.lineWidth=info'.tip_info.tip_line_width +. 2. *. margin
      	      }, info'.underlying_curve)
      		  :: info'.curves }
	else begin
	  Printf.fprintf stderr "Warning: nonsensical restriction instructions. Returning empty curve.\n";
	  flush stderr ;
	  empty
	  end

    end


    module Entity = struct       
      type raw={raw_contents:OutputCommon.raw list;raw_anchor:float*float}
      type t =
	Node of Node.t
      | Matrix of Matrix.t
      | Matrix3d of Matrix3d.t
      | Edge of Edge.t
      | Gentity of gentity
      | Raw of raw

      let to_raw_list = function
	| Node node -> Node.to_contents node 
	| Matrix matrix -> Matrix.to_contents matrix
	| Matrix3d matrix -> Matrix3d.to_contents matrix
	| Edge edge -> Edge.to_contents edge
	| Gentity g -> g.contents 
        | Raw x->x.raw_contents

      let anchor entity a = match entity with
	| Node node -> node.Node.anchor a
	| Edge edge -> edge.Edge.anchor a
	| Matrix matrix -> (matrix.Matrix.mainNode).Node.anchor a
	| Matrix3d matrix -> (matrix.Matrix3d.mainNode).Node.anchor a
	| Gentity g -> g.Gentity.anchor a
        | Raw x->x.raw_anchor

      let to_contents stack = 
	let contents = List.flatten (List.rev_map to_raw_list stack) in
	let rec order i res = function [] -> res 
	  | raw :: contents -> 
	    order (succ i) ((OutputCommon.in_order i raw) :: res) contents
	in
	List.rev (order 0 [] contents)

    end
    type entity = Entity.t
    open Entity

    module Env_Diagram (Args : sig val env : environment end) = struct
      open Entity
      let stack : entity list ref = ref []
      let env = Args.env
      let compute_intersections = ref (Some (fun x -> Edge.put_forth ~color:OutputCommon.white x))
      let epsilon = ref 1.0
      let margin = ref 1.0
      let t_margin = ref 0.05

      let node style contents =
	let a = Node.(make env (default_shape env :: style)) contents in
	stack := (Node a) :: !stack ;
	a

      let coordinate p = 
	let a = Node.coord p in
	let _ = stack := (Node a) :: !stack in
	a

      let edge style a ?controls:(controls=[]) b =
	let open Edge in
	let e = make style a ~controls:controls b in
	stack := (Edge e) :: !stack ;
	e

      let edges style edge_list = 
	let open Edge in
	let edges = makes style edge_list in
	let res = List.fold_left (fun stack edge -> Edge edge :: stack) !stack edges in
	stack := res ; 
	edges

      let path style s continues =
	let open Edge in
	let e = path style s continues in
	stack := Edge e :: !stack ;
	e

      let raw (x,y) l=
        let r= { raw_anchor=(x,y);
                 raw_contents=List.map (OutputCommon.translate x y) l }
        in
        stack:=Raw r :: !stack;
        r

      let edge_3d style a ?controls:(controls=[]) ?controls3d:(controls3d=[]) 
	  ?projection:(projection=Proj3d.cavaliere45bg) b =
	let open Edge in
	let e = make_3d style a ~controls:controls ~controls3d:controls3d ~projection:projection b in
	stack := (Edge e) :: !stack ;
	e

      let edges_3d style ?projection:(projection=Proj3d.cavaliere45bg) edge_list = 
	let open Edge in
	let edges = makes_3d style ~projection:projection edge_list in
	let res = List.fold_left (fun stack edge -> Edge edge :: stack) !stack edges in
	stack := res ; 
	edges

      let matrix_full style lines = 
	let open Matrix in
	let matrix = make env style lines in
	stack := Matrix matrix :: !stack ;
	matrix

      let math_matrix_full style l = 
	matrix_full style (List.map (fun line ->
	  (List.map (fun (style, math_list) -> 
	    (style, [bB (fun env -> Maths.draw [env] math_list)])) 
	     line))
			l)

      let matrix style lines = 
	let m = matrix_full style lines in
	Matrix.(m.mainNode, m.nodes)

      let math_matrix style lines = 
	let m = math_matrix_full style lines in
	Matrix.(m.mainNode, m.nodes)

      let matrix_3d_full style planes = 
	let open Matrix3d in
	let matrix = make env style planes in
	stack := Matrix3d matrix :: !stack ;
	matrix

      let matrix_3d_project m = 
	let ms = Array.map 
	  (fun matrix_info -> matrix_info.Matrix.nodes) 
	  Matrix3d.(m.planes)
	in
	Matrix3d.(m.mainNode),ms

      let matrix_3d style planes = 
	matrix_3d_project (matrix_3d_full style planes)

      let all_intersections stack =
	let rec fn acc = function
	[] -> acc
	  | Edge e::l when e.Edge.params.fillColor = None -> 
	  let c = List.map (fun b  -> b, Bezier.extremity b) e.Edge.underlying_curve in
	  fn ((e,c)::acc) l
	| _::l -> fn acc l
	in
	let stack = fn [] stack in
	let rec intersections_with inters e c stack = match stack with
	  | [] -> inters
	  | (e',c') :: stack_rest ->
	    let inters' = 
	      List.fold_left
		(fun inters inter -> (e,e',inter) :: inters)
		inters
		(Curve.intersections c c')
	    in intersections_with inters' e c stack_rest
	in
	let rec all_intersections_rec inters stack =
	  match stack with
	  | [] -> inters
	  | (e, c) :: stack_rest ->
	    let inters_e = intersections_with [] e c stack_rest in
	    all_intersections_rec (List.rev_append inters_e inters) stack_rest
	in
	all_intersections_rec [] stack

      let add_intersections f = 
	let inters = all_intersections !stack in
	List.iter
	  (fun (e1,e2,((i,t,b),(j,u,c))) -> 
	    let open Edge in 
	    let z1 = Edge.evaluate_z e1 (i,t) in
	    let z2 = Edge.evaluate_z e2 (j,u) in
	    if abs_float (z1 -. z2) > !epsilon then begin
	      let ei,k,l = if z1 < z2 then e2,j,u else
		  if z1 > z2 then e1,i,t 
		  else assert false in
	      let info' = f ei (k,l) !t_margin !margin in
	      stack := Edge info' :: !stack
	    end
	    else ())
	  inters

      let spit_stack () = !stack

      let spit_stack_with_intersections () =
	let tmp_stack = !stack in
	let _ = match !compute_intersections with
	  | None -> ()
	  | Some f -> add_intersections f
	in
	let res = !stack in
	let _ = (stack := tmp_stack) in
	res

      let include_diagram x = let _ = stack := x @ !stack in ()

      let make () =
	let _ = match !compute_intersections with
	  | None -> ()
	  | Some f -> add_intersections f
	in
	let ordered_contents = to_contents !stack in
	let fig = Box.drawing_inline ordered_contents
	in
	stack := [] ; fig

      open Node
      open Edge

      let label_anchor a ?pos:(pos=(`Temporal 0.5 : anchor)) 
	  ?shape:(shape=Node.rectangle env) 
	  ?style:(style=[]) 
	  contents = 
	label env ~pos:pos ~style:((Node.anchor a) :: shape :: style)  ([Scoped ((fun env -> { env with mathStyle = Mathematical.Script }), contents)])

      let labela ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `South ~pos:pos ~shape:shape ~style:style cont
      let labelb ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `North ~pos:pos ~shape:shape ~style:style cont
      let labell ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `East ~pos:pos ~shape:shape ~style:style cont 
      let labelr ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `West ~pos:pos ~shape:shape ~style:style cont
      let labelal ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `SouthEast ~pos:pos ~shape:shape ~style:style cont
      let labelar ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `SouthWest ~pos:pos ~shape:shape ~style:style cont
      let labelbl ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `NorthEast ~pos:pos ~shape:shape ~style:style cont
      let labelbr ?pos:(pos=(`Temporal 0.5 : anchor)) ?shape:(shape=Node.rectangle env)
	  ?style:(style=[]) cont = 
	label_anchor `NorthWest ~pos:pos ~shape:shape ~style:style cont


      let label_edge e style pos contents = 
	node  ((Node.at (e.anchor (`Temporal pos))) :: style) 
	  ([Scoped ((fun env -> { env with mathStyle = Mathematical.Script }), contents)])

      let label_edge_anchor e anchor pos contents = 
	label_edge e [Node.anchor anchor] pos contents

      let label_edgea ?style:(style=[]) e contents = label_edge e (anchor `South :: style) 0.5 contents
      let label_edgeb ?style:(style=[]) e contents = label_edge e (anchor `North :: style) 0.5 contents
      let label_edgel ?style:(style=[]) e contents = label_edge e (anchor `East :: style) 0.5 contents
      let label_edger ?style:(style=[]) e contents = label_edge e (anchor `West :: style) 0.5 contents
      let label_edgebr ?style:(style=[]) e contents = label_edge e (anchor `NorthWest :: style) 0.5 contents
      let label_edgebl ?style:(style=[]) e contents = label_edge e (anchor `NorthEast :: style) 0.5 contents
      let label_edgear ?style:(style=[]) e contents = label_edge e (anchor `SouthWest :: style) 0.5 contents
      let label_edgeal ?style:(style=[]) e contents = label_edge e (anchor `SouthEast :: style) 0.5 contents
      let label_edgec ?style:(style=[]) e contents = label_edge e (anchor `Main :: style) 0.5 contents

      let edge_anchor a b style anchor pos contents = 
	let e = edge (arrow :: draw :: style) a b in
	let l = label_edge_anchor e anchor pos contents in
	e,l

      let edges_anchor l = 
	List.map (fun (s,e,style,anchor,pos,contents) -> edge_anchor s e style anchor pos contents) l
	  
      let edges_anchor_mid l = 
	List.map (fun (s,e,style,anchor,contents) -> edge_anchor s e style anchor 0.5 contents) l



      let edge_anchor_of_gentities a b style anchor pos contents = 
	let e = Edge.(of_gentities (arrow :: draw :: style) a b) in
	let l = label_edge_anchor e anchor pos contents in
	e,l

      let edges_anchor_of_gentities l = 
	List.map (fun (s,e,style,anchor,pos,contents) -> edge_anchor_of_gentities s e style anchor pos contents) l
	  
      let edges_anchor_mid_of_gentities l = 
	List.map (fun (s,e,style,anchor,contents) -> edge_anchor_of_gentities s e style anchor 0.5 contents) l



      let edge_anchor_of_edges a b style anchor pos contents = 
	let e = Edge.(of_gentities (arrow :: draw :: style) (Edge.to_gentity a) (Edge.to_gentity b)) in
	let l = label_edge_anchor e anchor pos contents in
	e,l

      let edges_anchor_of_edges l = 
	List.map (fun (s,e,style,anchor,pos,contents) -> edge_anchor_of_edges s e style anchor pos contents) l
	  
      let edges_anchor_mid_of_edges l = 
	List.map (fun (s,e,style,anchor,contents) -> edge_anchor_of_edges s e style anchor 0.5 contents) l


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

	let between_borders vpad hpad anchors styles info = 
	  let nodes = info.Matrix.nodes in 
	  let matrix = Matrix.mapi 
	    (fun i j node_info -> Node.Transfo.transform (styles i j) node_info) nodes 
	  in
	  let vector_anchor_x node a a' = 
	    let (x,_) = node.Node.anchor a in
	    let (x',_) = node.Node.anchor a' in
	    x' -. x
	  in
	  let width = Array.length matrix.(0) in
	  let height = Array.length matrix in
	  let widths = Array.make width 0. in
	  let heights = Array.make height 0. in
	  let _ = begin widths.(0) <- vector_anchor_x matrix.(0).(0) `Pdf anchors.(0) end in
	  let _ = for j = 1 to width - 1 do
	      widths.(j) <- 	widths.(j-1)
	      +. (hpad j)
	      +. (fun_max (fun i -> vector_anchor_x matrix.(i).(j-1) (anchors.(j-1)) `East) height)
	      +. (fun_max (fun i -> vector_anchor_x matrix.(i).(j) `West (anchors.(j))) height)
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
		+. (fun_max ~max:(fun y y' -> min y y') (fun j -> snd (matrix.(i-1).(j).Node.anchor `South)) width)
		(*  minus the greatest y1 of the present row (because we go downwards) *)
		-. (fun_max (fun j -> snd (matrix.(i).(j).Node.anchor `North)) width)
	      end ;
	    done ;
	  in fun i j -> (widths.(j), heights.(i))

	let array anchors ?vertical_padding:(vpad=fun _ -> 1.) ?horizontal_padding:(hpad=fun _ -> 1.)
	    ?all_node_styles:(all_node_styles=[])
	    ?main_node_style:(mstyle=Node.([at (0.,0.);anchor `SouthWest]))
	    (* Mettre la valeur par defaut en ex *)
	    lines =
	  let style i j = [Node.anchor (List.nth anchors j); Node.at(0.,0.)] in
	  let anchors = Array.of_list anchors in
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
	  math_matrix (Matrix.placement (between_borders vpad hpad anchors style) ::
		       Matrix.mainNode mstyle ::
		       all_node_styles)
	    lines_contents 


      end

      let array = Arr.array

    end

    open Box



    let default_where ms =  
      let point_of_node n = n.Node.anchor `Main in
      Point.middle (point_of_node ms.(0).(0)) (point_of_node ms.(0).(1))

    let default_deco env ms = [], default_where ms

    let xarrow ?margin:(margin=2.) ?decoration:(deco=default_deco) 
	a = 
      [Maths.Binary { Maths.bin_priority = 2 ; Maths.bin_drawing = Maths.Normal 
	  (true, 
           (Maths.noad
              (fun env st->
		let dr=Document.draw_boxes env (Maths.draw [{env with mathStyle = Mathematical.Script}] a) in
		let (x0,y0,x1,y1)=match dr with [] -> (0.,0.,0.,0.) | _ -> OutputCommon.bounding_box dr 
		in
		(* let _ = Printf.fprintf stderr "Bb: %f,%f,%f,%f\n" x0 y0 x1 y1 ; flush stderr in *)
		let matrix = Matrix.(make env
		  [placement (between_centers 1. (x1 -. x0 +. 2. *. margin));
		   mainNode Node.([
		   innerSep 0. ; outerSep 0. ;
		     rectangle env ;
		     anchor `Pdf ;
		   at (0., ex env)])] 
		  Node.([[
		     ([innerSep 0.;outerSep 0.], []);
		     ([innerSep 0.;outerSep 0.], [])
		   ]]))
		in
		let m,ms = Matrix.(matrix.mainNode,matrix.nodes) in
		let e, where = deco env ms in
		let l = Node.(make_output
		  [outerSep 0.2 ; innerSep 0.; anchor `South;
		   default_shape env ; at where] dr)
		in
		let drawn = 
		  drawing
		    (List.flatten (List.map Entity.to_raw_list
		       (Node l :: Matrix matrix :: e)))
		in 
		let width = drawn.drawing_min_width in
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

    let xto = xarrow ~decoration:(fun env ms ->
      let e = Edge.(make [draw;lineWidth 0.1;arrow] ms.(0).(0) ms.(0).(1)) in
      [Edge e], default_where ms)
    let xot = xarrow ~decoration:(fun env ms ->
      let e = Edge.(make [draw;lineWidth 0.1;arrow] ms.(0).(1) ms.(0).(0)) in
      [Edge e], default_where ms)
