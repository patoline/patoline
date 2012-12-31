
let debug = ref false

exception Bad

type point = float * float
type interval = point * point
type vecteur = point
type droite = point *vecteur

let (--) (x,y) (x',y') = (x -. x', y -. y') 
let (++) (x,y) (x',y') = (x +. x', y +. y') 
let comblin a (x,y) a' (x',y') =
   (a *. x +. a' *. x', a *. y +. a' *. y') 
let opp (x,y) = (-.x,-.y)

(* determinant de p, q *)
let det22 (xp,yp) (xq,yq) =
  (yq *. xp -. xq *. yp)

(* coordonnée de v dans la base p q *)
let solve22 p q v =
  let d = det22 p q in
  det22 v q /. d, det22 v p /. d

let solve22' p q v =
  let d = det22 p q in
  det22 v q /. d

let coef_inter_lines (p1,d1) (p2,d2) =
  let l, m = solve22 d1 d2 (p2--p1) in
  l, m, comblin 1.0 p1 l d1

let rotate_90 (x,y) =
  (-.y , x)
let rotate_180 (x,y) =
  (-.x , -.y)
let rotate_270 (x,y) =
  (y , -.x)

let dot_prod (x,y) (x',y') =
  x *. x' +. y *. y'

let norm2 v = dot_prod v v

let norm v = sqrt (norm2 v)

type lin_obj =
  Line of point * vecteur
| HalfLine of point * vecteur * bool
| Segment of point * point

let print_pt ch (x,y) =Printf.fprintf ch "(%f,%f)" x y

let print_pt' ch = List.iter (print_pt ch)

let print_obj ch = function
  | Line(p,v) -> Printf.fprintf ch "(%a, %a)" print_pt p print_pt v
  | HalfLine(p,v,b) -> Printf.fprintf ch "[%a, %a, %b)" print_pt p print_pt v b
  | Segment(p,q) -> Printf.fprintf ch "[%a, %a]" print_pt p print_pt q

let carrier = function
  | Line(p,v) | HalfLine(p,v,_) -> p, v
  | Segment(p1,p2) -> p1, (p2 -- p1)

let cut p = function
  | Line(_,v) -> Line(p,v)
  | HalfLine(_,v,b) -> HalfLine(p,v,b) 
  | Segment(q,_) -> Segment(q,p)

let renverse = function
  | Line(p,v) -> Line(p,opp v)
  | HalfLine(p,v,b) -> HalfLine(p,opp v,not b) 
  | Segment(p,q) -> Segment(q,p)

let in_range o l = 
  compare l nan <> 0 && compare l infinity <> 0 &&   compare l (-. infinity) <> 0 &&
  match o with
    Line _ -> true
  | HalfLine(_, _, b) -> if b then l >= 0.0 else  l <= 0.0
  | Segment _ -> l >= 0.0 && l <= 1.0

let inter o1 o2 =
  let d1 = carrier o1 in
  let d2 = carrier o2 in
  let l1, l2, x = coef_inter_lines d1 d2 in
(*
  Printf.fprintf stdout "inter: %a et %a => l1 = %f, l2 = %f, i = %a\n"
    print_obj o1
    print_obj o2
    l1 l2 print_pt x;
*)
  if in_range o1 l1 &&  in_range o2 l2 then x
  else raise Not_found

  
(*  
let _O = (0.,0.)
let _A = (0.5,-0.5)
let _B = (1.5,0.5)
let _C = (0.5,1.5)
let i = (1.,0.)
let j = (0.,1.)
let _Ox = Line(_O,i)
let _Oy = Line(_O,j)
let d1 = HalfLine(_A,(1.0,2.0))
let p1x = inter d1 _Ox
let p1y = inter d1 _Oy
*)


let next dsup p c =  match c with
    [] -> HalfLine(p,dsup,true)
  | p'::_ -> Segment(p,p') 

let profil_union (dsup, dinf) c1 c2 =
  match c1,c2 with
    [],_ -> c2
  | _,[] -> c1
  | (p1::_),(p2::_) ->

  let dinf' = rotate_90 dinf in
  let dsup' = rotate_270 dsup in


  let c1,s1,c2,s2 =
    if dot_prod p1 dinf' > dot_prod p2 dinf' then
      c1, HalfLine(p1,dinf,true), c2, HalfLine(p2,dinf,true)
    else
      c2, HalfLine(p2,dinf,true), c1, HalfLine(p1,dinf,true)
  in

  let cons p l = match l with
      [] -> [p]
    | p'::_ -> if norm2 (p -- p') < 1e-6 then l else p::l
  in

(*   Printf.fprintf stdout "\nStart\n"; *)
  (*c1 et c2 trié selon la direction d' *)
  let rec fn acc c1 s1 c2 s2 = 


(*     Printf.fprintf stdout "acc = %a,\n c1 = %a, s1 = %a,\n c2 = %a, s2 = %a\n\n"
      print_pt' acc
      print_pt' c1
      print_obj s1
      print_pt' c2
      print_obj s2;*)

    match c1, c2 with
    | [], [] -> List.rev acc
    | p1::__, p2::c2' when dot_prod p1 dsup' >= dot_prod p2 dsup' ->
      let s2' = next dsup p2 c2' in
      fn acc c1 s1 c2' s2' 
    | p1::c1', _ ->
      let s1' = next dsup p1 c1' in
      (try 
	let i = inter s1' s2 in
	fn (cons i (cons p1 acc)) c2 s2 c1' s1'
      with 
	Not_found ->
	  fn (cons p1 acc) c1' s1' c2 s2)
    | [], _::_ -> List.fold_left (fun acc x -> cons x acc)  c2 acc
  in fn [] c1 s1 c2 s2

(*
let test0 = profil_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0)] [(0.0,2.0)]
let test1 = profil_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [0.5,1.0]
let test2 = profil_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(0.0,2.0);(-1.0,4.0)]
let test3 = profil_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(-1.0,2.0)] [(-1.0,2.0);(-1.0,4.0)]
let test4 = profil_union ((-1.0,1.0), (-1.0,-1.0)) test2 test3
*)


let segment_profile (dsup,dinf) (p,q) =
  let v = q -- p in
  match det22 dsup v >= 0.0, det22 dinf v >= 0.0 with
    true, false -> [p]
  | false, true -> [q]
  | true, true -> [q; p]
  | false, false -> [p; q]

let bezier_profile (dsup,dinf) epsilon curves =
  if !debug then begin
    Printf.fprintf stderr "Compute profile:\n";
      List.iter (fun (xa, ya) ->
	Printf.fprintf stderr "["; 
	Array.iteri (fun i x -> Printf.fprintf stderr "(%f, %f)" x ya.(i))
          xa;
	Printf.fprintf stderr "]  ") curves;
      Printf.fprintf stderr "\n";
  end;
      
  let curves = List.fold_left (fun acc b -> Bezier.subdivise epsilon b :: acc) [] curves in
  let r = List.fold_left (fun acc curve ->
    let curve = Array.of_list curve in
    let rec gn i j =
      if i = j then (
	let xa, ya = curve.(i) in
	let len = Array.length xa in
	segment_profile (dsup, dinf) ((xa.(0), ya.(0)), (xa.(len - 1), ya.(len - 1)))
      ) else (
	let k = (i + j) / 2 in
	profil_union (dsup,dinf) (gn i k) (gn (k+1) j))
    in
    profil_union (dsup, dinf) acc (gn 0 (Array.length curve - 1))) [] curves
  in
  if !debug then begin
    Printf.fprintf stderr "  ==>";
    List.iter (fun p -> print_pt stderr p) r;
    Printf.fprintf stderr "\n";
  end;
  r

let middle p q = comblin 0.5 p 0.5 q

let normalize (x,y as v) = 
  let n = norm v in
  (x /. n, y /. n)

let bissectrice o1 o2 =
  let p,v1 = carrier o1 in
  let q,v2 = carrier o2 in
  let v = normalize v1 ++ normalize v2 in
  let v' = rotate_90 v in
  let p' = try inter (Line(q,v2)) (Line(p,v')) with Not_found -> 
    Printf.fprintf stderr "Bad bissectrice: %a %a\n" print_obj o1 print_obj o2;
    raise Bad in
  Line(middle p p',v)

let project p o =
  let q,v = carrier o in
  let s = dot_prod v (p -- q) /. norm2 v in
  if not (in_range o s) then raise Not_found else
  comblin 1.0 q s v

(* assume profile2 uses (-dsup, -dinf), hence the list reversal *)
let bissectrice_profile (dsup,dinf) profile1 profile2 =
(*
  Printf.printf "left: ";
  List.iter (fun p -> print_pt stdout p) profile1;
  Printf.printf "\nright: ";
  List.iter (fun p -> print_pt stdout p) profile2;
  Printf.printf "\n";
*)

  let profile2 = List.rev profile2 in
  let dinf' = opp dsup in
  let dsup' = opp dinf in
  let init =
    HalfLine(List.hd profile1,dinf,true),
    true,
    HalfLine(List.hd profile2,dinf',true)
  in
(*  Printf.printf "\n\nStart\n";*)

  let rec fn acc (l,b,r) c1 c2 =
    (* b : the bissectrice is active *)

    let _ = 
      try ignore (inter l r); raise Exit with Not_found -> ()
    in
    match l with
      Segment(a,b) when norm2 (a -- b) < 1e-8 ->
	(match c1 with
	  [] -> assert false
	| p1::c1' ->
	  let l' = renverse (next dsup p1 c1') in
	  (*Printf.printf "short l\n";*)
	  fn acc (l', false, r) c1' c2)
   | _ ->
   match r with
      Segment(a,b) when norm2 (a -- b) < 1e-8 ->
	(match c2 with
	  [] -> assert false
	| p2::c2' ->
	  let r' = renverse (next dsup' p2 c2') in
	  (*Printf.printf "short r\n";*)
	  fn acc (l, false, r') c1 c2')
   | _ ->
	
    let m = bissectrice l r in
    (*Printf.printf "l = %a, r = %a, m = %a, c1 = %a, c2 = %a\n"
      print_obj l print_obj r
      print_obj m print_pt' c1 print_pt' c2;*)
      
    let x, v = carrier m in

    let _ = 
      let v' = rotate_90 v in
      let p, _ = carrier l in
      let q, _ = carrier r in
      (try
	let q' = inter r (Line(p,v')) in
	if dot_prod (q' -- p) v' <= 0.0 then raise Exit
      with Not_found -> ());
      (try
	let p' = inter l (Line(q,v')) in
	if dot_prod (q -- p') v' <= 0.0 then raise Exit
      with Not_found -> ());
    in

    if b then begin
      match c1, c2 with
	[], [] -> List.rev acc

      | (p1::c1'), [] ->
	let i1 = project p1 m in
	if norm2 (i1 -- p1) < 1e-8 then raise Exit;
	(try
	  let n2 = inter (Line(i1,i1--p1)) r in
	  let l' = renverse (next dsup p1 c1') in
	  fn ((p1,i1,n2)::acc) (l', false, cut n2 r) c1' []
	with
	  Not_found ->
	    Printf.fprintf stderr "Missing intersection (1a)\n";
	    raise Bad)
	  
      | [], (p2::c2') ->
	let i2 = project p2 m in
	if norm2 (i2 -- p2) < 1e-8 then raise Exit;
	(try
	  let n1 = inter (Line(i2,i2--p2)) l in
	  let r' = renverse (next dsup' p2 c2') in
	  fn ((n1,i2,p2)::acc) (cut n1 l, false, r') [] c2'
	with
	  Not_found ->
	    Printf.fprintf stderr "Missing intersection (1b)\n";
	    raise Bad)
 	  
      | (p1::c1'), (p2::c2') ->
	let i1 = project p1 m in
	let i2 = project p2 m in
	if norm2 (i1 -- p1) < 1e-8 || norm2 (i2 -- p2) < 1e-8 then raise Exit;
	let l' = renverse (next dsup p1 c1') in
	let r' = renverse (next dsup' p2 c2') in
	(*Printf.printf "Project new: i1 = %a, i2 = %a\n" print_pt i1 print_pt i2;*)

	let x1 = dot_prod v (i1 -- x) and x2 = dot_prod v (i2 -- x) in
	try 
	  if abs_float (x1 -. x2) < 1e-6 then (
	    (*Printf.printf "Cas p1 = %a, i12 = %a, p2 = %a\n" print_pt p1 print_pt i1 print_pt p2;*)
	    fn ((p1,i1, p2)::acc) (l', false, r') c1' c2')
	  else if x1 > x2 then (
	    if !debug then Printf.fprintf stderr "inter %a %a\n" print_obj (Line(i1,i1--p1)) print_obj r;
	    let n2 = inter (Line(i1,i1--p1)) r in
	    (*Printf.printf "Cas p1 = %a, i1 = %a, n2 = %a\n" print_pt p1 print_pt i1 print_pt n2;*)
	    fn ((p1,i1,n2)::acc) (l', false, Segment(p2,n2)) c1' c2)
	  else (
	    if !debug then Printf.fprintf stderr "inter %a %a\n" print_obj (Line(i2,i2--p2)) print_obj l;
	    let n1 = inter (Line(i2,i2--p2)) l in
	    (*Printf.printf "Cas n1 = %a, i2 = %a, p2 = %a\n" print_pt n1 print_pt i2 print_pt p2;*)
	    fn ((n1,i2,p2)::acc) (Segment(p1,n1), false, r') c1 c2')
	with
	  Not_found ->
	    Printf.fprintf stderr "Missing intersection (2)\n";
	    raise Bad
    end else begin

      match acc with 
	[] -> assert false
      | (o1,i,o2)::_ ->
	let x = dot_prod v (o2 -- o1) in
	if abs_float x < 1e-8 then begin
	  fn acc (l,true,r) c1 c2
	end else
	  try if x > 0.0 then begin 
	    let i1 = project o1 m in	    
	    let n2 = inter (Line(i1,i1--o1)) r in
	    (*Printf.printf "Cas o1 = %a, i1 = %a, n2 = %a\n" print_pt o1 print_pt i1 print_pt n2;*)
	    fn ((o1,i1,n2)::acc) (l, true, cut n2 r) c1 c2
	  end else begin
	    let i2 = project o2 m in
	    let n1 = inter (Line(i2,i2--o2)) l in
	    (*Printf.printf "Cas n1 = %a, i2 = %a, o2 = %a\n" print_pt n1 print_pt i2 print_pt o2;*)
	    fn ((n1,i2,o2)::acc) (cut n1 l, true, r) c1 c2
	  end
	  with
	    Not_found -> 
	      match c1, c2 with
		[], [] -> assert false
	      | p1::c1', [] ->
		let l' = renverse (next dsup p1 c1') in
		(*Printf.printf "Cas p1 = %a, mid = %a, o2 = %a\n"
		  print_pt p1 print_pt (middle p1 o2) print_pt o2;*)
		fn ((p1,middle p1 o2,o2)::acc) (l', false, r) c1' c2
	      | [], p2::c2' ->
		let r' = renverse (next dsup' p2 c2') in
		(*Printf.printf "Cas o1 = %a, mid = %a, p2 = %a\n"
		  print_pt o1 print_pt (middle o1 p2) print_pt p2;*)
		fn ((o1,middle o1 p2,p2)::acc) (l, false, r') c1 c2'
	      | p1::c1', p2::c2' ->
		let vert = rotate_90 (o1 -- o2) in
		if dot_prod p1 vert > dot_prod p2 vert then
		  let l' = renverse (next dsup p1 c1') in
		  (*Printf.printf "Cas p1 = %a, mid = %a, o2 = %a\n"
		    print_pt p1 print_pt (middle p1 o2) print_pt o2;*)
		  fn ((p1,middle p1 o2,o2)::acc) (l', false, r) c1' c2
		else
		  let r' = renverse (next dsup' p2 c2') in
		  (*Printf.printf "Cas o1 = %a, mid = %a, p2 = %a\n"
		    print_pt o1 print_pt (middle o1 p2) print_pt p2;*)
		  fn ((o1,middle o1 p2,p2)::acc) (l, false, r') c1 c2'
    end
  in
  fn [] init profile1 profile2

(*
let testb1 = bissectrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.0,1.0)]
let testb2 = bissectrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.25,1.25);(1.25,0.75)]
let testb3 = bissectrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.);(3.,2.);(2.,1.)]
let testb4 = bissectrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.5);(3.,2.5);(2.,1.5)]
let testb5 = bissectrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.25);(3.,2.25);(2.,1.25)]
*)

(* FIXME: set are better *)
let add_dprofile a p q l =
  let rec fn acc = function
      [] -> List.rev_append acc [a,p,q] 
    | ((b,p',q' as c)::l') as l ->
      if abs_float (a -. b) < 1e-9 then List.rev_append acc ((b,p+.p',q+.q')::l')
      else if a < b then List.rev_append acc ((a,p,q)::l)
      else fn (c::acc) l'
  in
  fn [] l

let area2 o1 p1 o2 p2 =
  let r =
    det22 (o2 -- o1) (p1 -- o1) +.
      det22 (o2 -- p1) (p2 -- p1)
  in
  abs_float r /. 2.0

let swap_min a b = 
  let a1 = min a b in
  let b1 = max (max a b) (a1 +. 1e-6) in
  a1, b1

let add_pqprofile o1 p1 o2 p2 l =
  (*Printf.printf "addpq o1 = %a, o2 = %a, p1 = %a, p2 = %a\n"
    print_pt o1
    print_pt o2
    print_pt p1
    print_pt p2;*)
  let s = area2 o1 p1 o2 p2 in
  let a = norm (o2 -- o1) in
  let b = norm (p2 -- p1) in
  let a,b,o1,o2,p1,p2 =
    if a <= b then a,b,o1,o2,p1,p2
    else b,a,p2,p1,o2,o1
  in
  if norm2 (o1 -- p1) < 1e-8 || norm2 (o2 -- p2) < 1e-8 then
    if b -. a < 1e-6 then l else
      let h = s /. (b -. a) in
      add_dprofile a h 0.0 (add_dprofile b (-.h) 0.0 l) 
  else if b -. a < 1e-6 then begin
    let a, b = swap_min a b in
    let h = s /. (b -. a) in
    add_dprofile a h 0.0 (add_dprofile b (-.h) 0.0 l) 
  end else begin
    let d2 = det22 (p2 -- p1) (o2 -- o1) in
    (*Printf.printf "  d2 = %f\n" d2; *)
    assert (abs_float d2 < 1e-6);
    let c = norm (middle o2 o1 -- middle p2 p1) in
    let p' = c /. (b -. a) in
    let a' = a *. p' in
    let p = p' /. 2. in    
    add_dprofile a a' p (add_dprofile b (-.a') (-.p) l) 
  end
  

let add_qprofile (m,l) o1 p1 o2 p2 =
  (*Printf.printf "addq o1 = %a, o2 = %a, p1 = %a, p2 = %a\n"
    print_pt o1
    print_pt o2
    print_pt p1
    print_pt p2;*)
  try
    if norm2 (o2 -- p2) < 1e-8 then
      let i1 = project o2 (Segment(o1,p1)) in
      min m (norm (o2 -- i1)),
      add_pqprofile o1 i1 o2 o2 
	(add_pqprofile i1 p1 o2 p2 l)
    else raise Not_found
  with Not_found -> try
    if norm2 (o1 -- p1) < 1e-8 then
      let i2 = project o1 (Segment(o2,p2)) in
      min m (norm (o1 -- i2)),
      add_pqprofile o1 o1 o2 i2 
	(add_pqprofile o1 p1 i2 p2 l)
    else raise Not_found
  with Not_found ->
    min  m (norm (p1 -- p2)),
    add_pqprofile o1 p1 o2 p2 l

let add_allprofile l =
  let rec fn (m, l as acc) = function
     [o1,_,o2] -> 
       let d = norm (o2 -- o1) in
       m, add_dprofile d d 0.5 l
    | (o1,_,o2)::((p1,_,p2)::_ as l') ->
	fn (add_qprofile acc o1 p1 o2 p2) l'
    | [] -> assert false
  in
  let o1,_,o2 = List.hd l in
  let d = norm (o2 -- o1) in
  fn (d, add_dprofile d d 0.5 []) l

let find_distance0 area l =
  let rec fn curd curh curarea curp l =
    (*Printf.printf "curd = %f, curh = %f, curarea = %f, curp = %f\n" curd curh curarea curp;*)
    if area < curarea then
      (Printf.fprintf stderr "area = %f < curarea = %f\n" area curarea; assert false);
    let da = area -. curarea in
    let delta = curh*.curh +. 2. *. curp *. da in
    let dd = curd +. 2.0 *. da /. (curh +. sqrt(delta)) in
    match l with
      (a,dh,p)::l ->
	if dd <= a then dd else
	  let curarea = curarea +. (a -. curd) *. (curh +. (a -. curd) *. curp /. 2.) in 
	  let curh = curh +. dh in
	  fn a curh curarea (p +. curp) l
    | [] -> dd
  in
  fn 0.0 0.0 0.0 0.0 l

let find_distance2 alpha l =
  let rec fn curd curh curarea curp l =
   (*Printf.printf "curd = %f, curh = %f, curarea = %f, curp = %f\n" curd curh curarea curp;*)
   (* (curh *x + curp /2 * x^2 + curarea = alpha * (curd + x)^2  *)
    let a = curp /. 2. -. alpha in
    let b = curh -. 2. *. alpha *. curd in
    let c = curarea -. alpha *. curd *. curd in
    let delta = b*.b -. 4. *. a *. c in
    let r1, r2 = 
      if b > 0.0 then
	-. 2.0 *. c /. (b +. sqrt(delta)), b +. sqrt(delta) /. (-. 2. *. a)
      else
	-. 2.0 *. c /. (b -. sqrt(delta)), b -. sqrt(delta) /. (-. 2. *. a)
    in
    let r = 
      if r1 < 0.0 then r2 else if r2 < 0.0 then r1 else min r1 r2
    in
    let dd = curd +. r in
    match l with
      (a,dh,p)::l ->
	if r >= 0.0 && curd <> 0.0 && dd <= a then dd else
	  let curarea = curarea +. (a -. curd) *. (curh +. (a -. curd) *. curp /. 2.) in 
	  let curh = curh +. dh in
	  fn a curh curarea (p +. curp) l
    | [] -> dd
  in
  fn 0.0 0.0 0.0 0.0 l

let distance alpha (dsup,dinf) profile1 profile2 =
  if !debug then begin
    Printf.fprintf stderr "distance:\n  left: ";
    List.iter (fun p -> print_pt stderr p) profile1;
    Printf.fprintf stderr "\n  right: ";
    List.iter (fun p -> print_pt stderr p) profile2;
    Printf.fprintf stderr "\n";
  end;

  let r = 
    if profile1 = [] || profile2 = [] then infinity else begin
      try
	let b = bissectrice_profile (dsup,dinf) profile1 profile2 in
	let m,l = add_allprofile b in
      (*Printf.printf "m = %f\n" m;
	List.iter (fun (a,h,p) -> Printf.printf "a = %f h = %f, p = %f -- " a h p) l;
	Printf.printf "\n"; *)
      (*let area = m *. m *. alpha in*)
	if m <= 0.0 then 0.0 else find_distance2 alpha l
      with
	Exit -> 0.0
      | Bad -> infinity
    end
  in
  if !debug then Printf.fprintf stderr "  ==> %f\n" r;
  r


let testc1 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.0,1.0)]
let testc2 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.25,1.25);(1.25,0.75)]
let testc3 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.);(3.,2.);(2.,1.)]
let testc4 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.5);(3.,2.5);(2.,1.5)]
let testc5 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.25);(3.,2.25);(2.,1.25)]

