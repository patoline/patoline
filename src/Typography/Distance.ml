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

let debug = ref false
let debug_union = ref false
let debug_mediatrice = ref false
let debug_bissectrice = ref false
let debug_addpro = ref false

type point = float * float
type interval = point * point
type vecteur = point
type droite = point *vecteur

let (--) (x,y) (x',y') = (x -. x', y -. y') 
let (++) (x,y) (x',y') = (x +. x', y +. y') 
let comblin a (x,y) a' (x',y') =
   (a *. x +. a' *. x', a *. y +. a' *. y') 
let opp (x,y) = (-.x,-.y)
let sprod l (x,y) = l *. x, l *. y
 
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

let near p q =
  p == q || p = q ||
  norm2 (p -- q) /. (max (norm2 p) (norm2 q)) < 1e-8

let norm v = sqrt (norm2 v)

let normalize (x,y as v) = 
  let n = norm v in
  x /. n, y /. n

type lin_obj =
  Line of point * vecteur
| HalfLine of point * vecteur * bool
| Segment of point * point

let print_pt ch (x,y) =Printf.fprintf ch "(%f,%f)" x y

let print_pt16 ch (x,y) =Printf.fprintf ch "(%.20f,%.20f)" x y

let print_pto ch = function
  | `Some p -> print_pt ch p
  |  _ -> Printf.fprintf ch "None"

let print_pt' ch = List.iter (print_pt ch)

let print_obj ch = function
  | Line(p,v) -> Printf.fprintf ch "(%a, %a)" print_pt p print_pt v
  | HalfLine(p,v,b) -> Printf.fprintf ch "[%a, %a, %b)" print_pt p print_pt v b
  | Segment(p,q) -> Printf.fprintf ch "[%a, %a]" print_pt p print_pt q

let carrier = function
  | Line(p,v) | HalfLine(p,v,_) -> p, v
  | Segment(p1,p2) -> p1, (p2 -- p1)

let length2 = function
    Segment(p,q) -> norm2 (p -- q)
  | _ -> infinity

let cut' p = function
  | Line(_,v) -> assert false
  | HalfLine(q,v,true) -> Segment(q,p)
  | HalfLine(q,v,false) ->  HalfLine(p,v,false)
  | Segment(q,_) -> Segment(q,p)

let cut p = function
  | Line(_,v) -> Line(p,v)
  | HalfLine(_,v,b) -> HalfLine(p,v,b) 
  | Segment(_,q) -> Segment(p,q)

let renverse = function
  | Line(p,v) -> Line(p,opp v)
  | HalfLine(p,v,b) -> HalfLine(p,opp v,not b) 
  | Segment(p,q) -> Segment(q,p)

let in_range o l = 
  compare l nan <> 0 && compare l infinity <> 0 &&   compare l (-. infinity) <> 0 &&
  match o with
    Line _ -> true
  | HalfLine(_, _, b) -> if b then l >= -1e-8 else  l <= 1e-8
  | Segment _ -> l >= -1e-8 && l <= 1. +. 1e-8

let in_range_far o l = 
  compare l nan <> 0 && compare l infinity <> 0 &&   compare l (-. infinity) <> 0 &&
  match o with
    Line _ -> true
  | HalfLine(_, _, b) -> if b then l >= 1e-8 else  l <= -1e-8
  | Segment _ -> l >= 1e-8 && l <= 1. -. 1e-8

exception Extrem

let in_range' o l = 
  if not (compare l nan <> 0 && compare l infinity <> 0 &&   compare l (-. infinity) <> 0) then raise  Not_found;
  match o with
    Line _ -> ()
  | HalfLine(_, _, b) ->
    if (b && l < 1e-8) then raise Extrem;
    if (not b && l > 1e-8) then raise Not_found;
  | Segment _ -> 
    if l < 1e-8 then raise Extrem;
    if l > 1.0 +. 1e-8 then raise Not_found

let inter o1 o2 =
  let d1 = carrier o1 in
  let d2 = carrier o2 in
  let l1, l2, x = coef_inter_lines d1 d2 in
(*
  Printf.fprintf stderr "inter: %a et %a => l1 = %f, l2 = %f, i = %a\n"
    print_obj o1
    print_obj o2
    l1 l2 print_pt x;
*)
  if in_range o1 l1 && in_range o2 l2 then x
  else raise Not_found

let inter_far o1 o2 =
  let d1 = carrier o1 in
  let d2 = carrier o2 in
  let l1, l2, x = coef_inter_lines d1 d2 in
(*
  Printf.fprintf stderr "inter: %a et %a => l1 = %f, l2 = %f, i = %a\n"
    print_obj o1
    print_obj o2
    l1 l2 print_pt x;
*)
  if in_range_far o1 l1 && in_range_far o2 l2 then x
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
    [] -> HalfLine(p,opp dsup,false)
  | p'::_ -> Segment(p',p) 

let next' dsup p c =  match c with
    [] -> HalfLine(p,dsup,true)
  | p'::_ -> Segment(p',p) 

let hd_dir = function
  [] -> `Sup
  | x::_ -> `Some x

let opt_dir = function
  None -> `Inf
  | Some x -> `Some x

let rec push x = function
    [] -> [x]
  | [x'] when near x x' -> [x']
  | x'::(x''::_ as l) when near x x' || abs_float (det22 (x' -- x'') (x -- x')) /. (norm (x -- x') *. norm (x' -- x'')) < 1e-5 -> push x l
  | l -> x::l

let profile_union (dsup, dinf) c1 c2 =
  if c1 = [] then c2 else if c2 = [] then c1 else (

    if !debug_union then begin
     Printf.fprintf stderr "Start union: dsup = %a, dinf = %a, c1 = %a,\n c2 = %a\n"
      print_pt dsup
      print_pt dinf
      print_pt' c1
      print_pt' c2
    end;


  let vert = dsup -- dinf in

  let eliminate p2 c2 p1 =
    det22 dsup (p1 -- p2) >= 1e-8 &&
    match c2 with 
      p2'::_ ->
        det22 (p1 -- p2) (p2' -- p2) >= 1e-8 && det22 (p1 -- p2') dinf >= 1e-8
    | _ ->  det22 (p1 -- p2) dinf >= 1e-8
  in

  let hn acc c = match acc, c with
      [], [] -> assert false
    | (x::_), (y::_) -> Segment(x, y)
    | [], (y::_) -> HalfLine(y,dinf,true)
    | (x::_), [] -> HalfLine(x,dsup,true)
  in

  let rec gn acc1 c1 acc2 c2 = 
    
    if !debug_union then begin
     Printf.fprintf stderr "gn: acc1 = %a\n c1 = %a\n acc2 = %a\n c2 = %a\n\n"
      print_pt' acc1
      print_pt' c1
      print_pt' acc2
      print_pt' c2
    end;

    let acc1, acc2 = 
      try 
        let o1 = hn acc1 c1 in
        let o2 = hn acc2 c2 in
        let i = inter o1 o2 in
        (push i acc1), (push i acc2)
      with Not_found -> acc1, acc2
    in

    match c1, c2 with
    | (p1::c1'), (p2::c2') ->
      let x1 = dot_prod p1 vert and x2 = dot_prod p2 vert in
      if x1 <= x2 then
        gn (push p1 acc1) c1' acc2 c2
      else
        gn acc1 c1 (push p2 acc2) c2'
    | (p1::c1'), [] ->
        gn (push p1 acc1) c1' acc2 c2
    | [], (p2::c2') ->
        gn acc1 c1 (push p2 acc2) c2'
    | [], [] ->
        acc1, acc2

  in
  let c1, c2 = gn [] c1 [] c2 in

  let rec fn acc c1 c2 = 

    if !debug_union then begin
     Printf.fprintf stderr "fn : acc = %a\n c1 = %a\n c2 = %a\n\n"
      print_pt' acc
      print_pt' c1
      print_pt' c2
    end;

    match c1, c2 with
    | p1::c1', p2::c2' ->
      if eliminate p2 c2' p1 then
        fn acc c1' c2
      else if eliminate p1 c1' p2 then
        fn acc c1 c2'
      else
        let x1 = dot_prod p1 vert and x2 = dot_prod p2 vert in
        if x1 >= x2 then
          fn (push p1 acc) c1' c2
        else
          fn (push p2 acc) c1 c2'
      | [], p2::c2' -> 
        fn (push p2 acc) c1 c2'
      | p1::c1', [] ->
        fn (push p1 acc) c1' c2
      | [], [] -> acc

  in fn [] c1 c2)

(*
let test0 = profile_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0)] [(0.0,2.0)]
let test1 = profile_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [0.5,1.0]
let test2 = profile_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(0.0,2.0);(-1.0,4.0)]
let test3 = profile_union ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(-1.0,2.0)] [(-1.0,2.0);(-1.0,4.0)]
let test4 = profile_union ((-1.0,1.0), (-1.0,-1.0)) test2 test3
let test5 = profile_union ((-1.0,1.0), (-1.0,-1.0)) [(0.,0.);(-1.1,1.0)] [(-1.1,2.0);(0.,3.)]
*)


let segment_profile (dsup,dinf) p q =
  let v = q -- p in
  match det22 dsup v >= 0.0, det22 dinf v >= 0.0 with
    true, false -> [p]
  | false, true -> [q]
  | true, true -> [q; p]
  | false, false -> [p; q]

let bezier_profile (dsup,dinf as dirs) epsilon curves0 =
  if !debug then begin
    Printf.fprintf stderr "Compute profile:\n";
    List.iter (fun (xa, ya) ->
      Printf.fprintf stderr "["; 
      Array.iteri (fun i x -> Printf.fprintf stderr "(%f, %f)" x ya.(i))
        xa;
      Printf.fprintf stderr "]  ") curves0;
    Printf.fprintf stderr "\n";
  end;

  let curves = List.rev (List.fold_left (fun acc b -> Bezier.subdivise epsilon b :: acc) [] curves0) in

  if !debug then begin
    Printf.fprintf stderr "  Subdivise ==>\n";
    List.iter (fun c -> List.iter (fun (xa, ya) ->
      Printf.fprintf stderr "["; 
      Array.iteri (fun i x -> Printf.fprintf stderr "(%f, %f)" x ya.(i))
          xa;
      Printf.fprintf stderr "]  ") c) curves;
    Printf.fprintf stderr "\n";
  end;

  let r = List.fold_left (fun acc curve ->
    let curve = Array.of_list curve in
    let rec gn i j =
      if i = j then (
        let xa, ya = curve.(i) in
        let len = Array.length xa in
        segment_profile dirs (xa.(0), ya.(0)) (xa.(len - 1), ya.(len - 1))
      ) else (
        let k = (i + j) / 2 in
        profile_union (dsup,dinf) (gn i k) (gn (k+1) j))
    in
    profile_union dirs acc (gn 0 (Array.length curve - 1))) [] curves
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
  let v1 = normalize v1 in
  let v2 = normalize v2 in

  let v, v' = 
    let v = v1 ++ v2 in
    let w = v1 -- v2 in
    if norm2 v < norm2 w then if det22 v1 v2 >= 0. then rotate_90 w, w else rotate_270 w, w
    else v, rotate_90 v
  in

  let p' = (*try*) inter (Line(q,v2)) (Line(p,v')) (*with Not_found -> 
    Printf.fprintf stderr "Bad bissectrice: %a %a\n" print_obj o1 print_obj o2;
    assert false*) in
  Line(middle p p',v)

let mediatrice p1 p2 = 
  let v = rotate_90 (p2 -- p1) in
  Line(middle p1 p2, v)

let project p o =
  let q,v = carrier o in
  let s = dot_prod v (p -- q) /. norm2 v in
  if !debug_mediatrice then Printf.fprintf stderr "project s = %f\n" s;
  if not (in_range o s) then raise Not_found else
  comblin 1.0 q s v

type mstate =
  Bissectrice of lin_obj * lin_obj
| Mediatrice of point * lin_obj * lin_obj * point
| LeftParabola of point * lin_obj * lin_obj
| RightParabola of lin_obj * lin_obj * point
| LeftBack of point * lin_obj * lin_obj
| RightBack of lin_obj * lin_obj * point

let print_st ch s = match s with
  | Bissectrice(o1,o2) -> Printf.fprintf ch "Bissectrice(%a,%a)" print_obj o1 print_obj o2
  | Mediatrice(p1,o1,o2,p2) -> Printf.fprintf ch "Mediatrice(%a,%a,%a,%a)" print_pt p1 print_obj o1 print_obj o2 print_pt p2
  | LeftParabola(p1,o1,o2) -> Printf.fprintf ch "LeftParabola(%a,%a,%a)" print_pt p1 print_obj o1 print_obj o2
  | RightParabola(o1,o2,p2) -> Printf.fprintf ch "RightParabola(%a,%a,%a)" print_obj o1 print_obj o2 print_pt p2
  | LeftBack(p1,o1,o2) -> Printf.fprintf ch "LeftBack(%a,%a,%a)" print_pt p1 print_obj o1 print_obj o2
  | RightBack(o1,o2,p2) -> Printf.fprintf ch "RightBack(%a,%a,%a)" print_obj o1 print_obj o2 print_pt p2

let oleft s = match s with
  | Bissectrice(o1,o2)
  | Mediatrice(_,o1,o2,_)
  | LeftParabola(_,o1,o2)
  | RightParabola(o1,o2,_)
  | LeftBack(_,o1,o2)
  | RightBack(o1,o2,_) -> o1

let oright s = match s with
  | Bissectrice(o1,o2)
  | Mediatrice(_,o1,o2,_)
  | LeftParabola(_,o1,o2)
  | RightParabola(o1,o2,_)
  | LeftBack(_,o1,o2)
  | RightBack(o1,o2,_) -> o2

let advance_parabola foyer directrice w =
  let (q,v') = carrier directrice in
  let v = normalize (rotate_90 v') in
  let u = foyer -- q in
  let v = if dot_prod u v > 0.0 then v else opp v in 
  let w = normalize w in
  let t = dot_prod u v /. (1. -. dot_prod w v) in
  if t < 0.0 then raise Extrem; (* CHECK / FIXME ??? *)
  let m = comblin 1.0 foyer t w in
  let s = dot_prod v' (m -- q) /. norm2 v' in
  if !debug_mediatrice then
    Printf.fprintf stderr "advance parabola: foyer = %a directrice = %a w = %a\n  v = %a u = %a t = %f m = %a s = %f\n"
      print_pt foyer print_obj directrice print_pt w
      print_pt v print_pt u t print_pt m s;
  in_range' directrice s;
  m, comblin 1.0 q s v'

let find_parabola foyer directrice p =
  let (_,v') = carrier directrice in
  let v = normalize (rotate_90 v') in
  let u = foyer -- p in
  let v = if dot_prod u v > 0.0 then v else opp v in 
  let t = norm2 u /. (2. *. dot_prod u v) in
  if !debug_mediatrice then
    Printf.fprintf stderr "find parabola: foyer = %a directrice = %a p = %a\n  v = %a u = %a t = %f\n"
      print_pt foyer print_obj directrice print_pt p
      print_pt v print_pt u t ;
  comblin 1.0 p t v
  

let ftrue _ = true 

let fand f g x = f x && g x


let frange o p =
  let q,v = carrier o in
  let s = dot_prod v (p -- q) /. norm2 v in
   if !debug_mediatrice then
    Printf.fprintf stderr "frange: s = %f\n" s; 
  in_range o s

let fortho o i = 
  try 
    let p,v = 
      match o with 
      | Segment(p,q) -> p, q -- p
      | HalfLine(p,v,true) -> p, v
      | _ -> raise Not_found
    in
    let s = dot_prod (i -- p) v /. (norm (i -- p) *. norm v) in
    if !debug_mediatrice then
      Printf.fprintf stderr "fortho: s = %f\n" s; 
    s <= 0.0
  with
    Not_found -> true

(* find on line d, a point equidistant from p and o.
   raise Not_found if there is no such point

   Assume at most one solution on o (CHECK)

   return the point of d and its projection on o
 *)

let eqdist d p o i1 i2 extras =
  let q, v = carrier d in
  let v = normalize v in
  let r, w0 = carrier o in
  let w = normalize (rotate_90 w0) in
  let vw = dot_prod v w in
  let qrw = dot_prod (q --r) w in
  let a = 1. -. vw *. vw in
  let b' = dot_prod (q -- p) v -.  qrw *. vw in
  let c = norm2 (q -- p) -. qrw *. qrw in
  let delta' = b' *. b' -. a*.c in
  let s, n = 
    if abs_float a < 1e-8 then
      let r1 = -. c /. (2. *. b') in
      let n = comblin 1.0 q r1 v in
      let s = dot_prod (n -- r) w0  /. norm2 w0 in 
      if !debug_mediatrice then
        Printf.fprintf stderr "eqdist:  r = %f n = %a s = %f one root a = 0\n"
          r1 print_pt n s;
      if in_range o s then s, n
      else raise Not_found
    else
      let delta' = if delta' < 0.0 && delta' > -1e-8 then 0.0 else delta' in
      let sq = sqrt delta' in
      let r1 = (-. b' -. sq) /. a in
      let r2 = (-. b' +. sq) /. a in
      if !debug_mediatrice then
        Printf.fprintf stderr "eqdist: d = %a p = %a o = %a\n  delta = %e r1 = %f r2 = %f\n"
          print_obj d print_pt p print_obj o delta' r1 r2;
      if delta' < 0.0 then raise Not_found;
      let n1 = comblin 1.0 q r1 v in
      let n2 = comblin 1.0 q r2 v in
      let s1 = dot_prod (n1 -- r) w0  /. norm2 w0 in 
      let s2 = dot_prod (n2 -- r) w0 /. norm2 w0 in 
      let x1 = dot_prod (n1 -- i1) (i2 -- i1) /. norm2 (i1 -- i2) in
      let x2 = dot_prod (n2 -- i1) (i2 -- i1) /. norm2 (i1 -- i2) in
      if !debug_mediatrice then
        Printf.fprintf stderr "  n1 = %a n2 = %a \n  s1 = %f s2 = %f\n"
          print_pt n1 print_pt n2 s1 s2;
      if !debug_mediatrice then
        Printf.fprintf stderr "  x1 = %f x2 = %f\n" x1 x2;

      match 
        in_range o s1 && x1 > -1e-8 && x1 <= 1. +. 1e-8 && extras n1,
        in_range o s2 && x2 > -1e-8 && x2 <= 1. +. 1e-8 && extras n2
      with
        true, false -> s1, n1
      | false, true -> s2, n2
      | true, true when s1 < s2 -> s2, n2
      | true, true -> s1, n1
      | _ -> raise Not_found
  in
  n, comblin 1.0 r s w0
  
let ortho p1 v1 p2 v2 i = 
  if near p1 p2 then p1
  else
    try inter (Line(p1,v1)) (Line(p2,v2)) with Not_found -> assert false

let mediatrice_profile (dsup,dinf) profile1 profile2 =

  let profile2 = List.rev profile2 in
  let dinf' = opp dsup in
  let dsup' = opp dinf in

  let l = HalfLine(List.hd profile1,dinf,true) in
  let r = HalfLine(List.hd profile2,dinf',true) in
  let init = Bissectrice(l,r) in


  let cons2 p1 q1 i p2 q2 acc =
    if near p1 p2 && near q1 q2 then
      (p1, i, p2)::acc
    else
      let x = ortho p1 dinf q1 dsup i in
      let y = ortho p2 dinf' q2 dsup' i in
      (p1, i, p2)::(x, i, y)::(q1, i, q2)::acc
  in

  let back o2 r2 q2 c2 =
    let _, v = carrier o2 in
    if near r2 q2 || abs_float (det22 v (r2 -- q2) /. (norm v *. norm (r2 -- q2))) < 1e-6 then (
      if !debug_mediatrice then
        Printf.fprintf stderr "Removed created point det = %f\n" (abs_float (det22 v (r2 -- q2)) /. (norm v *. norm (r2 -- q2)));
      cut' q2 o2, c2)
    else
      Segment(r2,q2), r2::c2
  in

  let forward on1 oq1 c1 c1' =
    if near on1 oq1 then 
      next dsup oq1 c1', c1', oq1
    else 
      Segment(oq1,on1), c1, on1
  in


  let profile1 = List.fold_right push profile1 [] in
  let profile2 = List.fold_right push profile2 [] in

  let rec fn acc state c1 c2 =
    if !debug_mediatrice then 
      Printf.fprintf stderr "state = %a\n c1 = %a\n c2 = %a\n acc = %a\n\n"
        print_st state print_pt' c1 print_pt' c2 (fun ch -> List.iter (fun (x,i,y) ->
          Printf.fprintf ch "(%a,%a,%a) " print_pt x print_pt i print_pt y)) acc;

      (try ignore (inter (oleft state) (oright state)); 
           if !debug_mediatrice then Printf.fprintf stderr "Finished(Exit)\n";
           raise Exit 
       with Not_found -> ());

    match state with
    | Bissectrice(o1,o2) -> (
      let b = bissectrice o1 o2 in
      let _, v' = carrier o1 in
      let v = rotate_90 v' in
      let _, w' = carrier o2 in
      let w = rotate_90 w' in
      if !debug_mediatrice then 
        Printf.fprintf stderr "  b = %a\n" print_obj b;
      match c1, c2 with 
      | [], [] -> 
          List.rev acc

      | (p1::c1'), [] -> 

        let i = try inter b (Line(p1,v)) with Not_found -> assert false in
        let p2 = try inter o2 (Line(i,w)) with Not_found -> assert false in
        fn ((p1,i,p2)::acc) (LeftParabola(p1,next dsup p1 c1',cut' p2 o2)) c1' c2

      |  [], (p2::c2') -> 

        let i = try inter b (Line(p2,w)) with Not_found -> assert false in
        let p1 = try inter o1 (Line(i,v)) with Not_found -> assert false in
        fn ((p1,i,p2)::acc) (RightParabola(cut' p1 o1, next dsup' p2 c2', p2)) c1 c2'

      | (p1::c1'), (p2::c2') -> 

        let _, vb = carrier b in
        let x = dot_prod (p1 -- p2) vb in
        if abs_float x < 1e-8 then
          let i = try inter b (Line(p2,w)) with Not_found -> assert false in
          fn ((p1,i,p2)::acc) (Mediatrice(p1, next dsup p1 c1', next dsup' p2 c2', p2)) c1' c2'
        else if x > 0.0 then
          let i = try inter b (Line(p1,v)) with Not_found -> assert false in
          let n2 = try inter o2 (Line(i,w)) with Not_found -> assert false in
          fn ((p1,i,n2)::acc) (LeftParabola(p1,next dsup p1 c1',cut' n2 o2)) c1' c2
        else
          let i = try inter b (Line(p2,w)) with Not_found -> assert false in
          let n1 = try inter o1 (Line(i,v)) with Not_found -> assert false in
          fn ((n1,i,p2)::acc) (RightParabola(cut' n1 o1, next dsup' p2 c2', p2)) c1 c2')

     | LeftParabola(p1,o1,o2) -> (
       let _, w' = carrier o1 in
       let w = rotate_90 w' in
       try 
         let i, n2 = advance_parabola p1 o2 w in
         fn ((p1,i,n2)::acc) (Bissectrice(o1,cut' n2 o2)) c1 c2
       with
       | Not_found -> fn acc (LeftBack(p1,o1,o2)) c1 c2
       | Extrem -> 
           match c2 with [] -> assert false | p2::c2' ->
             let i = find_parabola p1 o2 p2 in
             fn ((p1,i,p2)::acc) (Mediatrice(p1,o1,next dsup' p2 c2',p2)) c1 c2')

     | RightParabola(o1,o2,p2) -> (
       let _, w' = carrier o2 in
       let w = rotate_270 w' in
       try
         let i, n1 = advance_parabola p2 o1 w in
         fn ((n1,i,p2)::acc) (Bissectrice(cut' n1 o1,o2)) c1 c2
       with
       | Not_found -> fn acc (RightBack(o1,o2,p2)) c1 c2
       | Extrem -> 
           match c1 with [] -> assert false | p1::c1' ->
             let i = find_parabola p2 o1 p1 in
             fn ((p1,i,p2)::acc) (Mediatrice(p1,next dsup p1 c1', o2, p2)) c1' c2)

     | Mediatrice(p1,o1,o2,p2) ->
       let m = mediatrice p1 p2 in
       let _, v = carrier m in
       let v = if det22 (p2 -- p1) v < 0.0 then opp v else v in
       let _,v1 = carrier o1 in
       let v1 = normalize v1 in
       let v1' = rotate_90 v1 in
       let _,v2 = carrier o2 in
       let v2 = normalize v2 in
       let v2' = rotate_270 v2 in
       let x1 = dot_prod v1' v in
       let x2 = dot_prod v2' v in
       let x = x2 -. x1 in
       let u1,i,u2 = match acc with
           (r1,i,r2)::(q1,_,q2)::_ -> 
             let s = 1. /. norm (r1 -- r2) in
               sprod s (q1 -- r1), i, sprod s (q2 -- r2)
         | [_,i,_] -> dinf,i,dinf'
         | _ -> assert false
       in
       let b1 = abs_float (det22 u1 v1) < 1e-8 in
       let b2 = abs_float (det22 u2 v2) < 1e-8 in
       let b1' = det22 (p2 -- p1) v1 < 0.0 in
       let b2' = det22 (p2 -- p1) v2 < 0.0 in

       if !debug_mediatrice then Printf.fprintf stderr "x1 = %f, x2 = %f, b1 = %b, b2 = %b\n" x1 x2 b1 b2;

       (if abs_float x < 1e-7 then
         try let i1 = try inter m (Line(p1,v1')) with Not_found -> assert false in
           if b1' && b2' &&  dot_prod (i1 -- i) v /. norm (p1 -- p2) > -1e-6 then
             fn ((p1,i1,p2)::acc) (Bissectrice(o1,o2)) c1 c2
           else raise Not_found
         with Not_found ->
           if x > 0.0 then 
             fn acc (LeftBack(p1,o1,o2)) c1 c2
           else
             fn acc (RightBack(o1,o2,p2)) c1 c2
       else if x > 0.0 then
         try let i1 = inter m (Line(p1,v1')) in
             if !debug_mediatrice then Printf.fprintf stderr "i1 = %a v = %a\n" print_pt i1 print_pt v;
             if b1' && (b1 ||  dot_prod (i1 -- i) v /. norm (p1 -- p2) > -1e-6) then
               fn ((p1,i1,p2)::acc) (RightParabola(o1,o2,p2)) c1 c2
             else raise Not_found
         with Not_found -> 
           fn acc (LeftBack(p1,o1,o2)) c1 c2
       else
         try let i2 = inter m (Line(p2,v2')) in
             if !debug_mediatrice then Printf.fprintf stderr "i' = %a v = %a\n" print_pt i2 print_pt v;
             if b2' && (b2 || dot_prod (i2 -- i) v /. norm (p1 -- p2) > -1e-6) then 
               fn ((p1,i2,p2)::acc) (LeftParabola(p1,o1,o2)) c1 c2
             else raise Not_found
         with Not_found -> 
             fn acc (RightBack(o1,o2,p2)) c1 c2)
     
     | LeftBack(p1,o1,o2) -> (
       match c1, acc with
         [], _ | _, [] | _, [_] -> assert false
       | (oq1::c1'), ((r1,i1,r2)::((q1,i2,q2)::_ as acc')) ->

         (try let i = project i1 o1 in
              if norm2 (i1 -- r1) +. 1e-5 < norm2 (i1 -- i) then (
                Printf.fprintf stderr "Back too far (i = %a) %e\n" print_pt i (norm (i1 -- r1) -. norm (i1 -- i)); assert false)
          with Not_found -> ());

         try 
           if i1 == i2 then raise Not_found;
           if near r1 q1 then
             if near r2 q2 then
               let m = mediatrice r1 r2 in
               try
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Mediatrice inter\n";
                 let i, on1 = eqdist m r2 o1 i1 i2 ftrue in
                 if !debug_mediatrice then Printf.fprintf stderr "Mediatrice inter\n";
                   let o1', c1', p1' = forward on1 oq1 c1 c1' in
                 fn (cons2 on1 r1 i r2 r2 acc') (Mediatrice(p1',o1',o2,r2)) c1' c2
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Mediatrice extremity\n";
                 let i = inter (Segment(i1,i2)) (mediatrice oq1 r2) in
                 if not (fortho o1 i) then raise Not_found;
                 if !debug_mediatrice then Printf.fprintf stderr "Mediatrice extremity\n";
                 fn (cons2 oq1 r1 i r2 r2 acc') (Mediatrice(oq1,next dsup oq1 c1',o2,r2)) c1' c2
             else
               try
                 let b = bissectrice o1 (Segment(r2,q2)) in
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola opp inter\n";
                 let i, on1 = eqdist b r1 o1 i1 i2 (frange (Segment(r2,q2))) in
                 let n2 = project i (Segment(r2,q2)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola opp inter\n";
                 let o2', c2' = back o2 r2 n2 c2 in
                   let o1', c1', p1' = forward on1 oq1 c1 c1' in
                 fn (cons2 on1 r1 i n2 n2 acc') (Mediatrice(p1',o1',o2',n2)) c1' c2'
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola opp extremity\n";
                 let m = mediatrice oq1 r1 in
                 let i, n2 = eqdist m oq1 (Segment(r2,q2)) i1 i2 (fortho o1) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola opp extremity\n";
                 let o2', c2' = back o2 r2 n2 c2 in
                 fn (cons2 oq1 r1 i n2 n2 acc') (Mediatrice(oq1,next dsup oq1 c1',o2',n2)) c1' c2'
           else
             let b = bissectrice o1 (Segment(q1,r1)) in
             if near r2 q2 then
               try
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola inter\n";
                 let i, on1 = eqdist b r2 o1 i1 i2 (frange (Segment(r1,q1))) in
                 let n1 = project i (Segment(r1,q1)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola inter\n";
                   let o1', c1', p1' = forward on1 oq1 c1 c1' in
                 fn (cons2 on1 n1 i r2 r2 acc') (Mediatrice(p1',o1',o2,r2)) c1' c2
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola extremity\n";
                 let m = mediatrice oq1 r2 in
                 let i, n1 = eqdist m oq1 (Segment(r1,q1)) i1 i2 (fortho o1) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola extremity\n";
                 fn (cons2 oq1 n1 i r2 r2 acc') (Mediatrice(oq1,next dsup oq1 c1',o2,r2)) c1' c2
             else
               let m = bissectrice (Segment(r1,q1)) (Segment(r2,q2)) in
               try
                 if !debug_mediatrice then Printf.fprintf stderr "Try Bissectrice inter b = %a\n" print_obj b;
                 let i = inter b m in
                 if !debug_mediatrice then Printf.fprintf stderr "TRY(2) Bissectrice inter i = %a\n"  print_pt i;
                 let on1 = project i o1 in
                 if !debug_mediatrice then Printf.fprintf stderr "TRY(3) Bissectrice inter on1 = %a oq1 = %a\n" print_pt on1 print_pt oq1;
                 let n1 = project i (Segment(r1,q1)) in
                 let n2 = project i (Segment(r2,q2)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Bissectrice inter\n";
                 let o2', c2' = back o2 r2 n2 c2 in
                   let o1', c1', p1' = forward on1 oq1 c1 c1' in
                 fn (cons2 on1 n1 i n2 n2 acc') (Mediatrice(p1',o1',o2',n2)) c1' c2'
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "Try Bissectrice extremity\n";
                 let i, n2 = eqdist m oq1 (Segment(r2,q2)) i1 i2 (fand (fortho o1) (frange (Segment(r1,q1)))) in
                 if !debug_mediatrice then Printf.fprintf stderr "Try(2) Bissectrice extremity i = %a\n"  print_pt i;
                 let n1 = project i (Segment(r1,q1)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Bissectrice extremity\n";
                 let o2', c2' = back o2 r2 n2 c2 in
                 fn (cons2 oq1 n1 i n2 n2 acc') (Mediatrice(oq1,next dsup oq1 c1',o2',n2)) c1' c2'
                 
         with Not_found ->
           let o2', c2' = back o2 r2 q2 c2 in
           fn acc' (LeftBack(p1,o1,o2')) c1 c2')

     | RightBack(o1,o2,p2) -> (
       match c2, acc with
         [], _ | _, [] | _, [_] -> assert false
       | (oq2::c2'), ((r1,i1,r2)::((q1,i2,q2)::_ as acc')) ->

         (try let i = project i1 o2 in
              if norm (i1 -- r2) +. 1e-7 < norm (i1 -- i) then (
                Printf.fprintf stderr "Back too far (i = %a) %e\n" print_pt i (norm (i1 -- r2) -. norm (i1 -- i)); assert false)
          with Not_found -> ());

         try 
           if i1 == i2 then raise Not_found;
           if near r2 q2 then
             if near r1 q1 then
               let m = mediatrice r1 r2 in
               try
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Mediatrice inter\n";
                 let i, on2 = eqdist m r1 o2 i1 i2 ftrue in 
                 if !debug_mediatrice then Printf.fprintf stderr "Mediatrice inter\n";
                   let o2', c2', p2' = forward on2 oq2 c2 c2' in
                 fn (cons2 r1 r1 i on2 r2 acc') (Mediatrice(r1,o1,o2',p2')) c1 c2'
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Mediatrice extremity\n";
                 let i = inter (Segment(i1,i2)) (mediatrice oq2 r1) in
                 if not (fortho o2 i) then raise Not_found;
                 if !debug_mediatrice then Printf.fprintf stderr "Mediatrice extremity\n";
                 fn (cons2 r1 r1 i oq2 r2 acc') (Mediatrice(r1,o1,next dsup' oq2 c2',oq2)) c1 c2'
             else
               try
                 let b = bissectrice o2 (Segment(r1,q1)) in
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola opp inter\n";
                 let i, on2 = eqdist b r2 o2 i1 i2 (frange (Segment(r1,q1))) in
                 let n1 = project i (Segment(r1,q1)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola opp inter\n";
                   let o2', c2', p2' = forward on2 oq2 c2 c2' in
                 let o1', c1' = back o1 r1 n1 c1 in
                 fn (cons2 n1 n1 i on2 r2 acc') (Mediatrice(n1,o1',o2',p2')) c1' c2'
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola opp extremity\n";
                 let m = mediatrice oq2 r2 in
                 let i, n1 = eqdist m oq2 (Segment(r1,q1)) i1 i2 (fortho o2) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola opp extremity\n";
                 let o1', c1' = back o1 r1 n1 c1 in
                 fn (cons2 n1 n1 i oq2 r2 acc') (Mediatrice(n1,o1',next dsup' oq2 c2',oq2)) c1' c2'
           else
             let b = bissectrice o2 (Segment(q2,r2)) in
             if near r1 q1 then
               try
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola inter\n";
                 let i, on2 = eqdist b r1 o2 i1 i2 (frange (Segment(r2,q2))) in
                 let n2 = project i (Segment(r2,q2)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola inter\n";
                   let o2', c2', p2' = forward on2 oq2 c2 c2' in
                 fn (cons2 r1 r1 i on2 n2 acc') (Mediatrice(r1,o1,o2',p2')) c1 c2'
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Parabola extremity\n";
                 let m = mediatrice oq2 r1 in
                 let i, n2 = eqdist m oq2 (Segment(r2,q2)) i1 i2 (fortho o2) in
                 if !debug_mediatrice then Printf.fprintf stderr "Parabola extremity\n";
                 fn (cons2 r1 r1 i oq2 n2 acc') (Mediatrice(r1,o1,next dsup' oq2 c2',oq2)) c1 c2'
             else
               let m = bissectrice (Segment(r1,q1)) (Segment(r2,q2)) in
               try
                 if !debug_mediatrice then Printf.fprintf stderr "TRY Bissectrice inter b = %a\n" print_obj b;
                 let i = inter b m in
                 if !debug_mediatrice then Printf.fprintf stderr "TRY(2) Bissectrice inter i = %a\n" print_pt i;
                 let on2 = try project i o2 with Not_found -> raise Not_found in
                 if !debug_mediatrice then Printf.fprintf stderr "TRY(3) Bissectrice inter on2 = %a oq2 = %a\n" print_pt on2 print_pt oq2;
                 let n1 = project i (Segment(r1,q1)) in
                 let n2 = project i (Segment(r2,q2)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Bissectrice inter\n";
                  let o2', c2', p2' = forward on2 oq2 c2 c2' in
                 let o1', c1' = back o1 r1 n1 c1 in
                 fn (cons2 n1 n1 i on2 n2 acc') (Mediatrice(n1,o1',o2',p2')) c1' c2'
               with Not_found ->
                 if !debug_mediatrice then Printf.fprintf stderr "Try Bissectrice extremity\n";
                 let i, n1 = eqdist m oq2 (Segment(r1,q1)) i1 i2 (fand (fortho o2) (frange (Segment(r2,q2)))) in
                 if !debug_mediatrice then Printf.fprintf stderr "Try(2) Bissectrice extremity i = %a\n"  print_pt i;
                 let n2 = project i (Segment(r2,q2)) in
                 if !debug_mediatrice then Printf.fprintf stderr "Bissectrice extremity\n";
                 let o1', c1' = back o1 r1 n1 c1 in
                 fn (cons2 n1 n1 i oq2 n2 acc') (Mediatrice(n1,o1',next dsup' oq2 c2',oq2)) c1' c2'
                 
                 
         with Not_found ->
           let o1', c1' = back o1 r1 q1 c1 in
           fn acc' (RightBack(o1',o2,p2)) c1' c2)

 
           
  in
  try
    let r = fn [] init profile1 profile2 in
    if !debug_mediatrice then Printf.fprintf stderr "Finished\n"; 
    r
  with Assert_failure(_) as e ->
    Printf.fprintf stderr "left: ";
    List.iter (fun p -> print_pt16 stderr p) profile1;
    Printf.fprintf stderr "\n  right: ";
    List.iter (fun p -> print_pt16 stderr p) profile2;
    Printf.fprintf stderr "\n";
    raise e

            
(*
let testb0 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);] [(1.0,0.0)]
let testb1 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.0,1.0)]
let testb1' = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(0.0,1.0)] [(1.0,2.0);(1.0,0.0)]




let testb2 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.25,1.25);(1.25,0.75)]
let testb3 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.);(3.,2.);(2.,1.)]

let testb6 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) (List.rev [(-.1.5,2.0);(-.2.,1.);(-.1.,0.)]) [(-.2.0,4.0)] 
let testb7 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) (List.rev [(-.1.5,1.5);(-.2.,1.);(-.1.,0.)]) [(-.2.0,4.0)] 

let testb4 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.5);(3.,2.5);(2.,1.5)]
let testb5 = mediatrice_profile ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.25);(3.,2.25);(2.,1.25)]
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

let area o1 p1 o2 p2 =
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
  let s = area o1 p1 o2 p2 in
  let a = norm (o2 -- o1) in
  let b = norm (p2 -- p1) in
  let a,b,o1,o2,p1,p2 =
    if a <= b then a,b,o1,o2,p1,p2
    else b,a,p2,p1,o2,o1
  in
  if !debug_addpro then
    Printf.fprintf stderr "addpq o1 = %a, o2 = %a, p1 = %a, p2 = %a\n  s = %f a = %f b = %f\n"
      print_pt o1 print_pt o2 print_pt p1 print_pt p2
      s a b;
  if norm2 (o1 -- p1) < 1e-8 || norm2 (o2 -- p2) < 1e-8 then
    if b -. a < 1e-8 then l else (
      let h = s /. (b -. a) in
      if !debug_addpro then Printf.fprintf stderr "  triangle : h = %f\n" h;
      add_dprofile a h 0.0 (add_dprofile b (-.h) 0.0 l))
  else if b -. a < 1e-6 then begin (
    let a, b = swap_min a b in
    let h = s /. (b -. a) in
      if !debug_addpro then Printf.fprintf stderr "  rectangle : h = %f\n" h;
    add_dprofile a h 0.0 (add_dprofile b (-.h) 0.0 l))
  end else begin
(*    let d2 = det22 (p2 -- p1) (o2 -- o1) in
      if not (abs_float d2 < 1e-5) then (
      Printf.fprintf stderr "bad trapeze: d2 = %f p1 = %a p2 = %a o1 = %a o2 = %a\n" d2
        print_pt p1 print_pt p2 print_pt o1 print_pt o2; 
      assert false); *)
    let h = norm (middle o2 o1 -- middle p2 p1) in
(*    if not (dot_prod h (o2 -- o1) < 1e-4) then (
      Printf.fprintf stderr "  bad trapeze(2): dp = %f %a %a %a %a\n" (dot_prod h (o2 -- o1)) print_pt o1 print_pt p1 print_pt o2 print_pt p2; 
      assert false);    *)
    let p = h /. (b -. a) in
    if !debug_addpro then Printf.fprintf stderr "  trapeze : h = %f p = %f\n" h p;
    add_dprofile a 0. p (add_dprofile b 0. (-.p) l) 
  end
  

let add_qprofile (m,l) o1 p1 o2 p2 =
  (*Printf.fprintf stderr "addq o1 = %a, o2 = %a, p1 = %a, p2 = %a\n"
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
       m, add_dprofile d 0. 0.5 l
    | (o1,_,o2)::((p1,_,p2)::_ as l') ->
        fn (add_qprofile acc o1 p1 o2 p2) l'
    | [] -> assert false
  in
  let o1,_,o2 = List.hd l in
  let d = norm (o2 -- o1) in
  fn (d, add_dprofile d 0. 0.5 []) l

let find_distance beta m l =
(*  let area = m *. m *. beta in*)

  let rec fn curd curh curarea curp l =
    let a = curp /. 2. -. beta in
    let b = curh in
    let c = curarea (* -. area*) -. curh *. curd -. curd *. curd *. curp /. 2. in
    let delta = b*.b -. 4. *. a *. c in
    let r1, r2 = 
      if b > 0.0 then
        -. 2.0 *. c /. (b +. sqrt(delta)), b +. sqrt(delta) /. (-. 2. *. a)
      else
        -. 2.0 *. c /. (b -. sqrt(delta)), b -. sqrt(delta) /. (-. 2. *. a)
    in
    let dd = 
      if r1 < curd then r2 else if r2 < curd then r1 else min r1 r2
    in
  if !debug_addpro then   Printf.fprintf stderr "a = %f, b = %f, c = %f, delta = %f, r1 = %f, r2 = %f, dd = %f\n" 
      a b c delta r1 r2 dd;
    match l with
      (a,dh,p)::l ->
        if dd > 0.0 && delta >= 0.0 && dd >= curd && dd <= a then dd else
          let curarea = curarea +. (a  -. curd) *. curh  +. (a*.a  -. curd*.curd) *. curp /. 2. in 
          fn a (curh +. dh) curarea (p +. curp) l
    | [] -> dd
  in
  fn 0.0 0.0 0.0 0.0 l


let distance beta (dsup,dinf) profile1 profile2 =
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
        let b = mediatrice_profile (dsup,dinf) profile1 profile2 in
        let m,l = add_allprofile b in
      if !debug then (Printf.fprintf stderr "m = %f\n" m;
        List.iter (fun (a,h,p) -> Printf.fprintf stderr "a = %f h = %f, p = %f -- " a h p) l;
        Printf.fprintf stderr "\n"); 

        if m <= 0.0 then 0.0 else find_distance beta m l
      with
        Exit | Assert_failure _ -> 0.0
    end
  in
  if !debug then Printf.fprintf stderr "  ==> %f\n" r;
  r

(*
let testc1 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.0,1.0)]
let testc2 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(0.0,0.0);(0.0,2.0)] [(1.25,1.25);(1.25,0.75)]

let _ = debug_mediatrice := true
let testc3 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.);(3.,2.);(2.,1.)]
let testc4 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.5);(3.,2.5);(2.,1.5)]
let testc5 = distance 0.5 ((-1.0,1.0), (-1.0,-1.0)) [(1.0,1.0);(0.,2.);(1.,3.)] [(2.,3.25);(3.,2.25);(2.,1.25)]
*)

module Profile_data = struct
  type t = (float array * float array) list * ((float * float) * (float * float)) * float
  let compare = compare
end

module Profile_cache = Map.Make(Profile_data)

module Distance_data = struct
  type t = (float * float) list * (float * float) list * ((float * float) * (float * float)) * float
  let compare = compare
end

module Distance_cache = Map.Make(Distance_data)

type distance_cache = {
    mutable generation : int;
    mutable profile_cache : ((float * float) list * int ref) Profile_cache.t;
    mutable distance_cache : (float * int ref) Distance_cache.t;
}

let distance_cache = {
  generation = 0;
  profile_cache = Profile_cache.empty;
  distance_cache = Distance_cache.empty;
}

let read_cache filename =
  try 
    let ch = open_in_bin filename in
    let cache = input_value ch in
    close_in ch;
    distance_cache.generation <- cache.generation + 1;
    distance_cache.profile_cache <- cache.profile_cache;
    distance_cache.distance_cache <- cache.distance_cache;
  with
    Sys_error _ | End_of_file ->
      distance_cache.generation <- 0;
      distance_cache.profile_cache <- Profile_cache.empty;
      distance_cache.distance_cache <-Distance_cache.empty

let write_cache filename = 
  let g = distance_cache.generation in
  let purge (_, n) = g - !n > 10 in
  let cache = {
    generation = g;
    profile_cache =
      Profile_cache.fold (fun k t acc -> 
        if purge t then Profile_cache.remove k acc else acc)
        distance_cache.profile_cache
        distance_cache.profile_cache;
    distance_cache = 
      Distance_cache.fold (fun k t acc -> 
        if purge t then Distance_cache.remove k acc else acc)
        distance_cache.distance_cache
        distance_cache.distance_cache;
  }
  in
  try
    let ch = open_out_bin filename in
    output_value ch cache;
    close_out ch
  with
    Sys_error _ -> ()

let bezier_profile dirs epsilon curves0 =
  try 
    let (r,g) =
      Profile_cache.find (curves0, dirs, epsilon) distance_cache.profile_cache
    in
    g := distance_cache.generation;
    r
  with Not_found ->
    let r = bezier_profile dirs epsilon curves0 in
    distance_cache.profile_cache <- 
      Profile_cache.add (curves0, dirs, epsilon)
      (r, ref distance_cache.generation) distance_cache.profile_cache;
    r

let distance beta dirs profile1 profile2 =
  try 
    let (r,g) =
      Distance_cache.find (profile1, profile2, dirs, beta) distance_cache.distance_cache
    in
    g := distance_cache.generation;
    r
  with Not_found ->
    let r = distance beta dirs profile1 profile2 in
    distance_cache.distance_cache <- 
      Distance_cache.add (profile1, profile2, dirs, beta)
      (r, ref distance_cache.generation) distance_cache.distance_cache;
    r

let translate_profile p dx =
  if dx <> infinity && dx <> -. infinity then 
    List.map (fun (x,y) -> (x +. dx, y)) p
  else []
