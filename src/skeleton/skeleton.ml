open Pervasives

(* Type of a polygon and examples *)
type polygon = (float * float) list

let square   : polygon = [(1.,1.); (1.,2.); (2.,2.); (2.,1.)]
let triangle : polygon = [(1.,1.); (2.,1.); (1.5,2.)]

(* Vector type and basic operations *)
type point  = (float * float)
type vector = (float * float)
type scalar = float 

let vect : point -> point -> vector = fun (xa, ya) (xb, yb) ->
  let x = xb -. xa in
  let y = yb -. ya in
  (x, y)

let translate : point -> vector -> point =
  fun (x,y) (vx,vy) -> (x +. vx, y +. vy)

let norm : vector -> scalar =
  fun (x, y) -> sqrt (x *. x +. y *. y)

let mult : scalar -> vector -> vector =
  fun a (x, y) -> (a *. x, a *. y)

let sum : vector -> vector -> vector =
  fun (xa, ya) (xb, yb) -> (xa +. xb, ya +. yb)

let normalize : vector -> vector =
  fun v -> mult (1. /. (norm v)) v

let scalar_product : vector -> vector -> scalar =
  fun (xa, ya) (xb, yb) -> xa *. xb +. ya *. yb

(* Solve system: ax + by = e, cx + dy = f using Cramer's rule *)
let solve a b e c d f =
  let det = a *. d -. b *. c in
  let x = (e *. d -. b *. f) /. det in
  let y = (a *. f -. e *. c) /. det in
  (x, y)

(* Given points A B C, returns a normal vector bissecting angle ABC *)
let bissector : point -> point -> point -> vector =
  fun a b c ->
    let ba = normalize (vect b a) in
    let bc = normalize (vect b c) in
    let p = scalar_product ba bc in
    let (ba', bc') =
      if p < 0. then
        let (xba, yba) = ba in
        let (xbc, ybc) = bc in
        let ba' = (yba, -.xba) in
        let bc' = (-.ybc, xbc) in
        (ba', bc')
      else
        (ba, bc)
    in
    normalize (sum ba' bc')

(* Compute intersection of two lines given by a point and a vector.
 * We need to find ta and tb such that
 * (xa, ya) + ta * (xva, yva) = (xb, yb) + tb * (xvb, yvb)
 * <=>
 * ta * (xva, yva) - tb * (xvb, yvb) = (xb - xa, yb - ya)
 * <=>
 * xva * ta - xvb * tb = xb - xa
 * yva * ta - yvb * tb = yb - ya
 *)
let intersection : point -> vector -> point -> vector -> point =
  fun (xa,ya) (xva,yva) (xb,yb) (xvb,yvb) ->
    let (ta,tb) = solve xva (-.xvb) (xb -. xa) yva (-.yvb) (yb -.ya) in
    let x = xa +. ta *. xva in
    let y = ya +. ta *. yva in
    (x, y)

(* Orthogonal distance between line AB and point C *)
let dist_point_line : point * point -> point -> scalar =
  fun (a,b) c ->
    let vab = vect a b in
    let vac = vect a c in
    let nab = norm vab in
    let nac = norm vac in
    let abac = scalar_product vab vac in
    let f = abac /. nab in (* comme fuck *)
    let s = nac *. nac -. f *. f in
    sqrt s

(* Skeleton function *)
type edge = point * point

let epsilon = 0.0001

let equal : point -> point -> bool =
  fun p1 p2 ->
    let v = vect p1 p2 in
    let n = norm v in
    n <= epsilon

let dist : point -> point -> scalar =
  fun p1 p2 ->
    let v = vect p1 p2 in
    norm v

let eq_scalar : scalar -> scalar -> bool =
  fun s1 s2 ->
    abs_float (s1 -. s2) <= epsilon

let last : 'a list -> 'a =
  fun l -> List.hd (List.rev l)

(* Input points are given counter-clockwise *)
let skeleton : polygon -> edge list =
  let rec skel es = function
    | []             -> es
    | p1 :: []       -> es
    | p1 :: p2 :: [] -> (p1, p2) :: es
    | (p1 :: p2 :: p3 :: _ as ps) ->
        begin
          let ps = ps @ [p1;p2;p3] in
          let rec sp4 acc = function
            | a :: (b :: c :: d :: _ as r) -> sp4 ((a,b,c,d)::acc) r
            | _                            -> List.rev acc
          in
          let data = sp4 [] ps in

          (* data every group of four points in counterclockwise order *)
          let f (a,b,c,d) =
            let v1 = bissector a b c in
            let v2 = bissector b c d in
            let m = intersection b v1 c v2 in
            let h = dist_point_line (b,c) m in
            (h, (b,m,c))
          in
          let data = List.map f data in
          let f acc (d,_) = min acc d in
          let h = List.fold_left f max_float data in

          (* h contains minimum distance *)
          let f (pts, edgs) (d, (b,m,c)) =
            if eq_scalar d h then
              (m :: pts, (c,m) :: edgs)
            else
              let f a b m =
                let am = vect a m in
                let l = h *. (norm am) /. d in
                translate a (mult l (normalize am))
              in
              let m' = f c b m in
              ( m' :: pts, (c, m') :: edgs)
          in
          let (pts, edgs) = List.fold_left f ([],es) data in
          let rec rem_doubles = function
            | p1 :: p2 :: ps       when equal p1 p2                ->
                rem_doubles (p2 :: ps)
            | p1 :: p2 :: p3 :: ps when equal p1 (last (p3 :: ps)) ->
                rem_doubles (p2 :: p3 :: ps)
            | p1 :: p2 :: ps                                       ->
                p1 :: rem_doubles (p2 :: ps)
            | ps                                                   ->
                ps
          in
          let pts = rem_doubles pts in
          skel edgs pts
        end
  in
  skel []
