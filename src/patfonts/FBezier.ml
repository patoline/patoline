let derivee a =
  let len = Array.length a - 1 in
  let flen = float_of_int len in
  let b = Array.make len 0.0 in
  for i = 0 to len - 1 do
    b.(i) <- (a.(i+1) -. a.(i)) *. flen
  done; b

let casteljau_right f x =
  for t = Array.length f downto 2 do
    for i = 0 to (t-2) do
      f.(i) <- (1.0 -. x) *. f.(i) +. x *. f.(i+1)
    done
  done; f

let casteljau_left f x =
  for t = 1 to Array.length f - 1 do
    for i = Array.length f - 1 downto t do
      f.(i) <- (1.0 -. x) *. f.(i-1) +. x *. f.(i)
    done
  done; f

let eval f0 x =
  let f = Array.copy f0 in
  let a = casteljau_right f x in
  a.(0)

let descartes x0 x1 epsilon a =
  let has_root x =
    let rec has_root a b i =
      if i >= Array.length x then (a <= 0.0 && b >= 0.0)
      else if x.(i) <= a then has_root x.(i) b (i+1)
      else has_root a (max b x.(i)) (i+1)
    in
    has_root x.(0) x.(0) 1
  in
  let rec find_root x t0 t1 =
    if has_root x then
      let m = (t0 +. t1) /. 2.0 in
      if t1 -. t0 <= epsilon then
        (if x.(0) *. x.(Array.length x-1) <= 0.0 then [(t0,t1)] else [])
      else
        let left  = casteljau_left (Array.copy x) 0.5 in
        let right = casteljau_right x 0.5 in
        (find_root left t0 m) @ (find_root right m t1)
    else []
  in
  find_root a x0 x1

let bernstein_solve_int f eps =
  match f with
  | [||]      -> []
  | [|_|]     -> []
  | [|x;y|]   -> let t = x /. (x -. y) in
                 if t >= 0.0 && t <= 1.0 then [(t,t)] else []
  | [|x;y;z|] -> let a = x -. 2.0 *. y +. z in
                 let b = 2.0 *. (y -. x) in
                 let c = x in
                 let discr = b *. b -. 4.0 *. a *. c in
                 if discr < 0.0 then []
                 else if discr = 0.0 then
                   let x0 = -.b /. (2.0 *. a) in
                   if x0 >= 0.0 && x0 <= 1.0 then [(x0,x0)] else []
                 else
                   let x0 = (-.b -. sqrt discr) /. (2.0 *. a) in
                   let l0 =
                     if x0 >= 0.0 && x0 <= 1.0 then [(x0,x0)] else []
                   in
                   let x1 = (-.b +. sqrt discr) /. (2.0 *. a) in
                   if x1 >= 0.0 && x1 <= 1.0 then (x1,x1)::l0 else l0
  | _         -> descartes 0.0 1.0 eps (Array.copy f)

let bernstein_solve f eps=List.map (fun (a,b)->(a+.b)/.2.) (bernstein_solve_int f eps)

let bernstein_extr f =
  match f with
  | [||]      -> (infinity, -.infinity)
  | [|a|]     -> (a, a)
  | [|a;b|]   -> if a > b then (b, a) else (a, b)
  | [|a;b;c|] -> let ax = a -. b +. c in
                 let bx = b -. 2.0 *. a in
                 let xx = (-.bx) /. (2.0 *. ax) in
                 if ax <> 0.0 && bx <> 0.0 && xx > 0.0 && xx < 1.0 then
                   (min (min a c) (eval f xx), max (max a c) (eval f xx))
                 else (min a c, max a c)
  | _         -> let fmin = ref (min f.(0) f.(1)) in
                 let fmax = ref (max f.(0) f.(1)) in
                 let fn x =
                   if x >= 0.0 && x <= 1.0 then
                     begin
                       let y = eval f x in
                       fmin := min !fmin y;
                       fmax := max !fmax y
                     end
                 in
                 List.iter fn (bernstein_solve (derivee f) 1e-5);
                 (!fmin, !fmax)
