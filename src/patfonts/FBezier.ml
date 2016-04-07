let derivee a=
  let b=Array.make (Array.length a-1) 0. in
    for i=0 to Array.length b-1 do
      b.(i)<-(a.(i+1)-.a.(i)) *. (float_of_int (Array.length a-1))
    done;
    b

let casteljau_right f x=
  for t=Array.length f downto 2 do
    for i=0 to (t-2) do
      f.(i) <- (1. -. x) *. f.(i) +. x *. f.(i+1)
    done
  done;
  f


let casteljau_left f x=
  for t=1 to Array.length f-1 do
    for i=Array.length f-1 downto t do
      f.(i) <- (1. -. x) *. f.(i-1) +. x *. f.(i)
    done
  done;
  f

let eval f0 x=
  let f=Array.copy f0 in
    (casteljau_right f x).(0)

let length' nb_div t (xa,ya) =
  let xb = derivee xa and yb = derivee ya in
  let s = ref 0.0 in
  let n = t /. float nb_div in
  let n2 = n /. 2. in
  for i0 = 0 to nb_div do
    let i = float i0 in
    let t = i *. n in
    let t' = t +.  n2 in
    let xd1 = eval xb t in
    let yd1 = eval yb t in
    if i0 = nb_div then
      s := !s +. sqrt (xd1*.xd1 +. yd1*.yd1)
    else
      let xd2 = eval xb t' in
      let yd2 = eval yb t' in
      if i0 = 0 then
	s := !s +. sqrt (xd1*.xd1 +. yd1*.yd1) +. 4. *. sqrt (xd2*.xd2 +. yd2*.yd2)
      else
	s := !s +. 2. *. sqrt (xd1*.xd1 +. yd1*.yd1) +. 4. *. sqrt (xd2*.xd2 +. yd2*.yd2)
  done;
  !s /. 6. *. n

let length = length' 10 1.

let descartes x0 x1 epsilon a=
  let has_root x=
    let rec has_root a b i=
      if i>=Array.length x then (a<=0. && b>=0.) else
        if x.(i)<=a then
          has_root x.(i) b (i+1)
        else has_root a (max b x.(i)) (i+1)
    in
      has_root x.(0) x.(0) 1
  in
  let rec find_root x t0 t1=
    if has_root x then (
      let m=(t0+.t1)/.2. in
        if t1-.t0 <= epsilon then (
            if x.(0)*.x.(Array.length x-1)<=0. then [(t0,t1)] else []
        ) else (
          let left=casteljau_left (Array.copy x) 0.5 in
          let right=casteljau_right x 0.5 in
            (find_root left t0 m)@(find_root right m t1)
        )
    ) else (
      []
    )
  in
    find_root a x0 x1



(* Pour faire une bounding box correcte, il faudrait faire une regle de Descartes,
   decouper en intervalles, et faire un coup de Newton sur chaque intervalle, mais... *)

(*
let bounding_box (a,b)=
  if Array.length a=2 then
    (min a.(0) a.(1), min b.(0) b.(1)), (max a.(0) a.(1), max b.(0) b.(1))
  else
    if Array.length a=3 then
      let tx=(a.(0)-.a.(1))/.(a.(0)-.2.*.a.(1)+.a.(2)) in
      let ty=(b.(0)-.b.(1))/.(b.(0)-.2.*.b.(1)+.b.(2)) in
      let x0=if tx>=0 && tx<=1 then tx else a.(0) in
      let y0=if ty>=0 && ty<=1 then ty else b.(0) in
*)

let bernstein_solve_int f eps=
  if Array.length f<=0 then [] else (

    if Array.length f=1 then []
    else if Array.length f=2 then (
      let t=f.(0)/.(f.(0)-.f.(1)) in
        if t>=0. && t<=1. then [t,t] else []
    ) else (
      if Array.length f=3 then (
        let a=f.(0)-.2.*.f.(1)+.f.(2) in
        let b= 2.*.(f.(1) -. f.(0)) in
        let c=f.(0) in
        let discr= b*.b -. 4.*.a*.c in
          if discr<0. then [] else
            if discr=0. then (
              let x0= -.b/.(2.*.a) in
                if x0>=0. && x0<=1. then [x0,x0] else []
            ) else (
              let x0=(-.b-.sqrt discr)/.(2.*.a) in
              let l0=if x0>=0. && x0<=1. then [x0,x0] else [] in
              let x1=(-.b+.sqrt discr)/.(2.*.a) in
              let l1=if x1>=0. && x1<=1. then (x1,x1)::l0 else l0 in
                l1
            )
      ) else (
        descartes 0. 1. eps (Array.copy f)
      )
    )
  )

let bernstein_solve f eps=List.map (fun (a,b)->(a+.b)/.2.) (bernstein_solve_int f eps)

let bernstein_extr f=
  match Array.length f with
      0->(1./.0., -1./.0.)
    | 1->f.(0),f.(0)
    | 2->if f.(0) > f.(1) then f.(1),f.(0) else f.(0),f.(1)
    | 3->
        (
          let a=f.(0) and b=f.(1) and c=f.(2) in
          let fmin=ref (min a c) in
          let fmax=ref (max a c) in
          let ax=a-.b+.c in
          let bx=b-.2.*.a in
          let xx=(-.bx)/.(2.*.ax) in
            if ax <> 0. && bx<>0. && xx>0. && xx<1. then
              (fmin:=min !fmin (eval f xx); fmax:=max !fmax (eval f xx));
            (!fmin, !fmax)
        )
    | _->
        (let fmin=ref (infinity) in
         let fmax=ref (-.infinity) in
           fmin:=min !fmin f.(0); fmax:=max !fmax f.(0);
           fmin:=min !fmin f.(1); fmax:=max !fmax f.(1);
           List.iter (fun x->if x>=0. && x<=1. then let y=eval f x in (fmin:=min !fmin y; fmax:=max !fmax y))
             (bernstein_solve (derivee f) 1e-5);
           !fmin, !fmax
        )
