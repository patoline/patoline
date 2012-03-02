type curve=(float array)*(float array)

let curve (a,b) i=a.(i),b.(i)


let larger ((a,b),(c,d)) ((e,f),(g,h))= (min a e,min b f), (max c g, max d h)


let derivee a=
  let b=Array.create (Array.length a-1) 0. in
    for i=0 to Array.length b-1 do
      b.(i)<-a.(i+1)-.a.(i)
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

let restrict f0 a b=
  let f=Array.copy f0 in
    casteljau_left (casteljau_right f a) b

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
          let x0=eval x t0 in
          let x1=eval x t1 in
            if x0*.x1<=0. then [m] else []
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

let bernstein_solve f=
  if Array.length f<=0 then failwith "one-dimensional system" else (

    if Array.length f=1 then (if f.(0)=0. then failwith "one-dimensional system" else [])
    else if Array.length f=2 then (
      let t=f.(0)/.(f.(0)-.f.(1)) in
        if t>=0. && t<=1. then [t] else []
    ) else (
      if Array.length f=3 then (
        let a=f.(0)-.2.*.f.(1)+.f.(2) in
        let b= 2.*.(f.(1) -. f.(0)) in
        let c=f.(0) in
        let discr= b*.b -. 4.*.a*.c in
          if discr<0. then [] else
            if discr=0. then [-.b/.(2.*.a)] else
              [ (-.b-.sqrt discr)/.(2.*.a); (-.b+.sqrt discr)/.(2.*.a) ]
      ) else (
        descartes 0. 1. (1e-5) f
      )
    )
  )

let bernstein_extr f=
  match Array.length f with
      0->(1./.0., -1./.0.)
    | 1->f.(0),f.(0)
    | 2->if f.(0) > f.(1) then f.(1),f.(0) else f.(0),f.(1)
    | 3->
        (
          let a=f.(0) and b=f.(1) and c=f.(2) in
          let ax=a-.b+.c in
          let bx=b-.2.*.a in
          let xx=(-.bx)/.(2.*.ax) in
            if ax != 0. && xx>0. && xx<1. then
              if ax>0. then (eval f xx, max a c) else
                (min a c, eval f xx)
            else
              if c>=a then (a,c) else (c,a)
        )
    | 4->
        (
          let a=f.(0) and b=f.(1) and c=f.(2) and d=f.(3) in
          let ax= -3.*.a +. 9.*.b -. 9.*.c +.3.*.d in
          let bx= 6.*.a-.12.*.b+.6.*.c in
          let cx= -3.*.a+.3.*.b in
          let discr= bx*.bx -. 4.*.ax*.cx in
            if ax <> 0. then

              if discr < 0. then (if a<d then (a,d) else (d,a)) else
                let t0= (-.bx -. sqrt discr) /.(2.*.ax) in
                let t1= (-.bx +. sqrt discr) /.(2.*.ax) in
                let (f0,f1)=
                  if t0>0. && t0<1. then (
                    let ft0=eval f t0 in
                      if ft0<a then
                        if d<a then (min d ft0,a) else (ft0,d)
                      else
                        if d<a then (d,ft0) else (a,max ft0 d)
                  ) else (
                    if a<d then (a,d) else (d,a)
                  ) in

                  if t1>0. && t1<1. then (
                    let ft1=eval f t1 in
                      if ft1<f0 then (ft1,f1) else
                        if ft1>f1 then (f0,ft1) else (f0,f1)
                  ) else (f0,f1)

            else
              if bx <> 0. then
                (let t0= -.cx/.bx in
                 let f0=if t0>0. && t0<1. then eval f t0 else a in
                   if f0<a then
                     if a<d then (f0,d) else (min f0 d, a)
                   else
                     if a>d then (d,f0) else (a,max f0 d))
              else
                if a<=d then (a,d) else (d,a)
        )
    | _->
        (let rec bound x0 x1 i=
           if i>=Array.length f then (x0,x1) else
             if f.(i) < x0 then bound f.(i) x1 (i+1) else
               if f.(i) > x1 then bound x0 f.(i) (i+1) else
                 bound x0 x1 (i+1)
         in
           bound (1./.0.) (-1./.0.) 0)

let bounding_box (a,b)=
  let (x0,x1)=bernstein_extr a in
  let (y0,y1)=bernstein_extr b in
    (x0,y0), (x1,y1)


let intersect (a,b) (c,d)=
  let eps=1e-5 in
  let extr_a= List.filter (fun x-> x>=0. && x<=1.)
    (0. :: (List.sort compare (bernstein_solve (derivee a) @ bernstein_solve (derivee b)))) in
  let extr_c= List.filter (fun x-> x>=0. && x<=1.)
    (0. :: (List.sort compare (bernstein_solve (derivee c) @ bernstein_solve (derivee d)))) in
  let sort x y=if x<y then (x,y) else (y,x) in
  let rec make_intervals l1 l2=match l1,l2 with
      [],_-> []
    | [_],_->[]
    | _,[]-> []
    | _,[_]->[]
    | h1::h2::s, h1'::h2'::s'-> (h1,h2,h1',h2')::(make_intervals (h2::s) l2)@(make_intervals l1 (h2'::s'))
  in
  let rec inter l res=match l with
      []->res
    | (u1,v1,u2,v2)::s ->(
        let xa0,xa1=sort (eval a u1) (eval a v1) in
        let ya0,ya1=sort (eval b u1) (eval b v1) in
        let xc0,xc1=sort (eval c u2) (eval c v2) in
        let yc0,yc1=sort (eval d u2) (eval d v2) in
          if xa1<xc0 || xa0 > xc1 || ya1<yc0 || ya0 > yc1 then
            inter s res
          else (
            let m1=(u1+.v1)/.2. in
            let m2=(u2+.v2)/.2. in
              if v1-.u1 < eps && v2-.u2 < eps then
                inter s ((m1,m2)::res)
              else (
                inter ((u1,m1,u2,m2)::(u1,m1,m2,v2)::
                         (m1,v1,u2,m2)::(m1,v1,m2,v2)::s)
                  res
              )
          )
      )
  in
    inter (make_intervals extr_a extr_c) []

let x1=[| 0.; 100.; 200.; 200.|]
let y1=[| 0.; 0.; 100.; 200.|]

let x2=[| 0.; 0.; 100.; 200.|]
let y2=[| 200.; 100.; 0.; 0.|]

let _=intersect (x1,y1) (x2,y2)
