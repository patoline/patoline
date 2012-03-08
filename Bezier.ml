type curve=(float array)*(float array)

exception OneDimensionalSystem

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
  if Array.length f<=0 then [] else (

    if Array.length f=1 then []
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




let mult a b=
  let result=Array.make_matrix (Array.length a) (Array.length b.(0)) 0. in
    for i=0 to Array.length result-1 do
      for j=0 to Array.length result.(0)-1 do
        let rec compute k sum=
          if k>=Array.length b then result.(i).(j)<-sum else
            compute (k+1) (sum+.a.(i).(k)*.b.(k).(j))
        in
          compute 0 0.
      done
    done;
    result

(* let fwd_subst l= *)
(*   let result=Array.make_matrix (Array.length l) (Array.length l.(0)) 0. in *)
(*     for j=0 to Array.length l-1 do *)
(*       for i=0 to Array.length l-1 do *)
(*         let rec compute k sum= *)
(*           if k>=i then ( *)
(*             if i=j then *)
(*               result.(i).(j)<-(1.-.sum)/.l.(i).(i) *)
(*             else *)
(*               result.(i).(j)<-(-.sum)/.l.(i).(i) *)
(*           ) else *)
(*             compute (k+1) (sum +. l.(i).(k)*.result.(k).(j)) *)
(*         in *)
(*           compute 0 0. *)
(*       done *)
(*     done; *)
(*     result *)

(* let a=[| *)
(*   [|1.;0.;0.;0.|]; *)
(*   [|0.;1.;0.;0.|]; *)
(*   [|0.;4.;1.;0.|]; *)
(*   [|0.;2.;0.;1.|] *)
(* |] *)

(* let _=fwd_subst a *)


let det a=
  let sign=ref 1 in
    for j=0 to Array.length a.(0)-1 do
      let rec maxi i k=
        if i>=Array.length a then k else
          if abs_float a.(k).(j) < abs_float a.(i).(j) then maxi (i+1) i else
            maxi (i+1) k
      in
      let pivot=maxi j j in

      let tmp=a.(pivot) in
        a.(pivot)<-a.(j);
        a.(j)<- tmp;

        if (pivot-j) land 1 = 1 then sign:= - !sign;
        for i=j+1 to Array.length a-1 do
	  if a.(j).(j) <> 0.0 then
	    (let fact= a.(i).(j)/.a.(j).(j) in
             for k=0 to Array.length a.(0)-1 do
               a.(i).(k)<-a.(i).(k) -. fact *. a.(j).(k)
             done)
        done
    done;
    let rec d i s=
      if i>=Array.length a then
        if !sign>0 then s else -.s
      else
        d (i+1) (s*.a.(i).(i))
    in
      d 0 1.


(* let a= *)
(*   let n=4 in *)
(*   let b=10. in *)
(*   let a=Array.make_matrix n n 0. in *)
(*     for i=0 to n-1 do *)
(*       for j=0 to n-1 do *)
(*         a.(i).(j)<-(Random.float b)-.b/.2. *)
(*       done *)
(*     done; *)
(*     a *)

(* let print_maple a= *)
(*   Printf.printf "["; *)
(*   for i=0 to Array.length a-1 do *)
(*     Printf.printf "%s[" (if i=0 then "" else ","); *)
(*     for j=0 to Array.length a.(i)-1 do *)
(*       Printf.printf "%s%f" (if j>0 then "," else "") a.(i).(j); *)
(*     done; *)
(*     Printf.printf "]"; *)
(*   done; *)
(*   Printf.printf "]" *)

(* let _=print_maple a *)

(* let _=det a *)

let binom n=
  let a=Array.make_matrix n n 0 in
    for i=0 to n-1 do
      a.(0).(i)<-1
    done;
    for i=1 to n-1 do
      for j=i to n-1 do
        a.(i).(j)<-
          if i<=j then
            a.(i-1).(j-1)+a.(i).(j-1)
          else
            0
      done
    done;
    a
let _=binom 4

let monomial a=
  let b=binom (Array.length a) in
  let x=Array.make (Array.length a) 0. in
    for k=0 to Array.length a-1 do
      for i=k to Array.length a-1 do
        let f=(float_of_int (b.(k).(i)*b.(i).(Array.length a-1))) *. a.(k) in

        x.(i)<-x.(i) +. (if (i-k) land 1=1 then -.f else f)

      done
    done;
    x

(* let evalm a t= *)
(*   let rec ev i s=if i<0 then s else *)
(*     ev (i-1) ((s*.t)+.a.(i)) *)
(*   in *)
(*     ev (Array.length a-1) 0. *)

(* let a= *)
(*   let b=20. in *)
(*   let a=Array.make 10 0. in *)
(*     for i=0 to Array.length a-1 do *)
(*       a.(i)<-(Random.float b)-.b/.2. *)
(*     done; *)
(*     a *)

(* let _=eval a 0.25 *)
(* let _=evalm (monomial a) 0.25 *)


let intersect (a,b) (c,d)=
  let eps=1e-5 in
  let extr_a=
    List.filter (fun x-> x>=0. && x<=1.)
      ((List.sort compare (0. :: 1.::bernstein_solve (derivee a) @ bernstein_solve (derivee b)))) in
  let extr_c=
    List.filter (fun x-> x>=0. && x<=1.)
      ((List.sort compare (0. :: 1.::bernstein_solve (derivee c) @ bernstein_solve (derivee d)))) in
  let sort x y=if x<y then (x,y) else (y,x) in
  let rec make_intervals l1 l2=match l1,l2 with
      [],_-> []
    | [_],_->[]
    | _,[]-> []
    | _,[_]->[]
    | h1::h2::s, h1'::h2'::s'-> (h1,h2,h1',h2')::(make_intervals [h1;h2] (h2'::s'))@(make_intervals (h2::s) l2)
  in

  let inside x y x0 y0=
    let resultant=Array.make_matrix (Array.length x+Array.length y-2) (Array.length x+Array.length y-2) 0. in
    let ma=monomial x in
    let mb=monomial y in
      for i=0 to Array.length mb-2 do
        for j=0 to Array.length ma-1 do
          resultant.(i).(i+j)<-ma.(Array.length ma-1-j)
        done;

        resultant.(i).(i+Array.length ma-1)<-resultant.(i).(i+Array.length ma-1) -. x0
      done;
      for i=Array.length mb-1 to Array.length resultant-1 do
        for j=0 to Array.length mb-1 do
          resultant.(i).(i-Array.length mb+1 + j)<-mb.(Array.length mb-1-j)
        done;
        resultant.(i).(i)<-resultant.(i).(i) -. y0
      done;
      det resultant
    in
    let rec inter l res=match l with
        []->res
      | (u1,v1,x0,y0,x1,y1,u2,v2,x0',y0',x1',y1')::s ->(
          let xa0,xa1=sort x0 x1 in
          let ya0,ya1=sort y0 y1 in
          let xc0,xc1=sort x0' x1' in
          let yc0,yc1=sort y0' y1' in
            if xa1<xc0 || xa0 > xc1 || ya1<yc0 || ya0 > yc1 then
              inter s res
            else (
              let m1=(u1+.v1)/.2. in
              let m2=(u2+.v2)/.2. in
                if v1-.u1 < eps && v2-.u2 < eps then
                  (if (inside a b x0' y0' *. inside a b x1' y1' < 0. ||
			 inside a b x0' y0' = 0.)
		     &&
			 (inside c d x0 y0 *. inside c d x1 y1 <= 0.)
		   then inter s ((m1,m2)::res)
                   else
                     inter s res
                  )
                else (
                  let xm1=eval a m1 in
                  let ym1=eval b m1 in
                  let xm2=eval c m2 in
                  let ym2=eval d m2 in
                    inter ((u1,m1,x0,y0,xm1,ym1,   u2,m2,x0',y0',xm2,ym2)::
                             (m1,v1,xm1,ym1,x1,y1,   u2,m2,x0',y0',xm2,ym2)::
                             (u1,m1,x0,y0,xm1,ym1,   m2,v2,xm2,ym2,x1',y1')::
                             (m1,v1,xm1,ym1,x1,y1,   m2,v2,xm2,ym2,x1',y1')::
                             s)
                      res
                )
            )
        )
    in
      inter (List.map (fun (u1,v1,u2,v2)->
                         (u1,v1,eval a u1, eval b u1, eval a v1, eval b v1,
                          u2,v2,eval c u2, eval d u2, eval c v2, eval d v2)
                      ) (make_intervals extr_a extr_c)) []

(* let x1=[| 0.; 100.; 200.; 200.|] *)
(* let y1=[| 0.; 0.; 100.; 200.|] *)

(* let x2=[| 0.; 0.; 100.; 200.|] *)
(* let y2=[| 200.; 100.; 0.; 0.|] *)


let x1=[| -1.; 30.|]
let y1=[| 0.; 0.|]


let x2=[| 10.; 30.; 20.|]
let y2=[| -30.; 0.; 50.|]


let _=intersect (x1,y1) (x2,y2)
