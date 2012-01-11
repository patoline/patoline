type curve=(float array)*(float array)
 
let curve (a,b) i=a.(i),b.(i)


let larger ((a,b),(c,d)) ((e,f),(g,h))= (min a e,min b f), (max c g, max d h)


let derivee a=
  let b=Array.create (Array.length a-1) 0. in
    for i=0 to Array.length b-1 do
      b.(i)<-a.(i+1)-.a.(i)
    done;
    b
      

let casteljau f0 x=
  let f=Array.copy f0 in
  for t=Array.length f downto 2 do
    for i=0 to (t-2) do
      f.(i) <- (1. -. x) *. f.(i) +. x *. f.(i+1)
    done
  done;
  f.(0)


let newton x0 epsilon a=
  let b=derivee a in
  let rec find x=
    let fx=casteljau a x in
      if abs_float fx <=epsilon then
        let f'x=casteljau b x in
          find (x -. fx/.f'x)
      else
        x
  in
    find x0

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
              if ax>0. then (casteljau f xx, max a c) else
                (min a c, casteljau f xx)
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
                    let ft0=casteljau f t0 in
                      if ft0<a then 
                        if d<a then (min d ft0,a) else (ft0,d)
                      else
                        if d<a then (d,ft0) else (a,max ft0 d)
                  ) else (
                    if a<d then (a,d) else (d,a)
                  ) in

                  if t1>0. && t1<1. then (
                    let ft1=casteljau f t1 in
                      if ft1<f0 then (ft1,f1) else
                        if ft1>f1 then (f0,ft1) else (f0,f1)
                  ) else (f0,f1) 
                    
            else
              if bx <> 0. then
                (let t0= -.cx/.bx in
                 let f0=if t0>0. && t0<1. then casteljau f t0 else a in
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
