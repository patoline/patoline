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

let bounding_box' a=
  let rec bound x0 y0 x1 y1 i=
    if i>=Array.length a then ((x0,y0),(x1,y1)) else
      let x,y=a.(i) in bound (min x0 x) (min y0 y) (max x1 x) (max y1 y) (i+1)
  in
    bound (1./.0.) (1./.0.) (-1./.0.) (-1./.0.) 0
        
      
