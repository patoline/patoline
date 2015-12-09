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
type curve=(float array)*(float array)

exception OneDimensionalSystem

let curve (a,b) i=a.(i),b.(i)


let larger ((a,b),(c,d)) ((e,f),(g,h))= (min a e,min b f), (max c g, max d h)

let rev (a,b)=
  let a'=Array.make (Array.length a) a.(0) in
  let b'=Array.make (Array.length b) b.(0) in
  for i=0 to Array.length a'-1 do
    a'.(i)<-a.(Array.length a-1-i)
  done;
  for i=0 to Array.length b'-1 do
    b'.(i)<-b.(Array.length b'-1-i)
  done;
  a',b'


let derivee a=
  let b=Array.make (Array.length a-1) 0. in
    for i=0 to Array.length b-1 do
      b.(i)<-(a.(i+1)-.a.(i)) *. (float_of_int (Array.length a-1))
    done;
    b

  
let derivee_start a =
  (a.(1) -. a.(0)) *.  (float_of_int (Array.length a-1))

let derivee_end a =
  let n = Array.length a in
  (a.(n - 1) -. a.(n - 2)) *.  (float_of_int (n-1))

let derivee_approx a =
  let n = Array.length a in
  (a.(n - 1) -. a.(0)) *.  (float_of_int (n-1))

let sanitize (a,b as orig) =
  Printf.printf "sanitize start\n";
  let la = Array.length a in
  let lb = Array.length b in
  let lmin = min la lb in
  let new_start =  ref 0 in
  let modif = ref false in
  while !new_start < lmin - 1 &&
    abs_float (a.(!new_start) -. a.(!new_start + 1)) < 10.0 &&
    abs_float (b.(!new_start) -. b.(!new_start + 1)) < 10.0
  do
    incr new_start;
    modif := true;
  done;
  let new_end_a = ref (la - 1) in
  let new_end_b = ref (lb - 1) in
  while !new_end_a > !new_start && !new_end_b > !new_start &&
        abs_float (a.(!new_end_a) -. a.(!new_end_a - 1)) < 10.0 &&
        abs_float (b.(!new_end_b) -. b.(!new_end_b - 1)) < 10.0
  do
    decr new_end_a;
    decr new_end_b;
    modif := true;
  done;
  Printf.printf "sanitize end %b\n" !modif;

  if !modif then begin
(*
    Printf.printf "la = %d, lb = %d, new_start = %d; new_end_a = %d; new_end_b = %d\n"
     la lb !new_start !new_end_a !new_end_b;
*)
    Array.sub a !new_start (!new_end_a - !new_start + 1),
    Array.sub b !new_start (!new_end_b - !new_start + 1)
  end else orig 

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
let partial_length = length' 10

let restrict f0 a b=
  match Array.length f0 with
    | 0 -> Printf.printf ("Warning: attempt to restrict an empty curve.\n") ; [||]
    | 1 -> Printf.printf ("Warning: attempt to restrict a trivial curve.\n") ; [|f0.(0)|]
    | 2 ->
      let x = f0.(0) in
	   let y = f0.(1) in
	   let xy = y -. x in
	   [| x +. a *. xy ;
	      y -. (1. -. b) *. xy
	   |]
    | _ ->
      let f=Array.copy f0 in
      casteljau_left (casteljau_right f a) ((b-.a)/.(1.-.a))

let split2 f0 a b=
  let f1 =Array.copy f0 in
  let f2 =Array.copy f0 in
  let f3 =Array.copy f0 in
  let f1 = casteljau_left f1 a in
  let f2 = casteljau_left (casteljau_right f2 a) ((b-.a)/.(1.-.a)) in
  let f3 = casteljau_right f3 b in
  f1,f2,f3

let divide f0 n = (* n > 0 *)
  let rec divide_rec res f0 n =
    if n = 1 then List.rev res else
      let time_step = 1. /. (float_of_int n) in
      let left = casteljau_left (Array.copy f0) time_step in
      let right = casteljau_right (Array.copy f0) time_step in
      divide_rec (left :: res) right (n - 1)
  in divide_rec [] f0 n

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

let bounding_box (a,b)=
  let (x0,x1)=bernstein_extr a in
  let (y0,y1)=bernstein_extr b in
    (x0,y0), (x1,y1)




let mult_matrix a b=
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

let t = [[ [|0.0;1.0|], [|0.0;0.0|]; [|1.0;0.0|], [|0.0;1.0|]; [|0.0;0.0|], [|1.0;0.0|] ]]

let oriente all =
  let horizontal_intercept x y bs =
    let count = ref 0 in
    List.iter (fun (xs, ys) ->
	let s = Array.length xs - 1 in
	let xa = xs.(0) in
	let xb = xs.(s) in
	let ya = ys.(0) in
	let yb = ys.(s) in
      if (ya -. y)*.(yb -. y) <= 0.0 then begin
	let t = (ya -. y) /. (ya -. yb) in
	let xt = xa *. (1. -. t) +. xb *. t in
	if xt >= x then incr count
      end) bs;
    !count mod 2 <> 0
  in

  let orientation_composante bs =
    let area =
      List.fold_left (fun a (xs, ys) ->
	let s = Array.length xs - 1 in
	let xa = xs.(0) in
	let xb = xs.(s) in
	let ya = ys.(0) in
	let yb = ys.(s) in
	a +. xa *. yb -. xb *. ya) 0.0 bs 
    in
    area >= 0.0 (* =0 should be treated by subdivision,
	      but very smalle values too ?*)
  in

  let num_outside bs others =
    match bs with
      (xs,ys)::_ ->
	let x = xs.(0) in
	let y = ys.(0) in
	let l = List.filter (fun bs -> horizontal_intercept x y bs) others in
	List.length l = 0 mod 2
    | [] -> assert false
  in

(*  let all = List.map (fun l -> let l = List.map sanitize l in List.filter (fun (a,b) -> Array.length a > 1 && Array.length b > 1) l) all in*)
  List.map (fun bs -> bs, orientation_composante bs = num_outside bs (List.filter (fun x -> x != bs) all)) all

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

let print_maple a=
  Printf.printf "[";
  for i=0 to Array.length a-1 do
    Printf.printf "%s[" (if i=0 then "" else ",");
    for j=0 to Array.length a.(i)-1 do
      Printf.printf "%s%f" (if j>0 then "," else "") a.(i).(j);
    done;
    Printf.printf "]";
  done;
  Printf.printf "]\n"

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


let extremity (a, b) = 
  let eps=1e-3 in
  List.filter (fun x-> x>=0. && x<=1.)
    ((List.sort compare (0. :: 1.::bernstein_solve (derivee a) eps @ bernstein_solve (derivee b) eps)))

let intersect' (a,b) extr_a (c,d) extr_c =
  let eps=1e-3 in
  let sort x y=if x<y then (x,y) else (y,x) in
  let rec make_intervals l1 l2=
    match l1,l2 with
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
    let rec inter l res=
      match l with
        []->res
      | (u1,v1,x0,y0,x1,y1,u2,v2,x0',y0',x1',y1')::s ->(
          let xa0,xa1=sort x0 x1 in
          let ya0,ya1=sort y0 y1 in
          let xc0,xc1=sort x0' x1' in
          let yc0,yc1=sort y0' y1' in
            if not (xa1>=xc0 && xa0 <= xc1 && ya1>=yc0 && ya0 <= yc1) then
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

let intersect ab cd = 
  let extr_a = extremity ab in
  let extr_c = extremity cd in
  intersect' ab extr_a cd extr_c

(* let x1=[| 0.; 100.; 200.; 200.|] *)
(* let y1=[| 0.; 0.; 100.; 200.|] *)

(* let x2=[| 0.; 0.; 100.; 200.|] *)
(* let y2=[| 200.; 100.; 0.; 0.|] *)


(* let x1=[| -1.; 30.|] *)
(* let y1=[| 0.; 0.|] *)

(* let x2=[| 10.; 30.; 20.|] *)
(* let y2=[| -30.; 0.; 50.|] *)

(* let _=intersect (x1,y1) (x2,y2) *)





let elevate f r=
  if r<=0 then f else (
    let g=Array.make (Array.length f+r) 0. in
    let n=Array.length f-1 in
    let bin=binom (Array.length f+r) in
      for i=0 to n do
        for j=i to i+r do
          g.(j)<-g.(j)+. f.(i)*.(float_of_int (bin.(i).(n)*bin.(j-i).(r))) /.
            (float_of_int bin.(j).(n+r))
        done
      done;
      g)


let elevate2 f ra rb=
  let a=Array.length f in
  let b=if Array.length f>=0 then Array.length f.(0) else 0 in
  let res=Array.make_matrix (a+ra) (b+rb) 0. in
  let bin=binom (1+max (a+ra) (b+rb)) in
    if a>0 && b>0 then (
      for i=0 to a+ra-1 do
        for j=0 to b+rb-1 do

          for i'=0 to min (a-1) i do
            for j'=0 to min (b-1) j do
              let x0=(float_of_int (bin.(i').(a-1) * bin.(i-i').(ra)))/.
                (float_of_int bin.(i).(a+ra-1))
              in
              let x1=(float_of_int (bin.(j').(b-1) * bin.(j-j').(rb)))/.
                (float_of_int bin.(j).(b+rb-1))
              in
                res.(i).(j)<-res.(i).(j) +. x0*.x1*.f.(i').(j')
            done
          done

        done
      done
    );
    res


(* On a des polynomes en u sur la premiere dimension, en v sur la deuxieme *)
let eval2 f u v=eval (Array.map (fun g->eval g u) f) v

let restrict2 f u0 u1 v0 v1 =
  let transpose g=
    let t=Array.make_matrix (Array.length g.(0)) (Array.length g) 0. in
      for i=0 to Array.length t-1 do
        for j=0 to Array.length t.(0)-1 do
          t.(i).(j)<-g.(j).(i)
        done
      done;
      t
  in
  let restr g a b=casteljau_left (casteljau_right g a) ((b-.a)/.(1.-.a)) in
    transpose (Array.map (fun x->restr x v0 v1)
                 (transpose (Array.map (fun x->restrict x u0 u1) f)))


let times f g=
  let m=Array.length f-1 in
  let n=Array.length g-1 in
  let h=Array.make (m+n+1) 0. in
  let bin=binom (m+n+1) in
    for i=0 to m+n do
      for j=max 0 (i-n) to min m i do
        h.(i)<-h.(i)+. (float_of_int (bin.(j).(m)*bin.(i-j).(n))) /. (float_of_int bin.(i).(m+n)) *. f.(j) *. g.(i-j)
      done
    done;
    h

let times2 f g=
  let af=Array.length f in
  let bf=Array.length f.(0) in
  let ag=Array.length g in
  let bg=Array.length g.(0) in
  let h=Array.make_matrix (af+ag-1) (bf+bg-1) 0. in
  let bin=binom (max (af+ag) (bf+bg)) in
    for i=0 to af+ag-2 do
      for j=0 to bf+bg-2 do
        for i'=max 0 (i-ag+1) to min i (af-1) do
          for j'=max 0 (j-bg+1) to min j (bf-1) do
            let a=bin.(i').(af-1) * bin.(i-i').(ag-1) * bin.(j').(bf-1) * bin.(j-j').(bg-1) in
              h.(i).(j) <- h.(i).(j) +. f.(i').(j')*.g.(i-i').(j-j')*. (float_of_int a)

          done
        done;
        h.(i).(j)<-h.(i).(j) /. (float_of_int (bin.(i).(af+ag-2)*bin.(j).(bf+bg-2)))
      done
    done;
    h


let plus2 f g=
  let mf=Array.length f in
  let nf=Array.length f.(0) in
  let mg=Array.length g in
  let ng=Array.length g.(0) in
  let ff=elevate2 f (max mf mg-mf) (max nf ng-nf) in
  let gg=elevate2 g (max mf mg-mg) (max nf ng-ng) in
  let h=Array.make_matrix (max mf mg) (max nf ng) 0. in
    for i=0 to Array.length ff-1 do
      for j=0 to Array.length ff.(0)-1 do
        h.(i).(j) <- ff.(i).(j)+.gg.(i).(j)
      done
    done;
    h

let minus2 f g=
  let mf=Array.length f in
  let nf=Array.length f.(0) in
  let mg=Array.length g in
  let ng=Array.length g.(0) in
  let ff=elevate2 f (max mf mg-mf) (max nf ng-nf) in
  let gg=elevate2 g (max mf mg-mg) (max nf ng-ng) in
  let h=Array.make_matrix (max mf mg) (max nf ng) 0. in
    for i=0 to Array.length ff-1 do
      for j=0 to Array.length ff.(0)-1 do
        h.(i).(j) <- ff.(i).(j)-.gg.(i).(j)
      done
    done;
    h

let derivee2_0 f=Array.map derivee f
let derivee2_1 a=
  let b=Array.make_matrix (Array.length a-1)  (Array.length a.(0)) 0. in
    for i=0 to Array.length b-1 do
      for j=0 to Array.length b.(0)-1 do

        b.(i).(j)<-(a.(i+1).(j)-.a.(i).(j))*.(float_of_int (Array.length a-1))

      done
    done;
    b


let promote0 f=[|f|]
let promote1 f=Array.map (fun x->[|x|]) f

let sq2 f=times2 f f
let sq f=times f f

exception Found

let solve2 eq0 eq1=
  let eps=1e-3 in
  let rec solve2 l r=

    match l with
      []->r
    | ((u1,v1,u2,v2) as h)::s ->(
        if v1-.u1<eps && v2-.u2<eps then (
          let r0=restrict2 eq0 u1 v1 u2 v2 in
          let r1=restrict2 eq1 u1 v1 u2 v2 in
            if r0.(0).(0) *. r0.(0).(Array.length r0.(0)-1) > 0. &&
              r0.(0).(Array.length r0.(0)-1) *. r0.(Array.length r0-1).(Array.length r0.(0)-1) > 0. &&
              r0.(Array.length r0-1).(Array.length r0.(0)-1) *. r0.(Array.length r0-1).(0) > 0. &&
              r0.(Array.length r0-1).(0)*.r0.(0).(0) > 0. &&

              r1.(0).(0) *. r1.(0).(Array.length r1.(0)-1) > 0. &&
              r1.(0).(Array.length r1.(0)-1) *. r1.(Array.length r1-1).(Array.length r1.(0)-1) > 0. &&
              r1.(Array.length r1-1).(Array.length r1.(0)-1) *. r1.(Array.length r1-1).(0) > 0. &&
              r1.(Array.length r1-1).(0)*.r1.(0).(0) > 0.
            then solve2 s r else solve2 s (h::r)

        ) else (
          let r0=restrict2 eq0 u1 v1 u2 v2 in
          let r1=restrict2 eq1 u1 v1 u2 v2 in
            if (try
                  for i=0 to Array.length r0-1 do
                    for j=0 to Array.length r0.(i)-1 do
                      if r0.(i).(j) *. r0.(0).(0) <= 0. then raise Found
                    done
                  done;
                  false
                with
                    Found -> true) && (
              try
                for i=0 to Array.length r1-1 do
                  for j=0 to Array.length r1.(i)-1 do
                    if r1.(i).(j) *. r1.(0).(0) <= 0. then raise Found
                  done
                done;
                false
              with
                  Found -> true)
            then (

              let m1=(u1+.v1)/.2. in
              let m2=(u2+.v2)/.2. in
                solve2 ((u1,m1,u2,m2)::(u1,m1,m2,v2)::(m1,v1,u2,m2)::(m1,v1,m2,v2)::s) r
            ) else solve2 s r
        )
      )
  in
    solve2 [(0.,1.,0.,1.)] []


(* let rand ()= *)
(*   let n=41 in *)
(*   let m=41 in *)
(*   let x=10. in *)
(*   let a=Array.make_matrix m n 0. in *)
(*     for i=0 to m-1 do *)
(*       for j=0 to n-1 do *)
(*         a.(i).(j)<- (Random.float x)-.x *)
(*       done *)
(*     done; *)
(*     a *)

(* let _=Random.self_init () *)

(* let _= *)
(*   let a=rand () in *)
(*   let b=rand () in *)
(*   let x=Random.float 1. in *)
(*   let y=Random.float 1. in *)
(*     Printf.printf "%f %f\n"(eval2 (restrict2 a 0. 1. 0.5 1.) y 0.5) (eval2 a y 0.75); *)
(*     derivee2_1 a *)



let plus f g=
  let n=Array.length f in
  let m=Array.length g in
  let ff=elevate f (max n m - n) in
  let gg=elevate g (max n m - m) in
  let h=Array.make (max n m) 0. in
    for i=0 to Array.length ff-1 do
      h.(i)<-ff.(i)+.gg.(i)
    done;
    h

let minus f g=
  let n=Array.length f in
  let m=Array.length g in
  let ff=elevate f (max n m - n) in
  let gg=elevate g (max n m - m) in
  let h=Array.make (max n m) 0. in
    for i=0 to Array.length ff-1 do
      h.(i)<-ff.(i)-.gg.(i)
    done;
    h


let distance (xa,ya) (xb,yb)=
  (* On commence par traiter le cas special de deux droites paralleles *)
  (* let xa'=derivee xa in *)
  (* let xb'=derivee xb in *)
  (* let ya'=derivee ya in *)
  (* let yb'=derivee yb in *)

  (* let droite x'= *)
  (*   let i=ref 0 in *)
  (*     while !i< Array.length x' && abs_float (x'.(i) -. x'.(0)) > 1e-12 do incr i done; *)
  (*     !i>=Array.length x' *)
  (* in *)

  let dist=plus2 (sq2 (minus2 (promote0 xa) (promote1 xb))) (sq2 (minus2 (promote0 ya) (promote1 yb))) in
  let d0=derivee2_0 dist in
  let d1=derivee2_1 dist in
    List.fold_left (fun d (u1,v1,u2,v2)->
                      let m1=(u1+.v1)/.2. in
                      let m2=(u2+.v2)/.2. in
                      let x0=(eval xa m1 -. eval xb m2) in
                      let y0=(eval ya m1 -. eval yb m2) in
                        min d (sqrt (x0*.x0+.y0*.y0))
                   ) infinity ((0.,0.,0.,0.)::(0.,0.,1.,1.)::(1.,1.,0.,0.)::(1.,1.,1.,1.)::(solve2 d0 d1))


(* let xa=[|50.;400.;400.;200.|] *)
(* let ya=[|50.;100.;78.;400.|] *)

(* let xb=[|450.;400.;300.;500.|] *)
(* let yb=[|30.;30.;499.;400.|] *)


(* let _=distance (xa, ya) (xb, yb) *)




(* let draw x y= *)
(*   Graphics.moveto (int_of_float x.(0)) (int_of_float y.(0)); *)
(*   Graphics.curveto (int_of_float x.(1),int_of_float y.(1)) *)
(*     (int_of_float x.(2),int_of_float y.(2)) *)
(*     (int_of_float x.(3),int_of_float y.(3)) *)


(* let _= *)
(*   Graphics.open_graph ""; *)
(*   Graphics.clear_graph (); *)
(*   draw xa ya; *)
(*   draw xb yb; *)
(*   let s=0.616 in *)
(*   let t=0.37 in *)
(*   Graphics.moveto (int_of_float (eval xa s)) (int_of_float (eval ya s)); *)
(*   Graphics.lineto (int_of_float (eval xb t)) (int_of_float (eval yb t)); *)
(*   let _=Graphics.wait_next_event [Graphics.Key_pressed] in () *)


let distance1 (xa,ya) (xb,yb)=
  (* On commence par traiter le cas special de deux droites paralleles *)
  (* let xa'=derivee xa in *)
  (* let xb'=derivee xb in *)
  (* let ya'=derivee ya in *)
  (* let yb'=derivee yb in *)

  (* let droite x'= *)
  (*   let i=ref 0 in *)
  (*     while !i< Array.length x' && abs_float (x'.(i) -. x'.(0)) > 1e-12 do incr i done; *)
  (*     !i>=Array.length x' *)
  (* in *)

  let xa = [|xa|] and ya = [|ya|] in
  let dist=plus (sq (minus xb xa)) (sq (minus yb ya)) in
  let d=derivee dist in
    List.fold_left (fun d u ->
      let x = xa.(0) -. eval xb u and y = ya.(0) -. eval yb u in
      min d (sqrt (x*.x+.y*.y))
    ) infinity (0.::1.::(bernstein_solve d 1e-3))

let thickness (xa,ya) =
  let l = Array.length xa - 1 in
  let (xv, yv) = xa.(l) -. xa.(0), ya.(l) -. ya.(0) in
  let norm = sqrt (xv *. xv +. yv *. yv) in
  let (xn, yn) = (-. yv /. norm, xv /. norm) in
  let n = ref 0.0 and p = ref 0.0 in 
  for i = 1 to l - 1 do
    let s = ((xa.(i) -. xa.(0)) *. xn +. (ya.(i) -. ya.(0)) *. yn) in
    if s < !n then n := s;
    if s > !p then p := s
  done;
  !p -. !n

let subdivise thick b =
  let rec fn acc (xa, ya as b) =
    if thickness b > thick then
      let acc =  fn acc (restrict xa 0.0 0.5, restrict ya 0.0 0.5) in
      fn acc (restrict xa 0.5 1.0, restrict ya 0.5 1.0) 
  else
    b::acc
  in List.rev (fn [] b)

let det x y x' y' = x *. y' -. y *. x'

let thickness2 (xa,ya) =
  let len = Array.length xa - 1 in
  try
    let (xv, yv) = xa.(len) -. xa.(0), ya.(len) -. ya.(0) in
    let norm2 = xv *. xv +. yv *. yv in
    if norm2 < 1e-12 then raise Exit;
    let norm = sqrt norm2 in
    let (xn, yn) = (-. yv /. norm, xv /. norm) in
    let n = ref 0.0 and p = ref 0.0 in 
    for i = 1 to len - 1 do
      let xi = xa.(i) -. xa.(0) and yi = ya.(i) -. ya.(0) in
      let s = xi *. xn +. yi *. yn in
      if s < !n then n := s;
      if s > !p then p := s;
      let u = (xi *. xv +. yi *. yv) /. norm2 in
      if u < 0.0 || u > 1.0 then raise Exit
    done;
    let x,y = xa.(0), ya.(0) in
    let x',y' = xa.(len), ya.(len) in

    let p1 = x +. !n *. xn, y +. !n *. yn in
    let p2 = x' +. !n *. xn, y' +. !n *. yn in
    let p3 = x' +. !p *. xn, y' +. !p *. yn in
    let p4 = x +. !p *. xn, y +. !p *. yn in
    (p1,p2,p3,p4), !p -. !n
  with Exit ->
    let x,x' = Array.fold_left (fun (x,x' as acc) x0 -> if x0 < x then x0, x' else if x0 > x' then x,x0 else acc)
      (xa.(0), xa.(0)) xa in
    let y,y' = Array.fold_left (fun (x,x' as acc) x0 -> if x0 < x then x0, x' else if x0 > x' then x,x0 else acc)
      (ya.(0), ya.(0)) ya in
    ((x,y), (x,y'), (x',y'), (x',y)), max_float

let in_rect (x0,y0) ((x1,y1),(x2,y2),_,(x4,y4)) =
  let ux2 = x2 -. x1 and uy2 = y2 -. y1 in
  let lambda = (ux2 *. (x0 -. x1) +. uy2 *. (y0 -. y1)) /. (ux2 *. ux2 +. uy2 *. uy2) in
  let ux4 = x4 -. x1 and uy4 = y4 -. y1 in
  let mu = (ux4 *. (x0 -. x1) +. uy4 *. (y0 -. y1)) /. (ux4 *. ux4 +. uy4 *. uy4) in
  mu >= 0.0 && mu <= 1.0 && lambda >= 0.0 && lambda <= 1.0

let inter_segment epsilona epsilonb (xa_1, ya_1) (xa_2, ya_2) (xb_1, yb_1) (xb_2, yb_2) =
  let vxa = xa_2 -. xa_1 and vya = ya_2 -. ya_1 in
  let vxb = xb_2 -. xb_1 and vyb = yb_2 -. yb_1 in
  let abx = xb_1 -. xa_1 and aby = yb_1 -. ya_1 in
  
      (* b_1 + lambda v_b = a_1 + mu v_a
         det(ab_1,v_b) = mu det(v_a,v_b)
         det(ab_1,v_a) = lambda det(v_a,v_b) *)
  let denom = det vxa vya vxb vyb in
(*  Printf.eprintf "va = (%f, %f), vb = (%f, %f), ab = (%f, %f), denom = %f\n%!" vxa vya vxb vyb abx aby denom;*)
  if abs_float denom < epsilona *. epsilonb then raise Not_found
  else (
    let mu = det abx aby vxb vyb /. denom in 
    let lambda = det abx aby vxa vya /. denom in
(*    Printf.eprintf "mu = %f, lambda = %f\n%!" mu lambda;*)
    if mu >= -. epsilona && mu <= 1.0 +. epsilona && lambda >= -. epsilonb && lambda <= 1.0 +. epsilonb then
      mu, lambda
    else
      raise Not_found)

let test_inter_segment epsilona epsilonb (xa_1, ya_1) (xa_2, ya_2) (xb_1, yb_1) (xb_2, yb_2) =
  let vxa = xa_2 -. xa_1 and vya = ya_2 -. ya_1 in
  let vxb = xb_2 -. xb_1 and vyb = yb_2 -. yb_1 in
  let abx = xb_1 -. xa_1 and aby = yb_1 -. ya_1 in
  
      (* b_1 + lambda v_b = a_1 + mu v_a
         det(ab_1,v_b) = mu det(v_a,v_b)
         det(ab_1,v_a) = lambda det(v_a,v_b) *)
  let denom = det vxa vya vxb vyb in
  if abs_float denom < epsilona *. epsilonb then false
  else 
    let mu = det abx aby vxb vyb /. denom in 
    let lambda = det abx aby vxa vya /. denom in
     mu >= -. epsilona && mu <= 1.0 +. epsilona && lambda >= -. epsilonb && lambda <= 1.0 +. epsilonb

let area2_rect ((x0,y0),(x1,y1),(x2,y2),_) =
  ((y1 -. y0) ** 2. +. (x1 -. x0) ** 2.) +.
  ((y1 -. y2) ** 2. +. (x1 -. x2) ** 2.)

let intersection ?(epsilon=1e-6) ?(thick=1e-4) (xa, ya as ba) (xb, yb as bb) =
  let da = Array.length xa in
  let db = Array.length xb in
  let epsilon2 = epsilon ** 2.0 in
  let add (x,y as p) acc = 
      match acc with
	(x',y')::acc' -> if  (x' -. x) ** 2.0 +. (y' -. y) ** 2.0 < epsilon2 then acc else p::acc
      | [] -> [p]
  in
  let rec fn acc alpha_a beta_a xa ya ra ta alpha_b beta_b xb yb rb tb = 
(*    Printf.printf "alpha_a = %f (%f), alpha_b = %f (%f)\n%!" alpha_a beta_a alpha_b beta_b;*)
    let xa_1 = xa.(0) and ya_1 = ya.(0) in
    let xa_2 = xa.(da-1) and ya_2 = ya.(da-1) in
    
    let xb_1 = xb.(0) and yb_1 = yb.(0) in
    let xb_2 = xb.(db-1) and yb_2 = yb.(db-1) in

    (* valeur choisie pour espérer garantir la détection des doublons ... *)
    let ea = epsilon *. beta_a /. 3.0 in
    let eb = epsilon *. beta_b /. 3.0 in
    if ta < thick && tb < thick then (
(*      Printf.eprintf "not thick\n%!";*)
      try
	let mu, lambda = inter_segment ea eb (xa_1, ya_1) (xa_2, ya_2) (xb_1, yb_1) (xb_2, yb_2) in
	let r = (alpha_a +. mu *. beta_a, alpha_b +. lambda *. beta_b) in
	add r acc
      with Not_found -> acc)
    else 
    let aa = area2_rect ra and ab = area2_rect rb in
    if ea < epsilon2 && eb < epsilon2 then
      add (xa_1, ya_1) acc
    else (
(*      Printf.eprintf "thick\n%!";*)
      let p1,p2,p3,p4 = ra in
(*      Printf.printf "Rectangle a: (%f,%f) (%f,%f) (%f,%f) (%f,%f)\n%!"
        (fst p1) (snd p1) (fst p2) (snd p2) (fst p3) (snd p3) (fst p4) (snd p4);*)
      let q1,q2,q3,q4 = rb in
(*      Printf.printf "Rectangle b: (%f,%f) (%f,%f) (%f,%f) (%f,%f)\n%!"
	(fst q1) (snd q1) (fst q2) (snd q2) (fst q3) (snd q3) (fst q4) (snd q4);*)
      if not (
	  in_rect p1 rb || in_rect q1 ra ||
	  test_inter_segment ea eb p1 p2 q1 q2 ||
	  test_inter_segment ea eb p1 p2 q2 q3 ||
	  test_inter_segment ea eb p1 p2 q3 q4 ||
	  test_inter_segment ea eb p1 p2 q4 q1 ||
	  test_inter_segment ea eb p2 p3 q1 q2 ||
	  test_inter_segment ea eb p2 p3 q2 q3 ||
	  test_inter_segment ea eb p2 p3 q3 q4 ||
	  test_inter_segment ea eb p2 p3 q4 q1 ||
	  test_inter_segment ea eb p3 p4 q1 q2 ||
	  test_inter_segment ea eb p3 p4 q2 q3 ||
	  test_inter_segment ea eb p3 p4 q3 q4 ||
	  test_inter_segment ea eb p3 p4 q4 q1 ||
	  test_inter_segment ea eb p4 p1 q1 q2 ||
	  test_inter_segment ea eb p4 p1 q2 q3 ||
	  test_inter_segment ea eb p4 p1 q3 q4 ||
	  test_inter_segment ea eb p4 p1 q4 q1)
      then (
(*	  Printf.eprintf "no inter\n%!";*)
	  acc)
      else

	if aa > ab then (
(*	  Printf.eprintf "subdivise a\n%!";*)
	  let (xa1,ya1 as b1) = (restrict xa 0.0 0.5, restrict ya 0.0 0.5) in
	  let (xa2,ya2 as b2) = (restrict xa 0.5 1.0, restrict ya 0.5 1.0) in
	  let ra1, ta1 = thickness2 b1 and ra2, ta2 = thickness2 b2 in
	  let beta_a = beta_a /. 2. in
	  fn (fn acc alpha_a beta_a xa1 ya1 ra1 ta1 alpha_b beta_b xb yb rb tb)
	    (alpha_a +. beta_a) beta_a xa2 ya2 ra2 ta2 alpha_b beta_b xb yb rb tb
	) else (
(*	  Printf.eprintf "subdivise b\n%!";*)
	  let (xb1,yb1 as b1) = (restrict xb 0.0 0.5, restrict yb 0.0 0.5) in
	  let (xb2,yb2 as b2) = (restrict xb 0.5 1.0, restrict yb 0.5 1.0) in
	  let rb1, tb1 = thickness2 b1 and rb2, tb2 = thickness2 b2 in
	  let beta_b = beta_b /. 2. in
	  fn (fn acc alpha_a beta_a xa ya ra ta alpha_b beta_b xb1 yb1 rb1 tb1)
	    alpha_a beta_a xa ya ra ta (alpha_b +. beta_b) beta_b xb2 yb2 rb2 tb2
	))
  in
  let ra, ta = thickness2 ba and rb, tb = thickness2 bb in
  let r = fn [] 0.0 1.0 xa ya ra ta 0.0 1.0 xb yb rb tb in
(*  let print_maple r r' =
    Printf.printf "[";
    Array.iteri (fun i x -> Printf.printf "(%f,%f)" x r'.(i)) r;
    Printf.printf "]";
  in
  print_maple xa ya;
  print_newline ();
  print_maple xb yb;
  print_newline ();
  print_endline "===>";
  List.iter (fun (x,y) -> Printf.printf "(%f,%f) " x y) r;
  print_newline ();*)
  r


let pi = 4. *. atan 1.
let rotate angle  (xs,ys) = (* angle en degres *)
  let angle = angle *. pi /. 180. in
  Array.mapi (fun i x -> x *. cos angle -. ys.(i) *. sin angle) xs,
  Array.mapi (fun i y -> xs.(i) *. sin angle +. y *. cos angle) ys

    
