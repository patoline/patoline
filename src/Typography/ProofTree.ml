open Maths
open Document
open Document.Mathematical
open OutputCommon
open Box 
open Util

let rec spacing right left = 
  let above y l = match l with
      [] -> false
    | (_, y')::_ -> y >= y'
  in

  let rec fn right left = 
    match right, left with
      (x, y)::l, (x', y')::l' ->
	if above y l' then
	  fn right l'
	else if above y' l then
	  fn l left
	else
	  let d = x -. x' in
	  let d' = if y <= y' then fn l left else fn right l' in
	  max d d'
    | _ -> -. max_float

  in
  (*Printf.printf "left: ";
  List.iter (fun (x,y) -> Printf.printf "(%f,%f) " x y) left;
  print_newline ();
  Printf.printf "right: ";
  List.iter (fun (x,y) -> Printf.printf "(%f,%f) " x y) right;
  print_newline ();*)
  let r = fn right left in
  (*Printf.printf " ==> %f\n" r; print_newline ();*)
  r

let htr l dx = List.map (fun (x,y) -> (x +. dx, y)) l
let vtr l dy = List.map (fun (x,y) -> (x, y +. dy)) l

type 'a proof = 
    | Hyp of 'a
    | Rule of 'a proof list * 'a * 'a option (* premices, conclusion, rule name *)

let axiom x = Rule([], x, None)
let axiom' n x = Rule([], x, Some n)

let hyp x = Hyp x

let unary p c = Rule([p], c, None)
let unary' n p c = Rule([p], c, Some n)

let binary p p' c = Rule([p;p'], c, None)
let binary' n p p' c = Rule([p;p'], c, Some n)

let ternary p p' p'' c = Rule([p;p';p''], c, None)
let ternary' n p p' p'' c = Rule([p;p';p''], c, Some n)

let n_ary l c = Rule(l, c, None)
let n_ary' n l c = Rule(l, c, Some n)

module ProofTree = struct

  type 'a t = 'a proof

  let rec map = fun f e s p -> match p with
      Hyp h -> Hyp (f e s h)
    | Rule(premices, conclusion, name) ->
	Rule(
	  List.map (map f e s) premices, 
	  f e s conclusion,
	  match name with None -> None | Some n -> Some (f e s n))

  let draw env_ style proof =
    let env = env_style env_.mathsEnvironment style in
    let s=env.mathsSize*.env_.size in
    let ln = s*.0.05 in
    let sb = s*.0.3 in
    let sa = s*.0.3 in
    let rec fn proof =
      match proof with
	Hyp hyp ->
	  let hyp_box = draw_boxes env_ hyp in
	  let cx0, cy0, cx1, cy1 = bounding_box hyp_box in
	  let h = cy1 -. cy0 in
	  h, [cx0, 0.0; cx0, h], cx0, [cx1, 0.0; cx1, h], cx1, hyp

      | Rule(premices, conclusion, name) ->
	  let premices_box = List.map 
	    (fun x -> let (a,b,c,d,e,f) = fn x in
		      (a,b,c,d,e,draw_boxes env_ f))
	    premices
	  in
	  let conclusion_box = draw_boxes env_ conclusion in
	  let name_box = match name with None -> [] | Some name -> draw_boxes env_ name in
	  
	  let cx0, cy0, cx1, cy1 = bounding_box conclusion_box in
	  
	  let rec gn dx = function
  	      [] -> 0.0, [], max_float, [], -. max_float, [] 
	    | [h, left, mleft, right, mright, drawing] ->
	        h, htr left dx, mleft +. dx, htr right dx, mright +. dx,
	        List.map (translate dx 0.0) drawing
	    | (h, left, mleft, right, _, drawing)::((_, left', _, _, _, _)::_ as l) ->
	      let sp = spacing right left' +. 1.0 *. s in
	      let (h', _, _, right', mright', drawing') = gn (dx +. sp) l in
	      max h h', htr left dx, mleft +. dx, right', mright',
	      (List.map (translate dx 0.0) drawing @ drawing')
	  in
	  
	  let h, left, mleft, right, mright, numerator = gn 0.0 premices_box in
	  
	  let nx0 = match left with [] -> cx0 | (x,_)::_ -> x in
	  let nx1 = match right with [] -> cx1 | (x,_)::_ -> x in

	  let dx = (-. (cx1 +. cx0) +. (nx1 +. nx0)) /. 2.0 in
	  let cx0 = cx0 +. dx in
	  let cx1 = cx1 +. dx in
	  let rx0 = min cx0 nx0 in
	  let rx1 = max cx1 nx1 in
	  
	  let dy = cy1  -. cy0 +. sb +. ln +. sa in

	  let contents _ = 
	    [Path ({OutputCommon.default with lineWidth=ln}, [ [|line (rx0,cy1 +. sb) (rx1, cy1 +. sb)|] ]) ] @
	      (List.map (translate dx 0.0) conclusion_box) @
	      (List.map (translate 0.0 dy) numerator)
	  in

	  let left = (cx0, 0.0) :: (rx0, cy1  -. cy0) :: vtr left dy in
	  let right = (cx1, 0.0) :: (rx1, cy1  -. cy0) :: vtr right dy in

	  let mleft = min rx0 mleft in
	  let mright = max rx1 mright in
	  let w = mright -. mleft in

	  let h = h +. dy in

	  let final = 
	    [Drawing ({ drawing_min_width=w;
                       drawing_nominal_width=w;
                       drawing_max_width=w;
		       drawing_width_fixed = true;
		       drawing_adjust_before = false;
                       drawing_y0=cy0;
                       drawing_y1=cy0 +. h;
                       drawing_badness=(fun _->0.);
                       drawing_break_badness=0.;
                       drawing_states=IntSet.empty;
                       drawing_contents = contents })]

	  in

	  (h, left, mleft, right, mright, final)
    in
    let _, _, _, _, _, r = fn proof in
    r
  
end

let proofTree x = 
  let module M = Mk_Custom(ProofTree) in
  [M.custom x]
