open Maths
open Document
open Document.Mathematical
open OutputCommon
open Box 
open Util
open Fonts.FTypes

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
    | SubProof of 'a proof list * 'a * 'a option (* premices, conclusion, proof name *)

type paramProofTree = 
  { spaceUnderRule : float;
    spaceAboveRule : float;
    minSpaceAboveRule : float;
    thicknessRule : float;
    heightName : float;
    spaceBeforeName : float;
    spaceBetweenProof : float;
    extraRule : float;
    heightSubProof : float;
    noPremiseTopLineIncrease : float;
  }

let proofTreeDefault = 
  { spaceUnderRule = 0.4;
    spaceAboveRule = 0.4;
    minSpaceAboveRule = 0.05;
    thicknessRule = 0.1;
    heightName = 0.4;
    spaceBeforeName = 0.15;
    spaceBetweenProof = 1.5;
    extraRule = 0.1;
    heightSubProof = 4.0;
    noPremiseTopLineIncrease = 0.6;
  }

module ProofTree = struct

  type 'a t = paramProofTree * 'a proof

  let rec map' = fun f e s p -> match p with
      Hyp h -> Hyp (f e s h)
    | Rule(premices, conclusion, name) ->
        Rule(
          List.map (map' f e s) premices, 
          f e s conclusion,
          match name with None -> None | Some n -> Some (f e (cramp (scriptStyle s)) n))
    | SubProof(premices, conclusion, name) ->
        SubProof(
          List.map (map' f e s) premices, 
          f e s conclusion,
          match name with None -> None | Some n -> Some (f e (cramp (scriptStyle s)) n))

  let map f e s (param, prf) = (param, map' f e s prf)

  let draw env_ style (param, proof) =
    let env = env_style env_.mathsEnvironment style in
    let env' = env_style env_.mathsEnvironment (cramp (scriptStyle style)) in

    let heightx =
      let x=Fonts.loadGlyph (Lazy.force env.mathsFont)
        ({empty_glyph with glyph_index=Fonts.glyph_of_char (Lazy.force env.mathsFont) 'x'}) in
      env.mathsSize*.env_.size*.(Fonts.glyph_y1 x)/.1000.
    in
    let widthM, heightM =
      let x=Fonts.loadGlyph (Lazy.force env.mathsFont)
        ({empty_glyph with glyph_index=Fonts.glyph_of_char (Lazy.force env.mathsFont) 'M'}) in
      env.mathsSize*.env_.size*.(Fonts.glyph_x1 x-.Fonts.glyph_x0 x)/.1000.,
      env.mathsSize*.env_.size*.(Fonts.glyph_y1 x-.Fonts.glyph_y0 x)/.1000.
    in
    let widthSubM, heightSubM =
      let x=Fonts.loadGlyph (Lazy.force env'.mathsFont)
        ({empty_glyph with glyph_index=Fonts.glyph_of_char (Lazy.force env'.mathsFont) 'M'}) in
      env'.mathsSize*.env_.size*.(Fonts.glyph_x1 x-.Fonts.glyph_x0 x)/.1000.,
      env'.mathsSize*.env_.size*.(Fonts.glyph_y1 x-.Fonts.glyph_y0 x)/.1000.
    in

    let ln = heightM *. param.thicknessRule in
    let sb = heightM *. param.spaceUnderRule in
    let sa = heightM *. param.spaceAboveRule in
    let hn = heightSubM *. param.heightName in
    let sn = widthSubM *. param.spaceBeforeName in
    let sp = widthM *. param.spaceBetweenProof in
    let er = widthM *. param.extraRule in

    let rec fn top proof =
      match proof with
        Hyp hyp ->
          let hyp_box = draw_boxes env_ hyp in
          let cx0, cy0, cx1, cy1 = bounding_box hyp_box in
          let h = cy1 (* -. cy0 *) in
          h, [cx0, cy0; cx0, h], cx0, [cx1, cy0; cx1, h], cx1, hyp

      | Rule(premices, conclusion, name) ->
          let premices_box = List.map 
            (fun x -> let (a,b,c,d,e,f) = fn false x in
                      (a,b,c,d,e,draw_boxes env_ f))
            premices
          in
          let conclusion_box = draw_boxes env_ conclusion in
          let sn, name_box = match name with
              None -> 0.0, [] 
            | Some name -> sn, draw_boxes env_ name
          in

          let namex0, namey0, namex1, namey1 = bounding_box name_box in
          let cx0, cy0, cx1, cy1 = bounding_box conclusion_box in
          
          let extract_y0 l = 
            match l with
              [] -> 0.0
            | (_,y0)::_ -> y0
          in 

          let rec gn y0min dx = function
                [] -> y0min, (0.0, [], max_float, [], -. max_float, [])
            | [h, left, mleft, right, mright, drawing] ->
              let y0min' = min y0min (extract_y0 left) in 
              (y0min', (
                h, htr left dx, mleft +. dx, htr right dx, mright +. dx,
                List.map (translate dx 0.0) drawing))
            | (h, left, mleft, right, mright, drawing)::((_, left', _, _, _, _)::_ as l) ->
              let mleft = mleft +. dx and mright = mright +. dx in
              let sp = spacing right left' +. sp in
              let y0min' = min y0min (extract_y0 left) in 
              let y0min'', 
                (h', _, mleft', right', mright', drawing') = gn y0min' (dx +. sp) l in
              (y0min'', (              
              max h h', htr left dx, min mleft mleft', right', max mright mright',
              (List.map (translate dx 0.0) drawing @ drawing')))
          in
          
          let y0min, (h, left, mleft, right, mright, numerator) = gn 0.0 0.0 premices_box in
          
          let nx0 = match left with [] -> cx0 | (x,_)::_ -> x in
          let nx1 = match right with [] -> cx1 | (x,_)::_ -> x in

          let dx = (-. (cx1 +. cx0) +. (nx1 +. nx0)) /. 2.0 in
          let cx0 = cx0 +. dx in
          let cx1 = cx1 +. dx in
          let rx0 = min cx0 nx0 -. er in
          let rx1 = max cx1 nx1 +. er in
          
          let sa = max (param.minSpaceAboveRule -. y0min) sa
            (* match left with *)
            (*   [] -> 0.0 *)
            (* | (_,y0)::_ -> max (param.minSpaceAboveRule -. y0) sa *)
          in
          let dy = cy1 +. sb +. ln +. sa in
          
          let dnx = rx1 +. sn in
          let dny = cy1  -. hn +. sb +. ln /. 2.0 in


          let left = (cx0, cy0) :: (rx0, cy1  (* -. cy0*)) :: vtr left dy in
          let right = match name with
              None -> (cx1, cy0) :: (rx1, cy1 (* -. cy0*)) :: vtr right dy
            | Some _ ->
              (cx1, cy0) :: (dnx +. namex1 -. namex0, dny)  :: vtr right dy
          in
          let mleft = min rx0 mleft in
          let mright = max (if name = None then rx1 else rx1 +. sn +. namex1) mright in
          let w = mright -. mleft in

          let h = h +. dy in

          let dtop = if top then -. cy1 -. sb -. ln /. 2.0 +. heightx /. 2.0 else 0.0 in

          let contents _ = 
            let l = 
              [Path ({OutputCommon.default with strokingColor=Some env_.fontColor; lineWidth=ln}, [ [|line (rx0,cy1 +. sb) (rx1, cy1 +. sb)|] ]) ] @
                (List.map (translate dx 0.0) conclusion_box) @
                (List.map (translate 0.0 dy) numerator) @
                (List.map (translate dnx dny) name_box)
            in
            if top then List.map (translate (-.mleft) dtop) l else l
          in

          let final = 
            [Drawing ({ drawing_min_width=w;
                       drawing_nominal_width=w;
                       drawing_max_width=w;
                       drawing_width_fixed = true;
                       drawing_adjust_before = false;
                       drawing_y0=cy0 +. dtop;
                       drawing_y1=(* cy0 +. *) h +. dtop;
                       drawing_badness=(fun _->0.);
                       drawing_break_badness=0.;
                       drawing_states=IntSet.empty;
                       drawing_contents = contents })]

          in

          (h, left, mleft, right, mright, final)

      | SubProof(premices, conclusion, name) ->
          (* FIXME Begin work in progress *)
          let premices_box = List.map 
            (fun x -> let (a,b,c,d,e,f) = fn false x in
                      (a,b,c,d,e,draw_boxes env_ f))
            premices
          in
          let conclusion_box = draw_boxes env_ conclusion in

          let extract_y0 l = 
            match l with
              [] -> 0.0
            | (_,y0)::_ -> y0
          in 

          let rec gn y0min dx = function
                [] -> y0min, (0.0, [], max_float, [], -. max_float, [])
            | [h, left, mleft, right, mright, drawing] ->
              let y0min' = min y0min (extract_y0 left) in 
              (y0min', (
                h, htr left dx, mleft +. dx, htr right dx, mright +. dx,
                List.map (translate dx 0.0) drawing))
            | (h, left, mleft, right, mright, drawing)::((_, left', _, _, _, _)::_ as l) ->
              let mleft = mleft +. dx and mright = mright +. dx in
              let sp = spacing right left' +. sp in
              let y0min' = min y0min (extract_y0 left) in 
              let y0min'', 
                (h', _, mleft', right', mright', drawing') = gn y0min' (dx +. sp) l in
              (y0min'', (              
              max h h', htr left dx, min mleft mleft', right', max mright mright',
              (List.map (translate dx 0.0) drawing @ drawing')))
          in
          
          let y0min, (h, left, mleft, right, mright, premices) = gn 0.0 0.0 premices_box in
          let cx0, cy0, cx1, cy1 = bounding_box conclusion_box in

          (* Height of the trapesium *)
          let hsp = heightx *. param.heightSubProof in

          (* X axis extrema for premices, and the top line of the trapesium *)
          let wcx = cx1 -. cx0 in
          let pincr = hsp *. param.noPremiseTopLineIncrease in
          let nx0 = match left with [] -> cx0 -. pincr | (x,_)::_ -> x in
          let nx1 = match right with [] -> cx1 +. pincr | (x,_)::_ -> x in
          let rpx0 = nx0 -. er in
          let rpx1 = nx1 +. er in

          (* Idem for conclusion and bottom line of the trapesium *)
          let dx = (-. (cx1 +. cx0) +. (nx1 +. nx0)) /. 2.0 in
          let cx0 = cx0 +. dx in
          let cx1 = cx1 +. dx in
          let rcx0 = cx0 -. er in
          let rcx1 = cx1 +. er in

          (* Space above top line of the trapesium *)
          let sa = max (param.minSpaceAboveRule -. y0min) sa in

          let dy = cy1 +. sb +. ln +. hsp +. sa in (* 2 x ln/2 *)

          let left = (cx0, cy0) :: (rcx0, cy1) :: (rpx0, mleft) :: vtr left dy in
          let right = (cx1, cy0) :: (rcx1, cy1) :: (rpx1, mright) :: vtr right dy in
          let mleft = min mleft (min rpx0 rcx0) in
          let mright = max mright (max rpx1 rcx1) in
          let h = h +. dy in

          let w = mright -. mleft in

          let dtop = if top then -. cy1 -. sb -. ln /. 2.0 +. heightx /. 2.0 else 0.0 in

          (* Name placing *)
          let name = match name with
              None -> None
            | Some name ->
                let name_box = draw_boxes env_ name in
                let x0, y0, x1, y1 = bounding_box name_box in
                let dx = (nx0 +.nx1) /. 2. -. (x1 -. x0) /. 2. in
                let dy = cy1 +. sb +. hsp /. 2. -. (y1 -. y0) /. 2. in
                Some(List.map (translate dx dy) name_box)
          in
          
          let contents _ = 
            let l = 
              [Path ({OutputCommon.default with strokingColor=Some env_.fontColor; lineWidth=ln}, [ [|line (rcx0,cy1 +. sb) (rcx1, cy1 +. sb)|] ])] @
                [Path ({OutputCommon.default with strokingColor=Some env_.fontColor; lineWidth=ln}, [ [|line (rpx0,cy1 +. hsp) (rpx1, cy1 +. hsp)|] ])] @
                [Path ({OutputCommon.default with strokingColor=Some env_.fontColor; lineWidth=ln}, [ [|line (rpx0,cy1 +. hsp) (rcx0, cy1 +. sb)|] ])] @
                [Path ({OutputCommon.default with strokingColor=Some env_.fontColor; lineWidth=ln}, [ [|line (rcx1,cy1 +. sb) (rpx1, cy1 +. hsp)|] ])] @
                (List.map (translate dx 0.0) conclusion_box) @
                (List.map (translate 0.0 dy) premices) @
                match name with None -> [] | Some n -> n
            in
            if top then List.map (translate (-.mleft) dtop) l else l
          in

          let final = 
            [Drawing ({ drawing_min_width=w;
                       drawing_nominal_width=w;
                       drawing_max_width=w;
                       drawing_width_fixed = true;
                       drawing_adjust_before = false;
                       drawing_y0=cy0 +. dtop;
                       drawing_y1=(* cy0 +. *) h +. dtop;
                       drawing_badness=(fun _->0.);
                       drawing_break_badness=0.;
                       drawing_states=IntSet.empty;
                       drawing_contents = contents })]

          in

          (h, left, mleft, right, mright, final)
          (* FIXME End work in progress *)

    in
    let _, _, _, _, _, r = fn true proof in
    r
  
end

let proofTree ?(param=proofTreeDefault) x = 
  let module M = Mk_Custom(ProofTree) in
  [M.custom (param, x)]

let axiom x = Rule([], x, None)
let axiomN n x = Rule([], x, Some n)
let axiomR x = proofTree (Rule([], x, None))
let axiomRN n x = proofTree (Rule([], x, Some n))

let hyp x = Hyp x

let unary c p = Rule([p], c, None)
let unaryN n c p = Rule([p], c, Some n)
let unaryR p c =  proofTree (Rule([hyp p], c, None))
let unaryRN n p c = proofTree (Rule([hyp p], c, Some n))

let binary c p p' = Rule([p;p'], c, None)
let binaryN n c p p' = Rule([p;p'], c, Some n)
let binaryR p p' c = proofTree (Rule([hyp p; hyp p'], c, None))
let binaryRN n p p' c = proofTree (Rule([hyp p; hyp p'], c, Some n))

let ternary c p p' p'' = Rule([p;p';p''], c, None)
let ternaryN n c p p' p'' = Rule([p;p';p''], c, Some n)
let ternaryR p p' p'' c = proofTree (Rule([hyp p;hyp p';hyp p''], c, None))
let ternaryRN n p p' p'' c = proofTree (Rule([hyp p;hyp p';hyp p''], c, Some n))

let n_ary c l = Rule(l, c, None)
let n_aryN n c l = Rule(l, c, Some n)
let n_aryR c l = proofTree (Rule(List.map hyp l, c, None))
let n_aryRN n c l = proofTree (Rule(List.map hyp l, c, Some n))

let leafSP c = SubProof([], c, None)
let leafSPN n c = SubProof([], c, Some n)
let unarySP c p = SubProof([p], c, None)
let unarySPN n c p = SubProof([p], c, Some n)
let binarySP c p p' = SubProof([p;p'], c, None)
let binarySPN n c p p' = SubProof([p;p'], c, Some n)
let ternarySP c p p' p'' = SubProof([p;p';p''], c, None)
let ternarySPN n c p p' p'' = SubProof([p;p';p''], c, Some n)
let n_arySP c l = SubProof(l, c, None)
let n_arySPN c l n = SubProof(l, c, Some n)
