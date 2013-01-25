
type projection = {  
  (* distance du foyer de la projection > 0 et >> à tous les z de la figure *)
  distance_focale : float;
  (* coordonnée du foyer en x et y et aussi centre pour les axes de rotation *)
  origin_transfo_x : float;
  origin_transfo_y : float;
  (* point qui aura les coordonnées 0, 0 après transformation *) 
  origin_diag : (float * float * float);
  (* un facteur d'homotétie *)
  homotetie : float;
  (* angle de l'axe de rotation dans le plan z = 0 *)
  axe_angle : float;
  (* angle de la rotation *)
  rot_angle : float;
}
    
(* perspectiva cavaliere *)
(* 45bg = angle à 45 degré, face avant en bas à gauche *)

let cavaliere45bg = {
  distance_focale = 1e4;
  origin_transfo_x = 4e3;
  origin_transfo_y = 4e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

let cavaliere60bg = {
  distance_focale = 1e4;
  origin_transfo_x = 5e3;
  origin_transfo_y = 3e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

let cavaliere45bd = {
  distance_focale = 1e4;
  origin_transfo_x = -4e3;
  origin_transfo_y = 4e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

let cavaliere60bd = {
  distance_focale = 1e4;
  origin_transfo_x = -5e3;
  origin_transfo_y = 3e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

let cavaliere45hg = {
  distance_focale = 1e4;
  origin_transfo_x = 4e3;
  origin_transfo_y = -4e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

let cavaliere60hg = {
  distance_focale = 1e4;
  origin_transfo_x = 5e3;
  origin_transfo_y = -3e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

let cavaliere45hd = {
  distance_focale = 1e4;
  origin_transfo_x = -4e3;
  origin_transfo_y = -4e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

let cavaliere60hd = {
  distance_focale = 1e4;
  origin_transfo_x = -5e3;
  origin_transfo_y = -3e3;
  origin_diag = (5.,5.,5.);
  axe_angle = 0.0;
  rot_angle = 0.0;
  homotetie = 1.0;
}

(* perspectiva naturelle avec rotation à droite ou à gauche autour
   de l'axe des y. L'ajustement du centre est impératif *)

let rot_y45d = {
   distance_focale = 55.;
   origin_transfo_x = 0.;
   origin_transfo_y = 0.;
   origin_diag = (0.,0.,0.);
   axe_angle = 0.;
   rot_angle = 3.1416 /. 4.;
   homotetie = 1.0;
}

let rot_y30d = {
   distance_focale = 55.;
   origin_transfo_x = 0.;
   origin_transfo_y = 0.;
   origin_diag = (0.,0.,0.);
   axe_angle = 0.;
   rot_angle = 3.1416 /. 6.;
   homotetie = 1.0;
}
  
let rot_y45g = {
   distance_focale = 55.;
   origin_transfo_x = 0.;
   origin_transfo_y = 0.;
   origin_diag = (0.,0.,0.);
   axe_angle = 0.;
   rot_angle = -3.1416 /. 4.;
   homotetie = 1.0;
}

let rot_y30g = {
   distance_focale = 55.;
   origin_transfo_x = 0.;
   origin_transfo_y = 0.;
   origin_diag = (0.,0.,0.);
   axe_angle = 0.;
   rot_angle = -3.1416 /. 6.;
   homotetie = 1.0;
}  

let project projection (x,y,z) =
  let f (x,y,z) =
    let x = x -. projection.origin_transfo_x and y = y -. projection.origin_transfo_y in
    let x = projection.homotetie *. x and  y = projection.homotetie *. y and z = projection.homotetie *. z in
    let x = cos(projection.axe_angle) *. x +. sin(projection.axe_angle) *. y and y = -. sin(projection.axe_angle) *. x +. cos(projection.axe_angle) *. y in
    let x = cos(projection.rot_angle) *. x +. sin(projection.rot_angle) *. z and z = -. sin(projection.rot_angle) *. x +. cos(projection.rot_angle) *. z in
    let x = cos(projection.axe_angle) *. x -. sin(projection.axe_angle) *. y and y = sin(projection.axe_angle) *. x +. cos(projection.axe_angle) *. y in
    let c = projection.distance_focale /. (projection.distance_focale -. z) in
    let x = x *. c and y = y  *. c in
    (x, y)
  in
  let (x,y) = f (x,y,z) in
  let (x0,y0) = f projection.origin_diag in
  (x -. x0, y -. y0)
