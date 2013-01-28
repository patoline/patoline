(* geométrie de base *)

module Geo2d = struct
  let (++) (x,y) (x',y') = (x+.x',y+.y')
  let (--) (x,y) (x',y') = (x-.x',y-.y')
  let ( *+ )  (x,y) (x',y') = x*.x' +. y*.y'
  let ( ** )  l (x,y) = (l*.x, l*.y)
  let norm2 v = v *+ v
  let norm v = sqrt (norm2 v)
  let normalize v = (1. /. norm v) ** v
  let det (x,y) (x',y') = x *. y' -. x' *. y
end


module Geo3d = struct
  let (++) (x,y,z) (x',y',z') = (x+.x',y+.y',z+.z')
  let (--) (x,y,z) (x',y',z') = (x-.x',y-.y',z-.z')
  let ( *+ )  (x,y,z) (x',y',z') = x*.x' +. y*.y' +. z*.z'
  let ( ** )  l (x,y,z) = (l*.x, l*.y, l*.z)
  let ( *^ )  (x,y,z) (x',y',z') = (
    y *. z' -. y' *. z,
    x' *. z -. x *. z',
    x *. y' -. x' *. y)
  let norm2 v = v *+ v
  let norm v = sqrt (norm2 v)
  let normalize v = (1. /. norm v) ** v
  let det u v w = (u *^ v) *+ w
end

