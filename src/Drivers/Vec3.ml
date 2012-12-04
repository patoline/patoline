(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

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
let add (x,y,z) (x',y',z') =
  x +. x', y +. y', z+. z'

let sub (x,y,z) (x',y',z') =
  x -. x', y -. y', z -. z'

let dot(x,y,z) (x',y',z') =
  x *. x'+. y *. y'+. z *. z'

let norm(x,y,z) =
  sqrt (dot (x,y,z) (x,y,z))

let dist u v = norm (sub u v)

let zero = 0., 0., 0.
 
let mul a (x,y,z) = a *. x, a *. y, a *. z

let normalize v = mul (1.0 /. norm v) v

let vecp(x,y,z) (x',y',z') =
  (z' *. y -. z *. y',
   x' *. z -. x *. z',
   y' *. x -. x' *. y)

let pi = acos(0.0) *. 2.0
