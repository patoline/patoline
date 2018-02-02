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

open Extra

(** [pt_of_mm l] converts [l] from Adobe points to millimeters. *)
let pt_of_mm : float -> float = fun x -> (72.0 *. x) /. 25.4

(** [mm_of_pt l] converts [l] from millimeters to Adobe points. *)
let mm_of_pt : float -> float = fun x -> (25.4 *. x) /. 72.0

(** Width and height of the A4 page format (in millimeters). *)
let a4 : float * float = (210.0, 297.0)

(** Golden ratio. *)
let phi : float = (1.0 +. sqrt 5.0) /. 2.0

let readInt f n0 =
  let rec aux n x =
    if n = n0 then x
    else aux (n+1) ((x lsl 8) + (input_byte f))
  in
  aux 0 0

let int16 x=if x<0x8000 then x else x-0x10000

let is_space x0=
  let x=UChar.code x0 in
    (x>=0x0009 && x<=0x000d)
  || x=0x0020
  || x=0x00a0
  || x=0x1680
  || x=0x180e
  || (x>=0x2000 && x<=0x200a)
  || x=0x202f
  || x=0x205f
  || x=0x3000

let unspace s=
  let rec rem0 i=
    if is_space (UTF8.look s i) then
      rem0 (UTF8.next s i)
    else
      i
  in
  let rec rem1 j=
    if is_space (UTF8.look s j) then
      rem1 (UTF8.prev s j)
    else
      UTF8.next s j
  in
  let a=rem0 0 and b=rem1 (UTF8.last s) in
  String.sub s a (b-a)

let copy_file a b=
  let fa = open_in a in
  let fb = open_out b in
  let s = Bytes.create 1000 in
  let rec copy () =
    let x = input fa s 0 1000 in
    if x > 0 then (output fb s 0 x; copy ())
  in
  copy ();
  close_in fa;
  close_out fb

let btimer=ref StrMap.empty
let timer name f=
  let t0=Sys.time () in
  let x=f () in
  let t1=Sys.time () in
  let t=try StrMap.find name !btimer with Not_found->0. in
  btimer:= StrMap.add name (t+.t1-.t0) !btimer;
  StrMap.iter (fun k a->Printf.fprintf stderr "time %S: %g\n" k a) !btimer;flush stderr;
  x

(* a lighter split that calling str *)
let split char str =
  let len = String.length str in
  let rec fn beg pos acc =
    if pos >= len then List.rev (String.sub str beg (pos - beg)::acc)
    else if str.[pos] = char then fn (pos+1) (pos+1)  (String.sub str beg (pos - beg)::acc)
    else fn beg (pos+1) acc
  in
  fn 0 0 []

(* alternative implementation
let split c s =
  let rec split s acc =
    try
      let i = String.index s c in
      let e = String.sub s 0 i in
      let s' = String.sub s (i+1) (String.length s - i - 1) in
      split s' (e :: acc)
    with _ -> List.rev (s :: acc)
  in
  split s []
*)

(* A type needed both by Db and RawContent *)
type visibility = Private | Group | Public
let vis_to_string = function
  | Private -> "Private"
  | Group -> "Group"
  | Public -> "Public"
