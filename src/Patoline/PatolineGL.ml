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
open Arg
open GL

let spec = [
  "--rgb", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = RGB_SAA })
         , "Set subpixel anti aliasing for RGB lcd screens (default)";
  "--bgr", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = BGR_SAA })
         , "Set subpixel anti aliasing for BRG lcd screens";
  "--vrgb", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = VRGB_SAA })
         , "Set subpixel anti aliasing for VRGB lcd screens";
  "--vbgr", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = VBGR_SAA })
         , "Set subpixel anti aliasing for VBRG lcd screens";
  "--no-saa", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = No_SAA })
         , "Do not use subpixel anti aliasing";
  "--graisse", Float (fun x -> prefs := { !prefs with graisse = x })
         , (Printf.sprintf "Fixes the thickness of rendering (default: %.2f pixel), warning : -1 and 1 are big" !prefs.graisse);
  "--tesselation-factor", Float (fun x -> prefs := { !prefs with tesselation_factor = x })
         , (Printf.sprintf "Fixes the tessalation precision in pixels (default: %.2f)" !prefs.graisse);
  "--fit-width", Unit (fun () -> prefs := { !prefs with init_zoom = FitWidth })
         , "Start with fitting the page width in the window";
  "--fit-height", Unit (fun () -> prefs := { !prefs with init_zoom = FitHeight })
         , "Start with fitting the page height in the window";
  "--fit-page", Unit (fun () -> prefs := { !prefs with init_zoom = FitHeight })
         , "Start with fitting the page in the window";
  "--rotation", Float (fun x -> prefs := { !prefs with rotation = Some x })
         , "Animate page change with a rotation of the given duration (in second)";
  "--port-cmd", Int (fun p -> prefs := { !prefs with server_port = Some p })
         , "Give the port to control patolineGL (default 8080)" ;
  "--no-cmd", Int (fun p -> prefs := { !prefs with server_port = None })
         , "Do not allow remote control for patolineGL"
]

let files = ref []

let _ = 
  prefs :={ !prefs with server_port = Some 8080 };
  Arg.parse spec (fun x->files := x::(!files)) (Printf.sprintf "Usage : %s [options] file.bin" Sys.argv.(0));
  match !files with
    [f] ->
      let ch = open_in f in
      let prime = input_value ch in
      let structure = input_value ch in
      if prime then (
	let pages = input_value ch in
	close_in ch;
	output' ~structure pages f)
      else (
	let pages = input_value ch in
	close_in ch;
	output ~structure pages f)
  | _ ->
    Printf.fprintf stderr "%s: more than one file given!" Sys.argv.(0)



