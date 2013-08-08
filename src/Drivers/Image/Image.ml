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
open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util
open Rgba32
open Color
open Language

let format = ref "png"
let width = ref None
let height = ref None
let saa = ref DriverGL.No_SAA

let spec = [
  ("--format",Arg.String (fun s -> format := s), message (Language.Image IFormat));
  ("--width",Arg.Int (fun i -> width := Some i), message (Language.Image Width));
  ("--height",Arg.Int (fun i -> height := Some i), message (Language.Image Height));
  ("--rgb",Arg.Unit (fun () -> saa := DriverGL.RGB_SAA), message (Language.Image RGB));
  ("--bgr",Arg.Unit (fun () -> saa := DriverGL.BGR_SAA), message (Language.Image BGR));
  ("--vrgb",Arg.Unit (fun () -> saa := DriverGL.VRGB_SAA), message (Language.Image VRGB));
  ("--vbgr",Arg.Unit (fun () -> saa := DriverGL.VBGR_SAA), message (Language.Image VBGR));
]
 
let filename file = try (Filename.chop_extension file)^"_img_dir" with _->file^"_img_dir"



let filename' file i j = 
  try "page_"^string_of_int i^"."^ !format with _->"page_"^string_of_int i^"_"^string_of_int j^"."^ !format


let _ =
  Arg.parse spec (fun x-> raise (Arg.Bad x)) (Language.message Usage)

let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  Printf.printf "format: %s\n" !format;
  let dirname = filename fileName in
  if not (Sys.file_exists dirname) then
    Unix.mkdir dirname 0o777;

  let generate get_pixes =
    Printf.fprintf stderr "generate\n";flush stderr;
    let pages = get_pixes 1 !width !height !saa in
    Printf.printf "Images generated\n";
    Array.iteri (fun page states -> Array.iteri (
      fun state (raw,w,h) ->
	let image = Rgba32.create w h in 
	for j=0 to h-1 do	  
          for i=0 to w-1 do
	    let r = Raw.get raw ((j * w + i) * 4 + 0) in
	    let g = Raw.get raw ((j * w + i) * 4 + 1) in
	    let b = Raw.get raw ((j * w + i) * 4 + 2) in
	    let a = Raw.get raw ((j * w + i) * 4 + 3) in
	    let c = { color = { r = r; g = g; b = b }; alpha = a } in
(*	    Printf.printf "%d %d %d %d\n" r g b a;*)
	    Rgba32.set image i (h-1-j) c
	  done
	done;
	let fname = Filename.concat dirname (filename' fileName page state) in
	Printf.fprintf stderr "Wrinting %s\n" fname;
	Images.save fname None [] (Images.Rgba32 image)) states) pages;
    ()
  in

  DriverGL.prefs := { !DriverGL.prefs with DriverGL.batch_cmd = Some generate; DriverGL.server_port = None };

  DriverGL.output ~structure pages fileName

let output' = output_to_prime output
