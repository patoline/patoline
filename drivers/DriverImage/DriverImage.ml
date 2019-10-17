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

open Patoraw
open Driver

let image_format = ref "png"
let width = ref None
let height = ref None
let saa = ref DriverGL.No_SAA

let filter_options = DriverGL.filter_options
let driver_options =
  [ ( "--format"
    , Arg.String (fun s -> image_format := s)
    , "specify the image format" )
  ; ( "--width"
    , Arg.Int (fun i -> width := Some i)
    , "specify image width" )
  ; ( "--height"
    , Arg.Int (fun i -> height := Some i)
    , "specify image height" )
  ; ( "--rgb"
    , Arg.Unit (fun () -> saa := DriverGL.RGB_SAA)
    , "subpixel antialiasing for RGB screen" )
  ; ( "--bgr"
    , Arg.Unit (fun () -> saa := DriverGL.BGR_SAA)
    , "subpixel antialiasing for BGR screen" )
  ; ( "--vrgb"
    , Arg.Unit (fun () -> saa := DriverGL.VRGB_SAA)
    , "subpixel antialiasing for VRGB screen" )
  ; ( "--vbgr"
    , Arg.Unit (fun () -> saa := DriverGL.VBGR_SAA)
    , "subpixel antialiasing for VBGR screen" ) ]
 
let filename file =
  try Filename.chop_extension file ^ "_img_dir" with _ ->
    file ^ "_img_dir"



let filename' _ i j = 
  try "page_" ^ string_of_int i ^ "." ^ !image_format with _ ->
    "page_" ^ string_of_int i ^ "_" ^ string_of_int j ^ "." ^ !image_format
    (* FIXME isn't this dead code? *)


let output ?(structure:structure=empty_structure) pages fileName=

  Printf.printf "format: %s\n" !image_format;
  let dirname = filename fileName in
  if not (Sys.file_exists dirname) then
    Unix.mkdir dirname 0o777;

  let generate get_pixes =
    let pages = get_pixes 1 !width !height !saa in
    Array.iteri (fun page states -> Array.iteri (
      fun state (raw,w,h) ->
        let image = Image.create_rgb ~alpha:true w h in
        for i=0 to w-1 do
          for j=0 to h-1 do
            let r = Raw.get raw ~pos:((j * w + i) * 4 + 0) in
            let g = Raw.get raw ~pos:((j * w + i) * 4 + 1) in
            let b = Raw.get raw ~pos:((j * w + i) * 4 + 2) in
            let a = Raw.get raw ~pos:((j * w + i) * 4 + 3) in
            Image.write_rgba image i (h - j - 1) r g b a
          done
        done;
        let fname = Filename.concat dirname (filename' fileName page state) in
        Printf.fprintf stderr "Writing %s\n" fname;
        ImageLib_unix.writefile fname image) states) pages;
    ()
  in

  DriverGL.prefs :=
    { !DriverGL.prefs with DriverGL.batch_cmd = Some generate
    ; server = None };

  DriverGL.output ~structure pages fileName

let output' = output_to_prime output

let _ =
  Hashtbl.add DynDriver.drivers "Image"
    (module struct
      let output = output
      let output' = output'
     end : OutputDriver)
