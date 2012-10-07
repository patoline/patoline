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
let saa = ref GL.No_SAA

let spec = [
  ("--format",Arg.String (fun s -> format := s), message (Language.Image IFormat));
  ("--width",Arg.Int (fun i -> width := Some i), message (Language.Image Width));
  ("--height",Arg.Int (fun i -> height := Some i), message (Language.Image Height));
  ("--rgb",Arg.Unit (fun () -> saa := GL.RGB_SAA), message (Language.Image RGB));
  ("--bgr",Arg.Unit (fun () -> saa := GL.BGR_SAA), message (Language.Image BGR));
  ("--vrgb",Arg.Unit (fun () -> saa := GL.VRGB_SAA), message (Language.Image VRGB));
  ("--vbgr",Arg.Unit (fun () -> saa := GL.VBGR_SAA), message (Language.Image VBGR));
]
 
let filename file = try (Filename.chop_extension file)^"_img_dir" with _->file^"_img_dir"



let filename' file i = 
  try "page_"^string_of_int i^"."^ !format with _->"page_"^string_of_int i^"."^ !format


let _ =
  Arg.parse spec (fun x-> raise (Arg.Bad x)) (Language.message Usage)

let output ?(structure:structure={name="";displayname=[];metadata=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  Printf.printf "format: %s\n" !format;
  let dirname = filename fileName in
  if not (Sys.file_exists dirname) then
    Unix.mkdir dirname 0o777;

  let generate get_pixes =
    let pages = get_pixes 1 !width !height !saa in
    Printf.printf "Images generated\n";
    Array.iteri (
      fun page (raw,w,h) ->
	let image = Rgb24.create w h in 
	for j=0 to h-1 do	  
          for i=0 to w-1 do
	    let r = Raw.get raw ((j * w + i) * 4 + 0) in
	    let g = Raw.get raw ((j * w + i) * 4 + 1) in
	    let b = Raw.get raw ((j * w + i) * 4 + 2) in
	    let c = { r = r; g = g; b = b } in
(*	    Printf.printf "%d %d %d %d\n" r g b a;*)
	    Rgb24.set image i (h-1-j) c
	  done
	done;
	let fname = Filename.concat dirname (filename' fileName page) in
	Printf.fprintf stderr "Wrinting %s\n" fname;
	Images.save fname None [] (Images.Rgb24 image);) pages;
    ()
  in

  GL.prefs := { !GL.prefs with GL.batch_cmd = Some generate };

  GL.output ~structure pages fileName

  
