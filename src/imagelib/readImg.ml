open Filename
open Image
open ImageUtil
open ImagePNG
open ImagePPM
open ImageXCF
open ImageJPG
open ImageGIF

let convert fn fn' =
  let cmd = Printf.sprintf "convert %s %s" fn fn' in
  let ret = Sys.command cmd in
  if ret <> 0 then assert false

let rm fn =
  let cmd = Printf.sprintf "rm -f %s" fn in
  let _ = Sys.command cmd in ()

let warning fn msg =
  Printf.eprintf "[WARNING imagelib] file %s\n" fn;
  Printf.eprintf "  %s\n" msg;
  Printf.eprintf "  PNG is the prefered format!\n%!"

let size fn =
  let ext = String.lowercase (get_extension' fn) in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.size fn else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.size fn else
  if List.mem ext ReadXCF.extensions
  then ReadXCF.size fn else
  if List.mem ext ReadJPG.extensions
  then ReadJPG.size fn else
  if List.mem ext ReadGIF.extensions
  then ReadGIF.size fn else
  begin
    warning fn "No support for image size...";
    let fn' = temp_file "image" ".png" in
    convert fn fn';
    let sz = ReadPNG.size fn' in
    rm fn'; sz
  end

let openfile fn =
  let ext = String.lowercase (get_extension' fn) in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.openfile fn else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.openfile fn else
  begin
    warning fn "Cannot read this image format...";
    let fn' = temp_file "image" ".png" in
    convert fn fn';
    let img = ReadPNG.openfile fn' in
    rm fn'; img
  end

let writefile fn i =
  let ext = String.lowercase (get_extension' fn) in
  if List.mem ext ReadPNG.extensions
  then write_png fn i else
  if List.mem ext ReadPPM.extensions
  then write_ppm fn i Binary else
  begin
    warning fn "Cannot write to this image format...";
    let fn' = temp_file "image" ".png" in
    write_png fn' i;
    convert fn' fn;
    rm fn'
  end
