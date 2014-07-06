open Filename
open Image
open ImageUtil
open ImagePNG
open ImagePPM
open ImageXCF
open ImageJPG
open ImageGIF

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
    let fn' = temp_file "image" ".png" in
    let cmd = Printf.sprintf "convert %s %s" fn fn' in
    let ret = Sys.command cmd in
    if ret <> 0
    then assert false
    else ReadPNG.size fn'
  end

let openfile fn =
  let ext = String.lowercase (get_extension' fn) in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.openfile fn else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.openfile fn else
  begin
    let fn' = temp_file "image" ".png" in
    let cmd = Printf.sprintf "convert %s %s" fn fn' in
    let ret = Sys.command cmd in
    if ret <> 0
    then assert false
    else ReadPNG.openfile fn'
  end

let writefile fn i =
  let ext = String.lowercase (get_extension' fn) in
  if List.mem ext ReadPNG.extensions
  then write_png fn i else
  if List.mem ext ReadPPM.extensions
  then write_ppm fn i Binary else
  begin
    let fn' = temp_file "image" ".png" in
    write_png fn' i;
    let cmd = Printf.sprintf "convert %s %s" fn' fn in
    let ret = Sys.command cmd in
    if ret <> 0
    then assert false;
  end
