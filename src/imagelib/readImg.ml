open Filename
open Image
open Png
open Ppm
open FilenameExtra

let size fn =
  let ext = String.lowercase (get_extension' fn) in
  if List.mem ext ReadPNG.extensions
  then ReadPNG.size fn else
  if List.mem ext ReadPPM.extensions
  then ReadPPM.size fn else
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
