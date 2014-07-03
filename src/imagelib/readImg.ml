open Filename
open Image
open ImagePNG
open ImagePPM

(** [chop_extension' fname] is the same as [Filename.chop_extension fname] but
    if [fname] does not have an extension, [fname] is returned instead of
    raising [Invalid_argument]. *)
let chop_extension' fname =
  try Filename.chop_extension fname
  with _ -> fname

(** [get_extension fname] returns the extension of the file [fname]. If the
    file does not have an extension, [Invalid_argument] is raised. *)
let get_extension fname =
  let baselen = String.length (chop_extension' fname) in
  let extlen  = String.length fname - baselen - 1 in
  if extlen <= 0
  then let err = Printf.sprintf "No extension in filename '%s'." fname in
       raise (Invalid_argument err)
  else String.sub fname (baselen + 1) extlen

(** [get_extension' fname] is the same as [get_extension fname] but if [fname]
    does not have an extension, the empty string is returned and no exception
    is raised. *)
let get_extension' fname =
  try get_extension fname
  with _ -> ""

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
