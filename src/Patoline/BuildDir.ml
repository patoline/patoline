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
open Filename

let build_dir = ref "_patobuild"

let is_temp f =
  !build_dir <> "" && (
  let d = dirname f in
  let i = basename d in
  i = !build_dir)

let concat a b = if a = "." then b else Filename.concat a b

let ori_name f =
  if !build_dir = "" then f 
  else 
    let d = dirname f in
    let n = basename f in
    let i = basename d in
    if i = !build_dir then
      concat (dirname d) n
    else f
      
let make_temp f =
  if !build_dir = "" then f 
  else 
    let d = dirname f in
    let n = basename f in
    let i = basename d in
    if i = !build_dir then
      f
    else 
      let d' = concat d !build_dir in
      if not (Sys.file_exists d') then Unix.mkdir d' 0o700;
      concat d' n

let temp_extension = [ ".tml" ; ".ttml" ; ".cmo" ; ".cmi"; ".cmx" ; ".tdx" ; ".tmx" ; ".dep" ; ".tdep" ; ".o" ; "_.cmx" ; "_.tml" ; "_.tdep" ]

let chop_extension ?(compile=false) f =
  assert (f <> "");
  let f = try Filename.chop_extension f with _ -> f in
  if compile then f else
    if f.[String.length f - 1] = '_' then
      String.sub f 0 (String.length f - 1) else f

let chg_ext ?(compile=false) f ext =
  let f = chop_extension ~compile f in
  if List.mem ext temp_extension then
    make_temp f ^ ext
  else
    ori_name f ^ ext


let includes_opt source =
  let d =dirname (ori_name source) in
  Printf.sprintf "-I %s -I %s" d (concat d !build_dir)

let includes_opts source =
  let d =dirname (ori_name source) in
  ["-I"; d; "-I"; (concat d !build_dir)]

let refine_ext f =
  if List.exists (fun s -> Filename.check_suffix f s) temp_extension then
    make_temp f
  else
    ori_name f

let str_dirs optsdirs =
  let rec make_dirs l res=match l with
      []->List.rev res
    | h::s-> 
      let res = 
	if Sys.is_directory (Filename.concat h !build_dir) then
	  (Filename.concat h !build_dir) ::"-I"::res
	else
	  res
      in
      make_dirs s (h::"-I"::res)
  in
  make_dirs optsdirs []
