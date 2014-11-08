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

let f1 = Sys.argv.(1)
let f2 = Sys.argv.(2)

let table =
  let ch = open_in f1 in
  let r = ref [] in
  let regexp = Str.regexp "(\"\\([^\"]*\\)\"[ \t]*\\?\\([^?]*\\))" in
  try
    while true do
      let l = input_line ch in
      if Str.string_match regexp l 0 then begin
	let key = Str.matched_group 1 l in
	let char = Str.matched_group 2 l in
	if UTF8.length char = 1 then begin
(*	  Printf.fprintf stderr "read %s => %s\n" key char;*)
	  r := (key, char)::!r;
	end;
      end;
    done;
    assert false
  with _ ->
    close_in ch;
    !r

let find_all s l =
  let rec fn acc l = match l with
      [] -> List.rev acc
    | (k,c)::l ->
      let acc = if c = s then k::acc else acc in
      fn acc l
  in fn [] l

let _ =
  let ch = open_in f2 in
  try
    let regexp = Str.regexp "(\"\\([_^]\\)\\([^\"]*\\)\"[ \\t]*\\?\\([^)]*\\))" in
    while true do
      let l = input_line ch in
      if Str.string_match regexp l 0 then begin
	let typ = Str.matched_group 1 l in
	let key = Str.matched_group 2 l in
	let char = Str.matched_group 3 l in
	match find_all key table with
	  [] -> if UTF8.length key = String.length key then Printf.printf "%s\n" l
	| ls -> List.iter (fun k -> Printf.printf "(\"%s%s\" ?%s)\n" typ k char) ls
      end;
    done;
    assert false
  with _ ->
    close_in ch
