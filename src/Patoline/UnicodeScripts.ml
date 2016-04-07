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

(* First command-line argument: UnicodeData.txt input.
 * Second command-line argument: SubSuper.ml output.
 * Third command-line argument: SubSuper.el output in emacs directory.
 *)

let unicodedata_file = Sys.argv.(1)
let subsuperml_file = Sys.argv.(2)
let subsuperel_file = Sys.argv.(3)

let file = open_in unicodedata_file

type uchar = string array

let parse_line s =
  let rec scan_field i = if i < String.length s then
      match s.[i] with
      | ';' -> i
      | a -> scan_field (succ i)
    else i
  in
  let rec scan res n i =
    if n < 15 then
      let j = scan_field i in
      let field = String.sub s i (j-i) in
      let _ = res.(n) <- field in
      scan res (succ n) (succ j)
    else
      ()
  in
  let res = Array.make 15 "" in
  let _ = scan res 0 0 in
  res

let subscripts : uchar list ref = ref []
let superscripts : uchar list ref = ref []

let rec string_forall i len p s =
  if i >= String.length s then true else
    let len =
      if i+len-1 < String.length s then len else
	String.length s - i
    in
    let max = i + len in
    let rec string_forall_rec res k =
      if k < max then
	string_forall_rec ((p s.[k] k) && res) (succ k)
      else res
    in string_forall_rec true i


let is_superscript c =
  String.length c.(5) > 7 && String.sub c.(5) 0 7 = "<super>"

let is_subscript c =
  String.length c.(5) > 5 && String.sub c.(5) 0 5 = "<sub>"


let _ = try
	  while true do
	    let s = input_line file in
	    let c = parse_line s in
	    if is_superscript c then
	      superscripts := c :: !superscripts
	    else
	      if is_subscript c then
	      subscripts := c :: !subscripts
	      else ()
	  done
  with End_of_file -> ()

let _ = close_in file

let id x = x

let superscripts =
  List.map (fun c ->
    c.(1),
    Scanf.sscanf c.(0) " %x" id,
    Scanf.sscanf (String.sub c.(5) 7 (String.length c.(5) - 7)) " %x" id) !superscripts

let subscripts =
  List.map (fun c ->
    c.(1),
    Scanf.sscanf c.(0) " %x" id,
    Scanf.sscanf (String.sub c.(5) 5 (String.length c.(5) - 5)) " %x" id) !subscripts

(*
let _ = List.iter (fun (c,h,h') -> Printf.printf "%s %x %x\n" c h h') subscripts
let _ = List.iter (fun (c,h,h') -> Printf.printf "%s %x %x\n" c h h') superscripts
*)

let int_to_bytes n =
  UTF8.init 1 (fun _ -> UChar.chr n)

let esc_int_to_bytes n =
  String.escaped (int_to_bytes n)


let ch = open_out subsuperml_file

let _ =
  Printf.fprintf ch "open Decap\n";
  Printf.fprintf ch "let parser subscript =\n";
  List.iter (fun (c,h,h') ->
    Printf.fprintf ch "|\"%s\" -> \"%s\"\n" (esc_int_to_bytes h) (esc_int_to_bytes h')) subscripts;
  Printf.fprintf ch "\n";
  Printf.fprintf ch "let parser superscript =\n";
  List.iter (fun (c,h,h') ->
    Printf.fprintf ch "|\"%s\" -> \"%s\"\n" (esc_int_to_bytes h) (esc_int_to_bytes h')) superscripts;
  close_out ch


let ch = open_out subsuperel_file

let _ =
  List.iter (fun (c,h,h') ->
    Printf.fprintf ch "(\"_%s\" ?%s)\n" (int_to_bytes h') (int_to_bytes h)) subscripts;
  List.iter (fun (c,h,h') ->
    Printf.fprintf ch "(\"^%s\" ?%s)\n" (int_to_bytes h') (int_to_bytes h)) superscripts;
  close_out ch
