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
open Printf
open FTypes
open OutputCommon
open Box
open Document
open Util

module type Driver=sig
  val output : ?structure:structure -> page array -> string -> unit
  val output': ?structure:structure -> page array array -> string -> unit
end

let drivers = (Hashtbl.create 37 : (string, (module Driver)) Hashtbl.t)

let dependencies = ["Image",["DriverGL"];"Patonet",["SVG"]]
let rec load_driver name =
  Printf.fprintf stderr "Loading driver %S.\n%!" name;
  let _ =
    try List.iter load_driver (List.assoc name dependencies)
    with Not_found -> ()
  in
  let name = name^".cmxs" in
  let rec fn = function
      [] -> failwith (Printf.sprintf "Driver %S not found." name)
    | dir::l ->
      try
	Dynlink.loadfile (Filename.concat dir name);
	Printf.fprintf stderr "Driver %s loaded.\n%!" name
      with 
	Dynlink.Error (Dynlink.File_not_found _) -> fn l
      |	Dynlink.Error s -> Printf.fprintf stderr "Dynlink error: %s\n" (Dynlink.error_message s); exit 1
  in fn !Config.driverdir
