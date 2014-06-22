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
open Typography.OutputCommon
open Typography.OutputPaper

let bin_output structure pages filename isoutput' =
  (* For marshalling, do database retain pointer that can not be marshalled *)
  Typography.Db.do_interaction_start_hook ();
  let base_name = try Filename.chop_extension filename with _ -> filename in
  let outputfile = base_name ^ ".bin" in
  let ch = open_out_bin outputfile in
  output_value ch isoutput';
  Marshal.to_channel ch structure [Marshal.Closures];
  Marshal.to_channel ch pages [Marshal.Closures];

  close_out ch;
  Printf.fprintf stderr "File %s written.\n%!" outputfile

let output  ?(structure:structure=empty_structure) (pages : page array)
            (filename : string) = bin_output structure pages filename false

let output' ?(structure:structure=empty_structure) (pages : page array array)
            (filename : string) = bin_output structure pages filename true

let _ =
  Hashtbl.add drivers "Bin"
    (module struct
      let output  = output
      let output' = output'
    end : Driver)
