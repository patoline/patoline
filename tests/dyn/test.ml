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
let _=
  Build.macros:=
    Util.StrMap.add "diagram" (fun x->
      "[bB (fun env -> \n" ^
        "let module Res = struct\n "^
        "module Lib = Env_Diagram (struct let env = env end) \n open Lib \n"^
        x^
        "\n end \n"^
        "in [ Drawing (Res.Lib.make ()) ])]\n") !Build.macros;

  Printf.fprintf stderr "blabla\n";flush stderr
