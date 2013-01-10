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
open Ocamlbuild_plugin;;

dispatch begin function
  | After_options ->(
    (rule "unicode_ranges"
        ~prod:"Fonts/Sfnt/unicode_ranges.ml"
        ~deps:["Fonts/Sfnt/make_unicode_ranges.ml";"Fonts/Sfnt/unicode"]
        begin fun env _build->
          let _=_build [["Fonts/Sfnt/make_unicode_ranges.byte"]] in
          Cmd(S[A"Fonts/Sfnt/make_unicode_ranges.byte";A"Fonts/Sfnt/unicode"])
        end);

    (* flag ["ocaml";"compile";"rectypes"] & A"-rectypes"; *)
    flag ["ocaml";"compile"] & S[A"-I";A"../../Rbuffer"];
    flag ["ocaml";"compile";"rectypes"] & A"-rectypes";
    flag ["ocaml";"pack"] & S[A"-linkall"];
    flag ["ocaml";"link"] & S[A"-linkall"];
    (* flag ["ocaml"] & S[A"-pp";A"cpp"]; *)
  )
  | _ -> ()
end
