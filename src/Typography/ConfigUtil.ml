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
open Config
exception File_not_found of (string*string list)
(** Chercher un fichier dans un chemin *)
let findPath f path=
  let rec findPath f=function
      []->(Printf.printf "%s\n" (TypoLanguage.message (TypoLanguage.FileNotFound f));
           List.iter (Printf.printf "%s\n") path;raise (File_not_found (f,path)))
    | h::s when Sys.file_exists (Filename.concat h f)->(Filename.concat h f)
    | h::s -> (findPath f s)
  in
    findPath f path
(** Chercher un fichier dans le chemin des polices *)
let findFont f=findPath f ("."::(!fontspath))
(** Chercher un fichier dans le chemin des grammaires *)
let findGrammar f=findPath f ("." :: (!grammarspath))
(** Chercher un fichier dans le chemin des dictionnaires de césures *)
let findHyph f=findPath f ("."::(!hyphenpath))
