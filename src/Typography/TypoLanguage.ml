(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

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
open Lexing
open Line

type message=
  | No_solution of string
  | Opt_error of optimization_error
  | Normal
#ifdef BAN_COMIC_SANS
  | Ban_comic_sans
#endif

and optimization_error=
    Overfull_line of (line*string)
  | Widow of (line*string)
  | Orphan of (line*string)

#ifdef LANG_FR
let lang=`FR
let message=function
  | No_solution str when String.length str>0->sprintf "Pas de solution, document incomplet. Dernière ligne:\n%s" str
  | No_solution str->"Pas de solution, document vide"
  | Opt_error (Widow (line,s))->Printf.sprintf "Veuve : \n\t%s\n\t%s" (sprint_linef line) s
  | Opt_error (Orphan (line,s))->Printf.sprintf "Orphelin : \n\t%s\n\t%s" (sprint_linef line) s
  | Opt_error (Overfull_line (line,s))->Printf.sprintf "Ligne trop pleine \n\t%s\n\t%s" (sprint_linef line) s
#ifdef BAN_COMIC_SANS
  | Ban_comic_sans->"Votre choix de police (comic sans) est moche.\nConsultez http://bancomicsans.com pour plus d'informations\n"
#endif
  | _->""
#else
#define LANG_EN
let lang=`EN
let message=function
  | No_solution str when String.length str>0->sprintf "Pas de solution, document incomplet. Dernière ligne:\n%s" str
  | No_solution str->"Pas de solution, document vide"
  | Opt_error (Widow (line,s))->Printf.sprintf "Veuve : \n\t%s\n\t%s" (sprint_linef line) s
  | Opt_error (Orphan (line,s))->Printf.sprintf "Orphelin : \n\t%s\n\t%s" (sprint_linef line) s
  | Opt_error (Overfull_line (line,s))->Printf.sprintf "Ligne trop pleine \n\t%s\n\t%s" (sprint_linef line) s
#ifdef BAN_COMIC_SANS
  | Ban_comic_sans->"Bad taste detected in font choice.\nPlease go to http://bancomicsans.com for more informations\n"
#endif
  | _->""
#endif
