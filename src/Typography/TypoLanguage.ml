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
open Lexing

type line=string
let sprint_linef x=x

type message=
  | No_solution of string
  | Opt_error of optimization_error
  | Normal
  | PleaseReport of string
  | FileNotFound of string
  | BadEncoding of string
#ifdef BAN_COMIC_SANS
  | Ban_comic_sans
#endif

and optimization_error=
    Overfull_line of (string)
  | Underfull_line of (string)
  | Widow of (string)
  | Orphan of (string)

#ifdef LANG_FR
let lang=`FR
let message=function
  | No_solution str when String.length str>0->sprintf "Pas de solution, document incomplet. Dernière ligne:\n%s" str
  | No_solution str->"Pas de solution, document vide"
  | Opt_error (Widow (s))->Printf.sprintf "Veuve : \n\t%s" (*sprint_linef line*) s
  | Opt_error (Orphan (s))->Printf.sprintf "Orphelin : \n\t%s" (*sprint_linef line*) s
  | Opt_error (Overfull_line (s))->Printf.sprintf "Ligne trop pleine :\n\t%s" (*sprint_linef line*) s
  | Opt_error (Underfull_line (s))->Printf.sprintf "Ligne pas assez remplie :\n\t%s" (*sprint_linef line*) s
  | PleaseReport x->Printf.sprintf "Il y a quelque chose que Patoline ne gère pas normalement. Veuillez rapporter ceci par courriel à mltypography@googlegroups.com :\n%s" x
  | BadEncoding x->Printf.sprintf "Problème d'encodage du fichier, le texte suivant n'a pas pu être décodé :\n%S" x
#ifdef BAN_COMIC_SANS
  | Ban_comic_sans->"Votre choix de police (comic sans) est moche.\nConsultez http://bancomicsans.com pour plus d'informations\n"
#endif
  | FileNotFound s->Printf.sprintf "Le fichier \"%s\" n'a pas été trouvé\n" s
  | _->""
#else
#define LANG_EN
let lang=`EN
let message=function
  | No_solution str when String.length str>0->sprintf "No solution, empty document. Last line:\n%s" str
  | No_solution str->"No solution, empty document."
  | Opt_error (Widow (s))->Printf.sprintf "Widow:\n\t%s" (*sprint_linef line*) s
  | Opt_error (Orphan (s))->Printf.sprintf "Orphan:\n\t%s" (*sprint_linef line*) s
  | Opt_error (Overfull_line (s))->Printf.sprintf "Overfull line:\n\t%s" (*sprint_linef line*) s
  | Opt_error (Overfull_line (s))->Printf.sprintf "Underfull line:\n\t%s" (*sprint_linef line*) s
  | PleaseReport x->Printf.sprintf "Something (not \"the impossible\", though) has happened that Patoline is not comfortable with. Please report the following by email to mltypography@googlegroups.com:\n%s" x
  | BadEncoding x->Printf.sprintf "File encoding problem, could not decode:\n%S" x
#ifdef BAN_COMIC_SANS
  | Ban_comic_sans->"Bad taste detected in font choice.\nPlease go to http://bancomicsans.com for more informations\n"
#endif
  | FileNotFound s->Printf.sprintf "File \"%s\" was not found\n" s
  | _->""
#endif
