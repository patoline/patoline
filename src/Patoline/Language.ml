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

type message=
    End_of_parsing of int
  | Syntax_error of (string*position*syntax_error)
  | Inexistent_file of string
  | No_grammar_loaded of string*string
  | No_input_file
  | Usage
  | Unknown_command of string
  | Cli of command_line
  | Image of image
  | Dynlink_error of string

and syntax_error=
    Parse_error
  | Unexpected_char
  | Splitted_end_of_caml_code
  | Unterminated_comment
  | Unexpected_end_of_comment
  | Unterminated_math
  | Unterminated_text
  | Unterminated_ocaml

and command_line=
    Extra_fonts
  | Extra_hyph
  | Font_filter
  | Build_dir
  | No_build_dir
  | Dirs
  | No_grammar
  | Format
  | Driver
  | Dynlink
  | Separately
  | Noamble
  | Package
  | Ml
  | MainMl
  | Output
  | Bin
  | Edit_link
  | Patoline
  | Ocamlopt
  | Parallel
  | Quiet
  | Remaining
  | No_line_directive
  | Recompile
  | CCOpts
  | TopOpts
  | Debug_parser


and image =
    Width
  | Height
  | IFormat
  | RGB | BGR | VRGB | VBGR

#ifdef LANG_FR
let lang=`FR
let message=function
    End_of_parsing trees->
      sprintf "End of parsing (%d tree%s)" trees (if trees>1 then "s" else "")
  | Syntax_error (file, pos,msg)->
      sprintf "Erreur de syntaxe, file \"%s\", line %d, character %d:\n%s"
        file (* pos.pos_fname *) pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
        (match msg with
             Parse_error->"Erreur d'analyse"
           | Unexpected_char->"Caractère inattendu"
           | Splitted_end_of_caml_code->"Code caml terminé plusieurs fois"
           | Unterminated_comment->"Commentaire non terminé"
           | Unexpected_end_of_comment->"Fin de commentaire inattendue"
           | Unterminated_ocaml->"Section caml non terminée"
           | Unterminated_math->"Section maths non terminée"
           | Unterminated_text->"Section texte non terminée"
        )
  | No_grammar_loaded (file,msg)->sprintf "La grammaire %s n'a pas été chargée (erreur : %s)" file msg
  | Inexistent_file file->sprintf "%s : Le fichier n'existe pas" file
  | No_input_file->"Pas de fichier d'entrée"
  | Usage->"Utilisation :"
  | Unknown_command c->sprintf "%s : Commande inconnue" c
  | Cli cli->(
    match cli with
        Extra_fonts->"Rajoute le répertoire au chemin de recherche des polices"
      | Extra_hyph->"Rajoute le répertoire au chemin de recherche des dictionnaires de césures"
      | Font_filter->"Ajoute un filtre pour les fonts pour les drivers SVG et Patonet"
      | Build_dir->"Fixe le répertoire des fichiers intermédiaires"
      | No_build_dir ->"N'utilise pas de répertoire des fichiers intermédiaires"
      | Dirs->"Rajoute le répertoire au chemin de compilation"
      | No_grammar->"Commence l'analyse syntaxique avec une grammaire vide"
      | Format->"Compile avec ce format"
      | Dynlink->"Prepare un binaire permettant de lier dynamiquement le driver (expérimental)"
      | Driver->"Compile avec ce driver de sortie\n    drivers originaux : "
      | Separately->"Compile séparément"
      | Noamble->"Produit un module caml brut"
      | Package->"Rajoute le paquets dans la liste des paquets pour ocamlfind"
      | Ml->"Ne produire que le source caml (.ttml)"
      | MainMl->"Ne produire que le source principale caml (._tml)"
      | Output->"Écrire la sortie dans ce fichier"
      | Bin->"Produire le source caml et le compiler, mais pas l'éxécuter"
      | Edit_link->"Générer des liens URI de la forme \"edit:filename@line\""
      | Patoline->"Nom de l'exécutable patoline à utiliser pour les sous-documents"
      | Ocamlopt->"Nom de l'exécutable ocamlopt à utiliser pour la compilation"
      | Parallel->"Nombre maximal de processus (pour la compilation parallèle)"
      | Quiet->"Niveau d'affichade des commandes de compilation [0:rien, 1:court (defaut), 2:complet]"
      | Remaining->"Passer les arguments suivants au compilateur caml"
      | CCOpts->"Passer la chaine au compilateur caml"
      | TopOpts->"Passer la chaine à l'exécutable généré par Patoline"
      | No_line_directive->"Ne pas générer de #line dans le caml produit"
      | Recompile->"Tout recompiler"
      | Debug_parser->"Active le deboggage du parseur (developpeur)")
  | Image cli -> (
    match cli with
    | Width -> "specifie la largeur de l'image"
    | Height -> "specifie la hauteur de l'image"
    | IFormat -> "specifie le format de l'image"
    | RGB -> "active l'antialiasing pour les écrans RGB"
    | BGR -> "active l'antialiasing pour les écrans BGR"
    | VRGB -> "active l'antialiasing pour les écrans VRGB"
    | VBGR -> "active l'antialiasing pour les écrans VBGR")
  | Dynlink_error err->(
    Printf.sprintf "Erreur d'édition de liens dynamique :\n%s\n" err
  )

#else
#define LANG_EN
let lang=`EN
let message=function
    End_of_parsing trees->
      sprintf "End of parsing (%d tree%s)" trees (if trees>1 then "s" else "")
  | Syntax_error (file, pos,msg)->
      sprintf "Syntax error, file \"%s\", line %d, character %d:\n%s"
        file (* pos.pos_fname *) pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
        (match msg with
             Parse_error->"Parse error"
           | Unexpected_char->"Parse error"
           | Splitted_end_of_caml_code->"Splitted end of caml code"
           | Unterminated_comment->"Unterminated comment"
           | Unexpected_end_of_comment->"Unexpected end of comment"
           | Unterminated_ocaml->"Unterminated OCaml section"
           | Unterminated_math->"Unterminated texprime math section"
           | Unterminated_text->"Unterminated texprime text section"
        );
  | No_grammar_loaded (file,msg)->sprintf "Could not load grammar %s (error : %s)" file msg
  | Inexistent_file file->sprintf "%s: No such file or directory" file
  | No_input_file->"No input file"
  | Usage->"Usage:"
  | Unknown_command c->sprintf "%s: Unknown command" c
  | Cli cli->(
    match cli with
        Extra_fonts->"Add directory to the font search path"
      | Extra_hyph->"Add directory to the hyphenation dictionary search path"
      | Font_filter->"Add a font filter command for SVG and Patonet drivers"
      | Build_dir->"Fixes the directory for auxilliary files"
      | No_build_dir->"Use no directory for auxilliary files"
      | Dirs->"Add directory to the list of include directories"
      | No_grammar->"Parse with an initially empty grammar"
      | Format->"Compile with this format"
      | Driver->"Compile with this output driver\n    original drivers: "
      | Dynlink -> "Prepare a binary for using dynlinked driver (experimental)"
      | Separately->"Compile separately"
      | Noamble->"Output a raw caml file"
      | Package->"Add the package to the list of ocamlfind packages"
      | Ml->"Only output caml file (.ttml)"
      | MainMl->"Only output caml main file (._tml)"
      | Output->"Output to this file"
      | Bin->"Output and compile caml file, but do not execute it"
      | Edit_link->"Generate URI links of the form \"edit:filename@line\""
      | Patoline->"Name of the patoline executable to use for sub-documents"
      | Ocamlopt->"Name of the ocamlopt executable to use when compiling"
      | Parallel->"Maximal number of processes (for parallel build)"
      | Quiet->"Level of verbosity [0:nothing, 1:short (default), 2:full]"
      | Remaining->"Forward the remaining arguments to the caml compiler"
      | CCOpts->"Forward the string to the ocaml compiler"
      | TopOpts->"Forward the string to the generated executable"
      | No_line_directive->"Do not add #line directives in the generated caml code"
      | Recompile->"Recompile everything"
      | Debug_parser->"Activate paser debugging (developper)")
  | Image cli -> (
    match cli with
    | Width -> "specify image width"
    | Height -> "specify image height"
    | IFormat -> "specify image format"
    | RGB -> "subpixel antialiasing for RGB screen"
    | BGR -> "subpixel antialiasing for BGR screen"
    | VRGB -> "subpixel antialiasing for VRGB screen"
    | VBGR -> "subpixel antialiasing for VBGR screen")
  | Dynlink_error err->(
    Printf.sprintf "Dynamic linking error :\n%s\n" err
  )

#endif
