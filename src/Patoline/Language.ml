(*# OPTIONS -pp "cpp -w" #*)
open Printf
open Lexing

type message=
    End_of_parsing of int
  | Syntax_error of (string*position*syntax_error)
  | Unexisting_file of string
  | No_input_file
  | Usage
  | Unknown_command of string
  | Cli of command_line

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
  | Dirs
  | No_grammar
  | Format
  | Driver
  | Separately
  | Noamble
  | Package
  | Ml
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
  | Unexisting_file file->sprintf "%s : Le fichier n'existe pas" file
  | No_input_file->"Pas de fichier d'entrée"
  | Usage->"Utilisation :"
  | Unknown_command c->sprintf "%s : Commande inconnue" c
  | Cli cli->(
    match cli with
        Extra_fonts->"Rajoute le répertoire au chemin de recherche des polices"
      | Extra_hyph->"Rajoute le répertoire au chemin de recherche des dictionnaires de césures"
      | Dirs->"Rajoute le répertoire au chemin de compilation"
      | No_grammar->"Commence l'analyse syntaxique avec une grammaire vide"
      | Format->"Compile avec ce format"
      | Driver->"Compile avec ce driver de sortie"
      | Separately->"Compile séparément"
      | Noamble->"Produit un module caml brut"
      | Package->"Rajoute le paquets dans la liste des paquets pour ocamlfind"
      | Ml->"Ne produire que le source caml"
      | Bin->"Produire le source caml et le compiler, mais pas l'éxécuter"
      | Edit_link->"Générer des liens URI de la forme \"edit:filename@line\""
      | Patoline->"Nom de l'exécutable patoline à utiliser pour les sous-documents"
      | Ocamlopt->"Nom de l'exécutable ocamlopt à utiliser pour la compilation"
      | Parallel->"Nombre maximal de processus (pour la compilation parallèle)"
      | Quiet->"Ne pas afficher les commandes de compilation"
      | Remaining->"Passer les arguments suivants au compilateur caml"
      | CCOpts->"Passer la chaine au compilateur caml"
      | TopOpts->"Passer la chaine à l'exécutable généré par Patoline"
      | No_line_directive->"Ne pas générer de #line dans le caml produit"
      | Recompile->"Tout recompiler")


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
  | Unexisting_file file->sprintf "%s: No such file or directory" file
  | No_input_file->"No input file"
  | Usage->"Usage:"
  | Unknown_command c->sprintf "%s: Unknown command" c
  | Cli cli->(
    match cli with
        Extra_fonts->"Add directory to the font search path"
      | Extra_hyph->"Add directory to the hyphenation dictionary search path"
      | Dirs->"Add directory to the list of include directories"
      | No_grammar->"Parse with an initially empty grammar"
      | Format->"Compile with this format"
      | Driver->"Compile with this output driver"
      | Separately->"Compile separately"
      | Noamble->"Output a raw caml file"
      | Package->"Add the package to the list of ocamlfind packages"
      | Ml->"Only output caml file"
      | Bin->"Output and compile caml file, but do not execute it"
      | Edit_link->"Generate URI links of the form \"edit:filename@line\""
      | Patoline->"Name of the patoline executable to use for sub-documents"
      | Ocamlopt->"Name of the ocamlopt executable to use when compiling"
      | Parallel->"Maximal number of processes (for parallel build)"
      | Quiet->"Do not output the compilation commands"
      | Remaining->"Forward the remaining arguments to the caml compiler"
      | CCOpts->"Forward the string to the ocaml compiler"
      | TopOpts->"Forward the string to the generated executable"
      | No_line_directive->"Do not add #line directives in the generated caml code"
      | Recompile->"Recompile everything"
  )
#endif
