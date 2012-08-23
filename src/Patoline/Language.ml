(*# OPTIONS -pp "cpp -w" #*)
open Printf
open Lexing

type message=
    End_of_parsing of int
  | Syntax_error of (string*position*syntax_error)

and syntax_error=
    Parse_error
  | Unexpected_char
  | Splitted_end_of_caml_code
  | Unterminated_comment
  | Unexpected_end_of_comment
  | Unterminated_math
  | Unterminated_text
  | Unterminated_ocaml

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
        );
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
#endif
