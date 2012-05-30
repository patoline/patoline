open Printf
open Lexing
open Line

type message=
  | No_solution of string
  | Opt_error of optimization_error
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

#else
#define LANG_EN
let lang=`EN
let message=function
  | No_solution str when String.length str>0->sprintf "Pas de solution, document incomplet. Dernière ligne:\n%s" str
  | No_solution str->"Pas de solution, document vide"
  | Opt_error (Widow (line,s))->Printf.sprintf "Veuve : \n\t%s\n\t%s" (sprint_linef line) s
  | Opt_error (Orphan (line,s))->Printf.sprintf "Orphelin : \n\t%s\n\t%s" (sprint_linef line) s
  | Opt_error (Overfull_line (line,s))->Printf.sprintf "Ligne trop pleine \n\t%s\n\t%s" (sprint_linef line) s

#endif
