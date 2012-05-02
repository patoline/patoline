open Printf
open Lexing
open Line

type message=
  | No_solution of string
  | Opt_error of optimization_error
and optimization_error=
    Overfull_line of line
  | Widow of line
  | Orphan of line

#ifdef LANG_FR
let lang=`FR
let message=function
  | No_solution str when String.length str>0->sprintf "Pas de solution, document incomplet. Dernière ligne:\n%s" str
  | No_solution str->"Pas de solution, document vide"
  | Opt_error (Widow line)->"Veuve : "^sprint_linef line
  | Opt_error (Orphan line)->"Orphelin : "^sprint_linef line
  | Opt_error (Overfull_line line)->"Ligne trop pleine "^sprint_linef line

#else
#define LANG_EN
let lang=`EN
let message=function
  | No_solution str when String.length str>0->sprintf "No solution, incomplete document. Last printed line:\n%s" str
  | No_solution str->"No solution, empty document."
  | Opt_error (Widow line)->"Widow: "^sprint_linef line
  | Opt_error (Orphan line)->"Orphan: "^sprint_linef line
  | Opt_error (Overfull_line line)->"Overfull line: "^sprint_linef line

#endif
