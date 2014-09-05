==============================================
 Documentation of the \verb{glr} library and
 \verb{pa_ocaml} parser
----------------------------------------------
 Rodolphe Lepigre & Christophe Raffalli
----------------------------------------------
 Lama, UMR 5127 CNRS, Univesité Savoie Mont-Blanc
==============================================

== Introduction ==

The \verb{glr} library and
 \verb{pa_ocaml} parser gives a simple way to write parser and extend OCaml's grammar.
Compared to camlp4, it is simpler and does not use a lexer at all which gives much more freedom
to write syntax extension.

This softwere use propose the following:

\begin{itemize}
\item The \verb{input} module: transforms an input channel or string into an // input buffer //

\item \verb{glr} library : a minimalist parser combinator library, probably too small to be usable.
Still it proposes a notion of "blank" function you can change to ignore blanks and can return the list
of all possible parse tree or raise an error for ambiguous grammar

\item \verb{pa_ocaml} : a parser for OCaml languages using glr. If can be used using the "##-pp##" option of OCaml's tool.

\item \verb{pa_parser} : an extension of the OCaml syntax to write parser. The syntax is BNF like with one main restriction
that //left recursive// grammar are forbidden.

\item A quotation and anti-quotation mecanism that you can use to write parser to write your own extension to OCaml's syntax.
\end{itemize}

== Examples ==

=== The calculator ===

Here is the most classical example: a calculator, including variable.

### OCaml "calc_prio.ml"
open Glr

(* Two regexps + a blank function created from a regexp *)
let float_re = "[0-9]+\\([.][0-9]+\\)?\\([eE][-]?[0-9]+\\)?"
let ident_re = "[a-zA-Z_'][a-zA-Z0-9_']*"
let blank = blank_regexp (Str.regexp "[ \t\n\r]*")

(* definition of the pririoty levels and a hash tbl for the 
   values of variables *)
type calc_prio = Sum | Prod | Pow | Atom
let env = Hashtbl.create 101

(* we declare a "family of parser", because we want to
   define a recursive grammar *)
let expression, set_expression = grammar_family "expression" 

(* Two small parsers for infix symbols *)
let products = parser CHR('*') -> ( *. ) | CHR('/') -> ( /. )
let sums = parser CHR('+') -> ( +. ) | CHR('-') -> ( -. )

(* we define the main parser, parametrised by priority *)
let _ = set_expression (fun prio ->
  parser
  | f:RE(float_re) when prio = Atom -> float_of_string f
  | id:RE(ident_re) when prio = Atom ->
      (try Hashtbl.find env id
       with Not_found ->
         Printf.eprintf "Unbound %s\n%!" id; raise Exit)
  | CHR('(') e:(expression Sum) CHR(')') when prio = Atom -> e
  | CHR('-') e:(expression Pow) when prio = Pow -> -. e
  | CHR('+') e:(expression Pow) when prio = Pow -> e
  | e:(expression Atom) e':{STR("**") e':(expression Pow)}? when prio = Pow ->
         (match e' with None -> e | Some e' -> e ** e')
  | e:(expression Pow) l:{fn:products e':(expression Pow)}* when prio = Prod ->
      List.fold_left ( fun acc (fn, e') -> fn acc e') e l
  | e:(expression Prod) l:{fn:sums e':(expression Prod)}* when prio = Sum ->
      List.fold_left ( fun acc (fn, e') -> fn acc e') e l)

(* the parser for commands *)
let command = parser
  | id:RE(ident_re) CHR('=') e:(expression Sum) -> Hashtbl.add env id e; e
  | e:(expression Sum) -> e

let _ =
  try while true do
    try
      Printf.printf ">> %!";
      (* we call the parser with the choosen blank function
         and a file name used in error messages *)
      let x = parse_string command blank "stdin" (input_line stdin) in
      Printf.printf "=> %f\n%!" x
    with e -> handle_error e
  done with End_of_file -> ()
###

