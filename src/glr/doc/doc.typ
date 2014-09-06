==============================================================================
  Documentation of the ##glr## library and the ##pa_ocaml## parser
------------------------------------------------------------------------------
  Rodolphe Lepigre & Christophe Raffalli
------------------------------------------------------------------------------
  Lama, UMR 5127 CNRS, Univesité Savoie Mont-Blanc
==============================================================================

== Introduction ==

The ##glr## library and ##pa_ocaml## parser provide a simple way to write
parsers and extend the OCaml syntax. It is intended to be simpler to use than
##camlp4##, moreover, no lexer is used, which gives more freedom to write
syntax extensions.

This software provides the following:
\begin{itemize}
\item ##input##: a module allowing to transforms an input channel or string
      into an //input buffer//.
\item ##glr##: a minimalist parser combinator library, which is probably too
      small to be usable directly. Still it proposes a notion of "blank"
      function you can change to ignore blanks and can return the list of all
      possible parse tree or raise an error for ambiguous grammar.
\item ##pa_ocaml##: a parser for the OCaml languages implemented using glr. If
      can be used using the "##-pp##" option of OCaml's tool.
\item ##pa_parser##: an extension of the OCaml syntax to write parsers. The
      syntax is BNF like with one main restriction that //left recursive//
      grammar are forbidden.
\item A quotation and anti-quotation mecanism that you can use to write
      parser to write your own extension to OCaml's syntax.
\end{itemize}

== Blank function ==

While a string or file is being parsed, it is required to differenciate parts
of the parsed input that are meaningful, to those that need to be ignored.
This part of the work is usually handled by a lexer, but ##glr## relies on
another mechanism: blank functions.

A blank function inspects the input buffer at a given position, and returns
the position of the next meaningful data (i.e. the next character that is not
to be ignored. The type of a blank function is the following.

### OCaml

blank = buffer -> int -> buffer * int

###

When a parser is invoked (using the functon ##parse_string## for example), a
default blank function needs to be provided. It will then be used to discard
characters before every terminal is parsed until the function
##change_layout## is called to change the blank function.

== Examples ==

=== The calculator ===

Here is the most classical example: a calculator, including variables.

### OCaml "calc_prio.ml"
open Glr

(* Two regexps + a blank function created from a regexp *)
let float_re = {|[0-9]+\([.][0-9]+\)?\([eE][-+]?[0-9]+\)?|}
let ident_re = {|[a-zA-Z_'][a-zA-Z0-9_']*|}
let blank = blank_regexp (Str.regexp {|[ \t\n\r]*|})

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
    handle_exception (fun () ->
      Printf.printf ">> %!";
      (* we call the parser with the choosen blank function
         and a file name used in error messages *)
      let x = parse_string command blank "stdin" (input_line stdin) in
      Printf.printf "=> %f\n%!" x) ()
  done with End_of_file -> ()
###

