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

== The main types and functions ==

The main type proposed by the library in ``'a Glr.grammar`` which is the type of 
a function parsing a //buffer// and returning some data of type ``'a``.
The data to be parsed is provided in a data structure of type ``Input.buffer``.
Here are that main functions related to these types, we encourage the reader 
to consult the corresponding interface file for a complete list.

--- Buffer related functions ---

\begin{itemize}
\item ``input_from_file : string -> buffer`` to read a file
\item ``buffer_from_channel : ?filename:string -> in_channel -> buffer``
to read an input channel (the //filename// is used in 
exceptions only and defaults to ``""``).
\item ``buffer_from_string : ?filename:string -> string -> buffer``
to read a string. (the first string also plays a role of a //file name// used in 
exceptions, the data to parse is therefore in the second string).
\item ``read : buffer -> int -> char * buffer * int`` to manually
read the content of a buffer at a given position and return a buffer and position
that can be used to read the rest of the input. In the current implementation, the
   buffer is a lazy stream of lines and the 
   position is therefore the position in the line. This might change in the futur.
\end{itemize}

--- Parsing functions ---

Here are the function used to parse data. All theses function parses the data until the (or return an
exception). Function to parse parts of the input are also provided (see the file ``glr.mli``).

\begin{itemize}
\item ``parse_string : ?filename:string -> 'a grammar -> blank -> string -> 'a``
Parses a string, given a parser and //blank// function (see below).
\item ``parse_channel : ?filename:string -> 'a grammar -> blank -> in_channel -> 'a``
Similar to the previous one for input channel.
\item ``parse_file : 'a grammar -> blank -> string -> 'a``
Open the file and parses it using the previous function.
\item ``parse_buffer : 'a grammar -> blank -> buffer -> 'a`` The lowest level function
to call a parser.
\end{itemize}

== Blank function ==

While a string or file is being parsed, it is required to differenciate parts
of the parsed input that are meaningful, to those that need to be ignored.
This part of the work is usually handled by a lexer, but ##glr## relies on
another mechanism: blank functions.

A blank function inspects the input buffer at a given position, and returns
the position of the next meaningful data (i.e. the next character that is not
to be ignored. The type of a blank function is the following:

### OCaml
blank = buffer -> int -> buffer * int
###

When a parser is invoked (using the functon ##parse_string## for example), a
default blank function needs to be provided. It will then be used to discard
characters before every terminal is parsed.

On important feature if that the blank function can be changed using the function:

### OCaml
change_layout : ?old_blank_before:bool -> ?new_blank_after:bool -> 
  'a grammar -> blank -> 'a grammar
###

The grammar returned by ``change_layout parser blank`` will only use 
the provided blank function and ignore the old one. 

Still, the is a problem at the beginning and at the end of the input.
Blank function are called by the terminal of the grammar.
The first optional argument ``old_blank_before`` (``true`` by default) 
will force using first the current blank function and before parsing a terminal,
also the new one.

Similarly, ``new_blank_after`` (``false`` by default) will 
forces to use the newly provided blank function once at the end of the 
parsed input (and the old blank function will be used too as expected
before the next terminal.

To parse no blank, use the following definition:

### OCaml
let no_blank buffer position = buffer, position
###

If you want to parses blank according to a regexp, you may use
the function ``blank_regexp : Str.regexp -> blank`` as in:

### OCaml
let blank = blank_regexp (Str.regexp "[ \t\n\r]*")
###

Important remark: due to the fact that OCaml's ``Str`` module
does not allow to match other data type than string, 
a blank function using regexp that matches newline will be
applied successively to lines that contains only blank.
This means that only regexp that are idempotent should 
be used when they match newline.

Otherwise, you may read yourself the input buffer
using the ``Input.read`` function. Here is for instance
a blank function parsing at most one newline and all blank caractere:

### OCaml
let blank = 
  let rec fn accept_newline buffer pos =
    let c,buffer',pos' = Input.read buffer pos in
    match c with
      '\n' when accept_newline -> fn false buffer' pos'
    | ' ' | '\t' | '\r' ->  fn false buffer' pos'
    | _ -> buffer, pos
  in fn true
###   
  
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

