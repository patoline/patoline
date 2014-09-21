==============================================================================
  Getting started writing parsers and syntax extensions using ##DeCaP## and
  ##pa_ocaml##
------------------------------------------------------------------------------
  Rodolphe Lepigre & Christophe Raffalli
------------------------------------------------------------------------------
  Lama, UMR 5127 CNRS, Université Savoie Mont-Blanc
==============================================================================

\linesBefore(3)
//Delimited Continuation Parser// (or ##DeCaP##) is a small parser combinator
library written in ##OCaml##. Unlike most combinator libraries, its
performance is close to that of ##ocamlyacc##, as it relies heavily on
//continuation passing style// (CPS). ##DeCaP## provides a notion of //blank
function// which is used to discard parts of the input that should be ignored
(comments for example). Ambiguous grammars can be handled either by returning
the list of all possible parse trees or by raising an error in case of
ambiguity.

##DeCaP## has been used to write a full-featured ##OCaml## parser called
##pa_ocaml##, which operates at more than twice the speed of ##camlp4##, and
is only five times slower than the original ##OCaml## parser (written with
##ocamlyacc##). When used in conjunction with ##DeCaP##, ##pa_ocaml## offers
a simple and integrated way to write parsers, and even syntax extensions for
the ##OCaml## language. It is intended to be simpler to use than ##camlp4##,
and does not require the use of a lexer, which gives the user more freedom to
write syntax extensions. A quotation and anti-quotation mechanism similar to
that of ##camlp4## is provided, which greatly simplifies the writing of syntax
extensions.

This document is not intended as a full documentation of ##DeCaP## and
##pa_ocaml##, but rather as a quick description of their main features,
illustrated with simple examples. It should contain enough material to get
started writing parsers and syntax extensions, while providing an overall
idea of the working principles of ##DeCaP## and ##pa_ocaml##.

Even though ##DeCaP## can be used directly, a ##pa_ocaml## syntax extension
called ##pa_parser## allows the user to write parsers using a BNF-like
syntax. There is, however, one main restriction: //left recursive// grammars
are forbidden.

-- Input buffer and pre-processing --

The ##Input## module exports an abstract type ##buffer##, on which ##DeCaP##
relies for parsing. Several functions are provided for creating a buffer from
a file, an input channel or a ##string##. The reader can refer to the
interface file ##input.mli## (or to the associated generated ##ocamldoc##
file) for the full details.

We give bellow the type of the three main buffer-creating functions, but we do
not describe them in detail since their names and types should be explicit
enough for the reader to guess the usage. Note, however, that the functions
##buffer_from_string## and ##buffer_from_channel## have an optional argument
##filename## which is used to report better error messages.

###
  buffer_from_file : string -> buffer
  buffer_from_channel : ?filename:string -> in_channel -> buffer
  buffer_from_string : ?filename:string -> string -> buffer
###

We will see later that in some case it might be useful to manually read input
from the ##buffer##. This can be done with the function ##read##, which takes
as input a buffer and a position, and returns a triple containing the
character read, the new state of the buffer, and the position of the next
character.

###
  read : buffer -> int -> char * buffer * int
###

In the current implementation, the ##Input## module comes with a built-in
C-like pre-processor which is very useful to support several versions of
##OCaml##. In the near future, this feature will be disabled by default, as
it might be harmful and prevent the parsing of some languages.

-- Blank functions --

While a string or file is being parsed, it is required to differentiate parts
of the input that are meaningful, from those that need to be ignored.
This part of the work is usually handled by a lexer, but ##DeCaP## relies on
another mechanism: blank functions.

A blank function inspects the input buffer at a given position, and returns
the position of the next meaningful character (i.e. the next character that is
not to be ignored). The type of a blank function is the following:

### OCaml
blank = buffer -> int -> buffer * int
###

The simplest possible blank function is a function that ignores no character.
It simply returns the position and buffer that it was given as an argument:

### OCaml "blank0.ml"
let no_blank buffer position = buffer, position
###

It is possible to eliminate blanks according to a regual expression. To do so,
the function ##blank_regexp : Str.regexp -> blank## may be used. In the
following example, the blank function ignores an arbitrary number of spaces,
tabulations, newline and carriage return characters:

### OCaml
let blank = blank_regexp (Str.regexp "[ \t\n\r]*")
###

Important remark: due to the fact that ##OCaml##'s ##Str## module does not
allow to match other data type than string, a blank function using regular
expression that matches newline will be applied successively to lines that
contain only blank. This means that only regexp that are idempotent should 
be used when they match newline.

Otherwise, you may read yourself the input buffer using the ##Input.read##
function. Here is for instance a blank function parsing at most one newline
and all blank characters:

### OCaml "blank1.ml"
let blank = 
  let rec fn accept_newline buffer pos =
    let (c, buffer', pos') = Input.read buffer pos in
    match c with
    | '\n' when accept_newline -> fn false buffer' pos'
    | ' ' | '\t' | '\r'        -> fn accept_newline buffer' pos'
    | _                        -> buffer, pos
  in fn true
###

Remark: by using the function ##partial_parse_buffer## (see ##decap.mli##),
it is even possible to use a parser to write a blank functions.

-- Parsing functions --

The ##DeCaP## library exports an abstract type ##'a grammar## which is the
type of a function parsing a ##buffer## and returning data of type ##'a##.
Given a blank function and a grammar (i.e. an object of type ##'a grammar##),
input is parsed from a ##string##, input channel, file or ##buffer## using one
of the following functions.

### OCaml
  parse_string  : ?filename:string -> 'a grammar -> blank -> string -> 'a
  parse_channel : ?filename:string -> 'a grammar -> blank -> in_channel -> 'a
  parse_file    : 'a grammar -> blank -> string -> 'a
  parse_buffer  : 'a grammar -> blank -> buffer -> 'a
###

Note that the functions ##parse_string## and ##parse_channel## have an
optional argument ##filename## which is used to report better error messages.
The reader should refer to the file ##decap.mli## (or its associated generated
##ocamldoc## documentation) for more details. For instance, functions for
parsing only part of the input are also provided.

All of the parsing functions either succeed in parsing all the input, or
fail with an exception (##Parse_error for example##). The function
##handle_exception## is provided for this reason: it handles exceptions and
displays a human-readable error message.

###
  handle_exception : ('a -> 'b) -> 'a -> 'b
###

When a parser is invoked (using the function ##parse_string## for example), a
default blank function needs to be provided. It will then be used to discard
characters before every terminal is parsed.

-- Changing the layout of blanks --

On important feature if that the blank function can be changed using the function:

### OCaml
change_layout : ?old_blank_before:bool -> ?new_blank_after:bool -> 
  'a grammar -> blank -> 'a grammar
###

The grammar returned by ##change_layout parser blank## will only use
the provided blank function and ignore the old one. Since blank functions
are called before every terminals, it is not clear whether the old blank
function should be called before entering the scope of the ##change_layout##,
and whether the new blank function should be called after leaving the scope of
the ##change_layout##.

The first optional argument ##old_blank_before## (##true## by default) will
force using first the old blank function, and then the new one, before parsing
the first terminal inside the scope of the ##change_layout##.

Similarly, ##new_blank_after## (##false## by default) will forces to use the
newly provided blank function once at the end of the parsed input, and then
the old blank function will be used too as expected before the next terminal.

== Writing parsers ==

The combinators in the ``Glr`` library are not easy to use
directly. An OCaml's syntax extension is provided to solve this problem.

To use this syntax extension, you have to use the binary ``pa_ocaml``
distributed with ``Glr`` as an OCaml preprocessor, using the 
``-pp pa_ocaml`` option to ``ocamlc`` or ``ocamlopt``.

A parser is declared using ``parser`` //rules// or 
``parser*`` //rules//. The second form will return
all the possible parse trees for the given grammar.
This is therefore an expression of type ``'a list grammar``.

The first form will raise the exception ``Ambiguity(...)`` when there
are multiple parse trees for the same input.

We now give the BNF for the grammar, using the following convention:
"|"  denotes alternatives, "[ … ]" optional elements, 
"(…)*" repetition 0 or more times and "(…)+" 
repetition 1 or more times. When one of the symbol above
is used in the grammar being described, it is embraced by simple quotes.
 
\begin{itemize}
\item //rules// ::= //rules// '##|##' //rules// | //rules// '##| |##' //rules// | //left// ##->## //expression//
\item //left//  ::= //let_binding// //left// | - //left// | //left// ##->>## //left// | ([//pattern//##:## ] //parser// [//option//] [//modifier//])+
\item //parser// ::= //terminal// | //atom_expression// | ##{## //rules// ##}##
\item //terminal// ::= ##ANY## | ##CHR## //atom_expression// | ##STR## //atom_expression// | ##RE## //atom_expression//
| ##EOF##

| ##EMPTY## | ##FAIL## | ##DEBUG## //atom_expression//
\item //option// ::= '##[##' //expression// '##]##'
\item //modifier// ::= ##?## | ##??## | ##*## | ##**## | ##+## | ##++##
\end{itemize}

In this grammar, //let_binding// and //expression// correspond to their equivalent in OCaml.
//atom_expression//, mean an expression with the priority level just before function application, which means 
that we can use, without parenthesis, identifiers and projection.

We also see every constructor that is not a terminal in two versions. The version were the symbol is
doubled will not explore all possibilities: once a parse tree has been found it does not backtrack.
We also have to kind of alternatives in the same spirit (more or less backtracking).
We will come back to this later.

Lets give a first small example, a calcultor with just the addition and substraction:

### OCaml "example1.ml"
  let int = parser n:RE("[0-9]+") -> int_of_string n
  let op  = parser CHR('+') -> (+) | CHR('-') -> (-)
  let expression = parser
    n:int l:{ op:op m:int -> (op,m) }* ->
      List.fold_left (fun acc (op,f) -> op acc f) n l
###

Let us first details the terminal with the meaning of their option when option are available:

\begin{itemize}
\item ##ANY##: parses one char, except end of file but including newline and returns this char.
\item ##EOF##: parses end of file only and returns the given expression, unit if the option is not given. This is almost always useless because the function calling the parser (except ##partial_parse_XXX##)
automaticaly add ##EOF## at the end of the given grammar.
\item ##EMPTY[##//result//##]##: parses nothing and returns //result//, unit if //result// is not given.
\item ##FAIL[##//message//##]##: fail immediately. The given expression will appear in the error //message//.
\item ##DEGUG## //message//: parses nothing but print information on ##stderr##, including the given //message//.
\item ##CHR## //char// [//result//]:
parses only the given //char// and return the given //result//. Returns the parsed //char// if the option was not given.
\item ##STR## //string// [//result//]:
parses only the given //string// and return the given //result//. Returns the parsed //string// if the option was not given.
\item ##RE## //regexp// [//result//]:
parses only the given //regexp// of type ##Str.regexp## and returns the given //result//.
The identifier ##group## is bound in //result// and ##group## //n//
will return the corresponding matched group. 

Limitation: the regexp can at most parse one line and can not parse newlines. You must use the other terminal if you want to parse 
newline.
\end{itemize}

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

(* The main loop *)
let _ =
  try while true do (* we use the Glr function provided to handle exception *)
    handle_exception (fun () ->
      Printf.printf ">> %!";
      (* we call the parser with the choosen blank function *)
      let x = parse_string command blank (input_line stdin) in
      Printf.printf "=> %f\n%!" x) ()
  done with End_of_file -> ()
###

