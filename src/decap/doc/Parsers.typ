== Writing parsers ==

The combinators provided by the ##DeCaP## library are not easy to use
directly. That is why an ##OCaml## syntax extension called ##pa_parser## is
distributed along ##pa_ocaml##. It allows the user to write parsers using a
BNF-like syntax. ##OCaml## programs written using this syntax extension need
to be compiled using the ##-pp pa_ocaml## option of ##ocamlc## or
##ocamlopt##. The ##pa_parser## extension is enabled by default when using
the ##pa_ocaml## parser, but this behaviour can be changed.

The entry point of the ##pa_parser## syntax extension is a new expression
delimited by the keyword ##parser##, which is followed by an optional ##*##
symbol and the BNF rule for the grammar. If there is no ##*## symbol, the
parser will raise an exception in case of ambiguity. Otherwise, the list of
every possible parse tree is returned by the parser, which will have a type of
the form ##'a list grammar##.

We give below the BNF specification of the ##pa_parser## syntax extension,
using the following convention: ##|## separates alternatives, ##[…]##
delimits optional elements and ##(…)+## elements repeated one or more
times. Terminal symbols are wrapped into double quotes, and entry points are
wrapped into chevrons. Several entry points of the ##OCaml## language are
used: ##<expr>##, ##<expr_atom>##, ##<let_binding>##, ##char_literal## and
##string_literal##. They refer to expressions (any priority level),
expression at the level of atoms (for example constants, identifiers,
projections and anything between parenthesis) and ##let ... in## bindings
respectively.

###

<expr>     ::= ...
             | "parser" ["*"] ["|"] <rule>

###

###

<rule>     ::= <rule> "|"  <rule>
             | <rule> "||" <rule>
             | <left> "->" <expr>

<left>     ::= <let_binding> <left>
             | "-" <left>
             | <left> "->>" <left>
             | ([<pattern> ":"] <parser> [<option>] [<modifier>])+

<parser>   ::= <terminal>
             | <atom_expr>
             | "{" <rule> "}"

<terminal> ::= "ANY" | "EOF" | "EMPTY" | "FAIL"
             | "CHR"   <atom_expr>
             | <char_literal>
             | "STR"   <atom_expr>
             | <string_literal>
             | "RE"    <atom_expr>
             | <regexp_literal>
             | "DEBUG" <atom_expr>

<option>   ::= "[" <expr> "]"
<modifier> ::= "?" | "??" | "*" | "**" | "+" | "++"

###

Before entering into the details of the composition of parsers using modifiers
or the alternative marker ##|## and ##||##, we will describe the action of
each terminal:
\begin{itemize}
\item ##ANY## parses one character (that is not the end of file character),
      and the result of the parsing is the parse character. The type of this
      atomic parser is hence ##char grammar##.
\item ##EOF## parses the end of file character, and returns the expression
      contained in the option field, or unit if it is absent. This terminal
      is almost always useless because all the parsing functions that parse
      the whole input automatically append ##EOF## at the end of the given
      grammar (this is not the case for ##partial_parse_string## for example).
\item ##EMPTY## parses nothing and always succeeds. It returns the
      expression contained in the option field, or unit if it is absent.
\item ##FAIL## fails immediately. If there is an option field, the value of
      The given expression will appear in the error message.
\item ##DEBUG msg## parses nothing but print debugging information including
      the given string ##msg## to ##stderr##.
\item ##CHR c## parses the character ##c## and returns the expression
      contained in the option field, or the parsed character if the option
      is absent. If ##c## is a character literal, ##CHR## can be omitted.
\item ##STR s## parses the string ##s## an returns the expression contained
      in the option field, or the parsed string if the option is absent. If
      ##s## is a string literal, ##STR## can be omitted.
\item ##RE r## parses the input according to the regular expression ##r##,
      which should be a ##string## formated as described in the documentation
      of the ##Str## module. If the option field is not provided, the value
      returned by the parser if the ##string## that was matched. Otherwise,
      the value of the optional field is returned. Note that the identifier
      ##group## is bound in the optional field, and can be used to compute
      the return value of the parser. It corresponds to a function that
      maps the natural integer ##n## to the ##n##-th matched group of the
      regular expression.

      \begin{noindent}
      //Important remark: due to a limitation of the ##Str## module (which can
      only match regular expressions against strings), the current
      implementation of ##DeCaP## does not behave well on regular expressions
      containing the new line symbol (since the ##Input## module is
      implemented using a stream of lines). Hence, regular expression in the
      ##RE## terminal should not contain the new line symbol.//
      \end{noindent}

      You may yse ##''regexp''## for a constant regexp. This is a syntax for
      string constant adapted for regular expression. No need to escape the "##\##" character.
\end{itemize}

(* FIXME hack for correct indentation... *)
###

###

The usual BNF modifiers for optionality (##?##), repetition zero or more
times (##*##) and repetition one or more times (##+##) come in two versions.
The usual symbols (i.e. the ones that are not doubled) behave in the usual
way, in the sense that backtracking is used to explore every possible parse
tree. The symbols that are doubled (##??##, ##**##, ##++##) backtrack less, and
stop backtracking when one parse tree has been found. We also have two kinds
of alternative symbols: The usual ##|## symbol backtracks and explores every
alternative, while the alternative symbol ##||## backtracks less, and only
explores the alternative that parses the more input. Note that there should be
no difference on non-ambiguous grammars.

Let us now give a first example of a parser, implementing a very simplistic
calculator having as only operations addition and subtraction. The BNF
grammar of the parsed language will be the following, where ##<int>##
matches any integer:

###

<op>   ::= "+" | "-"
<expr> ::= <int> (<op> <int>)*

###

This grammar can be translated to ##pa_parser## syntax in a straight forward
way. The following ##OCaml## program will parse and compute the result of
any valid string it receives as a command-line argument.

###

###
### OCaml "calc_base.ml"
open Decap

let int = parser
  | n:''[0-9]+'' -> int_of_string n
let op = parser
  | '+' -> (+)
  | '-' -> (-)
let expr = parser
  | n:int l:{op:op m:int -> (op,m)}* ->
      List.fold_left (fun acc (op,f) -> op acc f) n l

let parse =
  let blank = blank_regexp ''[ \t]*'' in
  handle_exception (parse_string ~filename:"arg" expr blank)

let _ =
  let cmd = Sys.argv.(0) in
  match Sys.argv with
  | [|_;s|] -> Printf.printf "%s = %i\n" s (parse s)
  | _       -> Printf.fprintf stderr "Usage: %s \"1 + 2 - 4\"\n" cmd
###

== Declaration of grammars and grammar families ==

Until now, we never wrote any recursive grammar and rather handled repetition
using modifiers such as ##+## or ##*##. In order to define a recursive
grammar, one should define it using the ##parser## keyword at the level of definitions, using the following syntax:

###
<definition> ::= ...
               | "let" "parser" <lowercase-ident> [<pattern>] [":" <typexpr>] "=" <rule>
###

The ##pattern##, is an optional argument that we will describe letter.

(*
first declare it using the function ##declare_grammar##,
which take as input a ##string##, which is to refer to the grammar in error
messages.

###

declare_grammar : string -> 'a grammar

###

After the grammar has been declared, it can be use in the definition of any
other grammar. The definition of the grammar should the be given by calling
the function ##set_grammar##, which takes as first argument the grammar that
was returned by the ##declare_grammar## function, and as second argument
another grammar that will be take as its definition.

###

set_grammar : 'a grammar -> 'a grammar -> unit

###
*)

As a first very simple example of recursive grammar, we can construct a
parser that accepts the following the following BNF grammar, where ##<empty>##
designates the empty input:

###

<aabb> ::= "a" <aabb> "b"
         | <empty>

###

The following program takes its input from ##stdin##, and terminates if the
contents of the input can be parsed by the grammar. In case of error, the
program fails and prints an error message.

###

###
### OCaml "aabb.ml"
open Decap

let parser aabb =
  | 'a' aabb 'b'
  | EMPTY

let parser aabb_eof =
  | aabb EOF

let _ =
  let no_blank buf pos = (buf, pos) in
  handle_exception (parse_channel aabb_eof no_blank) stdin
###
###

###

One often needs to define a family of mutually recursive grammars depending on
a parameter. This can be used, for example, to parse a grammar of expressions
having different precedence levels. In order to define a grammar family, one
first needs to call the function ##grammar_family##, which takes as argument
an optional function for printing the argument type into a string, and a name
for the grammar. These two arguments are used to provide better error
messages. The function then returns a couple of a family of grammars (in the
form of a function from the parameter type ##'a## to grammars returning a
value of type ##'b##), and a function to be called to define the grammar
family in the end (it more or less plays the same role as the function
##set_grammar## for grammars declared using ##declare_grammar##).

###

grammar_family : ?param_to_string:('a -> string) -> string
                 -> ('a -> 'b grammar) * (('a -> 'b grammar) -> unit)

###

We give below the example of a calculator with addition, subtraction,
multiplication and division. The usual priority of operation is respected
and implemented using a ##grammar_family##.

###

###
### OCaml "calc_prio.ml"
open Decap

type calc_prio = Sum | Prod | Pow | Atom

let float_re = ''[0-9]+\([.][0-9]+\)?\([eE][-+]?[0-9]+\)?''
let parser float_num =
  f:RE(float_re) -> float_of_string f

let parser prod_sym =
  | '*' -> ( *. )
  | '/' -> ( /. )

let parser sum_sym =
  | '+' -> ( +. )
  | '-' -> ( -. )

let parser expr prio : float grammar =
  | f:float_num          when prio = Atom -> f
  | '(' e:(expr Sum) ')' when prio = Atom -> e
  | '-' e:(expr Pow)     when prio = Pow -> -. e
  | '+' e:(expr Pow)     when prio = Pow -> e
  | e:(expr Atom) e':{"**" (expr Pow)}? when prio = Pow ->
      match e' with None -> e | Some e' -> e ** e'
  | e:(expr Pow) l:{prod_sym (expr Pow)}*
                         when prio = Prod ->
      List.fold_left (fun acc (fn, e') -> fn acc e') e l
  | e:(expr Prod) l:{sum_sym  (expr Prod)}*
                         when prio = Sum  ->
      List.fold_left (fun acc (fn, e') -> fn acc e') e l

(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
  try while true do
    Printf.printf ">> %!";
    let l = input_line stdin in
    let r = handle_exception (parse_string (expr Sum) blank) l in
    Printf.printf "%f\n%!" r
  done with End_of_file -> ()
###

(* TODO
   - memoized grammars
*)

== Dependent sequence ==

It is sometimes necessary to build a parser, which parses input depending on
the result of the parsing of previous input. For example, one could want to
parse the representation of an integer ##n## followed by a whitespace, and
then parse exactly ##n## characters. This kind of behaviour can be achieved
by using the ##dependent_sequence## combinator, which corresponds to the
//bind// operation of the underlying monad.

###

dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar

###

A specific syntax is provided for this combinator by the ##pa_parser## syntax
extension: ##<left> "->>" <left>## (see the BNF specification of
##pa_parser##). It is used in the following example, which test whether a word
entered on ##stdin## belongs to the most famous example of a contextual
grammar, which accepts for example the words ##""##, ##"abc"## or ##"aabbcc"##.

###

###
### OCaml "aabbcc.ml"
open Decap

let parser nb_cc nb =
  | 'c' (nb_cc (nb - 1))  when nb > 0
  | EMPTY                 when nb <= 0

let parser aabb =
  | 'a' n:aabb 'b' -> n + 1
  | EMPTY          -> 0

let parser aabbcc =
  | n:aabb ->> (nb_cc n) EOF

let _ =
  let no_blank buf pos = (buf, pos) in
  handle_exception (parse_channel aabbcc no_blank) stdin
###

== Advanced use of blank functions ==

On important feature is that the blank function can be changed using the function:

### OCaml

change_layout : ?new_blank_before:bool -> ?old_blank_after:bool -> 
  'a grammar -> blank -> 'a grammar

###

The grammar returned by ##change_layout parser blank## will use the provided
blank function only and ignore the old one. Since blank functions
are called after every terminals, it is not clear whether the new blank
function should be called before entering the scope of the ##change_layout##,
and whether the old blank function should be called after leaving the scope of
the ##change_layout##.

The first optional argument ##new_blank_before## (##true## by default) will
force using first the old blank function, and then the new one, before parsing
the first terminal inside the scope of the ##change_layout##.

Similarly, ##old_blank_after## (##true## by default) will force to use the
newly provided blank function once at the end of the parsed input, and then
the old blank function will be used too as expected before the next terminal.

As an example, we provide the following program which parses a file containing
text, and counts the number of paragraphs. A paragraph consists in a sequence
of lines containing at least one non-blank character. A paragraph is ended by
leaving a line empty.

###

###
### OCaml "text.ml"
open Decap

let inter_paragraph = blank_regexp ''[ \t\r\n]*''

let inter_word str pos =
  let gram = parser ''[ \t\r]*'' '\n'? ''[ \t\r]*'' in
  let str, pos, _ = partial_parse_buffer gram no_blank str pos in
  str, pos

let word = parser w:''[^ \n\t\r]+'' -> w

let paragraph = change_layout (parser ws:word+ -> ws) inter_word

let text = parser ps:paragraph* -> ps

let _ =
  let ps = handle_exception (parse_channel text inter_paragraph) stdin in
  let nb = List.length ps in
  Printf.printf "%i paragraphs read.\n" nb
###
###

###

Blanks can also be controlled using the combinator ##ignore_next_blank##, which
prevents the parsing of blanks before entering the following grammar.

###

ignore_next_blank : 'a grammar -> 'a grammar

###
This combinator is accessible in the BNF syntax by using a dash symbol after
some left member of a rule (i.e. with the syntax ##<left> -##). This really
is a postfix operator, hence it can be used at the end of a rule.

As an example, the following change in the rules for the unary minus and plus
symbol of the calculator example would forbid blanks to be used between unary
symbols and the following expression:

### OCaml

  | '-' - e:(expr Atom)         when prio = Atom -> -. e
  | '+' - e:(expr Atom)         when prio = Atom -> e
###

== Optimisation and delimited grammars ==

The grammar given above for paragraph is not very efficient (try to parse a
text with a few thousand paragraphs of a few hundred word each...). This
comes from the semantics of the BNF operator ##|##, ##?##, ##*## and ##+##
(which is the usual one). When parsing ##"a"* <h>##, one may need to leave
some characters ##"a"## for ##<h>## to consume (for example, we could have
##<h> ::= "a" "a" "b"##). This means that the parsing of ##<g>*## for a
given grammar ##<g>## will keep an exception handler around the parsing of
the rest of the stream to be able to backtrack. This is done using a
//continuation passing style// approach to parsing combinator. 

This may leads to numerous failed partial parsing attempts which may takes
a lot of time. It may also create to a reasonably fast parser using a
lot of stack, which could lead to a ##Stack_overflow## exception being
raised. In general the above operator are not tail recursive.

In order to prevent this, our parsing combinators systematically compute the
set of allowed characters for the rest of the input stream. This means that if
one consider the grammar ##"a"* "b"##, and parses the string ##"aaaab"##, the
parser will know that it is useless not to parse every character ##"a"## in
the first place. This prediction mechanism is sometimes enough to avoid high
complexity, but not always. It is completely useless in the case of the
sequence of paragraph as we wrote it.

Another way to prevent this is to use the ##delim## combinator. ##delim g##
will not backtrack at all if the grammar ##g## successfully parses an initial
segment of the input.

###

delim : 'a grammar -> 'a grammar

###

The operators ##||##, ##??##, ##**## and ##++## of the BNF syntax simply
correspond to delimited versions of ##|##, ##?##, ##*## and ##+##. This means
for instance that ##g**## is equivalent to ##(delim (g*))## (and similarly for
the other operators).

One should be especially careful with the ##||## alternative operator. For
instance, the following grammar ##g1## will not parse the string  ##"ab"##
while ##g2## will:

###

let g1 = parser                            let g2 = parser
|| "a"     -> ()                           || "a" "b" -> ()
|| "a" "b" -> ()                           || "a"     -> ()
###

== Position in parsers ==

To provide error messages, it is important to be able to obtain the positions
of an element in the parse tree. To do this, one can write a function
##locate## having the following type, where ##position## is a user-defined
type representing a position:

### OCaml

val locate : buffer -> int -> buffer -> int -> position

###

The two first arguments of ##locate## will be the buffer and position at the
beginning of the text being parsed, and the two last arguments give the same
informations for the end of the text.

The following function from the ##Input## module can be used to produce
positions using the data provided by OCaml's ##Lexing## module:

### OCaml

val lexing_position : buffer -> int -> Lexing.position

###
Therefore, a possible implementation for ##locate## could be:

### OCaml

open Decap

type position = Lexing.position * Lexing.position

let locate buf_begin pos_begin buf_end pos_end =
  (Input.lexing_position buf_begin pos_begin,
   Input.lexing_position buf_end pos_end)

###

In order to use positions when defining a parser in //BNF// syntax, the
environment variable ##LOCATE## can be set to contain the name of the user's
locate function. The position are the accessible in the action rules of a
parser by using the variable ##_loc## for the global position and ##_loc_id##
for the position of the left member of the rule named ##id##.

We give below a more complete example, which should work with the above
definition of ##locate##:

### OCaml

 #define LOCATE locate

 let ints: (position * (position * int) list) grammar = 
   parser
     "ints" l:{ i:''[0-9]+'') -> (_loc, int_of_string i) }*
       -> (_loc_l, l) 
###
