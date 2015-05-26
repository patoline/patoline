== Input buffer and pre-processing ==

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

== Blank functions ==

While a string or file is being parsed, it is required to differentiate parts
of the input that are meaningful, from those that need to be ignored.
This part of the work is usually handled by a lexer, but ##DeCaP## relies on
another mechanism: blank functions.

A blank function inspects the input buffer at a given position, and returns
the position of the next meaningful character (i.e. the next character that is
not to be ignored). The type of a blank function is
##blank = buffer -> int -> buffer * int##. The simplest possible blank
function is one that does not ignore any character:

### OCaml

  let no_blank buf pos = (buf, pos)

###

It is possible to eliminate blanks according to a regular expression. To do so,
the function ##blank_regexp : string -> blank## may be used. In the
following example, the blank function ignores an arbitrary number of spaces,
tabulations, newline and carriage return characters:

### OCaml

  let blank_re = blank_regexp "[ \t\n\r]*"

###
//Important remark: due to a limitation of the ##Str## module (which can only
match regular expressions against strings), the current implementation of
##DeCaP## does not behave well on regular expressions containing the new line
symbol (since the ##Input## module is implemented using a stream of lines). A
blank function using a regular expression containing a new line symbol will be
applied to several lines in turn if they are matched completely. This means
that a regular expression containing a new line symbol will behave correctly
only if it is idempotent.//

Another way to build a blank function is to directly read the input buffer
using the ##Input.read## function. For example, the following blank function
ignores any number of spaces, tabulations, and carriage return symbols, but
at most one new line symbol:

### OCaml

  let blank_custom = 
    let rec fn accept_newline buffer pos =
      let (c, buffer', pos') = Input.read buffer pos in
      match c with
      | '\n' when accept_newline -> fn false buffer' pos'
      | ' ' | '\t' | '\r'        -> fn accept_newline buffer' pos'
      | _                        -> buffer, pos
    in fn true

###

As there is no limitation as to what can be used to write a blank function,
one could even decide to use the function ##partial_parse_buffer## (see
##decap.mli##) and use a ##DeCaP## parser to parse blanks.

== Parsing functions ==

The ##DeCaP## library exports an abstract type ##'a grammar## which is the
type of a function parsing a ##buffer## and returning data of type ##'a##.
Given a blank function and a grammar (i.e. an object of type ##'a grammar##),
input is parsed from a ##string##, input channel, file or ##buffer## using one
of the following functions:

### OCaml

  parse_string  : ?filename:string -> 'a grammar -> blank -> string -> 'a
  parse_channel : ?filename:string -> 'a grammar -> blank -> in_channel -> 'a
  parse_file    : 'a grammar -> blank -> string -> 'a
  parse_buffer  : 'a grammar -> blank -> buffer -> 'a

###

Every parsing function take as input a default blank function that will be
used to discard blank characters before every terminal is parsed. This
blank function can be changed at any time by calling the function
##change_layout## in a grammar (see ##decap.mli## and the next sections).

Note that the functions ##parse_string## and ##parse_channel## have an
optional argument ##filename## which is used to report better error messages.
The reader should refer to the file ##decap.mli## (or to the associated
generated ##ocamldoc## file) for more details.

All of the parsing functions above either succeed in parsing all the input,
or fail with an exception (##Parse_error## for example). The function
##handle_exception## is provided for this reason: it handles exceptions and
displays a human-readable error message.

###

  handle_exception : ('a -> 'b) -> 'a -> 'b

###

Parsing functions working on only part of the input are also provided. The
reader should again refer to the file ##decap.mli## for more details. These
functions can be used to implement a blank function using a parser, as was
noted at the end of the section about blank functions.

