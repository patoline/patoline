(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

open Charset
open Input

(** DeCaP (Delimited Continuation Parser): a minimalist combinator library. *)

(** {2 Exceptions} *)

(** [Parse_error (fn, lnum, col, msgs, expected)] is raised when the input cannot be
  parsed. It provides the file name [fn], line number [lnum] and column
  number [col] of the last succesfully parsed terminal. The list [msgs]
  contains a list of error messages and [expected] contains a description of the tokens
  that would have allowed the parsing process to continue. Normally at least [msgs]
  or [expected] is non empty *)
exception Parse_error of string * int * int * string list * string list

(** [give_up msg] can be called when a parsing rule needs to be rejected. It
  is strongly advised to provide a very explicit message [msg] while raising
  this exception, in order for DeCaP to provide useful error messages. *)
val give_up : string -> 'a

(** {2 Blank functions} *)

(** Type of functions that are used to parse "blanks" (i.e. parts of the input
  that should be ignored, like spaces or comments). Such function takes as
  input a buffer and a position [pos], and returns the new buffer together
  with the position [pos'] of the first meaningful character. This means that
  everything that stands between [pos] (included) and [pos'] (excluded) should
  be ignored. *)
type blank = buffer -> int -> buffer * int

(** [no_blank] is the blank function accepting no charaters *)
val no_blank : blank

(** [blank_regexp re] produces a blank function using a regular
  expression [re] following the syntax from the [Str] module. There is
  an important limitation regarding regular expressions containing the
  newline symbol [\n], due to the fact that the [Str] module only
  matches on strings (and not on an abstract notion of buffer). Such
  regular expressions should be idempotent, and match a string
  containing only a newline. *)
val blank_regexp : string -> blank

(** {2 Core type} *)

(** Type of a grammar returning a value of type ['a]. *)
type 'a grammar

(** {2 Parsing functions} *)

(** [parse_buffer g bl buf] parses input from the buffer [buf] using the
  grammar [g] and the blank function [bl]. *)
val parse_buffer : 'a grammar -> blank -> buffer -> 'a

(** [parse_string ~fn g bl str] parses the string [str] using the grammar [g]
  and the blank function [bl]. A filename [fn] can be provided in order to
  obtain better error messages. *)
val parse_string : ?filename:string -> 'a grammar -> blank -> string -> 'a

(** [parse_channel ~fn g bl ic] loads the content of the input channel [ic]
  and parses it using the grammar [g] and the blank function [bl]. A file name
  [fn] can be provided in order to obtain better error messages. *)
val parse_channel : ?filename:string -> 'a grammar -> blank -> in_channel
                    -> 'a

(** [parse_file g bl fn] opens the file [fn] and parses it using the grammar
 * [g] and the blank function [bl]. *)
val parse_file : 'a grammar -> blank -> string -> 'a

(** [partial_parse_buffer g bl buf pos] parses input from the buffer [buf],
  starting a position [pos], using the grammar [g] and the blank function
  [bl]. A triple is returned containing the new buffer, the position that was
  reached during parsing, and the result of the parsing. This function should
  NOT be used in a  [black_box]. *)
val partial_parse_buffer : 'a grammar -> blank -> buffer -> int
                           ->  'a * buffer * int

(** [partial_parse_string fn g bl str pos] parses input from the string [str],
  starting a position [pos], using the grammar [g] and the blank function
  [bl]. A triple is returned containing the new buffer, the position that was
  reached during parsing, and the result of the parsing. The optional file
  name [fn] is provided to obtain better error messages. This function should
  NOT be used in a  [black_box]. *)
val partial_parse_string : ?filename:string -> 'a grammar -> blank -> string
                           -> int -> 'a * buffer * int

(** {2 Atomic parsers} *)

(** [eof v] is a grammar that parses the end of file character [EOF], and
  returns [x]. *)
val eof : 'a -> 'a grammar

(** [any] is a grammar that parses any character (that is not [EOF], and
  returns it. *)
val any : char grammar

(** [empty v] is an empty grammar, it does not parse anything and always
  succeeds. It returns a the provided value [v]. *)
val empty : 'a -> 'a grammar

(** [debug msg] does not parse anything and always succeeds. It prints the
  message [msg] on [stderr] for debugging. *)
val debug : string -> unit grammar
val active_debug : bool ref

(** [fail msg] always fails, adding [msg] to the list of messages int the
  list of messages to be reported by [Parse_error]. *)
val fail : string -> 'a grammar

(** [black_box fn cs accept_empty name] is a grammar that parses the input
  buffer using the the provided function [fn]. [fn buf pos] should start
  parsing [buf] at position [pos], and return a couple containing the new
  buffer and the position of the first unread character. The character set
  [cs] should contain at least the characters that are accepted as first
  character by the parser, and no less. The boolean [accept_empty] shoud be
  set to [true] if the parsing function [fn] accepts the empty string, and to
  false otherwise. The [name] argument is used to give more readable error
  messages. In case of parse error, the function [fn] should raise the
  exception [Give_up msg], where [msg] is an explicit error message. *)
val  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> 'a option
                 -> string -> 'a grammar

(** [internal_parse_buffer g bl buf pos] can be used to parse using a
  grammar in the definition of a [black_box]. *)
val internal_parse_buffer : 'a grammar -> blank -> buffer -> int
                            -> 'a * buffer * int

(** [char c v] is a grammar that parses the character [c] and returns [v]. *)
val char : char -> 'a -> 'a grammar

(** [string str v] is a grammar that parses the string [str] and returns
  [v]. *)
val string : string -> 'a -> 'a grammar

(** [regexp re ?name g] is a grammar that parses the input according to the
  regular expression [re], and returns a value build by applying the function
  [g] to a function of type [int -> string] that returns the substring matched
  by the [n]-th match group of the regular expression [re] (as in the [Str]
  module). The optional [name] argument is used to give more readable error
  messages while refering to the regular expression [re]. *)
val regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a grammar

(** {2 Combinators acting on blanks} *)

(** [change_layout ~nbb ~oba g bl] replaces the default blank function with
  [bl] while parsing with using the grammar [g]. The optional parameter [nbb],
  which is [true] by default, forces the application of the new blank
  function, before parsing the first terminal of [g]. Note that the old blank
  function is always called before the first terminal of [g]. Similarly, the
  optional parameter [oba], which is [true] by default, forces a call to the
  old blank function, after the end of the parsing of [g]. The new blank
  function is always called after the last terminal. *)
val change_layout : ?new_blank_before:bool -> ?old_blank_after:bool
                    -> 'a grammar -> blank -> 'a grammar

(** [ignore_next_blank g] disables the call to the blank function before the
  first terminal of [g]. If the empty input is parsed using [g], blanks are
  ignored in the usual way ([ignore_next_blank empty] is equivalent to
  empty). *)
val ignore_next_blank : 'a grammar -> 'a grammar

(** {2 Sequencing combinators} *)

(** [sequence g1 g2 f] is a grammar that first parses using [g1], and then
  parses using [g2]. The results of the sequence is then obtained by applying
  [f] to the results of [g1] and [g2]. *)
val sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

(** [sequence_position g1 g2 f] is a grammar that first parses using [g1], and
  then parses using [g2]. The results of the sequence is then obtained by
  applying [f] to the results of [g1] and [g2], and to the positions (i.e.
  buffer and index) of the corresponding parsed input.

  Remark: [sequence g1 g2 f] is equivalent to
  [sequence_position g1 g2 (fun r1 r2 _ _ _ _ -> f r1 r2)]. *)
val sequence_position : 'a grammar -> 'b grammar
                        -> ('a -> 'b -> buffer -> int -> buffer -> int -> 'c)
                        -> 'c grammar

(** [fsequence g1 g2] is a grammar that first parses using [g1], and then
  parses using [g2]. The results of the sequence is then obtained by applying
  the result of [g1] to the result of [g2].

  Remark: [fsequence g1 g2] is equivalent to
  [sequence g1 g2 (fun x f -> f x)]. *)
val fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar

(** [fsequence_position g1 g2] is a grammar that first parses using [g1], and
  then parses using [g2]. The results of the sequence is then obtained by
  applying the result of [g1] and position information (see the definition of
  [sequence_position]) to the result of [g2]. *)
val fsequence_position : 'a grammar
                    -> ('a -> buffer -> int -> buffer -> int -> 'b) grammar
                    -> 'b grammar

(** [sequence3] is similar to [sequence], but it composes three grammars into
  a sequence.

  Remark: [sequence3 g1 g2 g3 f] is equivalent to
  [sequence (sequence g1 g2 f) g3 (fun f x -> f x)]. *)
val sequence3 : 'a grammar -> 'b grammar -> 'c grammar
                -> ('a -> 'b -> 'c -> 'd) -> 'd grammar

(** [dependent_sequence g1 g2] is a grammar that first parses using [g1],
  which returns a value [x], and then continues to parse with [g2 x] and
  return its result. *)
val dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar

(** [conditional_sequence cond g1 g2 f] is a grammar that first parses using [g1],
  which returns a value [x], and if [cond x] is true then if
  continues to parse with [g2] which returns [y] and return [f x y]. *)
val conditional_sequence : 'a grammar -> ('a -> bool) -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

(** {2 Misc} *)

val iter : 'a grammar grammar -> 'a grammar
  (* = fun g -> dependent_sequence g (fun x -> x) *)

(** [option v g] tries to parse the input as [g], and returns [v] in case of
  failure. *)
val option : 'a -> 'a grammar -> 'a grammar

(** [fixpoint v g] parses a repetition of zero or more times the input parsed
  by [g]. The value [v] is used as the initial value (i.e. to finish the
  sequence). *)
val fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar

(** [fixpoint v g] parses a repetition of one or more times the input parsed
  by [g]. The value [v] is used as the initial value (i.e. to finish the
  sequence). *)
val fixpoint1 : 'a -> ('a -> 'a) grammar -> 'a grammar

(** [alternatives [g1;...;gn]] tries to parse using all the grammars
  [[g1;...;gn]] and keeps only the first success. *)
val alternatives : 'a grammar list -> 'a grammar

(** the only difference between the next function and the previous one
    if that they do not backtrack if they completely parse an object
    of type 'a grammar.
 *)
val option' : 'a -> 'a grammar -> 'a grammar
val fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
val fixpoint1' : 'a -> ('a -> 'a) grammar -> 'a grammar
val alternatives' : 'a grammar list -> 'a grammar

(** [apply f g] applies function [f] to the value returned by the grammar
  [g]. *)
val apply : ('a -> 'b) -> 'a grammar -> 'b grammar

(** cache the parsing of the given grammar. This assumes that object of type
  'a can be compared using OCaml's polymorphic equality. This allows parsing
  of any bnf in polynomial time if there are only a polynomial number of
  possible actions. The cache is reset by the parsing function. *)
val cache : 'a grammar -> 'a grammar

(** [apply_position f g] applies function [f] to the value returned by the
  grammar [g] and the positions at the beginning and at the end of the
  input parsed input. *)
val apply_position : ('a -> buffer -> int -> buffer -> int -> 'b)
                     -> 'a grammar -> 'b grammar

(** [position g] tranforms the grammar [g] to add information about the
  position of the parsed text. *)
val position : 'a grammar -> (string * int * int * int * int * 'a) grammar

(* [delim g] behaves the same as [g], but backtraching is prevented beyond
  grammar [g]. *)
val delim : 'a grammar -> 'a grammar

(* [lists g] lists all the possible parse-trees produced by grammar [g].
  The parse-trees do not need to correspond to the same initial segment of
  the input. *)
val lists : 'a grammar -> 'a list grammar

val merge : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a grammar -> 'b grammar

(** {2 Support for recursive grammars} *)

(** [declare_grammar name] returns a new grammar that can be used in the
  definition of other grammars, but that cannot be used before it is
  initialized with the function [set_grammar]. The argument [name] is used to
  provide better error messages by naming the grammar. *)
val declare_grammar : string -> 'a grammar

(** [set_grammar g g'] set the definiton of grammar [g] (that was previously
  declared using [declare_grammar]), to be [g']. *)
val set_grammar : 'a grammar -> 'a grammar -> unit

(** The too previous function allow for recursive grammar, but left recursion
  is forbidden and triggers a [Failure] exception.

  For instance the following code is incorrect:

  {[
    let p = declare_grammar ()
    let p' = sequence (sequence (option (string "a" ["a"])) p) (string "b" "b") (fun l x -> x::l)
    let _ = set_grammar p p'
  ]}
*)

(** [grammar_familly to_str name] returns a pair [(gs, set_gs)], where [gs]
  is a finite family of grammars parametrized by a value of type ['a]. A name
  [name] is to be provided for the family, and an optional function [to_str]
  can be provided to print the parameter and display better error messages. *)
val grammar_family : ?param_to_string:('a -> string) -> string
                     -> ('a -> 'b grammar) * (('a -> 'b grammar) -> unit)

(**
  {[
    (* Declare the grammar family *)
    let (gr, set_gr) = grammar_family to_str name in

    ... code using grammars of gr to define mutually recursive grammars ...
    ... the grammars in gr cannot be used in "left position" ...
    ... (same restriction as for declare_grammar ...

    (* Define the grammar family *)
    let _ = set_gr the_grammars

    ... now the new family can be used ...
  ]}
*)

(** {2 Exception handling and debuging} *)

(** [print_exception e] displays the DeCaP exception [e] in a human-readable
  way. *)
val print_exception : exn -> unit

(** [handle_exception f x] applies the function [f] to [x] while handling
  DeCaP exceptions. In particular it notifies parse errors. The exception
  [Failure "No parse."] is raised when applicable. *)
val handle_exception : ('a -> 'b) -> 'a -> 'b

(** [accept_empty g] returns [true] if the grammar [g] accepts the empty input
  and [false] otherwise. *)
val accept_empty : 'a grammar -> bool

(** {2 Advanced blank function definition} *)

(** [blank_grammar grammar blank] produces a blank function using the grammar
  [grammar] and the blank function [blank]. It parses the input using the
  [partial_parse_buffer] function and returns the position reached. *)
val blank_grammar : unit grammar -> blank -> buffer -> int -> buffer * int

(* developper only *)
val debug_lvl : int ref
