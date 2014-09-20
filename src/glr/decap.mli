open Charset
open Input

(** DeCaP (Delimited Continuation Parser): a minimalist combinator library.
  @author Christophe Raffalli *)

(** {2 Exceptions} *)

(** [Parse_error (fn, lnum, col, msgs)] is raised when the input cannot be
  parsed. It provides the file name [fn], line number [lnum] and column
  number [col] of the last succesfully parsed terminal. The list [msgs]
  contains a description of the tokens that would have allowed the parsing
  process to continue. *)
exception Parse_error of string * int * int * string list

(** [Give_up msg] can be raised when a parsing rule needs to be rejected. It
  is strongly advised to provide a very explicit message [msg] while raising
  this exception, in order for DeCaP to provide useful error messages. *)
exception Give_up of string

(** {2 Blank functions} *)

(** Type of functions that are used to parse "blanks" (i.e. parts of the input
  that should be ignored, like spaces or comments). Such function takes as
  input a buffer and a position [pos], and returns the new buffer together
  with the position [pos'] of the first meaningful character. This means that
  everything that stands between [pos] (included) and [pos'] (excluded) should
  be ignored. *)
type blank = buffer -> int -> buffer * int

(** [blank_regexp re] produces a blank function using a regular expression
  [re]. There is an important limitation regarding regular expressions
  containing the newline symbol [\n], due to the fact that the [Str] module
  only matches on strings (and not on an abstract notion of buffer). Such
  regular expressions should be idempotent, and match a string containing
  only a newline. *)
val blank_regexp : Str.regexp -> blank

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
  reached during parsing, and the result of the parsing. *)
val partial_parse_buffer : 'a grammar -> blank -> buffer -> int
                           -> buffer * int * 'a

(** [partial_parse_string fn g bl str pos] parses input from the string [str],
  starting a position [pos], using the grammar [g] and the blank function
  [bl]. A triple is returned containing the new buffer, the position that was
  reached during parsing, and the result of the parsing. The optional file
  name [fn] is provided to obtain better error messages. *)
val partial_parse_string : ?filename:string -> 'a grammar -> blank -> string
                           -> int -> buffer * int * 'a

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
val  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool
                 -> string -> 'a grammar

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








val iter : 'a grammar grammar -> 'a grammar
  (* = fun g -> dependent_sequence g (fun x -> x) *)

(** [option a p]: parses input with [p], if and only if it fails return [a] consuming no input *)
val option : 'a -> 'a grammar -> 'a grammar

(** [fixpoint a p] : repetition 0 or more time *)
val fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar

(** [alternatives [p1;...;pn]] : parses as all [pi], and keep only the first success *)
val alternatives : 'a grammar list -> 'a grammar

(** the ony difference between the next function and the previous one
    if that they do not backtrack if the completely parse an object
    of type 'a grammar *)
val option' : 'a -> 'a grammar -> 'a grammar
val fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
val alternatives' : 'a grammar list -> 'a grammar

(** [apply f grammar]: apply [f] to the value returned by the given grammar *)
val apply : ('a -> 'b) -> 'a grammar -> 'b grammar

(** [position], tranform a given grammar to add the position of the parsed text *)
val position : 'a grammar -> (string * int * int * int * int * 'a) grammar

(** [apply_position f gr]: a variant of the previous function, giving directly the buffer and position
    at the beginning and end of the parsed input. You may then get the position (filename, line number ...)
    using the Input module. *)
val apply_position : ('a -> buffer -> int -> buffer -> int -> 'b) -> 'a grammar -> 'b grammar

(* [delim l] will prevent backtracking beyond grammar l *)
val delim : 'a grammar -> 'a grammar

(* [lists gr] lists all the parse tree produced by the first argument. The parse-tree do
   not need to correspond to the same initial segment of the input. *)
val lists : 'a grammar -> 'a list grammar

val merge : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a grammar -> 'b grammar

(** [declare_grammar name] return a new grammar, that can be used to define other grammar, but that
    can not be used yet. The name is used in error messages *)
val declare_grammar : string -> 'a grammar

(** [set_grammar p p']: defines a previously declared grammar p using p' *)
val set_grammar : 'a grammar -> 'a grammar -> unit
(** The too previous function allow for recursive grammar, but left recursion is
    forbidden (trigger a [Failure] exception).

    For instance the following code is incorrect:

{[
    let p = declare_grammar ()
    let p' = sequence (sequence (option (string "a" ["a"])) p) (string "b" "b") (fun l x -> x::l)
    let _ = set_grammar p p'
]}
*)

val grammar_family : ?param_to_string:('a -> string) -> string -> ('a -> 'b grammar) * (('a -> 'b grammar) -> unit)
(**
  [grammar_family seeds] return a pair [(gr, set_gr)] where gr is a finite family of
  grammars parametrized by value of type 'a the type of the elments of the list seeds.

  You use this function in this way:
{[
  let (gr, set_gr) = grammar_family seeds in

  ... code that uses all this grammar to define new mutually recursive grammars ...
  ... here you can not use the grammar in "left position" (same restriction ...
  ... as for declare grammar (1) ...

  (* once you really set the value of the family *)
  let _ = set_gr the_grammars

  ... now you can really use the new family ...
]}

  The [seeds] list are somahow the entry point of your family, any member of the
  family used in (1) must be reachable by the recursive definitions in (1) from
  [gr s] where [s] is member of the list [seeds]. Most of the time one seed in
  enough.
*)

(** To useful functions to manipulate the exceptions raised by cpspc *)
val print_exception : exn -> unit
val handle_exception : ('a -> 'b) -> 'a -> 'b


(** for debugging (mainly) *)
val accept_empty : 'a grammar -> bool
val firsts : 'a grammar -> charset
