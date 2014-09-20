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
exception Give_up (* of string *) (* FIXME *) 

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

(** [parse_buffer par bl buf] parses input from the buffer [buf] using the
  parser [par] and the blank function [bl]. *)
val parse_buffer : 'a grammar -> blank -> buffer -> 'a

(** [parse_string ~fn par bl str] parses the string [str] using the parser
  [par] and the blank function [bl]. A filename [fn] can be provided in order
  to obtain better error messages. *)
val parse_string : ?filename:string -> 'a grammar -> blank -> string -> 'a

(** [parse_channel ~fn par bl ic] loads the content of the input channel [ic]
  and parses it using the parser [par] and the blank function [bl]. A filename
  [fn] can be provided in order to obtain better error messages. *)
val parse_channel : ?filename:string -> 'a grammar -> blank -> in_channel -> 'a

(** [parse_file par bl fn] opens the file [fn] and parses it using the parser
  [par] and the blank function [bl]. *)
val parse_file : 'a grammar -> blank -> string -> 'a

(** [partial_parse_buffer par bl buf pos] parses input from the buffer [buf],
  starting a position [pos], using the parser [par] and the blank function
  [bl]. A triple is returned containing the new buffer, the position that was
  reached during parsing, and the result of the parsing. *)
val partial_parse_buffer : 'a grammar -> blank -> buffer -> int
                           -> buffer * int * 'a

(** [partial_parse_string fn par bl str pos] parses input from the string
  [str], starting a position [pos], using the parser [par] and the blank
  function [bl]. A triple is returned containing the new buffer, the position
  that was reached during parsing, and the result of the parsing. The optional
  file name [fn] is provided to obtain better error messages. *)
val partial_parse_string : ?filename:string -> 'a grammar -> blank -> string
                           -> int -> buffer * int * 'a

(** {2 Atomic parsers} *)

(** [eof v] parses the end of file character [EOF], and returns [x]. *)
val eof : 'a -> 'a grammar

(** [any] parses any character (that is not [EOF], and returns is. *)
val any : char grammar

(** [empty v] does not parse anything and always succeeds. It returns a the
  provided value [v]. *)
val empty : 'a -> 'a grammar

(** [debug msg] does not parse anything and always succeeds. It prints the
  message [msg] on [stderr] for debugging. *)
val debug : string -> unit grammar

(** [fail msg] always fails, adding [msg] to the list of messages int the
  list of messages to be reported by [Parse_error]. *)
val fail : string -> 'a grammar

(** [black_box fn cs accept_empty] parses the input buffer using the the
  provided function [fn]. [fn buf pos] should start parsing [buf] at position
  [pos], and return a couple containing the new buffer and the position of the
  first unread character. The character set [cs] should contain at least the
  characters that are accepted as first character by the parser, and no less.
  The boolean [accept_empty] shoud be set to [true] if the parsing function
  [fn] accepts the empty string, and to false otherwise. In case of parse
  error, the function [fn] should raise the exception [Give_up msg], where
  [msg] is an explicit error message. *)
(* FIXME the string argument should be removed since error message will be handled by Give_up *)
val  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool -> string -> 'a grammar

(** [char str x]: parses a given char and returns [x] *)
val char : char -> 'a -> 'a grammar

(** [string str x]: parses a given string and returns [x] *)
val string : string -> 'a -> 'a grammar

(** [regexp re g]: parses a given regexp, and returns [g groupe] where [groupe n] gives the
   n-th matching group as in the [Str] module. The optional argument is a name for
   the regexp, used in error messages *)
val regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a grammar


(** [change_layout ~nbb ~oba par bl] changes the blank function for a given grammar.
    Remark: the new layout is only used inside the input parsed by the grammar, not
    at the beginning nor at the end

    The optional parameter [new_blank_before], ([true] by default) forces parsing the
    new blank before parsing with the given grammar (the old blank function was used
    before the next terminal symbol anyway).

    Similarly, the optional parameter [old_blank_after], ([true] by default) forces
    the parsing of the old blank after parsing with the given grammar and the old blank.
*)
val change_layout : ?new_blank_before:bool -> ?old_blank_after:bool -> 'a grammar -> blank -> 'a grammar

(* [ignore_next_blank g] prevent parsing blank at the beginning of g.
   if g parses the empty input, blank will be parsed normally by the rest
   of the parser. Therefore, [ignore_next_blank empty] is equivalent to empty. *)
val ignore_next_blank : 'a grammar -> 'a grammar

(** [sequence p1 p2 action]: parses with [p1] which returns [x], then parses with [p2] the rest of
    the input which return [y] and returns [action x y] *)
val sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

val sequence_position : 'a grammar -> 'b grammar -> ('a -> 'b -> buffer -> int -> buffer -> int -> 'c) -> 'c grammar

(** [fsequence p1 p2 := fun l1 l2 -> sequence l1 l2 (fun x f -> f x)] *)
val fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar

val fsequence_position : 'a grammar -> ('a -> buffer -> int -> buffer -> int -> 'b) grammar -> 'b grammar

(** [sequence3 p1 p2 p3 action =
    fun l1 l2 l3 g ->
      sequence (sequence l1 l2 (fun x y z -> g x y z)) l3 (fun f -> f)] *)
val sequence3 : 'a grammar -> 'b grammar -> 'c grammar -> ('a -> 'b -> 'c -> 'd) -> 'd grammar

(** [dependent_sequence p1 p2]: parses with [p1] which returns [x] and then parses the rest of input
    with [p2 x] and returns its result. *)
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