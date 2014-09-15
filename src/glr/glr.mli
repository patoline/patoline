open Charset
open Input 

(** Glr: a module defining parser combinator similar to GLR grammar
    @author Christophe Raffalli *)

(** For the moment, ambiguous grammar are supported, but ambiguous exception
  raise the exception [Ambiguity(pos_start, pos_end)].
  A possibility of providing your own merge function is planed *)
exception Ambiguity of string * int * int * string * int * int

(** [Parse_error (pos, str)], give the last point succesfully reached by the parser in 
  the input and what was expected to continue*)
exception Parse_error of string * int * int * string list

(** You may raise this exception to reject a parsing rule from the action code *)
exception Give_up

(** Type of a function parsing "blank" (i.e. characters to be ignored, like spaces or comments).
  [f str pos = pos'] means that all characteres from [pos] (included) to [pos'] (excluded) must
  be ignored *)
type blank = buffer -> int -> buffer * int

(** [blank_regexp re]: produces a blank function from a regexp *)
val blank_regexp : Str.regexp -> blank

(** type of a grammar returning value of type ['a] *)
type ('a) grammar

(** [parse_buffer parser blank buffer] parses the given buffer using the provided parser and blank function. *)
val parse_buffer : 'a grammar -> blank -> buffer -> 'a
 
(** [parse_string ~filename parser blank str]: parses the string [str] with the given grammar and blank function.
    a "filename" is provided for error messages only *)
val parse_string : ?filename:string -> 'a grammar -> blank -> string -> 'a

(** [parse_channel ~filename parser blank in_channel]: load the content of the channel in a string
     and parses it. *)
val parse_channel : ?filename:string -> 'a grammar -> blank -> in_channel -> 'a

(** [parse_file parser blank filename]: open the file and calls the previous function. *)
val parse_file : 'a grammar -> blank -> string -> 'a

(* The two following function allows to parse only the beginning of the file and they return
   the buffer after parsing. *)
val partial_parse_buffer : 'a grammar -> blank -> buffer-> int -> (buffer * int * 'a) list
val partial_parse_string : ?filename:string -> 'a grammar -> blank -> string -> int -> (buffer * int * 'a) list

(** [change_layout parser blank]: change the blank function for a given grammar.
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

(** [eof x]: parses the end of input only and returns [x] *)
val eof : 'a -> 'a grammar

(** [any]: parses one char and returns it *)
val any : char grammar

(** [empty x]: parses no characters (always succeed) and return [x]*)
val empty : 'a -> 'a grammar

(** [debug msg] is identical to [empty ()] but print [msg] on stderr for debugging *)
val debug : string -> unit grammar

(** [fail msg]: always fails and add msg to Parse_error *)
val fail : string -> 'a grammar

(** [black_box fn cs accept_empty msg]: parses with the function [fn str pos], that shoud start
    parsing [str] at the position [pos] and return the position of the first unread char.
    [cs] is the set of characters accepted as first char by your parser (or a supperset of).
    [accept_empty] shoud be false if your parser does not accept the empty string.
    If [fn] raises [Give_up], [msg] is added to Parse_error.
*) 
val  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool -> string -> 'a grammar

(** [char str x]: parses a given char and returns [x] *)
val char : char -> 'a -> 'a grammar

(** [string str x]: parses a given string and returns [x] *)
val string : string -> 'a -> 'a grammar

(** [regexp re g]: parses a given regexp, and returns [g groupe] where [groupe n] gives the
   n-th matching group as in the [Str] module. The optional argument is a name for
   the regexp, used in error messages *)
val regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a grammar

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

(** [filter_position gr f]: return the same grammar with in the semantics 
    the information about the position with the type the users want.
    this position information is build by calling [f str begin_line begin_col end_line end_col]
    where str is the full string being parsed and begin_pos, end_pos are
    the position of the parsed tree in str. *)
val filter_position : 'a grammar -> (string -> int -> int -> int -> int -> int -> int -> 'b) -> ('b * 'a) grammar

(** [apply_position f gr]: a variant of the previous function, giving directly the buffer and position
    at the beginning and end of the parsed input. You may then get the position (filename, line number ...)
    using the Input module. *)
val apply_position : ('a -> buffer -> int -> buffer -> int -> 'b) -> 'a grammar -> 'b grammar

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
