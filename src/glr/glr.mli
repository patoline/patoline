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
    
    The optional parameter [old_blank_before], ([true] by default) forces parsing the 
    old blank before parsing with the given grammar (the new blank function will be used
    before the next terminal symbol anyway).

    Similarly, the optional parameter [new_blank_after], ([false] by default) forces
    the parsing of the new blank after parsing with the given grammar.
*)
val change_layout : ?old_blank_before:bool -> ?new_blank_after:bool -> 'a grammar -> blank -> 'a grammar

(** [eof x]: parses the end of input only and returns [x] *)
val eof : 'a -> 'a grammar

(** [char]: parses one char and returns it *)
val one : char grammar

(** [empty x]: parses no characters (always succeed) and return [x]*)
val empty : 'a -> 'a grammar

(** [list_empty x := empty [x]] *)
val list_empty : 'a -> 'a list grammar

(** [debug msg] is identical to [empty ()] but print [msg] on stderr for debugging *)
val debug : string -> unit grammar

(** [list_debug msg] is identical to [list_empty ()] but print [msg] on stderr for debugging *)
val list_debug :  string -> unit list grammar

(** [fail msg]: always fails and add msg to Parse_error *)
val fail : string -> 'a grammar

(** [black_box fn cs accept_empty msg]: parses with the function [fn str pos], that shoud start
    parsing [str] at the position [pos] and return the position of the first unread char.
    [cs] is the set of characters accepted as first char by your parser (or a supperset of).
    [accept_empty] shoud be false if your parser does not accept the empty string.
    If [fn] raises [Give_up], [msg] is added to Parse_error.
*) 
val  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool -> string -> 'a grammar

(** [list_eof x := eof [x]] *)
val list_eof : 'a -> 'a list grammar

(** [char str x]: parses a given char and returns [x] *)
val char : char -> 'a -> 'a grammar

(** [list_char str x := char s [a]] *)
val list_char : char -> 'a -> 'a list grammar

(** [string str x]: parses a given string and returns [x] *)
val string : string -> 'a -> 'a grammar

(** [list_string str x := string s [a]] *)
val list_string : string -> 'a -> 'a list grammar

(** [regexp re g]: parses a given regexp, and returns [g groupe] where [groupe n] gives the
   n-th matching group as in the [Str] module. The optional argument is a name for
   the regexp, used in error messages *)
val regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a grammar

(** [list_regexp r f := regexp r (fun groupe -> [f groupe])] *)
val list_regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a list grammar

(** [sequence p1 p2 action]: parses with [p1] which returns [x], then parses with [p2] the rest of
    the input which return [y] and returns [action x y] *)
val sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

(** like the previous function, but use the first argument to merge ambiguity *)
val merge_sequence : ('c -> 'c -> 'c) -> 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar

(** [fsequence p1 p2 := fun l1 l2 -> sequence l1 l2 (fun x f -> f x)] *)
val fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar

(** [sequence3 p1 p2 p3 action =
    fun l1 l2 l3 g ->
      sequence (sequence l1 l2 (fun x y z -> g x y z)) l3 (fun f -> f)] *)
val sequence3 : 'a grammar -> 'b grammar -> 'c grammar -> ('a -> 'b -> 'c -> 'd) -> 'd grammar

(** [dependent_sequence p1 p2]: parses with [p1] which returns [x] and then parses the rest of input
    with [p2 x] and returns its result. *)
val dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar

val iter : 'a grammar grammar -> 'a grammar 
  (* = fun g -> dependent_sequence g (fun x -> x) *)

(** like the previous function, but use the first argument to merge ambiguity *)
val dependent_merge_sequence : ('b -> 'b -> 'b) -> 'a grammar -> ('a -> 'b grammar) -> 'b grammar

val iter_merge : ('a -> 'a -> 'a) -> 'a grammar grammar -> 'a grammar 
  (* = fun f g -> dependent_merge_sequence f g (fun x -> x) *)

(** [list_sequence g1 g2 action :=
   merge_sequence List.append g1 g2 (fun al bl -> flat_map (fun a -> List.map (fun b -> action a b) bl) al)] *)
val list_sequence : 'a list grammar -> 'b list grammar -> ('a -> 'b -> 'c) -> 'c list grammar 

(** [dependent_list_sequence g1 f2] is similar to dependent_merge_sequence, but using lists to store all results *)
val dependent_list_sequence : 'a list grammar -> ('a -> 'b list grammar) -> 'b list grammar

val iter_list : 'a list grammar list grammar -> 'a list grammar 
  (* = fun l1 -> dependent_list_sequence l1 (fun x -> x) *)

(** [option' a p]: parses input with [p] or return [a] consuming no input (both are done) *)
val option' : 'a -> 'a grammar -> 'a grammar

(** like the previous function, but use the first argument to merge ambiguity *)
val merge_option : ('a -> 'a -> 'a) -> 'a -> 'a grammar -> 'a grammar

(** [list_option' a grammar :=  merge_option List.append [a] l]*)
val list_option' : 'a -> 'a list grammar -> 'a list grammar

(** [option a p]: parses input with [p], if and only if it fails return [a] consuming no input *)
val option : 'a -> 'a grammar -> 'a grammar

(** [list_option a grammar :=  option [a] l]*)
val list_option : 'a -> 'a list grammar -> 'a list grammar

(** [fixpoint' a p]: parses input with p repeatedly. If parsing returns [f1 ... fn], it returns
    [fn (... (f1 a))]. All possibility are done, meaning that partial parsing of the fixpoint
    are considered. *)
val fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar

(** like the previous function, but use the first argument to merge ambiguity *)
val merge_fixpoint : ('a -> 'a -> 'a) -> 'a -> ('a -> 'a) grammar -> 'a grammar

(** [list_fixpoint' a grammar := merge_fixpoint List.append [a] (apply (fun f l -> flat_map f l) grammar)]*)
val list_fixpoint' : 'a -> ('a -> 'a) list grammar -> 'a list grammar

(** [fixpoint a p] : similar to the previous function, but only consider the maximum parsing *)
val fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar

(** [list_fixpoint a grammar :=  fixpoint [a] (apply (fun f l -> flat_map f l) grammar)] *)
val list_fixpoint : 'a -> ('a -> 'a) list grammar -> 'a list grammar

(** [alternatives' [p1;...;pn]] : parses as all [pi], and keep all success *)
val alternatives' : 'a grammar list -> 'a grammar 

(** like the previous function, but use the first argument to merge ambiguity *)
val merge_alternatives : ('a -> 'a -> 'a) -> 'a grammar list -> 'a grammar 

(** [list_alternatives' grammars := merge_alternatives List.append grammars] *)
val list_alternatives' : 'a list grammar list -> 'a list grammar 

(** [alternatives [p1;...;pn]] : parses as all [pi], and keep only the first success *)
val alternatives : 'a grammar list -> 'a grammar 

(** list_alternatives := alternatives*)
val list_alternatives : 'a list grammar list -> 'a list grammar 

(** [apply f grammar]: apply [f] to the value returned by the given grammar *)
val apply : ('a -> 'b) -> 'a grammar -> 'b grammar

(** [list_apply f grammar := apply (List.map f) grammar] *)
val list_apply : ('a -> 'b) -> 'a list grammar -> 'b list grammar

(** [list_merge f grammar] use the first argument to merge all results from the grammar *)
val list_merge : ('a -> 'a -> 'a) -> 'a list grammar -> 'a grammar

(** [position], tranform a given grammar to add the position of the parsed text *)
val position : 'a grammar -> (string * int * int * int * int * 'a) grammar

(** [filter_position gr f]: return the same grammar with in the semantics 
    the information about the position with the type the users want.
    this position information is build by calling [f str begin_line begin_col end_line end_col]
    where str is the full string being parsed and begin_pos, end_pos are
    the position of the parsed tree in str. *)
val filter_position : 'a grammar -> (string -> int -> int -> int -> int -> int -> int -> 'b) -> ('b * 'a) grammar


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

val print_exception : exn -> unit
val handle_exception : ('a -> 'b) -> 'a -> 'b

(** Camlp4 syntax extension, use with [ -pp pa_glr v ] option *)

(** [glr parser end] returns a ['a grammar].
    [glr* parser end] returns a ['a list grammar] parsing the same grammar, but returning all the possible results.

 The syntax for [parser] is the following: 

{ul
{- a parser is a sequence of rule like:
  [rule11 | rule12 | ... else rule21 | rule22 ...]
  simple bar correspond tu [alternatives] while [else] corresponds to [alternatives']. The above
  is therefore a syntax for
  [alternatives' [alternatives [rule11 ; rule12 ; ...]; alternatives [rule21 ; rule22 ; ...]; ...]]}
{- a [rule] is [lid_1:atom_1 ... lid_n:atom_n -> action] and corresponds to a parsing sequence. The action
  may use the bound variables [lid_1] ... [lid_n]. [lid_i:] is optional (in this case the result of the
  parsing of atom_i is lost),
  [-> action] is optional too, in this case it corresponds to [lid_n] (in fact the last named value).
  the action is parsed at the priority level "apply" of expression.}
{- an [atom] may be {ul
  {-  [EOF [value] modifiers] which correspond to [eof value] if value is given and [eof ()] otherwise}
  {- [STR expr [value] modifiers] for [string expr value] or [string expr ()] if value is not given}
  {- [RE expr [value] modifiers] for [regexp expr value] or [regexp expr (groupe 0)] if value is not given}
  {- [{ parser } modifiers] for a nested parser with camlp4 syntax}
  {- [ expr modifiers] for a caml expression of type 'a grammar (the expresion is parsed at the priority level
     "simple" of expression.)}}}
{- [modifiers] may be {ul
  {- nothing }
  {- [?] : for option (gives a ['a option grammar]) }
  {- [?[expr]] : for option with a default value (gives a ['a grammar]) }
  {- [??] : for option' (gives a ['a option grammar]) }
  {- [??[expr]] : for option' with a default value (gives a ['a grammar]) }
  {- [*] : for fixpoint (gives a ['a list grammar]) }
  {- [*[expr]] : for fixpoint with an initial value (gives a ['a grammar]) }
  {- [**] : for fixpoint' (gives a ['a list grammar]) }
  {- [**[expr]] : for fixpoint' with an initial value (gives a ['a grammar]) }
  {- In the above, [*] may be replaces by [+] to mean at least one item.}}}
}

*)

(** for debugging (mainly) *)
val accept_empty : 'a grammar -> bool
val firsts : 'a grammar -> charset
