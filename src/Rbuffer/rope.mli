(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Ropes are persistent data structures for long sequences.  Elements
    are of any type. When elements are characters, ropes thus implement
    strings (with an interface identical to that of [String]) but with
    far better performances w.r.t. concatenation of substring
    extraction, especially on very large strings. *)

(** Ropes are naturally implemented as a functor turning a (possibly
    inefficient) data structure of ``strings'' into another (more
    efficient) data structure with the same signature. *)

exception Out_of_bounds

(** Input signature for the functor *)

module type STRING = sig

  type t

  type char

  val length : t -> int

  val empty : t

  val singleton : char -> t
    
  val append : t -> t -> t

  val get : t -> int -> char

  val sub : t -> int -> int -> t
    (** [sub t ofs len] extracts the substring of length [len] at offset
        [ofs], that is [t[ofs..ofs+len-1]].
        Will always be called with a valid range. *)
    
  val iter_range : (char -> unit) -> t -> int -> int -> unit
    (** [iter_range f t ofs len] successively iterates [f] over characters
        of [t] at offsets [ofs], [ofs+1], ..., [ofs+len-1], in this order.
        Will always be called with a valid range. *)

  val print : Format.formatter -> t -> unit

end

(** Output signature of the functor. Note that it extends signature
    [STRING] and thus functor [Make] below can be iterated several
    times. *)

module type ROPE = sig

  include STRING

  val set : t -> int -> char -> t
    (** [set t i c] returns a new rope identical to [t],
        apart character [i] which is set to [c].
	Raises [Out_of_bounds] if [i < 0 || i >= length t].
        It is more equivalent to (but more efficient than) 
        [sub t 0 i ++ singleton c ++ sub t (i+1) (length t-i-1)] *)

  val delete : t -> int -> t
    (** [delete t i] returns a new rope obtained by removing character [i]
        in [t]. Raises [Out_of_bounds] if [i < 0 || i >= length t].
        It is more equivalent to (but more efficient than)
        [sub t 0 i ++ sub t (i + 1) (length t - i - 1)] *)

  val insert_char : t -> int -> char -> t
    (** [insert t i c] returns a new rope resulting from the insertion of 
        character [c] at position [i] in [t], that right before character [i].
	Raises [Out_of_bounds] if [i < 0 || i > length t].
        It is more equivalent to (but more efficient than)
        [sub t 0 i ++ singleton c ++ sub t i (length t - i)] *)

  val insert : t -> int -> t -> t
    (** [insert t i r] returns a new rope resulting from the insertion
        of rope [r] at position [i] in [t].
	Raises [Out_of_bounds] if [i < 0 || i > length t].
        It is more equivalent to (but more efficient than)
        [sub t 0 i ++ r ++ sub t i (length t - i)] *)

  (** Cursors are persistent data structures to navigate within ropes.
      When several operations are to be performed locally on a rope
      (such as local deletions, insertions or even simple accesses),
      then the use of cursors can be more efficient than the use of
      rope operations. 
      It is convenient to see the cursor as placed between two characters,
      so that a rope of length [n] has [n+1] cursor positions. *)

  module Cursor : sig

    type cursor

    val create : t -> int -> cursor
      (** [create t i] returns a cursor placed before character [i] of rope 
          [t]. Raises [Out_of_bounds] is [i < 0 || i > length t].
	  Note that [i = length t] is a valid argument, resulting in a cursor
	  placed right after the last character of the rope (i.e. at the
	  end of the rope). *)
      
    val position : cursor -> int
      (** [position c] returns the position of cursor [c] in its rope. *)
      
    val to_rope : cursor -> t
      (** [to_rope c] returns the rope corresponding to cursor [c]. *)
      
    val move_forward : cursor -> int -> cursor
      (** [move_forward c n] moves cursor [c] [n] characters forward.
	  Raises [Invalid_argument] if [n < 0]. 
	  Raises [Out_of_bounds] if it moves the cursor beyond the end of
	  the rope. *)
      
    val move_backward : cursor -> int -> cursor
      (** [move_backward c n] moves cursor [c] [n] characters
	  backward.  Raises [Invalid_argument] if [n < 0].  Raises
	  [Out_of_bounds] if it moves the cursor beyond the start of
	  the rope. *)

    val move : cursor -> int -> cursor
      (** [move c n] moves cursor [c] [n] characters away from its current
	  location, relatively to the sign of [n] (i.e. forward if [n > 0] and
          backward if [n < 0]). Raises [Out_of_bounds] if it moves the cursor
          beyond the start or the end of the rope. *)

    val get : cursor -> char
      (** [get c] returns the character right after cursor
	  [c].  Raises [Out_of_bounds] if the cursor is located at the
	  end of the rope. *)
      
    val set : cursor -> char -> cursor
      (** [set c x] returns a new cursor identical to [c] apart from
	  the character right after the cursor position, which is set
	  to [x].  Raises [Out_of_bounds] if the cursor is located at
	  the end of the rope. *)

    val insert_char : cursor -> char -> cursor
      (** [insert_char c x] returns a new cursor obtained from [c] by
	  inserting character [x] at the cursor position. The new
	  cursor is located right before the newly inserted character
	  (i.e. at the same absolute position in the rope). *)

    val insert : cursor -> t -> cursor
      (** [insert c r] is similar to [insert_char] but inserts a rope [r] at
	  the cursor point instead of a character. *)

(* Not Yet Implemented
    val delete : cursor -> cursor
      (** [delete c] deletes the character right after the cursor location.
	  Raises [Out_of_bounds] if the cursor is located at the end of the 
	  rope. *)
*)

    val print : Format.formatter -> cursor -> unit
      (** [print fmt c] prints cursor [c] on formatter [fmt], as a string
	  ["abc...|def..."] where ["abc..."] is the portion of the rope
	  before the cursor position and ["def..."] the portion after. *)
      
  end

end

(** The functor to build ropes, turning an implemention of strings [S]
    into an implemention of ropes.

    It is controlled by two parameters: 
    - [small_length] is the maximal length under which we perform
    concatenation of flat strings, i.e. when two ropes of length at most
    [small_length] are concatenated, then the corresponding flat string is
    built. 
    - [maximal_height] is the threshold for rebalancing: when a rope has
    height at least [maximal_height] it is then rebalanced; setting 
    [small_length] to [max_int] will result in ropes that are never 
    rebalanced (which is perfectly fine in many applications).
*)

module type CONTROL = sig

  val small_length : int

  val maximal_height : int

end 

module Make(S : STRING)(C : CONTROL) : sig

  include ROPE with type char = S.char

  val of_string : S.t -> t

end


(** Instance: usual strings (i.e. with [type char = Char.t]) is a
    particular instance of functor [Make] above, which is directly
    provided here as module [S] *)

module S : sig
  
  include ROPE with type char = Char.t

  val of_string : string -> t

end


(** Another instance: ropes with function leaves. This allows to have
    pieces of the rope represented as a function (instead of a flat
    string). This is convenient to represent large strings such as
    files on disk. *)

module SF : sig
  
  include ROPE with type char = Char.t

  val of_string : string -> t

  val of_function : int -> (int -> char) -> t

end


(** Elements of ropes can be of any type, of course. In that case,
    they must rather be seen as arrays instead of strings. The
    following functor builds ropes for a given (printable) type of
    elements (using arrays internally for flat strings). *)

module type PrintableType = sig 
  type t
  val print : Format.formatter -> t -> unit 
end

module MakeArray(X : PrintableType) : sig  

  include ROPE with type char = X.t

  val of_array : X.t array -> t

  val create : int -> X.t -> t

  val init : int -> (int -> X.t) -> t

end

