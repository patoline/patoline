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

(** Extensible string buffers.

    This is a ropes-based implementation of buffers, with exactly the same
    interface and semantics as module [Buffer] from ocaml's standard library,
    so that you can switch from one to the other transparently.
 
    There are a few differences, though:

    - Contrary to ocaml's standard library, a buffer size is not limited to
      [Sys.max_string_length], but to [max_int] (sizes are represented 
      internally using native ocaml integers).

    - [contents] and [sub] raise [Invalid_argument] if the resulting string
      would be larger than [Sys.max_string_length] bytes.

    - The meaning of [create]'s argument is not exactly the same, though its 
      value only affects performances, as for [Buffer]; see below.

    - An additional function [print] is provided.
 
    Note: ocaml's [Buffer] is already quite efficient and this alternate
    implementation is to be considered only when the size limit of [Buffer]
    becomes an issue.
*)

type t
(** The abstract type of buffers. *)

val create : int -> t
(** [create n] returns a fresh buffer, initially empty.
   The [n] parameter will be the size of each chunk in the internal rope.
   For best performance, [n] should be within a few orders of magnitude
   as the number of characters that are expected to be stored in
   the buffer. Note that, contrary to module [Buffer], a too small value 
   of [n] may result in bad performances.

   If [n] is not between 1 and {!Sys.max_string_length}, it will
   be clipped to that interval. *)

val contents : t -> string
(** Return a copy of the current contents of the buffer.
   The buffer itself is unchanged. *)

val sub : t -> int -> int -> string
(** [Buffer.sub b off len] returns (a copy of) the substring of the
current contents of the buffer [b] starting at offset [off] of length
[len] bytes. May raise [Invalid_argument] if out of bounds request. The
buffer itself is unaffected. *)

val nth : t -> int -> char
(** get the n-th character of the buffer. Raise [Invalid_argument] if
index out of bounds *)

val length : t -> int
(** Return the number of characters currently contained in the buffer. *)

val clear : t -> unit
(** Empty the buffer. *)

val reset : t -> unit
(** Empty the buffer. *)

val add_char : t -> char -> unit
(** [add_char b c] appends the character [c] at the end of the buffer [b]. *)

val add_string : t -> string -> unit
(** [add_string b s] appends the string [s] at the end of the buffer [b]. *)

val add_substring : t -> string -> int -> int -> unit
(** [add_substring b s ofs len] takes [len] characters from offset
   [ofs] in string [s] and appends them at the end of the buffer [b]. *)

val add_substitute : t -> (string -> string) -> string -> unit
(** [add_substitute b f s] appends the string pattern [s] at the end
   of the buffer [b] with substitution.
   The substitution process looks for variables into
   the pattern and substitutes each variable name by its value, as
   obtained by applying the mapping [f] to the variable name. Inside the
   string pattern, a variable name immediately follows a non-escaped
   [$] character and is one of the following:
   - a non empty sequence of alphanumeric or [_] characters,
   - an arbitrary sequence of characters enclosed by a pair of
   matching parentheses or curly brackets.
   An escaped [$] character is a [$] that immediately follows a backslash
   character; it then stands for a plain [$].
   Raise [Not_found] if the closing character of a parenthesized variable
   cannot be found. *)

val add_buffer : t -> t -> unit
(** [add_buffer b1 b2] appends the current contents of buffer [b2]
   at the end of buffer [b1].  [b2] is not modified. *)

val add_channel : t -> in_channel -> int -> unit
(** [add_channel b ic n] reads exactly [n] character from the
   input channel [ic] and stores them at the end of buffer [b].
   Raise [End_of_file] if the channel contains fewer than [n]
   characters. *)

val output_buffer : out_channel -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b]
   on the output channel [oc]. *)

val print : Format.formatter -> t -> unit
(** [print fmt b] prints the current contents of buffer [b]
   on the formatter [fmt]. *)
