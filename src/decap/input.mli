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

(** A module implementing and input buffer for the DECAP parser combinator
   library. *)

(** {2 Type} *)

(** The abstract type for an input buffer. *)
type buffer

(** {2 Reading from a buffer} *)

(** [read buf pos] returns the character at position [pos] in the buffer
   [buf], together with the new buffer and position.

   Remark: the buffer is currently implemented as a lazy stream of lines and
   the position is therefore the position in the line. This might be subject
   to change in the future, so code should not rely on this feature ... *)
val read : buffer -> int -> char * buffer * int

(** [get] is similar to [read], but it only returns the character that has
   been read. *)
val get : buffer -> int -> char

(** {2 Creating a buffer} *)

(** [buffer_from_channel fn ic] constructs a buffer using the input channel
   [ic]. The optional file name [fn] is only provided to name the buffer in
   exceptions. *)
val buffer_from_channel : ?filename:string -> in_channel -> buffer

(** [buffer_from_file fn] constructs a buffer using the file [fn]. The file
   is open for reading, and the function [buffer_from_channel] is called. *)
val buffer_from_file : string -> buffer

(** [buffer_from_string fn s] constructs a buffer using the string [s]. The
   optional file name [fn] is only provided to name the buffer in
   exceptions. *)
val buffer_from_string : ?filename:string -> string -> buffer

(** [empty_buffer fn lnum bol] constructs an empty buffer. The file name [fn]
   is used to name the buffer in exceptions. The integer [lnum] and [bol] give
   the position of the buffer as a line number and the offset to the beginning
   of the current line. *)
val empty_buffer : string -> int -> int -> buffer

(** {2 Managing positions} *)

(** [lexing_position buf pos]: return a record of type [Lexing.position] from
   a buffer and a position in this buffer. *)
val lexing_position : buffer -> int -> Lexing.position

(** {2 Accessing the internal content of a buffer} *)

(** [is_empty buf] test whether the buffer [buf] is empty. *)
val is_empty : buffer -> bool

(** [line_num buf] returns the current line number in the buffer [buf]. *)
val line_num : buffer -> int

(** [line_beginning buf] returns the offset of the beginning of the current
   line number in the buffer [buf]. *)
val line_beginning : buffer -> int

(** [line buf] returns the contents of the buffer [buf]. *)
val line : buffer -> string

(** [fname buf] returns the file name associated to the buffer [buf]. *)
val fname : buffer -> string