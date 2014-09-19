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
