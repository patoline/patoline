(* [buffer] : a abstrct type for the input beffer of 
   a glr parser. *)
type buffer

(* [read buffer position]
   given a buffer and a position in this buffer,
   returns the character at this position, and
   the new buffer and position.

   Remark: in the current implementation, the
   buffer is a lazy stream of lines and the 
   position is therefore the position in the line.
   This might not be the case in the futur, so
   code should not rely on this feature ... *)
val read : buffer -> int -> char * buffer * int

(* [buffer_from_channel filename ch] build
   a buffer from a channel. The provided filename
   is only used in exceptions *)
val buffer_from_channel : ?filename:string -> in_channel -> buffer

(* [buffer_from_file ch] open the file for reading and call
   the previous function *)
val buffer_from_file : string -> buffer

(* [buffer_from_string filename data] similar
   to [buffer_from_channel], but for string. *)
val buffer_from_string : ?filename:string -> string -> buffer

(* [empty_buffer] the name says it all.*)
val empty_buffer : string -> int -> int -> buffer

(* The next functions allow to access the internal
   content of a buffer in the current implementation. *)
val is_empty : buffer -> bool

val line_num : buffer -> int

val line_beginning : buffer -> int

val line : buffer -> string

val fname : buffer -> string


