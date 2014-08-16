type buffer

val buffer_from_channel : string -> in_channel -> buffer

val buffer_from_file : string -> buffer

val buffer_from_string : string -> string -> buffer

val empty_buffer : string -> int -> buffer

val is_empty : buffer -> bool

val line_num : buffer -> int

val line : buffer -> string

val fname : buffer -> string

val read : buffer -> int -> char * buffer * int

