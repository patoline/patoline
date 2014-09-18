(** A module implementing character sets efficiently. *)

(** {2 Type} *)

(** The abstract type for a character set. *)
type charset

(** {2 Constant charsets} *)

(** The empty character set. *)
val empty_charset :charset

(** The full character set. *)
val full_charset :charset

(** {2 Manipulating charsets} *)

(** [mem cs c] tests whether the character [c] appears in the charset [cs]. *)
val mem : charset -> char -> bool

(** [addq cs c] adds the character [c] to the charset [cs]. *)
val addq : charset -> char -> unit

(** [add cs c] adds the character [c] to the charset [cs] and returns the new
   charset. *)
val add : charset -> char -> charset

(** [delq cs c] deletes the character [c] in the charset [cs] if it is
   appears. *)
val delq : charset -> char -> unit

(** [delq cs c] deletes the character [c] in the charset [cs] if it is
   appears, and returns the new charset. *)
val del : charset -> char -> charset

(** [union cs1 cs2] returns a charset that is the union of charset [cs1] and
   charset [cs2]. *)
val union: charset -> charset -> charset

(** [singleton c] returns a charset containing only character [c]. *)
val singleton : char -> charset

(** [copy cs] make a copy of the charset [cs]. *)
val copy : charset -> charset

(** [print_charset oc cs] prints the charset [cs] on the output channel [oc].
   If no charset is provided, ["None"] is printed. *)
val print_charset : out_channel -> charset option -> unit

(** [list_of_charset cs] transforms a charset into a list of strings, each
   containing an escaped character. *)
val list_of_charset : charset -> string list
