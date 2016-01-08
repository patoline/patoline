(** Datatype for regexps. *)
type regexp =
  | Chr of char            (* Single character. *)
  | Set of Charset.charset (* Any character in a charset. *)
  | Seq of regexp list     (* Sequence of two regular expressions. *)
  | Alt of regexp list     (* Alternative between several regexps. *)
  | Opt of regexp          (* Optional regexp. *)
  | Str of regexp          (* Zero or more times the regexp. *)
  | Pls of regexp          (* One or more times the regexp. *)
  | Grp of regexp          (* Group containing a regular expression. *)
  | Ref of int             (* Reference to a group. *)
  | BOL                    (* Beginning of line. *)
  | EOL                    (* End of line. *)
  | Bnd                    (* Word boundary. *)

(** [raw_regexp r] produces a [Decap] parser using the regexp [re]. *)
val raw_regexp : regexp -> string Decap.grammar

(** [raw_regexp s] parses the string [s] using the [Str] regexp syntax and
   returns the corresponding [Decap] parser. *)
val regexp : string -> string Decap.grammar
