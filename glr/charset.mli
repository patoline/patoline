type charset = int array
val empty_charset :charset
val full_charset :charset
val get : charset -> char -> bool
val addq : charset -> char -> unit
val add : charset -> char -> charset
val union: charset -> charset -> charset
val singleton : char -> charset
val print_charset : out_channel -> charset option -> unit
