val pt_of_mm : float -> float
val mm_of_pt : float -> float
val a4 : float * float
val phi : float

(** reads a caml int of the given number of bytes, in big endian *)
val readInt : in_channel -> int -> int
(** reads a 16-bit, unsigned big endian int *)
val readInt2 : in_channel -> int
(** reads a 16-bit, signed big endian int *)
val sreadInt2 : in_channel -> int

(** reads a 32-bit, big endian int *)
#ifdef INT32
val readInt4 : in_channel -> Int32.t
val readInt4_int : in_channel -> int
#else
val readInt4 : in_channel -> int
val readInt4_int : in_channel -> int
#endif
val strInt2 : string -> int -> int -> unit

#ifdef INT32
val strInt4 : string -> int -> Int32.t -> unit
val strInt4_int : string -> int -> int -> unit
#else
val strInt4 : string -> int -> int -> unit
val strInt4_int : string -> int -> int -> unit
#endif

val writeInt2 : out_channel -> int -> unit
#ifdef INT32
val writeInt4 : out_channel -> Int32.t -> unit
#else
val writeInt4 : out_channel -> int -> unit
#endif

val bufInt1 : Rbuffer.t -> int -> unit
val bufInt2 : Rbuffer.t -> int -> unit
#ifdef INT32
val bufInt4 : Rbuffer.t -> Int32.t -> unit
#else
val bufInt4 : Rbuffer.t -> int -> unit
#endif
val bufInt4_int : Rbuffer.t -> int -> unit

  (** converts a 16 bit integer in two's complement representation to a
      caml int *)
val int16 : int -> int
  (** rounds a float to the closest integer, and to the closest odd
      integer in the case of equality *)
val round : float -> int
val round_float : float -> float

(** returns the first elements of a list satisfying a predicate, and
    the rest of the list. *)
val span : ('a -> bool) -> 'a list -> 'a list * 'a list

(** returns the first elements of a list not satisfying a predicate,
    and the rest of the list. *)
val break : ('a -> bool) -> 'a list -> 'a list * 'a list
(** the n first elements of a list *)
val take : int -> 'a list -> 'a list
(** the input list, minus its n first elements *)
val drop : int -> 'a list -> 'a list
(** the last element of a list *)
val last : 'a list -> 'a
(** sorts a list, then leaves exactly one copy of each element *)
val unique : 'a list -> 'a list
(** the list, minus its last element *)
val init : 'a list -> 'a list
val is_space : CamomileLibrary.UChar.t -> bool
val unspace : string -> string

open UsualMake

type 'a tree=
    N of ('a tree) IntMap.t
  | L of 'a

val compose : ('b->'c)->('a->'b)->'a->'c

val open_in_cached:string->in_channel
val open_in_bin_cached:string->in_channel
val close_in_cache:unit->unit
val copy_file:string->string->unit
val btimer:float StrMap.t ref
val timer:string->(unit->'a)->'a

(* a lighter split that calling str *)
val split : char -> string -> string list

val base64_encode : string -> string
val base64_decode : string -> string

