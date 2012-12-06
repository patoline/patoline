(** Auxiliary functions *)

exception File_not_found of (string * string list)
val findPath : string -> string list -> string
val findFont : string -> string
val findGrammar : string -> string
val findHyph : string -> string
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
(** the list, minus its last element *)
val init : 'a list -> 'a list
val is_space : CamomileLibrary.UChar.t -> bool
val unspace : string -> string

module IntMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
module StrMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module IntListMap :
  sig
    type key = int list
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end

type 'a tree=
    N of ('a tree) IntMap.t
  | L of 'a

val compose : ('b->'c)->('a->'b)->'a->'c

val open_in_cached:string->in_channel
val open_in_bin_cached:string->in_channel
val copy_file:string->string->unit
