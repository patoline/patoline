open UsualMake

val pt_of_mm : float -> float
val mm_of_pt : float -> float
val a4 : float * float
val phi : float
val readInt : in_channel -> int -> int
val buf : string
val readInt2 : in_channel -> int
val sreadInt2 : in_channel -> int
val strInt2 : string -> int -> int -> unit
#ifdef INT32
val strInt4 : string -> int -> Int32.t -> unit
val strInt4_int : string -> int -> int -> unit
#else
val strInt4 : string -> int -> int -> unit
val strInt4_int : string -> int -> int -> unit
#endif
val buf2 : string
val writeInt2 : out_channel -> int -> unit
val buf4 : string
val writeInt4 : out_channel -> int -> unit
val bufInt1 : Rbuffer.t -> int -> unit
val bufInt2 : Rbuffer.t -> int -> unit
#ifdef INT32
val bufInt4 : Rbuffer.t -> Int32.t -> unit
val bufInt4_int : Rbuffer.t -> int -> unit
#else
val bufInt4 : Rbuffer.t -> int -> unit
val bufInt4_int : Rbuffer.t -> int -> unit
#endif
val readInt4 : in_channel -> int
val readInt4_int : in_channel -> int
val int16 : int -> int
val round : float -> int
val round_float : float -> float
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
val break : ('a -> bool) -> 'a list -> 'a list * 'a list
val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val last : 'a list -> 'a
val init : 'a list -> 'a list
val is_space : CamomileLibrary.UChar.t -> bool
val unspace : CamomileLibrary.UTF8.t -> string
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val unique : 'a list -> 'a list
type 'a tree = N of 'a tree IntMap.t | L of 'a
val bin_cache : in_channel StrMap.t ref
val cache : in_channel StrMap.t ref
val close_in_cache : unit -> unit
val open_in_bin_cached : StrMap.key -> in_channel
val open_in_cached : StrMap.key -> in_channel
val copy_file : string -> string -> unit
val btimer : float StrMap.t ref
val timer : StrMap.key -> (unit -> 'a) -> 'a
val split : char -> string -> string list
val base64_decode : string -> string
val base64_encode : string -> string
