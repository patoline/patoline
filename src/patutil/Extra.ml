(** Extension of the [List] module. *)
module List =
  struct
    include List

    (** [take n l] returns the list of the [n] first elements of [l]. If there
     * are less than [n] elements in [l],  the return list is physically equal
     * to [l]. If [n] is less or equal to zero, an empty list is returned. *)
    let take : int -> 'a list -> 'a list = fun n l ->
      let rec take n xs =
        match xs with
        | []   when n > 0 -> l (* Not long enough, return original list. *)
        | x::l when n > 0 -> x :: take (n-1) l
        | _               -> []
      in take n l
    
    (** [drop n l] returns a list containing the first [n] elements of [l]. If
     * [n] is less or equal to zero,  then the return list is physically equal
     * to [l]. An empty list is returned if [l] has less than [n] elements. *)
    let rec drop : int -> 'a list -> 'a list = fun n l ->
      if n <= 0 then l else
      match l with
      | []   -> []
      | _::l -> drop (n-1) l
    
    (** [last l] returns the last element of the list [l]. If [l] is empty the
     * exception [Failure "last"] is raised. *)
    let rec last : 'a list -> 'a = fun l ->
      match l with
      | []   -> raise (Failure "last")
      | [x]  -> x
      | _::l -> last l
    
    (** [init l]  returns the list formed by removing the last element of [l].
     * If [l] is empty, the exception [Failure "init"] is raised. *)
    let rec init : 'a list -> 'a list = fun l ->
      match l with
      | []   -> raise (Failure "init")
      | [_]  -> []
      | h::s -> h :: init s
  end

(** Extension of the [String] module. *)
module String =
  struct
    include String

    (** [split_on_char c s] compute the list of all the substings of [s] that
     * are separated by the character [c]. NOTE included in OCaml >= 4.04. *)
    let split_on_char c s =
      let len = String.length s in
      let rec fn beg pos acc =
        if pos >= len then
          List.rev (String.sub s beg (pos - beg) :: acc)
        else if s.[pos] = c then
          fn (pos+1) (pos+1) (String.sub s beg (pos - beg) :: acc)
        else fn beg (pos+1) acc
      in
      fn 0 0 []

    let _ = assert(split_on_char ',' "bla,ba," = ["bla";"ba";""])
    let _ = assert(split_on_char ',' "" = [""])
    let _ = assert(split_on_char ',' "bla" = ["bla"])
  end

(** Extension of the [Filename] module. *)
module Filename =
  struct
    include Filename

    (** [concat dir path] is the same as the usual [Filename.concat dir path],
     * except that it returns [path] when [dir] is equal to ["."]. *)
    let concat : string -> string -> string = fun dir path ->
      if dir = "." then path else concat dir path

    (** [find_file name dirs] returns the path to the first file named [name],
     * that exists in the directories of the [dirs] list, in order. If no such
     * file is found, the exception [Not_found] is raised. *)
    let find_file : string -> string list -> string = fun name dirs ->
      let rec find dirs =
        match dirs with
        | []      -> raise Not_found
        | d::dirs -> let path = Filename.concat d name in
                     if Sys.file_exists path then path else find dirs
      in find dirs

    (** [decompose path] decomposes the file path [path] into [(dir,base,ext)]
     * where [dir] is the directory name, [base] the base name,  and [ext] the
     * extension. If [path] does not contains a directory separator then [dir]
     * is given value ["."]. If [path] does not contain an extension, then the
     * [ext] is set to [""]. *)
    let decompose : string -> string * string * string = fun fn ->
      let dir  = dirname  fn in
      let base = basename fn in
      let name = chop_extension base in
      let base_len = String.length base in
      let name_len = String.length name in
      let ext =
        if base_len = name_len then ""
        else String.sub base name_len (base_len - name_len)
      in
      (dir, name, ext)

    (** [set_extension path e] returns a copy of [path] in which the extension
     * has been replaced by [e]. If [path] has no extension, the extension [e]
     * is appended to [path]. *)
    let set_extension : string -> string -> string = fun path e ->
      try Filename.chop_extension path ^ e with _ -> path ^ e

    (** [get_extension path] returns the extension of the given file. If there
     * is no extension, the value [""] is returned. *)
    let get_extension : string -> string = fun path ->
      let (_,_,ext) = decompose path in ext
  end

(** Maps with [string] keys. *)
module StrMap = Map.Make(String)

(** Maps with [int] keys. *)
module IntMap = Map.Make(
  struct
    type t = int
    let compare = (-)
  end)

(** Sets of [string]. *)
module StrSet = Set.Make(String)

(** Sets of [int]. *)
module IntSet = Set.Make(
  struct
    type t = int
    let compare = (-)
  end)

(** Golden ratio. *)
let phi : float =
  (1.0 +. sqrt 5.0) /. 2.0

(** Pi (ratio of a circle's circumference to its diameter). *)
let pi  : float =
  4.0 *. atan 1.0

(** [round f] rounds the floating point number [f]. *)
let round : float -> int =
  fun x -> int_of_float (floor (x +. 0.5))

(** [round f] rounds the floating point number [f] to a [float]. *)
let round_float : float -> float =
  fun x -> floor (x +. 0.5)
