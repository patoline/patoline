(*
 * Type of a unicode character.
 *)
type uchar = int
type t = uchar

(*
 * Type of the index of an encoded uchar in a string.
 *)
type index = int

exception Out_of_bound

(*
 * Converts a char into a UTF8 character.
 *)
let of_char : char -> uchar = Char.code

(*
 * Converts a unicode character into a char.
 * Raise Out_of_bound in case the unicode character is not representable as a
 * char.
 *)
let to_char : uchar -> char = fun c ->
  if c < 0 || c > 0xFF then
    raise Out_of_bound
  else
    Char.chr c

(*
 * Converts an int into a unicode character.
 * Raise Out_of_bound if the character number is invalid.
 *)
let of_int : int -> uchar = fun i ->
  if i < 0 || i > 0x10FFFF then
    raise Out_of_bound
  else i

(*
 * Converts a unicode character into an int.
 * Raise Out_of_bound if the unicode character is invalid.
 *)
let to_int : uchar -> int = fun c ->
  if c < 0 || c > 0x10FFFF then
    raise Out_of_bound
  else c

(*
 * Returns the code of a unicode character.
 * Raise Out_of_bound if the unicode character is invalid.
 *)
let code : uchar -> int = to_int

(*
 * Returns the unicode character corresponding to a code.
 * Raise Out_of_bound if the code is invalid.
 *)
let chr : int -> uchar = of_int

(*
 * Map indexed with uchar.
 *)
module UMap = Map.Make (
  struct
    type t = uchar
    let compare = compare
  end)

(*
 * Tells whether the given unicode character corresponds to a spacing
 * character, including regular space, tabulation, newline, ...
 *)
let is_space : uchar -> bool = fun c ->
  (0x0009 <= c && c <= 0x000d) || c = 0x0020 || c = 0x00a0
  || c = 0x1680 || c = 0x180e || (0x2000 <= c && c <= 0x200a)
  || c = 0x202f || c = 0x205f || c = 0x3000
