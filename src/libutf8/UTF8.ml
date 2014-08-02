(*
 * Type of a UTF8 character.
 *)
type uchar = int

(*
 * Synonym of int for an index in a string.
 *)
type index = int

exception Out_of_bound

(*
 * Converts a char into a UTF8 character.
 *)
let of_char : char -> uchar = Char.code

(*
 * Converts a UTF8 character into a char.
 * Raise Out_of_bound in cas the UTF8 character is not representable as a
 * char.
 *)
let to_char : uchar -> char = fun c ->
  if c < 0 || c > 0xFF then
    raise Out_of_bound
  else
    Char.chr c

(*
 * Converts an int into a UTF8 character.
 * Raise Out_of_bound if the character number is invalid.
 *)
let of_int : int -> uchar = fun i ->
  if i < 0 || i > 0x10FFFF then
    raise Out_of_bound
  else i

(*
 * Converts a UTF8 character into an int.
 * Raise Out_of_bound if the UTF8 character is invalid.
 *)
let to_int : uchar -> int = fun c ->
  if c < 0 || c > 0x10FFFF then
    raise Out_of_bound
  else c

(*
 * Returns the code of a UTF8 character.
 * Raise Out_of_bound if the UTF8 character is invalid.
 *)
let code : uchar -> int = to_int

(*
 * Returns the UTF8 character corresponding to a code.
 * Raise Out_of_bound if the code is invalid.
 *)
let chr : int -> uchar = of_int

(*
 * Encode a UTF8 character into a UTF8 string.
 * Argument:
 *   i : the UTF8 character.
 * Returns a string of size between 1 and 4.
 * Raise invalid_arg if i is not in the U+0000..U+10FFFF range.
 *)
let encode : uchar -> string = fun i ->
  if i < 0 then
    raise (invalid_arg "UF8.encode")
  else if i <= 0x7F then
    String.make 1 (char_of_int i)
  else if i <= 0x077F then
    let c0 = char_of_int (((i lsr 6) land 0b00011111) lor 0b11000000) in
    let c1 = char_of_int ((i         land 0b00111111) lor 0b10000000) in
    let s = String.create 2 in s.[0] <- c0; s.[1] <- c1; s
  else if i <= 0xFFFF then
    let c0 = char_of_int (((i lsr 12) land 0b00001111) lor 0b11100000) in
    let c1 = char_of_int (((i lsr 6)  land 0b00111111) lor 0b10000000) in
    let c2 = char_of_int ((i          land 0b00111111) lor 0b10000000) in
    let s = String.create 3 in
    s.[0] <- c0; s.[1] <- c1;
    s.[2] <- c2; s
  else if i <= 0x10FFFF then
    let c0 = char_of_int (((i lsr 18) land 0b00000111) lor 0b11110000) in
    let c1 = char_of_int (((i lsr 12) land 0b00111111) lor 0b10000000) in
    let c2 = char_of_int (((i lsr 6)  land 0b00111111) lor 0b10000000) in
    let c3 = char_of_int ((i          land 0b00111111) lor 0b10000000) in
    let s = String.create 4 in
    s.[0] <- c0; s.[1] <- c1;
    s.[2] <- c2; s.[3] <- c3; s
  else
    raise (invalid_arg "UTF8.encode")

(*
 * Decode a UTF8 character at a given position in a string.
 * Arguments:
 *   s : the string,
 *   i : index where to look.
 * Returns a couple (c, l) where c is the code of the character and l is the
 * number of bytes read.
 * Raise invalid_arg if s.[i] is not a valid first byte for a UTF8 character.
 * No checks are run on the hypothetical second, third and fourth byte (i.e.
 * length of s is not checked, and shape 0x10xxxxxx of byte is not checked).
 *)
let decode : string -> index -> (uchar * int) = fun s i ->
  let cc = Char.code s.[i] in
  if cc lsr 7 = 0 then
    (cc, 1)
  else if (cc lsr 6) land 1 = 0 then
    raise (invalid_arg "UTF8.decode")
  else if (cc lsr 5) land 1 = 0 then
    let i0 = (cc land 0b00011111) lsl 6 in
    let i1 = (Char.code s.[i+1]) land 0b00111111 in
    (i0 lor i1, 2)
  else if (cc lsr 4) land 1 = 0 then
    let i0 = (cc land 0b00001111) lsl 12 in
    let i1 = ((Char.code s.[i+1]) land 0b00111111) lsl 6 in
    let i2 = (Char.code s.[i+2])  land 0b00111111 in
    (i0 lor i1 lor i2, 3)
  else if (cc lsr 3) land 1 = 0 then
    let i0 = (cc land 0b00000111) lsl 18 in
    let i1 = ((Char.code s.[i+1]) land 0b00111111) lsl 12 in
    let i2 = ((Char.code s.[i+2]) land 0b00111111) lsl 6 in
    let i3 = (Char.code s.[i+3])  land 0b00111111 in
    (i0 lor i1 lor i2 lor i3, 4)
  else
    raise (invalid_arg "UTF8.decode")

(*
 * Check a string for correct UTF8 encoding.
 * Argument:
 *   s : the string.
 * Returns true in case of success, and false in case of error.
 * All exceptions are captured.
 *)
let validate : string -> bool = fun s ->
  let len = String.length s in
  let rec valid i =
    if i = len then
      true
    else if i > len then
      false
    else
      let (_, sz) = decode s i in
      valid (i + sz)
  in
  try valid 0 with _ -> false

(*
 * Compute the length of an UTF8 string.
 * Argument:
 *   s : the string (that is supposed to be valid).
 * Returns an int.
 *)
let length : string -> int = fun s ->
  let slen = String.length s in
  let rec len count pos =
    if pos >= slen then
      count
    else
      let (_, sz) = decode s pos in
      len (count + 1) (pos + sz)
  in len 0 0

(*
 * Compute the index of the n-th UTF8 character if it exists.
 * Arguments:
 *   s : the string (that is supposed to be valid),
 *   n : the character number (should be greater or equal to 0).
 * Returns the index.
 * Raise Out_of_bound in case the string is not long enough.
 *)
let nth_index : string -> int -> index = fun s n ->
  let len = String.length s in
  let rec nth_ind count pos =
    if pos >= len then
      raise Out_of_bound
    else if count = 0 then
      pos
    else
      let (_, sz) = decode s pos in
      nth_ind (count - 1) (pos + sz)
  in nth_ind n 0

(*
 * Compute the value of the n-th UTF8 character of a string if it exists.
 * Arguments:
 *   s : the string (that is supposed to be valid),
 *   n : the character number (should be greater or equal to 0).
 * Returns the character.
 * Raise Out_of_bound in case the string is not long enough.
 *)
let nth : string -> int -> (uchar * int) = fun s n ->
  let pos = nth_index s n in
  decode s pos

(*
 * Compute the index of the next UTF8 character.
 * Arguments:
 *   s : the string (that is supposed to be valid),
 *   i : the index of the current UTF8 character.
 * Returns the index of the next UTF8 character.
 * The behaviour is not specified if i is not a valid index in s, or if s is
 * not a valid UTF8 string.
 *)
let next : string -> index -> index = fun s i ->
  let (_, sz) = decode s i in
  i + sz

(*
 * Compute the index of the previous UTF8 character.
 * Arguments:
 *   s : the string (that is supposed to be valid),
 *   i : the index of the current UTF8 character.
 * Returns the index of the next UTF8 character.
 * The behaviour is not specified if i is not a valid index in s, or if s is
 * not a valid UTF8 string.
 *)
let prev : string -> index -> index = fun s i ->
  let ps = List.filter (fun i -> i >= 0) [i-1; i-2; i-3; i-4] in
  let rec try_until_found l =
    match l with
    | []    -> assert false
    | p::ps -> try
                 let (_, sz) = decode s p in
                 if i - sz <> p then assert false;
                 p
               with
                 _ -> try_until_found ps
  in try_until_found ps

(*
 * Compute the index of the first UTF8 character in a string.
 * Argument:
 *   s : a non-empty string (that is supposed to be valid).
 * Returns the index of the first UTF8 character in s.
 * Raise invalid_arg if the string s is empty.
 *)
let first : string -> index = fun s ->
  if s = "" then
    raise (invalid_arg "UTF8.first")
  else 0

(*
 * Compute the index of the last UTF8 character in a string.
 * Argument:
 *   s : a non-empty string (that is supposed to be valid).
 * Returns the index of the last UTF8 character in s.
 * Raise invalid_arg if the string s is empty.
 *)
let last : string -> index = fun s ->
  let len = String.length s in
  if len = 0 then
    raise (invalid_arg "UTF8.first")
  else
    prev s len
