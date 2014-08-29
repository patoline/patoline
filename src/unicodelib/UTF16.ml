open UChar

(*
 * Synonym of int for an index in a string.
 *)
type index = int

(*
 * Encode a unicode character into a UTF16 string.
 * Argument:
 *   i : the unicode character.
 * Returns a string of size either 2 or 4.
 * Raise invalid_arg if i is not in the U+0000..U+10FFFF range.
 *)
let encode : uchar -> string = fun u ->
  if u < 0 || u > 0x10FFFF then
    raise (invalid_arg "UF16.encode")
  else if u < 0x10000 then
    let s = String.create 2 in
    s.[0] <- char_of_int ((u lsr 8) land 0xFF);
    s.[1] <- char_of_int (u land 0xFF);
    s
  else
    let u' = u - 0x10000 in
    let w1 = ((u' lsr 10) land 0b1111111111) lor 0xD800 in
    let w2 = (u' land 0b1111111111) lor 0xDC00 in
    let s = String.create 4 in
    s.[0] <- char_of_int ((w1 lsr 8) land 0xFF);
    s.[1] <- char_of_int (w1 land 0xFF);
    s.[2] <- char_of_int ((w2 lsr 8) land 0xFF);
    s.[3] <- char_of_int (w2 land 0xFF);
    s

(*
 * Decode a UTF16 character at a given position in a string.
 * Arguments:
 *   s : the string,
 *   i : index where to look.
 * Returns a couple (c, l) where c is the code of the character and l is the
 * number of bytes read.
 * Raise invalid_arg if no valid UTF16 character starts at poisition i in s.
 *)
let decode : string -> index -> (uchar * int) = fun s i ->
  let l = String.length s in
  if i > l - 1 then
    raise (invalid_arg "UTF16.decode")
  else
    let w1 = ((Char.code s.[i]) lsl 16) land (Char.code s.[i+1]) in
    if w1 < 0xD800 || w1 > 0xDFFF then
      (w1, 2)
    else if w1 >= 0xD800 && w1 <= 0xD8FF then
      raise (invalid_arg "UTF16.decode")
    else if i > l - 3 then
      raise (invalid_arg "UTF16.decode")
    else
      let w2 = ((Char.code s.[i+2]) lsl 16) land (Char.code s.[i+3]) in
      if w2 < 0xDC00 || w2 > 0xDFFF then
        raise (invalid_arg "UTF16.decode")
      else
        let u1 = w1 land 0x1111111111 in
        let u2 = w2 land 0x1111111111 in
        let u = (u1 lsl 10) lor u2 in
        let u = u + 0x10000 in
        (u, 4)

(*
 * Decode a UTF16 character at a given position in a string.
 * Arguments:
 *   s : the string,
 *   i : index where to look.
 * Returns the character decoded.
 * Raise invalid_arg if s.[i] is not a valid first byte for a UTF16 character.
 *)
let look : string -> index -> uchar = fun s i ->
  fst (decode s i)

(*
 * Check a string for correct UTF16 encoding.
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
 * Compute the length of a UTF16 string.
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
 * Compute the index of the n-th UTF16 character if it exists.
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
 * Compute the value of the n-th UTF16 character of a string if it exists.
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
 * Compute the index of the next UTF16 character.
 * Arguments:
 *   s : the string (that is supposed to be valid),
 *   i : the index of the current UTF16 character.
 * Returns the index of the next UTF16 character.
 * The behaviour is not specified if i is not a valid index in s, or if s is
 * not a valid UTF16 string.
 *)
let next : string -> index -> index = fun s i ->
  let (_, sz) = decode s i in
  i + sz

(*
 * Compute the index of the previous UTF16 character.
 * Arguments:
 *   s : the string (that is supposed to be valid),
 *   i : the index of the current UTF16 character.
 * Returns the index of the next UTF16 character.
 * The behaviour is not specified if i is not a valid index in s, or if s is
 * not a valid UTF16 string.
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
 * Compute the index of the first UTF16 character in a string.
 * Argument:
 *   s : a non-empty string (that is supposed to be valid).
 * Returns the index of the first UTF16 character in s.
 * Raise invalid_arg if the string s is empty.
 *)
let first : string -> index = fun s ->
  if s = "" then
    raise (invalid_arg "UTF8.first")
  else 0

(*
 * Compute the index of the last UTF16 character in a string.
 * Argument:
 *   s : a non-empty string (that is supposed to be valid).
 * Returns the index of the last UTF16 character in s.
 * Raise invalid_arg if the string s is empty.
 *)
let last : string -> index = fun s ->
  let len = String.length s in
  if len = 0 then
    raise (invalid_arg "UTF8.last")
  else
    prev s len

(*
 * Check whether an index is out of the range of a string or not.
 * Arguments:
 *   s : the string,
 *   i : the index.
 * Returns true if the index is out of range, false otherwise.
 *)
let out_of_range : string -> index -> bool = fun s i ->
  i < 0 || i >= String.length s

(*
 * Buffer for UTF16 strings.
 *)
module Buf = 
  struct
    include Buffer
    type buf = string

    let add_char buf u =
      let s = encode u in
      for i = 0 to String.length s - 1 do
        Buffer.add_char buf s.[i]
      done

    let init len f =
      let buf = Buffer.create len in
      for c = 0 to len - 1 do
        add_char buf (f c)
      done;
      Buffer.contents buf
  end
