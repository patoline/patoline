open UChar

include UTF.Make(
  struct
    (*
     * Encode a unicode character into a UTF8 string.
     * Argument:
     *   i : the unicode character.
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
        let s = Bytes.create 2 in Bytes.set s 0 c0; Bytes.set s 1 c1; s
      else if i <= 0xFFFF then
        let c0 = char_of_int (((i lsr 12) land 0b00001111) lor 0b11100000) in
        let c1 = char_of_int (((i lsr 6)  land 0b00111111) lor 0b10000000) in
        let c2 = char_of_int ((i          land 0b00111111) lor 0b10000000) in
        let s = Bytes.create 3 in
        Bytes.set s 0 c0; Bytes.set s 1 c1;
        Bytes.set s 2 c2; s
      else if i <= 0x10FFFF then
        let c0 = char_of_int (((i lsr 18) land 0b00000111) lor 0b11110000) in
        let c1 = char_of_int (((i lsr 12) land 0b00111111) lor 0b10000000) in
        let c2 = char_of_int (((i lsr 6)  land 0b00111111) lor 0b10000000) in
        let c3 = char_of_int ((i          land 0b00111111) lor 0b10000000) in
        let s = Bytes.create 4 in
        Bytes.set s 0 c0; Bytes.set s 1 c1;
        Bytes.set s 2 c2; Bytes.set s 3 c3; s
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
  end)
