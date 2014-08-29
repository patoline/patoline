open UChar

include UTF.Make(
  struct
    (*
     * Encode a unicode character into a UTF32 string.
     * Argument:
     *   i : the unicode character.
     * Returns a string of size 4.
     * Raise invalid_arg if i is not in the unicode range.
     *)
    let encode : uchar -> string = fun u ->
      if u < 0 || u > 0x10FFFF || (u >= 0xD800 && u <= 0xDFFF) then
        raise (invalid_arg "UF16.encode")
      else
        let s = String.create 4 in
        s.[0] <- char_of_int ((u lsr 24) land 0xFF);
        s.[1] <- char_of_int ((u lsr 16) land 0xFF);
        s.[2] <- char_of_int ((u lsr 8) land 0xFF);
        s.[3] <- char_of_int (u land 0xFF);
        s

    (*
     * Decode a UTF32 character at a given position in a string.
     * Arguments:
     *   s : the string,
     *   i : index where to look.
     * Returns a couple (c, l) where c is the code of the character and l is the
     * number of bytes read (always 4 in this case).
     * Raise invalid_arg if no valid UTF32 character starts at poisition i in s.
     *)
    let decode : string -> index -> (uchar * int) = fun s i ->
      let l = String.length s in
      if i > l - 3 then
        raise (invalid_arg "UTF16.decode")
      else
        let u0 = Char.code s.[i] in
        let u1 = Char.code s.[i+1] in
        let u2 = Char.code s.[i+2] in
        let u3 = Char.code s.[i+3] in
        (* FIXME problem on 32bit architecture) *)
        let u = ((((u0 lsl 8) lor u1) lsl 8) lor u2) lsl 8 lor u3 in
        (u, 4)
  end)
