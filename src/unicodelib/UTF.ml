open UChar

module type EncDec =
  sig
    (*
     * Encode a unicode character into a string.
     * Argument:
     *   u : the unicode character.
     * Returns a string containing exactly the number of bytes of the encoded
     * character. Raise invalid_arg if u is not in the unicode range.
     *)
    val encode : uchar -> string

    (*
     * Decode a unicode character at a given position in a string.
     * Arguments:
     *   s : the string,
     *   i : index where to look.
     * Returns a couple (u, l) where u is the code of the character and l is
     * the number of bytes read.
     * Raise invalid_arg if s.[i] is not a valid first byte for an encoded
     * character, if the encoding is not valid or if s is not long enough.
     *)
    val decode : string -> index -> (uchar * int)
  end

module Make = functor ( ED : EncDec ) ->
  struct
    include ED

    (*
     * Decode a unicode character at a given position in a string.
     * Arguments:
     *   s : the string,
     *   i : index where to look.
     * Returns the code of the character.
     * Raise invalid_arg if s.[i] is not a valid first byte for an encoded
     * character, if the encoding is not valid or if s is not long enough.
     *)
    let look : string -> index -> uchar = fun s i ->
      fst (decode s i)

    (*
     * Check a string for correct encoding.
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
     * Compute the length of an encoded unicode string.
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
     * Compute the index of the n-th unicode character ecoded in a string if
     * it exists.
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
     * Compute the value of the n-th unicode character encoded in a string if
     * it exists.
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
     * Compute the index of the next unicode character encoded in a string
     * starting from a given index.
     * Arguments:
     *   s : the string (that is supposed to be valid),
     *   i : the index of the current encoded character.
     * Returns the index of the next encoded character.
     * The behaviour is not specified if i is not a valid index in s, or if s
     * is not a valid unicode string.
     *)
    let next : string -> index -> index = fun s i ->
      let (_, sz) = decode s i in
      i + sz

    (*
     * Compute the index of the previous unicode character encoded in a string
     * starting from a given index.
     * Arguments:
     *   s : the string (that is supposed to be valid),
     *   i : the index of the current encoded character.
     * Returns the index of the next encoded character.
     * The behaviour is not specified if i is not a valid index in s, or if s is
     * not a valid unicode string.
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
     * Compute the index of the first unicode character in a string.
     * Argument:
     *   s : a non-empty string (that is supposed to be valid).
     * Returns the index of the first unicode character in s.
     * Raise invalid_arg if the string s is empty.
     *)
    let first : string -> index = fun s ->
      if s = "" then
        raise (invalid_arg "UTF8.first")
      else 0

    (*
     * Compute the index of the last unicode character in a string.
     * Argument:
     *   s : a non-empty string (that is supposed to be valid).
     * Returns the index of the last unicode  character in s.
     * Raise invalid_arg if the string s is empty.
     *)
    let last : string -> index = fun s ->
      let len = String.length s in
      if len = 0 then
        raise (invalid_arg "UTF8.last")
      else
        prev s len

    (*
     * Buffer for unicode strings.
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

  end

