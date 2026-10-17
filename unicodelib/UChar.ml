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

(* NFKC normalization of a UTF-8 string. We delegate to [Uunf_string]
   which provides a one-shot [normalize_utf_8] wrapper. This restores
   behavior that was lost when the previous identity stub was
   introduced. *)
let nfkc_string : string -> string = fun s ->
  Uunf_string.normalize_utf_8 (`NFKC :> [< Uunf.form]) s

(* True when the given code point is a CJK Unified Ideograph, Hiragana,
   Katakana, or Hangul Syllable. Used by the line break classifier. *)
let is_cjk : uchar -> bool = fun c ->
  (0x3040 <= c && c <= 0x30FF)
  || (0x3400 <= c && c <= 0x4DBF)
  || (0x4E00 <= c && c <= 0x9FFF)
  || (0xA000 <= c && c <= 0xA4CF)
  || (0xAC00 <= c && c <= 0xD7A3)
  || (0xF900 <= c && c <= 0xFAFF)
  || (0x20000 <= c && c <= 0x2FFFD)
  || (0x30000 <= c && c <= 0x3FFFD)

(* Rough UAX#14 line breaking classes. We collapse the full
   classification (~30 classes) into four buckets that are sufficient
   for the paragraph optimizer in [typography/Document.boxify]:

     Space       - white space and zero-width characters (matches the
                   pre-existing [is_space] semantics);
     Ideographic - the CJK ranges plus kana and hangul syllables;
     Punctuation - the ASCII and CJK punctuation ranges;
     Other       - everything else (Latin, Greek, Cyrillic, ...).

   A finer-grained UAX#14 table will land in P1-2 follow-up work,
   but these four buckets are enough to enable inter-character break
   opportunities for CJK without spurious gaps in Latin text. *)
type line_break_class = Space | Ideographic | Punctuation | Other

let line_break_class : uchar -> line_break_class = fun c ->
  if is_space c then Space
  else if is_cjk c then Ideographic
  else if
    (* ASCII punctuation + CJK punctuation (general category Po, Ps, Pe,
       Pi, Pf, Pd, Pc in the relevant ranges). *)
    (0x21 <= c && c <= 0x2F)
    || (0x3A <= c && c <= 0x40)
    || (0x5B <= c && c <= 0x60)
    || (0x7B <= c && c <= 0x7E)
    || (0x3000 <= c && c <= 0x303F)
    || (0xFF00 <= c && c <= 0xFFEF)
  then Punctuation
  else Other

(* CJK kinsoku (行首禁则 / 行尾禁则) sets.

   [no_break_before]: characters that may not *begin* a line — closing
   brackets and trailing (full-width) punctuation.
   [no_break_after]: characters that may not *end* a line — opening
   brackets and opening quotes. *)
let no_break_before : uchar -> bool = fun c ->
  c = 0x29 (* ) *)  || c = 0x2C (* , *)  || c = 0x2E (* . *)
  || c = 0x3A (* : *)  || c = 0x3B (* ; *)  || c = 0x3F (* ? *)
  || c = 0x21 (* ! *)  || c = 0x5D (* ] *)  || c = 0x7D (* } *)
  || c = 0x2013 (* – *) || c = 0x2014 (* — *) || c = 0x2026 (* … *)
  || c = 0x2019 (* ’ *) || c = 0x201D (* ” *)
  (* full-width/CJK trailing punctuation *)
  || c = 0x3001 (* 、 *) || c = 0x3002 (* 。 *)
  || c = 0x3009 (* 〉 *) || c = 0x300B (* 》 *) || c = 0x300D (* 」 *)
  || c = 0x300F (* 』 *) || c = 0x3011 (* 】 *) || c = 0x3015 (* 〕 *)
  || c = 0x3017 (* 〗 *) || c = 0x3019 (* 〙 *) || c = 0x301B (* 〛 *)
  || c = 0x301F (* ｣ *)
  || c = 0xFF01 (* ！ *) || c = 0xFF09 (* ） *) || c = 0xFF0C (* ， *)
  || c = 0xFF0E (* ． *) || c = 0xFF1A (* ： *) || c = 0xFF1B (* ； *)
  || c = 0xFF1F (* ？ *)

let no_break_after : uchar -> bool = fun c ->
  c = 0x28 (* ( *) || c = 0x5B (* [ *) || c = 0x7B (* { *)
  || c = 0x2018 (* ‘ *) || c = 0x201C (* “ *)
  || c = 0x3008 (* 〈 *) || c = 0x300A (* 《 *) || c = 0x300C (* 「 *)
  || c = 0x300E (* 『 *) || c = 0x3010 (* 【 *) || c = 0x3014 (* 〔 *)
  || c = 0x3016 (* 〖 *) || c = 0x3018 (* 〘 *) || c = 0x301A (* 〚 *)
  || c = 0xFF08 (* （ *) || c = 0xFF3B (* ［ *) || c = 0xFF5B (* ｛ *)

(* Closing (trailing) punctuation after which a line *may* end.
   ASCII punctuation is deliberately excluded: "v1.2" must not be
   breakable after the dot. *)
let line_closing : uchar -> bool = fun c ->
  c = 0x2F (* / *) || c = 0x3A (* : *)  (* URLs may break after : and / *)
  || c = 0x3001 || c = 0x3002 || c = 0x3009 || c = 0x300B
  || c = 0x300D || c = 0x300F || c = 0x3011 || c = 0x3015
  || c = 0x3017 || c = 0x3019 || c = 0x301B || c = 0x301F
  || c = 0xFF01 || c = 0xFF09 || c = 0xFF0C || c = 0xFF0E
  || c = 0xFF1A || c = 0xFF1B || c = 0xFF1F
  || c = 0x2013 || c = 0x2014 || c = 0x2026
  || c = 0x2019 || c = 0x201D

(* [is_break_opportunity cp1 cp2] reports whether a line break is
   allowed between [cp1] (the preceding character) and [cp2] (the
   following one), following a UAX#14-flavoured heuristic enriched
   with CJK kinsoku rules:

     - both spaces            no break (collapse runs of white space)
     - either space           break
     - forbidden line start   no break (。，、… and closing brackets)
     - forbidden line end     no break (opening brackets and quotes)
     - both ideographic       break
     - ideographic + other    break (CJK/Latin boundary)
     - after closing punct.   break (a line may end with 。」…)
     - punctuation neighbours do not break inside a Latin run
   The function never reports a break inside a Latin word. *)
let is_break_opportunity : uchar -> uchar -> bool = fun prev next ->
  if no_break_before next then false
  else if no_break_after prev then false
  else
    match line_break_class prev, line_break_class next with
    | Space, Space -> false
    | Space, _ | _, Space -> true
    | Ideographic, Ideographic -> true
    | Ideographic, Other -> true
    | Other, Ideographic -> true
    | Punctuation, _ when line_closing prev -> true
    | Punctuation, _ | _, Punctuation -> false
    | Other, Other -> false
