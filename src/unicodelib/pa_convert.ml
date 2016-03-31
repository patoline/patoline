open Decap
open Pa_ocaml_prelude

#define LOCATE locate

module Ext = functor(In:Extension) -> struct
include In

(* Blank function *)
let blank str pos =
  let rec fn state prev ((str, pos) as cur) =
    let (c, str', pos') = Input.read str pos in
    let next = (str', pos') in
    match state, c with
    | _   , '\255'              -> cur (* EOF reached *)
    | `Ini, (' ' | '\t' | '\r') -> fn `Ini cur next
    | `Ini, '#'                 -> fn `Com cur next
    | `Ini, _                   -> cur
    | `Com, '\n'                -> fn `Ini cur next
    | `Com, _                   -> fn `Com cur next
  in fn `Ini (str, pos) (str, pos)

(* Parser for hexadecimal integers *)
let ex_int = parser i:''0x[0-9a-fA-F]+'' -> int_of_string i

(* Single mapping parser *)
let mapping = change_layout (
    parser i:ex_int _:''[ \t]*'' j:ex_int?[-1]
  ) no_blank

let build_file _loc ms =
  let combine (i, j) e = <:expr<arr.($int:i$) <- $int:j$; $e$>> in
  let e = List.fold_right combine ms <:expr<arr>> in
  <:struct<
    exception Undefined

    let conversion_array : int array =
      let arr = Array.make 256 (-1) in
      $e$

    let to_uchar : char -> UChar.uchar = fun c ->
      let i = Char.code c in
      if i < 0 || i > 255 then raise Undefined;
      let u = conversion_array.(i) in
      if u < 0 then raise Undefined;
      u

    let to_utf8 : string -> string = fun s ->
      UTF8.init (String.length s) (fun i -> to_uchar s.[i-1])

    let to_utf16 : string -> string = fun s ->
      UTF16.init (String.length s) (fun i -> to_uchar s.[i-1])

    let to_utf32 : string -> string = fun s ->
      UTF32.init (String.length s) (fun i -> to_uchar s.[i-1])
  >>

let mappings =
  parser ms:mapping* "\x0A\x1A"? EOF -> build_file _loc ms

let entry_points = (".TXT", Implementation (mappings, blank)) :: entry_points

end

(* Creating and running the extension *)
module PatolineDefault = Pa_ocaml.Make(Ext(Pa_ocaml_prelude.Initial))
module M = Pa_main.Start(PatolineDefault)
