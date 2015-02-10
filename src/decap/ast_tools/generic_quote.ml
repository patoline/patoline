open Asttypes
open Parsetree
open Longident

(* Generic functions *)
type loct = Location.t
type exp = expression

let quote_bool : loct -> bool -> exp =
  assert false (* TODO *)

let quote_int : loct -> int -> exp =
  assert false (* TODO *)

let quote_char : loct -> char -> exp =
  assert false (* TODO *)

let quote_string : loct -> string -> exp =
  assert false (* TODO *)

let quote_int32 : loct -> int32 -> exp =
  assert false (* TODO *)

let quote_int64 : loct -> int64 -> exp =
  assert false (* TODO *)

let quote_nativeint : loct -> nativeint -> exp =
  assert false (* TODO *)

let quote_option : 'a. (loct -> 'a -> exp) -> loct -> 'a option -> exp =
  fun qe _loc eo -> assert false (* TODO *)

let quote_list : 'a. (loct -> 'a -> exp) -> loct -> 'a list -> exp =
  fun qe _loc el -> assert false (* TODO *)

let quote_location_t : loct -> loct -> exp =
  assert false (* TODO *)

let quote_longident : loct -> Longident.t -> exp =
  assert false (* TODO *)

let quote_class_infos : 'a. (loct -> 'a -> exp) -> loct -> 'a class_infos -> exp =
  fun qe _loc eci -> assert false (* TODO *)

let quote_include_infos : 'a. (loct -> 'a -> exp) -> loct -> 'a include_infos -> exp =
  fun qe _loc eii -> assert false (* TODO *)

let quote_loc : 'a. (loct -> 'a -> exp) -> loct -> 'a loc -> exp =
  fun qe _loc el -> assert false (* TODO *)

let quote_tuple : loct -> exp list -> exp =
  assert false (* TODO *)

let quote_const : loct -> string -> exp list -> exp =
  assert false (* TODO *)

let quote_record : loct -> (string * exp) list -> exp =
  assert false (* TODO *)

