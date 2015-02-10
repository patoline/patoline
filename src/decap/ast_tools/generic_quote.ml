open Asttypes
open Parsetree
open Longident

(* Generic functions *)
let quote_bool : Location.t -> bool -> expression =
  Pa_ast.exp_bool

let quote_int : Location.t -> int -> expression =
  Pa_ast.exp_int

let quote_char : Location.t -> char -> expression =
  Pa_ast.exp_char

let quote_string : Location.t -> string -> expression =
  Pa_ast.exp_string

let quote_int32 : Location.t -> int32 -> expression =
  Pa_ast.exp_int32

let quote_int64 : Location.t -> int64 -> expression =
  Pa_ast.exp_int64

let quote_nativeint : Location.t -> nativeint -> expression =
  Pa_ast.exp_nativeint

let quote_option : 'a. (Location.t -> 'a -> expression) -> Location.t -> 'a option -> expression =
  fun qe _loc eo ->
    let e =
      match eo with
      | None   -> None
      | Some e -> Some (qe _loc e)
    in Pa_ast.exp_option _loc e

let quote_list : 'a. (Location.t -> 'a -> expression) -> Location.t -> 'a list -> expression =
  fun qe _loc el ->
    let el = List.map (qe _loc) el in
    Pa_ast.exp_list _loc el

let quote_location_t : Location.t -> Location.t -> expression =
  assert false (* TODO *)

let quote_longident : Location.t -> Longident.t -> expression =
  assert false (* TODO *)

let quote_class_infos : 'a. (Location.t -> 'a -> expression) -> Location.t -> 'a class_infos -> expression =
  fun qe _loc eci -> assert false (* TODO *)

let quote_include_infos : 'a. (Location.t -> 'a -> expression) -> Location.t -> 'a include_infos -> expression =
  fun qe _loc eii -> assert false (* TODO *)

let quote_loc : 'a. (Location.t -> 'a -> expression) -> Location.t -> 'a loc -> expression =
  fun qe _loc el -> assert false (* TODO *)

let quote_tuple : Location.t -> expression list -> expression =
  assert false (* TODO *)

let quote_const : Location.t -> string -> expression list -> expression =
  assert false (* TODO *)

let quote_record : Location.t -> (string * expression) list -> expression =
  assert false (* TODO *)

