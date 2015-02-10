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

let quote_tuple : Location.t -> expression list -> expression =
  Pa_ast.exp_tuple

let quote_const : Location.t -> string -> expression list -> expression =
  Pa_ast.exp_const

let rec quote_longident : Location.t -> Longident.t -> expression =
  fun _loc l ->
    match l with
    | Lident s       -> let s = quote_string _loc s in
                        Pa_ast.exp_const _loc "Lident" [s]
    | Ldot (l, s)    -> let l = quote_longident _loc l in
                        let s = quote_string _loc s in
                        Pa_ast.exp_const _loc "Ldot" [l, s]
    | Lapply (l, l') -> let l = quote_longident _loc l in
                        let l' = quote_longident _loc l' in
                        Pa_ast.exp_const _loc "Lapply" [l, l']

let quote_record : Location.t -> (string * expression) list -> expression =
  Pa_ast.exp_record

let quote_position : Location.t -> Lexing.position -> expression =
  fun _loc {Lexing.pos_fname = pfn; Lexing.pos_lnum = ln; Lexing.pos_bol = bl; Lexing.pos_cnum = pcn} ->
    let pfn = quote_string _loc pfn in
    let ln  = quote_int _loc ln in
    let bl  = quote_int _loc bl in
    let pcn = quote_int _loc pcn in
    quote_record _loc
      [("pos_fname",pfn); ("pos_lnum",ln); ("pos_bol",bl); ("pos_cnum",pcn)]

let quote_location_t : Location.t -> Location.t -> expression =
  fun _loc {Location.loc_start = ls; Location.loc_end = le; Location.loc_ghost = g} ->
    let ls = quote_position _loc ls in
    let le = quote_position _loc le in
    let g  = quote_bool _loc g in
    quote_record _loc [("loc_start",ls); ("loc_end",le); ("loc_ghost",g)]
