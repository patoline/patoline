(* pa_convert.ml - Generates charset conversion modules from .TXT mapping files.
   Rewritten to not use earley quotation syntax (<:expr<...>>, <:struct<...>>)
   which is no longer supported by earley master. Instead we construct AST nodes
   directly using Ast_helper and output via Pprintast. *)

open Earley_core
open Earley
open Ast_helper
open Parsetree
open Asttypes
open Longident

let loc = Location.none

(* Helper to create an identifier expression *)
let evar s = Exp.ident ~loc {txt = Lident s; loc}

(* Helper to create an integer literal expression *)
let eint n = Exp.constant ~loc (Const.int n)

(* Helper to create a string literal expression *)
let estring s = Exp.constant ~loc (Const.string s)

(* Helper to create function application *)
let eapply f args = Exp.apply ~loc f (List.map (fun a -> (Nolabel, a)) args)

(* Helper to create a simple pattern variable *)
let pvar s = Pat.var ~loc {txt = s; loc}

(* Helper to create a type annotation *)
let ptyp_constr name = Typ.constr ~loc {txt = Lident name; loc} []

(* Blank function: skip spaces, tabs, returns, and # comments *)
let blank str pos =
  let rec fn state ((str, pos) as cur) =
    let (c, str', pos') = Input.read str pos in
    let next = (str', pos') in
    match state, c with
    | _   , '\255'              -> cur
    | `Ini, (' ' | '\t' | '\r') -> fn `Ini next
    | `Ini, '#'                 -> fn `Com next
    | `Ini, _                   -> cur
    | `Com, '\n'                -> fn `Ini next
    | `Com, _                   -> fn `Com next
  in fn `Ini (str, pos)

(* Parser for hexadecimal integers *)
let ex_int = parser i:''0x[0-9a-fA-F]+'' -> int_of_string i

(* Single mapping parser: source_code [whitespace] dest_code *)
let mapping = change_layout (
    parser i:ex_int _:''[ \t]*'' j:ex_int?[-1]
  ) no_blank

(* Build the output OCaml structure from parsed mappings *)
let build_file (ms : (int * int) list) : structure =
  (* Build the array initialization expression:
     arr.(i) <- j; arr.(i2) <- j2; ... arr *)
  let combine (i, j) e =
    Exp.sequence ~loc
      (eapply (evar "Array.unsafe_set") [evar "arr"; eint i; eint j])
      e
  in
  let init_expr = List.fold_right combine ms (evar "arr") in

  (* exception Undefined *)
  let exn_decl =
    let ctor = Te.constructor ~loc {txt = "Undefined"; loc}
                 (Pext_decl (Pcstr_tuple [], None)) in
    Str.exception_ ~loc
      { ptyexn_constructor = ctor;
        ptyexn_loc = loc;
        ptyexn_attributes = [] }
  in

  (* let conversion_array : int array = let arr = Array.make 256 (-1) in <init> *)
  let conv_array =
    let body =
      Exp.let_ ~loc Nonrecursive
        [Vb.mk ~loc (pvar "arr")
           (eapply (Exp.ident ~loc {txt = Ldot (Lident "Array", "make"); loc})
              [eint 256; eint (-1)])]
        init_expr
    in
    let typ = Typ.constr ~loc {txt = Lident "array"; loc} [ptyp_constr "int"] in
    Str.value ~loc Nonrecursive
      [Vb.mk ~loc
         (Pat.constraint_ ~loc (pvar "conversion_array") typ)
         body]
  in

  (* let to_uchar : char -> UChar.uchar = fun c -> ... *)
  let to_uchar =
    let body =
      Exp.fun_ ~loc Nolabel None (pvar "c")
        (Exp.let_ ~loc Nonrecursive
           [Vb.mk ~loc (pvar "i")
              (eapply (Exp.ident ~loc {txt = Ldot (Lident "Char", "code"); loc})
                 [evar "c"])]
           (Exp.sequence ~loc
              (Exp.ifthenelse ~loc
                 (eapply (evar "||")
                    [eapply (evar "<") [evar "i"; eint 0];
                     eapply (evar ">") [evar "i"; eint 255]])
                 (eapply (evar "raise") [Exp.construct ~loc {txt = Lident "Undefined"; loc} None])
                 None)
              (Exp.let_ ~loc Nonrecursive
                 [Vb.mk ~loc (pvar "u")
                    (Exp.apply ~loc
                       (Exp.ident ~loc {txt = Lident "Array.unsafe_get"; loc})
                       [(Nolabel, evar "conversion_array"); (Nolabel, evar "i")])]
                 (Exp.sequence ~loc
                    (Exp.ifthenelse ~loc
                       (eapply (evar "<") [evar "u"; eint 0])
                       (eapply (evar "raise") [Exp.construct ~loc {txt = Lident "Undefined"; loc} None])
                       None)
                    (evar "u")))))
    in
    let typ =
      Typ.arrow ~loc Nolabel (ptyp_constr "char")
        (Typ.constr ~loc {txt = Ldot (Lident "UChar", "uchar"); loc} [])
    in
    Str.value ~loc Nonrecursive
      [Vb.mk ~loc
         (Pat.constraint_ ~loc (pvar "to_uchar") typ)
         body]
  in

  (* Helper: build to_utfN functions *)
  let mk_to_utf name modname =
    let body =
      Exp.fun_ ~loc Nolabel None (pvar "s")
        (eapply
           (Exp.ident ~loc {txt = Ldot (Lident modname, "init"); loc})
           [eapply (Exp.ident ~loc {txt = Ldot (Lident "String", "length"); loc})
              [evar "s"];
            Exp.fun_ ~loc Nolabel None (pvar "i")
              (eapply (evar "to_uchar")
                 [eapply (Exp.ident ~loc {txt = Ldot (Lident "String", "get"); loc})
                    [evar "s";
                     eapply (evar "-") [evar "i"; eint 1]]])])
    in
    let typ =
      Typ.arrow ~loc Nolabel (ptyp_constr "string") (ptyp_constr "string")
    in
    Str.value ~loc Nonrecursive
      [Vb.mk ~loc
         (Pat.constraint_ ~loc (pvar name) typ)
         body]
  in

  [exn_decl; conv_array; to_uchar;
   mk_to_utf "to_utf8" "UTF8";
   mk_to_utf "to_utf16" "UTF16";
   mk_to_utf "to_utf32" "UTF32"]

(* Main: parse mappings then output the structure *)
let mappings =
  parser ms:mapping* "\x0A\x1A"? EOF -> build_file ms

let () =
  (* Parse command-line: [--ascii] filename *)
  let file = ref None in
  let _ascii = ref false in
  let spec = [("--ascii", Arg.Set _ascii, "ASCII mode")] in
  Arg.parse spec (fun s -> file := Some s) "pa_convert [--ascii] <file>";
  let filename = match !file with
    | Some f -> f
    | None -> Printf.eprintf "No input file\n"; exit 1
  in
  let ic = open_in filename in
  let structure =
    Earley.handle_exception
      (Earley.parse_channel ~filename mappings blank) ic
  in
  close_in ic;
  Format.printf "%a@." Pprintast.structure structure
