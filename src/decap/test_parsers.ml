(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

(* FILE Borowed from Planck parser combinator *)

(* This is required to call Syntaxerr.report_error correctly. *)
let _ = Location.input_name := ""

(* necessite la librairie UNIX *)
let nb_tests = 10

let with_time f x =
  Gc.full_major ();
  let {Unix.tms_utime = ut;Unix.tms_stime = st} = Unix.times () in
  try
    for i = 1 to nb_tests do
      ignore (f x);
    done;
    let r = f x in
    let {Unix.tms_utime = ut';Unix.tms_stime = st'} = Unix.times () in
    (r, (ut' -. ut) +. (st' -. st))
  with e ->
    let {Unix.tms_utime = ut';Unix.tms_stime = st'} = Unix.times () in
    Format.eprintf "exception after: %.2fs@." ((ut' -. ut) +. (st' -. st));
    flush stderr;
    raise e

(* GLR *)
let rec parse_implementation path = 
  Decap.handle_exception (Decap.parse_file Pa_compose.Final.structure Pa_ocaml_prelude.blank) path

let rec parse_interface path = 
  Decap.handle_exception (Decap.parse_file Pa_compose.Final.signature Pa_ocaml_prelude.blank) path

(* OCaml *)
let parse_implementation_orig f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf f;
  let res = Parse.implementation lexbuf in
  close_in ic;
  res

let parse_interface_orig f =
  let ic = open_in f in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf f;
  let res = Parse.interface lexbuf in
  close_in ic;
  res

(* camlp4 *)
open Camlp4.PreCast;;

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make
          (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation))))

let parse_implem_camlp4 f =
  let ic = open_in f in
  let strm = Stream.of_channel ic in
  let res = Caml.parse_implem (Loc.mk f) strm in
  close_in ic;
  res

let parse_interf_camlp4 f =
  let ic = open_in f in
  let strm = Stream.of_channel ic in
  let res = Caml.parse_interf (Loc.mk f) strm in
  close_in ic;
  res

(* Tests *)
let _ = 
  let time_sum_orig = ref 0.0 in
  let time_sum_camlp4 = ref 0.0 in
  let time_sum_pa_ocaml = ref 0.0 in

  let print_times time_orig time_camlp4 time_pa_ocaml = 
    Format.eprintf "x%f x%f (original %f, camlp4 %f, pa_ocaml %f)@."
      (time_pa_ocaml /. time_orig)
      (time_pa_ocaml /. time_camlp4)
      time_orig time_camlp4 time_pa_ocaml
  in

  Array.iteri (fun i path -> if i <> 0 then begin
    Format.eprintf "%s@." path;
    if Filename.check_suffix path ".ml" then begin
      let res, time_orig = with_time parse_implementation_orig path in
      let p4res, time_camlp4 = with_time parse_implem_camlp4 path in
      let plres, time_pa_ocaml = with_time parse_implementation path in
      time_sum_orig := !time_sum_orig +. time_orig;
      time_sum_camlp4 := !time_sum_camlp4 +. time_camlp4;
      time_sum_pa_ocaml := !time_sum_pa_ocaml +. time_pa_ocaml;
      print_times time_orig time_camlp4 time_pa_ocaml
    end else if Filename.check_suffix path ".mli" then begin
        let res, time_orig = with_time parse_interface_orig path in
        let p4res, time_camlp4 = with_time parse_interf_camlp4 path in
        let plres, time_pa_ocaml = with_time parse_interface path in
        time_sum_orig := !time_sum_orig +. time_orig;
        time_sum_camlp4 := !time_sum_camlp4 +. time_camlp4;
        time_sum_pa_ocaml := !time_sum_pa_ocaml +. time_pa_ocaml;
        print_times time_orig time_camlp4 time_pa_ocaml;
    end;
  end) Sys.argv;
  prerr_endline "ALL TEST ENDED";
  print_times !time_sum_orig !time_sum_camlp4 !time_sum_pa_ocaml