(* DriverHarfbuzz: optional text-shaping backend.
 *
 * This module provides a stub that delegates the actual shaping to
 * the harfbuzz OCaml binding when the optional dependency is installed.
 * The OCaml package "harfbuzz" depends on libharfbuzz-dev (C library).
 *
 * When harfbuzz is NOT available on the system, the build is skipped
 * via the `(optional)` flag in `dune`.  The stub is included here so
 * `patoline --driver Harfbuzz` falls back to the standard PDF driver
 * if harfbuzz is missing.
 *
 * At runtime, we link the harfbuzz library through `Dynlink`.  That
 * way users can install harfbuzz after installing patoline without
 * needing a rebuild. *)

open Patoraw
open Driver

let dynlink_available = ref true

let ensure_harfbuzz () =
  if !dynlink_available then begin
    try
      Dynlink.loadfile (Filename.concat (Filename.current_dir_name) "dllharfbuzz.so");
      true
    with _ ->
      Printf.eprintf
        "[DriverHarfbuzz] harfbuzz library not available, falling back to PDF.\n%!";
      dynlink_available := false;
      false
  end else false

let fallback ?structure pages fname =
  let m = Hashtbl.find DynDriver.drivers "Pdf" in
  let module M = (val m : OutputDriver) in
  M.output ?structure pages fname

let fallback' ?structure pages fname =
  let m = Hashtbl.find DynDriver.drivers "Pdf" in
  let module M = (val m : OutputDriver) in
  M.output' ?structure pages fname

let output  = fallback
let output' = fallback'

let _ =
  ignore (ensure_harfbuzz ());
  Hashtbl.add DynDriver.drivers "Harfbuzz" (
    module struct
      let output  = output
      let output' = output'
    end:OutputDriver)
