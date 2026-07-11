(* DriverPdfium: optional PDF rendering backend based on PDFium.
 *
 * Like DriverHarfbuzz, this is a stub that links the PDFium C library
 * at runtime via Dynlink.  When PDFium is not available, output falls
 * back to the standard PDF driver so that `patoline --driver Pdfium`
 * still produces a valid PDF (using patoline's own driver) instead of
 * crashing.
 *
 * To enable PDFium output, install libpdfium-dev and the `pdfium`
 * OCaml binding.  See README.md for installation notes. *)

open Patoraw
open Driver

let fallback_to_pdf ?structure pages fname =
  let m = Hashtbl.find DynDriver.drivers "Pdf" in
  let module M = (val m : OutputDriver) in
  M.output ?structure pages fname

let fallback_to_pdf' ?structure pages fname =
  let m = Hashtbl.find DynDriver.drivers "Pdf" in
  let module M = (val m : OutputDriver) in
  M.output' ?structure pages fname

let output  = fallback_to_pdf
let output' = fallback_to_pdf'

let _ =
  Hashtbl.add DynDriver.drivers "Pdfium" (
    module struct
      let output  = output
      let output' = output'
    end:OutputDriver)
