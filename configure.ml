#!/usr/bin/env -S ocaml unix.cma

let default_prefix = "/usr/local"
let deps =
  [ ("DriverCairo", "cairo2"   )
  ; ("DriverGL"   , "lablgl"   )
  ; ("DriverImage", "lablgl"   )
  ; ("Patonet"    , "kryptokit") ]

(** Get the prefix from the Opam variables, fallback to [default_prefix]. *)
let opam_prefix () =
  try
    let ic = Unix.open_process_in "opam var prefix" in
    let res = input_line ic in
    ignore (Unix.close_process_in ic); res
  with _ -> default_prefix

(** Check whether some package is installed. *)
let has_package name =
  Sys.command ("ocamlfind query -qo -qe " ^ name) = 0

(** Read command lien arguments. *)
let prefix =
  let prefix = ref None in
  let spec =
    Arg.[("--prefix", String(fun s -> prefix := Some(s)), " Sets the PREFIX")]
  in
  let anon s =
    raise (Arg.Bad (Printf.sprintf "don't know what to do with %S" s))
  in
  Arg.parse (Arg.align spec) anon "Usage: ./configure [--prefix=PREFIX]";
  let prefix =
    match !prefix with
    | Some(prefix) -> prefix
    | None         -> opam_prefix ()
  in
  Printf.printf "Using prefix [%s]\n%!" prefix; prefix

(** Share directory is relative to prefix. *)
let share = Filename.concat prefix "share"

(** Generate ["unicodelib/config.ml"]. *)
let _ =
  let data_file = Filename.concat share "patoline/unicode/unicode.data" in
  let oc = open_out "unicodelib/config.ml" in
  Printf.fprintf oc "let unicode_data_file =\n";
  Printf.fprintf oc "  \"%s\"\n%!" data_file;
  close_out oc

(** Generate ["patconfig/patDefault.ml"]. *)
let _ =
  let fonts_dir = Filename.concat share "patoline/fonts" in
  let grammars_dir = Filename.concat share "patoline/grammars" in
  let hyphen_dir = Filename.concat share "patoline/hyphen" in
  let formats =
    let fs = Array.to_list (Sys.readdir "formats") in
    let fs = List.filter (fun f -> Filename.check_suffix f ".ml") fs in
    List.map (fun f -> Filename.chop_suffix f ".ml") fs
  in
  let all_drivers = Array.to_list (Sys.readdir "drivers") in
  let drivers =
    let pred d =
      if List.mem_assoc d deps then has_package (List.assoc d deps)
      else true
    in
    List.filter pred all_drivers
  in
  let oc = open_out "patconfig/patDefault.ml" in
	Printf.fprintf oc "let fonts_dir          = %S\n" fonts_dir;
	Printf.fprintf oc "let grammars_dir       = %S\n" grammars_dir;
	Printf.fprintf oc "let hyphen_dir         = %S\n" hyphen_dir;
	Printf.fprintf oc "let extra_fonts_dir    = []\n";
	Printf.fprintf oc "let extra_grammars_dir = []\n";
	Printf.fprintf oc "let extra_hyphen_dir   = []\n";
  Printf.fprintf oc "let formats            = [ ";
  List.iter (Printf.fprintf oc "\"%s\"; ") formats;
  Printf.fprintf oc "]\n";
  Printf.fprintf oc "let drivers            = [ ";
  List.iter (Printf.fprintf oc "\"%s\"; ") drivers;
  Printf.fprintf oc "]\n";
  close_out oc
