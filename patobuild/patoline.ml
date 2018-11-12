(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
open Patconfig.PatConfig

(** Parse a string containing a command line options and returns the list.
    This function is useful to be able to forward command line options. *)
let parse_args =
  let open Earley_core.Earley in
  let arg = parser
    | "'" o:RE("[^']+") "'" -> o
    | '"' o:RE("[^']+") '"' -> o
    | o:RE("[^ \t\n'\"]+")  -> o
  in
  let args = parser arg+ in
  let parse_args str =
    try parse_string args (Earley_str.blank_regexp "[ \t]+") str
    with _ ->
      Printf.eprintf "Invalid command-line option list %S." str;
      exit 1
  in parse_args

(** Parse a string containing a list of ocamlfind packages separated by a
    comma. The list of the package names is returned. *)
let parse_packages =
  let open Earley_core.Earley in
  let pack_re = "[a-zA-Z][a-zA-Z0-9_.]*" in
  let packs = parser p:RE(pack_re) ps:{',' RE(pack_re)}* -> p::ps in
  let parse_packages str =
    try parse_string packs no_blank str
    with _ ->
      Printf.eprintf "Invalid list of ocamlfind packages %S." str;
      exit 1
  in parse_packages

let bin_args   = ref []
let opt_args   = ref []
let pp_args    = ref []
let local_path = ref []
let packages   = ref [ "patoline.patoraw"; "patoline.typography"
                     ; "patoline.format.DefaultFormat"
                     ; "earley"; "earley.str"]
let pat_format = ref None
let pat_driver = ref None
let do_clean   = ref false
let file       = ref None
let run_bin    = ref true

let add_bin_args l = bin_args := !bin_args @ l
let add_opt_args l = opt_args := !opt_args @ l
let add_pp_args  l = pp_args  := !pp_args  @ l

let add_file f =
  if !file <> None then
    begin
      Printf.eprintf "A file has already been given...\n";
      exit 1
    end;
  if not (Sys.file_exists f) then
    begin
      Printf.eprintf "The file %s does not exist...\n" f;
      exit 1
    end;
  file := Some f


let add_local_path p = local_path := !local_path @ [p]

let add_package p =
  if not (List.mem p !packages) then packages := !packages @ [p]
let add_packages s = List.iter add_package (parse_packages s)

let spec = Arg.align
  (* Arguments forwarded to the binary. *)
  [ ( "--extra-fonts-dir"
    , Arg.String (fun d -> add_bin_args ["--extra-fonts-dir"; d])
    , "dir Add a fonts directory to the search path." )
  ; ( "--extra-hyph-dir"
    , Arg.String (fun d -> add_bin_args ["--extra-hyph-dir"; d])
    , "dir Add an hyphenation dictionary directory to the search path." )
  ; ( "--font-filter"
    , Arg.String (fun c -> add_bin_args ["--font-filter"; c])
    , "cmd Add a font filter command for the SVG or Patonet drivers." )
  ; ( "--bin-args"
    , Arg.String (fun s -> add_bin_args (parse_args s))
    , "args Forward the given arguments to the binary." )

  (* Configuration of paths, packages, format and driver. *)
  ; ( "-I"
    , Arg.String add_local_path
    , "dir Add the given path to the source directories." )
  ; ( "--package"
    , Arg.String add_packages
    , "packs Use the provided ocamlfind packages." )
  ; ( "--format"
    , Arg.String (fun f -> pat_format := Some f)
    , "f Set the document format." )
  ; ( "--driver"
    , Arg.String (fun d -> pat_driver := Some d)
    , "d Set the document driver." )

  (* Other configurations. *)
  ; ( "-j"
    , Arg.Int (fun s -> Parallel.nb_threads := max !Parallel.nb_threads s)
    , "i Compile with the given number of threads." )
  ; ( "--verbose"
    , Arg.Int (fun l -> Build.verbose := l)
    , "i Set the verbosity level." )
  ; ( "--clean"
    , Arg.Set do_clean
    , " Cleanup the build directories." )
  ; ( "--bin"
    , Arg.Clear run_bin
    , " Does not run the produced binary, only produces it." )
  ; ( "--ascii"
    , Arg.Unit (fun _ -> add_pp_args ["--ascii"])
    , " Preprocess to ASCII files." )

  (* Forwarding of arguments to the compiler or to the preprocessor. *)
  ; ( "--opt-args"
    , Arg.String (fun s -> add_opt_args (parse_args s))
    , "args Forwart the given arguments to the compiler." )
  ; ( "--pp-args"
    , Arg.String (fun s -> add_pp_args (parse_args s))
    , "args Forwart the given arguments to the preprocessor." )
  ]

let usage =
  Printf.sprintf "Usage: %s [drivers | formats | config | [options] [file]]"

let _ =
  match Sys.argv with
  | [| _ ; "drivers" |] -> let f = Printf.printf "%s\n" in
                           List.iter f patoconfig.drivers; exit 0
  | [| _ ; "formats" |] -> let f = Printf.printf "%s\n" in
                           List.iter f patoconfig.formats; exit 0
  | [| _ ; "config"  |] -> print_config stdout; exit 0
  | _                   -> Arg.parse spec add_file (usage Sys.argv.(0))

(* The data after parsing the command-line arguments. *)
let cfg =
  let path = "." :: !local_path in (* FIXME remove duplicates *)
  let driver_packages =
    match !pat_driver with
    | None                                      -> ["patoline.driver.Pdf"]
    | Some d when List.mem d patoconfig.drivers -> ["patoline.driver." ^ d]
    | _                                         -> []
  in
  let format_packages =
    match !pat_format with
    | None                                      -> []
    | Some "DefaultFormat"                      -> []
    | Some f when List.mem f patoconfig.formats -> ["patoline.format." ^ f]
    | _                                         -> []
  in
  let packages = !packages @ format_packages @ driver_packages in
  let open Build in
  { bin_args   = !bin_args
  ; opt_args   = !opt_args
  ; pp_args    = !pp_args
  ; packages
  ; path
  ; pat_format = !pat_format
  ; pat_driver = !pat_driver
  ; run_binary = !run_bin }

(* Cleaning if required. *)
let _ = if !do_clean then Build.clean_build_dirs cfg

(* Compilation of the file. *)
let _ =
  match !file with
  | None    -> if !Build.verbose > 1 then Printf.eprintf "Nothing to do.\n%!"
  | Some fn -> Build.compile cfg fn
