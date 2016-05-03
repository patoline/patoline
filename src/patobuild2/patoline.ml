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
open PatConfig

(** Parse a string containing a command line options and returns the list.
    This function is useful to be able to forward command line options. *)
let parse_args =
  let open Decap in
  let arg = parser
    | "'" o:RE("[^']+") "'" -> o
    | '"' o:RE("[^']+") '"' -> o
    | o:RE("[^ \t\n'\"]+")  -> o
  in
  let args = parser arg+ in
  let parse_args str =
    try parse_string args (blank_regexp "[ \t]+") str
    with _ ->
      Printf.eprintf "Invalid command-line option list %S." str;
      exit 1
  in parse_args

(** Parse a string containing a list of ocamlfind packages separated by a
    comma. The list of the package names is returned. *)
let parse_packages =
  let open Decap in
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
let build_dir  = ref (Some "_patobuild")
let local_path = ref []
let packages   = ref ["rawlib"; "db"; "Typography"]
let pat_format = ref None
let pat_driver = ref None
let j          = ref 1
let verbose    = ref 0
let do_clean   = ref false
let files      = ref []

let add_bin_args l = bin_args := !bin_args @ l
let add_opt_args l = opt_args := !opt_args @ l
let add_pp_args  l = pp_args  := !pp_args  @ l

let add_file f =
  if not (Sys.file_exists f) then
    begin
      Printf.eprintf "The file %s does not exist...\n" f;
      exit 1
    end;
  files := !files @ [f]
 

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

  (* Build directory configuration. *)
  ; ( "--build-dir"
    , Arg.String (fun d -> build_dir := Some d)
    , "dir Set the build directory." )
  ; ( "--no-build-dir"
    , Arg.Unit (fun () -> build_dir := None)
    , " Disable the build directory." )

  (* Configuration of paths, packages, format and driver. *)
  ; ( "-I"
    , Arg.String add_local_path
    , "dir Add the given path to the source directories." )
  ; ( "-package"
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
    , Arg.Int (fun s -> j := max !j s)
    , "i Compile with the given number of threads." )
  ; ( "--verbose"
    , Arg.Int (fun l -> verbose := l)
    , "i Set the verbosity level." )
  ; ( "--clean"
    , Arg.Set do_clean
    , " Cleanup the build directory." )

  (* Forwarding of arguments to the compiler or to the preprocessor. *)
  ; ( "--opt-args"
    , Arg.String (fun s -> add_opt_args (parse_args s))
    , "args Forwart the given arguments to the compiler." )
  ; ( "--pp-args"
    , Arg.String (fun s -> add_pp_args (parse_args s))
    , "args Forwart the given arguments to the preprocessor." )
  ]

let usage =
  Printf.sprintf "Usage: %s [clean | drivers | config | [options] [files]]"

let _ =
  match Sys.argv with
  | [| _ ; "clean"   |] -> do_clean := true
  | [| _ ; "drivers" |] -> let f = Printf.printf "%s\n" in
                           List.iter f patoconfig.drivers
  | [| _ ; "config"  |] -> print_config stdout
  | _                   -> Arg.parse spec add_file (usage Sys.argv.(0))

(* The data after parsing the command-line arguments. *)
let bin_args   = !bin_args
let opt_args   = !opt_args
let pp_args    = !pp_args
let build_dir  = !build_dir
let local_path = !local_path
let packages   = !packages
let pat_format = !pat_format
let pat_driver = !pat_driver
let j          = !j
let verbose    = !verbose
let do_clean   = !do_clean
let files      = !files

(* Cleaning if required. *)
let _ =
  if do_clean then
    match build_dir with
    | None   ->
        Printf.eprintf "Cannot clean (no build directory is set)\n%!";
        exit 1
    | Some d when Sys.file_exists d ->
        if verbose > 0 then Printf.printf "Removing directory %S\n." d;
        ignore (Sys.command ("rm -rf " ^ d))
    | Some d ->
        if verbose > 0 then Printf.printf "Nothing to clean.\n"

let source_files path =
  let files = ref [] in
  let read_dir d =
    if Sys.file_exists d then
    let fs = Sys.readdir d in
    let add_file fn =
      if not (Sys.is_directory fn) then
      if List.exists (Filename.check_suffix fn) [".ml"; ".mli"; ".txp"] then
      files := (if d = "." then fn else Filename.concat d fn) :: !files
    in Array.iter add_file fs
  in
  List.iter read_dir path; !files

let decompose_filename : string -> string * string * string = fun fn ->
  let dir  = Filename.dirname  fn in
  let base = Filename.basename fn in
  let name = Filename.chop_extension base in 
  let base_len = String.length base in
  let name_len = String.length name in
  let ext =
    if base_len = name_len then ""
    else String.sub base name_len (base_len - name_len)
  in
  (dir, name, ext)

let more_recent source target =
   Unix.((stat source).st_mtime > (stat target).st_mtime)

(* Preprocessor command. *)
let m_stdout = Mutex.create ()
let pp_if_more_recent is_main source target =
  (* Update if source more recent that target. *)
  let update = not (Sys.file_exists target) || more_recent source target in
  (* Also update if main file does not exist (only when processing main). *)
  let main = (Filename.chop_extension target) ^ "_.ml" in
  if update || (is_main && not (Sys.file_exists main)) then
  let pp_args =
    match pat_driver with
    | None   -> pp_args
    | Some d -> "--driver" :: d :: pp_args
  in
  let pp_args =
    match pat_format with
    | None   -> pp_args
    | Some f -> "--format" :: f :: pp_args
  in
  let pp_args = if is_main then "--main" :: pp_args else pp_args in
  let cmd =
    String.concat " " ("pa_patoline" :: pp_args @ [source ; ">" ; target])
  in
  Mutex.lock m_stdout;
  Printf.printf "[PAT] %s\n%!" cmd;
  Mutex.unlock m_stdout;
  if Sys.command cmd <> 0 then failwith "Preprocessor error..."

(* Computing dependencies *)
let run_dep build_dir target =
  let cmd =
    Printf.sprintf "cd %s && ocamldep *.ml *.mli > %s" build_dir target
  in
  Printf.printf "[DEP] %s\n%!" cmd;
  if Sys.command cmd <> 0 then failwith "OCamldep error..."

(* Parsing dependencies. *)
let read_dep build_dir dep_file =
  let open Decap in
  let file_re = "[a-zA-Z][a-zA-Z0-9_]*[.]cm[xoi]" in
  let file = parser f:RE(file_re) -> Filename.concat build_dir f in
  let line = parser t:file " :" ds:{' ' _:"\\\n    "? d:file}* '\n' in
  let deps = parser line* in
  let parse_deps fn =
    try handle_exception (parse_file deps no_blank) fn
    with _ ->
      Printf.eprintf "Problem while parsing dependency file %S." fn;
      exit 1
  in parse_deps dep_file

let parallel_iter nb_threads f ls =
  let m = Mutex.create () in
  let bag = ref ls in
  let rec thread_fun () =
    Mutex.lock m;
    match !bag with
    | t::ts -> bag := ts; Mutex.unlock m; f t; thread_fun ()
    | []    -> Mutex.unlock m; Thread.exit ()
  in
  let ths = Array.init nb_threads (fun _ -> Thread.create thread_fun ()) in
  Array.iter Thread.join ths

(* Compilation function. *)
let compile_targets nb_threads build_dir all_deps targets =
  let files = ref (List.map (fun t -> (t, Mutex.create ())) targets) in
  let tasks = ref targets in
  let m_files = Mutex.create () in
  let m_tasks = Mutex.create () in

  let add_file fn =
    Mutex.lock m_files;
    files := (fn, Mutex.create ()) :: !files;
    Mutex.unlock m_files
  in

  let get_task : unit -> string = fun () ->
    Mutex.lock m_tasks;
    match !tasks with
    | t::ts -> tasks := ts; Mutex.unlock m_tasks; t
    (* FIXME We need to wait on another condition. *)
    | []    -> Mutex.unlock m_tasks; Thread.exit (); assert false
  in

  let add_tasks ts =
    Mutex.lock m_tasks;
    tasks := ts @ !tasks;
    Mutex.unlock m_tasks
  in

  let do_compile t =
    let base = Filename.chop_extension t in
    let source_ext =
      if Filename.check_suffix t ".cmi" then ".mli" else ".ml"
    in
    let source = base ^ source_ext in
    if not (Sys.file_exists t) || more_recent source t then
      begin
        let packs = String.concat "," packages in
        let args =
          ["-package"; packs; "-I"; build_dir; "-c"] @ opt_args @
          ["-o"; t; source]
        in
        let cmd = "ocamlfind ocamlopt " ^ (String.concat " " args) in
        Mutex.lock m_stdout;
        Printf.printf "[OPT] %s\n%!" cmd;
        Mutex.unlock m_stdout;
        if Sys.command cmd <> 0 then failwith "Compilation error..."
      end
  in

  let rec thread_fun () =
    let t = get_task () in
    let deps = try List.assoc t all_deps with Not_found -> assert false in
    (* Quick filter (unreliable). *)
    let is_done f =
      try
        let m = List.assoc f !files in
        if Mutex.try_lock m then (Mutex.unlock m; true) else false
      with Not_found -> false
    in
    let deps = List.filter (fun f -> not (is_done f)) deps in
    (* Obtain a task. *)
    begin
      if deps = [] then
        begin
          do_compile t;
          add_file t
        end
      else
        begin
          Mutex.lock m_files;
          let is_done f =
            try List.mem_assoc f !files
            with Not_found -> false
          in
          let deps = List.filter (fun f -> not (is_done f)) deps in
          add_tasks (deps @ [t]);
          Mutex.unlock m_files;
        end
    end;
    (* Continue to work. *)
    thread_fun ()
  in
  let ths = Array.init nb_threads (fun _ -> Thread.create thread_fun ()) in
  Array.iter Thread.join ths

(* Compilation of the files. *)
let _ =
  if files = [] then
    begin
      if verbose > 1 then Printf.eprintf "Nothing to do.\n%!";
      exit 0
    end;
  (* Check for builddir. *)
  let build_dir = match build_dir with None -> "." | Some d -> d in
  if not (Sys.file_exists build_dir) then Unix.mkdir build_dir 0o700;
  (* Updating sources. *)
  let update_file fn =
    let (_, base, ext) = decompose_filename fn in
    let target_ext = match ext with ".txp" -> ".ml" | e -> e in
    let target = Filename.concat build_dir (base ^ target_ext) in
    let is_main = ext = ".txp" && List.mem fn files in
    pp_if_more_recent is_main fn target
  in
  parallel_iter j update_file (source_files ("." :: local_path));
  (* Computing dependencies. *)
  run_dep build_dir ".depend";
  let deps = read_dep build_dir (Filename.concat build_dir ".depend") in
  let is_cmx_or_cmi f =
    Filename.check_suffix f ".cmx" || Filename.check_suffix f ".cmi"
  in
  let deps = List.filter (fun (s,_) -> is_cmx_or_cmi s) deps in
  (* Actually compiling. *)
  let to_target fn =
    let (_, base, ext) = decompose_filename fn in
    let target_ext = if ext = ".txp" then "_.cmx" else ".cmx" in
    Filename.concat build_dir (base ^ target_ext)
  in
  let targets = List.map to_target files in
  compile_targets j build_dir deps targets
