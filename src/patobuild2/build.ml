open Parallel

(* Verbosity level (0 is none). *)
let verbose = ref 0

(* Build directory. *)
let build_dir = ".patobuild"

(* Decompose a filename into directory, basename and extension. *)
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

(* Type of a compilation configuration. *)
type config =
  { bin_args   : string list     (* Arguments to be fed to the executable. *)
  ; opt_args   : string list     (* Arguments to be fed to ocamlopt. *)
  ; pp_args    : string list     (* Arguments to be fed to pa_patoline. *)
  ; packages   : string list     (* Packages (ocamlfind) to include. *)
  ; path       : string list     (* Source directories. *)
  ; pat_format : string option   (* Patoline format. *)
  ; pat_driver : string option } (* Patoline driver. *)

(* Run a command. The first argument is a short command name like "OPT". *)
let command : string -> string -> unit = fun n cmd ->
  if !verbose > 0 then
    begin
      let pad = String.make (max 0 (3 - (String.length n))) ' ' in
      printf "[%s] %s%s\n%!" n pad cmd
    end;
  if Sys.command cmd <> 0 then
    begin
      eprintf "Command failure...\n%!";
      exit 1
    end

(* Remove the build directory in the given directory, if it exists. *)
let clean_build_dirs config =
  let clean_build_dir path =
    let d = Filename.concat path build_dir in
    if Sys.file_exists d then
      begin
        if !verbose > 1 then printf "Removing directory %S\n" d;
        command "RM" ("rm -rf " ^ d)
      end
    else if !verbose > 1 then
      printf "Directory %S does not exist\n" d
  in
  iter clean_build_dir config.path

(* Test if a file is more recent than another file. *)
let more_recent source target =
  not (Sys.file_exists target) ||
  Unix.((stat source).st_mtime > (stat target).st_mtime)

(* Preprocessor command. *)
let pp_if_more_recent config is_main source target =
  (* Update if source more recent that target. *)
  let update = more_recent source target in
  (* Also update if main file does not exist (only when processing main). *)
  let main = (Filename.chop_extension target) ^ "_.ml" in
  if update || (is_main && not (Sys.file_exists main)) then
  let pp_args =
    match config.pat_driver with
    | None   -> config.pp_args
    | Some d -> "--driver" :: d :: config.pp_args
  in
  let pp_args =
    match config.pat_format with
    | None   -> pp_args
    | Some f -> "--format" :: f :: pp_args
  in
  let pp_args = if is_main then "--main" :: pp_args else pp_args in
  let pp_args = "--build-dir" :: build_dir :: pp_args in
  let cmd =
    String.concat " " ("pa_patoline" :: pp_args @ [source ; ">" ; target])
  in
  command "PP" cmd

(* Compute the list of all the source files in the path. *)
let source_files path =
  let files = ref [] in
  let read_dir d =
    if Sys.file_exists d then
    let fs = Sys.readdir d in
    let add_file fn =
      let fn = if d = "." then fn else Filename.concat d fn in
      if not (Sys.is_directory fn) then
      if List.exists (Filename.check_suffix fn) [".ml"; ".mli"; ".txp"] then
      files := fn :: !files
    in Array.iter add_file fs;
  in
  List.iter read_dir path; !files

(* Computing dependencies *)
let run_deps path target =
  let to_build_dir d =
    if d = "." then build_dir else Filename.concat d build_dir
  in
  let build_dirs = List.map to_build_dir path in
  let includes = List.map (fun n -> "-I " ^ n) build_dirs in
  let includes = String.concat " " includes in
  let patterns =
    List.map (fun n -> Printf.sprintf "%s/*.ml %s/*.mli" n n) build_dirs
  in
  let patterns = String.concat " " patterns in
  let cmd =
    Printf.sprintf "ocamldep -one-line %s %s > %s" includes patterns target
  in command "DEP" cmd

(* Parsing dependencies. *)
let read_deps dep_file =
  let open Decap in
  let file = parser f:''[^ \n]+'' in
  let line = parser t:file " :" ds:{' ' d:file}* '\n' in
  let deps = parser line* in
  let parse_deps fn =
    try handle_exception (parse_file deps no_blank) fn
    with _ ->
      Printf.eprintf "Problem while parsing dependency file %S." fn;
      exit 1
  in
  let deps = parse_deps dep_file in
  (* Hacks due to ocamldep limitations. *)
  let is_native f = List.exists (Filename.check_suffix f) [".cmx"; ".cmi"] in
  let deps = List.filter (fun (s,_) -> is_native s) deps in
  let to_build_dir fn =
    if Filename.basename fn = fn then Filename.concat build_dir fn else fn
  in
  List.map (fun (n,ds) -> (n, List.map to_build_dir ds)) deps


(*
(* Compilation function. *)
let compile_targets config targets =
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
    if more_recent source t then
      begin
        let packs = String.concat "," config.packages in
        let args =
          ["-package"; packs; "-I"; build_dir; "-c"] @ config.opt_args @
          ["-o"; t; source]
        in
        let cmd = "ocamlfind ocamlopt " ^ (String.concat " " args) in
        command "OPT" cmd
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
  let fn _ = Thread.create thread_fun () in
  let ths = Array.init !Parallel.nb_threads fn in
  Array.iter Thread.join ths

*)

(* Main compilation function. *)
let compile config files =
  if files = [] then
    begin
      if !verbose > 1 then eprintf "Nothing to do.\n%!";
      exit 0
    end;
  (* Finding all the source files. *)
  let sources = source_files config.path in
  (* Updating the source files that have changed in the build directories. *)
  let update_file fn =
    let (dir, base, ext) = decompose_filename fn in
    let bdir = Filename.concat dir build_dir in
    if not (Sys.file_exists bdir) then Unix.mkdir bdir 0o700;
    let target_ext = match ext with ".txp" -> ".ml" | e -> e in
    let target = Filename.concat bdir (base ^ target_ext) in
    let is_main = ext = ".txp" && List.mem fn files in
    pp_if_more_recent config is_main fn target
  in
  iter update_file sources;
  (* Computing dependencies. *)
  if not (Sys.file_exists build_dir) then Unix.mkdir build_dir 0o700;
  let depfile = Filename.concat build_dir "depend" in
  run_deps config.path depfile;
  let deps = read_deps depfile in
  (* Actually compiling. *)
  ignore deps


(*
  (* Actually compiling. *)
  let to_target fn =
    let (_, base, ext) = decompose_filename fn in
    let target_ext = if ext = ".txp" then ".tmx" else ".opt" in
    Filename.concat build_dir (base ^ target_ext)
  in
  let targets = List.map to_target files in
  compile_targets config targets
*)
