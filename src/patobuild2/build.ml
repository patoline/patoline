open Parallel

(* Verbosity level (0 is none). *)
let verbose = ref 0

(* Build directory. *)
let build_dir = ".patobuild"

(* Type of a compilation configuration. *)
type config =
  { bin_args   : string list     (* Arguments to be fed to the tmx. *)
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
let pp_if_more_recent config is_main source target =
  (* Update if source more recent that target. *)
  let update = not (Sys.file_exists target) || more_recent source target in
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
  printf "[PAT] %s\n%!" cmd;
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

(* Compilation function. *)
let compile_targets config all_deps targets =
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
    if not (Sys.file_exists t) || more_recent source t then
      begin
        let packs = String.concat "," config.packages in
        let args =
          ["-package"; packs; "-I"; build_dir; "-c"] @ config.opt_args @
          ["-o"; t; source]
        in
        let cmd = "ocamlfind ocamlopt " ^ (String.concat " " args) in
        printf "[OPT] %s\n%!" cmd;
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
  let fn _ = Thread.create thread_fun () in
  let ths = Array.init !Parallel.nb_threads fn in
  Array.iter Thread.join ths

(* Main compilation function. *)
let compile config files =
  if files = [] then
    begin
      if !verbose > 1 then eprintf "Nothing to do.\n%!";
      exit 0
    end;
  (* Check for builddir. *)
  if not (Sys.file_exists build_dir) then Unix.mkdir build_dir 0o700;
  (* Updating sources. *)
  let update_file fn =
    let (_, base, ext) = decompose_filename fn in
    let target_ext = match ext with ".txp" -> ".ml" | e -> e in
    let target = Filename.concat build_dir (base ^ target_ext) in
    let is_main = ext = ".txp" && List.mem fn files in
    pp_if_more_recent config is_main fn target
  in
  iter update_file (source_files ("." :: config.path));
  (* Computing dependencies. *)
  run_dep build_dir "depend";
  let deps = read_dep build_dir (Filename.concat build_dir "depend") in
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
  compile_targets config deps targets
