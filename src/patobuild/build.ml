open Parallel
open PatConfig

(* Verbosity level (0 is none). *)
let verbose = ref 2

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
  ; pat_driver : string option
  ; run_binary : bool
  } (* Patoline driver. *)

(* Run a command. The first argument is a short command name like "OPT", the
   second argument is the file concerned by the command. The return value is
   true if everything went well, false otherwise. *)
let run_command : string -> string -> string -> bool = fun n fn cmd ->
  if !verbose > 0 then
    begin
      let pad = String.make (max 0 (3 - (String.length n))) ' ' in
      if !verbose = 1 then printf "[%s] %s%s\n%!" n pad fn
      else printf "[%s] %s%s\n%!" n pad cmd
    end;
  Sys.command cmd = 0

(* Same as run_command, but exits the program in case of failure. *)
let command : string -> string -> string -> unit = fun n fn cmd ->
  if not (run_command n fn cmd) then
    begin
      eprintf "Command failure...\n%!";
      exit 1
    end

(* Transform a directory into the corresponding build directory. *)
let to_build_dir d =
  if d = "." then build_dir else Filename.concat d build_dir

(* Remove the build directory in the given directory, if it exists. *)
let clean_build_dirs config =
  let clean_build_dir path =
    let d = to_build_dir path in
    if Sys.file_exists d then
      begin
        if !verbose > 2 then printf "Removing directory %S\n" d;
        command "RM" d ("rm -rf " ^ d)
      end
    else if !verbose > 2 then
      printf "Directory %S does not exist\n" d
  in
  iter clean_build_dir config.path

(* Test if a file is more recent than another file. *)
let more_recent source target =
  not (Sys.file_exists target) ||
  Unix.((stat source).st_mtime > (stat target).st_mtime)

let add_pp_args config args =
  { config with pp_args = args @ config.pp_args }

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
  if not (run_command "PPP" source cmd) then
    eprintf "\027[93mFailed to parse file %S...\027[39m\n%!" source

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
  let build_dirs = List.map to_build_dir path in
  let includes = List.map (fun n -> "-I " ^ n) build_dirs in
  let includes = String.concat " " includes in
  let patterns =
    List.map (fun n -> Printf.sprintf "%s/*.ml %s/*.mli" n n) build_dirs
  in
  let patterns = String.concat " " patterns in
  let cmd =
    Printf.sprintf "ocamldep -one-line %s %s > %s" includes patterns target
  in command "DEP" target cmd

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

(* Produce the ocamlopt command using the configuration. *)
let opt_command config =
  let packs = String.concat "," config.packages in
  let includes = List.map (fun d -> "-I " ^ (to_build_dir d)) config.path in
  let includes = String.concat " " includes in
  Printf.sprintf "ocamlfind ocamlopt -package %s %s " packs includes

(* Compilation function. *)
let compile_targets config deps targets =
  (* Stack of tasks, and its mutex. *)
  let tasks = ref targets in
  let m_tasks = Mutex.create () in

  (* Function used to obtain a new task (i.e. a new file to produce). *)
  let get_task : unit -> string = fun () ->
    Mutex.lock m_tasks;
    match !tasks with
    | t::ts -> tasks := ts; Mutex.unlock m_tasks; t
    | []    -> Mutex.unlock m_tasks; Thread.exit (); assert false
  in

  (* Function used to add a list of tasks on top of the task stack. *)
  let add_tasks ts =
    Mutex.lock m_tasks;
    tasks := ts @ !tasks;
    Mutex.unlock m_tasks
  in

  (* The raw compilation command. *)
  let optcmd = opt_command config in
  let do_compile deps t =
    let base = Filename.chop_extension t in
    let source_ext =
      if Filename.check_suffix t ".cmi" then ".mli" else ".ml"
    in
    let source = base ^ source_ext in
    let dep_changed = List.exists (fun d -> more_recent d t) deps in
    if dep_changed || more_recent source t then
      begin
        let args = ["-c"] @ config.opt_args @ ["-o"; t; source] in
        let cmd = optcmd ^ (String.concat " " args) in
        command "OPT" source cmd
      end
  in

  (* A map of mutex indexed by files. If a file is mapped, it means that it
     it being taken care of by some thread. The thread keeps the lock on the
     mutex while it is working. To check if a file has been generated, one
     simply locks-unlocks the mutex. *)
  let files = ref [] in
  let m_files = Mutex.create () in

  (* Quickly (but unreliably) check if a file has been generated. If it
     returns true, then we know the file has been produced. *)
  let unsafe_exists fn =
    try
      let m = List.assoc fn !files in
      if Mutex.try_lock m then (Mutex.unlock m; true) else false
    with Not_found -> false
  in

  (* Find out if a file has been generated (or is being generated), and if
     it is the case returns None. Otherwise, insert the file with a fresh
     locked mutex, and returns the mutex. *)
  let lock_file fn =
    if List.mem_assoc fn !files then None else
    begin
      Mutex.lock m_files;
      let res =
        if List.mem_assoc fn !files then None else
        begin
          let m = Mutex.create () in
          Mutex.lock m;
          files := (fn, m) :: !files;
          Some m
        end
      in
      Mutex.unlock m_files; res
    end
  in

  let rec thread_fun i =
    (* Obtain a new task, and compute its dependencies. *)
    let t = get_task () in
    if !verbose > 2 then eprintf "[%2i] got task %S\n%!" i t;
    let all_deps = try List.assoc t deps with Not_found -> assert false in
    (* Quick (unreliable) filter on dependencies. *)
    let deps = List.filter (fun f -> not (unsafe_exists f)) all_deps in
    (* Actually do some work. *)
    begin
      if deps = [] then
        begin
          match lock_file t with
          | None   -> () (* Already being created. *)
          | Some m -> do_compile all_deps t; Mutex.unlock m
        end
      else
        begin
          add_tasks (deps @ [t]);
          (* Wait for up to 250ms *)
          ignore (Unix.select [] [] [] (Random.float 0.25))
        end
    end;
    (* Continue to work. *)
    thread_fun i
  in
  let fn i = Thread.create thread_fun i in
  let ths = Array.init !Parallel.nb_threads fn in
  Array.iter Thread.join ths

let rec get_cmxs deps cmx =
  let is_cmx f = Filename.check_suffix f ".cmx" in
  let ds = List.filter is_cmx (List.assoc cmx deps) in
  let cmxs = List.concat (List.map (get_cmxs deps) ds) @ [cmx] in
  let fn acc f = if List.mem f acc then acc else f::acc in
  List.rev (List.fold_left fn [] cmxs)

(* Produce the binary of the document. *)
let produce_binary config deps main =
  let target = (Filename.chop_extension main) ^ ".opt" in
  let cmxs = get_cmxs deps main in
  let args = ["-o"; target; "-linkpkg"] @ cmxs in
  let optcmd = opt_command config in
  let cmd = optcmd ^ (String.concat " " args) in
  command "LNK" main cmd

(* Run a document binary. *)
let run_binary config fn =
  let args = config.bin_args in
  let args = if !verbose < 2 then "--quiet" :: args else args in
  let cmd = String.concat " " (fn :: args) in
  command "RUN" fn cmd

(* Build the configuration according to pragmas. *)
let extend_config config ls =
  let set_format config f =
    if config.pat_format <> None then config else
    let packages =
      if List.mem f patoconfig.formats then
        ("Typography." ^ f) :: config.packages
      else config.packages
    in
    { config with packages; pat_format = Some f }
  in
  let set_driver config d =
    if config.pat_driver <> None then config else
    let packages =
      if List.mem d patoconfig.drivers then
        ("Typography." ^ d) :: config.packages
      else config.packages
    in
    { config with packages; pat_driver = Some d }
  in
  let combine config (k,vo) =
    match (k, vo) with
    | ("FORMAT", Some f) -> set_format config f
    | ("DRIVER", Some d) -> set_driver config d
    | ("FORMAT", None  ) -> eprintf "Pragma FORMAT needs an argument.\n%!";
                            config
    | ("DRIVER", None  ) -> eprintf "Pragma FORMAT needs an argument.\n%!";
                            config
    | _                  -> eprintf "Unknown pragma %s\n%!" k;
                            config
  in
  List.fold_left combine config ls

(* Main compilation function. *)
let compile config file =
  (* Reading configurations in the file. *)
  let pragmas = Pragma.pragma_from_file file in
  let config = extend_config config pragmas in
  (* Finding all the source files. *)
  let sources = source_files config.path in
  (* Making sure the build directories exist. *)
  let create_build_dir dir =
    if Sys.file_exists dir && Sys.is_directory dir then
    let bdir = Filename.concat dir build_dir in
    if not (Sys.file_exists bdir) then
    Unix.mkdir bdir 0o700
  in
  iter create_build_dir config.path;
  (* Updating the source files that have changed in the build directories. *)
  let update_file fn =
    let (dir, base, ext) = decompose_filename fn in
    let bdir = Filename.concat dir build_dir in
    let target_ext = match ext with ".txp" -> ".ml" | e -> e in
    let target = Filename.concat bdir (base ^ target_ext) in
    let is_main = ext = ".txp" && fn = file in
    pp_if_more_recent config is_main fn target
  in
  iter update_file sources;
  (* Computing dependencies. *)
  let depfile = Filename.concat build_dir "depend" in
  run_deps config.path depfile;
  let deps = read_deps depfile in
  (* Building the primary target. *)
  let to_target fn =
    let (dir, base, ext) = decompose_filename fn in
    let target_ext = if ext = ".txp" then "_.cmx" else ".cmx" in
    let bdir =
      if dir = "." then build_dir else Filename.concat dir build_dir
    in
    Filename.concat bdir (base ^ target_ext)
  in
  let target = to_target file in
  (* Actually compiling. *)
  compile_targets config deps [target];
  produce_binary config deps target;
  (* Producing the document. *)
  if config.run_binary then
    begin
      let to_bin fn =
        let (dir, base, ext) = decompose_filename fn in
        let bdir = to_build_dir dir in
        let target_ext = match ext with ".txp" -> "_.opt" | e -> ".opt" in
        Filename.concat bdir (base ^ target_ext)
      in
      run_binary config (to_bin file)
    end
