open Parallel
open PatConfig

(* Some useful functions for manipulating files. *)
module Filename =
  struct
    include Filename

    (* Decompose a filename into directory, basename and extension. *)
    let decompose : string -> string * string * string = fun fn ->
      let dir  = dirname  fn in
      let base = basename fn in
      let name = chop_extension base in
      let base_len = String.length base in
      let name_len = String.length name in
      let ext =
        if base_len = name_len then ""
        else String.sub base name_len (base_len - name_len)
      in
      (dir, name, ext)

    (* Set the extension of the given file. *)
    let set_extension : string -> string -> string = fun fn ext ->
      chop_extension fn ^ ext

    (* Ensure that concat ignores the "." directory. *)
    let concat : string -> string -> string = fun dir fn ->
      if dir = "." then fn else concat dir fn
  end

(* Time representation as a float. *)
module Time =
  struct
    type time = float

    (* Obtain the modification time of a file, minus infinity being used in the
       case where the file does not exist. *)
    let mod_time : string -> time = fun fname ->
      if Sys.file_exists fname then Unix.((stat fname).st_mtime)
      else neg_infinity

    (* Modification time of the current binary. *)
    let binary_time : float = mod_time "/proc/self/exe"

    (* Test if a file is more recent than another file (or the binary). *)
    let more_recent : string -> string -> bool = fun source target ->
      mod_time source > mod_time target || binary_time > mod_time target
  end
open Time

(* Verbosity level (0 is none). *)
let verbose = ref 2

(* Transform a directory into the corresponding build directory. *)
let to_build_dir dir =
  Filename.concat dir ".patobuild"

(* Put a filename in the corrsponding build directory (checking first that
   it is not already in a build directory). *)
let file_to_build_dir fn =
  let dir  = Filename.dirname fn in
  if Filename.basename dir = ".patobuild" then fn else
  let base = Filename.basename fn in
  Filename.concat (to_build_dir dir) base

(* Type of a compilation configuration. *)
type config =
  { bin_args   : string list   (* Arguments to be fed to the executable. *)
  ; opt_args   : string list   (* Arguments to be fed to ocamlopt. *)
  ; pp_args    : string list   (* Arguments to be fed to pa_patoline. *)
  ; packages   : string list   (* Packages (ocamlfind) to include. *)
  ; path       : string list   (* Source directories. *)
  ; pat_format : string option (* Patoline format. *)
  ; pat_driver : string option (* Patoline driver. *)
  ; run_binary : bool          (* Run the binary only if true. *) }

(* Run a command. The first argument is a 3 character command name (e.g.,
   "OPT"), the second argument is the file concerned by the command. Errors
   are handled by given an error message in case of failure. *)
let command : ?cleanup:(unit -> unit) -> string -> string -> string -> unit =
  fun ?(cleanup=fun _ -> ()) n fn cmd ->
  if !verbose > 0 then
    begin
      if !verbose = 1 then printf "[%s] %s\n%!" n fn
      else printf "[%s] %s\n%!" n cmd
    end;
  if Sys.command cmd <> 0 then
    begin
      cleanup ();
      eprintf "\027[31m%s failed on %S...\027[39m\n%!" n fn;
      exit 1
    end

(* Remove the build directory in the given directory, if it exists. *)
let clean_build_dirs config =
  let clean_build_dir path =
    let d = to_build_dir path in
    if Sys.file_exists d then
      begin
        if !verbose > 2 then printf "Removing directory %S\n" d;
        command "RMV" d ("rm -rf " ^ d)
      end
    else if !verbose > 2 then
      printf "Directory %S does not exist\n" d
  in
  iter clean_build_dir config.path

let add_pp_args config args =
  { config with pp_args = args @ config.pp_args }

let driver_changed config target =
  let new_driver = match config.pat_driver with
    | None -> "Pdf" | Some d -> d
  in
  let driver_file = Filename.set_extension target "_.driver" in
  let record_driver res =
    if res then
      begin
        let ch = open_out driver_file in
        Printf.fprintf ch "%s\n" new_driver;
        close_out ch
      end;
    res
  in
  try
    let ch = open_in driver_file in
    let driver = input_line ch in
    close_in ch;
    record_driver (driver <> new_driver)
  with _ -> record_driver true

(* Compute the dependences for the given file. *)
let file_deps : config -> string -> string list =
  let includes = Hashtbl.create 7 in
  fun config fn ->
    assert (List.exists (Filename.check_suffix fn) [".ml"; ".mli"]);
    let includes =
      try Hashtbl.find includes config with Not_found ->
        let s = String.concat " " (List.map ((^) "-I ") config.path) in
        Hashtbl.add includes config s; s
    in
    let cmd = "ocamldep " ^ includes in
    let cmd = cmd ^ " -one-line -native -ml-synonym .txp " ^ fn in
    if !verbose > 0 then
      begin
        if !verbose = 1 then printf "[DEP] %s\n%!" fn
        else printf "[DEP] %s\n%!" cmd
      end;
    let ic = Unix.open_process_in cmd in
    let parse_dep =
      let file = parser f:''[^ \n]+'' in
      let dep = parser _:file " :" ds:{' ' d:file}* '\n' in
      Earley.parse_channel dep Earley.no_blank
    in
    let deps =
      try Earley.handle_exception parse_dep ic with _ ->
        Printf.eprintf "Problem while parsing dependency file %S.\n%!" fn;
        exit 1
    in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> List.map file_to_build_dir deps
    | _              ->
        Printf.eprintf "Problem while parsing dependency file %S.\n%!" fn;
        exit 1

(* Preprocessor command. *)
let pp_if_more_recent config is_main source target =
  (* Update if source more recent that target. *)
  let update = more_recent source target
               || (is_main && driver_changed config target)
  in
  (* Also update if main file does not exist (only when processing main). *)
  let main = Filename.set_extension target "_.ml" in
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
  let pp_args = "--build-dir" :: (Filename.dirname target) :: pp_args in
  let cmd =
    String.concat " " ("pa_patoline" :: pp_args @ [source ; ">" ; target])
  in
  let cleanup () = command "RMV" target ("rm -f " ^ target) in
  command ~cleanup "PPP" source cmd

(* Compute the list of all the source files in the path. *)
let source_files path =
  let files = ref [] in
  let read_dir d =
    if Sys.file_exists d then
    let fs = Sys.readdir d in
    let add_file fn =
      if fn.[0] <> '.' then
      let fn = if d = "." then fn else Filename.concat d fn in
      if not (Sys.is_directory fn) then
      if List.exists (Filename.check_suffix fn) [".ml"; ".mli"; ".txp"] then
      files := fn :: !files
    in Array.iter add_file fs;
  in
  List.iter read_dir path; !files

(* Produce the ocamlopt command using the configuration. *)
let opt_command config =
  let packs = String.concat "," config.packages in
  let includes = List.map (fun d -> "-I " ^ (to_build_dir d)) config.path in
  let includes = String.concat " " includes in
  Printf.sprintf "ocamlfind ocamlopt -package %s %s " packs includes

(* Compilation function. *)
let compile_targets config (deps : (string, string list) Hashtbl.t) target =
  (* Stack of tasks, and its mutex. *)
  let tasks = ref [target] in
  let stop  = ref false in
  let m_tasks = Mutex.create () in
  let m_deps = Mutex.create () in

  (* Function used to obtain a new task (i.e. a new file to produce). *)
  let get_task : unit -> string option = fun () ->
    if !stop then Thread.exit ();
    Mutex.lock m_tasks;
    match !tasks with
    | t::ts -> tasks := ts; Mutex.unlock m_tasks; Some(t)
    | []    -> Mutex.unlock m_tasks; None
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
    if !verbose > 3 then eprintf "[%2i] trying to get task (%i)\n%!" i (List.length !tasks);
    let t = get_task () in
    match t with
    | None    -> ignore (Unix.select [] [] [] (Random.float 0.25));
                 thread_fun i
    | Some(t) ->

    if !verbose > 2 then
      eprintf "[%2i] got task %S\n%!" i t;
    if !verbose > 3 then
      eprintf "[%2i] remaining tasks: %S\n%!" i (String.concat ", " !tasks);
    let all_deps =
      try Hashtbl.find deps t with Not_found ->
        if !verbose > 2 then eprintf "%S not in deps...\n%!" t;
        let ml =
          if Filename.check_suffix t ".cmx" then
            Filename.set_extension t ".ml"
          else if Filename.check_suffix t ".cmi" then
            Filename.set_extension t ".mli"
          else
            assert false
        in
        let fs = file_deps config ml in
        Mutex.lock m_deps; Hashtbl.add deps t fs; Mutex.unlock m_deps; fs
    in
    (* Quick (unreliable) filter on dependencies. *)
    let deps = List.filter (fun f -> not (unsafe_exists f)) all_deps in
    (* Actually do some work. *)
    begin
      if deps = [] then
        begin
          if t = target then stop := true;
          match lock_file t with
          | None   -> () (* Already being created. *)
          | Some m ->
              if !verbose > 2 then eprintf "[%2i] working on %S\n%!" i t;
              do_compile all_deps t; Mutex.unlock m;
              if !verbose > 2 then eprintf "[%2i] done with %S\n%!" i t
        end
      else
        begin
          add_tasks (deps @ [t]);
          if !verbose > 2 then eprintf "[%2i] pushed tasks for %S\n%!" i t;
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

(* Produce the binary of the document. *)
let produce_binary config deps main =
  let target = Filename.set_extension main ".opt" in
  let deps_ref = ref [] in
  Hashtbl.iter (fun k v -> deps_ref := (k,v) :: !deps_ref) deps;
  let deps = !deps_ref in
  let rec get_cmxs cmx =
    let is_cmx f = Filename.check_suffix f ".cmx" in
    let ds = List.filter is_cmx (List.assoc cmx deps) in
    let cmxs = List.concat (List.map get_cmxs ds) @ [cmx] in
    let fn acc f = if List.mem f acc then acc else f::acc in
    List.rev (List.fold_left fn [] cmxs)
  in
  let cmxs = get_cmxs main in
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
  let add_package config p =
    let packages = config.packages @ [p] in
    { config with packages; }
  in
  let combine config (k,vo) =
    match (k, vo) with
    | ("FORMAT" , Some f) -> set_format config f
    | ("DRIVER" , Some d) -> set_driver config d
    | ("FORMAT" , None  ) -> eprintf "Pragma FORMAT needs an argument.\n%!";
                             config
    | ("DRIVER" , None  ) -> eprintf "Pragma FORMAT needs an argument.\n%!";
                             config
    | ("PACKAGE", Some p) -> add_package config p
    | ("PACKAGE", None  ) -> eprintf "Pragma PACKAGE needs an argument.\n%!";
                             config
    | _                   -> eprintf "Unknown pragma %s\n%!" k;
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
    let bdir = to_build_dir dir in
    if not (Sys.file_exists bdir) then
    Unix.mkdir bdir 0o700
  in
  iter create_build_dir config.path;
  (* Updating the source files that have changed in the build directories. *)
  let update_file fn =
    let (dir, base, ext) = Filename.decompose fn in
    let bdir = to_build_dir dir in
    let target_ext = match ext with ".txp" -> ".ml" | e -> e in
    let target = Filename.concat bdir (base ^ target_ext) in
    let is_main = ext = ".txp" && fn = file in
    pp_if_more_recent config is_main fn target
  in
  iter update_file sources;
  (* Building the primary target. *)
  let to_target fn =
    let (dir, base, ext) = Filename.decompose fn in
    let target_ext = if ext = ".txp" then "_.cmx" else ".cmx" in
    let bdir = to_build_dir dir in
    Filename.concat bdir (base ^ target_ext)
  in
  let target = to_target file in
  (* Actually compiling. *)
  let deps = Hashtbl.create 37 in
  compile_targets config deps target;
  produce_binary config deps target;
  (* Producing the document. *)
  if config.run_binary then
    begin
      let to_bin fn =
        let (dir, base, ext) = Filename.decompose fn in
        let bdir = to_build_dir dir in
        let target_ext = match ext with ".txp" -> "_.opt" | e -> ".opt" in
        Filename.concat bdir (base ^ target_ext)
      in
      run_binary config (to_bin file)
    end
