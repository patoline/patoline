open Parallel
open Patconfig.PatConfig
open Patutil.Extra

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

let build_source fn =
  let (dir, base, ext) = Filename.decompose fn in
  let ext =
    match ext with
    | ".cmi" -> ".mli"
    | ".cmx" -> ".ml"
    | _      -> assert false
  in
  Filename.concat dir (base ^ ext)

let original_source fn =
  let (dir, base, ext) = Filename.decompose fn in
  let dir =
    let name = Filename.basename dir in
    let path = Filename.dirname  dir in
    if name = ".patobuild" then path else dir
  in
  match ext with
  | ".cmi" -> let mli = Filename.concat dir (base ^ ".mli") in
              if Sys.file_exists mli then mli else assert false
  | ".cmx" -> let ml  = Filename.concat dir (base ^ ".ml" ) in
              let txp = Filename.concat dir (base ^ ".txp") in
              if Sys.file_exists ml then ml else
              if Sys.file_exists txp then txp else
              let base_len = String.length base in
              if base.[base_len - 1] = '_' then
                let main_base = String.sub base 0 (base_len - 1) in
                let main = Filename.concat dir (main_base ^ ".txp") in
                if Sys.file_exists main then main else
                begin
                  eprintf "Cannot find file %S\n%!" main;
                  exit 1
                end
              else
                begin
                  eprintf "Cannot find file %S (or %S)\n%!" txp ml;
                  exit 1
                end
  | _      -> assert false

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
    end;
  if !verbose > 2 then printf "[%s] DONE (%s)\n%!" n fn

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
    let deps =
      let ic = Unix.open_process_in cmd in
      let line =
        try input_line ic with End_of_file ->
          eprintf "dependency analysis failed on %S:\n%!" fn;
          eprintf "  (unable to read the output line)\n%!";
          exit 1
      in
      let _ =
        try
          ignore (input_line ic);
          eprintf "dependency analysis failed on %S:\n%!" fn;
          eprintf "  (output contains multiple lines)\n%!";
          exit 1
        with End_of_file -> ()
      in
      if Unix.close_process_in ic <> Unix.WEXITED 0 then
        begin
          eprintf "dependency analysis failed on %S:\n%!" fn;
          eprintf "  (the process failed)\n%!";
          exit 1
        end;
      line
    in
    let open Earley_core in
    let parse_dep =
      let file = parser f:''[^ \n]+'' in
      let dep = parser _:file " :" ds:{' ' d:file}* in
      Earley.parse_string dep Earley.no_blank
    in
    let deps =
      try Earley.handle_exception parse_dep deps with _ ->
        eprintf "dependency analysis failed on %S:\n%!" fn;
        eprintf "  (parsing failed)\n%!";
        exit 1
    in
    List.map file_to_build_dir deps
    

(* Preprocessor command. *)
let run_pp ?(is_main=false) config source =
  let (dir, base, ext) = Filename.decompose source in
  (* Check that source is not already in the build directory... *)
  assert (Filename.basename dir <> ".patobuild");
  (* Obtain the build dir and target. *)
  let bdir = to_build_dir dir in
  let target =
    let ext = match ext with ".txp" -> ".ml" | _ -> ext in
    Filename.concat bdir (base ^ ext)
  in
  (* Create the build directory if necessary. *)
  if not (Sys.file_exists bdir) then
    begin
      eprintf "[DIR] %s\n%!" bdir;
      Unix.mkdir bdir 0o700
    end;
  (* Build the command. *)
  let pp_args = ["--build-dir"; bdir] @ config.pp_args in
  let pp_args =
    match config.pat_format with
    | None   -> pp_args
    | Some f -> "--format" :: f :: pp_args
  in
  let pp_args =
    match config.pat_driver with
    | None   -> pp_args
    | Some d -> "--driver" :: d :: pp_args
  in
  let pp_args = if not is_main then pp_args else "--main" :: pp_args in
  let args = String.concat " " pp_args in
  let cmd = Printf.sprintf "pa_patoline %s %s > %s" args source target in
  (* Run the command. *)
  let cleanup () = command "RMV" target ("rm -f " ^ target) in
  command ~cleanup "PPP" target cmd

(* Produce the ocamlopt command using the configuration. *)
let opt_command config =
  let packs = String.concat "," config.packages in
  let includes = List.map (fun d -> "-I " ^ (to_build_dir d)) config.path in
  let includes = String.concat " " includes in
  Printf.sprintf "ocamlfind ocamlopt -package %s %s " packs includes

(* Compilation function. *)
let compile_targets config deps target =
  (* Stack of tasks, and its mutex. *)
  let tasks   = TaskBag.create (=) in
  let m_deps  = Mutex.create () in

  (* Add initial task. *)
  TaskBag.post tasks [target];

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
  let filter_deps = List.filter (fun f -> not (unsafe_exists f)) in

  let not_handled fn =
    try ignore (List.assoc fn !files); false
    with Not_found -> true
  in
  let unhandled_files = List.filter not_handled in

  (* Set of tasks that are not ready (missing dependencies). *)
  let waiting = ref [] in
  let m_wait  = Mutex.create () in

  let update_waiting () =
    Mutex.lock m_wait;
    let new_waiting = ref [] in
    let fn (t, deps) =
      match filter_deps deps with
      | []   -> if !verbose > 2 then eprintf "%S is ready\n%!" t;
                TaskBag.post tasks [t]
      | deps -> new_waiting := (t,deps) :: !new_waiting
    in
    List.iter fn !waiting;
    waiting := !new_waiting;
    Mutex.unlock m_wait
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

  let is_ready fn =
    try
      let m = List.assoc fn !files in
      if Mutex.try_lock m then (Mutex.unlock m; true) else false
    with Not_found -> false
  in

  let done_signal = "<DONE>" in
  let stop_signal () =
    let l = Array.to_list (Array.make !Parallel.nb_threads done_signal) in
    TaskBag.raw_post tasks l
  in

  let get_source t =
    let src_build = build_source t in
    let src_orig  = original_source t in
    if more_recent src_orig src_build then run_pp config src_orig;
    src_build
  in

  let rec thread_fun i =
    (* Obtain a new task, and compute its dependencies. *)
    if !verbose > 2 then eprintf "[%3i] ready for a new task\n%!" i;
    if !verbose > 4 then
      begin
        let msg = String.concat ", " (TaskBag.get tasks) in
        eprintf "[BUG] Tasks: %s\n%!" msg
      end;
    let t = TaskBag.wait tasks in
    if !verbose > 2 then eprintf "[%3i] got task %S\n%!" i t;
    (* Exit if this is the dummy end task. *)
    if t = done_signal then Thread.exit ();
    (* Preprocess and compute dependencies if necessary. *)
    let source_ready =
      let src_build = build_source t in
      match lock_file src_build with
      | None   -> is_ready src_build
      | Some m ->
          if !verbose > 2 then eprintf "[%3i] working on %S\n%!" i src_build;
          let ml = get_source t in
          let fs = file_deps config ml in
          Mutex.lock m_deps; Hashtbl.add deps t fs; Mutex.unlock m_deps;
          Mutex.unlock m;
          if !verbose > 2 then eprintf "[%3i] done with %S\n%!" i src_build;
          true
    in
    (* Stop there if the source is not ready (handled by another thread). *)
    if not source_ready then thread_fun i else
    (* Get dependencies. *)
    let all_deps = Hashtbl.find deps t in
    (* Quick (unreliable) filter on dependencies. *)
    let deps = filter_deps all_deps in
    (* Actually do some work. *)
    begin
      if deps = [] then
        begin
          match lock_file t with
          | None   -> () (* Already being created. *)
          | Some m ->
              if t = target then stop_signal ();
              if !verbose > 2 then eprintf "[%3i] working on %S\n%!" i t;
              do_compile all_deps t; Mutex.unlock m;
              if !verbose > 2 then eprintf "[%3i] done with %S\n%!" i t;
              update_waiting ();
        end
      else
        begin
          if !verbose > 2 then eprintf "[%3i] %S is not ready\n%!" i t;
          Mutex.lock m_wait;
          let deps = filter_deps deps in
          waiting := (t, deps) :: !waiting;
          let not_handled = unhandled_files deps in
          TaskBag.post tasks not_handled;
          Mutex.unlock m_wait
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
        ("patoline.format." ^ f) :: config.packages
      else config.packages
    in
    { config with packages; pat_format = Some f }
  in
  let set_driver config d =
    if config.pat_driver <> None then config else
    let packages =
      if List.mem d patoconfig.drivers then
        ("patoline.driver." ^ d) :: config.packages
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
  (* Preprocessing the main file. *)
  run_pp ~is_main:true config file;
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
        let target_ext = match ext with ".txp" -> "_.opt" | _ -> ".opt" in
        Filename.concat bdir (base ^ target_ext)
      in
      run_binary config (to_bin file)
    end
