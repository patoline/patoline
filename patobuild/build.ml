open Parallel
open Patconfig.Config
open Patutil.Extra
module Time =
  struct
    type time = float
    let mod_time : string -> time =
      fun fname ->
        if Sys.file_exists fname
        then let open Unix in (stat fname).st_mtime
        else neg_infinity
    let binary_time : float = mod_time "/proc/self/exe"
    let more_recent : string -> string -> bool =
      fun source ->
        fun target ->
          ((mod_time source) > (mod_time target)) ||
            (binary_time > (mod_time target))
  end
open Time
let verbose = ref 2
let to_build_dir dir = Filename.concat dir ".patobuild"
let file_to_build_dir fn =
  let dir = Filename.dirname fn in
  if (Filename.basename dir) = ".patobuild"
  then fn
  else
    (let base = Filename.basename fn in
     Filename.concat (to_build_dir dir) base)
let build_source fn =
  let (dir, base, ext) = Filename.decompose fn in
  let ext =
    match ext with
    | ".cmi" -> ".mli"
    | ".cmx" -> ".ml"
    | _ -> assert false in
  Filename.concat dir (base ^ ext)
let original_source fn =
  let (dir, base, ext) = Filename.decompose fn in
  let dir =
    let name = Filename.basename dir in
    let path = Filename.dirname dir in
    if name = ".patobuild" then path else dir in
  match ext with
  | ".cmi" ->
      let mli = Filename.concat dir (base ^ ".mli") in
      if Sys.file_exists mli then mli else assert false
  | ".cmx" ->
      let ml = Filename.concat dir (base ^ ".ml") in
      let txp = Filename.concat dir (base ^ ".txp") in
      if Sys.file_exists ml
      then ml
      else
        if Sys.file_exists txp
        then txp
        else
          (let base_len = String.length base in
           if (base.[base_len - 1]) = '_'
           then
             let main_base = String.sub base 0 (base_len - 1) in
             let main = Filename.concat dir (main_base ^ ".txp") in
             (if Sys.file_exists main
              then main
              else (eprintf "Cannot find file %S\n%!" main; exit 1))
           else (eprintf "Cannot find file %S (or %S)\n%!" txp ml; exit 1))
  | _ -> assert false
type config =
  {
  bin_args: string list ;
  opt_args: string list ;
  pp_args: string list ;
  packages: string list ;
  path: string list ;
  pat_format: string option ;
  pat_driver: string option ;
  run_binary: bool }
let command : ?cleanup:(unit -> unit) -> string -> string -> string -> unit =
  fun ?(cleanup= fun _ -> ()) ->
    fun n ->
      fun fn ->
        fun cmd ->
          if (!verbose) > 0
          then
            (if (!verbose) = 1
             then printf "[%s] %s\n%!" n fn
             else printf "[%s] %s\n%!" n cmd);
          if (Sys.command cmd) <> 0
          then
            (cleanup ();
             eprintf "\027[31m%s failed on %S...\027[39m\n%!" n fn;
             exit 1);
          if (!verbose) > 2 then printf "[%s] DONE (%s)\n%!" n fn

(** P1-8: Hash-based incremental cache. The historical cache relied
    on file mtimes (see [Time.more_recent]), which meant any change
    to a directory (e.g. a touch on a parent) would invalidate the
    cache and force a full rebuild. Here we record a SHA-1 digest of
    each input file plus the binary timestamp and the build
    configuration. A target whose stored hash matches the current
    one can be skipped outright. *)
module Hash = struct
  let file fn =
    if not (Sys.file_exists fn) then
      Digest.string ("missing:" ^ fn)
    else
      Digest.file fn

  let string s = Digest.string s

  let list l =
    Digest.string (String.concat "\000" (List.sort String.compare l))
end

(** The on-disk cache lives at [\<build_dir\>/cache.bin]. We store a
    mapping from absolute target filename to the digest of its inputs
    plus the patobuild binary timestamp. *)
type cache_entry = string  (* hex digest *)

let cache_file build_dir =
  Filename.concat (to_build_dir build_dir) "cache.bin"

let read_cache build_dir : (string, cache_entry) Hashtbl.t =
  let tbl = Hashtbl.create 37 in
  let fn = cache_file build_dir in
  if Sys.file_exists fn then
    (try
       let ic = open_in_bin fn in
       let size = input_binary_int ic in
       for _ = 1 to size do
         let k = input_line ic in
         let v = input_line ic in
         Hashtbl.add tbl k v
       done;
       close_in ic
     with _ -> ());
  tbl

let write_cache build_dir (tbl : (string, cache_entry) Hashtbl.t) =
  let fn = cache_file build_dir in
  (try
     let ch = open_out_bin fn in
     output_binary_int ch (Hashtbl.length tbl);
     Hashtbl.iter (fun k v ->
         output_string ch (k ^ "\n");
         output_string ch (v ^ "\n")) tbl;
     close_out ch
   with _ ->
     eprintf "[CACHE] could not write %S\n%!" fn)

(** Decide whether [target] can be skipped. Returns [true] when the
    stored digest matches the current one AND the build binary has
    not changed since the cache entry was written. *)
let up_to_date cache_tbl target deps =
  let cur =
    Digest.to_hex (Hash.list
      (Printf.sprintf "%f" Time.binary_time ::
       List.map (fun d -> Hash.file d) deps))
  in
  match Hashtbl.find_opt cache_tbl target with
  | Some stored when stored = cur -> true
  | _ -> false

(** Mark [target] as up-to-date with the digest of its current
    inputs. *)
let mark_built cache_tbl target deps =
  let cur =
    Digest.to_hex (Hash.list
      (Printf.sprintf "%f" Time.binary_time ::
       List.map (fun d -> Hash.file d) deps))
  in
  Hashtbl.replace cache_tbl target cur

let clean_build_dirs config =
  let clean_build_dir path =
    let d = to_build_dir path in
    if Sys.file_exists d
    then
      (if (!verbose) > 2 then printf "Removing directory %S\n" d;
       command "RMV" d ("rm -rf " ^ d))
    else if (!verbose) > 2 then printf "Directory %S does not exist\n" d in
  iter clean_build_dir config.path
let add_pp_args config args =
  { config with pp_args = (args @ config.pp_args) }
let driver_changed config target =
  let new_driver = match config.pat_driver with | None -> "Pdf" | Some d -> d in
  let driver_file = Filename.set_extension target "_.driver" in
  let record_driver res =
    if res
    then
      (let ch = open_out driver_file in
       Printf.fprintf ch "%s\n" new_driver; close_out ch);
    res in
  try
    let ch = open_in driver_file in
    let driver = input_line ch in
    close_in ch; record_driver (driver <> new_driver)
  with | _ -> record_driver true
let file_deps : config -> string -> string list =
  let includes = Hashtbl.create 7 in
  fun config ->
    fun fn ->
      assert (List.exists (Filename.check_suffix fn) [".ml"; ".mli"]);
      (let includes =
         try Hashtbl.find includes config
         with
         | Not_found ->
             let s = String.concat " " (List.map ((^) "-I ") config.path) in
             (Hashtbl.add includes config s; s) in
       let cmd = "ocamldep " ^ includes in
       let cmd = cmd ^ (" -one-line -native -ml-synonym .txp " ^ fn) in
       if (!verbose) > 0
       then
         (if (!verbose) = 1
          then printf "[DEP] %s\n%!" fn
          else printf "[DEP] %s\n%!" cmd);
       (let deps =
          let ic = Unix.open_process_in cmd in
          let line =
            (* Windows ocamldep prints CRLF: [input_line] leaves the CR
               behind, which used to corrupt the final token
               (ext=".cmx\r") and hit the [assert false] in
               [build_source]. *)
            try String.trim (input_line ic)
            with
            | End_of_file ->
                (eprintf "dependency analysis failed on %S:\n%!" fn;
                 eprintf "  (unable to read the output line)\n%!";
                 exit 1) in
          let _ =
            try
              ignore (input_line ic);
              eprintf "dependency analysis failed on %S:\n%!" fn;
              eprintf "  (output contains multiple lines)\n%!";
              exit 1
            with | End_of_file -> () in
          if (Unix.close_process_in ic) <> (Unix.WEXITED 0)
          then
            (eprintf "dependency analysis failed on %S:\n%!" fn;
             eprintf "  (the process failed)\n%!";
             exit 1);
          line in
        let open Earley_core in
          let parse_dep =
            let file =
              Earley_str.regexp ~name:"[^ \\n]+" "[^ \n]+"
                (fun group -> group 0) in
            let dep =
              Earley_core.Earley.fsequence_ignore file
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string " :" " :")
                   (Earley_core.Earley.fsequence
                      (Earley_core.Earley.apply (fun f -> f [])
                         (Earley_core.Earley.fixpoint' (fun l -> l)
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.char ' ' ' ')
                               (Earley_core.Earley.fsequence file
                                  (Earley_core.Earley.empty (fun d -> d))))
                            (fun x -> fun f -> fun l -> f (List.cons x l))))
                      (Earley_core.Earley.empty (fun ds -> ds)))) in
            Earley.parse_string dep Earley.no_blank in
          let deps =
            try Earley.handle_exception parse_dep deps
            with
            | _ ->
                (eprintf "dependency analysis failed on %S:\n%!" fn;
                 eprintf "  (parsing failed)\n%!";
                 exit 1) in
          List.map file_to_build_dir deps))
let run_pp ?(is_main= false)  config source =
  let (dir, base, ext) = Filename.decompose source in
  assert ((Filename.basename dir) <> ".patobuild");
  (let bdir = to_build_dir dir in
   let target =
     let ext = match ext with | ".txp" -> ".ml" | _ -> ext in
     Filename.concat bdir (base ^ ext) in
   if not (Sys.file_exists bdir)
   then (eprintf "[DIR] %s\n%!" bdir; Unix.mkdir bdir 0o700);
   (let pp_args = ["--build-dir"; bdir] @ config.pp_args in
    let pp_args =
      match config.pat_format with
      | None -> pp_args
      | Some f -> "--format" :: f :: pp_args in
    let pp_args =
      match config.pat_driver with
      | None -> pp_args
      | Some d -> "--driver" :: d :: pp_args in
    let pp_args = if not is_main then pp_args else "--main" :: pp_args in
    let args = String.concat " " pp_args in
    let cmd = Printf.sprintf "pa_patoline %s %s > %s" args source target in
    let cleanup () = command "RMV" target ("rm -f " ^ target) in
    command ~cleanup "PPP" target cmd))
let opt_command config =
  let packs = String.concat "," config.packages in
  let includes = List.map (fun d -> "-I " ^ (to_build_dir d)) config.path in
  let includes = String.concat " " includes in
  Printf.sprintf "ocamlfind ocamlopt -package %s %s " packs includes
let compile_targets config deps target =
  let tasks = TaskBag.create (=) in
  let m_deps = Mutex.create () in
  TaskBag.post tasks [target];
  (let optcmd = opt_command config in
   (* The first path in [config.path] is where [.patobuild/] lives; the
      cache file [cache.bin] is stored there. *)
   let cache_tbl = read_cache (List.hd config.path) in
   let do_compile deps t =
     let base = Filename.chop_extension t in
     let source_ext =
       if Filename.check_suffix t ".cmi" then ".mli" else ".ml" in
     let source = base ^ source_ext in
     let needs_rebuild =
       not (up_to_date cache_tbl t deps) ||
       more_recent source t
     in
     if needs_rebuild
     then
       (let args = ["-c"] @ (config.opt_args @ ["-o"; t; source]) in
        let cmd = optcmd ^ (String.concat " " args) in
        command "OPT" source cmd);
     if Sys.file_exists t then mark_built cache_tbl t deps
   in
   let files = ref [] in
   let m_files = Mutex.create () in
   let unsafe_exists fn =
     try
       let m = List.assoc fn (!files) in
       if Mutex.try_lock m then (Mutex.unlock m; true) else false
     with | Not_found -> false in
   let filter_deps = List.filter (fun f -> not (unsafe_exists f)) in
   let not_handled fn =
     try ignore (List.assoc fn (!files)); false with | Not_found -> true in
   let unhandled_files = List.filter not_handled in
   let waiting = ref [] in
   let m_wait = Mutex.create () in
   let update_waiting () =
     Mutex.lock m_wait;
     (let new_waiting = ref [] in
      let fn (t, deps) =
        match filter_deps deps with
        | [] ->
            (if (!verbose) > 2 then eprintf "%S is ready\n%!" t;
             TaskBag.post tasks [t])
        | deps -> new_waiting := ((t, deps) :: (!new_waiting)) in
      List.iter fn (!waiting); waiting := (!new_waiting); Mutex.unlock m_wait) in
   let lock_file fn =
     if List.mem_assoc fn (!files)
     then None
     else
       (Mutex.lock m_files;
        (let res =
           if List.mem_assoc fn (!files)
           then None
           else
             (let m = Mutex.create () in
              Mutex.lock m; files := ((fn, m) :: (!files)); Some m) in
         Mutex.unlock m_files; res)) in
   let is_ready fn =
     try
       let m = List.assoc fn (!files) in
       if Mutex.try_lock m then (Mutex.unlock m; true) else false
     with | Not_found -> false in
   let done_signal = "<DONE>" in
   let stop_signal () =
     let l = Array.to_list (Array.make (!Parallel.nb_threads) done_signal) in
     TaskBag.raw_post tasks l in
   let get_source t =
     let src_build = build_source t in
     let src_orig = original_source t in
     if more_recent src_orig src_build then run_pp config src_orig; src_build in
   let rec thread_fun i =
     if (!verbose) > 2 then eprintf "[%3i] ready for a new task\n%!" i;
     if (!verbose) > 4
     then
       (let msg = String.concat ", " (TaskBag.get tasks) in
        eprintf "[BUG] Tasks: %s\n%!" msg);
     (let t = TaskBag.wait tasks in
      if (!verbose) > 2 then eprintf "[%3i] got task %S\n%!" i t;
      if t = done_signal then raise Thread.Exit;
      (let source_ready =
         let src_build = build_source t in
         match lock_file src_build with
         | None -> is_ready src_build
         | Some m ->
             (if (!verbose) > 2
              then eprintf "[%3i] working on %S\n%!" i src_build;
              (let ml = get_source t in
               let fs = file_deps config ml in
               Mutex.lock m_deps;
               Hashtbl.add deps t fs;
               Mutex.unlock m_deps;
               Mutex.unlock m;
               if (!verbose) > 2
               then eprintf "[%3i] done with %S\n%!" i src_build;
               true)) in
       if not source_ready
       then thread_fun i
       else
         (let all_deps = Hashtbl.find deps t in
          let deps = filter_deps all_deps in
          if deps = []
          then
            (match lock_file t with
             | None -> ()
             | Some m ->
                 (if t = target then stop_signal ();
                  if (!verbose) > 2
                  then eprintf "[%3i] working on %S\n%!" i t;
                  do_compile all_deps t;
                  Mutex.unlock m;
                  if (!verbose) > 2 then eprintf "[%3i] done with %S\n%!" i t;
                  update_waiting ()))
          else
            (if (!verbose) > 2 then eprintf "[%3i] %S is not ready\n%!" i t;
             Mutex.lock m_wait;
             (let deps = filter_deps deps in
              waiting := ((t, deps) :: (!waiting));
              (let not_handled = unhandled_files deps in
               TaskBag.post tasks not_handled; Mutex.unlock m_wait)));
          thread_fun i))) in
   let fn i = Thread.create thread_fun i in
   let ths = Array.init (!Parallel.nb_threads) fn in
   Array.iter Thread.join ths;
   (* Persist the cache for next invocation. *)
   write_cache (List.hd config.path) cache_tbl)
let produce_binary config deps main =
  let target = Filename.set_extension main ".opt" in
  let deps_ref = ref [] in
  Hashtbl.iter (fun k -> fun v -> deps_ref := ((k, v) :: (!deps_ref))) deps;
  (let deps = !deps_ref in
   let rec get_cmxs cmx =
     let is_cmx f = Filename.check_suffix f ".cmx" in
     let ds = List.filter is_cmx (List.assoc cmx deps) in
     let cmxs = (List.concat (List.map get_cmxs ds)) @ [cmx] in
     let fn acc f = if List.mem f acc then acc else f :: acc in
     List.rev (List.fold_left fn [] cmxs) in
   let cmxs = get_cmxs main in
   let args = ["-o"; target; "-linkpkg"] @ cmxs in
   let optcmd = opt_command config in
   let cmd = optcmd ^ (String.concat " " args) in command "LNK" main cmd)
let run_binary config fn =
  let args = config.bin_args in
  let args = if (!verbose) < 2 then "--quiet" :: args else args in
  let cmd = String.concat " " (fn :: args) in command "RUN" fn cmd
let extend_config config ls =
  let set_format config f =
    if config.pat_format <> None
    then config
    else
      (let packages =
         if List.mem f patoconfig.formats
         then ("patoline.format." ^ f) :: (config.packages)
         else config.packages in
       { config with packages; pat_format = (Some f) }) in
  let set_driver config d =
    if config.pat_driver <> None
    then config
    else
      (let packages =
         if List.mem d patoconfig.drivers
         then ("patoline.driver." ^ d) :: (config.packages)
         else config.packages in
       { config with packages; pat_driver = (Some d) }) in
  let add_package config p =
    let packages = config.packages @ [p] in { config with packages } in
  let combine config (k, vo) =
    match (k, vo) with
    | ("FORMAT", Some f) -> set_format config f
    | ("DRIVER", Some d) -> set_driver config d
    | ("FORMAT", None) ->
        (eprintf "Pragma FORMAT needs an argument.\n%!"; config)
    | ("DRIVER", None) ->
        (eprintf "Pragma FORMAT needs an argument.\n%!"; config)
    | ("PACKAGE", Some p) -> add_package config p
    | ("PACKAGE", None) ->
        (eprintf "Pragma PACKAGE needs an argument.\n%!"; config)
    | _ -> (eprintf "Unknown pragma %s\n%!" k; config) in
  List.fold_left combine config ls
let compile config file =
  let pragmas = Pragma.pragma_from_file file in
  let config = extend_config config pragmas in
  run_pp ~is_main:true config file;
  (let to_target fn =
     let (dir, base, ext) = Filename.decompose fn in
     let target_ext = if ext = ".txp" then "_.cmx" else ".cmx" in
     let bdir = to_build_dir dir in Filename.concat bdir (base ^ target_ext) in
   let target = to_target file in
   let deps = Hashtbl.create 37 in
   compile_targets config deps target;
   produce_binary config deps target;
   if config.run_binary
   then
     (let to_bin fn =
        let (dir, base, ext) = Filename.decompose fn in
        let bdir = to_build_dir dir in
        let target_ext = match ext with | ".txp" -> "_.opt" | _ -> ".opt" in
        Filename.concat bdir (base ^ target_ext) in
      run_binary config (to_bin file)))
