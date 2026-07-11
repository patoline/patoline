open Patconfig.Config
let parse_args =
  let open Earley_core.Earley in
    let arg =
      Earley_core.Earley.alternatives
        (List.cons (Earley_str.regexp "[^ \t\n'\"]+" (fun group -> group 0))
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "'" "'")
                 (Earley_core.Earley.fsequence
                    (Earley_str.regexp "[^']+" (fun group -> group 0))
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "'" "'")
                       (Earley_core.Earley.empty (fun o -> o)))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '"' '"')
                    (Earley_core.Earley.fsequence
                       (Earley_str.regexp "[^']+" (fun group -> group 0))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '"' '"')
                          (Earley_core.Earley.empty (fun o -> o))))) []))) in
    let args =
      Earley_core.Earley.apply (fun f -> f [])
        (Earley_core.Earley.fixpoint1' (fun l -> l) arg
           (fun x -> fun f -> fun l -> f (List.cons x l))) in
    let parse_args str =
      try parse_string args (Earley_str.blank_regexp "[ \t]+") str
      with
      | _ ->
          (Printf.eprintf "Invalid command-line option list %S." str; exit 1) in
    parse_args[@@ocaml.doc
                " Parse a string containing a command line options and returns the list.\n    This function is useful to be able to forward command line options. "]
let parse_packages =
  let open Earley_core.Earley in
    let pack_re = "[a-zA-Z][a-zA-Z0-9_.]*" in
    let packs =
      Earley_core.Earley.fsequence
        (Earley_str.regexp ~name:"pack" pack_re (fun group -> group 0))
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint' (fun l -> l)
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char ',' ',')
                    (Earley_core.Earley.fsequence
                       (Earley_str.regexp ~name:"pack" pack_re
                          (fun group -> group 0))
                       (Earley_core.Earley.empty
                          (fun _default_0 -> _default_0))))
                 (fun x -> fun f -> fun l -> f (List.cons x l))))
           (Earley_core.Earley.empty (fun ps -> fun p -> p :: ps))) in
    let parse_packages str =
      try parse_string packs no_blank str
      with
      | _ ->
          (Printf.eprintf "Invalid list of ocamlfind packages %S." str;
           exit 1) in
    parse_packages[@@ocaml.doc
                    " Parse a string containing a list of ocamlfind packages separated by a\n    comma. The list of the package names is returned. "]
let bin_args = ref []
let opt_args = ref []
let pp_args = ref []
let local_path = ref []
let packages =
  ref
    ["patoline.patoraw";
    "patoline.typography";
    "patoline.format.DefaultFormat";
    "earley.core";
    "earley.str"]
let pat_format = ref None
let pat_driver = ref None
let do_clean = ref false
let do_clean_cache = ref false
let do_rebuild = ref false
let do_serve = ref false
let serve_port = ref 8080
let serve_root = ref None
let file = ref None
let run_bin = ref true
let add_bin_args l = bin_args := ((!bin_args) @ l)
let add_opt_args l = opt_args := ((!opt_args) @ l)
let add_pp_args l = pp_args := ((!pp_args) @ l)
let add_file f =
  if (!file) <> None
  then (Printf.eprintf "A file has already been given...\n"; exit 1);
  if not (Sys.file_exists f)
  then (Printf.eprintf "The file %s does not exist...\n" f; exit 1);
  file := (Some f)
let add_local_path p = local_path := ((!local_path) @ [p])
let add_package p =
  if not (List.mem p (!packages)) then packages := ((!packages) @ [p])
let add_packages s = List.iter add_package (parse_packages s)
let spec =
  Arg.align
    [("--extra-fonts-dir",
       (Arg.String ((fun d -> add_bin_args ["--extra-fonts-dir"; d]))),
       "dir Add a fonts directory to the search path.");
    ("--extra-hyph-dir",
      (Arg.String ((fun d -> add_bin_args ["--extra-hyph-dir"; d]))),
      "dir Add an hyphenation dictionary directory to the search path.");
    ("--hyphen-lang",
      (Arg.String ((fun l -> add_bin_args ["--hyphen-lang"; l]))),
      "lang Hyphenation language (en-us, fr, de, de-1901, es, it, nl, pl, pt, ru, cs, sv, fi, hu).");
    ("--font-filter",
      (Arg.String ((fun c -> add_bin_args ["--font-filter"; c]))),
      "cmd Add a font filter command for the SVG or Patonet drivers.");
    ("--bin-args", (Arg.String ((fun s -> add_bin_args (parse_args s)))),
      "args Forward the given arguments to the binary.");
    ("-o", (Arg.String ((fun d -> add_bin_args ["-o"; d]))),
      "<file> Output file path (forwarded to the binary).");
    ("--output", (Arg.String ((fun d -> add_bin_args ["-o"; d]))),
      "<file> Output file path (forwarded to the binary).");
    ("-I", (Arg.String add_local_path),
      "dir Add the given path to the source directories.");
    ("--package", (Arg.String add_packages),
      "packs Use the provided ocamlfind packages.");
    ("--format", (Arg.String ((fun f -> pat_format := (Some f)))),
      "f Set the document format.");
    ("--driver", (Arg.String ((fun d -> pat_driver := (Some d)))),
      "d Set the document driver.");
    ("-j",
      (Arg.Int
         ((fun s -> Parallel.nb_threads := (max (!Parallel.nb_threads) s)))),
      "i Compile with the given number of threads.");
    ("--verbose", (Arg.Int ((fun l -> Build.verbose := l))),
      "i Set the verbosity level.");
    ("--clean", (Arg.Set do_clean), " Cleanup the build directories.");
    ("--clean-cache",
      (Arg.Set do_clean_cache),
      " Remove the hash-based incremental cache (cache.bin).");
    ("--rebuild",
      (Arg.Set do_rebuild),
      " Force a full rebuild (ignore the hash-based cache).");
    ("--serve",
      (Arg.Set do_serve),
      " Start an HTTP server for the generated document.");
    ("--port",
      (Arg.Set_int serve_port),
      "i Set the HTTP server port (default 8080).");
    ("--root",
      (Arg.String ((fun d -> serve_root := Some d))),
      "dir Set the HTTP server root directory.");
    ("--bin", (Arg.Clear run_bin),
      " Does not run the produced binary, only produces it.");
    ("--ascii", (Arg.Unit ((fun _ -> add_pp_args ["--ascii"]))),
      " Preprocess to ASCII files.");
    ("--opt-args", (Arg.String ((fun s -> add_opt_args (parse_args s)))),
      "args Forwart the given arguments to the compiler.");
    ("--pp-args", (Arg.String ((fun s -> add_pp_args (parse_args s)))),
      "args Forwart the given arguments to the preprocessor.")]
let usage =
  Printf.sprintf "Usage: %s [drivers | formats | config | [options] [file]]"
let _ =
  match Sys.argv with
  | [|_;"drivers"|] ->
      let f = Printf.printf "%s\n" in
      (List.iter f patoconfig.drivers; exit 0)
  | [|_;"formats"|] ->
      let f = Printf.printf "%s\n" in
      (List.iter f patoconfig.formats; exit 0)
  | [|_;"config"|] -> (print_config stdout; exit 0)
  | _ -> Arg.parse spec add_file (usage (Sys.argv.(0)))
let cfg =
  let path =
    match !file with
    | Some fn -> (Filename.dirname fn) :: (!local_path)
    | None -> !local_path in
  let driver_packages =
    match !pat_driver with
    | None -> ["patoline.driver.Pdf"]
    | Some d when List.mem d patoconfig.drivers -> ["patoline.driver." ^ d]
    | _ -> [] in
  let format_packages =
    match !pat_format with
    | None -> []
    | Some "DefaultFormat" -> []
    | Some f when List.mem f patoconfig.formats -> ["patoline.format." ^ f]
    | _ -> [] in
  let packages = (!packages) @ (format_packages @ driver_packages) in
  let open Build in
    {
      bin_args = (!bin_args);
      opt_args = (!opt_args);
      pp_args = (!pp_args);
      packages;
      path;
      pat_format = (!pat_format);
      pat_driver = (!pat_driver);
      run_binary = (!run_bin)
    }
let _ = if !do_clean then Build.clean_build_dirs cfg
let _ =
  (* Handle cache maintenance flags before [Build.compile] runs. *)
  if !do_clean_cache then
    let cache_fn = Build.cache_file (List.hd cfg.path) in
    if Sys.file_exists cache_fn then
      (Unix.unlink cache_fn;
       if (!Build.verbose) > 0 then
         Printf.printf "[CACHE] removed %S\n%!" cache_fn);
  let cfg =
    if !do_rebuild then
      (* Force every input to look "changed" by appending a marker
         to [opt_args]. The hash cache compares binary mtime + file
         digests, so changing the arg list alone is not enough; the
         simplest portable way is to remove cache.bin (the user
         would normally pair [--rebuild] with [--clean-cache], but we
         do it automatically here). *)
      let cache_fn = Build.cache_file (List.hd cfg.path) in
      (if Sys.file_exists cache_fn then Unix.unlink cache_fn;
       if (!Build.verbose) > 0 then
         Printf.printf "[CACHE] removed %S (--rebuild)\n%!" cache_fn;
       cfg)
    else cfg
  in
  match !file with
  | None ->
      if (!Build.verbose) > 1 then Printf.eprintf "Nothing to do.\n%!";
      if !do_serve then
        (Printf.eprintf "[serve] No input file given, serving current directory.\n%!";
         Serve.serve ~port:!serve_port ~root:(Serve.pdf_root_from_arg None) ())
      else ()
  | Some fn ->
      if !do_serve then
        (Printf.eprintf "[serve] --serve is not yet supported with file input.\n%!";
         exit 1)
      else Build.compile cfg fn
