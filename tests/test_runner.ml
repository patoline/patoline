(** Dune test runner: compiles each example with the PDF driver and
    compares the extracted text against a baseline stored in
    [tests/baselines/].  Exit code 0 = all pass, 1 = at least one fail.

    The runner is run by [dune runtest].  In a development build it finds
    the baselines via [Sys.executable_name]; after [opam install] it also
    checks [opam var share/patoline/patoline_test/].

    Required external tools: [patoline], [pdftotext] (poppler-utils). *)

let () =
  let project_root =
    Filename.dirname
      (Filename.dirname
         (Filename.dirname
            (Filename.dirname Sys.executable_name)))
  in
  let examples_dir = Filename.concat project_root "examples" in
  let baselines_dir = Filename.concat project_root "tests/baselines" in
  let opam_prefix =
    match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
    | Some p -> p
    | None -> "/"
  in
  let share_baselines =
    Filename.concat opam_prefix "share/patoline/patoline_test"
  in

  (* Find patoline. *)
  let opam_bin_dir = Filename.concat opam_prefix "bin" in
  let opam_patoline = Filename.concat opam_bin_dir "patoline" in
  let patoline_exe =
    if Sys.file_exists opam_patoline then opam_patoline
    else "patoline"  (* rely on PATH *)
  in

  (* Find baselines. Prefer local, fall back to share after opam install. *)
  let baselines_dir =
    if Sys.file_exists (Filename.concat baselines_dir "hello_world.txt")
    then baselines_dir
    else if Sys.file_exists (Filename.concat share_baselines "hello_world.txt")
    then share_baselines
    else baselines_dir
  in

  let fonts_dir = Filename.concat project_root "fonts" in
  let hyph_dir  = Filename.concat project_root "hyphs" in

  (* Smoke: check required external tools are present. *)
  if Sys.command (Printf.sprintf "which %S > /dev/null 2>&1" patoline_exe) <> 0
     && not (Sys.file_exists patoline_exe) then
    (Printf.eprintf "test_runner: patoline not found on PATH (tried %S)\n" patoline_exe;
     exit 125);

  let tests = [
    "hello_world";
    "minimal";
    "xkcd";
    "genealogy";
    "bilingual_zh_en";
  ] in

  let open Test_lib in
  let results = ref [] in
  List.iter (fun name ->
    try
      let r = run_test
        ~patoline_exe
        ~fonts_dir
        ~hyph_dir
        ~examples_dir
        ~baselines_dir
        ~test_name:name
      in
      results := r :: !results
    with Failure msg ->
      (* A crash means patoline failed; record as FAIL. *)
      results := {
        name; pdf_path = ""; baseline_path = "";
        extracted = ""; baseline = "";
        passed = false;
        diff = ["patoline crashed: " ^ msg];
      } :: !results
  ) tests;

  let results = List.rev !results in
  let open Format in
  let fmt = std_formatter in
  pp_print_newline fmt ();
  pp_print_string fmt "PDF regression tests";
  pp_print_newline fmt ();
  pp_print_string fmt (String.make 72 '-');
  pp_print_newline fmt ();
  List.iter (pp_result fmt) results;
  pp_print_string fmt (String.make 72 '-');
  pp_print_newline fmt ();

  let passed = List.filter (fun r -> r.passed) results in
  let failed = List.length results - List.length passed in
  pp_print_newline fmt ();
  if failed = 0 then
    (pp_print_string fmt "All tests passed.";
     pp_print_newline fmt ();
     exit 0)
  else
    (pp_print_string fmt (Printf.sprintf "FAILED: %d/%d test(s)" failed (List.length results));
     pp_print_newline fmt ();
     exit 1)
