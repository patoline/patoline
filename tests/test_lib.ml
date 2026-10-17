(* Test library: helpers for running patoline and extracting PDF text. *)

type test_result = {
  name : string;
  pdf_path : string;
  baseline_path : string;
  extracted : string;
  baseline : string;
  passed : bool;
  diff : string list;
}

(* Read an entire file into a string. *)
let read_file filename =
  let ic = open_in filename in
  let buf = Buffer.create 1024 in
  try
    while true do
      Buffer.add_string buf (input_line ic);
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  with End_of_file ->
    close_in ic;
    Buffer.contents buf

(* Run patoline on a .txp file, extract plain text from the PDF via
   pdftotext, and compare it against a baseline. Returns a [test_result].
   Raises [Failure] if patoline itself crashes. *)
let run_test
    ~(patoline_exe : string)
    ~(fonts_dir : string)
    ~(hyph_dir : string)
    ~(examples_dir : string)
    ~(baselines_dir : string)
    ~(test_name : string) : test_result =
  let txp_file = Filename.concat examples_dir (test_name ^ ".txp") in
  let baseline_file = Filename.concat baselines_dir (test_name ^ ".txt") in
  let pdf_file = Filename.concat examples_dir (test_name ^ ".pdf") in
  let err_file = Filename.temp_file "patoline_test_err" ".txt" in

  (* Build the PDF. *)
  let exit_code =
    Sys.command
      (Printf.sprintf
         "%s --driver Pdf --extra-fonts-dir %s --extra-hyph-dir %s \
          -o %s %s 2> %s"
         patoline_exe fonts_dir hyph_dir pdf_file txp_file err_file)
  in
  if exit_code <> 0 then begin
    let err =
      try read_file err_file
      with _ -> "(could not read stderr)" in
    Sys.remove err_file;
    failwith
      (Printf.sprintf "patoline exited with code %d on %s\nstderr: %s"
         exit_code test_name err)
  end;

  (* Extract plain text from the PDF. *)
  let text_file = Filename.temp_file "patoline_test_extract" ".txt" in
  let extract_exit =
    Sys.command
      (Printf.sprintf "pdftotext -layout %s %s 2> /dev/null" pdf_file text_file)
  in
  let extracted =
    if extract_exit = 0 then
      (try read_file text_file with _ -> "")
    else ""
  in
  (try Sys.remove text_file with _ -> ());
  (try Sys.remove err_file with _ -> ());

  (* Load baseline. *)
  let baseline =
    if Sys.file_exists baseline_file then
      (try read_file baseline_file with _ -> "")
    else ""
  in

  (* Compute line-level diff. *)
  let extracted_lines = String.split_on_char '\n' extracted in
  let baseline_lines  = String.split_on_char '\n' baseline in
  let diff = ref [] in
  let max_len = max (List.length extracted_lines) (List.length baseline_lines) in
  for i = 0 to max_len - 1 do
    let el = try List.nth extracted_lines i with _ -> "" in
    let bl = try List.nth baseline_lines i with _ -> "" in
    if el <> bl then
      diff := Printf.sprintf "line %d:\n  got:      %S\n  expected: %S" i el bl
              :: !diff
  done;
  let diff = List.rev !diff in

  { name = test_name;
    pdf_path = pdf_file;
    baseline_path = baseline_file;
    extracted;
    baseline;
    passed = (extracted = baseline);
    diff; }

(* Format a test result for the console. *)
let pp_result fmt r =
  if r.passed then
    Format.fprintf fmt "  %s: PASS@." r.name
  else
    (Format.fprintf fmt "  %s: FAIL@." r.name;
     if r.diff <> [] then
       List.iter (Format.fprintf fmt "    %s@.") r.diff
     else
       Format.fprintf fmt
         "    (empty baseline, %d lines extracted)" (List.length (String.split_on_char '\n' r.extracted)))
