let amble=ref Generateur.Main
let files=ref []
let compile = ref true
let run = ref true
let cmd_line = ref []
let format=ref "DefaultFormat"
let cmd_line_format = ref false
let cmd_line_driver= ref false
let dirs = ref []
let package_list = ref ["Typography"]
let no_grammar=ref false
let deps_only=ref false
let extras = ref ""
let driver = ref "Pdf"
let spec = [("--extra-fonts-dir",Arg.String (fun x->cmd_line:=("--extra-fonts-dir "^x)::(!cmd_line)), "Adds directories to the font search path");

	    ("--extra-hyph-dir",Arg.String (fun x->cmd_line:=("--extra-hyph-dir "^x)::(!cmd_line)), "Adds directories to the hyphenation search path");

            ("-I",Arg.String (fun x->
                                PatolineConfig.local_path:=x::(!PatolineConfig.local_path);
                                PatolineConfig.grammarsdir:=x::(!PatolineConfig.grammarsdir);
				dirs := ("-I " ^ x) :: !dirs),
             "Adds directories to the search path");

            ("--no-grammar",Arg.Unit (fun ()->PatolineConfig.grammarsdir:=[]), "Empty grammar search path");
            ("--format",Arg.String
              (fun f ->format := Filename.basename f; cmd_line_format := true), "Change the default document format");
            ("--driver",Arg.String
               (fun f ->driver := f; cmd_line_driver := true), "Change the default document driver");
            ("-c",Arg.Unit (fun ()->amble:=Generateur.Separate), "Compile separately");
            ("--noamble",Arg.Unit (fun ()->amble:=Generateur.Noamble), "Compile separately");
            ("-package",Arg.String (fun s-> package_list:= s::(!package_list)), "Use package given as argument when compiling");
            ("--caml",Arg.String (fun arg -> (dirs := arg :: !dirs)), "Add the given arguments to the OCaml command line");
            ("--ml",Arg.Unit (fun () -> compile:=false; run:= false), "Only generates OCaml code");
            ("--bin",Arg.Unit (fun () -> compile:=true; run:= false), "Generates OCaml code and compiles it");
            ("--pdf",Arg.Unit (fun () -> compile:=true; run:= true), "Generates OCaml code, compiles it and runs it");
	    ("--edit-link", Arg.Unit (fun () -> Generateur.edit_link:=true), "Generated uri link of the form \"edit:filename@line");
	    ("--no-line-directive", Arg.Unit (fun () -> Generateur.line_directive:=false), "Disable generation of \"# line\" directive in the generated ml (for debugging the generator)");
	    ("--",Arg.Rest (fun s -> extras := !extras ^ " " ^s), "Remaining arguments are passed to the OCaml executable")
           ]

let str_dirs () = (String.concat " " !dirs)

let pdfname_of f = (Filename.chop_extension f)^".pdf"
let mlname_of f = (Filename.chop_extension f)^".tml"
let binname_of f = (Filename.chop_extension f)^".tmx"
let execname_of f = if Filename.is_implicit f then "./"^(binname_of f) else (binname_of f)

let read_options_from_source_file fread =
  let nothing = Str.regexp "^[ \t]*\\((\\*.*\\*)\\)?[ \t\r]*$" in
  let set_format = Str.regexp "^[ \t]*(\\*[ \t]*#FORMAT[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let set_driver = Str.regexp "^[ \t]*(\\*[ \t]*#DRIVER[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let add_compilation_option = Str.regexp "^[ \t]*(\\*[ \t]*#COMPILATION[ \t]+\\(.+\\)[ \t]*\\*)[ \t\r]*$" in
  let add_package = Str.regexp "^[ \t]*(\\*[ \t]*#PACKAGES[ \t]+\\([^ \t]+\\(,[ \t]*[^ \t]+\\)*\\)[ \t]*\\*)[ \t\r]*$" in
  let rec pump () =
    let s = input_line fread in
    if Str.string_match set_format s 0 then (
      if not !cmd_line_format then format := Str.matched_group 1 s;
      pump ()
    ) 
    else if Str.string_match set_driver s 0 then (
      if not !cmd_line_driver then driver := Str.matched_group 1 s;
      pump ()
    ) 
    else if Str.string_match add_compilation_option s 0 then (dirs := (Str.matched_group 1 s)::(!dirs); pump ())
    else if Str.string_match add_package s 0 then (package_list := (Str.split (Str.regexp ",[ \t]*") (Str.matched_group 1 s)) @ (!dirs); pump ())
    else if Str.string_match nothing s 0 then pump ()
  in
  (try 
     pump ()
   with End_of_file -> ());
  seek_in fread 0
    
    
let rec process_each_file = 
  let doit f = 
    let where_ml = open_out (mlname_of f) in
    let fread = open_in f in
    Parser.out_grammar_name:=Filename.chop_extension f;
    if Filename.check_suffix f "txt" then (
      SimpleGenerateur.gen_ml !format SimpleGenerateur.Main f fread (mlname_of f) where_ml (pdfname_of f);
    ) else (
      read_options_from_source_file fread;
      Parser.fprint_caml_buf :=
        (fun ld gr buf s e txps opos ->
          let pos = pos_in fread in
          Generateur.print_caml_buf (Parser.pp ()) ld gr (Generateur.Source.of_in_channel fread) buf s e txps opos;
          seek_in fread pos);
      Generateur.gen_ml !format !driver !amble f fread (mlname_of f) where_ml (pdfname_of f);
    );
    Printf.fprintf stderr "ML generated.\n" ; flush stderr ;
    close_out where_ml;
    close_in fread;
      (* Printf.fprintf stderr "File %s generated.\n" (mlname_of f); *)
    if !format <> "DefaultFormat" then
      package_list := ("Typography." ^ !format) :: !package_list;
    package_list := ("Typography."^ !driver):: !package_list;

    if !compile then (
      let lespackages = String.concat "," (List.rev !package_list) in
      let lesincludes = String.concat " " (List.map (fun s -> "\""^s^".cmx\"") !Generateur.includeList) in 
      let build_command = Printf.sprintf "ocamlfind ocamlopt -package %s %s %s -linkpkg -o \"%s\" -impl \"%s\"" lespackages (str_dirs ()) lesincludes (binname_of f) (mlname_of f)in
      Printf.fprintf stderr "Compiling OCaml code...\n";
      Printf.fprintf stderr "%s\n" build_command;
      flush stderr;
      let r = Sys.command build_command in
      flush stderr;
      if r=0 then (
        Printf.fprintf stderr "File %s generated.\n" (binname_of f);
        flush stderr;
        if !run then (
          let cline = (List.fold_left (fun acc x -> (x^" ")^acc) !extras !cmd_line) in
          Printf.fprintf stderr "Running OCaml code... \n";
          flush stderr;
          let r= Sys.command (Printf.sprintf "\"%s\" %s" (execname_of f) cline) in
          if r=0 then Printf.fprintf stderr "Execution successful.\n"
          else (Printf.fprintf stderr "Execution returned with exit code %d.\n" r; exit r)
        )
      ) else (Printf.fprintf stderr "Compilation returned with exit code %d.\n" r; exit r)
    )
  in
  (function 
    | [] -> Printf.fprintf stderr "No input file given!\n"
    | [f] -> doit f 
    | f::l -> (doit f; process_each_file l)
  )

let _ =
  Arg.parse spec (fun x->files := x::(!files)) "Usage :";
  process_each_file (List.rev !files)
