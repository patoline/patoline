let amble=ref Generateur.Main
let files=ref []
let compile = ref true
let run = ref true
let cmd_line = ref []
let format=ref "DefaultFormat"
let dirs = ref []
let package_list = ref ["Typography"]
let no_grammar=ref false
let deps_only=ref false
let spec = [("--extra-fonts-dir",Arg.String (fun x->cmd_line:=("--extra-fonts-dir "^x)::(!cmd_line)), "Adds directories to the font search path");

            ("-I",Arg.String (fun x->
                                PatolineConfig.local_path:=x::(!PatolineConfig.local_path);
                                PatolineConfig.grammarsdir:=x::(!PatolineConfig.grammarsdir)),
             "Adds directories to the search path");

            ("--no-grammar",Arg.Unit (fun ()->PatolineConfig.grammarsdir:=[]), "Empty grammar search path");
            ("--format",Arg.String
	      (fun f ->format := Filename.basename f), "Change the default document format");
            ("-c",Arg.Unit (fun ()->amble:=Generateur.Separate), "Compile separately");
            ("--noamble",Arg.Unit (fun ()->amble:=Generateur.Noamble), "Compile separately");
            ("-package",Arg.String (fun s-> package_list:= s::(!package_list)), "Use package given as argument when compiling");
            ("--caml",Arg.String (fun arg -> (dirs := arg :: !dirs)), "Add the given arguments to the OCaml command line");
	    ("--ml",Arg.Unit (fun () -> compile:=false; run:= false), "Only generates OCaml code");
	    ("--bin",Arg.Unit (fun () -> compile:=true; run:= false), "Generates OCaml code and compiles it");
	    ("--pdf",Arg.Unit (fun () -> compile:=true; run:= true), "Generates OCaml code, compiles it and runs it");
           ]

let str_dirs () = (String.concat " " !dirs)

let pdfname_of f = (Filename.chop_extension f)^".pdf"
let mlname_of f = (Filename.chop_extension f)^".tml"
let binname_of f = (Filename.chop_extension f)^".tmx"
let execname_of f = if Filename.is_implicit f then "./"^(binname_of f) else (binname_of f)


let rec process_each_file = 
  let doit f = 
    let where_ml = open_out (mlname_of f) in
    let fread = open_in f in
      (* Printf.fprintf stderr "Generating OCaml code "; *)
      Parser.out_grammar_name:=Filename.chop_extension f;
      if Filename.check_suffix f "txt" then (
	(* Printf.fprintf stderr "from simple text file...\n "; *)
	SimpleGenerateur.gen_ml !format SimpleGenerateur.Main f fread (mlname_of f) where_ml (pdfname_of f);
      ) else (
	(* Printf.fprintf stderr "from Patoline file...\n "; *)
	Parser.fprint_caml_buf :=
	  (fun ld gr buf s e txps ->
	     let pos = pos_in fread in
	       Generateur.print_caml_buf (Parser.pp ()) ld gr (Generateur.Source.of_in_channel fread) buf s e txps;
               seek_in fread pos);
	Generateur.gen_ml !format !amble f fread (mlname_of f) where_ml (pdfname_of f);
      );
      Printf.fprintf stderr "ML generated.\n" ; flush stderr ;
      close_out where_ml;
      close_in fread;
      (* Printf.fprintf stderr "File %s generated.\n" (mlname_of f); *)
      let format_cline = if !format = "DefaultFormat" then "" else
	(!format)^".cmxa"
      in
	if !compile then (
	  let lespackages = String.concat "," !package_list in
	    Printf.fprintf stderr "Compiling OCaml code...\n";
	    Printf.fprintf stderr "ocamlfind ocamlopt -package %s %s -linkpkg -o %s -impl %s\n" lespackages ((str_dirs ()) ^ " " ^ format_cline) (binname_of f) (mlname_of f);
	    flush stderr;
	    let r = Sys.command (Printf.sprintf "ocamlfind ocamlopt -package %s str.cmxa %s -linkpkg -o %s -impl %s" lespackages ((str_dirs ()) ^ " " ^ format_cline) (binname_of f) (mlname_of f)) in
	      flush stderr;
	      if r=0 then (
		Printf.fprintf stderr "File %s generated.\n" (binname_of f);
		flush stderr;
		if !run then (
		  let cline = (List.fold_left (fun acc x -> (x^" ")^acc) "" !cmd_line) in
		    Printf.fprintf stderr "Running OCaml code... \n";
		    flush stderr;
		    let r= Sys.command (Printf.sprintf "%s %s" (execname_of f) cline) in
		      flush stderr;
		      if r=0 then Printf.fprintf stderr "File %s generated.\n" (pdfname_of f)
		      else Printf.fprintf stderr "Execution returned with exit code %d.\n" r
		)
	      ) else Printf.fprintf stderr "Compilation returned with exit code %d.\n" r;
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
