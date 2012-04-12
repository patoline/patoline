let fugue=ref true
let files=ref []
let compile = ref true
let run = ref true
let cmd_line = ref []
let format=ref "DefaultFormat"

let spec = [("--extra-fonts-dir",Arg.String (fun x->cmd_line:=("--extra-fonts-dir "^x)::(!cmd_line)), "Adds directories to the font search path");
            ("--format",Arg.Set_string format, "Change the default document format");
            ("-c",Arg.Unit (fun ()->fugue:=false), "Compile separately");
	    ("--ml",Arg.Unit (fun () -> compile:=false; run:= false), "Only generates OCaml code");
	    ("--bin",Arg.Unit (fun () -> compile:=true; run:= false), "Generates OCaml code and compiles it");
	    ("--pdf",Arg.Unit (fun () -> compile:=true; run:= true), "Generates OCaml code, compiles it and runs it");
           ]


let pdfname_of f = (Filename.chop_extension f)^".pdf"
let mlname_of f = (Filename.chop_extension f)^".tml"
let binname_of f = (Filename.chop_extension f)^".tmx"
let execname_of f = if Filename.is_implicit f then "./"^(binname_of f) else (binname_of f)

let rec process_each_file = 
  let doit f = 
    let where_ml = open_out (mlname_of f) in
    let fread = open_in f in
    Printf.fprintf stderr "Generating OCaml code...\n";
    Texprime.gen_ml !format !fugue fread where_ml (pdfname_of f);
    close_out where_ml;
    close_in fread;
    Printf.fprintf stderr "File %s generated.\n" (mlname_of f);
    if !compile then (
      Printf.fprintf stderr "Compiling OCaml code...\n";
      flush stderr;
      let r = Sys.command (Printf.sprintf "ocamlfind ocamlopt -package camomile,dyp,Typography -linkpkg -o %s DefaultFormat.cmxa -impl %s" (binname_of f) (mlname_of f)) in
      flush stderr;
      if r=0 then (
	Printf.fprintf stderr "File %s generated.\n" (binname_of f);
	flush stderr;
	if !run then (
	  let cline = (List.fold_left (fun acc x -> (x^" ")^acc) "" !cmd_line) in
	  Printf.fprintf stderr "Runing OCaml code... \n";
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
