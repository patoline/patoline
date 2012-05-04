open Lexing


let preambule format fugue = "
  open Typography
  open Typography.Util
  open Typography.Config
  open Typography.Document
  open Typography.OutputCommon
"^(if fugue then
     "let spec = [(\"--extra-fonts-dir\",Arg.String (fun x->Config.fontsdir:=x::(!Config.fontsdir)),
\"Adds directories to the font search path\");
(\"--extra-hyph-dir\",Arg.String (fun x->Config.hyphendir:=x::(!Config.hyphendir)), \"Adds directories to the font search path\")];;
let _=Arg.parse spec ignore \"Usage :\";;
     module D=(struct let structure=ref (Node { empty with node_tags=[InTOC] },[]) let fixable=ref false end:DocumentStructure)\n"
   else "module Document=functor(D:DocumentStructure)->struct\n")
  ^
  "module Format="^format^".Format(D);;\nopen Format;;\n"


let postambule outfile = Printf.sprintf "
  module Out=OutputPaper.Output(Pdf)

  let _ = 
    let filename=\"%s\" in
    let rec resolve i env0=
     Printf.printf \"Compilation %%d\\n\" i; flush stdout;
     let o=open_out (\"graph\"^string_of_int i) in doc_graph o (fst !D.structure); close_out o;
     D.fixable:=false;
     let tree=postprocess_tree (fst (top (!D.structure))) in
     let env1,fig_params,params,compl,pars,figures=flatten env0 D.fixable tree in
     let (_,pages,figs',user')=TS.typeset
       ~completeLine:compl
       ~figure_parameters:fig_params
       ~figures:figures
       ~parameters:params
       ~badness:(Badness.badness pars)
       pars
     in
     let env2, reboot=update_names env1 figs' user' in
     if i<10 && reboot && !D.fixable then (
       resolve (i+1) env2
     ) else Out.output tree pars figures env2 pages filename
  in
     resolve 0 defaultEnv
" outfile





let gen_ml format amble filename from wherename where pdfname =
    try
      let lexbuf = Dyp.from_channel (SimpleParser.pp ()) from in
      try
	let res = SimpleParser.main lexbuf in
	Printf.fprintf stderr "Fin du parsing (%d arbres)\n" (List.length res); 
	let (monarbre, truc) = List.hd res in
	Printf.fprintf stderr "Arbre %s %d\n" truc (List.length monarbre); 
	(* List.iter (fun s -> Printf.fprintf stderr "%s\n" s) monarbre;  *)
	flush stderr;
      with
	| Dyp.Syntax_error ->
	  raise
	    (SimpleParser.Syntax_Error (Dyp.lexeme_start_p lexbuf,
					Language.Parse_error))
	| Failure("lexing: empty token") ->
	  raise
	    (SimpleParser.Syntax_Error (Dyp.lexeme_start_p lexbuf,
					Language.Unexpected_char))
    with
	SimpleParser.Syntax_Error(pos,msg) ->
	  Sys.remove wherename;
	  Printf.fprintf stderr "%s\n"
	    (Language.message (Language.Syntax_error (filename, pos, msg)));
	  exit 1
	    
