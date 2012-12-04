(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
open Lexing

type amble=Noamble | Separate | Main

let hashed="(Filename.concat Filename.temp_dir_name (Digest.to_hex (Digest.string ((Sys.getcwd ())^Sys.executable_name))))"

let preambule format amble filename=
  match amble with
      Noamble->""
    | _->(
        "open Typography\nopen Typography.Util\n"^
        "open Typography.Config\nopen Typography.Document\nopen Typography.OutputCommon\n"^
          (match amble with
               Main->
                 "let spec = [(\"--extra-fonts-dir\",Arg.String (fun x->Config.fontsdir:=x::(!Config.fontsdir)),\"Adds directories to the font search path\");
(\"--extra-hyph-dir\",Arg.String (fun x->Config.hyphendir:=x::(!Config.hyphendir)), \"Adds directories to the font search path\");
(\"--clean\", Arg.Unit (fun ()->let hashed_tmp="^hashed^" in if Sys.file_exists hashed_tmp then Sys.remove hashed_tmp;exit 0),\"Cleans the saved environment\")];;
let _=Arg.parse spec ignore \"Usage :\";;
module D=(struct let structure=ref (Node { empty with node_tags=[InTOC] },[]) let fixable=ref false end:DocumentStructure)\n"
             | Separate->"module Document=functor(D:DocumentStructure)->struct\n"
             | _->"")^
          "module Format="^format^".Format(D);;\nopen Format;;\n"
      )

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
  let (logs,pages,figs',user')=TS.typeset
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
  ) else (
    List.iter (fun x->Printf.fprintf stderr \"%%s\\n\" (Typography.Language.message x)) logs;
    let tmp=Filename.concat Filename.temp_dir_name (Digest.to_hex (Digest.string ((Sys.getcwd ())^filename))) in
    let f=open_out %s in
    output_value f (env2.names,env2.user_positions);
    close_out f;
    Out.output tree pars figures env2 pages filename
  )
  in
  let env0=
    if Sys.file_exists %s then (
      let f=open_in %s in
      let u,v=input_value f in
      let env={ defaultEnv with names=u;user_positions=v } in
      close_in f; env
    ) else defaultEnv
  in
  resolve 0 env0
" outfile hashed hashed hashed


type contenu =
    Txt of string
  | Spc
  | FromFile of int*int

let get_interval =
  let pos = ref 0 in
  (fun f -> let d = !pos in pos := pos_in f; (d, (!pos-d)))

let par_split from =
  let space = Str.regexp "^[ \t\r]*$" in
  let decide_newpar = Str.regexp "^[ \t]+" in
  let cleanup s =  let a,b= (get_interval from) in FromFile (a,b)
(* (String.escaped (Str.global_replace remove "" s))  *)
  in
  let rec doit parlist textlist =
    try 
      let s = input_line from in
	if Str.string_match space s 0 then 
	  doit (if List.length textlist = 0 then parlist else ((List.rev textlist)::parlist)) []
	else if Str.string_match decide_newpar s 0 then 
	  doit (if List.length textlist = 0 then parlist else ((List.rev textlist)::parlist)) [cleanup s]
	else doit parlist (Spc::(cleanup s)::textlist)
    with End_of_file -> List.rev ((List.rev textlist)::parlist)
  in
    doit [] []


let gen_ml format amble filename from wherename where pdfname =
  Printf.fprintf where "%s" (preambule format amble filename);
  List.iter (fun unpar ->
	       Printf.fprintf where "let _ = newPar D.structure ~environment:(fun x -> { x with par_indent = [] }) Complete.normal parameters [\n";
	       List.iter (function 
		 | Txt txt -> Printf.fprintf where "T \"%s\"; " txt
		 | Spc -> Printf.fprintf where "T \" \"; "
		 | FromFile (d,e) -> Printf.fprintf where "FileRef (\"%s\",%d,%d); " filename d e
	       ) unpar;
	       Printf.fprintf where "]\n\n"
	    ) (par_split from);
  match amble with
      Main->output_string where (postambule pdfname)
    | Noamble->()
    | Separate->Printf.fprintf where "\nend\n"
	
    (* try *)
    (*   let lexbuf = Dyp.from_channel (SimpleParser.pp ()) from in *)
    (*   try *)
    (* 	let res = SimpleParser.main lexbuf in *)
    (* 	Printf.fprintf stderr "Fin du parsing (%d arbres)\n" (List.length res);  *)
    (* 	let (monarbre, truc) = List.hd res in *)
    (* 	Printf.fprintf stderr "Arbre %s %d\n" truc (List.length monarbre);  *)
    (* 	(\* List.iter (fun s -> Printf.fprintf stderr "%s\n" s) monarbre;  *\) *)
    (* 	flush stderr; *)
    (*   with *)
    (* 	| Dyp.Syntax_error -> *)
    (* 	  raise *)
    (* 	    (SimpleParser.Syntax_Error (Dyp.lexeme_start_p lexbuf, *)
    (* 					Language.Parse_error)) *)
    (* 	| Failure("lexing: empty token") -> *)
    (* 	  raise *)
    (* 	    (SimpleParser.Syntax_Error (Dyp.lexeme_start_p lexbuf, *)
    (* 					Language.Unexpected_char)) *)
    (* with *)
    (* 	SimpleParser.Syntax_Error(pos,msg) -> *)
    (* 	  Sys.remove wherename; *)
    (* 	  Printf.fprintf stderr "%s\n" *)
    (* 	    (Language.message (Language.Syntax_error (filename, pos, msg))); *)
    (* 	  exit 1 *)
	    
