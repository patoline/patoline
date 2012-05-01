open Typography
open Typography.Config
open Typography.Util
open Typography.Syntax
open Typography.Language

(* let fugue=ref true *)
(* let spec = [("--extra-fonts-dir",Arg.String (fun x->fontsdir:=x::(!fontsdir)), "Adds directories to the font search path"); *)
(*             ("-c",Arg.Unit (fun ()->fugue:=false), "compile separately"); *)
(*            ] *)
(* let filename=ref [] *)
(* let _=Arg.parse spec (fun x->filename:=x::(!filename)) "Usage :" *)


(* let math arg = [Document.B (fun env0 -> List.map (fun b -> Box.resize env0.Document.size b) (\* math <$lala$> *\) *)
(*   (let style = Mathematical.Text and _env = (Maths.env_style Maths.default Mathematical.Text) in  *)
(*    Maths.draw_maths Maths.default style ((arg ))))] *)

let _=macros:=StrMap.add "diagram" (fun x-> begin
  " (let module MaFigure (Arg : sig val env : user environment end) = struct \n" ^
    "module Lib = Env_Diagram (struct let arg1 = \"\" end) (Arg) \n open Lib \n" ^
    x ^
    "end in \n" ^
    "[B (fun env -> \n" ^
    "let module Res = MaFigure (struct let env = env end) in \n" ^
    "[ Drawing (Res.Lib.make ()) ])]) " end) !macros


open Lexing
open Parser


let preambule format fugue = "
  open Typography
  open Typography.Util
  open Typography.Box
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
    Out.output tree pars figures env2 pages filename
  )
  in
  resolve 0 defaultEnv
" outfile

let moduleCounter=ref 0
let no_ind = { up_right = None; up_left = None; down_right = None; down_left = None }

let split_ind indices =
  { no_ind with up_left = indices.up_left; down_left = indices.down_left; },
  { no_ind with up_right = indices.up_right; down_right = indices.down_right; }

let print_math_buf buf m = 
  let rec fn indices buf m =
    match m with
	Var name | Num name ->
	  let elt = "Maths.glyphs \""^name^"\"" in
	  Printf.bprintf buf "[Maths.Ordinary %a ]" (hn elt) indices
      | Symbol name ->
	let elt = "Maths.symbol \""^name^"\"" in
	Printf.bprintf buf "[Maths.Ordinary %a ]" (hn elt) indices
      | Fun name ->
	let elt = "fun env -> Maths.glyphs \""^name^"\" env" in
	Printf.bprintf buf "[Maths.Env (fun env->Maths.change_fonts env env.font); Maths.Ordinary %a ]" (hn elt) indices
      | Indices(ind', m) ->
	fn ind' buf m
      | Binary(_, a, _,"over",_,b) ->
	if (indices <> no_ind) then failwith "Indices on fraction.";
	Printf.bprintf buf "[Maths.Fraction {  Maths.numerator=(%a); Maths.denominator=(%a); Maths.line={OutputCommon.default with lineWidth = env0.Maths.default_rule_thickness }}]" (fn indices) a (fn indices) b
      | Binary(pr, a, _,"",_,b) ->
	if (indices <> no_ind) then failwith "Indices on binary.";
	Printf.bprintf buf "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Invisible; Maths.bin_left=(%a); Maths.bin_right=(%a) }]" pr (fn indices) a (fn indices) b
      | Binary(pr,a,nsl,op,nsr,b) ->
	if (indices <> no_ind) then failwith "Indices on binary.";
	Printf.bprintf buf "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Normal(%b,Maths.noad (Maths.glyphs \"%s\"), %b); Maths.bin_left=(%a); Maths.bin_right=(%a) }]" pr nsl op nsr (fn indices) a (fn indices) b
      | Apply(f,a) ->
	let ind_left, ind_right = split_ind indices in
	Printf.bprintf buf "(%a)@(%a)" (fn ind_left) f (fn ind_right) a 
      | MathMacro (macro, args) ->
	Printf.bprintf buf "(%s " macro ;
	List.iter
	  (fun arg ->
	    Printf.bprintf buf "(%a) " (fn indices) arg)
	  args ;
	Printf.bprintf buf ")"
      | Delim(op,a,cl) ->
	  dn indices op cl buf a
      | Prefix(pr, op, nsp, b) ->
	let ind_left, ind_right = split_ind indices in
	let elt = "Maths.glyphs \""^op^"\"" in
	Printf.bprintf buf "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Normal(%b,%a, true); Maths.bin_left=[]; Maths.bin_right=(%a) }]" pr nsp (hn elt) ind_left (fn ind_right) b
      | Postfix(pr, a, nsp, op) ->
	let ind_left, ind_right = split_ind indices in
	let elt = "Maths.glyphs \""^op^"\"" in
	Printf.bprintf buf "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Normal(true,%a, %b); Maths.bin_left=(%a); Maths.bin_right=[] }]" pr (hn elt) ind_right nsp (fn ind_left) a
      | MScope a->
	  Printf.bprintf buf "[Maths.Scope (";
          List.iter (fn indices buf) a;
          Printf.bprintf buf ")]";
  and gn buf name ind =
    match ind with 
	None -> assert false
      | Some m ->
	Printf.bprintf buf "%s = (%a);" name (fn no_ind) m
  and hn elt buf ind =
    if ind = no_ind then (
      Printf.bprintf buf "(Maths.noad (%s))" elt
    ) else begin
      Printf.bprintf buf "{ (Maths.noad (%s)) with " elt;
      if ind.up_right <> None then gn buf "Maths.superscript_right" ind.up_right;
      if ind.up_left <> None then gn buf "Maths.superscript_left" ind.up_left;
      if ind.down_right <> None then gn buf "Maths.subscript_right" ind.down_right;
      if ind.down_left <> None then gn buf "Maths.subscript_left" ind.down_left;
      Printf.bprintf buf "}"
    end
  and dn ind op cl buf m =
    if ind = no_ind then
      Printf.bprintf buf 
	"[Maths.Decoration (Maths.open_close (Maths.glyphs \"%s\" env0 style) (Maths.glyphs \"%s\" env0 style), %a)]"
	op cl (fn no_ind) m
    else (
      Buffer.add_string buf "[Maths.Ordinary ";
      let buf'=Buffer.create 100 in
        Buffer.add_string buf' "fun envs st->Maths.draw_maths envs st ";
        dn no_ind op cl buf' m;
        hn (Buffer.contents buf') buf ind;
        Buffer.add_string buf "]"
    )
  in fn no_ind buf m

let print_math ch m = begin
  let buf = Buffer.create 80 in
  print_math_buf buf m ;
  output_string ch (Buffer.contents buf) ;
end

let print_math_par_buf buf display m =
  let style = if display then "Mathematical.Display" else "Mathematical.Text" in
  Printf.bprintf buf
    "[B (fun env0 -> List.map (fun b -> Box.resize env0.size b) (let style = %s in Maths.draw_maths env0 style ("
    style;
  print_math_buf buf m;
  Printf.bprintf buf ")))] "

let print_math_par ch display m = begin 
  let buf = Buffer.create 80 in
  print_math_par_buf buf display m ;
  output_string ch (Buffer.contents buf) 
end

let rec print_macro_buf buf op mtype name args =
  begin
    match mtype with
      | `Single -> 
	begin
	  Printf.bprintf buf " (%s " name;
	  List.iter (function
            | Paragraph(p) -> Printf.bprintf buf " %a" (print_contents_buf op) p
	    | Caml(ld,gr,s,e,txps) ->
              Printf.bprintf buf "(";
              print_caml_buf ld gr op buf s e txps;
              Printf.bprintf buf ")";
	    | _ -> assert false) args;
	  if args = [] then Printf.bprintf buf " ()";
	end ;
	Printf.bprintf buf ") ";
      | `Preproc -> 
	begin
	  match args with 
	    | Caml (ld,gr,s,e,txps) :: _ -> begin
	      let buf' = Buffer.create 80 in
              print_caml_buf ld gr op buf' s e txps;
	      let s = Buffer.contents buf' in 
              let f=StrMap.find name !macros in
	      let s' = f s in
	      (* Printf.fprintf stderr "Started from : \n %s \n" s ; *)
	      (* Printf.fprintf stderr "Printed : \n %s \n" s' ; *)
	      Printf.bprintf buf " ( %s ) " s'
	    end
	    | _ -> assert false
	end
      | `Module | `Begin -> 
	let end_open =
	  if args = [] then 
	    if mtype = `Begin && name = "Diagram" then begin
	      Printf.bprintf buf
		"module Args = (struct let arg1 = \"figure%d\" end)" !moduleCounter;
	      "(Args)"
	    end
	    else 
	      ""
	  else begin
	    let num = ref 1 in
	    Printf.bprintf buf "module Args = struct\n";
	    List.iter (function
            Paragraph(p) -> Printf.bprintf buf "let arg%d = %a" !num (print_contents_buf op) p;
	      incr num
	      | Caml(ld,gr,s,e,txps) -> begin
		Printf.bprintf buf "let arg%d = begin " !num;
		print_caml_buf ld gr op buf s e txps;
		Printf.bprintf buf " end ";
		incr num
	      end
	      | _ -> assert false) args;
	    Printf.bprintf buf "end\n";
	    "(Args)"
	  end
	in
	let modname = 
	  if mtype = `Begin then begin
            incr moduleCounter;
	    Printf.bprintf buf "module TEMP%d = struct\n" !moduleCounter;
	    "Env_"^name
	  end
	  else name
	in
	if mtype = `Begin && name = "Diagram" then begin
	(* Printf.bprintf buf "open %s%s\n let _ = do_begin_env()\n" modname end_open (\* name *\) ; *)
	  Printf.bprintf buf 
	    ("module MaFigure(Args : sig val arg1 : string end) (Args' : sig val env : user environment end) = struct \n") ; 
	  Printf.bprintf buf "module Lib = Env_Diagram (Args) (Args')\n include Lib\n" (* name *) 
	end
	else begin
	  Printf.bprintf buf "open %s%s\n let _ = do_begin_env()" modname end_open (* name *)
	end
      | `End -> 

	if name = "Diagram" then begin
	  Printf.bprintf buf "\n end \n" (* name *) ;
	  Printf.bprintf buf "let _ = figure D.structure ~name:Args.arg1 (fun env -> \n" (* name *) ;
	  Printf.bprintf buf "   let module Res = MaFigure (Args) (struct let env = env end) in \n" ;
	  Printf.bprintf buf "   Res.make ())\n end \n " end
	else Printf.bprintf buf "let _ = do_end_env()\nend" (* name *)
      | `Include ->
	incr moduleCounter;
	Printf.bprintf buf
	  "module TEMP%d=%s.Document(D);;\nmodule TEMP%d=struct open TEMP%d end\n" !moduleCounter name (!moduleCounter+1) !moduleCounter;
	incr moduleCounter
  end

and print_macro ch op mtype name args = begin
  let buf = Buffer.create 80 in
  print_macro_buf buf op mtype name args ;
  output_string ch (Buffer.contents buf) 
end

and print_caml_buf ld gr op buf s e txps = match txps with
  | [] -> 
    let size = e - s in
    let buf'=String.create size in
    let _= seek_in op s; really_input op buf' 0 size in
    Printf.bprintf buf "%s" buf'
  | (style, s',e') :: txps' -> begin
    (* On imprime du caml avant le premier "<<" *)
    let offset = match style with
      | TxpMath -> 2
      | TxpText -> 2
    in
    let size = s' - s - offset in
    (* Printf.bprintf stderr "s = %d, s' = %d, e' = %d, e = %d\n" s s' e' e; *)
    let buf'=String.create size in
    let _= seek_in op s; really_input op buf' 0 size in
    Printf.bprintf buf "%s" buf';
    (* On imprime la premiere section texprime *)
    let size_txp = e' - s' in
    let buf'=String.create size_txp in
    let _= seek_in op s'; really_input op buf' 0 size_txp in
    (* Printf.fprintf stderr "Texprime parse dans du Caml: %s\n" buf'; (\* Debug *\) *)
    let lexbuf_txp = Dyp.from_string (Parser.pp ()) buf' in
    begin match style with
      | TxpMath ->  begin
	let parser_pilot = { (Parser.pp ()) with Dyp.pp_ld = ld ; Dyp.pp_dev = Obj.obj gr;  } in
	let txp = Dyp.lexparse parser_pilot "allmath" lexbuf_txp in
	match txp with
	  | (Obj_allmath docs, _) :: _ -> print_math_buf buf docs
	  | _ -> assert false
      end
      | TxpText -> 
	let txp = Parser.allparagraph lexbuf_txp in
	match txp with
	  | [] -> assert false
	  | (docs, _) :: _ -> print_contents_buf op buf docs
    end ;
    print_caml_buf ld gr op buf (e' + offset) e txps'
  end

and print_caml ld gr op (ch : out_channel) s e txps = begin
  let buf = Buffer.create 80 in
  print_caml_buf ld gr op buf s e txps;
  output_string ch (Buffer.contents buf) 
end

and print_contents_buf op buf l = 
  Printf.bprintf buf "(";
  let rec fn l = 
    begin match l with
      [] ->  Printf.bprintf buf "[]";
    | TC s :: l -> 
      Printf.bprintf buf "(T \"%s\")::" (String.escaped s);
      fn l
    | GC :: l -> 
      Printf.bprintf buf "T \" \"::";
      fn l
    | MC(mtype, name, args) :: l -> 
      Printf.bprintf buf " (";
      print_macro_buf buf op mtype name args;
      Printf.bprintf buf ")@ ";
      fn l
    | FC(b,m) :: l ->
      Printf.bprintf buf "(";
      print_math_par_buf buf b m;
      Printf.bprintf buf ")@";
      fn l
    end;
  in fn l;
  Printf.bprintf buf ")"

and print_contents op (ch : out_channel) l = begin
  let buf = Buffer.create 80 in
  print_contents_buf op buf l;
  output_string ch (Buffer.contents buf) 
end

and output_list from where no_indent lvl docs = 
  match docs with
      [] -> ()
	(* for i = 1 to lvl - 1 do *)
	(*   Printf.fprintf where "let _ = go_up D.structure;;(\* 1 *\)\n\n" *)
	(* done; *)
    | doc::docs -> 
      let lvl = ref lvl in 
      let next_no_indent = ref false in
      (match doc with
	| Paragraph p ->
	  let env = if no_indent then "(fun x -> { x with par_indent = [] })" 
	    else "(fun x -> x)"
	  in
	  Printf.fprintf where "let _ = newPar D.structure ~environment:%s Complete.normal parameters %a;;\n" 
	    env (print_contents from) p
	| Caml(ld,gr,s,e,txps) -> print_caml ld gr from where s e txps
	| Struct(title, numbered, docs) ->
	  let num = if numbered then "" else " ~numbered:false" in
	  (match docs with
	      Relative docs ->
		Printf.fprintf where "let _ = newStruct%s D.structure %a;;\n\n" num (print_contents from) title;
		output_list from where true (!lvl + 1) docs;
		Printf.fprintf where "let _ = go_up D.structure ;;(* 2 *)\n\n"
	     | Absolute l ->
	      if l > !lvl + 1 then failwith "Illegal level skip";
	      for i = 1 to !lvl - l do
		Printf.fprintf where "let _ = go_up D.structure ;;(* 3 *)\n\n"
	      done;
	      Printf.fprintf where "let _ = newStruct%s D.structure %a;;\n\n" num (print_contents from) title;
	      lvl := l
	  )
	| Macro(mtype, name, args) ->
	  print_macro where from mtype name args;
	  Printf.fprintf where "\n\n" 
	| Preproc t -> begin
	  Printf.fprintf where "%s\n\n" t ;
	  Printf.fprintf stderr "Printed : \n %s \n" t ;
	end
	| Math m ->
	  Printf.fprintf where "let _ = newPar D.structure ~environment:(fun x->{x with par_indent = []}) Complete.normal center %a;;\n" 
	    (fun ch -> print_math_par ch true) m;
	  next_no_indent := true
        | Ignore -> 
	  next_no_indent := no_indent
	| Verbatim(lang, lines) ->
	  Printf.fprintf where "module VERB = struct\n\n";
	  Printf.fprintf where "let verbEnv x = { (envFamily lmmono x)
                                                     with normalMeasure=infinity; par_indent = [] }\n\n";
	  let lang = match lang with
	      None -> "T"
	    | Some s -> s
	  in
	  List.iter (fun l ->
	    Printf.fprintf where
	      "let _ = newPar D.structure ~environment:verbEnv C.normal ragged_left (lang_%s \"%s\");;\n"
	      lang l)
	    lines;
	  Printf.fprintf where "end\n\n";
	  next_no_indent := true
      );
      output_list from where !next_no_indent !lvl docs

let gen_ml format fugue filename from wherename where pdfname =
    try
      (* match filename with *)
      (*     []-> Printf.fprintf stderr "no input files\n" *)
      (*   | h::_-> *)
            (* let op=open_in h in *)
            let lexbuf = Dyp.from_channel (Parser.pp ()) from in
            try
	      let docs = Parser.main lexbuf in
	      Printf.fprintf stderr "%s\n" (Language.message (Language.End_of_parsing (List.length docs))); flush stderr;
	      match docs with
	        [] -> assert false
	      | ((pre, docs), _) :: _  ->
		  Printf.fprintf where "%s" (preambule format fugue);
		  begin match pre with
		    None -> ()
		  | Some(title, at) -> 
		      Printf.fprintf where "let _ = title D.structure %S;;\n\n" title;
		      match at with
			None -> ()
		      | Some(auth,inst) ->
			Printf.fprintf where "let _ = author %S;;\n\n" auth;
			  match inst with
			    None -> ()
			  | Some(inst) ->
			      Printf.fprintf where "let _ = institute %S;;\n\n" inst
		  end;
		  output_list from where true 0 docs;
		  (* close_in op; *)
                  if fugue then
		    output_string where (postambule pdfname)
                  else
		    Printf.fprintf where "\nend\n"
	    with
	    | Dyp.Syntax_error ->
	      raise
	        (Parser.Syntax_Error (Dyp.lexeme_start_p lexbuf,
			              Parse_error))
	    | Failure("lexing: empty token") ->
	      raise
	        (Parser.Syntax_Error (Dyp.lexeme_start_p lexbuf,
			              Unexpected_char))
    with
        Parser.Syntax_Error(pos,msg) ->
          Sys.remove wherename;
	  Printf.fprintf stderr "%s\n"
            (Language.message (Language.Syntax_error (filename, pos, msg)));
	  exit 1
