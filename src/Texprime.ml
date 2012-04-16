open Typography.Config
(* let fugue=ref true *)
(* let spec = [("--extra-fonts-dir",Arg.String (fun x->fontsdir:=x::(!fontsdir)), "Adds directories to the font search path"); *)
(*             ("-c",Arg.Unit (fun ()->fugue:=false), "compile separately"); *)
(*            ] *)
(* let filename=ref [] *)
(* let _=Arg.parse spec (fun x->filename:=x::(!filename)) "Usage :" *)

open Lexing
open Parser


let preambule format fugue = "
  open Typography
  open Typography.Util
  open Typography.Config
  open Typography.Document
  open Typography.OutputCommon
"^(if fugue then
     "let spec = [(\"--extra-fonts-dir\",Arg.String (fun x->Config.fontsdir:=x::(!Config.fontsdir)), \"\
Adds directories to the font search path\")];;
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
     let (_,pages,user')=TS.typeset
       ~completeLine:compl
       ~figure_parameters:fig_params
       ~figures:figures
       ~parameters:params
       ~badness:(Badness.badness pars)
       pars
     in
     let env2, reboot=update_names env1 user' in
     if i<10 && reboot && !D.fixable then (
       resolve (i+1) env2
     ) else Out.output tree pars figures env2 pages filename
  in
     resolve 0 defaultEnv
" outfile

let moduleCounter=ref 0
let no_ind = { up_right = None; up_left = None; down_right = None; down_left = None }

let split_ind indices =
  { no_ind with up_left = indices.up_left; down_left = indices.down_left; },
  { no_ind with up_right = indices.up_right; down_right = indices.down_right; }

let print_math ch m = 
  let rec fn indices ch m =
    match m with
	Var name | Num name ->
	  let elt = "Maths.glyphs \""^name^"\"" in
	  Printf.fprintf ch "[Maths.Ordinary %a ]" (hn elt) indices
      | Symbol name ->
	let elt = "Maths.symbol \""^name^"\"" in
	Printf.fprintf ch "[Maths.Ordinary %a ]" (hn elt) indices
      | Fun name ->
	let elt = "fun env -> Maths.glyphs \""^name^"\" { env with Maths.mathsFont=Lazy.lazy_from_val env0.font }" in
	Printf.fprintf ch "[Maths.Ordinary %a ]" (hn elt) indices
      | Indices(ind', m) ->
	fn ind' ch m 
      | Binary(_, a, _,"over",_,b) ->
	if (indices <> no_ind) then failwith "Indices on fraction.";
	Printf.fprintf ch "[Maths.Fraction {  Maths.numerator=(%a); Maths.denominator=(%a); Maths.line={OutputCommon.default with lineWidth = _env.Maths.default_rule_thickness }}]" (fn indices) a (fn indices) b
      | Binary(pr, a, _,"",_,b) ->
	if (indices <> no_ind) then failwith "Indices on binary.";
	Printf.fprintf ch "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Invisible; Maths.bin_left=(%a); Maths.bin_right=(%a) }]" pr (fn indices) a (fn indices) b
      | Binary(pr,a,nsl,op,nsr,b) ->
	if (indices <> no_ind) then failwith "Indices on binary.";
	Printf.fprintf ch "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Normal(%b,Maths.noad (Maths.glyphs \"%s\"), %b); Maths.bin_left=(%a); Maths.bin_right=(%a) }]" pr nsl op nsr (fn indices) a (fn indices) b
      | Apply(f,a) ->
	let ind_left, ind_right = split_ind indices in
	Printf.fprintf ch "(%a)@(%a)" (fn ind_left) f (fn ind_right) a 
      | MathMacro (macro, args) ->
	Printf.fprintf ch "(%s " macro ;
	List.iter
	  (fun arg ->
	    Printf.fprintf ch "(%a) " (fn indices) arg)
	  args ;
	Printf.fprintf ch ")"
      | Delim(op,a,cl) ->
	dn indices op cl ch a
      | Prefix(pr, op, nsp, b) ->
	let ind_left, ind_right = split_ind indices in
	let elt = "Maths.glyphs \""^op^"\"" in
	Printf.fprintf ch "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Normal(%b,%a, true); Maths.bin_left=[]; Maths.bin_right=(%a) }]" pr nsp (hn elt) ind_left (fn ind_right) b
      | Postfix(pr, a, nsp, op) ->
	let ind_left, ind_right = split_ind indices in
	let elt = "Maths.glyphs \""^op^"\"" in
	Printf.fprintf ch "[Maths.Binary { Maths.bin_priority=%d; Maths.bin_drawing=Maths.Normal(true,%a, %b); Maths.bin_left=(%a); Maths.bin_right=[] }]" pr (hn elt) ind_right nsp (fn ind_left) a
  and gn ch name ind =
    match ind with 
	None -> assert false
      | Some m ->
	Printf.fprintf ch "%s = (%a);" name (fn no_ind) m
  and hn elt ch ind =
    if ind = no_ind then
      Printf.fprintf ch "(Maths.noad (%s))" elt
    else begin
      Printf.fprintf ch "{ (Maths.noad (%s)) with " elt;
      if ind.up_right <> None then gn ch "Maths.superscript_right" ind.up_right;
      if ind.up_left <> None then gn ch "Maths.superscript_left" ind.up_left;
      if ind.down_right <> None then gn ch "Maths.subscript_right" ind.down_right;
      if ind.down_left <> None then gn ch "Maths.subscript_left" ind.down_left;
      Printf.fprintf ch "}"
    end
  and dn ind op cl ch m =
    if ind = no_ind then
      Printf.fprintf ch 
	"[Maths.Decoration (Maths.open_close (Maths.glyphs \"%s\" _env style) (Maths.glyphs \"%s\" _env style), %a)]"
	op cl (fn no_ind) m
    else
      (* FIXME: indice sur les délimiteurs *)
      Printf.fprintf ch "[]"
  in fn no_ind ch m

let print_math_par ch display m =
  let style = if display then "Maths.Display" else "Maths.Text" in
  Printf.fprintf ch
    "[B (fun env0 -> List.map (fun b -> Util.resize env0.size b) (let style = %s and _env = Maths.default.(Maths.int_of_style %s) in Maths.draw_maths Maths.default style ("
    style style;
  print_math ch m;
  Printf.fprintf ch ")))] "

let rec print_macro ch op mtype name args =
  begin
    match mtype with
      | `Single -> 
	  Printf.fprintf ch "%s " name;
	List.iter (function
        Paragraph(p) -> Printf.fprintf ch " %a" (print_contents op) p
	  | Caml(s,e,txps) -> print_caml op ch s e txps
	  | _ -> assert false) args;
	if args = [] then Printf.fprintf ch " ()";
      | `Module | `Begin -> 
	let end_open =
	  if args = [] then 
	    ""
	  else begin
	    let num = ref 1 in
	    Printf.fprintf ch "module Args = struct\n";
	    List.iter (function
            Paragraph(p) -> Printf.fprintf ch "let arg%d = %a" !num (print_contents op) p;
	      incr num
	      | Caml(s,e,txps) -> begin
		Printf.fprintf ch "let arg%d = begin " !num;
		print_caml op ch s e txps;
		Printf.fprintf ch " end ";
		incr num
	      end
	      | _ -> assert false) args;
	    Printf.fprintf ch "end\n";
	    "(Args)"
	  end
	in
	let modname = 
	  if mtype = `Begin then begin
            incr moduleCounter;
	    Printf.fprintf ch "module TEMP%d = struct\n" !moduleCounter;
	    "Env_"^name
	  end
	  else name
	in
	Printf.fprintf ch "open %s%s\n let _ = do_begin_env()" modname end_open (* name *)
      | `End -> Printf.fprintf ch "let _ = do_end_env()\nend" (* name *)
      | `Include ->
        incr moduleCounter;
        Printf.fprintf ch
          "module TEMP%d=%s.Document(D);;\nmodule TEMP%d=struct open TEMP%d end\n" !moduleCounter name (!moduleCounter+1) !moduleCounter;
        incr moduleCounter
  end

and print_caml op ch s e txps = match txps with
  | [] -> 
    let size = e - s in
    let buf=String.create size in
    let _= seek_in op s; input op buf 0 size in
    Printf.fprintf ch "%s" buf
  | (style, s',e') :: txps' -> begin
    (* On imprime du caml avant le premier "<<" *)
    let offset = match style with
      | TxpMath -> 2
      | TxpText -> 2
    in
    let size = s' - s - offset in
    (* Printf.fprintf stderr "s = %d, s' = %d, e' = %d, e = %d\n" s s' e' e;  *)
    let buf=String.create size in
    let _= seek_in op s; input op buf 0 size in
    Printf.fprintf ch "%s" buf;
    (* On imprime la premiere section texprime *)
    let size_txp = e' - s' in
    let buf'=String.create size_txp in
    let _= seek_in op s'; input op buf' 0 size_txp in
    (* Printf.fprintf stderr "Texprime parse dans du Caml: %s\n" buf'; (\* Debug *\) *)
    let lexbuf_txp = Dyp.from_string (Parser.pp ()) buf' in
    begin match style with
      | TxpMath ->  begin
	let txp = Parser.math lexbuf_txp in
	match txp with
	  | [] -> assert false
	  | (docs, _) :: _ -> print_math ch docs
      end
      | TxpText -> 
	let txp = Parser.eparagraph lexbuf_txp in
	match txp with
	  | [] -> assert false
	  | (docs, _) :: _ -> print_contents op ch docs
    end ;
    print_caml op ch (e' + offset) e txps'
  end

and print_contents op ch l = 
  Printf.fprintf ch "(";
  let rec fn l = 
    begin match l with
      [] ->  Printf.fprintf ch "[]";
    | TC s :: l -> 
      Printf.fprintf ch "(T \"%s\")::" (String.escaped s);
      fn l
    | GC :: l -> 
      Printf.fprintf ch "(B (fun env -> env.stdGlue))::";
      fn l
    | MC(mtype, name, args) :: l -> 
      Printf.fprintf ch "(";
      print_macro ch op mtype name args;
      Printf.fprintf ch ")@";
      fn l
    | FC(b,m) :: l ->
      Printf.fprintf ch "(";
      print_math_par ch b m;
      Printf.fprintf ch ")@";
      fn l
    end;
  in fn l;
  Printf.fprintf ch ")"

and output_list from where no_indent lvl docs = 
  match docs with
      [] -> 
	for i = 1 to lvl - 1 do
	  Printf.fprintf where "let _ = go_up D.structure;;(* 1 *)\n\n"
	done;
    | doc::docs -> 
      let lvl = ref lvl in 
      let next_no_indent = ref false in
      (match doc with
	| Paragraph p ->
	  let env = if no_indent then "(fun x -> { x with par_indent = [] })" 
	    else "(fun x -> x)"
	  in
	  Printf.fprintf where "let _ = newPar D.structure ~environment:%s Document.C.normal parameters %a;;\n" 
	    env (print_contents from) p
	| Caml(s,e,txps) -> print_caml from where s e txps
	| Struct(title, numbered, docs) ->
	  let num = if numbered then "" else " ~numbered:false" in
	  (match docs with
	      Relative docs ->
		Printf.fprintf where "let _ = newStruct%s D.structure %a;;\n\n" num (print_contents from) title;
		output_list from where true (!lvl + 1) docs;
		
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
	| Math m ->
	  Printf.fprintf where "let _ = newPar D.structure ~environment:(fun x->{x with par_indent = []}) Document.C.normal center %a;;\n" 
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

let gen_ml format fugue from where pdfname =
    try
      (* match filename with *)
      (*     []-> Printf.fprintf stderr "no input files\n" *)
      (*   | h::_-> *)
            (* let op=open_in h in *)
            let lexbuf = Dyp.from_channel (Parser.pp ()) from in
            try
	      let docs = Parser.main lexbuf in
	      Printf.fprintf stderr "Fin du parsing (%d trees)\n" (List.length docs); flush stderr;
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
	        (Syntax_Error (Dyp.lexeme_start_p lexbuf,
			       "parsing error"))
	    | Failure("lexing: empty token") ->
	      raise
	        (Syntax_Error (Dyp.lexeme_start_p lexbuf,
			       "unexpected char"))
    with
        Syntax_Error(pos,msg) ->
	  Printf.fprintf stderr "%s:%d,%d %s\n" 
	    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg;
	  exit 1
