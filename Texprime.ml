open Binary
open Constants
open Lexing
open Util
open Fonts
open Fonts.FTypes
open Parser

let spec = []

let preambule = "
  open Typography
  open Parameters
  open Fonts.FTypes
  open Util
  open Fonts
  open OutputCommon
  open OutputPaper
  open DefaultFormat;;

"

let postambule : ('a, 'b, 'c) format = "
  module Out=OutputPaper.Output(Pdf);;
  let filename=\"%s.pdf\" in
  let rec resolve i env user fig=

     let env'={ env with user_positions=user; figure_names=fig }
     in
     let o=open_out \"doc\" in doc_graph o (fst (top !str)); close_out o;
     fixable:=false;
     let fig_params,params,compl,pars,figNames,figures=flatten env' (fst (top !str)) in
     let (_,pages,user')=TS.typeset
       ~completeLine:compl
       ~figure_parameters:fig_params
       ~figures:figures
       ~parameters:params
       ~badness:(Badness.badness pars)
       pars
     in
     if (user<>user') && !fixable  then (
       resolve (i+1) env' user' figNames
     ) else Out.output (fst (top !str)) pars figures env pages filename
  in
     resolve 0 defaultEnv defaultEnv.user_positions Binary.StrMap.empty
"

let no_ind = { up_right = None; up_left = None; down_right = None; down_left = None }

let split_ind indices =
  { no_ind with up_left = indices.up_left; down_left = indices.down_left; },
  { no_ind with up_right = indices.up_right; down_right = indices.down_right; }

let print_math ch display m =
  let style = if display then "Maths.Display" else "Maths.Text" in
  Printf.fprintf ch 
    "[B (fun env0 -> List.map (fun b -> Util.resize env0.size b) (let style = %s and _env = Maths.default.(Maths.int_of_style %s) in Maths.draw_maths Maths.default style ("
    style style;
  let rec fn indices ch m =
    match m with
      Var name | Num name ->
	let elt = "Maths.glyphs \""^name^"\"" in
	Printf.fprintf ch "[Maths.Ordinary %a ]" (hn elt) indices
      | Symbol name ->
	let elt = "Maths.symbol \""^name^"\"" in
	Printf.fprintf ch "[Maths.Ordinary %a ]" (hn elt) indices
    | Fun name ->
	let elt = "fun env -> Maths.glyphs \""^name^"\" { env with Maths.mathsFont=env0.font }" in
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
  in
  fn no_ind ch m;
  Printf.fprintf ch ")))] "

let rec print_macro ch op mtype name args =
  begin
    match mtype with
    | `Single -> 
      Printf.fprintf ch "%s" name;
      List.iter (function
        Paragraph p -> Printf.fprintf ch " %a" (print_contents op) p
      | Caml(s,e) ->
	let size = e - s in
	let buf=String.create size in
	let _= seek_in op s; input op buf 0 size in
	Printf.fprintf ch " (%s)" buf
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
            Paragraph p -> Printf.fprintf ch "arg%d = %a;;" !num (print_contents op) p;
	      incr num
	  | Caml(s,e) ->
	    let size = e - s in
	    let buf=String.create size in
	    let _= seek_in op s; input op buf 0 size in
	    Printf.fprintf ch "arg%d = %s;;" !num buf;
	    incr num
	  | _ -> assert false) args;
	  Printf.printf "end;;\n";
	  "(Args)"
	end
      in
      let modname = 
	if mtype = `Begin then begin
	  Printf.fprintf ch "module TEMP = struct\n";
	  "Env_"^name
	end
	else name
      in
      Printf.fprintf ch "open %s%s;;\n do_begin_%s()" modname end_open name
    | `End -> Printf.fprintf ch "do_end_%s();;\nend" name
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
    | FC m :: l ->
      Printf.fprintf ch "(";
      print_math ch false m;
      Printf.fprintf ch ")@";
      fn l
    end;
  in fn l;
  Printf.fprintf ch ")"

let _=
  let filename=ref [] in
    Arg.parse spec (fun x->filename:=x::(!filename)) "Usage :";
    try
      match !filename with
          []-> Printf.printf "no input files\n"
        | h::_->
            let op=open_in h in
            let lexbuf = Dyp.from_channel (Parser.pp ()) op in
            try
	      let docs = Parser.main lexbuf in
	      Printf.fprintf stderr "Fin du parsing (%d trees)\n" (List.length docs); flush stderr;
	      match docs with
	        [] -> assert false
	      | ((pre, docs), _) :: _  ->
		  Printf.printf "%s" preambule;
		  begin match pre with
		    None -> ()
		  | Some(title, at) -> 
		      Printf.printf "title %b %a;;\n\n" (at = None) (print_contents op) title;
		      match at with
			None -> ()
		      | Some(auth,inst) ->
			Printf.printf "author %b %a;;\n\n" (inst = None) (print_contents op) auth;
			  match inst with
			    None -> ()
			  | Some(inst) ->
			    Printf.printf "institute true %a;;\n\n" (print_contents op) inst
		  end;
		  let rec output_list lvl docs = 
		    match docs with
		      [] -> 
			for i = 0 to lvl - 1 do
			  Printf.printf "str:=up !str;;\n\n"
			done;
		    | doc::docs -> 
		      let lvl = ref lvl in 
		    (match doc with
		    | Paragraph p ->
		      Printf.printf "newPar ~environment:(fun x->x) textWidth parameters %a;;\n" 
			(print_contents op) p
		    | Caml(s,e) ->
		      let size = e - s in
		      let buf=String.create size in
		      let _= seek_in op s; input op buf 0 size in
		      Printf.printf "%s;;\n\n" buf
		    | Struct(title, numbered, docs) ->
		      let num = if numbered then "" else "'" in
		      (match docs with
			Relative docs ->
			  Printf.printf "newStruct%s %a;;\n\n" num (print_contents op) title;
			  output_list (!lvl + 1) docs;
			  Printf.printf "str:=up !str;;\n\n"
		      | Absolute l ->
			  if l > !lvl + 1 then failwith "Illegal level skip";
			  for i = 0 to !lvl - l do
			    Printf.printf "str:=up !str;;\n\n"
			  done;
			  Printf.printf "newStruct%s %a;;\n\n" num (print_contents op) title;
			  lvl := l
		      )
		    | Macro(mtype, name, args) ->
		      print_macro stdout op mtype name args;
		      Printf.printf ";;\n\n" 
		    | Math m ->
		      Printf.printf "newPar ~environment:(fun x->{x with par_indent = []}) textWidth center %a;;\n" 
		        (fun ch -> print_math ch true) m
                    | Ignore -> ()
		    | Verbatim(lang, lines) ->
		      Printf.printf "module VERB = struct\n\n";
		      Printf.printf "let verbEnv x = { (envFamily defaultMono x)
                                                     with par_indent = [] };;\n\n";
		      let lang = match lang with
			  None -> "T"
			 | Some s -> s
		      in
		      List.iter (fun l ->
			Printf.printf
			  "newPar ~environment:verbEnv (C.normal 1e100) ragged_left (lang_%s \"%s\");;\n"
			  lang l)
			lines;
		      Printf.printf "end;;\n\n");
		    output_list !lvl docs
		  in
		  output_list 0 docs;
		  close_in op;
		  Printf.printf postambule (Filename.chop_extension h)
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
