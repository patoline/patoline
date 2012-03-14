open Drivers
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
  open Drivers
  open DefaultFormat;;

"

let postambule : ('a, 'b, 'c) format = "
  let gr=open_out \"doc_graph\" in
    doc_graph gr !str;
    close_out gr;

  let params,compl,pars,figures=flatten defaultEnv !str in
  let (_,pages)=Typeset.typeset
    ~completeLine:compl
    ~parameters:params
    ~badness:(Badness.badness pars)
    ~figures:figures
    pars
  in
  let u,v=Output.routine pars figures defaultEnv pages in
    Drivers.Pdf.output ~structure:(make_struct v !str) u \"%s.pdf\" 
" 

let print_math ch display m =
  let style = if display then "Maths.Display" else "Maths.Text" in
  Printf.fprintf ch 
    "(List.map (fun b -> B (fun env -> Util.resize env.size b)) (let env = Maths.default and style = %s in Maths.draw_maths env style ("
    style;
  let no_ind = { up_right = None; up_left = None; down_right = None; down_left = None } in
  let rec fn indices ch m =
    match m with
      Var name | Num name ->
	Printf.fprintf ch "[Maths.Ordinary  { (Maths.noad (Maths.gl env style \"%s\")) with %a } ]"
	  name hn indices
    | Indices(ind', m) ->
      fn ind' ch m 
    | _ -> 
      Printf.fprintf ch "[]"
  and gn ch name ind =
    match ind with 
      None ->
      Printf.fprintf ch "%s = [];" name
    | Some m ->
      Printf.fprintf ch "%s = (%a);" name (fn no_ind) m
  and hn ch ind =
    gn ch "Maths.superscript_right" ind.up_right;
    gn ch "Maths.superscript_left" ind.up_left;
    gn ch "Maths.subscript_right" ind.down_right;
    gn ch "Maths.subscript_left" ind.down_left;
  in
  fn no_ind ch m;
  Printf.fprintf ch "))) "

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
    | TC s :: ((_::_) as l) -> 
      Printf.fprintf ch "(T \"%s\")::(B (fun env -> env.stdGlue))::" (String.escaped s);
      fn l
    | TC s :: [] -> 
      Printf.fprintf ch "[T \"%s\"]" (String.escaped s);
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
	      match docs with
	        [] | (_::_::_) -> assert false
	      | [(pre, docs), _] ->
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
		  let rec output_list docs = List.iter output_doc docs
		  and output_doc = function
		    | Paragraph p ->
		      Printf.printf "newPar ~environment:defaultEnv textWidth parameters %a;;\n" 
			(print_contents op) p
		    | Caml(s,e) ->
		      let size = e - s in
		      let buf=String.create size in
		      let _= seek_in op s; input op buf 0 size in
		      Printf.printf "%s;;\n\n" buf
		    | Struct(title, docs) ->
		      Printf.printf "newStruct %a;;\n\n" (print_contents op) title;
		      output_list docs;
		      Printf.printf "up();;\n\n"
		    | Macro(mtype, name, args) ->
		      print_macro stdout op mtype name args;
		      Printf.printf ";;\n\n" 
		    | Math m ->
		      Printf.printf "newPar ~environment:{defaultEnv with par_indent = []} textWidth center %a;;\n" 
		        (fun ch -> print_math ch true) m
		    | Verbatim(lang, lines) ->
		      Printf.printf "module VERB = struct\n\n";
		      Printf.printf "let verbEnv = { (envFamily defaultMono defaultEnv)
                                                     with par_indent = [] };;\n\n";
		      let lang = match lang with
			  None -> "T"
			 | Some s -> s
		      in
		      List.iter (fun l ->
			Printf.printf
			  "newPar ~environment:verbEnv (normal 1e100) ragged_left (lang_%s \"%s\");;\n"
			  lang l)
			lines;
		      Printf.printf "end;;\n\n"
		      
		  in
		  output_list docs;
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
