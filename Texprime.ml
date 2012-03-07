open Drivers
open Binary
open Constants
open Lexing
open Util
open Fonts
open Fonts.FTypes
open Parser

exception Syntax_Error of Lexing.position * string

let spec = []

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
	      close_in op;
	      match docs with
		[(pre, doc), _] ->
		  begin match pre with
		    None -> Printf.printf "open Typography\nopen Parameters\nopen Fonts.FTypes\nopen Util\nopen Fonts\nopen Drivers\nopen DefaultFormat;;\n\n"
		  | Some pre -> Printf.printf "%s\n\n" pre
		  end;
		  List.iter (fun p ->
		    Printf.printf "newPar textWidth parameters [T \"%s\"];;\n"  (String.escaped p))
	
		    doc;
		  Printf.printf "
  let gr=open_out \"doc_graph\" in
    doc_graph gr !str;
    close_out gr;

  let params,compl,pars=flatten defaultEnv !str in
  let (_,pages)=Typeset.typeset
    ~completeLine:compl
    ~parameters:params
    ~badness:(Badness.badness pars)
    pars
  in
  let u,v=Output.routine pars [||] defaultEnv pages in
    Drivers.Pdf.output ~structure:(make_struct v !str) u \"%s.pdf\"
" (Filename.chop_extension h)
	      | _ -> failwith "Ambiguous document"
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
	  Printf.printf "%s:%d,%d %s\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg
