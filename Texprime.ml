open Drivers
open Binary
open Constants
open Lexing
open Util

module M=Output.Routine(Pdf)

let spec = []

exception Syntax_Error of Lexing.position * string

let default line=
  let l=38 in
    { format=a4; lead=5.; measure=70.+.50.*.(float_of_int ((line.height+1) mod l))/.(float_of_int l); lines_by_page=l; left_margin=20. }

let _=
  let filename=ref [] in
    Arg.parse spec (fun x->filename:=x::(!filename)) "Usage :";
    try
      match !filename with
          []->Printf.printf "no input files\n"
        | h::_->
            let op=open_in h in
            let lexbuf = Dyp.from_channel (Parser.pp ()) op in
            let text=
              try
	        Parser.main lexbuf
	      with
	        | Dyp.Syntax_error ->
	            raise
	              (Syntax_Error (Dyp.lexeme_start_p lexbuf,
			             "parsing error"))
	        | Failure("lexing: empty token") ->
	            raise
	              (Syntax_Error (Dyp.lexeme_start_p lexbuf,
			             "unexpected char"))
	    in
	      if List.length text > 1 then
	        raise (Failure "detecting parsing ambiguities, please report");
              let parsed=fst (List.hd text) in
              let paragraphs=Array.of_list (List.map (Array.of_list) parsed) in
              let log,pages=Boxes.lineBreak default paragraphs in
                List.iter (function
                               Overfull_line h->(Printf.printf "Overfull line : "; print_text_line paragraphs h)
                          ) log;
                M.output_routine h pages
    with
        Syntax_Error(pos,msg) ->
	  Printf.printf "%s:%d,%d %s\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg
