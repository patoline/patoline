open Drivers
open Binary
open Boxes
open Constants
open Lexing 

module UTF8=Batteries.UTF8
module UChar=Batteries.UChar
module DynArray=Batteries.DynArray


module M=Output.Routine(Pdf)

let spec = []

exception Syntax_Error of Lexing.position * string

let _=
  let filename=ref [] in
    Arg.parse spec (fun x->filename:=x::(!filename)) "Usage :";    
    try 
      match !filename with
        []->Printf.printf "no input files\n"
      | h::_->
        let op=open_in h in
	let lexbuf = Dyp.from_channel (Parser.pp ()) op in
	let text = 
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
        let pages=lineBreak (fst (List.hd text)) in
        M.output_routine h pages
    with
      Syntax_Error(pos,msg) ->
	Printf.printf "%s:%d,%d %s\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg

