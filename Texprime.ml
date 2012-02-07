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
    { format=a4; lead=5.; measure=150.; lines_by_page=l; left_margin=20. }


let bacon="Bacon ipsum dolor sit amet ut bacon deserunt, eu pancetta aliqua ham hock sed pig pastrami elit et. Ribeye qui cillum sirloin, reprehenderit pork chop aliqua. In pariatur laborum est chuck in, et commodo culpa excepteur tri-tip tenderloin. Occaecat meatball proident, labore ground round salami in sed beef ribs officia. Spare ribs qui sausage, beef et beef ribs strip steak leberkase."

let figure=
  { drawing_x0= 0.; drawing_x1= 50.; drawing_y0= 0.; drawing_y1= 50.; drawing_contents=[] }
    (* let lexbuf=Dyp.from_string (Parser.pp ()) bacon in *)
    (* let text=Parser.main lexbuf in *)
    (* let parsed=fst (List.hd text) in *)
    (* let _,pages=Boxes.lineBreak *)
    (*   (fun _->{format=a4; lead=5.; measure=50.; lines_by_page=max_int; left_margin=0. }) *)
    (*   (Array.of_list (List.map (Array.of_list) parsed)) *)
    (* in *)
    (*   (Output.drawings_of_pages pages).(0) *)


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
              let log,pages=Boxes.lineBreak default ~figures:[|figure|] paragraphs in
                (* List.iter (function *)
                (*                Overfull_line h->(Printf.printf "Overfull line : "; print_text_line paragraphs h) *)
                (*              | Widow h->(Printf.printf "Widow : "; print_text_line paragraphs h) *)
                (*              | Orphan h->(Printf.printf "Orphan : "; print_text_line paragraphs h) *)
                (*           ) log; *)
                (* flush stdout; *)
                M.output_routine h paragraphs pages
    with
        Syntax_Error(pos,msg) ->
	  Printf.printf "%s:%d,%d %s\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg
