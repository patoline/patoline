open Drivers
open Binary
open Constants
open Lexing
open Util
open Fonts
open Fonts.FTypes


let spec = []

exception Syntax_Error of Lexing.position * string

module KernMap=Map.Make (struct type t=(string*int*float) let compare=compare end)


let kern_cache_:((float*float) KernMap.t ref)=ref KernMap.empty
let eyeWidth=1.
let kern_cache name gl size=0.,0.
  (* try *)
  (*   KernMap.find (name,(glyphNumber gl).glyph_index,size) !kern_cache_ *)
  (* with *)
  (*     Not_found ->( *)
  (*       let d0=1000. *. size *. size in *)
  (*       let xg=(find_left_density (outlines gl) (glyphWidth gl) (fun y -> exp (-.(500. -. float_of_int y)**2.)/.1000.) d0) in *)
  (*       let xd=(find_right_density (outlines gl) (glyphWidth gl) (fun y -> exp (-.(500. -. float_of_int y)**2.)/.1000.) d0) in *)
  (*       let wg=size*.xg/.1000. in *)
  (*       let wd=size*.(glyphWidth gl-.xd)/.1000. in *)
  (*         kern_cache_:= KernMap.add (name,(glyphNumber gl).glyph_index,size) (wg,wd) !kern_cache_; *)
  (*         (wg,wd) *)
  (*     ) *)

let measure line=150.(* if line.height<20 then 150. else 100. *)
let default paragraphs figures last_parameters line _ _=
  let left_protr=try
    match first_line paragraphs line with
        GlyphBox (s,gl) -> fst (kern_cache (fontName (glyphFont gl.glyph)) gl.glyph s)
      | b -> 0.
  with _-> 0.
  in
  let right_protr=try
    match last_line paragraphs line with
        GlyphBox (s,gl) -> snd (kern_cache (fontName (glyphFont gl.glyph)) gl.glyph s)
      | _ -> 0.
  with _-> 0.
  in
    fold_left_line paragraphs
      (fun a b->match b with
           Parameters p -> p a
         | _ -> a)
      { format=a4; lead=5.;
        measure=measure line+.right_protr+.left_protr;
        lines_by_page=
          if line.page_height <= 0 then 48 else last_parameters.lines_by_page;
        left_margin=
          (if line.isFigure then (
             20.+.(measure line -. (figures.(line.lastFigure).drawing_max_width +. figures.(line.lastFigure).drawing_min_width)/.2.)/.2.
           ) else 20.) -. left_protr;
        local_optimization=0;
        allow_widows=false;
        allow_orphans=false
      }
      line

let figure=
  { drawing_min_width=50.; drawing_max_width=50.; drawing_y0= 0.; drawing_y1= 50.; drawing_contents=[] }
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
              let badness=Badness.badness paragraphs [||] [||] in
              let figures=[||] in
              let log,pages=Boxes.lineBreak
                ~measure:measure
                ~parameters:(default paragraphs figures)
                ~figures:figures
                ~badness:badness paragraphs in
                List.iter (function
                               Overfull_line h->(Printf.printf "Overfull line : "; print_text_line paragraphs h)
                             | Widow h->(Printf.printf "Widow : "; print_text_line paragraphs h)
                             | Orphan h->(Printf.printf "Orphan : "; print_text_line paragraphs h)
                          ) log;
                flush stdout;
                Pdf.output (Output.routine paragraphs figures pages) (Pdf.filename h)
    with
        Syntax_Error(pos,msg) ->
	  Printf.printf "%s:%d,%d %s\n" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg
