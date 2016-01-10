open Decap

(* blank_regexp will automatically accept many newlines, so we can't use it to accept
   at most one .*)
let blank1 str pos =
  let rec fn got_newline str pos =
    let c,str',pos' = Input.read str pos in
    if c = ' ' || c = '\t' || c = '\r' then fn got_newline str' pos'
    else if c = '\n' && not got_newline then fn true str' pos'
    else str, pos
  in fn false str pos

let blank2 = blank_regexp ''[ \n\t\r]*''

let paragraph =
  change_layout
    (parser
      l:{w:''[^ \t\r\n]+''[let w = groupe 0 in fun l -> w::l]}+[[]] -> List.rev l)
  blank1

let text =
    parser
      ll:{l:paragraph -> fun ll -> l::ll}*[[]] EOF -> List.rev ll

let _ = handle_exception (fun () ->
			  let l = parse_channel text blank2 stdin in
			  Printf.printf "=> %d paragraphs\n" (List.length l))
			 ()
