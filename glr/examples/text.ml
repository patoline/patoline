open Glr

let blank1 = blank_regexp (Str.regexp "[ \t\r]*\\([\n][ \t\r]*\\)?")
let blank2 = blank_regexp (Str.regexp "[ \n\t\r]*")

let word_re = "[^ \t\r\n]+"

let paragraph =
  change_layout
    glr
      l:{w:RE(word_re) -> w}++
    end 
  blank1

let text =
    glr
      ll:{l:paragraph -> l}* EOF
    end

let _ =
      try
	let l = parse_channel text blank2 stdin in
	Printf.printf "=> %d paragraphs\n" (List.length l)
      with
	Parse_error n -> Printf.fprintf stderr "Parse error after char %d\n%!" n
      | Ambiguity(n,p) -> Printf.fprintf stderr "Ambiguous expression from %d to %d\n%!" n p

