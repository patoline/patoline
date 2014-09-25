open Parsing

let _ =
  if Unix.((fstat (descr_of_in_channel Pervasives.stdin)).st_kind = S_REG)
  then
      try
	let lexbuf = Lexing.from_channel stdin in
	let x = Parser.main Lexer.token lexbuf in
	Printf.printf "=> %f\n" x
      with
	Parse_error -> Printf.fprintf stderr "Parse error\n%!"
  else
    try
      while true do
	try
	  Printf.printf ">> %!";
	  let line = input_line stdin in
	  let lexbuf = Lexing.from_string line in
	  let x = Parser.main Lexer.token lexbuf in
	  Printf.printf "=> %f\n%!" x
	with
	  Parse_error -> Printf.fprintf stderr "Parse error\n%!"

      done
    with End_of_file -> ()


