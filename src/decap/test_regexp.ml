open Regexp

let _ =
  let re = Sys.argv.(2) in
  let usage () =
    Printf.eprintf "Usage: %s [str|decap] [regexp]\n" Sys.argv.(0);
    exit 1
  in
  let parser_re =
    match Sys.argv.(1) with
    | "decap" -> regexp re
    | "str"   -> Decap.regexp re (fun f -> f 0)
    | _       -> usage ()
  in
  let parse_r = Decap.parse_string parser_re Decap.no_blank in
  let parse = Decap.handle_exception parse_r in
  while true do
    try
      let input = read_line () in
      Printf.fprintf stdout "Parsed: %S\n" (parse input)
    with End_of_file -> exit 0
  done
