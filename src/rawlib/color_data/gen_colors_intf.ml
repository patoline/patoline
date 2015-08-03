let _ =
  if Array.length Sys.argv <> 2 then
    failwith "This program expects one argument.";

  let datafile = open_in Sys.argv.(1) in

  try while true do
    let line = input_line datafile in
    let print n _ _ _ = Printf.printf "val %s : color\n%!" n in
    Scanf.sscanf line "%s #%2X%2X%2X" print
  done with End_of_file -> close_in datafile
