let _ =
  if Array.length Sys.argv <> 2 then
    failwith "This program expects one argument.";

  let datafile = open_in Sys.argv.(1) in

  try while true do
    let line = input_line datafile in
    let print n r g b =
      let r = float_of_int r /. 255.0 in
      let g = float_of_int g /. 255.0 in
      let b = float_of_int b /. 255.0 in
      Printf.printf "let %s = rgb %f %f %f\n%!" n r g b
    in
    Scanf.sscanf line "%s #%2X%2X%2X" print
  done with End_of_file -> close_in datafile
