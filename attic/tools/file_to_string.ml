let filename = Sys.argv.(1)

let ic = open_in filename

let load_file ic =
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let s = load_file ic

let name = Filename.chop_extension (Filename.basename filename)

let _ = Printf.printf "let _%s_js = %S\n" name s
