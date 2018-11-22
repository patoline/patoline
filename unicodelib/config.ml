let unicode_data_file =
  let share =
    try
      let (ic, _, _) as cs =
        let env = Unix.environment () in
        Unix.open_process_full "opam var share" env
      in
      let res = input_line ic in
      ignore (Unix.close_process_full cs);
      res
    with _ -> "/usr/local/share"
  in
  Filename.concat share "patoline/unicode/unicode.data"
