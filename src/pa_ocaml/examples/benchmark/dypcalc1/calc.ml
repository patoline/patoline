
(*let pp = Calc_parser.pp ()
let lexbuf = Dyp.from_channel pp stdin*)
(*let lexbuf = Dyp.from_channel (Calc_parser.pp ()) stdin*)

let _ =
  try
      ((*Dyp.flush_input lexbuf;*)
      try
        (*let pf = Calc_parser.main lexbuf in*)
        let pf =
          Calc_parser.main
          (Dyp.from_channel (Calc_parser.pp ()) stdin)
        in
        Printf.printf "= %f\n\n" (fst (List.hd pf))
      with
        Dyp.Syntax_error -> Printf.printf "Syntax error\n\n"
      );
      flush stdout
  with Failure _ -> exit 0
