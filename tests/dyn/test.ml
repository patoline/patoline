let _=
  Build.macros:=
    Util.StrMap.add "diagram" (fun x->
      "[bB (fun env -> \n" ^
        "let module Res = struct\n "^
        "module Lib = Env_Diagram (struct let env = env end) \n open Lib \n"^
        x^
        "\n end \n"^
        "in [ Drawing (Res.Lib.make ()) ])]\n") !Build.macros;

  Printf.fprintf stderr "blabla\n";flush stderr
