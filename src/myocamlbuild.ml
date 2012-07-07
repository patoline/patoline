open Ocamlbuild_plugin
(* open Command -- no longer needed for OCaml >= 3.10.2 *)

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let split s ch =
  let x = ref [] in
  let rec go s =
    let pos = String.index s ch in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try
    go s
  with Not_found -> !x

let split_nl s = split s '\n'

let before_space s =
  try
    String.before s (String.index s ' ')
  with Not_found -> s

(* this lists all supported packages *)
let find_packages () =
  List.map before_space (split_nl & run_and_read "ocamlfind list")

(* this is supposed to list available syntaxes, but I don't know how to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

let _ = dispatch begin function
  | Before_options ->
       (* by using Before_options one let command line options have an higher priority *)
       (* on the contrary using After_options will guarantee to have the higher priority *)

       (* override default commands by ocamlfind ones *)
       Options.ocamlc     := ocamlfind & A"ocamlc";
       Options.ocamlopt   := ocamlfind & A"ocamlopt";
       Options.ocamldep   := ocamlfind & A"ocamldep";
       Options.ocamldoc   := ocamlfind & A"ocamldoc";
       Options.ocamlmktop := ocamlfind & A"ocamlmktop"

  | After_rules ->
      ocaml_lib "Typography/Typography";

      (* When one link an OCaml library/binary/package, one should use -linkpkg *)
      flag ["ocaml"; "link"; "program"] & A"-linkpkg";

      flag ["ocaml"; "compile";  "Typography"] & S[A"-I"; A"Typography"];
      flag ["ocaml"; "ocamldep"; "Typography"] & S[A"-I"; A"Typography"];
      flag ["ocaml"; "doc";      "Typography"] & S[A"-I"; A"Typography"];
      flag ["ocaml"; "link";     "Typography"] & S[A"-I"; A"Typography"];
      flag ["ocaml"; "infer_interface"; "Typography"] & S[A"-I"; A"Typography"];

      flag ["ocaml"; "compile"; "rectypes"] & S[A"-rectypes"];

      (* For each ocamlfind package one inject the -package option when
       * compiling, computing dependencies, generating documentation and
       * * linking. *)
      List.iter begin fun pkg ->
        flag ["ocaml"; "compile";  "package("^pkg^")"] & S[A"-package"; A pkg];
        flag ["ocaml"; "ocamldep"; "package("^pkg^")"] & S[A"-package"; A pkg];
        flag ["ocaml"; "doc";      "package("^pkg^")"] & S[A"-package"; A pkg];
        flag ["ocaml"; "link";     "package("^pkg^")"] & S[A"-package"; A pkg];
        flag ["ocaml"; "infer_interface"; "package("^pkg^")"] & S[A"-package"; A pkg];
      end (find_packages ());

      (* Like -package but for extensions syntax. Morover -syntax is useless
       * when linking. *)
      List.iter begin fun syntax ->
        flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
        flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
      end (find_syntaxes ());

      rule "dypgen:dyp->ml"
	~prods:["%.ml"]
	~deps:["%.dyp"]
	begin fun env _->
          Seq[Cmd(S([A"dypgen";A"--no-mli";A"--merge-warning";P(env "%.dyp")]))]
	end;

      rule "patoline:tml->ml"
	~prods:["%.ml"]
	~deps:["%.tml"]
	(* FIXME: --no-grammar should be computed automatically *)
	begin fun env _->
          Seq[Cmd(S([A"mv";P(env "%.tml");P(env "%.ml")]))]
	end;	

      rule "patoline:txp->tml"
	~prods:["%.tml";"%.tgx"]
	~deps:["%.txp";]
	(* FIXME: --no-grammar should be computed automatically *)
	begin fun env _->
          Seq[Cmd(S([A"./Patoline/Main.native";A"-c";A"--ml";A"--no-grammar";P(env "%.txp")]))]
	end;

  | _ -> ()
end
