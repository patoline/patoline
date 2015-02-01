type kind = Quick | Normal | Full
let kind = ref Normal

let test_cases (a, b, c) =
  match !kind with Quick -> a | Normal -> b | Full -> c

let spec = [ ("--quick", Arg.Unit (fun () -> kind := Quick),
	      "quick tests");
	     ("--normal", Arg.Unit (fun () -> kind := Normal),
	      "normal tests");
	     ("--full", Arg.Unit (fun () -> kind := Full),
	      "full tests (very long)"); ]

let _  = Arg.parse spec
		   (fun _ -> raise (Arg.Bad "extra arguments"))
		   "run unit tests on decap combinators"
