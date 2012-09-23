
let spec = []

let files = ref []

let _ = 
  Arg.parse spec (fun x->files := x::(!files)) "Usage :";
  match !files with
    [f] ->
      let ch = open_in f in
      let pages = input_value ch in
      close_in ch;
      GL2.output pages f
  | _ ->
    Printf.fprintf stderr "%s: more than one file given!" Sys.argv.(0)



