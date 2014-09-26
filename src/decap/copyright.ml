type comment_kind = None
                  | Single of string
                  | Multi of string * string

let comment = ref None

let files = ref []

let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [option] [LICENCE] [FILE1] [FILE2] ...\n"

let anon_fun s = files := !files @ [s]

let spec =
  [ "-bash", Arg.Unit (fun () -> comment := Single "#"), "Sets comment mode to bash (# ...)."
  ; "-caml", Arg.Unit (fun () -> comment := Multi ("(*", "*)")), "Sets comment mode to ML ((* ... *)."
  ; "-none", Arg.Unit (fun () -> comment := None), "No comment mode (default)."
  ; "--help", Arg.Unit (fun () -> output_string stderr usage_msg; exit 0), "Show usage message." ]

let _ = Arg.parse spec anon_fun usage_msg

let (licence_file, files) = match !files with
                            | l :: f :: fs -> (l, f :: fs)
                            | _            -> assert false

let read_lines filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    while true do
      let l = input_line ic in
      lines := l :: !lines;
    done;
    []
  with
  | End_of_file -> close_in ic; List.rev !lines

let read_file filename =
  let ls = read_lines filename in
  String.concat "\n" ls

let raw_notice = read_lines licence_file

let header =
  let ls = match !comment with
           | None          -> raw_notice
           | Single c      -> List.map (fun l -> if l = "" then c else (c ^ " " ^ l)) raw_notice
           | Multi (lc,rc) -> lc :: (List.map (fun l -> if l = "" then "" else ("  " ^ l)) raw_notice) @ [rc]
  in (String.concat "\n" ls) ^ "\n\n"

let add_copyright file =
  let content = read_file file in
  let oc = open_out file in
  output_string oc (header ^ content);
  close_out oc

let _ =
  List.map add_copyright files
