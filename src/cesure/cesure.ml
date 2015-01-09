open Typography.Hyphenate

let group name = parser | "\\" - STR(name) "{" ''[^ \t\n\r{}]+''* "}"
let patt = group "patterns"
let hyph = group "hyphenation"

let parser cesure_file =
  | ps:patt?[[]] hy:hyph?[[]] -> (ps, hy)
  | hy:hyph?[[]] ps:patt?[[]] -> (ps, hy)

let blank str pos =
  let rec fn state prev ((str, pos) as cur) =
    let (c, str', pos') = Input.read str pos in 
    let next = (str', pos') in
    match state, c with
    | `Ini , (' '|'\t'|'\r'|'\n') -> fn `Ini cur next
    | `Ini , '%'                  -> fn `Com cur next
    | `Com , '\n'                 -> fn `Ini cur next
    | `Com , _                    -> fn `Com cur next
    | _    , _                    -> cur
  in fn `Ini (str, pos) (str, pos)

let parse_file fn =
  let parse = Decap.parse_file cesure_file blank in
  Decap.handle_exception parse fn

let split c s =
  let rec split s acc =
    try
      let i = String.index s c in
      let e = String.sub s 0 i in
      let s' = String.sub s (i+1) (String.length s - i - 1) in
      split s' (e :: acc)
    with _ -> List.rev (s :: acc)
  in
  split s []

let build_cesure_file fn =
  let (pa, hy) = parse_file fn in
  let hy = List.map (split '-') hy in
  let tree = List.fold_left insert empty pa in
  let tree = List.fold_left insert_exception tree hy in
  let fn' = (try Filename.chop_extension fn with _ -> fn) ^ ".hdict" in
  let o = open_out fn' in
  output_value o tree;
  close_out o

let _ =
  if Array.length Sys.argv > 1 then
    for i = 1 to Array.length Sys.argv - 1 do
      build_cesure_file Sys.argv.(i)
    done
  else
    Printf.eprintf "Usage: %s [File] ... [File]\n%!" Sys.argv.(0)
