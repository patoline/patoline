let parser pattern = ''[^- \t\n\r{}]+''
let parser hyphenation = x:pattern xs:{"-" pattern}* -> x::xs
let hyphenation = Earley.change_layout hyphenation Earley.no_blank

let parser patt = "\\patterns" "{" pattern* "}"
let parser hyph = "\\hyphenation" "{" hyphenation* "}"

let parser cesure_file =
  | ps:patt hy:hyph?[[]] -> (ps, hy)
  | hy:hyph ps:patt?[[]] -> (ps, hy)

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
  let parse = Earley.parse_file cesure_file blank in
  Earley.handle_exception parse fn

let build_cesure_file fn =
  let (pa, hy) = parse_file fn in
  let tree = List.fold_left Hyphen.insert Hyphen.empty pa in
  let tree = List.fold_left Hyphen.insert_exception tree hy in
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
