open Earley_core
let pattern = Earley_core.Earley.declare_grammar "pattern"
let _ =
  Earley_core.Earley.set_grammar pattern
    (Earley_str.regexp ~name:"[^- \\t\\n\\r{}]+" "[^- \t\n\r{}]+"
       (fun group -> group 0))
let hyphenation = Earley_core.Earley.declare_grammar "hyphenation"
let _ =
  Earley_core.Earley.set_grammar hyphenation
    (Earley_core.Earley.fsequence pattern
       (Earley_core.Earley.fsequence
          (Earley_core.Earley.apply (fun f -> f [])
             (Earley_core.Earley.fixpoint' (fun l -> l)
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string "-" "-")
                   (Earley_core.Earley.fsequence pattern
                      (Earley_core.Earley.empty
                         (fun _default_0 -> _default_0))))
                (fun x -> fun f -> fun l -> f (List.cons x l))))
          (Earley_core.Earley.empty (fun xs -> fun x -> x :: xs))))
let hyphenation = Earley.change_layout hyphenation Earley.no_blank
let patt = Earley_core.Earley.declare_grammar "patt"
let _ =
  Earley_core.Earley.set_grammar patt
    (Earley_core.Earley.fsequence_ignore
       (Earley_core.Earley.string "\\patterns" "\\patterns")
       (Earley_core.Earley.fsequence_ignore
          (Earley_core.Earley.string "{" "{")
          (Earley_core.Earley.fsequence
             (Earley_core.Earley.apply (fun f -> f [])
                (Earley_core.Earley.fixpoint' (fun l -> l) pattern
                   (fun x -> fun f -> fun l -> f (List.cons x l))))
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.string "}" "}")
                (Earley_core.Earley.empty (fun _default_0 -> _default_0))))))
let hyph = Earley_core.Earley.declare_grammar "hyph"
let _ =
  Earley_core.Earley.set_grammar hyph
    (Earley_core.Earley.fsequence_ignore
       (Earley_core.Earley.string "\\hyphenation" "\\hyphenation")
       (Earley_core.Earley.fsequence_ignore
          (Earley_core.Earley.string "{" "{")
          (Earley_core.Earley.fsequence
             (Earley_core.Earley.apply (fun f -> f [])
                (Earley_core.Earley.fixpoint' (fun l -> l) hyphenation
                   (fun x -> fun f -> fun l -> f (List.cons x l))))
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.string "}" "}")
                (Earley_core.Earley.empty (fun _default_0 -> _default_0))))))
let cesure_file = Earley_core.Earley.declare_grammar "cesure_file"
let _ =
  Earley_core.Earley.set_grammar cesure_file
    (Earley_core.Earley.alternatives
       (List.cons
          (Earley_core.Earley.fsequence hyph
             (Earley_core.Earley.fsequence
                (Earley_core.Earley.option [] patt)
                (Earley_core.Earley.empty (fun ps -> fun hy -> (ps, hy)))))
          (List.cons
             (Earley_core.Earley.fsequence patt
                (Earley_core.Earley.fsequence
                   (Earley_core.Earley.option [] hyph)
                   (Earley_core.Earley.empty (fun hy -> fun ps -> (ps, hy)))))
             [])))
let blank str pos =
  let rec fn state ((str, pos) as cur) =
    let (c, str', pos') = Input.read str pos in
    let next = (str', pos') in
    match (state, c) with
    | (`Ini, (' '|'\t'|'\r'|'\n')) -> fn `Ini next
    | (`Ini, '%') -> fn `Com next
    | (`Com, '\n') -> fn `Ini next
    | (`Com, _) -> fn `Com next
    | (_, _) -> cur in
  fn `Ini (str, pos)
let parse_file fn =
  let parse = Earley.parse_file cesure_file blank in
  Earley.handle_exception parse fn
let build_cesure_file fn =
  let (pa, hy) = parse_file fn in
  let tree = List.fold_left Hyphen.insert Hyphen.empty pa in
  let tree = List.fold_left Hyphen.insert_exception tree hy in
  let fn' = (try Filename.chop_extension fn with | _ -> fn) ^ ".hdict" in
  let o = open_out_bin fn' in output_value o tree; close_out o
let _ =
  if (Array.length Sys.argv) > 1
  then
    for i = 1 to (Array.length Sys.argv) - 1 do
      build_cesure_file (Sys.argv.(i))
    done
  else Printf.eprintf "Usage: %s [File] ... [File]\n%!" (Sys.argv.(0))
