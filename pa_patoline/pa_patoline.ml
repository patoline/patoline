open Earley_core
open Unicodelib
open Patconfig
open Patutil.Extra
open Earley
open Pa_ocaml_prelude
let _ = Printexc.record_backtrace true; Sys.catch_break true
let patoline_format = ref "DefaultFormat"
let patoline_driver = ref "Pdf"
let patoline_packages = ref ["Typography"]
let patoline_grammar = ref ["DefaultGrammar"]
let debug = ref false
let is_main = ref false
let set_patoline_format f = patoline_format := f
let set_patoline_driver d = patoline_driver := d
let add_patoline_packages ps =
  let ps = String.split_on_char ',' ps in
  patoline_packages := ((!patoline_packages) @ ps)
let no_default_grammar = ref false
let add_patoline_grammar g =
  let g = try Filename.chop_extension g with | _ -> g in
  patoline_grammar := (g :: (!patoline_grammar))
let in_ocamldep = ref false
let build_dir = ref "."
let set_build_dir d = build_dir := d
let quail_out_name = ref ""
let quail_ch =
  Lazy.from_fun (fun () -> let ch = open_out_bin (!quail_out_name) in ch)
let quail_out mnames unames =
  if (!quail_out_name) <> ""
  then
    let (unames, others) =
      List.partition (fun s -> (UTF8.validate s) && ((UTF8.length s) = 1))
        unames in
    match unames with
    | [] -> ()
    | u::_ ->
        (List.iter
           (fun name ->
              Printf.fprintf (Lazy.force quail_ch) "(\"%s\" ?%s)\n"
                (String.escaped name) u) mnames;
         List.iter
           (fun name ->
              Printf.fprintf (Lazy.force quail_ch) "(\"%s\" ?%s)\n"
                (String.escaped name) u) others)
let extra_spec =
  [("--driver", (Arg.String set_patoline_driver),
     "The driver against which to compile.");
  ("--format", (Arg.String set_patoline_format),
    "The document format to use.");
  ("--package", (Arg.String add_patoline_packages), "Package to link.");
  ("--no-default-grammar", (Arg.Set no_default_grammar),
    "do not load DefaultGrammar");
  ("--grammar", (Arg.String add_patoline_grammar),
    "load the given grammar file.");
  ("--ocamldep", (Arg.Set in_ocamldep),
    "set a flag to inform parser that we are computing dependencies");
  ("--quail-out", (Arg.Set_string quail_out_name),
    "set a filename to output quail.el like file for emacs short cur");
  ("--debug-patoline", (Arg.Set debug),
    "turn on debuging mode for pa_patoline.");
  ("--main", (Arg.Set is_main), "generate a main file.");
  ("--build-dir", (Arg.String set_build_dir), "Change the build directory.")]
module Ext(In:Extension) =
  struct
    include In
    open Pa_patoline_ast_helpers
    open Longident
    let spec = extra_spec @ spec
    let blank_sline buf pos =
      let open Pa_lexing in
        let ocamldoc = ref false in
        let ocamldoc_buf = Buffer.create 1024 in
        let rec fn state stack prev curr nl =
          let (buf, pos) = curr in
          let (c, buf', pos') = Input.read buf pos in
          if !ocamldoc then Buffer.add_char ocamldoc_buf c;
          (let next = (buf', pos') in
           match (state, stack, c) with
           | (`Ini, [], ' ')|(`Ini, [], '\t')|(`Ini, [], '\r') ->
               fn `Ini stack curr next nl
           | (`Ini, [], '\n') ->
               if not nl then curr else fn `Ini stack curr next false
           | (`Ini, _, '(') -> fn (`Opn curr) stack curr next nl
           | (`Ini, [], _) -> curr
           | (`Opn p, _, '*') ->
               let nl = true in
               if stack = []
               then
                 let (c, buf', pos') = Input.read buf' pos' in
                 let (c', _, _) = Input.read buf' pos' in
                 (if (c = '*') && (c' <> '*')
                  then
                    (ocamldoc := true;
                     fn `Ini (p :: stack) curr (buf', pos') nl)
                  else fn `Ini (p :: stack) curr next nl)
               else fn `Ini (p :: stack) curr next nl
           | (`Opn _, _::_, '"') -> fn (`Str curr) stack curr next nl
           | (`Opn _, _::_, '{') -> fn (`SOp ([], curr)) stack curr next nl
           | (`Opn _, [], _) -> prev
           | (`Opn _, _, _) -> fn `Ini stack curr next nl
           | (`Ini, _::_, '"') -> fn (`Str curr) stack curr next nl
           | (`Str _, _::_, '"') -> fn `Ini stack curr next nl
           | (`Str p, _::_, '\\') -> fn (`Esc p) stack curr next nl
           | (`Esc p, _::_, _) -> fn (`Str p) stack curr next nl
           | (`Str p, _::_, '\255') -> unclosed_comment_string p
           | (`Str _, _::_, _) -> fn state stack curr next nl
           | (`Str _, [], _) -> assert false
           | (`Esc _, [], _) -> assert false
           | (`Ini, _::_, '{') -> fn (`SOp ([], curr)) stack curr next nl
           | (`SOp (l, p), _::_, 'a'..'z')|(`SOp (l, p), _::_, '_') ->
               fn (`SOp ((c :: l), p)) stack curr next nl
           | (`SOp (_, _), p::_, '\255') -> unclosed_comment p
           | (`SOp (l, p), _::_, '|') ->
               fn (`SIn ((List.rev l), p)) stack curr next nl
           | (`SOp (_, _), _::_, _) -> fn `Ini stack curr next nl
           | (`SIn (l, p), _::_, '|') ->
               fn (`SCl (l, (l, p))) stack curr next nl
           | (`SIn (_, p), _::_, '\255') -> unclosed_comment_string p
           | (`SIn (_, _), _::_, _) -> fn state stack curr next nl
           | (`SCl ([], _), _::_, '}') -> fn `Ini stack curr next nl
           | (`SCl ([], b), _::_, '\255') -> unclosed_comment_string (snd b)
           | (`SCl ([], b), _::_, _) -> fn (`SIn b) stack curr next nl
           | (`SCl (l, b), _::_, c) ->
               if c = (List.hd l)
               then let l = List.tl l in fn (`SCl (l, b)) stack curr next nl
               else fn (`SIn b) stack curr next nl
           | (`SOp (_, _), [], _) -> assert false
           | (`SIn (_, _), [], _) -> assert false
           | (`SCl (_, _), [], _) -> assert false
           | (`Ini, _::_, '*') -> fn `Cls stack curr next nl
           | (`Cls, _::_, '*') -> fn `Cls stack curr next nl
           | (`Cls, p::s, ')') ->
               (if (!ocamldoc) && (s = [])
                then
                  (let comment =
                     Buffer.sub ocamldoc_buf 0
                       ((Buffer.length ocamldoc_buf) - 2) in
                   Buffer.clear ocamldoc_buf;
                   (let lnum = Input.line_num (fst p) in
                    ocamldoc_comments := ((p, next, comment, lnum) ::
                      (!ocamldoc_comments));
                    ocamldoc := false));
                fn `Ini s curr next nl)
           | (`Cls, _::_, _) -> fn `Ini stack curr next nl
           | (`Cls, [], _) -> assert false
           | (`Ini, p::_, '\255') -> unclosed_comment p
           | (`Ini, _::_, _) -> fn `Ini stack curr next nl) in
        fn `Ini [] (buf, pos) (buf, pos) true
    let blank1 = blank_sline
    let blank2 = Pa_lexing.ocaml_blank
    let counter = ref 1
    let freshUid () =
      let current = !counter in incr counter; "MOD" ^ (string_of_int current)
    let caml_structure = change_layout structure blank2
    let wrapped_caml_structure =
      Earley_core.Earley.declare_grammar "wrapped_caml_structure"
    let _ =
      Earley_core.Earley.set_grammar wrapped_caml_structure
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.char '(' '(')
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.alternatives
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.empty ())
                       (Earley_core.Earley.empty []))
                    (List.cons caml_structure [])))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char ')' ')')
                 (Earley_core.Earley.empty (fun _default_0 -> _default_0)))))
    let caml_expr = change_layout expression blank2
    let wrapped_caml_expr =
      Earley_core.Earley.declare_grammar "wrapped_caml_expr"
    let _ =
      Earley_core.Earley.set_grammar wrapped_caml_expr
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.char '(' '(')
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.alternatives
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.empty ())
                       (Earley_core.Earley.empty
                          (Pa_ast.exp_unit Location.none)))
                    (List.cons caml_expr [])))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char ')' ')')
                 (Earley_core.Earley.empty (fun _default_0 -> _default_0)))))
    let wrapped_caml_list =
      Earley_core.Earley.declare_grammar "wrapped_caml_list"
    let _ =
      Earley_core.Earley.set_grammar wrapped_caml_list
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.char '[' '[')
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option []
                 (Earley_core.Earley.fsequence expression
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.apply (fun f -> f [])
                          (Earley_core.Earley.fixpoint' (fun l -> l)
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.char ';' ';')
                                (Earley_core.Earley.fsequence expression
                                   (Earley_core.Earley.empty (fun e -> e))))
                             (fun x -> fun f -> fun l -> f (List.cons x l))))
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.option None
                             (Earley_core.Earley.apply (fun x -> Some x)
                                (Earley_core.Earley.char ';' ';')))
                          (Earley_core.Earley.empty
                             (fun _default_0 -> fun l -> fun e -> e :: l))))))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.char ']' ']')
                 (Earley_core.Earley.empty (fun _default_0 -> _default_0)))))
    let wrapped_caml_array =
      Earley_core.Earley.fsequence_ignore
        (Earley_core.Earley.string "[|" "[|")
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.option []
              (Earley_core.Earley.fsequence expression
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint' (fun l -> l)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char ';' ';')
                             (Earley_core.Earley.fsequence expression
                                (Earley_core.Earley.empty (fun e -> e))))
                          (fun x -> fun f -> fun l -> f (List.cons x l))))
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option None
                          (Earley_core.Earley.apply (fun x -> Some x)
                             (Earley_core.Earley.char ';' ';')))
                       (Earley_core.Earley.empty
                          (fun _default_0 -> fun l -> fun e -> e :: l))))))
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.string "|]" "|]")
              (Earley_core.Earley.empty (fun l -> l))))
    let uchar =
      let char_range min max =
        Earley_core.Earley.fsequence Earley_core.Earley.any
          (Earley_core.Earley.empty
             (fun c ->
                let cc = Char.code c in
                if (cc < min) || (cc > max) then give_up (); c)) in
      let tl = char_range 128 191 in
      let hd1 = char_range 0 127 in
      let hd2 = char_range 192 223 in
      let hd3 = char_range 224 239 in
      let hd4 = char_range 240 247 in
      Earley_core.Earley.alternatives
        (List.cons
           (Earley_core.Earley.fsequence hd4
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.no_blank_test ())
                 (Earley_core.Earley.fsequence tl
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.no_blank_test ())
                       (Earley_core.Earley.fsequence tl
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.no_blank_test ())
                             (Earley_core.Earley.fsequence tl
                                (Earley_core.Earley.empty
                                   (fun c3 ->
                                      fun c2 ->
                                        fun c1 ->
                                          fun c0 ->
                                            Printf.sprintf "%c%c%c%c" c0 c1
                                              c2 c3)))))))))
           (List.cons
              (Earley_core.Earley.fsequence hd1
                 (Earley_core.Earley.empty (fun c0 -> Printf.sprintf "%c" c0)))
              (List.cons
                 (Earley_core.Earley.fsequence hd2
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.no_blank_test ())
                       (Earley_core.Earley.fsequence tl
                          (Earley_core.Earley.empty
                             (fun c1 -> fun c0 -> Printf.sprintf "%c%c" c0 c1)))))
                 (List.cons
                    (Earley_core.Earley.fsequence hd3
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.no_blank_test ())
                          (Earley_core.Earley.fsequence tl
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.no_blank_test ())
                                (Earley_core.Earley.fsequence tl
                                   (Earley_core.Earley.empty
                                      (fun c2 ->
                                         fun c1 ->
                                           fun c0 ->
                                             Printf.sprintf "%c%c%c" c0 c1 c2)))))))
                    []))))
    let char_re = "[^ \"\t\r\n\\#*/|_$>{}-]"
    let escaped_re = "\\\\[\\#*/|_$&>{}-]"
    let non_special = ['>'; '*'; '/'; '|'; '-'; '_'; '<'; '='; '`'; '\'']
    let char_alone =
      black_box
        (fun str ->
           fun pos ->
             let (c, str', pos') = Input.read str pos in
             if List.mem c non_special
             then
               let (c', _, _) = Input.read str' pos' in
               (if
                  (c = c') ||
                    (((c = '-') || (c = '=')) && ((c' = '>') || (c' = '<')))
                then give_up ()
                else (c, str', pos'))
             else give_up ())
        (List.fold_left Charset.add Charset.empty non_special) false
        (String.concat " | "
           (List.map (fun c -> String.make 1 c) non_special))
    let special_char =
      [' ';
      '"';
      '\t';
      '\r';
      '\n';
      '\\';
      '#';
      '*';
      '/';
      '|';
      '_';
      '$';
      '<';
      '>';
      '{';
      '}';
      '-';
      '=';
      '&';
      '`';
      '\'']
    let no_spe =
      let f buf pos =
        let (c, _, _) = Input.read buf pos in
        ((), (not (List.mem c special_char))) in
      Earley.test ~name:"no_special" Charset.full f
    let character = Earley_core.Earley.declare_grammar "character"
    let _ =
      Earley_core.Earley.set_grammar character
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence char_alone
                 (Earley_core.Earley.empty (fun c -> String.make 1 c)))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore no_spe
                    (Earley_core.Earley.fsequence uchar
                       (Earley_core.Earley.empty (fun c -> c))))
                 (List.cons
                    (Earley_core.Earley.fsequence
                       (Earley_str.regexp ~name:"escaped" escaped_re
                          (fun group -> group 0))
                       (Earley_core.Earley.empty
                          (fun s ->
                             let open String in
                               escaped (sub s 1 ((length s) - 1))))) []))))
    let word =
      change_layout
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint1' (fun l -> l) character
                 (fun x -> fun f -> fun l -> f (List.cons x l))))
           (Earley_core.Earley.empty (fun cs -> String.concat "" cs)))
        no_blank
    let rec rem_hyphen =
      function
      | [] -> []
      | w::[] -> [w]
      | w1::w2::l ->
          let l1 = String.length w1 in
          if (w1.[l1 - 1]) = '-'
          then let w = (String.sub w1 0 (l1 - 1)) ^ w2 in rem_hyphen (w :: l)
          else w1 :: (rem_hyphen (w2 :: l))
    let verbatim_line = Earley_core.Earley.declare_grammar "verbatim_line"
    let _ =
      Earley_core.Earley.set_grammar verbatim_line
        (Earley_str.regexp
           ~name:"\\\\(^#?#?\\\\([^#\\t\\n][^\\t\\n]*\\\\)?\\\\)"
           "\\(^#?#?\\([^#\t\n][^\t\n]*\\)?\\)" (fun group -> group 0))
    let mode_ident = Earley_core.Earley.declare_grammar "mode_ident"
    let _ =
      Earley_core.Earley.set_grammar mode_ident
        (Earley_str.regexp ~name:"[a-zA-Z0-9_']+" "[a-zA-Z0-9_']+"
           (fun group -> group 0))
    let filename = Earley_core.Earley.declare_grammar "filename"
    let _ =
      Earley_core.Earley.set_grammar filename
        (Earley_str.regexp ~name:"[a-zA-Z0-9-_./]*" "[a-zA-Z0-9-_./]*"
           (fun group -> group 0))
    let verbatim_environment =
      Earley_core.Earley.declare_grammar "verbatim_environment"
    let _ =
      Earley_core.Earley.set_grammar verbatim_environment
        (Earley_core.Earley.fsequence
           (Earley_str.regexp ~name:"^###" "^###" (fun group -> group 0))
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.option None
                 (Earley_core.Earley.apply (fun x -> Some x)
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_str.regexp ~name:"[ \\t]+" "[ \t]+"
                          (fun group -> group 0))
                       (Earley_core.Earley.fsequence mode_ident
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0))))))
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option None
                    (Earley_core.Earley.apply (fun x -> Some x)
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_str.regexp ~name:"[ \\t]+" "[ \t]+"
                             (fun group -> group 0))
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "\"" "\"")
                             (Earley_core.Earley.fsequence filename
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.string "\"" "\"")
                                   (Earley_core.Earley.empty
                                      (fun _default_0 -> _default_0))))))))
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_str.regexp ~name:"[ \\t]*" "[ \t]*"
                       (fun group -> group 0))
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option None
                          (Earley_core.Earley.apply (fun x -> Some x)
                             (Earley_core.Earley.string "\r" "\r")))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "\n" "\n")
                          (Earley_core.Earley.fsequence
                             (Earley_core.Earley.apply (fun f -> f [])
                                (Earley_core.Earley.fixpoint1' (fun l -> l)
                                   (Earley_core.Earley.fsequence
                                      verbatim_line
                                      (Earley_core.Earley.fsequence
                                         (Earley_core.Earley.option None
                                            (Earley_core.Earley.apply
                                               (fun x -> Some x)
                                               (Earley_core.Earley.string
                                                  "\r" "\r")))
                                         (Earley_core.Earley.fsequence_ignore
                                            (Earley_core.Earley.string "\n"
                                               "\n")
                                            (Earley_core.Earley.empty
                                               (fun _default_0 ->
                                                  fun l -> l ^ "\n")))))
                                   (fun x ->
                                      fun f -> fun l -> f (List.cons x l))))
                             (Earley_core.Earley.fsequence
                                (Earley_str.regexp ~name:"^###" "^###"
                                   (fun group -> group 0))
                                (Earley_core.Earley.empty_pos
                                   (fun __loc__start__buf ->
                                      fun __loc__start__pos ->
                                        fun __loc__end__buf ->
                                          fun __loc__end__pos ->
                                            let _loc =
                                              locate __loc__start__buf
                                                __loc__start__pos
                                                __loc__end__buf
                                                __loc__end__pos in
                                            fun _default_0 ->
                                              fun lines ->
                                                fun _default_1 ->
                                                  fun file ->
                                                    fun mode ->
                                                      fun _default_2 ->
                                                        if lines = []
                                                        then give_up ();
                                                        (let nb_hspaces l =
                                                           let len =
                                                             String.length l in
                                                           let nb = ref 0 in
                                                           let found =
                                                             ref false in
                                                           while
                                                             ((!nb) < len) &&
                                                               (not (!found))
                                                             do
                                                             if
                                                               (l.[!nb]) =
                                                                 ' '
                                                             then incr nb
                                                             else
                                                               found := true
                                                             done;
                                                           if !found
                                                           then !nb
                                                           else max_int in
                                                         let f m l =
                                                           min m
                                                             (nb_hspaces l) in
                                                         let minhsp =
                                                           List.fold_left f
                                                             max_int lines in
                                                         let remhsp l =
                                                           let len =
                                                             String.length l in
                                                           if len <= minhsp
                                                           then ""
                                                           else
                                                             String.sub l
                                                               minhsp
                                                               (len - minhsp) in
                                                         let lines =
                                                           let f s tl =
                                                             Pa_ast.exp_Cons
                                                               _loc
                                                               (Pa_ast.exp_string
                                                                  _loc s) tl in
                                                           List.fold_right f
                                                             (List.map remhsp
                                                                lines)
                                                             (Pa_ast.exp_Nil
                                                                _loc) in
                                                         let mode =
                                                           "verbs_" ^
                                                             (match mode with
                                                              | None ->
                                                                  "default"
                                                              | Some m -> m) in
                                                         let file =
                                                           match file with
                                                           | None ->
                                                               Pa_ast.exp_None
                                                                 _loc
                                                           | Some f ->
                                                               Pa_ast.exp_Some
                                                                 _loc
                                                                 (Pa_ast.exp_string
                                                                    _loc f) in
                                                         let body =
                                                           Pa_ast.exp_apply1
                                                             _loc
                                                             (Pa_ast.exp_apply1
                                                                _loc
                                                                (Pa_ast.exp_ident
                                                                   _loc mode)
                                                                file) lines in
                                                         [Ast_helper.Str.value
                                                            Nonrecursive
                                                            [Ast_helper.Vb.mk
                                                               (Ast_helper.Pat.any
                                                                  ~loc:_loc
                                                                  ()) body]])))))))))))
    let verbatim_environment = change_layout verbatim_environment no_blank
    let verbatim_generic st forbid nd =
      let line_re = "[^\n" ^ (forbid ^ "]+") in
      change_layout
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string st st)
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l)
                    (Earley_core.Earley.fsequence
                       (Earley_str.regexp ~name:"line" line_re
                          (fun group -> group 0))
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.option None
                             (Earley_core.Earley.apply (fun x -> Some x)
                                (Earley_core.Earley.char '\r' '\r')))
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char '\n' '\n')
                             (Earley_core.Earley.empty
                                (fun _default_0 -> fun l -> l)))))
                    (fun x -> fun f -> fun l -> f (List.cons x l))))
              (Earley_core.Earley.fsequence
                 (Earley_str.regexp ~name:"line" line_re
                    (fun group -> group 0))
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string nd nd)
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun l ->
                                  fun ls ->
                                    let lines = ls @ [l] in
                                    let lines = rem_hyphen lines in
                                    let txt = String.concat " " lines in
                                    Pa_ast.exp_apply1 _loc
                                      (Pa_ast.exp_ident _loc "verbatim")
                                      (Pa_ast.exp_string _loc txt)))))))
        no_blank
    let verbatim_macro = verbatim_generic "\\verb{" "{}" "}"
    let verbatim_sharp = verbatim_generic "##" "#" "##"
    let verbatim_bquote = verbatim_generic "``" "`" "``"
    type tag_syntax =
      | Italic 
      | Bold 
      | SmallCap 
      | Underline 
      | Strike 
      | Quote 
    module Tag_syntax = struct type t = tag_syntax
                               let compare = compare end
    module TagSet = (Set.Make)(Tag_syntax)
    let addTag = TagSet.add
    let allowed t f = not (TagSet.mem t f)
    type math_prio =
      | AtomM 
      | Accent 
      | LInd 
      | Ind 
      | IApply 
      | IProd 
      | Prod 
      | Sum 
      | Operator 
      | Rel 
      | Neg 
      | Conj 
      | Impl 
      | Punc 
    let math_prios =
      [AtomM;
      Accent;
      LInd;
      Ind;
      IProd;
      Prod;
      Sum;
      Operator;
      Rel;
      Neg;
      Conj;
      Impl;
      Punc]
    let next_prio =
      function
      | Punc -> Impl
      | Impl -> Conj
      | Conj -> Neg
      | Neg -> Rel
      | Rel -> Operator
      | Operator -> Sum
      | Sum -> Prod
      | Prod -> IProd
      | IProd -> IApply
      | IApply -> Ind
      | Ind -> LInd
      | LInd -> Accent
      | Accent -> AtomM
      | AtomM -> assert false
    type symbol =
      | Invisible 
      | SimpleSym of string 
      | MultiSym of Parsetree.expression 
      | CamlSym of Parsetree.expression 
    let symbol = Earley_core.Earley.declare_grammar "symbol"
    let _ =
      Earley_core.Earley.set_grammar symbol
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_str.regexp ~name:"[^ \\t\\r\\n{}]+" "[^ \t\r\n{}]+"
                 (fun group -> group 0))
              (List.cons (Earley_core.Earley.string "\\}" "\\}")
                 (List.cons (Earley_core.Earley.string "\\{" "\\{") []))))
    let symbols =
      let space_blank = Earley_str.blank_regexp "[ ]*" in
      change_layout
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "{" "{")
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l) symbol
                    (fun x -> fun f -> fun l -> f (List.cons x l))))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "}" "}")
                 (Earley_core.Earley.empty (fun ss -> ss))))) space_blank
    let symbol_value = Earley_core.Earley.declare_grammar "symbol_value"
    let _ =
      Earley_core.Earley.set_grammar symbol_value
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence wrapped_caml_expr
                 (Earley_core.Earley.empty (fun e -> CamlSym e)))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "{" "{")
                    (Earley_core.Earley.fsequence symbol
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "}" "}")
                          (Earley_core.Earley.empty (fun s -> SimpleSym s)))))
                 [])))
    let symbol_values = Earley_core.Earley.declare_grammar "symbol_values"
    let _ = Earley_core.Earley.set_grammar symbol_values wrapped_caml_expr
    let lid = Earley_core.Earley.declare_grammar "lid"
    let _ =
      Earley_core.Earley.set_grammar lid
        (Earley_str.regexp ~name:"[_a-z][_a-zA-Z0-9']*"
           "[_a-z][_a-zA-Z0-9']*" (fun group -> group 0))
    let uid = Earley_core.Earley.declare_grammar "uid"
    let _ =
      Earley_core.Earley.set_grammar uid
        (Earley_str.regexp ~name:"[A-Z][_a-zA-Z0-9']*" "[A-Z][_a-zA-Z0-9']*"
           (fun group -> group 0))
    let num = Earley_core.Earley.declare_grammar "num"
    let _ =
      Earley_core.Earley.set_grammar num
        (Earley_core.Earley.fsequence
           (Earley_str.regexp ~name:"[0-9]+" "[0-9]+" (fun group -> group 0))
           (Earley_core.Earley.empty (fun n -> int_of_string n)))
    let int = Earley_core.Earley.declare_grammar "int"
    let _ =
      Earley_core.Earley.set_grammar int
        (Earley_core.Earley.fsequence
           (Earley_str.regexp ~name:"-?[0-9]+" "-?[0-9]+"
              (fun group -> group 0))
           (Earley_core.Earley.empty (fun n -> int_of_string n)))
    let float = Earley_core.Earley.declare_grammar "float"
    let _ =
      Earley_core.Earley.set_grammar float
        (Earley_str.regexp
           ~name:"-?[0-9]+\\\\(.[0-9]*\\\\)?\\\\([eE]-?[0-9]+\\\\)?"
           "-?[0-9]+\\(.[0-9]*\\)?\\([eE]-?[0-9]+\\)?" (fun group -> group 0))
    let symbol ss =
      List.partition
        (fun s -> assert ((String.length s) > 0); (s.[0]) = '\\') ss
    type entry =
      | Caml 
      | CamlStruct 
      | String 
      | Math 
      | MathLine 
      | MathMatrix 
      | Text 
      | TextLine 
      | TextMatrix 
      | Current 
      | Int 
      | Float 
    type arg_config = {
      entry: entry ;
      filter_name: string }
    let default_config = { entry = Current; filter_name = "" }
    type config =
      | EatR 
      | EatL 
      | Name of string list * string 
      | Syntax of arg_config list 
    let real_name _loc id cs =
      let rec find_name : config list -> (string list * string) =
        function
        | [] -> raise Not_found
        | (Name (mp, id))::_ -> (mp, id)
        | _::cs -> find_name cs in
      let (mp, mid) = try find_name cs with | Not_found -> ([], id) in
      List.fold_left (fun acc -> fun m -> exp_mod_open _loc m acc)
        (Pa_ast.exp_ident _loc mid) mp
    let macro_args cs =
      let rec find_args : config list -> arg_config list option =
        function
        | [] -> None
        | (Syntax l)::_ -> Some l
        | _::cs -> find_args cs in
      find_args cs
    let arg_type = Earley_core.Earley.declare_grammar "arg_type"
    let _ =
      Earley_core.Earley.set_grammar arg_type
        (Earley_core.Earley.fsequence
           (Earley_str.regexp ~name:"[a-zA-Z0-9_]+" "[a-zA-Z0-9_]+"
              (fun group -> group 0))
           (Earley_core.Earley.empty
              (fun s ->
                 match s with
                 | "math_line" -> MathLine
                 | "math_matrix" -> MathMatrix
                 | "math" -> Math
                 | "text" -> Text
                 | "text_line" -> TextLine
                 | "text_matrix" -> TextMatrix
                 | "caml" -> Caml
                 | "struct" -> CamlStruct
                 | "current" -> Current
                 | "int" -> Int
                 | "float" -> Float
                 | "string" -> String
                 | _ -> give_up ())))
    let arg_description =
      Earley_core.Earley.declare_grammar "arg_description"
    let _ =
      Earley_core.Earley.set_grammar arg_description
        (Earley_core.Earley.fsequence arg_type
           (Earley_core.Earley.fsequence (Earley_core.Earley.option "" lid)
              (Earley_core.Earley.empty
                 (fun filter_name -> fun entry -> { entry; filter_name }))))
    let arg_descriptions =
      Earley_core.Earley.declare_grammar "arg_descriptions"
    let _ =
      Earley_core.Earley.set_grammar arg_descriptions
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence arg_description
                 (Earley_core.Earley.fsequence
                    (Earley_core.Earley.apply (fun f -> f [])
                       (Earley_core.Earley.fixpoint' (fun l -> l)
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "," ",")
                             (Earley_core.Earley.fsequence arg_description
                                (Earley_core.Earley.empty
                                   (fun _default_0 -> _default_0))))
                          (fun x -> fun f -> fun l -> f (List.cons x l))))
                    (Earley_core.Earley.empty (fun l -> fun a -> a :: l))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.empty ())
                    (Earley_core.Earley.empty [])) [])))
    let expr_filter
      : (string, Parsetree.expression -> Location.t -> Parsetree.expression)
          Hashtbl.t
      = Hashtbl.create 31
    let struct_filter
      : (string, Parsetree.structure -> Location.t -> Parsetree.expression)
          Hashtbl.t
      = Hashtbl.create 31
    let string_filter
      : (string, string -> Location.t -> Parsetree.expression) Hashtbl.t =
      Hashtbl.create 31
    let apply_string_filter config s _loc =
      try Hashtbl.find string_filter config.filter_name s _loc
      with
      | Not_found ->
          (Printf.eprintf "Unknown string filter: %S (%a)\n%!"
             config.filter_name Pa_ocaml_prelude.print_location _loc;
           exit 1)
    let apply_expr_filter config s _loc =
      try Hashtbl.find expr_filter config.filter_name s _loc
      with
      | Not_found ->
          (Printf.eprintf "Unknown expression filter: %S (%a)\n%!"
             config.filter_name Pa_ocaml_prelude.print_location _loc;
           exit 1)
    let apply_struct_filter config s _loc =
      try Hashtbl.find struct_filter config.filter_name s _loc
      with
      | Not_found ->
          (Printf.eprintf "Unknown structure filter: %S (%a)\n%!"
             config.filter_name Pa_ocaml_prelude.print_location _loc;
           exit 1)
    let _ =
      Hashtbl.add string_filter ""
        (fun s -> fun _loc -> Pa_ast.exp_string _loc s)
    let _ = Hashtbl.add expr_filter "" (fun e -> fun _loc -> e)
    let _ =
      Hashtbl.add struct_filter ""
        (fun s -> fun _loc -> struct_filter_default_expr _loc s)
    let _ =
      Hashtbl.add string_filter "genumerate"
        (fun s ->
           fun _loc ->
             let pos = Str.search_forward (Str.regexp "&\\([1iIaA]\\)") s 0 in
             let c = s.[pos + 1] in
             let prefix = String.sub s 0 pos in
             let suffix =
               String.sub s (pos + 2) (((String.length s) - pos) - 2) in
             let nb_kind =
               match c with
               | '1' -> "Arabic"
               | 'i' -> "RomanLower"
               | 'I' -> "RomanUpper"
               | 'a' -> "AlphaLower"
               | 'A' -> "AlphaUpper"
               | _ ->
                   (Printf.eprintf
                      "Invalid argument to genumerate: %c. Falling back to arabic.\n"
                      c;
                    flush stderr;
                    "Arabic") in
             let caml =
               "(" ^
                 (nb_kind ^
                    (",(fun num_sec -> <<" ^
                       (prefix ^
                          ("\\caml( [tT num_sec] )" ^ (suffix ^ ">>))"))))) in
             Earley.parse_string Pa_ocaml_prelude.expression blank2 caml)
    let _ =
      Hashtbl.add struct_filter "diagram"
        (fun s -> fun _loc -> struct_filter_diagram_expr _loc s)
    let config = Earley_core.Earley.declare_grammar "config"
    let _ =
      Earley_core.Earley.set_grammar config
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "syntax" "syntax")
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "=" "=")
                    (Earley_core.Earley.fsequence arg_descriptions
                       (Earley_core.Earley.empty (fun l -> Syntax l)))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "eat_right" "eat_right")
                    (Earley_core.Earley.empty EatR))
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "eat_left" "eat_left")
                       (Earley_core.Earley.empty EatL))
                    (List.cons
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "name" "name")
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "=" "=")
                             (Earley_core.Earley.fsequence
                                (Earley_core.Earley.apply (fun f -> f [])
                                   (Earley_core.Earley.fixpoint' (fun l -> l)
                                      (Earley_core.Earley.fsequence uid
                                         (Earley_core.Earley.fsequence_ignore
                                            (Earley_core.Earley.no_blank_test
                                               ())
                                            (Earley_core.Earley.fsequence_ignore
                                               (Earley_core.Earley.string "."
                                                  ".")
                                               (Earley_core.Earley.fsequence_ignore
                                                  (Earley_core.Earley.no_blank_test
                                                     ())
                                                  (Earley_core.Earley.empty
                                                     (fun u -> u))))))
                                      (fun x ->
                                         fun f -> fun l -> f (List.cons x l))))
                                (Earley_core.Earley.fsequence lid
                                   (Earley_core.Earley.empty
                                      (fun id -> fun ms -> Name (ms, id)))))))
                       [])))))
    let configs = Earley_core.Earley.declare_grammar "configs"
    let _ =
      Earley_core.Earley.set_grammar configs
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "{" "{")
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l)
                    (Earley_core.Earley.fsequence config
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char ';' ';')
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0))))
                    (fun x -> fun f -> fun l -> f (List.cons x l))))
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "}" "}")
                 (Earley_core.Earley.empty (fun cs -> cs)))))
    type 'a indices =
      {
      up_right: 'a option ;
      up_right_same_script: bool ;
      down_right: 'a option ;
      up_left_same_script: bool ;
      up_left: 'a option ;
      down_left: 'a option }
    let no_ind =
      {
        up_right = None;
        up_left = None;
        down_right = None;
        down_left = None;
        up_right_same_script = false;
        up_left_same_script = false
      }
    type infix =
      {
      infix_prio: math_prio ;
      infix_utf8_names: string list ;
      infix_macro_names: string list ;
      infix_value: symbol ;
      infix_space: int ;
      infix_no_left_space: bool ;
      infix_no_right_space: bool }
    let invisible_product =
      {
        infix_prio = Prod;
        infix_utf8_names = [];
        infix_macro_names = [];
        infix_value = Invisible;
        infix_space = 3;
        infix_no_left_space = false;
        infix_no_right_space = false
      }
    let invisible_apply =
      {
        infix_prio = Prod;
        infix_utf8_names = [];
        infix_macro_names = [];
        infix_value = Invisible;
        infix_space = 5;
        infix_no_left_space = false;
        infix_no_right_space = false
      }
    type prefix =
      {
      prefix_prio: math_prio ;
      prefix_utf8_names: string list ;
      prefix_macro_names: string list ;
      prefix_value: symbol ;
      prefix_space: int ;
      prefix_no_space: bool }
    type postfix =
      {
      postfix_prio: math_prio ;
      postfix_utf8_names: string list ;
      postfix_macro_names: string list ;
      postfix_value: symbol ;
      postfix_space: int ;
      postfix_no_space: bool }
    type atom_symbol =
      {
      symbol_utf8_names: string list ;
      symbol_macro_names: string list ;
      symbol_value: symbol }
    type operator_kind =
      | Limits 
      | NoLimits 
    type operator =
      {
      operator_prio: math_prio ;
      operator_utf8_names: string list ;
      operator_macro_names: string list ;
      operator_values: Parsetree.expression ;
      operator_kind: operator_kind }
    type delimiter =
      {
      delimiter_utf8_names: string list ;
      delimiter_macro_names: string list ;
      delimiter_values: Parsetree.expression }
    let invisible_delimiter =
      {
        delimiter_utf8_names = [];
        delimiter_macro_names = [];
        delimiter_values = (let _loc = Location.none in Pa_ast.exp_Nil _loc)
      }
    module PMap = PrefixTree
    type grammar_state =
      {
      mutable verbose: bool ;
      mutable infix_symbols: infix PrefixTree.t ;
      mutable prefix_symbols: prefix PrefixTree.t ;
      mutable postfix_symbols: postfix PrefixTree.t ;
      mutable quantifier_symbols: atom_symbol PrefixTree.t ;
      mutable atom_symbols: atom_symbol PrefixTree.t ;
      mutable any_symbols: atom_symbol PrefixTree.t ;
      mutable accent_symbols: atom_symbol PrefixTree.t ;
      mutable left_delimiter_symbols: delimiter PrefixTree.t ;
      mutable right_delimiter_symbols: delimiter PrefixTree.t ;
      mutable operator_symbols: operator PrefixTree.t ;
      mutable combining_symbols: string PrefixTree.t ;
      mutable reserved_symbols: unit PrefixTree.t ;
      mutable word_macros: (string * config list) list ;
      mutable math_macros: (string * config list) list ;
      mutable environment: (string * config list) list }
    let state =
      {
        verbose = false;
        infix_symbols = PrefixTree.empty;
        prefix_symbols = PrefixTree.empty;
        postfix_symbols = PrefixTree.empty;
        quantifier_symbols = PrefixTree.empty;
        atom_symbols = PrefixTree.empty;
        any_symbols = PrefixTree.empty;
        accent_symbols = PrefixTree.empty;
        left_delimiter_symbols = PrefixTree.empty;
        right_delimiter_symbols = PrefixTree.empty;
        operator_symbols = PrefixTree.empty;
        combining_symbols = PrefixTree.empty;
        reserved_symbols =
          (PrefixTree.add "left" ()
             (PrefixTree.add "right" () PrefixTree.empty));
        word_macros = [];
        math_macros = [];
        environment = []
      }
    let mathlid = Earley_core.Earley.declare_grammar "mathlid"
    let _ =
      Earley_core.Earley.set_grammar mathlid
        (Earley_core.Earley.fsequence
           (Earley_str.regexp ~name:"[a-z][a-zA-Z0-9']*" "[a-z][a-zA-Z0-9']*"
              (fun group -> group 0))
           (Earley_core.Earley.empty
              (fun id ->
                 if PMap.mem id state.reserved_symbols then give_up (); id)))
    let reserved_uid =
      ["Include";
      "Caml";
      "Configure_math_macro";
      "Configure_word_macro";
      "Configure_environment";
      "Verbose_Changes";
      "Save_Grammar";
      "Add_relation";
      "Add_addition_like";
      "Add_product_like";
      "Add_connector";
      "Add_arrow";
      "Add_punctuation";
      "Add_quantifier";
      "Add_prefix";
      "Add_postfix";
      "Add_accent";
      "Add_symbol";
      "Add_operator";
      "Add_limits_operator";
      "Add_left";
      "Add_right";
      "Add_combining"]
    let macrouid = Earley_core.Earley.declare_grammar "macrouid"
    let _ =
      Earley_core.Earley.set_grammar macrouid
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.alternatives
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "item" "item")
                    (Earley_core.Earley.empty "Item")) (List.cons uid [])))
           (Earley_core.Earley.empty
              (fun id -> if List.mem id reserved_uid then give_up (); id)))
    let empty_state =
      {
        verbose = false;
        infix_symbols = PrefixTree.empty;
        prefix_symbols = PrefixTree.empty;
        postfix_symbols = PrefixTree.empty;
        quantifier_symbols = PrefixTree.empty;
        atom_symbols = PrefixTree.empty;
        any_symbols = PrefixTree.empty;
        accent_symbols = PrefixTree.empty;
        left_delimiter_symbols = PrefixTree.empty;
        right_delimiter_symbols = PrefixTree.empty;
        operator_symbols = PrefixTree.empty;
        combining_symbols = PrefixTree.empty;
        reserved_symbols = PrefixTree.empty;
        word_macros = [];
        math_macros = [];
        environment = []
      }
    let local_state =
      {
        verbose = false;
        infix_symbols = PrefixTree.empty;
        prefix_symbols = PrefixTree.empty;
        postfix_symbols = PrefixTree.empty;
        quantifier_symbols = PrefixTree.empty;
        atom_symbols = PrefixTree.empty;
        any_symbols = PrefixTree.empty;
        accent_symbols = PrefixTree.empty;
        left_delimiter_symbols = PrefixTree.empty;
        right_delimiter_symbols = PrefixTree.empty;
        operator_symbols = PrefixTree.empty;
        combining_symbols = PrefixTree.empty;
        reserved_symbols = PrefixTree.empty;
        word_macros = [];
        math_macros = [];
        environment = []
      }
    let merge_states : grammar_state -> grammar_state -> unit =
      fun s1 ->
        fun s2 ->
          s1.verbose <- (s1.verbose || s2.verbose);
          s1.infix_symbols <-
            (PrefixTree.union s1.infix_symbols s2.infix_symbols);
          s1.prefix_symbols <-
            (PrefixTree.union s1.prefix_symbols s2.prefix_symbols);
          s1.postfix_symbols <-
            (PrefixTree.union s1.postfix_symbols s2.postfix_symbols);
          s1.quantifier_symbols <-
            (PrefixTree.union s1.quantifier_symbols s2.quantifier_symbols);
          s1.atom_symbols <-
            (PrefixTree.union s1.atom_symbols s2.atom_symbols);
          s1.any_symbols <- (PrefixTree.union s1.any_symbols s2.any_symbols);
          s1.accent_symbols <-
            (PrefixTree.union s1.accent_symbols s2.accent_symbols);
          s1.left_delimiter_symbols <-
            (PrefixTree.union s1.left_delimiter_symbols
               s2.left_delimiter_symbols);
          s1.right_delimiter_symbols <-
            (PrefixTree.union s1.right_delimiter_symbols
               s2.right_delimiter_symbols);
          s1.operator_symbols <-
            (PrefixTree.union s1.operator_symbols s2.operator_symbols);
          s1.combining_symbols <-
            (PrefixTree.union s1.combining_symbols s2.combining_symbols);
          s1.reserved_symbols <-
            (PrefixTree.union s1.reserved_symbols s2.reserved_symbols);
          s1.word_macros <- (s2.word_macros @ s1.word_macros);
          s1.math_macros <- (s2.math_macros @ s1.math_macros);
          s1.environment <- (s2.environment @ s1.environment)
    let (math_prefix_symbol, set_math_prefix_symbol) =
      grammar_family "prefix"
    let (math_postfix_symbol, set_math_postfix_symbol) =
      grammar_family "postfix"
    let (math_operator, set_math_operator) = grammar_family "operator"
    let (math_infix_symbol, set_math_infix_symbol) = grammar_family "infix"
    let math_atom_symbol = declare_grammar "atom_symbol"
    let math_any_symbol = declare_grammar "any_symbol"
    let math_quantifier_symbol = declare_grammar "quantifier"
    let math_accent_symbol = declare_grammar "accent"
    let math_left_delimiter = declare_grammar "left delimiter"
    let math_right_delimiter = declare_grammar "right delimiter"
    let math_combining_symbol = declare_grammar "combining"
    let math_punctuation_symbol = declare_grammar "punctuation"
    let math_relation_symbol = declare_grammar "relation"
    let macro_char : Charset.t =
      Charset.union (Charset.range 'a' 'z') (Charset.range 'A' 'Z')
    let is_macro_char : char -> bool = Charset.mem macro_char
    let tree_to_grammar
      : ?filter:('a -> bool) -> string -> 'a PMap.tree -> 'a grammar =
      fun ?(filter= fun _ -> true) ->
        fun name ->
          fun t ->
            let PMap.Node (_, l) = t in
            let fn buf pos =
              let line = Input.line buf in
              let line = String.sub line pos ((String.length line) - pos) in
              try
                let (n, v) = PMap.longest_prefix ~filter line t in
                if (n > 1) && ((line.[0]) = '\\')
                then
                  (let is_macro = ref true in
                   for i = 1 to n - 1 do
                     is_macro := ((!is_macro) && (is_macro_char (line.[i])))
                   done;
                   (let len = String.length line in
                    if
                      (!is_macro) &&
                        ((len >= n) && (is_macro_char (line.[n])))
                    then give_up ()));
                (v, buf, (pos + n))
              with | Not_found -> give_up () in
            let charset =
              let f acc (c, _) = Charset.add acc c in
              List.fold_left f Charset.empty l in
            black_box fn charset false name
    let build_grammar () =
      set_math_infix_symbol
        (fun p ->
           tree_to_grammar ~filter:(fun s -> s.infix_prio = p) "infix_symbol"
             state.infix_symbols);
      set_grammar math_punctuation_symbol
        (tree_to_grammar ~filter:(fun s -> s.infix_prio = Punc)
           "punctuation_symbol" state.infix_symbols);
      set_grammar math_relation_symbol
        (tree_to_grammar ~filter:(fun s -> s.infix_prio = Rel)
           "relation_symbol" state.infix_symbols);
      set_math_prefix_symbol
        (fun p ->
           tree_to_grammar ~filter:(fun s -> s.prefix_prio = p)
             "prefix_symbol" state.prefix_symbols);
      set_math_postfix_symbol
        (fun p ->
           tree_to_grammar ~filter:(fun s -> s.postfix_prio = p)
             "postfix_symbol" state.postfix_symbols);
      set_math_operator
        (fun p ->
           tree_to_grammar ~filter:(fun s -> s.operator_prio = p)
             "operator_symbol" state.operator_symbols);
      set_grammar math_quantifier_symbol
        (tree_to_grammar "quantifier_symbol" state.quantifier_symbols);
      set_grammar math_atom_symbol
        (tree_to_grammar "atom_symbol" state.atom_symbols);
      set_grammar math_any_symbol
        (tree_to_grammar "any_symbol" state.any_symbols);
      set_grammar math_accent_symbol
        (tree_to_grammar "accent_symbol" state.accent_symbols);
      set_grammar math_left_delimiter
        (tree_to_grammar "left_delimiter_symbol" state.left_delimiter_symbols);
      set_grammar math_right_delimiter
        (tree_to_grammar "right_delimiter_symbol"
           state.right_delimiter_symbols);
      set_grammar math_combining_symbol
        (tree_to_grammar "combining_symbol" state.combining_symbols)
    let add_grammar g =
      let open Config in
        let (gpath, gpaths) = patoconfig.grammars_dir in
        let path = "." :: ".patobuild" :: gpath :: gpaths in
        if (!no_default_grammar) && (g = "DefaultGrammar")
        then ()
        else
          (let g =
             try Filename.find_file (g ^ ".tgy") path
             with | Not_found -> raise Not_found in
           let ch = open_in_bin g in
           let st = input_value ch in merge_states state st; close_in ch)
    let all_left_delimiter =
      Earley_core.Earley.declare_grammar "all_left_delimiter"
    let _ =
      Earley_core.Earley.set_grammar all_left_delimiter
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "\\left." "\\left.")
                 (Earley_core.Earley.empty invisible_delimiter))
              (List.cons math_left_delimiter
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "\\left" "\\left")
                       (Earley_core.Earley.fsequence math_right_delimiter
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0)))) []))))
    let all_right_delimiter =
      Earley_core.Earley.declare_grammar "all_right_delimiter"
    let _ =
      Earley_core.Earley.set_grammar all_right_delimiter
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "\\right." "\\right.")
                 (Earley_core.Earley.empty invisible_delimiter))
              (List.cons math_right_delimiter
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "\\right" "\\right")
                       (Earley_core.Earley.fsequence math_left_delimiter
                          (Earley_core.Earley.empty
                             (fun _default_0 -> _default_0)))) []))))
    let symbol_paragraph _loc syms names =
      symbol_paragraph_structure _loc ~syms ~names
    let math_list _loc l =
      let merge x y =
        let comma =
          Pa_ast.exp_apply1 _loc
            (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "node" }) }))
            (maths_glyphs_str _loc ",") in
        let normal =
          Pa_ast.exp_const_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "Normal" }) })
            (Some
               (Pa_ast.exp_tuple _loc
                  [Pa_ast.exp_bool _loc false;
                  comma;
                  Pa_ast.exp_bool _loc false])) in
        Pa_ast.exp_list _loc
          [Pa_ast.exp_apply _loc
             (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "bin" }) }))
             [Pa_ast.exp_int _loc 0; normal; x; y]] in
      List.fold_left merge (List.hd l) (List.tl l)
    let dollar = Pa_lexing.single_char '$'
    let no_brace =
      Earley.test ~name:"no_brace" Charset.full
        (fun buf ->
           fun pos ->
             if (Input.get buf pos) <> '{' then ((), true) else ((), false))
    let br_string = Earley_core.Earley.declare_grammar "br_string"
    let _ =
      Earley_core.Earley.set_grammar br_string
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence br_string
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '\n' '\n')
                    (Earley_core.Earley.empty (fun s1 -> s1 ^ "\n"))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.empty ())
                    (Earley_core.Earley.empty ""))
                 (List.cons
                    (Earley_core.Earley.fsequence br_string
                       (Earley_core.Earley.fsequence
                          (Earley_str.regexp ~name:"[^{}]+" "[^{}]+"
                             (fun group -> group 0))
                          (Earley_core.Earley.empty
                             (fun s2 -> fun s1 -> s1 ^ s2))))
                    (List.cons
                       (Earley_core.Earley.fsequence br_string
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char '{' '{')
                             (Earley_core.Earley.fsequence br_string
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.char '}' '}')
                                   (Earley_core.Earley.empty
                                      (fun s2 ->
                                         fun s1 -> s1 ^ ("{" ^ (s2 ^ "}"))))))))
                       [])))))
    let (paragraph_basic_text, set_paragraph_basic_text) =
      grammar_family "paragraph_basic_text"
    let math_toplevel = declare_grammar "math_toplevel"
    let nil = let _loc = Location.none in Pa_ast.exp_Nil _loc
    let math_line =
      Earley_core.Earley.fsequence
        (Earley_core.Earley.option nil math_toplevel)
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint' (fun l -> l)
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '&' '&')
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option nil math_toplevel)
                       (Earley_core.Earley.empty (fun l -> l))))
                 (fun x -> fun f -> fun l -> f (List.cons x l))))
           (Earley_core.Earley.empty_pos
              (fun __loc__start__buf ->
                 fun __loc__start__pos ->
                   fun __loc__end__buf ->
                     fun __loc__end__pos ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       fun ls ->
                         fun m ->
                           List.fold_right
                             (fun x -> fun acc -> Pa_ast.exp_Cons _loc x acc)
                             (m :: ls) (Pa_ast.exp_Nil _loc))))
    let math_matrix =
      Earley_core.Earley.alternatives
        (List.cons
           (Earley_core.Earley.fsequence math_line
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l)
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "\\\\" "\\\\")
                          (Earley_core.Earley.fsequence math_line
                             (Earley_core.Earley.empty (fun m -> m))))
                       (fun x -> fun f -> fun l -> f (List.cons x l))))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun ls ->
                               fun l ->
                                 List.fold_right
                                   (fun x ->
                                      fun acc -> Pa_ast.exp_Cons _loc x acc)
                                   (l :: ls) (Pa_ast.exp_Nil _loc)))))
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.empty ())
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             Pa_ast.exp_Nil _loc))) []))
    let simple_text =
      change_layout (paragraph_basic_text TagSet.empty)
        ~new_blank_after:false blank1
    let text_line =
      Earley_core.Earley.fsequence
        (Earley_core.Earley.option nil simple_text)
        (Earley_core.Earley.fsequence
           (Earley_core.Earley.apply (fun f -> f [])
              (Earley_core.Earley.fixpoint' (fun l -> l)
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '&' '&')
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.option nil simple_text)
                       (Earley_core.Earley.empty (fun l -> l))))
                 (fun x -> fun f -> fun l -> f (List.cons x l))))
           (Earley_core.Earley.empty_pos
              (fun __loc__start__buf ->
                 fun __loc__start__pos ->
                   fun __loc__end__buf ->
                     fun __loc__end__pos ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       fun ls ->
                         fun m ->
                           List.fold_right
                             (fun x -> fun acc -> Pa_ast.exp_Cons _loc x acc)
                             (m :: ls) (Pa_ast.exp_Nil _loc))))
    let text_matrix =
      Earley_core.Earley.alternatives
        (List.cons
           (Earley_core.Earley.fsequence text_line
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l)
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "\\\\" "\\\\")
                          (Earley_core.Earley.fsequence text_line
                             (Earley_core.Earley.empty (fun m -> m))))
                       (fun x -> fun f -> fun l -> f (List.cons x l))))
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun ls ->
                               fun l ->
                                 List.fold_right
                                   (fun x ->
                                      fun acc -> Pa_ast.exp_Cons _loc x acc)
                                   (l :: ls) (Pa_ast.exp_Nil _loc)))))
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.empty ())
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             Pa_ast.exp_Nil _loc))) []))
    let (macro_argument, macro_argument__set__grammar) =
      Earley_core.Earley.grammar_family "macro_argument"
    let _ =
      macro_argument__set__grammar
        (fun config ->
           Earley_core.Earley.alternatives
             (List.append
                (if config.entry = String
                 then
                   List.cons
                     (Earley_core.Earley.fsequence_ignore
                        (Earley_core.Earley.char '{' '{')
                        (Earley_core.Earley.fsequence
                           (change_layout br_string no_blank)
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.char '}' '}')
                              (Earley_core.Earley.empty_pos
                                 (fun __loc__start__buf ->
                                    fun __loc__start__pos ->
                                      fun __loc__end__buf ->
                                        fun __loc__end__pos ->
                                          let _loc =
                                            locate __loc__start__buf
                                              __loc__start__pos
                                              __loc__end__buf __loc__end__pos in
                                          fun s ->
                                            apply_string_filter config s _loc)))))
                     []
                 else [])
                (List.append
                   (if config.entry = Math
                    then
                      List.cons
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.char '{' '{')
                           (Earley_core.Earley.fsequence math_toplevel
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char '}' '}')
                                 (Earley_core.Earley.empty_pos
                                    (fun __loc__start__buf ->
                                       fun __loc__start__pos ->
                                         fun __loc__end__buf ->
                                           fun __loc__end__pos ->
                                             let _loc =
                                               locate __loc__start__buf
                                                 __loc__start__pos
                                                 __loc__end__buf
                                                 __loc__end__pos in
                                             fun m ->
                                               apply_expr_filter config m
                                                 _loc))))) []
                    else [])
                   (List.append
                      (if config.entry = MathLine
                       then
                         List.cons
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.char '{' '{')
                              (Earley_core.Earley.fsequence math_line
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.char '}' '}')
                                    (Earley_core.Earley.empty_pos
                                       (fun __loc__start__buf ->
                                          fun __loc__start__pos ->
                                            fun __loc__end__buf ->
                                              fun __loc__end__pos ->
                                                let _loc =
                                                  locate __loc__start__buf
                                                    __loc__start__pos
                                                    __loc__end__buf
                                                    __loc__end__pos in
                                                fun m ->
                                                  apply_expr_filter config m
                                                    _loc))))) []
                       else [])
                      (List.append
                         (if config.entry = MathMatrix
                          then
                            List.cons
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.char '{' '{')
                                 (Earley_core.Earley.fsequence math_matrix
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.char '}' '}')
                                       (Earley_core.Earley.empty_pos
                                          (fun __loc__start__buf ->
                                             fun __loc__start__pos ->
                                               fun __loc__end__buf ->
                                                 fun __loc__end__pos ->
                                                   let _loc =
                                                     locate __loc__start__buf
                                                       __loc__start__pos
                                                       __loc__end__buf
                                                       __loc__end__pos in
                                                   fun m ->
                                                     apply_expr_filter config
                                                       m _loc))))) []
                          else [])
                         (List.append
                            (if config.entry = Text
                             then
                               List.cons
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.char '{' '{')
                                    (Earley_core.Earley.fsequence simple_text
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.char '}' '}')
                                          (Earley_core.Earley.empty_pos
                                             (fun __loc__start__buf ->
                                                fun __loc__start__pos ->
                                                  fun __loc__end__buf ->
                                                    fun __loc__end__pos ->
                                                      let _loc =
                                                        locate
                                                          __loc__start__buf
                                                          __loc__start__pos
                                                          __loc__end__buf
                                                          __loc__end__pos in
                                                      fun e ->
                                                        apply_expr_filter
                                                          config e _loc)))))
                                 []
                             else [])
                            (List.append
                               (if config.entry = TextLine
                                then
                                  List.cons
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.char '{' '{')
                                       (Earley_core.Earley.fsequence
                                          text_line
                                          (Earley_core.Earley.fsequence_ignore
                                             (Earley_core.Earley.char '}' '}')
                                             (Earley_core.Earley.empty_pos
                                                (fun __loc__start__buf ->
                                                   fun __loc__start__pos ->
                                                     fun __loc__end__buf ->
                                                       fun __loc__end__pos ->
                                                         let _loc =
                                                           locate
                                                             __loc__start__buf
                                                             __loc__start__pos
                                                             __loc__end__buf
                                                             __loc__end__pos in
                                                         fun e ->
                                                           apply_expr_filter
                                                             config e _loc)))))
                                    []
                                else [])
                               (List.append
                                  (if config.entry = TextMatrix
                                   then
                                     List.cons
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.char '{' '{')
                                          (Earley_core.Earley.fsequence
                                             text_matrix
                                             (Earley_core.Earley.fsequence_ignore
                                                (Earley_core.Earley.char '}'
                                                   '}')
                                                (Earley_core.Earley.empty_pos
                                                   (fun __loc__start__buf ->
                                                      fun __loc__start__pos
                                                        ->
                                                        fun __loc__end__buf
                                                          ->
                                                          fun __loc__end__pos
                                                            ->
                                                            let _loc =
                                                              locate
                                                                __loc__start__buf
                                                                __loc__start__pos
                                                                __loc__end__buf
                                                                __loc__end__pos in
                                                            fun e ->
                                                              apply_expr_filter
                                                                config e _loc)))))
                                       []
                                   else [])
                                  (List.append
                                     (if config.entry = Int
                                      then
                                        List.cons
                                          (Earley_core.Earley.fsequence_ignore
                                             (Earley_core.Earley.char '{' '{')
                                             (Earley_core.Earley.fsequence
                                                int
                                                (Earley_core.Earley.fsequence_ignore
                                                   (Earley_core.Earley.char
                                                      '}' '}')
                                                   (Earley_core.Earley.empty_pos
                                                      (fun __loc__start__buf
                                                         ->
                                                         fun
                                                           __loc__start__pos
                                                           ->
                                                           fun
                                                             __loc__end__buf
                                                             ->
                                                             fun
                                                               __loc__end__pos
                                                               ->
                                                               let _loc =
                                                                 locate
                                                                   __loc__start__buf
                                                                   __loc__start__pos
                                                                   __loc__end__buf
                                                                   __loc__end__pos in
                                                               fun e ->
                                                                 apply_expr_filter
                                                                   config
                                                                   (Pa_ast.exp_int
                                                                    _loc e)
                                                                   _loc)))))
                                          []
                                      else [])
                                     (List.append
                                        (if config.entry = Float
                                         then
                                           List.cons
                                             (Earley_core.Earley.fsequence_ignore
                                                (Earley_core.Earley.char '{'
                                                   '{')
                                                (Earley_core.Earley.fsequence
                                                   float
                                                   (Earley_core.Earley.fsequence_ignore
                                                      (Earley_core.Earley.char
                                                         '}' '}')
                                                      (Earley_core.Earley.empty_pos
                                                         (fun
                                                            __loc__start__buf
                                                            ->
                                                            fun
                                                              __loc__start__pos
                                                              ->
                                                              fun
                                                                __loc__end__buf
                                                                ->
                                                                fun
                                                                  __loc__end__pos
                                                                  ->
                                                                  let _loc =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                  fun e ->
                                                                    apply_expr_filter
                                                                    config
                                                                    (Pa_ast.exp_float
                                                                    _loc e)
                                                                    _loc)))))
                                             []
                                         else [])
                                        (List.append
                                           (if config.entry <> CamlStruct
                                            then
                                              List.cons
                                                (Earley_core.Earley.fsequence
                                                   wrapped_caml_expr
                                                   (Earley_core.Earley.empty_pos
                                                      (fun __loc__start__buf
                                                         ->
                                                         fun
                                                           __loc__start__pos
                                                           ->
                                                           fun
                                                             __loc__end__buf
                                                             ->
                                                             fun
                                                               __loc__end__pos
                                                               ->
                                                               let _loc =
                                                                 locate
                                                                   __loc__start__buf
                                                                   __loc__start__pos
                                                                   __loc__end__buf
                                                                   __loc__end__pos in
                                                               fun e ->
                                                                 apply_expr_filter
                                                                   config e
                                                                   _loc))) []
                                            else [])
                                           (List.append
                                              (if config.entry <> CamlStruct
                                               then
                                                 List.cons
                                                   (Earley_core.Earley.fsequence
                                                      wrapped_caml_array
                                                      (Earley_core.Earley.empty_pos
                                                         (fun
                                                            __loc__start__buf
                                                            ->
                                                            fun
                                                              __loc__start__pos
                                                              ->
                                                              fun
                                                                __loc__end__buf
                                                                ->
                                                                fun
                                                                  __loc__end__pos
                                                                  ->
                                                                  let _loc =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                  fun e ->
                                                                    apply_expr_filter
                                                                    config
                                                                    (Pa_ast.exp_array
                                                                    _loc e)
                                                                    _loc)))
                                                   []
                                               else [])
                                              (List.append
                                                 (if
                                                    config.entry <>
                                                      CamlStruct
                                                  then
                                                    List.cons
                                                      (Earley_core.Earley.fsequence
                                                         wrapped_caml_list
                                                         (Earley_core.Earley.empty_pos
                                                            (fun
                                                               __loc__start__buf
                                                               ->
                                                               fun
                                                                 __loc__start__pos
                                                                 ->
                                                                 fun
                                                                   __loc__end__buf
                                                                   ->
                                                                   fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    apply_expr_filter
                                                                    config
                                                                    (Pa_ast.exp_list
                                                                    _loc e)
                                                                    _loc)))
                                                      []
                                                  else [])
                                                 (List.append
                                                    (if
                                                       config.entry =
                                                         CamlStruct
                                                     then
                                                       List.cons
                                                         (Earley_core.Earley.fsequence
                                                            wrapped_caml_structure
                                                            (Earley_core.Earley.empty_pos
                                                               (fun
                                                                  __loc__start__buf
                                                                  ->
                                                                  fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun s ->
                                                                    apply_struct_filter
                                                                    config s
                                                                    _loc)))
                                                         []
                                                     else [])
                                                    (List.append
                                                       (if
                                                          config.entry = Caml
                                                        then
                                                          List.cons
                                                            (Earley_core.Earley.fsequence_ignore
                                                               (Earley_core.Earley.char
                                                                  '{' '{')
                                                               (Earley_core.Earley.fsequence
                                                                  caml_expr
                                                                  (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '}' '}')
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    apply_expr_filter
                                                                    config e
                                                                    _loc)))))
                                                            []
                                                        else [])
                                                       (List.append
                                                          (if
                                                             config.entry =
                                                               CamlStruct
                                                           then
                                                             List.cons
                                                               (Earley_core.Earley.fsequence_ignore
                                                                  (Earley_core.Earley.char
                                                                    '{' '{')
                                                                  (Earley_core.Earley.fsequence
                                                                    caml_structure
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '}' '}')
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun s ->
                                                                    apply_struct_filter
                                                                    config s
                                                                    _loc)))))
                                                               []
                                                           else []) []))))))))))))))))
    let simple_text_macro_argument =
      Earley_core.Earley.declare_grammar "simple_text_macro_argument"
    let _ =
      Earley_core.Earley.set_grammar simple_text_macro_argument
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence wrapped_caml_list
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun e -> Pa_ast.exp_list _loc e)))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '{' '{')
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.greedy
                          (Earley_core.Earley.option None
                             (Earley_core.Earley.apply (fun x -> Some x)
                                simple_text)))
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '}' '}')
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun l ->
                                        match l with
                                        | Some l -> l
                                        | None -> Pa_ast.exp_Nil _loc)))))
                 (List.cons wrapped_caml_expr
                    (List.cons
                       (Earley_core.Earley.fsequence wrapped_caml_array
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun e -> Pa_ast.exp_array _loc e))) [])))))
    let simple_math_macro_argument =
      Earley_core.Earley.declare_grammar "simple_math_macro_argument"
    let _ =
      Earley_core.Earley.set_grammar simple_math_macro_argument
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence wrapped_caml_list
                 (Earley_core.Earley.empty_pos
                    (fun __loc__start__buf ->
                       fun __loc__start__pos ->
                         fun __loc__end__buf ->
                           fun __loc__end__pos ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             fun e -> Pa_ast.exp_list _loc e)))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '{' '{')
                    (Earley_core.Earley.fsequence
                       (change_layout math_toplevel blank2)
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '}' '}')
                          (Earley_core.Earley.empty (fun m -> m)))))
                 (List.cons wrapped_caml_expr
                    (List.cons
                       (Earley_core.Earley.fsequence wrapped_caml_array
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun e -> Pa_ast.exp_array _loc e))) [])))))
    let (macro_arguments_aux, macro_arguments_aux__set__grammar) =
      Earley_core.Earley.grammar_family "macro_arguments_aux"
    let _ =
      macro_arguments_aux__set__grammar
        (fun l ->
           Earley_core.Earley.alternatives
             (List.append
                (if l <> []
                 then
                   List.cons
                     (Earley_core.Earley.fsequence
                        (macro_argument (List.hd l))
                        (Earley_core.Earley.fsequence
                           (macro_arguments_aux (List.tl l))
                           (Earley_core.Earley.empty
                              (fun args -> fun arg -> arg :: args)))) []
                 else [])
                (List.append
                   (if l = []
                    then
                      List.cons
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.empty ())
                           (Earley_core.Earley.empty [])) []
                    else []) [])))
    let macro_arguments current config =
      match ((macro_args config), current) with
      | (None, Math) ->
          Earley_core.Earley.greedy
            (Earley_core.Earley.apply (fun f -> f [])
               (Earley_core.Earley.fixpoint' (fun l -> l)
                  simple_math_macro_argument
                  (fun x -> fun f -> fun l -> f (List.cons x l))))
      | (None, Text) ->
          Earley_core.Earley.greedy
            (Earley_core.Earley.apply (fun f -> f [])
               (Earley_core.Earley.fixpoint' (fun l -> l)
                  simple_text_macro_argument
                  (fun x -> fun f -> fun l -> f (List.cons x l))))
      | (None, _) -> assert false
      | (Some l, _) ->
          let l =
            List.map
              (fun s ->
                 if s.entry = Current then { s with entry = current } else s)
              l in
          macro_arguments_aux l
    let hash_sym = Hashtbl.create 1001
    let count_sym = ref 0
    let hash_msym = Hashtbl.create 1001
    let count_msym = ref 0
    let mcache_buf = ref []
    let cache = ref ""
    let cache_buf = ref []
    let print_math_symbol _loc sym =
      let (s, b) =
        match sym with
        | SimpleSym s -> ((maths_glyphs_str _loc s), false)
        | CamlSym s -> (s, false)
        | MultiSym s -> (s, true)
        | Invisible -> ((maths_glyphs_str _loc "invisible"), false) in
      if b
      then
        (if (!cache) = ""
         then s
         else
           (try
              let nom = "m" ^ (!cache) in
              let index = Hashtbl.find hash_msym s in
              array_get_lid _loc nom index
            with
            | Not_found ->
                (Hashtbl.add hash_msym s (!count_msym);
                 mcache_buf := (s :: (!mcache_buf));
                 (let res = array_get_lid _loc ("m" ^ (!cache)) (!count_msym) in
                  let _ = incr count_msym in res))))
      else
        if (!cache) = ""
        then s
        else
          (try
             let r = Hashtbl.find hash_sym s in array_get_lid _loc (!cache) r
           with
           | Not_found ->
               (Hashtbl.add hash_sym s (!count_sym);
                cache_buf := (s :: (!cache_buf));
                (let res = array_get_lid _loc (!cache) (!count_sym) in
                 let _ = incr count_sym in res)))
    let print_ordinary_math_symbol _loc sym =
      Pa_ast.exp_list _loc
        [Pa_ast.exp_const_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "Ordinary" }) })
           (Some
              (Pa_ast.exp_apply1 _loc
                 (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "node" }) }))
                 (print_math_symbol _loc sym)))]
    let print_math_deco_sym _loc elt ind =
      if ind = no_ind
      then
        Pa_ast.exp_apply1 _loc
          (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "node" }) }))
          (print_math_symbol _loc elt)
      else
        (let r : (Longident.t Location.loc * Parsetree.expression) list ref =
           ref [] in
         (match ind.up_right with
          | Some i ->
              (if ind.up_right_same_script
               then r := ((maths_field_super_right_same_script _loc) :: (!r));
               r := ((maths_field_superscript_right _loc i) :: (!r)))
          | _ -> ());
         (match ind.down_right with
          | Some i -> r := ((maths_field_subscript_right _loc i) :: (!r))
          | _ -> ());
         (match ind.up_left with
          | Some i ->
              (if ind.up_left_same_script
               then r := ((maths_field_super_left_same_script _loc) :: (!r));
               r := ((maths_field_superscript_left _loc i) :: (!r)))
          | _ -> ());
         (match ind.down_left with
          | Some i -> r := ((maths_field_subscript_left _loc i) :: (!r))
          | _ -> ());
         (let node_e =
            Pa_ast.exp_apply1 _loc
              (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "node" }) }))
              (print_math_symbol _loc elt) in
          Pa_ast.loc_expr _loc (Parsetree.Pexp_record ((!r), (Some node_e)))))
    let print_math_deco _loc elt ind =
      if ind = no_ind
      then elt
      else
        (let r : (Longident.t Location.loc * Parsetree.expression) list ref =
           ref [] in
         (match ind.up_right with
          | Some i ->
              (if ind.up_right_same_script
               then r := ((maths_field_super_right_same_script _loc) :: (!r));
               r := ((maths_field_superscript_right _loc i) :: (!r)))
          | _ -> ());
         (match ind.down_right with
          | Some i -> r := ((maths_field_subscript_right _loc i) :: (!r))
          | _ -> ());
         (match ind.up_left with
          | Some i ->
              (if ind.up_left_same_script
               then r := ((maths_field_super_left_same_script _loc) :: (!r));
               r := ((maths_field_superscript_left _loc i) :: (!r)))
          | _ -> ());
         (match ind.down_left with
          | Some i -> r := ((maths_field_subscript_left _loc i) :: (!r))
          | _ -> ());
         print_math_deco_finish _loc (!r) elt)
    let add_reserved sym_names =
      let insert map name =
        let len = String.length name in
        if (len > 0) && ((name.[0]) = '\\')
        then
          let name = String.sub name 1 (len - 1) in
          PrefixTree.add name () map
        else map in
      state.reserved_symbols <-
        (List.fold_left insert state.reserved_symbols sym_names);
      local_state.reserved_symbols <-
        (List.fold_left insert local_state.reserved_symbols sym_names)
    let new_infix_symbol _loc infix_prio sym_names infix_value =
      let (infix_macro_names, infix_utf8_names) = symbol sym_names in
      let infix_no_left_space = infix_prio = Punc in
      let infix_no_right_space = false in
      let infix_space =
        match infix_prio with
        | Sum -> 2
        | Prod -> 3
        | Rel|Punc -> 1
        | Conj|Impl -> 0
        | _ -> assert false in
      let sym =
        {
          infix_prio;
          infix_macro_names;
          infix_utf8_names;
          infix_value;
          infix_space;
          infix_no_left_space;
          infix_no_right_space
        } in
      quail_out infix_macro_names infix_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.infix_symbols <-
         (List.fold_left insert state.infix_symbols sym_names);
       local_state.infix_symbols <-
         (List.fold_left insert local_state.infix_symbols sym_names);
       (let asym =
          {
            symbol_macro_names = infix_macro_names;
            symbol_utf8_names = infix_utf8_names;
            symbol_value = infix_value
          } in
        let insert map name = PrefixTree.add name asym map in
        state.any_symbols <-
          (List.fold_left insert state.any_symbols sym_names);
        local_state.any_symbols <-
          (List.fold_left insert local_state.any_symbols sym_names);
        add_reserved sym_names;
        if state.verbose
        then
          (let sym = print_ordinary_math_symbol _loc infix_value in
           let showuname _ = sym in
           let showmname m = print_ordinary_math_symbol _loc (SimpleSym m) in
           let unames = List.map showuname infix_utf8_names in
           let mnames = List.map showmname infix_macro_names in
           symbol_paragraph _loc sym (math_list _loc (unames @ mnames)))
        else []))
    let new_symbol _loc sym_names symbol_value =
      let (symbol_macro_names, symbol_utf8_names) = symbol sym_names in
      let sym = { symbol_macro_names; symbol_utf8_names; symbol_value } in
      quail_out symbol_macro_names symbol_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.atom_symbols <-
         (List.fold_left insert state.atom_symbols sym_names);
       local_state.atom_symbols <-
         (List.fold_left insert local_state.atom_symbols sym_names);
       add_reserved sym_names;
       if state.verbose
       then
         (let sym_val = print_ordinary_math_symbol _loc symbol_value in
          let sym s =
            print_ordinary_math_symbol _loc
              (CamlSym (maths_glyphs_str _loc s)) in
          let names =
            (List.map sym symbol_macro_names) @
              (List.map (fun _ -> sym_val) symbol_utf8_names) in
          symbol_paragraph _loc sym_val (math_list _loc names))
       else [])
    let new_accent_symbol _loc sym_names symbol_value =
      let (symbol_macro_names, symbol_utf8_names) = symbol sym_names in
      let sym = { symbol_macro_names; symbol_utf8_names; symbol_value } in
      quail_out symbol_macro_names symbol_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.accent_symbols <-
         (List.fold_left insert state.accent_symbols sym_names);
       local_state.accent_symbols <-
         (List.fold_left insert local_state.accent_symbols sym_names);
       add_reserved sym_names;
       if state.verbose
       then
         (let sym_val = print_ordinary_math_symbol _loc symbol_value in
          let sym s =
            print_ordinary_math_symbol _loc
              (CamlSym (maths_glyphs_str _loc s)) in
          let names =
            (List.map sym symbol_macro_names) @
              (List.map (fun _ -> sym_val) symbol_utf8_names) in
          symbol_paragraph _loc sym_val (math_list _loc names))
       else [])
    let new_prefix_symbol _loc sym_names prefix_value =
      let (prefix_macro_names, prefix_utf8_names) = symbol sym_names in
      let sym =
        {
          prefix_prio = IProd;
          prefix_space = 3;
          prefix_no_space = false;
          prefix_macro_names;
          prefix_utf8_names;
          prefix_value
        } in
      quail_out prefix_macro_names prefix_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.prefix_symbols <-
         (List.fold_left insert state.prefix_symbols sym_names);
       local_state.prefix_symbols <-
         (List.fold_left insert local_state.prefix_symbols sym_names);
       (let asym =
          {
            symbol_macro_names = prefix_macro_names;
            symbol_utf8_names = prefix_utf8_names;
            symbol_value = prefix_value
          } in
        let insert map name = PrefixTree.add name asym map in
        state.any_symbols <-
          (List.fold_left insert state.any_symbols sym_names);
        local_state.any_symbols <-
          (List.fold_left insert local_state.any_symbols sym_names);
        add_reserved sym_names;
        if state.verbose
        then
          (let sym_val = print_ordinary_math_symbol _loc prefix_value in
           let sym s =
             print_ordinary_math_symbol _loc
               (CamlSym (maths_glyphs_str _loc s)) in
           let names =
             (List.map sym prefix_macro_names) @
               (List.map (fun _ -> sym_val) prefix_utf8_names) in
           symbol_paragraph _loc sym_val (math_list _loc names))
        else []))
    let new_postfix_symbol _loc sym_names postfix_value =
      let (postfix_macro_names, postfix_utf8_names) = symbol sym_names in
      let sym =
        {
          postfix_prio = Prod;
          postfix_space = 3;
          postfix_no_space = false;
          postfix_macro_names;
          postfix_utf8_names;
          postfix_value
        } in
      quail_out postfix_macro_names postfix_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.postfix_symbols <-
         (List.fold_left insert state.postfix_symbols sym_names);
       local_state.postfix_symbols <-
         (List.fold_left insert local_state.postfix_symbols sym_names);
       (let asym =
          {
            symbol_macro_names = postfix_macro_names;
            symbol_utf8_names = postfix_utf8_names;
            symbol_value = postfix_value
          } in
        let insert map name = PrefixTree.add name asym map in
        state.any_symbols <-
          (List.fold_left insert state.any_symbols sym_names);
        local_state.any_symbols <-
          (List.fold_left insert local_state.any_symbols sym_names);
        add_reserved sym_names;
        if state.verbose
        then
          (let sym_val = print_ordinary_math_symbol _loc postfix_value in
           let sym s =
             print_ordinary_math_symbol _loc
               (CamlSym (maths_glyphs_str _loc s)) in
           let names =
             (List.map sym postfix_macro_names) @
               (List.map (fun _ -> sym_val) postfix_utf8_names) in
           symbol_paragraph _loc sym_val (math_list _loc names))
        else []))
    let new_quantifier_symbol _loc sym_names symbol_value =
      let (symbol_macro_names, symbol_utf8_names) = symbol sym_names in
      let sym = { symbol_macro_names; symbol_utf8_names; symbol_value } in
      quail_out symbol_macro_names symbol_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.quantifier_symbols <-
         (List.fold_left insert state.quantifier_symbols sym_names);
       local_state.quantifier_symbols <-
         (List.fold_left insert local_state.quantifier_symbols sym_names);
       state.any_symbols <-
         (List.fold_left insert state.any_symbols sym_names);
       local_state.any_symbols <-
         (List.fold_left insert local_state.any_symbols sym_names);
       add_reserved sym_names;
       if state.verbose
       then
         (let sym_val = print_ordinary_math_symbol _loc symbol_value in
          let sym s =
            print_ordinary_math_symbol _loc
              (CamlSym (maths_glyphs_str _loc s)) in
          let names =
            (List.map sym symbol_macro_names) @
              (List.map (fun _ -> sym_val) symbol_utf8_names) in
          symbol_paragraph _loc sym_val (math_list _loc names))
       else [])
    let new_left_delimiter _loc sym_names delimiter_values =
      let (delimiter_macro_names, delimiter_utf8_names) = symbol sym_names in
      let sym =
        { delimiter_macro_names; delimiter_utf8_names; delimiter_values } in
      quail_out delimiter_macro_names delimiter_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.left_delimiter_symbols <-
         (List.fold_left insert state.left_delimiter_symbols sym_names);
       local_state.left_delimiter_symbols <-
         (List.fold_left insert local_state.left_delimiter_symbols sym_names);
       add_reserved sym_names;
       if state.verbose
       then
         (let syms = ordinary_multi_glyphs_node _loc delimiter_values in
          let sym_val = ordinary_hd_glyphs_node _loc delimiter_values in
          let sym s =
            print_ordinary_math_symbol _loc
              (CamlSym (maths_glyphs_str _loc s)) in
          let names =
            (List.map sym delimiter_macro_names) @
              (List.map (fun _ -> sym_val) delimiter_utf8_names) in
          symbol_paragraph _loc syms (math_list _loc names))
       else [])
    let new_right_delimiter _loc sym_names delimiter_values =
      let (delimiter_macro_names, delimiter_utf8_names) = symbol sym_names in
      let sym =
        { delimiter_macro_names; delimiter_utf8_names; delimiter_values } in
      quail_out delimiter_macro_names delimiter_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.right_delimiter_symbols <-
         (List.fold_left insert state.right_delimiter_symbols sym_names);
       local_state.right_delimiter_symbols <-
         (List.fold_left insert local_state.right_delimiter_symbols sym_names);
       add_reserved sym_names;
       if state.verbose
       then
         (let syms = ordinary_multi_glyphs_node _loc delimiter_values in
          let sym_val = ordinary_hd_glyphs_node _loc delimiter_values in
          let sym s =
            print_ordinary_math_symbol _loc
              (CamlSym (maths_glyphs_str _loc s)) in
          let names =
            (List.map sym delimiter_macro_names) @
              (List.map (fun _ -> sym_val) delimiter_utf8_names) in
          symbol_paragraph _loc syms (math_list _loc names))
       else [])
    let new_operator_symbol _loc operator_kind sym_names operator_values =
      let (operator_macro_names, operator_utf8_names) = symbol sym_names in
      let operator_prio = Operator in
      let sym =
        {
          operator_prio;
          operator_kind;
          operator_macro_names;
          operator_utf8_names;
          operator_values
        } in
      quail_out operator_macro_names operator_utf8_names;
      (let insert map name = PrefixTree.add name sym map in
       state.operator_symbols <-
         (List.fold_left insert state.operator_symbols sym_names);
       local_state.operator_symbols <-
         (List.fold_left insert local_state.operator_symbols sym_names);
       (let asym =
          {
            symbol_macro_names = operator_macro_names;
            symbol_utf8_names = operator_utf8_names;
            symbol_value =
              (CamlSym
                 (Pa_ast.exp_apply1 _loc
                    (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "List" }, { Location.loc = _loc; txt = "hd" }) }))
                    operator_values))
          } in
        let insert map name = PrefixTree.add name asym map in
        state.any_symbols <-
          (List.fold_left insert state.any_symbols sym_names);
        local_state.any_symbols <-
          (List.fold_left insert local_state.any_symbols sym_names);
        add_reserved sym_names;
        if state.verbose
        then
          (let syms = ordinary_multi_glyphs_node _loc operator_values in
           let sym_val = ordinary_hd_glyphs_node _loc operator_values in
           let sym s =
             print_ordinary_math_symbol _loc
               (CamlSym (maths_glyphs_str _loc s)) in
           let names =
             (List.map sym operator_macro_names) @
               (List.map (fun _ -> sym_val) operator_utf8_names) in
           symbol_paragraph _loc syms (math_list _loc names))
        else []))
    let new_combining_symbol _loc uchr macro =
      let _parse_sym = string uchr () in
      state.combining_symbols <-
        (PrefixTree.add uchr macro state.combining_symbols);
      local_state.combining_symbols <-
        (PrefixTree.add uchr macro local_state.combining_symbols);
      if state.verbose
      then
        (let sym =
           ordinary_list_node _loc
             (Pa_ast.exp_apply1 _loc
                (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "node" }) }))
                (maths_glyphs_str _loc uchr)) in
         let macro = "\\" ^ macro in
         let macro =
           ordinary_list_node _loc
             (Pa_ast.exp_apply1 _loc
                (Pa_ast.exp_ident_longident _loc ({ Location.loc = _loc; txt = Longident.Ldot ({ Location.loc = _loc; txt = Longident.Lident "Maths" }, { Location.loc = _loc; txt = "node" }) }))
                (maths_glyphs_str _loc macro)) in
         symbol_paragraph _loc sym macro)
      else []
    let symbol_def = Earley_core.Earley.declare_grammar "symbol_def"
    let _ =
      Earley_core.Earley.set_grammar symbol_def
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "\\Add_combining"
                    "\\Add_combining")
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "{" "{")
                    (Earley_core.Earley.fsequence uchar
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "}" "}")
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "{" "{")
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string "\\" "\\")
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.no_blank_test ())
                                   (Earley_core.Earley.fsequence lid
                                      (Earley_core.Earley.fsequence_ignore
                                         (Earley_core.Earley.string "}" "}")
                                         (Earley_core.Earley.empty_pos
                                            (fun __loc__start__buf ->
                                               fun __loc__start__pos ->
                                                 fun __loc__end__buf ->
                                                   fun __loc__end__pos ->
                                                     let _loc =
                                                       locate
                                                         __loc__start__buf
                                                         __loc__start__pos
                                                         __loc__end__buf
                                                         __loc__end__pos in
                                                     fun m ->
                                                       fun c ->
                                                         new_combining_symbol
                                                           _loc c m)))))))))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "\\Configure_math_macro"
                       "\\Configure_math_macro")
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "{" "{")
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.option None
                             (Earley_core.Earley.apply (fun x -> Some x)
                                (Earley_core.Earley.string "\\" "\\")))
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.no_blank_test ())
                             (Earley_core.Earley.fsequence lid
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.string "}" "}")
                                   (Earley_core.Earley.fsequence configs
                                      (Earley_core.Earley.empty
                                         (fun cs ->
                                            fun id ->
                                              fun _default_0 ->
                                                state.math_macros <-
                                                  ((id, cs) ::
                                                  (state.math_macros));
                                                local_state.math_macros <-
                                                  ((id, cs) ::
                                                  (local_state.math_macros));
                                                [])))))))))
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "\\Configure_word_macro"
                          "\\Configure_word_macro")
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "{" "{")
                          (Earley_core.Earley.fsequence
                             (Earley_core.Earley.option None
                                (Earley_core.Earley.apply (fun x -> Some x)
                                   (Earley_core.Earley.string "\\" "\\")))
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.no_blank_test ())
                                (Earley_core.Earley.fsequence lid
                                   (Earley_core.Earley.fsequence_ignore
                                      (Earley_core.Earley.string "}" "}")
                                      (Earley_core.Earley.fsequence configs
                                         (Earley_core.Earley.empty
                                            (fun cs ->
                                               fun id ->
                                                 fun _default_0 ->
                                                   state.word_macros <-
                                                     ((id, cs) ::
                                                     (state.word_macros));
                                                   local_state.word_macros <-
                                                     ((id, cs) ::
                                                     (local_state.word_macros));
                                                   [])))))))))
                    (List.cons
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string
                             "\\Configure_environment"
                             "\\Configure_environment")
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "{" "{")
                             (Earley_core.Earley.fsequence lid
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.string "}" "}")
                                   (Earley_core.Earley.fsequence configs
                                      (Earley_core.Earley.empty
                                         (fun cs ->
                                            fun id ->
                                              state.environment <- ((id, cs)
                                                :: (state.environment));
                                              local_state.environment <-
                                                ((id, cs) ::
                                                (local_state.environment));
                                              [])))))))
                       (List.cons
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "\\Verbose_Changes"
                                "\\Verbose_Changes")
                             (Earley_core.Earley.empty
                                (state.verbose <- true;
                                 local_state.verbose <- true;
                                 [])))
                          (List.cons
                             (Earley_core.Earley.fsequence_ignore
                                (Earley_core.Earley.string "\\Save_Grammar"
                                   "\\Save_Grammar")
                                (Earley_core.Earley.empty
                                   (build_grammar (); [])))
                             (List.cons
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.string
                                      "\\Add_relation" "\\Add_relation")
                                   (Earley_core.Earley.fsequence symbols
                                      (Earley_core.Earley.fsequence
                                         symbol_value
                                         (Earley_core.Earley.empty_pos
                                            (fun __loc__start__buf ->
                                               fun __loc__start__pos ->
                                                 fun __loc__end__buf ->
                                                   fun __loc__end__pos ->
                                                     let _loc =
                                                       locate
                                                         __loc__start__buf
                                                         __loc__start__pos
                                                         __loc__end__buf
                                                         __loc__end__pos in
                                                     fun e ->
                                                       fun ss ->
                                                         new_infix_symbol
                                                           _loc Rel ss e)))))
                                (List.cons
                                   (Earley_core.Earley.fsequence_ignore
                                      (Earley_core.Earley.string
                                         "\\Add_addition_like"
                                         "\\Add_addition_like")
                                      (Earley_core.Earley.fsequence symbols
                                         (Earley_core.Earley.fsequence
                                            symbol_value
                                            (Earley_core.Earley.empty_pos
                                               (fun __loc__start__buf ->
                                                  fun __loc__start__pos ->
                                                    fun __loc__end__buf ->
                                                      fun __loc__end__pos ->
                                                        let _loc =
                                                          locate
                                                            __loc__start__buf
                                                            __loc__start__pos
                                                            __loc__end__buf
                                                            __loc__end__pos in
                                                        fun e ->
                                                          fun ss ->
                                                            new_infix_symbol
                                                              _loc Sum ss e)))))
                                   (List.cons
                                      (Earley_core.Earley.fsequence_ignore
                                         (Earley_core.Earley.string
                                            "\\Add_product_like"
                                            "\\Add_product_like")
                                         (Earley_core.Earley.fsequence
                                            symbols
                                            (Earley_core.Earley.fsequence
                                               symbol_value
                                               (Earley_core.Earley.empty_pos
                                                  (fun __loc__start__buf ->
                                                     fun __loc__start__pos ->
                                                       fun __loc__end__buf ->
                                                         fun __loc__end__pos
                                                           ->
                                                           let _loc =
                                                             locate
                                                               __loc__start__buf
                                                               __loc__start__pos
                                                               __loc__end__buf
                                                               __loc__end__pos in
                                                           fun e ->
                                                             fun ss ->
                                                               new_infix_symbol
                                                                 _loc Prod ss
                                                                 e)))))
                                      (List.cons
                                         (Earley_core.Earley.fsequence_ignore
                                            (Earley_core.Earley.string
                                               "\\Add_connector"
                                               "\\Add_connector")
                                            (Earley_core.Earley.fsequence
                                               symbols
                                               (Earley_core.Earley.fsequence
                                                  symbol_value
                                                  (Earley_core.Earley.empty_pos
                                                     (fun __loc__start__buf
                                                        ->
                                                        fun __loc__start__pos
                                                          ->
                                                          fun __loc__end__buf
                                                            ->
                                                            fun
                                                              __loc__end__pos
                                                              ->
                                                              let _loc =
                                                                locate
                                                                  __loc__start__buf
                                                                  __loc__start__pos
                                                                  __loc__end__buf
                                                                  __loc__end__pos in
                                                              fun e ->
                                                                fun ss ->
                                                                  new_infix_symbol
                                                                    _loc Conj
                                                                    ss e)))))
                                         (List.cons
                                            (Earley_core.Earley.fsequence_ignore
                                               (Earley_core.Earley.string
                                                  "\\Add_arrow" "\\Add_arrow")
                                               (Earley_core.Earley.fsequence
                                                  symbols
                                                  (Earley_core.Earley.fsequence
                                                     symbol_value
                                                     (Earley_core.Earley.empty_pos
                                                        (fun
                                                           __loc__start__buf
                                                           ->
                                                           fun
                                                             __loc__start__pos
                                                             ->
                                                             fun
                                                               __loc__end__buf
                                                               ->
                                                               fun
                                                                 __loc__end__pos
                                                                 ->
                                                                 let _loc =
                                                                   locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                 fun e ->
                                                                   fun ss ->
                                                                    new_infix_symbol
                                                                    _loc Impl
                                                                    ss e)))))
                                            (List.cons
                                               (Earley_core.Earley.fsequence_ignore
                                                  (Earley_core.Earley.string
                                                     "\\Add_punctuation"
                                                     "\\Add_punctuation")
                                                  (Earley_core.Earley.fsequence
                                                     symbols
                                                     (Earley_core.Earley.fsequence
                                                        symbol_value
                                                        (Earley_core.Earley.empty_pos
                                                           (fun
                                                              __loc__start__buf
                                                              ->
                                                              fun
                                                                __loc__start__pos
                                                                ->
                                                                fun
                                                                  __loc__end__buf
                                                                  ->
                                                                  fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_infix_symbol
                                                                    _loc Punc
                                                                    ss e)))))
                                               (List.cons
                                                  (Earley_core.Earley.fsequence_ignore
                                                     (Earley_core.Earley.string
                                                        "\\Add_quantifier"
                                                        "\\Add_quantifier")
                                                     (Earley_core.Earley.fsequence
                                                        symbols
                                                        (Earley_core.Earley.fsequence
                                                           symbol_value
                                                           (Earley_core.Earley.empty_pos
                                                              (fun
                                                                 __loc__start__buf
                                                                 ->
                                                                 fun
                                                                   __loc__start__pos
                                                                   ->
                                                                   fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_quantifier_symbol
                                                                    _loc ss e)))))
                                                  (List.cons
                                                     (Earley_core.Earley.fsequence_ignore
                                                        (Earley_core.Earley.string
                                                           "\\Add_prefix"
                                                           "\\Add_prefix")
                                                        (Earley_core.Earley.fsequence
                                                           symbols
                                                           (Earley_core.Earley.fsequence
                                                              symbol_value
                                                              (Earley_core.Earley.empty_pos
                                                                 (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_prefix_symbol
                                                                    _loc ss e)))))
                                                     (List.cons
                                                        (Earley_core.Earley.fsequence_ignore
                                                           (Earley_core.Earley.string
                                                              "\\Add_postfix"
                                                              "\\Add_postfix")
                                                           (Earley_core.Earley.fsequence
                                                              symbols
                                                              (Earley_core.Earley.fsequence
                                                                 symbol_value
                                                                 (Earley_core.Earley.empty_pos
                                                                    (
                                                                    fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_postfix_symbol
                                                                    _loc ss e)))))
                                                        (List.cons
                                                           (Earley_core.Earley.fsequence_ignore
                                                              (Earley_core.Earley.string
                                                                 "\\Add_accent"
                                                                 "\\Add_accent")
                                                              (Earley_core.Earley.fsequence
                                                                 symbols
                                                                 (Earley_core.Earley.fsequence
                                                                    symbol_value
                                                                    (
                                                                    Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_accent_symbol
                                                                    _loc ss e)))))
                                                           (List.cons
                                                              (Earley_core.Earley.fsequence_ignore
                                                                 (Earley_core.Earley.string
                                                                    "\\Add_symbol"
                                                                    "\\Add_symbol")
                                                                 (Earley_core.Earley.fsequence
                                                                    symbols
                                                                    (
                                                                    Earley_core.Earley.fsequence
                                                                    symbol_value
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_symbol
                                                                    _loc ss e)))))
                                                              (List.cons
                                                                 (Earley_core.Earley.fsequence_ignore
                                                                    (
                                                                    Earley_core.Earley.string
                                                                    "\\Add_operator"
                                                                    "\\Add_operator")
                                                                    (
                                                                    Earley_core.Earley.fsequence
                                                                    symbols
                                                                    (Earley_core.Earley.fsequence
                                                                    symbol_values
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_operator_symbol
                                                                    _loc
                                                                    NoLimits
                                                                    ss e)))))
                                                                 (List.cons
                                                                    (
                                                                    Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.string
                                                                    "\\Add_limits_operator"
                                                                    "\\Add_limits_operator")
                                                                    (Earley_core.Earley.fsequence
                                                                    symbols
                                                                    (Earley_core.Earley.fsequence
                                                                    symbol_values
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_operator_symbol
                                                                    _loc
                                                                    Limits ss
                                                                    e)))))
                                                                    (
                                                                    List.cons
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.string
                                                                    "\\Add_left"
                                                                    "\\Add_left")
                                                                    (Earley_core.Earley.fsequence
                                                                    symbols
                                                                    (Earley_core.Earley.fsequence
                                                                    symbol_values
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_left_delimiter
                                                                    _loc ss e)))))
                                                                    (List.cons
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.string
                                                                    "\\Add_right"
                                                                    "\\Add_right")
                                                                    (Earley_core.Earley.fsequence
                                                                    symbols
                                                                    (Earley_core.Earley.fsequence
                                                                    symbol_values
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun e ->
                                                                    fun ss ->
                                                                    new_right_delimiter
                                                                    _loc ss e)))))
                                                                    []))))))))))))))))))))))
    type indice_height =
      | Up 
      | Down 
    let left_indices = Earley_core.Earley.declare_grammar "left_indices"
    let _ =
      Earley_core.Earley.set_grammar left_indices
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "^^" "^^")
                 (Earley_core.Earley.empty Up))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "__" "__")
                    (Earley_core.Earley.empty Down)) [])))
    let right_indices = Earley_core.Earley.declare_grammar "right_indices"
    let _ =
      Earley_core.Earley.set_grammar right_indices
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "^" "^")
                 (Earley_core.Earley.empty Up))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "_" "_")
                    (Earley_core.Earley.empty Down)) [])))
    let any_symbol = Earley_core.Earley.declare_grammar "any_symbol"
    let _ =
      Earley_core.Earley.set_grammar any_symbol
        (Earley_core.Earley.fsequence math_any_symbol
           (Earley_core.Earley.empty (fun sym -> sym.symbol_value)))
    let merge_indices indices ind =
      assert (ind.down_left = None);
      assert (ind.up_left = None);
      if
        ((indices.down_right <> None) && (ind.down_right <> None)) ||
          ((indices.up_right <> None) && (ind.up_right <> None))
      then give_up ();
      {
        indices with
        down_right =
          (if ind.down_right <> None
           then ind.down_right
           else indices.down_right);
        up_right =
          (if ind.up_right <> None then ind.up_right else indices.up_right)
      }
    let (math_aux, math_aux__set__grammar) =
      Earley_core.Earley.grammar_family "math_aux"
    let (math_prefix, math_prefix__set__grammar) =
      Earley_core.Earley.grammar_family "math_prefix"
    let (math_postfix, math_postfix__set__grammar) =
      Earley_core.Earley.grammar_family "math_postfix"
    let with_indices = Earley_core.Earley.declare_grammar "with_indices"
    let math_punc_list = Earley_core.Earley.declare_grammar "math_punc_list"
    let long_math_declaration =
      Earley_core.Earley.declare_grammar "long_math_declaration"
    let math_declaration =
      Earley_core.Earley.declare_grammar "math_declaration"
    let _ =
      math_aux__set__grammar
        (fun prio ->
           Earley_core.Earley.alternatives
             (List.append
                (if prio = LInd
                 then
                   List.cons
                     (Earley_core.Earley.fsequence any_symbol
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.no_blank_test ())
                           (Earley_core.Earley.fsequence left_indices
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.no_blank_test ())
                                 (Earley_core.Earley.fsequence
                                    (math_aux LInd)
                                    (Earley_core.Earley.empty_pos
                                       (fun __loc__start__buf ->
                                          fun __loc__start__pos ->
                                            fun __loc__end__buf ->
                                              fun __loc__end__pos ->
                                                let _loc =
                                                  locate __loc__start__buf
                                                    __loc__start__pos
                                                    __loc__end__buf
                                                    __loc__end__pos in
                                                fun r ->
                                                  fun h ->
                                                    fun s ->
                                                      let s =
                                                        print_ordinary_math_symbol
                                                          _loc s in
                                                      fun indices ->
                                                        match h with
                                                        | Down ->
                                                            (if
                                                               indices.down_left
                                                                 <> None
                                                             then give_up ();
                                                             r
                                                               {
                                                                 indices with
                                                                 down_left =
                                                                   (Some s)
                                                               })
                                                        | Up ->
                                                            (if
                                                               indices.up_left
                                                                 <> None
                                                             then give_up ();
                                                             r
                                                               {
                                                                 indices with
                                                                 up_left =
                                                                   (Some s)
                                                               })))))))) []
                 else [])
                (List.append
                   (if prio <> AtomM
                    then List.cons (math_prefix (next_prio prio)) []
                    else [])
                   (List.append
                      (if prio = Operator
                       then
                         List.cons
                           (Earley_core.Earley.fsequence_position
                              math_quantifier_symbol
                              (Earley_core.Earley.fsequence with_indices
                                 (Earley_core.Earley.fsequence
                                    math_declaration
                                    (Earley_core.Earley.fsequence_position
                                       (Earley_core.Earley.option None
                                          (Earley_core.Earley.apply
                                             (fun x -> Some x)
                                             math_punctuation_symbol))
                                       (Earley_core.Earley.fsequence
                                          (math_aux prio)
                                          (Earley_core.Earley.empty_pos
                                             (fun __loc__start__buf ->
                                                fun __loc__start__pos ->
                                                  fun __loc__end__buf ->
                                                    fun __loc__end__pos ->
                                                      let _loc =
                                                        locate
                                                          __loc__start__buf
                                                          __loc__start__pos
                                                          __loc__end__buf
                                                          __loc__end__pos in
                                                      fun m ->
                                                        fun str1 ->
                                                          fun pos1 ->
                                                            fun str2 ->
                                                              fun pos2 ->
                                                                fun p ->
                                                                  let _loc_p
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                  fun d ->
                                                                    fun ind
                                                                    ->
                                                                    fun str1
                                                                    ->
                                                                    fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun sym
                                                                    ->
                                                                    let _loc_sym
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    let indices
                                                                    =
                                                                    merge_indices
                                                                    indices
                                                                    ind in
                                                                    let inter
                                                                    =
                                                                    match p
                                                                    with
                                                                    | 
                                                                    None ->
                                                                    maths_invisible
                                                                    _loc
                                                                    | 
                                                                    Some s ->
                                                                    let nsl =
                                                                    s.infix_no_left_space in
                                                                    let nsr =
                                                                    s.infix_no_right_space in
                                                                    let md =
                                                                    print_math_deco_sym
                                                                    _loc_p
                                                                    s.infix_value
                                                                    no_ind in
                                                                    maths_normal
                                                                    _loc nsl
                                                                    md nsr in
                                                                    let md =
                                                                    print_math_deco_sym
                                                                    _loc_sym
                                                                    sym.symbol_value
                                                                    indices in
                                                                    quantifier_nested_bin
                                                                    _loc md
                                                                    inter d
                                                                    (m no_ind))))))))
                           []
                       else [])
                      (List.cons
                         (Earley_core.Earley.fsequence_position
                            (math_operator prio)
                            (Earley_core.Earley.fsequence with_indices
                               (Earley_core.Earley.fsequence (math_aux prio)
                                  (Earley_core.Earley.empty_pos
                                     (fun __loc__start__buf ->
                                        fun __loc__start__pos ->
                                          fun __loc__end__buf ->
                                            fun __loc__end__pos ->
                                              let _loc =
                                                locate __loc__start__buf
                                                  __loc__start__pos
                                                  __loc__end__buf
                                                  __loc__end__pos in
                                              fun m ->
                                                fun ind ->
                                                  fun str1 ->
                                                    fun pos1 ->
                                                      fun str2 ->
                                                        fun pos2 ->
                                                          fun op ->
                                                            let _loc_op =
                                                              locate str1
                                                                pos1 str2
                                                                pos2 in
                                                            fun indices ->
                                                              let ind =
                                                                merge_indices
                                                                  indices ind in
                                                              match op.operator_kind
                                                              with
                                                              | Limits ->
                                                                  maths_op_limits
                                                                    _loc
                                                                    (
                                                                    print_math_deco_sym
                                                                    _loc_op
                                                                    (MultiSym
                                                                    (op.operator_values))
                                                                    ind)
                                                                    (
                                                                    m no_ind)
                                                              | NoLimits ->
                                                                  maths_op_nolimits
                                                                    _loc
                                                                    (
                                                                    print_math_deco_sym
                                                                    _loc_op
                                                                    (MultiSym
                                                                    (op.operator_values))
                                                                    ind)
                                                                    (
                                                                    m no_ind))))))
                         (List.append
                            (if prio <> AtomM
                             then
                               List.cons
                                 (Earley_core.Earley.fsequence
                                    (math_aux prio)
                                    (Earley_core.Earley.fsequence_position
                                       (Earley_core.Earley.alternatives
                                          (List.append
                                             (if prio = IApply
                                              then
                                                List.cons
                                                  (Earley_core.Earley.fsequence_ignore
                                                     (Earley_core.Earley.no_blank_test
                                                        ())
                                                     (Earley_core.Earley.empty
                                                        (invisible_apply,
                                                          no_ind))) []
                                              else [])
                                             (List.cons
                                                (Earley_core.Earley.fsequence
                                                   (math_infix_symbol prio)
                                                   (Earley_core.Earley.fsequence
                                                      with_indices
                                                      (Earley_core.Earley.empty
                                                         (fun i ->
                                                            fun s -> (s, i)))))
                                                (List.append
                                                   (if prio = IProd
                                                    then
                                                      List.cons
                                                        (Earley_core.Earley.fsequence_ignore
                                                           (Earley_core.Earley.with_blank_test
                                                              ())
                                                           (Earley_core.Earley.empty
                                                              (invisible_product,
                                                                no_ind))) []
                                                    else []) []))))
                                       (Earley_core.Earley.fsequence
                                          (math_aux (next_prio prio))
                                          (Earley_core.Earley.empty_pos
                                             (fun __loc__start__buf ->
                                                fun __loc__start__pos ->
                                                  fun __loc__end__buf ->
                                                    fun __loc__end__pos ->
                                                      let _loc =
                                                        locate
                                                          __loc__start__buf
                                                          __loc__start__pos
                                                          __loc__end__buf
                                                          __loc__end__pos in
                                                      fun r ->
                                                        fun str1 ->
                                                          fun pos1 ->
                                                            fun str2 ->
                                                              fun pos2 ->
                                                                fun st ->
                                                                  let _loc_st
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                  fun l ->
                                                                    let 
                                                                    (s, ind)
                                                                    = st in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    let sp =
                                                                    s.infix_space in
                                                                    let nsl =
                                                                    s.infix_no_left_space in
                                                                    let nsr =
                                                                    s.infix_no_right_space in
                                                                    let indices
                                                                    =
                                                                    merge_indices
                                                                    indices
                                                                    ind in
                                                                    let l =
                                                                    l no_ind
                                                                    and r =
                                                                    r
                                                                    (if
                                                                    s.infix_value
                                                                    =
                                                                    Invisible
                                                                    then
                                                                    indices
                                                                    else
                                                                    no_ind) in
                                                                    if
                                                                    s.infix_value
                                                                    =
                                                                    (SimpleSym
                                                                    "over")
                                                                    then
                                                                    (if
                                                                    indices
                                                                    <> no_ind
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    maths_fraction
                                                                    _loc l r)
                                                                    else
                                                                    (let inter
                                                                    =
                                                                    if
                                                                    s.infix_value
                                                                    =
                                                                    Invisible
                                                                    then
                                                                    maths_invisible
                                                                    _loc
                                                                    else
                                                                    (let v =
                                                                    print_math_deco_sym
                                                                    _loc_st
                                                                    s.infix_value
                                                                    indices in
                                                                    maths_normal
                                                                    _loc nsl
                                                                    v nsr) in
                                                                    maths_binary_record
                                                                    _loc sp
                                                                    inter l r))))))
                                 []
                             else [])
                            (List.append
                               (if prio = AtomM
                                then
                                  List.cons
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.char '{' '{')
                                       (Earley_core.Earley.fsequence
                                          (math_aux Punc)
                                          (Earley_core.Earley.fsequence_ignore
                                             (Earley_core.Earley.char '}' '}')
                                             (Earley_core.Earley.empty
                                                (fun m -> m))))) []
                                else [])
                               (List.append
                                  (if prio = AtomM
                                   then
                                     List.cons
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.char '{' '{')
                                          (Earley_core.Earley.fsequence_position
                                             any_symbol
                                             (Earley_core.Earley.fsequence
                                                with_indices
                                                (Earley_core.Earley.fsequence_ignore
                                                   (Earley_core.Earley.char
                                                      '}' '}')
                                                   (Earley_core.Earley.empty_pos
                                                      (fun __loc__start__buf
                                                         ->
                                                         fun
                                                           __loc__start__pos
                                                           ->
                                                           fun
                                                             __loc__end__buf
                                                             ->
                                                             fun
                                                               __loc__end__pos
                                                               ->
                                                               let _loc =
                                                                 locate
                                                                   __loc__start__buf
                                                                   __loc__start__pos
                                                                   __loc__end__buf
                                                                   __loc__end__pos in
                                                               fun ind ->
                                                                 fun str1 ->
                                                                   fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun s ->
                                                                    let _loc_s
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    if
                                                                    s =
                                                                    Invisible
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    (
                                                                    let f
                                                                    indices =
                                                                    let indices
                                                                    =
                                                                    merge_indices
                                                                    indices
                                                                    ind in
                                                                    let md =
                                                                    print_math_deco_sym
                                                                    _loc_s s
                                                                    indices in
                                                                    maths_ordinary_cell
                                                                    _loc md in
                                                                    f)))))))
                                       []
                                   else [])
                                  (List.append
                                     (if prio = AtomM
                                      then
                                        List.cons
                                          (Earley_core.Earley.fsequence_position
                                             all_left_delimiter
                                             (Earley_core.Earley.fsequence
                                                (math_aux Punc)
                                                (Earley_core.Earley.fsequence_position
                                                   all_right_delimiter
                                                   (Earley_core.Earley.empty_pos
                                                      (fun __loc__start__buf
                                                         ->
                                                         fun
                                                           __loc__start__pos
                                                           ->
                                                           fun
                                                             __loc__end__buf
                                                             ->
                                                             fun
                                                               __loc__end__pos
                                                               ->
                                                               let _loc =
                                                                 locate
                                                                   __loc__start__buf
                                                                   __loc__start__pos
                                                                   __loc__end__buf
                                                                   __loc__end__pos in
                                                               fun str1 ->
                                                                 fun pos1 ->
                                                                   fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun r ->
                                                                    let _loc_r
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun m ->
                                                                    fun str1
                                                                    ->
                                                                    fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun l ->
                                                                    let _loc_l
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    let l =
                                                                    print_math_symbol
                                                                    _loc_l
                                                                    (MultiSym
                                                                    (l.delimiter_values)) in
                                                                    let r =
                                                                    print_math_symbol
                                                                    _loc_r
                                                                    (MultiSym
                                                                    (r.delimiter_values)) in
                                                                    print_math_deco
                                                                    _loc
                                                                    (maths_decoration
                                                                    _loc l r
                                                                    (m no_ind))
                                                                    indices)))))
                                          []
                                      else [])
                                     (List.append
                                        (if prio = AtomM
                                         then
                                           List.cons
                                             (Earley_core.Earley.fsequence_position
                                                (Earley_str.regexp
                                                   ~name:"[a-zA-Z][a-zA-Z0-9]*"
                                                   "[a-zA-Z][a-zA-Z0-9]*"
                                                   (fun group -> group 0))
                                                (Earley_core.Earley.empty_pos
                                                   (fun __loc__start__buf ->
                                                      fun __loc__start__pos
                                                        ->
                                                        fun __loc__end__buf
                                                          ->
                                                          fun __loc__end__pos
                                                            ->
                                                            let _loc =
                                                              locate
                                                                __loc__start__buf
                                                                __loc__start__pos
                                                                __loc__end__buf
                                                                __loc__end__pos in
                                                            fun str1 ->
                                                              fun pos1 ->
                                                                fun str2 ->
                                                                  fun pos2 ->
                                                                    fun name
                                                                    ->
                                                                    let _loc_name
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    if
                                                                    (String.length
                                                                    name) > 1
                                                                    then
                                                                    let elt =
                                                                    Pa_ast.exp_fun
                                                                    _loc_name
                                                                    "env"
                                                                    (Pa_ast.exp_apply2
                                                                    _loc_name
                                                                    (Pa_ast.exp_ident_longident
                                                                    _loc_name
                                                                    { Location.loc = _loc_name; txt = Longident.Ldot ({ Location.loc = _loc_name; txt = Longident.Lident "Maths" }, { Location.loc = _loc_name; txt = "glyphs" }) })
                                                                    (Pa_ast.exp_string
                                                                    _loc_name
                                                                    name)
                                                                    (Pa_ast.exp_apply2
                                                                    _loc_name
                                                                    (Pa_ast.exp_ident_longident
                                                                    _loc_name
                                                                    { Location.loc = _loc_name; txt = Longident.Ldot ({ Location.loc = _loc_name; txt = Longident.Lident "Maths" }, { Location.loc = _loc_name; txt = "change_fonts" }) })
                                                                    (Pa_ast.exp_ident
                                                                    _loc_name
                                                                    "env")
                                                                    (Ast_helper.Exp.field
                                                                    ~loc:_loc_name
                                                                    (Pa_ast.exp_ident
                                                                    _loc_name
                                                                    "env")
                                                                    {
                                                                    loc =
                                                                    _loc_name;
                                                                    txt =
                                                                    (Lident
                                                                    "font")
                                                                    }))) in
                                                                    maths_ordinary_cell
                                                                    _loc
                                                                    (print_math_deco_sym
                                                                    _loc_name
                                                                    (CamlSym
                                                                    elt)
                                                                    indices)
                                                                    else
                                                                    maths_ordinary_cell
                                                                    _loc
                                                                    (print_math_deco_sym
                                                                    _loc_name
                                                                    (SimpleSym
                                                                    name)
                                                                    indices))))
                                             []
                                         else [])
                                        (List.append
                                           (if prio = AtomM
                                            then
                                              List.cons
                                                (Earley_core.Earley.fsequence_position
                                                   math_atom_symbol
                                                   (Earley_core.Earley.empty_pos
                                                      (fun __loc__start__buf
                                                         ->
                                                         fun
                                                           __loc__start__pos
                                                           ->
                                                           fun
                                                             __loc__end__buf
                                                             ->
                                                             fun
                                                               __loc__end__pos
                                                               ->
                                                               let _loc =
                                                                 locate
                                                                   __loc__start__buf
                                                                   __loc__start__pos
                                                                   __loc__end__buf
                                                                   __loc__end__pos in
                                                               fun str1 ->
                                                                 fun pos1 ->
                                                                   fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun sym
                                                                    ->
                                                                    let _loc_sym
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    maths_ordinary_cell
                                                                    _loc
                                                                    (print_math_deco_sym
                                                                    _loc_sym
                                                                    sym.symbol_value
                                                                    indices))))
                                                []
                                            else [])
                                           (List.append
                                              (if prio = AtomM
                                               then
                                                 List.cons
                                                   (Earley_core.Earley.fsequence_position
                                                      (Earley_str.regexp
                                                         ~name:"[0-9]+\\\\([.][0-9]+\\\\)?"
                                                         "[0-9]+\\([.][0-9]+\\)?"
                                                         (fun group ->
                                                            group 0))
                                                      (Earley_core.Earley.empty_pos
                                                         (fun
                                                            __loc__start__buf
                                                            ->
                                                            fun
                                                              __loc__start__pos
                                                              ->
                                                              fun
                                                                __loc__end__buf
                                                                ->
                                                                fun
                                                                  __loc__end__pos
                                                                  ->
                                                                  let _loc =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                  fun str1 ->
                                                                    fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun num
                                                                    ->
                                                                    let _loc_num
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    maths_ordinary_cell
                                                                    _loc
                                                                    (print_math_deco_sym
                                                                    _loc_num
                                                                    (SimpleSym
                                                                    num)
                                                                    indices))))
                                                   []
                                               else [])
                                              (List.append
                                                 (if prio = AtomM
                                                  then
                                                    List.cons
                                                      (Earley_core.Earley.iter
                                                         (Earley_core.Earley.fsequence_ignore
                                                            (Earley_core.Earley.char
                                                               '\\' '\\')
                                                            (Earley_core.Earley.fsequence_position
                                                               mathlid
                                                               (Earley_core.Earley.empty_pos
                                                                  (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun str1
                                                                    ->
                                                                    fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun id ->
                                                                    let _loc_id
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    let config
                                                                    =
                                                                    try
                                                                    List.assoc
                                                                    id
                                                                    state.math_macros
                                                                    with
                                                                    | 
                                                                    Not_found
                                                                    -> [] in
                                                                    Earley_core.Earley.fsequence
                                                                    (macro_arguments
                                                                    Math
                                                                    config)
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun args
                                                                    ->
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    let m =
                                                                    real_name
                                                                    _loc id
                                                                    config in
                                                                    let apply
                                                                    acc arg =
                                                                    Pa_ast.exp_apply1
                                                                    _loc_id
                                                                    acc arg in
                                                                    let e =
                                                                    List.fold_left
                                                                    apply m
                                                                    args in
                                                                    print_math_deco
                                                                    _loc_id e
                                                                    indices)))))))
                                                      []
                                                  else [])
                                                 (List.append
                                                    (if prio = Accent
                                                     then
                                                       List.cons
                                                         (Earley_core.Earley.fsequence
                                                            (math_aux Accent)
                                                            (Earley_core.Earley.fsequence
                                                               math_combining_symbol
                                                               (Earley_core.Earley.empty_pos
                                                                  (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun sym
                                                                    ->
                                                                    fun m ->
                                                                    print_math_deco
                                                                    _loc
                                                                    (Pa_ast.exp_apply1
                                                                    _loc
                                                                    (Pa_ast.exp_ident
                                                                    _loc sym)
                                                                    (m no_ind))))))
                                                         []
                                                     else [])
                                                    (List.append
                                                       (if prio = Accent
                                                        then
                                                          List.cons
                                                            (Earley_core.Earley.fsequence
                                                               (math_aux
                                                                  Accent)
                                                               (Earley_core.Earley.fsequence_position
                                                                  math_accent_symbol
                                                                  (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun str1
                                                                    ->
                                                                    fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun s ->
                                                                    let _loc_s
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun m ->
                                                                    let s =
                                                                    maths_ordinary_cell
                                                                    _loc
                                                                    (print_math_deco_sym
                                                                    _loc_s
                                                                    s.symbol_value
                                                                    no_ind) in
                                                                    let rd
                                                                    indices =
                                                                    if
                                                                    indices.up_right
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    {
                                                                    indices with
                                                                    up_right
                                                                    =
                                                                    (Some s);
                                                                    up_right_same_script
                                                                    = true
                                                                    } in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    m
                                                                    (rd
                                                                    indices)))))
                                                            []
                                                        else [])
                                                       (List.append
                                                          (if prio = Ind
                                                           then
                                                             List.cons
                                                               (Earley_core.Earley.fsequence
                                                                  (math_aux
                                                                    Ind)
                                                                  (Earley_core.Earley.fsequence_position
                                                                    Subsup.subscript
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun str1
                                                                    ->
                                                                    fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun s ->
                                                                    let _loc_s
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun m ->
                                                                    let s =
                                                                    maths_ordinary_cell
                                                                    _loc
                                                                    (print_math_deco_sym
                                                                    _loc_s
                                                                    (SimpleSym
                                                                    s) no_ind) in
                                                                    let rd
                                                                    indices =
                                                                    if
                                                                    indices.down_right
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    {
                                                                    indices with
                                                                    down_right
                                                                    =
                                                                    (Some s)
                                                                    } in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    m
                                                                    (rd
                                                                    indices)))))
                                                               []
                                                           else [])
                                                          (List.append
                                                             (if prio = Ind
                                                              then
                                                                List.cons
                                                                  (Earley_core.Earley.fsequence
                                                                    (math_aux
                                                                    Ind)
                                                                    (Earley_core.Earley.fsequence_position
                                                                    Subsup.superscript
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun str1
                                                                    ->
                                                                    fun pos1
                                                                    ->
                                                                    fun str2
                                                                    ->
                                                                    fun pos2
                                                                    ->
                                                                    fun s ->
                                                                    let _loc_s
                                                                    =
                                                                    locate
                                                                    str1 pos1
                                                                    str2 pos2 in
                                                                    fun m ->
                                                                    let s =
                                                                    maths_ordinary_cell
                                                                    _loc
                                                                    (print_math_deco_sym
                                                                    _loc_s
                                                                    (SimpleSym
                                                                    s) no_ind) in
                                                                    let rd
                                                                    indices =
                                                                    if
                                                                    indices.up_right
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    {
                                                                    indices with
                                                                    up_right
                                                                    =
                                                                    (Some s)
                                                                    } in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    m
                                                                    (rd
                                                                    indices)))))
                                                                  []
                                                              else [])
                                                             (List.append
                                                                (if
                                                                   prio = Ind
                                                                 then
                                                                   List.cons
                                                                    (Earley_core.Earley.fsequence
                                                                    (math_aux
                                                                    Ind)
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.no_blank_test
                                                                    ())
                                                                    (Earley_core.Earley.fsequence
                                                                    right_indices
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.no_blank_test
                                                                    ())
                                                                    (Earley_core.Earley.fsequence
                                                                    (math_aux
                                                                    Accent)
                                                                    (Earley_core.Earley.empty
                                                                    (fun r ->
                                                                    fun h ->
                                                                    fun m ->
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    match h
                                                                    with
                                                                    | 
                                                                    Down ->
                                                                    (if
                                                                    indices.down_right
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    m
                                                                    {
                                                                    indices with
                                                                    down_right
                                                                    =
                                                                    (Some
                                                                    (r no_ind))
                                                                    })
                                                                    | 
                                                                    Up ->
                                                                    (if
                                                                    indices.up_right
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    m
                                                                    {
                                                                    indices with
                                                                    up_right
                                                                    =
                                                                    (Some
                                                                    (r no_ind))
                                                                    }))))))))
                                                                    []
                                                                 else [])
                                                                (List.append
                                                                   (if
                                                                    prio =
                                                                    Ind
                                                                    then
                                                                    List.cons
                                                                    (Earley_core.Earley.fsequence
                                                                    (math_aux
                                                                    Ind)
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.no_blank_test
                                                                    ())
                                                                    (Earley_core.Earley.fsequence
                                                                    right_indices
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.no_blank_test
                                                                    ())
                                                                    (Earley_core.Earley.fsequence
                                                                    any_symbol
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun s ->
                                                                    fun h ->
                                                                    fun m ->
                                                                    let s =
                                                                    print_ordinary_math_symbol
                                                                    _loc s in
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    match h
                                                                    with
                                                                    | 
                                                                    Down ->
                                                                    (if
                                                                    indices.down_right
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    m
                                                                    {
                                                                    indices with
                                                                    down_right
                                                                    =
                                                                    (Some s)
                                                                    })
                                                                    | 
                                                                    Up ->
                                                                    (if
                                                                    indices.up_right
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    m
                                                                    {
                                                                    indices with
                                                                    up_right
                                                                    =
                                                                    (Some s)
                                                                    }))))))))
                                                                    []
                                                                    else [])
                                                                   (List.append
                                                                    (if
                                                                    prio =
                                                                    LInd
                                                                    then
                                                                    List.cons
                                                                    (Earley_core.Earley.fsequence
                                                                    (math_aux
                                                                    Accent)
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.no_blank_test
                                                                    ())
                                                                    (Earley_core.Earley.fsequence
                                                                    left_indices
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.no_blank_test
                                                                    ())
                                                                    (Earley_core.Earley.fsequence
                                                                    (math_aux
                                                                    LInd)
                                                                    (Earley_core.Earley.empty
                                                                    (fun r ->
                                                                    fun h ->
                                                                    fun m ->
                                                                    fun
                                                                    indices
                                                                    ->
                                                                    match h
                                                                    with
                                                                    | 
                                                                    Down ->
                                                                    (if
                                                                    indices.down_left
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    r
                                                                    {
                                                                    indices with
                                                                    down_left
                                                                    =
                                                                    (Some
                                                                    (m no_ind))
                                                                    })
                                                                    | 
                                                                    Up ->
                                                                    (if
                                                                    indices.up_left
                                                                    <> None
                                                                    then
                                                                    give_up
                                                                    ();
                                                                    r
                                                                    {
                                                                    indices with
                                                                    up_left =
                                                                    (Some
                                                                    (m no_ind))
                                                                    }))))))))
                                                                    []
                                                                    else [])
                                                                    []))))))))))))))))))))
    let _ =
      math_prefix__set__grammar
        (fun prio ->
           Earley_core.Earley.alternatives
             (List.cons
                (Earley_core.Earley.fsequence_position
                   (math_prefix_symbol prio)
                   (Earley_core.Earley.fsequence with_indices
                      (Earley_core.Earley.fsequence (math_prefix prio)
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun m ->
                                       fun ind ->
                                         fun str1 ->
                                           fun pos1 ->
                                             fun str2 ->
                                               fun pos2 ->
                                                 fun sym ->
                                                   let _loc_sym =
                                                     locate str1 pos1 str2
                                                       pos2 in
                                                   fun indices ->
                                                     let indices =
                                                       merge_indices indices
                                                         ind in
                                                     let psp =
                                                       sym.prefix_space in
                                                     let pnsp =
                                                       sym.prefix_no_space in
                                                     let md =
                                                       print_math_deco_sym
                                                         _loc_sym
                                                         sym.prefix_value
                                                         indices in
                                                     Pa_ast.exp_list _loc
                                                       [maths_bin _loc psp
                                                          (maths_normal _loc
                                                             true md pnsp)
                                                          (Pa_ast.exp_list
                                                             _loc [])
                                                          (m no_ind)])))))
                (List.cons (math_postfix prio) [])))
    let _ =
      math_postfix__set__grammar
        (fun prio ->
           Earley_core.Earley.alternatives
             (List.cons
                (Earley_core.Earley.fsequence (math_postfix prio)
                   (Earley_core.Earley.fsequence_position
                      (math_postfix_symbol prio)
                      (Earley_core.Earley.empty_pos
                         (fun __loc__start__buf ->
                            fun __loc__start__pos ->
                              fun __loc__end__buf ->
                                fun __loc__end__pos ->
                                  let _loc =
                                    locate __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  fun str1 ->
                                    fun pos1 ->
                                      fun str2 ->
                                        fun pos2 ->
                                          fun sym ->
                                            let _loc_sym =
                                              locate str1 pos1 str2 pos2 in
                                            fun m ->
                                              fun indices ->
                                                let psp = sym.postfix_space in
                                                let nsp =
                                                  sym.postfix_no_space in
                                                let md =
                                                  print_math_deco_sym
                                                    _loc_sym
                                                    sym.postfix_value indices in
                                                let m = m no_ind in
                                                Pa_ast.exp_list _loc
                                                  [maths_bin _loc psp
                                                     (maths_normal _loc nsp
                                                        md true) m
                                                     (Pa_ast.exp_list _loc [])]))))
                (List.cons (math_aux prio) [])))
    let _ =
      Earley_core.Earley.set_grammar with_indices
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence with_indices
                 (Earley_core.Earley.fsequence_position Subsup.subscript
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun str1 ->
                                  fun pos1 ->
                                    fun str2 ->
                                      fun pos2 ->
                                        fun s ->
                                          let _loc_s =
                                            locate str1 pos1 str2 pos2 in
                                          fun i ->
                                            let s =
                                              maths_ordinary_cell _loc
                                                (print_math_deco_sym _loc_s
                                                   (SimpleSym s) no_ind) in
                                            if i.down_right <> None
                                            then give_up ();
                                            { i with down_right = (Some s) }))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.empty ())
                    (Earley_core.Earley.empty no_ind))
                 (List.cons
                    (Earley_core.Earley.fsequence with_indices
                       (Earley_core.Earley.fsequence right_indices
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.no_blank_test ())
                             (Earley_core.Earley.fsequence (math_aux Accent)
                                (Earley_core.Earley.empty
                                   (fun r ->
                                      fun h ->
                                        fun i ->
                                          match h with
                                          | Down ->
                                              (if i.down_right <> None
                                               then give_up ();
                                               {
                                                 i with
                                                 down_right =
                                                   (Some (r no_ind))
                                               })
                                          | Up ->
                                              (if i.up_right <> None
                                               then give_up ();
                                               {
                                                 i with
                                                 up_right = (Some (r no_ind))
                                               })))))))
                    (List.cons
                       (Earley_core.Earley.fsequence with_indices
                          (Earley_core.Earley.fsequence_position
                             Subsup.superscript
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun str1 ->
                                           fun pos1 ->
                                             fun str2 ->
                                               fun pos2 ->
                                                 fun s ->
                                                   let _loc_s =
                                                     locate str1 pos1 str2
                                                       pos2 in
                                                   fun i ->
                                                     let s =
                                                       maths_ordinary_cell
                                                         _loc
                                                         (print_math_deco_sym
                                                            _loc_s
                                                            (SimpleSym s)
                                                            no_ind) in
                                                     if i.up_right <> None
                                                     then give_up ();
                                                     {
                                                       i with
                                                       up_right = (Some s)
                                                     })))) [])))))
    let _ =
      Earley_core.Earley.set_grammar math_punc_list
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence math_punc_list
                 (Earley_core.Earley.fsequence_position
                    math_punctuation_symbol
                    (Earley_core.Earley.fsequence (math_aux Ind)
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun m ->
                                     fun str1 ->
                                       fun pos1 ->
                                         fun str2 ->
                                           fun pos2 ->
                                             fun s ->
                                               let _loc_s =
                                                 locate str1 pos1 str2 pos2 in
                                               fun l ->
                                                 let nsl =
                                                   s.infix_no_left_space in
                                                 let nsr =
                                                   s.infix_no_right_space in
                                                 let r = m no_ind in
                                                 let inter =
                                                   maths_normal _loc nsl
                                                     (print_math_deco_sym
                                                        _loc_s s.infix_value
                                                        no_ind) nsr in
                                                 Pa_ast.exp_list _loc
                                                   [maths_bin _loc 3 inter l
                                                      r])))))
              (List.cons
                 (Earley_core.Earley.fsequence (math_aux Ind)
                    (Earley_core.Earley.empty (fun m -> m no_ind))) [])))
    let _ =
      Earley_core.Earley.set_grammar long_math_declaration
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence long_math_declaration
                 (Earley_core.Earley.fsequence_position math_relation_symbol
                    (Earley_core.Earley.fsequence with_indices
                       (Earley_core.Earley.fsequence math_punc_list
                          (Earley_core.Earley.empty_pos
                             (fun __loc__start__buf ->
                                fun __loc__start__pos ->
                                  fun __loc__end__buf ->
                                    fun __loc__end__pos ->
                                      let _loc =
                                        locate __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      fun r ->
                                        fun ind ->
                                          fun str1 ->
                                            fun pos1 ->
                                              fun str2 ->
                                                fun pos2 ->
                                                  fun s ->
                                                    let _loc_s =
                                                      locate str1 pos1 str2
                                                        pos2 in
                                                    fun l ->
                                                      let nsl =
                                                        s.infix_no_left_space in
                                                      let nsr =
                                                        s.infix_no_right_space in
                                                      let inter =
                                                        maths_normal _loc nsl
                                                          (print_math_deco_sym
                                                             _loc_s
                                                             s.infix_value
                                                             ind) nsr in
                                                      Pa_ast.exp_list _loc
                                                        [maths_bin _loc 2
                                                           inter l r]))))))
              (List.cons math_punc_list [])))
    let _ =
      Earley_core.Earley.set_grammar math_declaration
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence no_brace
                 (Earley_core.Earley.fsequence (math_aux Ind)
                    (Earley_core.Earley.fsequence_position
                       math_relation_symbol
                       (Earley_core.Earley.fsequence with_indices
                          (Earley_core.Earley.fsequence (math_aux Ind)
                             (Earley_core.Earley.empty_pos
                                (fun __loc__start__buf ->
                                   fun __loc__start__pos ->
                                     fun __loc__end__buf ->
                                       fun __loc__end__pos ->
                                         let _loc =
                                           locate __loc__start__buf
                                             __loc__start__pos
                                             __loc__end__buf __loc__end__pos in
                                         fun r ->
                                           fun ind ->
                                             fun str1 ->
                                               fun pos1 ->
                                                 fun str2 ->
                                                   fun pos2 ->
                                                     fun s ->
                                                       let _loc_s =
                                                         locate str1 pos1
                                                           str2 pos2 in
                                                       fun m ->
                                                         fun _default_0 ->
                                                           let nsl =
                                                             s.infix_no_left_space in
                                                           let nsr =
                                                             s.infix_no_right_space in
                                                           let inter =
                                                             maths_normal
                                                               _loc nsl
                                                               (print_math_deco_sym
                                                                  _loc_s
                                                                  s.infix_value
                                                                  ind) nsr in
                                                           Pa_ast.exp_list
                                                             _loc
                                                             [maths_bin _loc
                                                                2 inter
                                                                (m no_ind)
                                                                (r no_ind)])))))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.char '{' '{')
                    (Earley_core.Earley.fsequence long_math_declaration
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.char '}' '}')
                          (Earley_core.Earley.empty (fun m -> m)))))
                 (List.cons
                    (Earley_core.Earley.fsequence no_brace
                       (Earley_core.Earley.fsequence (math_aux Ind)
                          (Earley_core.Earley.empty
                             (fun m -> fun _default_0 -> m no_ind)))) []))))
    let _ =
      set_grammar math_toplevel
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_position any_symbol
                 (Earley_core.Earley.fsequence with_indices
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun i ->
                                  fun str1 ->
                                    fun pos1 ->
                                      fun str2 ->
                                        fun pos2 ->
                                          fun s ->
                                            let _loc_s =
                                              locate str1 pos1 str2 pos2 in
                                            if s = Invisible then give_up ();
                                            maths_ordinary_cell _loc
                                              (print_math_deco_sym _loc_s s i)))))
              (List.cons
                 (Earley_core.Earley.fsequence (math_aux Punc)
                    (Earley_core.Earley.empty (fun m -> m no_ind))) [])))
    let reserved_macro = ["begin"; "end"; "item"; "verb"]
    let macro_name =
      change_layout
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.string "\\" "\\")
           (Earley_core.Earley.fsequence_ignore
              (Earley_core.Earley.no_blank_test ())
              (Earley_core.Earley.fsequence lid
                 (Earley_core.Earley.empty
                    (fun m -> if List.mem m reserved_macro then give_up (); m)))))
        no_blank
    let macro = Earley_core.Earley.declare_grammar "macro"
    let _ =
      Earley_core.Earley.set_grammar macro
        (Earley_core.Earley.alternatives
           (List.cons verbatim_macro
              (List.cons
                 (Earley_core.Earley.iter
                    (Earley_core.Earley.fsequence macro_name
                       (Earley_core.Earley.empty_pos
                          (fun __loc__start__buf ->
                             fun __loc__start__pos ->
                               fun __loc__end__buf ->
                                 fun __loc__end__pos ->
                                   let _loc =
                                     locate __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   fun id ->
                                     let config =
                                       try List.assoc id state.word_macros
                                       with | Not_found -> [] in
                                     Earley_core.Earley.fsequence
                                       (macro_arguments Text config)
                                       (Earley_core.Earley.empty_pos
                                          (fun __loc__start__buf ->
                                             fun __loc__start__pos ->
                                               fun __loc__end__buf ->
                                                 fun __loc__end__pos ->
                                                   let _loc =
                                                     locate __loc__start__buf
                                                       __loc__start__pos
                                                       __loc__end__buf
                                                       __loc__end__pos in
                                                   fun args ->
                                                     let fn acc r =
                                                       Pa_ast.exp_apply1 _loc
                                                         acc r in
                                                     List.fold_left fn
                                                       (Pa_ast.exp_ident _loc
                                                          id) args)))))) [])))
    let (text_paragraph_elt, text_paragraph_elt__set__grammar) =
      Earley_core.Earley.grammar_family "text_paragraph_elt"
    let _ =
      text_paragraph_elt__set__grammar
        (fun (tags : TagSet.t) ->
           Earley_core.Earley.alternatives
             (List.cons
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char '{' '{')
                   (Earley_core.Earley.fsequence
                      (paragraph_basic_text TagSet.empty)
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char '}' '}')
                         (Earley_core.Earley.empty (fun p -> p)))))
                (List.cons macro
                   (List.append
                      (if allowed Italic tags
                       then
                         List.cons
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.string "//" "//")
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.no_blank_test ())
                                 (Earley_core.Earley.fsequence
                                    (paragraph_basic_text
                                       (addTag Italic tags))
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.no_blank_test ())
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.string "//"
                                             "//")
                                          (Earley_core.Earley.empty_pos
                                             (fun __loc__start__buf ->
                                                fun __loc__start__pos ->
                                                  fun __loc__end__buf ->
                                                    fun __loc__end__pos ->
                                                      let _loc =
                                                        locate
                                                          __loc__start__buf
                                                          __loc__start__pos
                                                          __loc__end__buf
                                                          __loc__end__pos in
                                                      fun p ->
                                                        Pa_ast.exp_apply1
                                                          _loc
                                                          (Pa_ast.exp_ident
                                                             _loc
                                                             "toggleItalic")
                                                          p))))))) []
                       else [])
                      (List.append
                         (if allowed Bold tags
                          then
                            List.cons
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.string "**" "**")
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.no_blank_test ())
                                    (Earley_core.Earley.fsequence
                                       (paragraph_basic_text
                                          (addTag Bold tags))
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.no_blank_test
                                             ())
                                          (Earley_core.Earley.fsequence_ignore
                                             (Earley_core.Earley.string "**"
                                                "**")
                                             (Earley_core.Earley.empty_pos
                                                (fun __loc__start__buf ->
                                                   fun __loc__start__pos ->
                                                     fun __loc__end__buf ->
                                                       fun __loc__end__pos ->
                                                         let _loc =
                                                           locate
                                                             __loc__start__buf
                                                             __loc__start__pos
                                                             __loc__end__buf
                                                             __loc__end__pos in
                                                         fun p ->
                                                           Pa_ast.exp_apply1
                                                             _loc
                                                             (Pa_ast.exp_ident
                                                                _loc "bold")
                                                             p))))))) []
                          else [])
                         (List.append
                            (if allowed SmallCap tags
                             then
                               List.cons
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.string "||" "||")
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.no_blank_test ())
                                       (Earley_core.Earley.fsequence
                                          (paragraph_basic_text
                                             (addTag SmallCap tags))
                                          (Earley_core.Earley.fsequence_ignore
                                             (Earley_core.Earley.no_blank_test
                                                ())
                                             (Earley_core.Earley.fsequence_ignore
                                                (Earley_core.Earley.string
                                                   "||" "||")
                                                (Earley_core.Earley.empty_pos
                                                   (fun __loc__start__buf ->
                                                      fun __loc__start__pos
                                                        ->
                                                        fun __loc__end__buf
                                                          ->
                                                          fun __loc__end__pos
                                                            ->
                                                            let _loc =
                                                              locate
                                                                __loc__start__buf
                                                                __loc__start__pos
                                                                __loc__end__buf
                                                                __loc__end__pos in
                                                            fun p ->
                                                              Pa_ast.exp_apply1
                                                                _loc
                                                                (Pa_ast.exp_ident
                                                                   _loc "sc")
                                                                p))))))) []
                             else [])
                            (List.append
                               (if allowed Quote tags
                                then
                                  List.cons
                                    (Earley_core.Earley.fsequence_ignore
                                       (Earley_core.Earley.char '"' '"')
                                       (Earley_core.Earley.fsequence
                                          (paragraph_basic_text
                                             (addTag Quote tags))
                                          (Earley_core.Earley.fsequence_ignore
                                             (Earley_core.Earley.char '"' '"')
                                             (Earley_core.Earley.empty_pos
                                                (fun __loc__start__buf ->
                                                   fun __loc__start__pos ->
                                                     fun __loc__end__buf ->
                                                       fun __loc__end__pos ->
                                                         let _loc =
                                                           locate
                                                             __loc__start__buf
                                                             __loc__start__pos
                                                             __loc__end__buf
                                                             __loc__end__pos in
                                                         fun p ->
                                                           let opening =
                                                             "\226\128\156" in
                                                           let closing =
                                                             "\226\128\157" in
                                                           let tail =
                                                             exp_infix _loc
                                                               "@" p
                                                               (Pa_ast.exp_list
                                                                  _loc
                                                                  [Pa_ast.exp_apply1
                                                                    _loc
                                                                    (Pa_ast.exp_ident
                                                                    _loc "tT")
                                                                    (Pa_ast.exp_string
                                                                    _loc
                                                                    closing)]) in
                                                           Pa_ast.exp_Cons
                                                             _loc
                                                             (Pa_ast.exp_apply1
                                                                _loc
                                                                (Pa_ast.exp_ident
                                                                   _loc "tT")
                                                                (Pa_ast.exp_string
                                                                   _loc
                                                                   opening))
                                                             tail))))) []
                                else [])
                               (List.append
                                  (if allowed Quote tags
                                   then
                                     List.cons
                                       (Earley_core.Earley.fsequence_ignore
                                          (Earley_core.Earley.string "``"
                                             "``")
                                          (Earley_core.Earley.fsequence
                                             (paragraph_basic_text
                                                (addTag Quote tags))
                                             (Earley_core.Earley.fsequence_ignore
                                                (Earley_core.Earley.string
                                                   "''" "''")
                                                (Earley_core.Earley.empty_pos
                                                   (fun __loc__start__buf ->
                                                      fun __loc__start__pos
                                                        ->
                                                        fun __loc__end__buf
                                                          ->
                                                          fun __loc__end__pos
                                                            ->
                                                            let _loc =
                                                              locate
                                                                __loc__start__buf
                                                                __loc__start__pos
                                                                __loc__end__buf
                                                                __loc__end__pos in
                                                            fun p ->
                                                              let opening =
                                                                "\226\128\156" in
                                                              let closing =
                                                                "\226\128\157" in
                                                              let tail =
                                                                exp_infix
                                                                  _loc "@" p
                                                                  (Pa_ast.exp_list
                                                                    _loc
                                                                    [
                                                                    Pa_ast.exp_apply1
                                                                    _loc
                                                                    (Pa_ast.exp_ident
                                                                    _loc "tT")
                                                                    (Pa_ast.exp_string
                                                                    _loc
                                                                    closing)]) in
                                                              Pa_ast.exp_Cons
                                                                _loc
                                                                (Pa_ast.exp_apply1
                                                                   _loc
                                                                   (Pa_ast.exp_ident
                                                                    _loc "tT")
                                                                   (Pa_ast.exp_string
                                                                    _loc
                                                                    opening))
                                                                tail))))) []
                                   else [])
                                  (List.cons verbatim_sharp
                                     (List.cons verbatim_bquote
                                        (List.cons
                                           (Earley_core.Earley.fsequence
                                              dollar
                                              (Earley_core.Earley.fsequence
                                                 math_toplevel
                                                 (Earley_core.Earley.fsequence
                                                    dollar
                                                    (Earley_core.Earley.empty_pos
                                                       (fun __loc__start__buf
                                                          ->
                                                          fun
                                                            __loc__start__pos
                                                            ->
                                                            fun
                                                              __loc__end__buf
                                                              ->
                                                              fun
                                                                __loc__end__pos
                                                                ->
                                                                let _loc =
                                                                  locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                fun
                                                                  _default_0
                                                                  ->
                                                                  fun m ->
                                                                    fun
                                                                    _default_1
                                                                    ->
                                                                    inline_math_kdraw_bb
                                                                    _loc m)))))
                                           (List.cons
                                              (Earley_core.Earley.fsequence_ignore
                                                 (Earley_core.Earley.string
                                                    "\\(" "\\(")
                                                 (Earley_core.Earley.fsequence
                                                    math_toplevel
                                                    (Earley_core.Earley.fsequence_ignore
                                                       (Earley_core.Earley.string
                                                          "\\)" "\\)")
                                                       (Earley_core.Earley.empty_pos
                                                          (fun
                                                             __loc__start__buf
                                                             ->
                                                             fun
                                                               __loc__start__pos
                                                               ->
                                                               fun
                                                                 __loc__end__buf
                                                                 ->
                                                                 fun
                                                                   __loc__end__pos
                                                                   ->
                                                                   let _loc =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                   fun m ->
                                                                    display_math_kdraw_bb
                                                                    _loc m)))))
                                              (List.cons
                                                 (Earley_core.Earley.fsequence
                                                    (Earley_core.Earley.greedy
                                                       (Earley_core.Earley.apply
                                                          (fun f -> f [])
                                                          (Earley_core.Earley.fixpoint1'
                                                             (fun l -> l)
                                                             word
                                                             (fun x ->
                                                                fun f ->
                                                                  fun l ->
                                                                    f
                                                                    (List.cons
                                                                    x l)))))
                                                    (Earley_core.Earley.empty_pos
                                                       (fun __loc__start__buf
                                                          ->
                                                          fun
                                                            __loc__start__pos
                                                            ->
                                                            fun
                                                              __loc__end__buf
                                                              ->
                                                              fun
                                                                __loc__end__pos
                                                                ->
                                                                let _loc =
                                                                  locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                fun ws ->
                                                                  Pa_ast.exp_list
                                                                    _loc
                                                                    [
                                                                    Pa_ast.exp_apply1
                                                                    _loc
                                                                    (Pa_ast.exp_ident
                                                                    _loc "tT")
                                                                    (Pa_ast.exp_string
                                                                    _loc
                                                                    (String.concat
                                                                    " " ws))])))
                                                 [])))))))))))))
    let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
      let (x, y) =
        let open Lexing in
          (((end_pos _loc_p1).pos_cnum), ((start_pos _loc_p2).pos_cnum)) in
      let _loc = _loc_p2 in
      let bl e =
        if (y - x) >= 1
        then
          Pa_ast.exp_Cons _loc
            (Pa_ast.exp_apply1 _loc (Pa_ast.exp_ident _loc "tT")
               (Pa_ast.exp_string _loc " ")) e
        else e in
      let _loc = Pa_ast.merge2 _loc_p1 _loc_p2 in
      exp_infix _loc "@" p1 (bl p2)
    let _ =
      set_paragraph_basic_text
        (fun tags ->
           Earley_core.Earley.fsequence
             (Earley_core.Earley.greedy
                (Earley_core.Earley.apply (fun f -> f [])
                   (Earley_core.Earley.fixpoint1' (fun l -> l)
                      (Earley_core.Earley.fsequence (text_paragraph_elt tags)
                         (Earley_core.Earley.empty_pos
                            (fun __loc__start__buf ->
                               fun __loc__start__pos ->
                                 fun __loc__end__buf ->
                                   fun __loc__end__pos ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     fun p -> (_loc, p))))
                      (fun x -> fun f -> fun l -> f (List.cons x l)))))
             (Earley_core.Earley.empty
                (fun l ->
                   match List.rev l with
                   | [] -> assert false
                   | m::l ->
                       let fn (_loc_m, m) (_loc_p, p) =
                         ((Pa_ast.merge2 _loc_p _loc_m),
                           (concat_paragraph p _loc_p m _loc_m)) in
                       snd (List.fold_left fn m l))))
    let oparagraph_basic_text = paragraph_basic_text
    let paragraph_basic_text =
      Earley_core.Earley.declare_grammar "paragraph_basic_text"
    let _ =
      Earley_core.Earley.set_grammar paragraph_basic_text
        (Earley_core.Earley.fsequence (oparagraph_basic_text TagSet.empty)
           (Earley_core.Earley.empty_pos
              (fun __loc__start__buf ->
                 fun __loc__start__pos ->
                   fun __loc__end__buf ->
                     fun __loc__end__pos ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       fun p ->
                         fun indented ->
                           paragraph_newpar_struct _loc p ~indented)))
    let paragraph = declare_grammar "paragraph"
    let paragraphs = declare_grammar "paragraphs"
    let nb_includes = ref 0
    let paragraph_elt = Earley_core.Earley.declare_grammar "paragraph_elt"
    let _ =
      Earley_core.Earley.set_grammar paragraph_elt
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence symbol_def
                 (Earley_core.Earley.empty (fun s -> fun _ -> s)))
              (List.cons
                 (Earley_core.Earley.fsequence verbatim_environment
                    (Earley_core.Earley.empty (fun verb -> fun _ -> verb)))
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "\\Caml" "\\Caml")
                       (Earley_core.Earley.fsequence wrapped_caml_structure
                          (Earley_core.Earley.empty (fun s -> fun _ -> s))))
                    (List.cons
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "\\Include" "\\Include")
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.char '{' '{')
                             (Earley_core.Earley.fsequence uid
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.char '}' '}')
                                   (Earley_core.Earley.empty_pos
                                      (fun __loc__start__buf ->
                                         fun __loc__start__pos ->
                                           fun __loc__end__buf ->
                                             fun __loc__end__pos ->
                                               let _loc =
                                                 locate __loc__start__buf
                                                   __loc__start__pos
                                                   __loc__end__buf
                                                   __loc__end__pos in
                                               fun id ->
                                                 fun _ ->
                                                   incr nb_includes;
                                                   (try
                                                      add_grammar id;
                                                      build_grammar ()
                                                    with | Not_found -> ());
                                                   (let temp_id =
                                                      Printf.sprintf "TEMP%d"
                                                        (!nb_includes) in
                                                    include_document_structure
                                                      _loc ~temp_id ~id)))))))
                       (List.cons
                          (Earley_core.Earley.fsequence_ignore
                             (Earley_core.Earley.string "\\" "\\")
                             (Earley_core.Earley.fsequence macrouid
                                (Earley_core.Earley.fsequence
                                   (Earley_core.Earley.greedy
                                      (Earley_core.Earley.apply
                                         (fun f -> f [])
                                         (Earley_core.Earley.fixpoint'
                                            (fun l -> l)
                                            simple_text_macro_argument
                                            (fun x ->
                                               fun f ->
                                                 fun l -> f (List.cons x l)))))
                                   (Earley_core.Earley.empty_pos
                                      (fun __loc__start__buf ->
                                         fun __loc__start__pos ->
                                           fun __loc__end__buf ->
                                             fun __loc__end__pos ->
                                               let _loc =
                                                 locate __loc__start__buf
                                                   __loc__start__pos
                                                   __loc__end__buf
                                                   __loc__end__pos in
                                               fun ts ->
                                                 fun id ->
                                                   fun _ ->
                                                     let m1 = freshUid () in
                                                     if ts <> []
                                                     then
                                                       let m2 = freshUid () in
                                                       macrouid_with_args_structure
                                                         _loc ~m1 ~m2 ~id ~ts
                                                     else
                                                       macrouid_plain_structure
                                                         _loc ~m1 ~id)))))
                          (List.cons
                             (Earley_core.Earley.iter
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_core.Earley.string "\\begin"
                                      "\\begin")
                                   (Earley_core.Earley.fsequence_ignore
                                      (Earley_core.Earley.char '{' '{')
                                      (Earley_core.Earley.fsequence lid
                                         (Earley_core.Earley.fsequence_ignore
                                            (Earley_core.Earley.char '}' '}')
                                            (Earley_core.Earley.empty_pos
                                               (fun __loc__start__buf ->
                                                  fun __loc__start__pos ->
                                                    fun __loc__end__buf ->
                                                      fun __loc__end__pos ->
                                                        let _loc =
                                                          locate
                                                            __loc__start__buf
                                                            __loc__start__pos
                                                            __loc__end__buf
                                                            __loc__end__pos in
                                                        fun idb ->
                                                          let config =
                                                            try
                                                              List.assoc idb
                                                                state.environment
                                                            with
                                                            | Not_found -> [] in
                                                          Earley_core.Earley.fsequence
                                                            (macro_arguments
                                                               Text config)
                                                            (Earley_core.Earley.fsequence
                                                               (change_layout
                                                                  paragraphs
                                                                  blank2)
                                                               (Earley_core.Earley.fsequence_ignore
                                                                  (Earley_core.Earley.string
                                                                    "\\end"
                                                                    "\\end")
                                                                  (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '{' '{')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.string
                                                                    idb idb)
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '}' '}')
                                                                    (Earley_core.Earley.empty_pos
                                                                    (fun
                                                                    __loc__start__buf
                                                                    ->
                                                                    fun
                                                                    __loc__start__pos
                                                                    ->
                                                                    fun
                                                                    __loc__end__buf
                                                                    ->
                                                                    fun
                                                                    __loc__end__pos
                                                                    ->
                                                                    let _loc
                                                                    =
                                                                    locate
                                                                    __loc__start__buf
                                                                    __loc__start__pos
                                                                    __loc__end__buf
                                                                    __loc__end__pos in
                                                                    fun ps ->
                                                                    fun args
                                                                    ->
                                                                    fun
                                                                    indent_first
                                                                    ->
                                                                    let m1 =
                                                                    freshUid
                                                                    () in
                                                                    let m2 =
                                                                    freshUid
                                                                    () in
                                                                    begin_env_paragraph_structure
                                                                    _loc ~m1
                                                                    ~m2 ~idb
                                                                    ~args ~ps
                                                                    ~indent_first))))))))))))))
                             (List.cons
                                (Earley_core.Earley.fsequence
                                   (Earley_core.Earley.alternatives
                                      (List.cons
                                         (Earley_core.Earley.fsequence_ignore
                                            (Earley_core.Earley.string "$$"
                                               "$$")
                                            (Earley_core.Earley.fsequence
                                               math_toplevel
                                               (Earley_core.Earley.fsequence_ignore
                                                  (Earley_core.Earley.string
                                                     "$$" "$$")
                                                  (Earley_core.Earley.empty
                                                     (fun _default_0 ->
                                                        _default_0)))))
                                         (List.cons
                                            (Earley_core.Earley.fsequence_ignore
                                               (Earley_core.Earley.string
                                                  "\\[" "\\[")
                                               (Earley_core.Earley.fsequence
                                                  math_toplevel
                                                  (Earley_core.Earley.fsequence_ignore
                                                     (Earley_core.Earley.string
                                                        "\\]" "\\]")
                                                     (Earley_core.Earley.empty
                                                        (fun _default_0 ->
                                                           _default_0))))) [])))
                                   (Earley_core.Earley.empty_pos
                                      (fun __loc__start__buf ->
                                         fun __loc__start__pos ->
                                           fun __loc__end__buf ->
                                             fun __loc__end__pos ->
                                               let _loc =
                                                 locate __loc__start__buf
                                                   __loc__start__pos
                                                   __loc__end__buf
                                                   __loc__end__pos in
                                               fun m ->
                                                 fun _ ->
                                                   display_math_paragraph_structure
                                                     _loc m)))
                                (List.cons paragraph_basic_text [])))))))))
    let _ =
      set_grammar paragraph
        (change_layout
           (Earley_core.Earley.fsequence paragraph_elt
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.apply (fun f -> f [])
                    (Earley_core.Earley.fixpoint' (fun l -> l) paragraph_elt
                       (fun x -> fun f -> fun l -> f (List.cons x l))))
                 (Earley_core.Earley.empty
                    (fun l ->
                       fun e ->
                         fun i ->
                           List.flatten ((e i) ::
                             (List.map (fun r -> r false) l))))))
           ~new_blank_after:false blank1)
    let _ =
      set_grammar paragraphs
        (Earley_core.Earley.fsequence paragraph
           (Earley_core.Earley.fsequence
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l) paragraph
                    (fun x -> fun f -> fun l -> f (List.cons x l))))
              (Earley_core.Earley.empty
                 (fun ps ->
                    fun p ->
                      let ps = List.flatten (List.map (fun r -> r true) ps) in
                      fun indent_first -> (p indent_first) @ ps))))
    let numbered op cl =
      match ((op.[0]), (cl.[0])) with
      | ('=', '=') -> (true, true)
      | ('-', '-') -> (false, true)
      | ('_', '_') -> (false, false)
      | _ -> give_up ()
    let sect lvl =
      Earley_core.Earley.alternatives
        (List.append
           (if lvl = 7
            then
              List.cons
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string "=========" "=========")
                   (Earley_core.Earley.empty ())) []
            else [])
           (List.append
              (if lvl = 0
               then
                 List.cons
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "==" "==")
                      (Earley_core.Earley.empty ())) []
               else [])
              (List.append
                 (if lvl = 1
                  then
                    List.cons
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "===" "===")
                         (Earley_core.Earley.empty ())) []
                  else [])
                 (List.append
                    (if lvl = 2
                     then
                       List.cons
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.string "====" "====")
                            (Earley_core.Earley.empty ())) []
                     else [])
                    (List.append
                       (if lvl = 3
                        then
                          List.cons
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string "=====" "=====")
                               (Earley_core.Earley.empty ())) []
                        else [])
                       (List.append
                          (if lvl = 4
                           then
                             List.cons
                               (Earley_core.Earley.fsequence_ignore
                                  (Earley_core.Earley.string "======"
                                     "======") (Earley_core.Earley.empty ()))
                               []
                           else [])
                          (List.append
                             (if lvl = 5
                              then
                                List.cons
                                  (Earley_core.Earley.fsequence_ignore
                                     (Earley_core.Earley.string "======="
                                        "=======")
                                     (Earley_core.Earley.empty ())) []
                              else [])
                             (List.append
                                (if lvl = 6
                                 then
                                   List.cons
                                     (Earley_core.Earley.fsequence_ignore
                                        (Earley_core.Earley.string "========"
                                           "========")
                                        (Earley_core.Earley.empty ())) []
                                 else []) []))))))))
    let usect lvl =
      Earley_core.Earley.alternatives
        (List.append
           (if lvl = 7
            then
              List.cons
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string "---------" "---------")
                   (Earley_core.Earley.empty ())) []
            else [])
           (List.append
              (if lvl = 0
               then
                 List.cons
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.string "--" "--")
                      (Earley_core.Earley.empty ())) []
               else [])
              (List.append
                 (if lvl = 1
                  then
                    List.cons
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.string "---" "---")
                         (Earley_core.Earley.empty ())) []
                  else [])
                 (List.append
                    (if lvl = 2
                     then
                       List.cons
                         (Earley_core.Earley.fsequence_ignore
                            (Earley_core.Earley.string "----" "----")
                            (Earley_core.Earley.empty ())) []
                     else [])
                    (List.append
                       (if lvl = 3
                        then
                          List.cons
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.string "-----" "-----")
                               (Earley_core.Earley.empty ())) []
                        else [])
                       (List.append
                          (if lvl = 4
                           then
                             List.cons
                               (Earley_core.Earley.fsequence_ignore
                                  (Earley_core.Earley.string "------"
                                     "------") (Earley_core.Earley.empty ()))
                               []
                           else [])
                          (List.append
                             (if lvl = 5
                              then
                                List.cons
                                  (Earley_core.Earley.fsequence_ignore
                                     (Earley_core.Earley.string "-------"
                                        "-------")
                                     (Earley_core.Earley.empty ())) []
                              else [])
                             (List.append
                                (if lvl = 6
                                 then
                                   List.cons
                                     (Earley_core.Earley.fsequence_ignore
                                        (Earley_core.Earley.string "--------"
                                           "--------")
                                        (Earley_core.Earley.empty ())) []
                                 else []) []))))))))
    let (text_item, text_item__set__grammar) =
      Earley_core.Earley.grammar_family "text_item"
    let (topleveltext, topleveltext__set__grammar) =
      Earley_core.Earley.grammar_family "topleveltext"
    let text = Earley_core.Earley.declare_grammar "text"
    let _ =
      text_item__set__grammar
        (fun lvl ->
           Earley_core.Earley.alternatives
             (List.append
                (if lvl < 8
                 then
                   List.cons
                     (Earley_core.Earley.fsequence paragraph
                        (Earley_core.Earley.empty
                           (fun ps ->
                              fun indent ->
                                fun lvl -> (true, lvl, (ps indent))))) []
                 else [])
                (List.append
                   (if lvl < 8
                    then
                      List.cons
                        (Earley_core.Earley.fsequence
                           (Earley_str.regexp ~name:"[-=_]>" "[-=_]>"
                              (fun group -> group 0))
                           (Earley_core.Earley.fsequence simple_text
                              (Earley_core.Earley.fsequence
                                 (topleveltext (lvl + 1))
                                 (Earley_core.Earley.fsequence
                                    (Earley_str.regexp ~name:"[-=_]<"
                                       "[-=_]<" (fun group -> group 0))
                                    (Earley_core.Earley.empty_pos
                                       (fun __loc__start__buf ->
                                          fun __loc__start__pos ->
                                            fun __loc__end__buf ->
                                              fun __loc__end__pos ->
                                                let _loc =
                                                  locate __loc__start__buf
                                                    __loc__start__pos
                                                    __loc__end__buf
                                                    __loc__end__pos in
                                                fun cl ->
                                                  fun txt ->
                                                    fun title ->
                                                      fun op ->
                                                        let (num, in_toc) =
                                                          numbered op cl in
                                                        fun _ ->
                                                          fun lvl' ->
                                                            assert
                                                              (lvl' = lvl);
                                                            (let code =
                                                               section_text_structure_in_toc
                                                                 _loc ~in_toc
                                                                 ~num ~title
                                                                 ~body:(
                                                                 txt false
                                                                   (lvl + 1)) in
                                                             (true, lvl,
                                                               code))))))))
                        []
                    else [])
                   (List.append
                      (if lvl < 8
                       then
                         List.cons
                           (Earley_core.Earley.fsequence
                              (Earley_core.Earley.alternatives
                                 (List.cons
                                    (Earley_core.Earley.fsequence_ignore
                                       (usect lvl)
                                       (Earley_core.Earley.fsequence
                                          simple_text
                                          (Earley_core.Earley.fsequence_ignore
                                             (usect lvl)
                                             (Earley_core.Earley.empty
                                                (fun title -> (false, title))))))
                                    (List.cons
                                       (Earley_core.Earley.fsequence_ignore
                                          (sect lvl)
                                          (Earley_core.Earley.fsequence
                                             simple_text
                                             (Earley_core.Earley.fsequence_ignore
                                                (sect lvl)
                                                (Earley_core.Earley.empty
                                                   (fun title ->
                                                      (true, title)))))) [])))
                              (Earley_core.Earley.fsequence
                                 (Earley_core.Earley.greedy
                                    (topleveltext (lvl + 1)))
                                 (Earley_core.Earley.empty_pos
                                    (fun __loc__start__buf ->
                                       fun __loc__start__pos ->
                                         fun __loc__end__buf ->
                                           fun __loc__end__pos ->
                                             let _loc =
                                               locate __loc__start__buf
                                                 __loc__start__pos
                                                 __loc__end__buf
                                                 __loc__end__pos in
                                             fun txt ->
                                               fun
                                                 ((num, title) as _default_0)
                                                 ->
                                                 fun _ ->
                                                   fun lvl' ->
                                                     assert (lvl' >= lvl);
                                                     (let code =
                                                        section_text_structure_numbered
                                                          _loc ~num ~title
                                                          ~body:(txt false
                                                                   (lvl + 1)) in
                                                      (true, lvl, code))))))
                           []
                       else []) []))))
    let _ =
      topleveltext__set__grammar
        (fun lvl ->
           Earley_core.Earley.fsequence
             (Earley_core.Earley.apply (fun f -> f [])
                (Earley_core.Earley.fixpoint' (fun l -> l) (text_item lvl)
                   (fun x -> fun f -> fun l -> f (List.cons x l))))
             (Earley_core.Earley.empty
                (fun l ->
                   fun indent ->
                     fun lvl ->
                       let fn (indent, lvl, ast) txt =
                         let (indent, lvl, ast') = txt indent lvl in
                         (indent, lvl, (ast @ ast')) in
                       let (_, _, r) = List.fold_left fn (indent, lvl, []) l in
                       r)))
    let _ =
      Earley_core.Earley.set_grammar text
        (Earley_core.Earley.fsequence (topleveltext 0)
           (Earley_core.Earley.empty (fun txt -> txt true 0)))
    let patoline_config : unit grammar =
      change_layout
        (Earley_core.Earley.alternatives
           (List.cons
              (Earley_core.Earley.fsequence_ignore
                 (Earley_core.Earley.string "#GRAMMAR " "#GRAMMAR ")
                 (Earley_core.Earley.fsequence
                    (Earley_str.regexp ~name:"[a-zA-Z]+" "[a-zA-Z]+"
                       (fun group -> group 0))
                    (Earley_core.Earley.empty (fun g -> add_grammar g))))
              (List.cons
                 (Earley_core.Earley.fsequence_ignore
                    (Earley_core.Earley.string "#FORMAT " "#FORMAT ")
                    (Earley_core.Earley.fsequence uid
                       (Earley_core.Earley.empty
                          (fun f ->
                             set_patoline_format f;
                             (try add_grammar f with | _ -> ())))))
                 (List.cons
                    (Earley_core.Earley.fsequence_ignore
                       (Earley_core.Earley.string "#DRIVER " "#DRIVER ")
                       (Earley_core.Earley.fsequence uid
                          (Earley_core.Earley.empty
                             (fun d -> set_patoline_driver d))))
                    (List.cons
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_core.Earley.string "#PACKAGES "
                             "#PACKAGES ")
                          (Earley_core.Earley.fsequence
                             (Earley_str.regexp ~name:"[,a-zA-Z]+"
                                "[,a-zA-Z]+" (fun group -> group 0))
                             (Earley_core.Earley.empty
                                (fun ps -> add_patoline_packages ps)))) [])))))
        no_blank
    let header = Earley_core.Earley.declare_grammar "header"
    let _ =
      Earley_core.Earley.set_grammar header
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.greedy
              (Earley_core.Earley.apply (fun f -> f [])
                 (Earley_core.Earley.fixpoint' (fun l -> l) patoline_config
                    (fun x -> fun f -> fun l -> f (List.cons x l)))))
           (Earley_core.Earley.empty
              (fun () ->
                 List.iter add_grammar (!patoline_grammar); build_grammar ())))
    let title = Earley_core.Earley.declare_grammar "title"
    let _ =
      Earley_core.Earley.set_grammar title
        (Earley_core.Earley.fsequence
           (Earley_str.regexp "==========\\(=*\\)" (fun group -> group 0))
           (Earley_core.Earley.fsequence simple_text
              (Earley_core.Earley.fsequence
                 (Earley_core.Earley.option (None, None, None)
                    (Earley_core.Earley.fsequence
                       (Earley_core.Earley.fsequence_ignore
                          (Earley_str.regexp "----------\\(-*\\)"
                             (fun group -> group 0))
                          (Earley_core.Earley.fsequence simple_text
                             (Earley_core.Earley.empty
                                (fun _default_0 -> _default_0))))
                       (Earley_core.Earley.fsequence
                          (Earley_core.Earley.option (None, None)
                             (Earley_core.Earley.fsequence
                                (Earley_core.Earley.fsequence_ignore
                                   (Earley_str.regexp "----------\\(-*\\)"
                                      (fun group -> group 0))
                                   (Earley_core.Earley.fsequence simple_text
                                      (Earley_core.Earley.empty
                                         (fun _default_0 -> _default_0))))
                                (Earley_core.Earley.fsequence
                                   (Earley_core.Earley.option None
                                      (Earley_core.Earley.apply
                                         (fun x -> Some x)
                                         (Earley_core.Earley.fsequence_ignore
                                            (Earley_str.regexp
                                               "----------\\(-*\\)"
                                               (fun group -> group 0))
                                            (Earley_core.Earley.fsequence
                                               simple_text
                                               (Earley_core.Earley.empty
                                                  (fun _default_0 ->
                                                     _default_0))))))
                                   (Earley_core.Earley.empty
                                      (fun date ->
                                         fun inst -> ((Some inst), date))))))
                          (Earley_core.Earley.empty
                             (fun ((inst, date) as _default_0) ->
                                fun auth -> ((Some auth), inst, date))))))
                 (Earley_core.Earley.fsequence
                    (Earley_str.regexp "==========\\(=*\\)"
                       (fun group -> group 0))
                    (Earley_core.Earley.empty_pos
                       (fun __loc__start__buf ->
                          fun __loc__start__pos ->
                            fun __loc__end__buf ->
                              fun __loc__end__pos ->
                                let _loc =
                                  locate __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun _default_0 ->
                                  fun ((auth, inst, date) as _default_1) ->
                                    fun title ->
                                      fun _default_2 ->
                                        let date =
                                          match date with
                                          | None -> Pa_ast.exp_Nil _loc
                                          | Some t ->
                                              Pa_ast.exp_list _loc
                                                [Ast_helper.Exp.tuple
                                                   ~loc:_loc
                                                   [None, Pa_ast.exp_string _loc
                                                      "Date";
                                                   None, Pa_ast.exp_apply1 _loc
                                                     (Pa_ast.exp_ident _loc
                                                        "string_of_contents")
                                                     t]] in
                                        let inst =
                                          match inst with
                                          | None -> Pa_ast.exp_Nil _loc
                                          | Some t ->
                                              Pa_ast.exp_list _loc
                                                [Ast_helper.Exp.tuple
                                                   ~loc:_loc
                                                   [None, Pa_ast.exp_string _loc
                                                      "Institute";
                                                   None, Pa_ast.exp_apply1 _loc
                                                     (Pa_ast.exp_ident _loc
                                                        "string_of_contents")
                                                     t]] in
                                        let auth =
                                          match auth with
                                          | None -> Pa_ast.exp_Nil _loc
                                          | Some t ->
                                              Pa_ast.exp_list _loc
                                                [Ast_helper.Exp.tuple
                                                   ~loc:_loc
                                                   [None, Pa_ast.exp_string _loc
                                                      "Author";
                                                   None, Pa_ast.exp_apply1 _loc
                                                     (Pa_ast.exp_ident _loc
                                                        "string_of_contents")
                                                     t]] in
                                        let tags =
                                          exp_infix _loc "@" auth
                                            (exp_infix _loc "@" inst date) in
                                        let title_call =
                                          Ast_helper.Exp.apply ~loc:_loc
                                            (Ast_helper.Exp.ident ~loc:_loc
                                               (iddot _loc "Patoline_Format"
                                                  "title"))
                                            [(Asttypes.Nolabel,
                                               (Ast_helper.Exp.ident
                                                  ~loc:_loc
                                                  (iddot _loc "D" "structure")));
                                            ((Labelled "extra_tags"), tags);
                                            (Asttypes.Nolabel, title)] in
                                        [str_let_any _loc title_call]))))))
    let wrap basename _loc ast =
      wrap_document_structure _loc ~basename
        ~patoline_format:(!patoline_format)
        ~cache_arr:(Pa_ast.exp_array _loc (List.rev (!cache_buf)))
        ~mcache_arr:(Pa_ast.exp_array _loc (List.rev (!mcache_buf))) ~ast
    let full_text = Earley_core.Earley.declare_grammar "full_text"
    let _ =
      Earley_core.Earley.set_grammar full_text
        (Earley_core.Earley.iter
           (Earley_core.Earley.fsequence header
              (Earley_core.Earley.empty_pos
                 (fun __loc__start__buf ->
                    fun __loc__start__pos ->
                      fun __loc__end__buf ->
                        fun __loc__end__pos ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun f ->
                            let _ = f () in
                            let file =
                              match !file with | None -> "" | Some f -> f in
                            let (_, base, _) = Filename.decompose file in
                            let _ = cache := ("cache_" ^ base) in
                            Earley_core.Earley.fsequence
                              (Earley_core.Earley.option None
                                 (Earley_core.Earley.apply (fun x -> Some x)
                                    (Earley_core.Earley.fsequence text
                                       (Earley_core.Earley.fsequence title
                                          (Earley_core.Earley.empty
                                             (fun t -> fun tx1 -> (tx1, t)))))))
                              (Earley_core.Earley.fsequence text
                                 (Earley_core.Earley.fsequence_ignore
                                    (Earley_core.Earley.eof ())
                                    (Earley_core.Earley.empty_pos
                                       (fun __loc__start__buf ->
                                          fun __loc__start__pos ->
                                            fun __loc__end__buf ->
                                              fun __loc__end__pos ->
                                                let _loc =
                                                  locate __loc__start__buf
                                                    __loc__start__pos
                                                    __loc__end__buf
                                                    __loc__end__pos in
                                                fun tx2 ->
                                                  fun t ->
                                                    let t =
                                                      match t with
                                                      | None -> []
                                                      | Some (tx1, t) ->
                                                          tx1 @ t in
                                                    wrap base _loc (t @ tx2)))))))))
    let directive = Earley_core.Earley.declare_grammar "directive"
    let _ =
      Earley_core.Earley.set_grammar directive
        (Earley_core.Earley.fsequence_ignore
           (Earley_core.Earley.char '#' '#')
           (Earley_core.Earley.fsequence uid
              (Earley_core.Earley.fsequence uid
                 (Earley_core.Earley.empty
                    (fun a ->
                       fun n ->
                         (match n with
                          | "FORMAT" -> patoline_format := a
                          | "DRIVER" -> patoline_driver := a
                          | "PACKAGE" ->
                              patoline_packages := (a ::
                                (!patoline_packages))
                          | _ -> give_up ());
                         [])))))
    let extra_structure = directive :: extra_structure
    let (patoline_quotations, patoline_quotations__set__grammar) =
      Earley_core.Earley.grammar_family "patoline_quotations"
    let _ =
      patoline_quotations__set__grammar
        (fun (_, lvl) ->
           Earley_core.Earley.alternatives
             (List.append
                (if lvl <= Atom
                 then
                   List.cons
                     (Earley_core.Earley.fsequence_ignore
                        (Earley_core.Earley.string "<$" "<$")
                        (Earley_core.Earley.fsequence math_toplevel
                           (Earley_core.Earley.fsequence_ignore
                              (Earley_core.Earley.string "$>" "$>")
                              (Earley_core.Earley.empty (fun mat -> mat)))))
                     []
                 else [])
                (List.append
                   (if lvl <= Atom
                    then
                      List.cons
                        (Earley_core.Earley.fsequence_ignore
                           (Earley_core.Earley.string "<<" "<<")
                           (Earley_core.Earley.fsequence simple_text
                              (Earley_core.Earley.fsequence_ignore
                                 (Earley_core.Earley.string ">>" ">>")
                                 (Earley_core.Earley.empty (fun par -> par)))))
                        []
                    else []) [])))
    let _ =
      let reserved = ["<<"; ">>"; "<$"; "$>"; "<<$"; "$>>"] in
      List.iter Pa_lexing.add_reserved_symb reserved
    let extra_expressions = patoline_quotations :: extra_expressions
    let entry_points =
      let parse_ml =
        Earley_core.Earley.iter
          (Earley_core.Earley.fsequence header
             (Earley_core.Earley.empty
                (fun f ->
                   let _ =
                     try f ()
                     with
                     | e ->
                         Printf.eprintf "Exception: %s\nTrace:\n%!"
                           (Printexc.to_string e) in
                   structure))) in
      let parse_mli =
        Earley_core.Earley.iter
          (Earley_core.Earley.fsequence header
             (Earley_core.Earley.empty
                (fun f ->
                   let _ =
                     try f ()
                     with
                     | e ->
                         Printf.eprintf "Exception: %s\nTrace:\n%!"
                           (Printexc.to_string e) in
                   signature))) in
      [(".txp", (Implementation (full_text, blank2)));
      (".ml", (Implementation (parse_ml, blank2)));
      (".mli", (Interface (parse_mli, blank2)))]
  end
let write_main_file ?(output_file : string option = None) driver form build_dir dir name =
  let full = match output_file with
    | Some f -> f
    | None -> Filename.concat dir name
  in
  let fullb = Filename.concat build_dir name in
  let file = fullb ^ "_.ml" in
  let oc = open_out file in
  let dcache = fullb ^ ".tdx" in
  let fmt = Format.formatter_of_out_channel oc in
  let _loc = Location.none in
  let m =
    let c = String.make 1 (Char.uppercase_ascii (name.[0])) in
    let cs = String.sub name 1 ((String.length name) - 1) in c ^ cs in
  let ast =
    Pa_patoline_ast_helpers.write_main_structure _loc ~dcache ~driver ~form
      ~m ~full in
  Format.fprintf fmt "%a\n%!" Pprintast.structure ast;
  close_out oc;
  if !debug then Printf.eprintf "Written main file %s\n%!" file
let _ =
  try
    let module ParserExt = (Pa_parser.Ext)(Pa_ocaml_prelude.Initial) in
      let module PaExt = (Ext)(ParserExt) in
        let module PatolineDefault = (Pa_ocaml.Make)(PaExt) in
          let module M = (Pa_main.Start)(PatolineDefault) in
            let open PaExt in
              match ((!Pa_ocaml_prelude.file), (!in_ocamldep)) with
              | (Some s, false) ->
                  let (dir, base, _) = Filename.decompose s in
                  let name = base ^ ".tgy" in
                  let build_dir = !build_dir in
                  let name = Filename.concat build_dir name in
                  (if !debug
                   then Printf.eprintf "Writing grammar %s\n%!" name;
                   if not (Sys.file_exists build_dir)
                   then Unix.mkdir build_dir 0o700;
                   if local_state <> empty_state
                   then
                     (let ch = open_out_bin name in
                      output_value ch local_state;
                      close_out ch;
                      if !debug
                      then Printf.eprintf "Written grammar %s\n%!" name);
                   if !is_main
                   then
                     (let (drv, fmt) =
                        ((!patoline_driver), (!patoline_format)) in
                      write_main_file drv fmt build_dir dir base))
              | _ -> ()
  with
  | e ->
      (Printf.eprintf "Exception: %s\nTrace:\n%!" (Printexc.to_string e);
       Printexc.print_backtrace stderr;
       exit 1)
