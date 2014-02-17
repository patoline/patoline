(* ocamlc -pp ../pa_glr -I .. -I +camlp4 dynlink.cma camlp4lib.cma str.cma umap.cmo glr.cmo -o pato pato.ml
    ocamlfind ocamlopt -pp ./pato -package Typography,Typography.Pdf -linkpkg  -impl toto.txt
*)

open Glr
open Charset
open Camlp4.PreCast
open Syntax
open FilenameExtra

module Id : Camlp4.Sig.Id =
  struct
    let name = "pa_patoline"
    let version = "0.1"
  end

(****************************************************************************
 * Things that have to do with comments and things to be ignored            *
 ****************************************************************************)

exception Unclosed_comment of int

(*
 * Characters to be ignored are:
 *   - ' ', '\t', '\r',
 *   - '\n' if mline is true,
 *   - everything between "(*" and "*)" (ocaml-like comments).
 * Remarks on what is allowed inside an ocaml-like comment:
 *   - nested comments,
 *   - single-line string literals including those containing the substrings
 *     "(*" and or "*)",
 *   - single '"' character.
 *)
let blank mline str pos =
  let len = String.length str in
  let rec fn nb lvl state pos =
    if pos >= len then (if lvl > 0 then raise (Unclosed_comment len) else len)
    else match state, str.[pos] with
      | `Ini , '('               -> fn nb lvl `Opn (pos + 1)
      | `Opn , '*'               -> fn nb (lvl + 1) `Ini (pos + 1)
      | `Opn , _  when lvl = 0   -> pos - 1
      | `Opn , _                 -> fn nb lvl `Ini (pos + 1)
      | `Ini , '*' when lvl = 0  -> pos
      | `Ini , '*'               -> fn nb lvl `Cls (pos + 1)
      | `Cls , '*'               -> fn nb lvl `Cls (pos + 1)
      | `Cls , ')'               -> fn 0 (lvl - 1) `Ini (pos + 1)
      | `Cls , _                 -> fn nb lvl `Ini (pos + 1)

      | _    , '\n' when lvl > 0 -> fn nb lvl `Ini (pos + 1)
      | _    , '\n' when mline   -> fn nb lvl `Ini (pos + 1)
      | _    , '\n' when nb > 0  -> pos
      | _    , '\n'              -> fn (nb + 1) lvl `Ini (pos + 1)

      | `Str , '"'               -> fn nb lvl `Ini (pos + 1)
      | _    , '"' when lvl > 0  -> (try fn nb lvl `Str (pos + 1) with
                                      Unclosed_comment _ ->
                                        fn nb lvl `Ini (pos + 1))
      | `Str , '\\'              -> fn nb lvl `Esc (pos + 1)
      | `Esc , _                 -> fn nb lvl `Str (pos + 1)
      | `Str , _                 -> fn nb lvl `Str (pos + 1)

      | _    , (' '|'\t'|'\r')   -> fn nb lvl `Ini (pos + 1)
      | _    , _ when lvl > 0    -> fn nb lvl `Ini (pos + 1)
      | _    , _                 -> pos
  in fn 0 0 `Ini pos

let blank1 = blank false
let blank2 = blank true

let no_blank _ pos = pos

(****************************************************************************
 * Some state information (Driver in use, ...) + Code generation helpers    *
 ****************************************************************************)

let patoline_format   = ref "DefaultFormat"
let patoline_driver   = ref "Pdf"
let patoline_packages = ref ["Typography"]

(* Should contain the name of the file being parsed. *)
let fname = ref ""

(* Function for geting fresh module names (Uid) *)
let counter = ref 1
let freshUid () =
  let current = !counter in
  incr counter;
  "MOD" ^ (string_of_int current)

(****************************************************************************
 * Functions for computing line numbers and positions.                      *
 ****************************************************************************)

(* computes the line number (with a cache) for a given position in a string *)
let bol = Hashtbl.create 1001
let find_pos str n =
  let rec fn i =
    (*Printf.fprintf stderr "str: %s, i: %d\n%!" str i;*)
    if i < String.length str && str.[i] = '\n' then
      try Hashtbl.find bol i, i
      with Not_found ->
        if i = 0 then (2,i) else
          let lnum, _ = fn (i-1) in
          let lnum = lnum + 1 in
          Hashtbl.add bol i lnum;
          lnum, i
    else if i <= 0 then (1, i)
    else fn (i-1)
  in
  let (lnum, bol) = fn n in
  Lexing.({ pos_fname = !fname;
            pos_lnum  = lnum;
            pos_bol   = bol;
            pos_cnum  = n })

let locate g =
  filter_position g (fun str pos pos' ->
    let s = find_pos str pos in
    let e = find_pos str pos' in
    Loc.(merge (of_lexing_position s) (of_lexing_position e)))

let _ = glr_locate locate Loc.merge

(****************************************************************************
 * Camlp4 extension and interaction between OCaml and Patoline's parser.   *
 ****************************************************************************)

let patoline_basic_text_paragraph = declare_grammar ()

let parser_stack = Stack.create ()

module Extension (Syntax : Camlp4.Sig.Camlp4Syntax) =
  struct
    let patoline_caml_expr = Gram.Entry.mk "patoline_caml_expr"

    let patoline_caml_struct = Gram.Entry.mk "patoline_caml_struct"

    let patoline_caml_list = Gram.Entry.mk "patoline_caml_list"

    let patoline_caml_array = Gram.Entry.mk "patoline_caml_array"

    EXTEND Gram
      patoline_caml_expr: [ [
        e = expr LEVEL "top"; ")" -> <:expr<(fun x -> x) $e$>>
        (* e only does not set the position to contain the ")" *)
      ] ];

      patoline_caml_struct: [ [
        e = str_items; ")" -> <:str_item<$e$ let _ = ()>>
        (* e only does not set the position to contain the ")" *)
      ] ];

      patoline_caml_list: [ [
        l = LIST0 (expr LEVEL "top") SEP ";"; "]" ->
          List.fold_left
            (fun tl hd -> <:expr< $hd$ :: $tl$ >>) <:expr< [] >>
            (List.rev l)
      ] ];

      patoline_caml_array: [ [
        l = LIST0 (expr LEVEL "top") SEP ";"; "|]" ->
          let l = List.fold_left
                    (fun tl hd -> <:expr< $hd$ :: $tl$ >>) <:expr< [] >>
                    (List.rev l) in
          <:expr<Array.of_list $l$>>
      ] ];

      str_item: [ [
        "#"; n = UIDENT; a = UIDENT ->
          (match n with
             | "FORMAT"  -> patoline_format := a
             | "DRIVER"  -> patoline_driver := a
             | "PACKAGE" -> patoline_packages := a :: !patoline_packages;
             | _ -> let err = Stream.Error "Unknown Patoline directive."
                    in Loc.raise _loc err);
          <:str_item<>>
      ] ];

      expr: LEVEL "simple" [ [
        "<|" ->
          let str, ptr = try Stack.top parser_stack
                         with Stack.Empty -> assert false
          in
          assert (!ptr = 0);
          let pos = Loc.stop_off _loc + 1 in
          let parse = glr p:patoline_basic_text_paragraph STR("|>") -> p end in
          let new_pos, ast = partial_parse_string parse blank2 str pos in
          ptr := new_pos - pos - 1; ast
      ] ];
    END;;
  end

module M0 = Camlp4OCamlRevisedParser.Make(Syntax)
module M1 = Camlp4OCamlParser.Make(M0)
module M = Extension(M1)
open M

let print_state ch state =
  match state with
    `Start -> Printf.fprintf ch "Start"
  | `Less ->  Printf.fprintf ch "Less"
  | `Patoline ->  Printf.fprintf ch "Patoline"
  | `PatolineNext ->  Printf.fprintf ch "PatolineNext"
  | `Comment n ->  Printf.fprintf ch "Comment(%d)" n
  | `Par(n,_) ->  Printf.fprintf ch "Par(%d)" n
  | `Star(n,_) ->  Printf.fprintf ch "Star(%d)" n
  | _ -> Printf.fprintf ch "?"

let next_state state char = match state, char with
    `Start, '(' -> `Par(0, state)
  | `Comment n, '(' -> `Par(n, state)
  | `Par (n,st), '*' -> `Comment (n+1)
  | `Par(n,st), '(' -> state
  | `Par(_,st), _ -> st
  | `Comment n, '*' -> `Star(n,state)
  | `Star(1,st), ')' -> `Start
  | `Star(n,st), ')' -> `Comment (n-1)
  | `Star(_,st), '*' -> state
  | `Star(_,st), _ -> st

  | `String st, '\\' -> `Escape st
  | `Escape st, _ -> `String st
  | `String st, '"' -> st
  | `Start, '<' -> `Less
  | `Less, ('|'|'$') -> `Patoline
  | st, '"' -> `String st
  | st, _ -> st

let mk_stream str pos =
  let len = String.length str in
  let ptr = ref 0 in
  let state = ref `Start in
  Stack.push (str, ptr) parser_stack;
  Stream.from (fun n ->
    (*Printf.fprintf stderr
       "n: %d, pos: %d, c: %c, len %d, ptr: %d, state %a\n%!"
       n pos str.[n+pos] len !ptr print_state !state;*)
    if n+pos>= len then None
     else if !state = `Patoline then begin
        state := `Start; Some ' '
      end else if !ptr > 0 then begin
        decr ptr; Some ' '
      end else begin
        let char = str.[n+pos] in
        state := next_state !state char;
        Some char
      end;
      )

let caml_expr _loc =
  let fn str pos =
(*    Printf.fprintf stderr "entering caml_expr %d\n%!" pos;*)
    let cs = mk_stream str pos in
    let r = Gram.parse patoline_caml_expr _loc cs in
    ignore (Stack.pop parser_stack);
    let pos = Loc.stop_off (Ast.loc_of_expr r) in
(*    Printf.fprintf stderr "pos after caml_expr: %d\n%!" pos;*)
    r, pos+1
  in
  black_box fn full_charset false

let caml_struct _loc =
  let fn str pos =
(*    Printf.fprintf stderr "entering caml_struct %d\n%!" pos;*)
    let cs = mk_stream str pos in
    let r = Gram.parse patoline_caml_struct _loc cs in
    ignore (Stack.pop parser_stack);
    let pos = Loc.stop_off (Ast.loc_of_str_item r) in
    (*Printf.fprintf stderr "pos after caml_struct: %d, %d\n%!"
      (Loc.start_off (Ast.loc_of_str_item r)) pos;*)
    r, pos+1
  in
  black_box fn full_charset false

let caml_list _loc =
  let fn str pos =
(*    Printf.fprintf stderr "entering caml_struct %d\n%!" pos;*)
    let cs = mk_stream str pos in
    let r = Gram.parse patoline_caml_list _loc cs in
    ignore (Stack.pop parser_stack);
    let pos = Loc.stop_off (Ast.loc_of_expr r) in
    (*Printf.fprintf stderr "pos after caml_struct: %d, %d\n%!"
      (Loc.start_off (Ast.loc_of_str_item r)) pos;*)
    r, pos+1
  in
  black_box fn full_charset false

let caml_array _loc =
  let fn str pos =
(*    Printf.fprintf stderr "entering caml_struct %d\n%!" pos;*)
    let cs = mk_stream str pos in
    let r = Gram.parse patoline_caml_array _loc cs in
    ignore (Stack.pop parser_stack);
    let pos = Loc.stop_off (Ast.loc_of_expr r) in
    (*Printf.fprintf stderr "pos after caml_struct: %d, %d\n%!"
      (Loc.start_off (Ast.loc_of_str_item r)) pos;*)
    r, pos+1
  in
  black_box fn full_charset false

(****************************************************************************
 * Parsing OCaml code inside patoline.                                      *
 ****************************************************************************)

(* Parse a caml "str_item" wrapped with parentheses prefixed by init_str *)
let wrapped_caml_str_item init_str =
  dependent_sequence (locate (glr p:STR(init_str ^ "(") end))
    (fun (_loc,_) -> caml_struct _loc)

(* Parse a caml "expr" wrapped with parentheses *)
let wrapped_caml_expr =
  dependent_sequence (locate (glr p:STR("(") end))
    (fun (_loc,_) -> caml_expr _loc)

(* Parse a list of caml "expr" *)
let wrapped_caml_list =
  dependent_sequence (locate (glr p:STR("[") end))
    (fun (_loc,_) -> caml_list _loc)

(* Parse an array of caml "expr" *)
let wrapped_caml_array =
  dependent_sequence (locate (glr p:STR("[|") end))
    (fun (_loc,_) -> caml_array _loc)

let patoline_ocaml = wrapped_caml_str_item "\\Caml"

(****************************************************************************
 * Words.                                                                   *
 ****************************************************************************)

let char_re    = "[^ \t\r\n{}\\_$|/*#\"]"
let escaped_re = "\\\\[\\$|({)}/*#\"]"

let character =
  glr
    c:RE(char_re) -> c
  | s:RE(escaped_re) ->
     String.escaped (String.sub s 1 (String.length s - 1))
  end

let word =
  change_layout (
    glr
      cs:character++ ->
        let w = String.concat "" cs in
        if String.length w >= 2 &&
           List.mem (String.sub w 0 2) ["==";"=>";"=<";"--";"->";"-<"]
        then raise Give_up;
        w
    end
  ) no_blank

let rec rem_hyphen = function
  | []        -> []
  | w::[]     -> w::[]
  | w1::w2::l -> let l1 = String.length w1 in
                 if w1.[l1 - 1] = '-'
                 then let w = String.sub w1 0 (l1 - 1) ^ w2
                      in rem_hyphen (w :: l)
                 else w1 :: rem_hyphen (w2::l)

let words =
  glr
    ws:word++ -> String.concat " " (rem_hyphen ws)
  end

(****************************************************************************
 * Verbatim environment / macro                                             *
 ****************************************************************************)

let verbatim_line   =
  "\\(^[^#\t\n][^\t\n]*\\)\\|\\(^#?#?[^#\t\n][^\t\n]*\\)\\|\\(^[ \t]*\\)"

let string_filename = "\\\"[a-zA-Z0-9-_.]*\\\""
let uid_coloring    = "[A-Z][_'a-zA-Z0-9]*"

let verbatim_environment =
  change_layout (
    glr
      RE("^###")
      lang:glr RE("[ \t]+") id:RE(uid_coloring) -> id end?
      file:glr RE("[ \t]+") fn:RE(string_filename) -> fn end?
      RE("[ \t]*\n")
      lines:glr l:RE(verbatim_line) RE("\n") -> l end++
      RE("^###\n") -> (
        (* TODO do something with "lang" and "file" *)
        assert(List.length lines <> 0); (* Forbid empty verbatim env *)

        (* Remove the maximum of head spaces uniformly *)
        let rec count_head_spaces s acc =
          if s.[acc] = ' '
          then count_head_spaces s (acc+1)
          else acc
        in
        let count_head_spaces s =
          if String.length s = 0
          then max_int
          else count_head_spaces s 0
        in
        let fn = fun acc s -> min acc (count_head_spaces s) in
        let sps = List.fold_left fn max_int lines in
        let sps = if sps = max_int then 0 else sps in
        let fn = fun s -> try String.sub s sps (String.length s - sps)
                          with Invalid_argument _ -> ""
        in
        let lines = List.map fn lines in

        let buildline l =
          <:str_item<let _ = newPar D.structure ~environment:verbEnv
                             Complete.normal ragged_left
                             (lang_default $str:l$) >>
        in
        let fn = fun l -> buildline (String.escaped l) in
        let lines = List.map fn lines in

        let fn = fun acc r -> <:str_item<$acc$ $r$>> in
        List.fold_left fn <:str_item<>> lines )
    end
  ) no_blank

let verbatim_generic st forbid nd =
  let line_re = "[^\n" ^ forbid ^ "]+" in
  change_layout (
    glr
      STR(st)
      ls:glr l:RE(line_re) STR("\n") -> l end**
      l:RE(line_re)
      STR(nd) ->
        let lines = ls @ [l] in
        let lines = rem_hyphen lines in
        let txt = String.concat " " lines in
        <:expr<$lid:"verb"$ [tT $str:txt$]>>
    end
  ) no_blank

let verbatim_macro = verbatim_generic "\\verb{" "{}" "}"
let verbatim_sharp = verbatim_generic "##" "#" "##"

(****************************************************************************
 * Text content of paragraphs and macros (mutually recursive).              *
 ****************************************************************************)

(* bool param -> can contain special text environments //...// **...** ... *)
let paragraph_basic_text, set_paragraph_basic_text = grammar_family ()

(***** Patoline macros  *****)
let macro_argument =
  glr
     STR("{") l:(paragraph_basic_text true) STR("}") -> l
  || e:wrapped_caml_expr  -> e
  || e:wrapped_caml_array -> e
  || e:wrapped_caml_list  -> e
  end

let lident = "[_a-z][_a-zA-Z0-9']*"

let reserved_macro = [ "Caml"; "begin"; "end"; "item"; "verb" ]

let macro_name =
  change_layout (
    glr
      STR("\\") m:RE(lident) ->
        if List.mem m reserved_macro then raise Give_up; m
    end
  ) no_blank

let macro =
  glr
     m:macro_name args:macro_argument** ->
       (let fn = fun acc r -> <:expr@_loc_args<$acc$ $r$>> in
        List.fold_left fn <:expr@_loc_m<$lid:m$>> args)
  || m:verbatim_macro -> m
  end
(****************************)

let text_paragraph_elt allowed =
    glr
       m:macro -> m
    || l:words -> <:expr@_loc_l<[tT($str:l$)]>>
    || STR("//") p:(paragraph_basic_text false) _e:STR("//") when allowed ->
         <:expr@_loc_p<toggleItalic $p$>>
    || STR("**") p:(paragraph_basic_text false) _e:STR("**") when allowed ->
         <:expr@_loc_p<bold $p$>>
    (*
    || STR("__") p:(paragraph_basic_text false) _e:STR("__") when allowed ->
         <:expr@_loc_p<underline $p$>>
    *)
    || STR("\"") p:(paragraph_basic_text false) _e:STR("\"") when allowed ->
        (let opening = "``" in (* TODO addapt with the current language*)
         let closing = "''" in (* TODO addapt with the current language*)
         <:expr@_loc_p<tT($str:opening$) :: $p$ @ [tT($str:closing$)]>>)
    || v:verbatim_sharp -> <:expr@_loc_v<$v$>>
    (* || STR("$") ... STR("$") -> TODO *)
    (* || STR("$$") ... STR("$$") -> TODO *)
    end

let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
    let x = Loc.stop_off _loc_p1 and y = Loc.start_off _loc_p2 in
    (*Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
    let bl e = if y - x >= 1 then <:expr@_loc_p1<tT" "::$e$>> else e in
    let _loc = Loc.merge _loc_p1 _loc_p2 in
    <:expr<$p1$ @ $bl p2$>>

let _ = set_paragraph_basic_text
  (fun spec_allowed ->
    glr
      l:{p:(text_paragraph_elt spec_allowed) -> (_loc, p)}++ ->
        match List.rev l with
          | []   -> assert false
          | m::l ->
              let fn = fun (_loc_m, m) (_loc_p, p) ->
                         (Loc.merge _loc_p _loc_m
                         , concat_paragraph p _loc_p m _loc_m)
              in snd (List.fold_left fn m l)
    end
  ) [ true ]

let text_only = change_layout (paragraph_basic_text true) blank1

let paragraph_basic_text =
  glr
    p:(paragraph_basic_text true) ->
      (fun indented ->
        if indented then
            <:str_item@_loc_p<
               let _ = newPar D.structure
                         Complete.normal Patoline_Format.parameters $p$ >>
        else
              <:str_item@_loc_p<
               let _ = newPar D.structure
                         ~environment:(fun x -> { x with par_indent = [] })
                         Complete.normal Patoline_Format.parameters $p$>>)
  end

let _ = set_grammar patoline_basic_text_paragraph text_only

(****************************************************************************
 * Paragraphs                                                               *
 ****************************************************************************)

let paragraph = declare_grammar ()
let paragraphs = declare_grammar ()

let paragraph_elt =
  glr
       verb:verbatim_environment -> (fun _ -> verb)
    || s:patoline_ocaml -> (fun _ -> s)
    || l:paragraph_basic_text -> l
    || STR("\\item") -> (fun _ ->
        (let m1 = freshUid () in
         let m2 = freshUid () in
         <:str_item< module $uid:m1$ =
                     struct
                       module $uid:m2$ = $uid:"Item"$ ;;
                       let _ = $uid:m2$.do_begin_env () ;;
                       let _ = $uid:m2$.do_end_env ()
                     end>>))
    || STR("\\begin{") idb:RE(lident) STR("}")
       ps:(change_layout paragraphs blank2)
       STR("\\end{") ide:RE(lident) STR("}") ->
         (fun indent_first ->
           if idb <> ide then raise Give_up;
           let m1 = freshUid () in
           let m2 = freshUid () in
           <:str_item< module $uid:m1$ =
                       struct
                         module $uid:m2$ = $uid:"Env_"^idb$ ;;
                         open $uid:m2$ ;;
                         let _ = $uid:m2$.do_begin_env () ;;
                         $ps indent_first$ ;;
                         let _ = $uid:m2$.do_end_env ()
                        end>>)
  end

let _ = set_grammar paragraph (
  change_layout (
    glr
      e:paragraph_elt es:paragraph_elt** ->
        (let fn = fun acc r -> <:str_item<$acc$ $r false$>> in
         let es = List.fold_left fn <:str_item<>> es in
         fun indent -> <:str_item<$e indent$;; $es$>>
        )
    end
  ) blank1)

let _ = set_grammar paragraphs (
  glr
    p:paragraph ps:paragraph** ->
      (fun indent_first ->
        let p  = p indent_first in
        let fn = fun acc r -> <:str_item<$acc$ $r true$>> in
        List.fold_left fn p ps)
  end)

(****************************************************************************
 * Sections, layout of the document.                                        *
 ****************************************************************************)

let text = declare_grammar ()

let section = "\\(===?=?=?=?=?=?=?\\)\\|\\(---?-?-?-?-?-?-?\\)"
let op_section = "[-=]>"
let cl_section = "[-=]<"

let text_item =
  glr
     op:RE(op_section) title:text_only txt:text cl:RE(cl_section) ->
       (fun _ lvl ->
        let numbered = match op.[0], cl.[0] with
            '=', '=' -> <:expr@_loc_op<newStruct>>
          | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
          | _ -> raise Give_up
        in
        true, lvl, <:str_item< let _ = $numbered$ D.structure $title$;;
                               $txt false (lvl+1)$;;
                               let _ = go_up D.structure >>)

  || op:RE(section) title:text_only cl:RE(section) txt:text ->
       (fun _ lvl ->
        if String.length op <> String.length cl then raise Give_up;
        let numbered = match op.[0], cl.[0] with
            '=', '=' -> <:expr@_loc_op<newStruct>>
          | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
          | _ -> raise Give_up
        in
        let l = String.length op - 1 in
        if l > lvl + 1 then failwith "Illegal level skip";
        let res = ref <:str_item@_loc_op<>> in
        for i = 0 to lvl - l do
          res := <:str_item@_loc_op< $!res$ let _ = go_up D.structure>>
        done;
        true, lvl, <:str_item< $!res$ let _ = $numbered$ D.structure $title$;;
                               $txt false l$>>)

  || ps:paragraphs -> 
      (fun indent lvl -> indent, lvl, ps indent)
  end

let _ = set_grammar text (
  glr
    l:text_item** ->
      (fun indent lvl ->
        let fn = fun (indent, lvl, ast) txt ->
                   let indent, lvl, ast' = txt indent lvl in
                   indent, lvl, <:str_item<$ast$;; $ast'$>>
        in
        let _,_,r = List.fold_left fn (indent, lvl, <:str_item<>>) l in
        r)
  end)

let text =
  glr
    txt:text -> <:str_item<$txt true 0$>>
  end

(****************************************************************************
 * Header, Title, Document body.                                            *
 ****************************************************************************)

let uident = "[A-Z][a-zA-Z0-9_']*"

let patoline_config =
  change_layout (
    glr
      STR("#") n:RE(uident) STR(" ") a:RE(uident) ->
        ((match n with
            | "FORMAT"  -> patoline_format := a
            | "DRIVER"  -> patoline_driver := a
            | "PACKAGE" -> patoline_packages := a :: !patoline_packages;
            | _         -> raise Give_up);
        <:str_item<>>)
    end
  ) no_blank

let header =
  glr
    hs:patoline_config** -> <:str_item<>>
  end

let title =
  glr
    RE("==========\\(=*\\)") t:text_only
    author:{RE("----------\\(-*\\)") t:text_only}??
    institute:{RE("----------\\(-*\\)") t:text_only}??
    date:{RE("----------\\(-*\\)") t:text_only}??
    RE("==========\\(=*\\)") ->
  let extras =
    match date with
      None -> <:expr@_loc_date<[]>>
    | Some(t) -> <:expr@_loc_date<[("Date", string_of_contents $t$)]>>
  in
  let extras =
    match institute with
      None -> <:expr@_loc_date<$extras$>>
    | Some(t) -> <:expr@_loc_date<("Institute", string_of_contents $t$)::$extras$>>
  in
  let extras =
    match author with
      None -> <:expr@_loc_date<$extras$>>
    | Some(t) -> <:expr@_loc_date<("Author", string_of_contents $t$)::$extras$>>
  in
  <:str_item@_loc_t<
    let _ = Patoline_Format.title D.structure ~extra_tags:$extras$ $t$>>
 end

let full_text =
  glr
    h:header t:title?? txt:text EOF ->
      let t = match t with
                | None   -> <:str_item<>>
                | Some t -> t
      in
      <:str_item<$h$ $t$ $txt$>>
  end

(****************************************************************************
 * Parsing main functions.                                                  *
 ****************************************************************************)

(* Parse a string with Patoline as the entry point. *)
let parse_patoline str =
  try
    parse_string full_text blank2 str
  with
    | Parse_error n ->
        let _loc = find_pos str n in
        Loc.raise Loc.(of_lexing_position _loc) Stream.Failure
    | Ambiguity(n,p) ->
        let _loc1 = find_pos str n in
        let _loc2 = find_pos str p in
        Loc.raise Loc.(merge (of_lexing_position _loc1)
                             (of_lexing_position _loc2))
                  (Stream.Error "Ambiguous expression")
    | exc ->
        Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exc;
        exit 1

(* Parse a string with OCaml Camlp4 extension as the entry point. *)
let parse_patoline_caml str =
  try
    Gram.parse_string patoline_caml_struct (Loc.mk !fname) str
  with
    | exc ->
        Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exc;
        exit 1

(****************************************************************************
 * Main program + Command-line argument parsing.                            *
 ****************************************************************************)

let spec =
  [ ("--driver",  Arg.String (fun d -> patoline_driver := d),
                  "The driver against which to compile.")
  ; ("--format",  Arg.String (fun f -> patoline_format := f),
                  "The document format to use.")
  ; ("--package", Arg.String (fun p -> let pkgs = p :: !patoline_packages
                                       in patoline_packages := pkgs),
                  "Package to link.")
  ]

let patoline_extension      = [ "txp" ; "typ" ]
let patoline_extension_caml = [ "mlp" ; "ml" ]

let _ = try
  let anon_args = ref [] in
  Arg.parse spec (fun a -> anon_args := a :: !anon_args) "Usage:";
  if List.length !anon_args < 1
     then Arg.usage spec "No file specified:";
  if List.length !anon_args > 1
     then Arg.usage spec "Too many files specified:";
  let filename = List.hd !anon_args in
  let basename = chop_extension' filename in
  let extension = get_extension' filename in
  fname := filename;

  let ch = open_in filename in
  let n = in_channel_length ch in
  let str = String.create n in
  really_input ch str 0 n;

  let parse =
    if List.mem extension patoline_extension
    then parse_patoline
    else
      begin
        if List.mem extension patoline_extension_caml
        then parse_patoline_caml
        else
          begin
            let msg =
              (if extension = ""
               then "no file extension..."
               else ("wrong file extension \"" ^ extension ^ "\" ...")) in
            Printf.fprintf stderr "Warning: %s\n%!" msg;
            parse_patoline
          end
      end
  in

  let ast = parse str in
  let _loc = Loc.(of_lexing_position (find_pos str 0)) in

  let wrapped = <:str_item<
    open Typography
    open Util
    open Typography.Box
    open Typography.Config
    open Typography.Document
    open Typography.OutputCommon
    open DefaultFormat.MathsFormat
    let $lid:"cache_"^basename$  =
      ref ([||] : (environment -> Mathematical.style -> box list) array)
    let $lid:"mcache_"^basename$ =
      ref ([||] : (environment -> Mathematical.style -> box list) list array)
    module Document = functor(Patoline_Output:DefaultFormat.Output)
      -> functor(D:DocumentStructure)->struct
      module Patoline_Format = $uid:!patoline_format$.Format(D)
      open $uid:!patoline_format$
      open Patoline_Format
      let temp1 = List.map fst (snd !D.structure);;
      $ast$
      let _ = D.structure:=follow (top !D.structure) (List.rev temp1)
    end;;
    let _ = $lid:"cache_"^basename$ :=[||];;
    let _ = $lid:"mcache_"^basename$ :=[||];; >> in
  Printers.OCaml.print_implem wrapped;
  close_in ch

  with
    | exc -> Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exc;
             exit 1
