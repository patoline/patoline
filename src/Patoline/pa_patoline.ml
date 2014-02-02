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

let patoline_paragraph = declare_grammar ()

let parser_stack = Stack.create ()

module Extension (Syntax : Camlp4.Sig.Camlp4Syntax) =
  struct
    let patoline_caml_expr = Gram.Entry.mk "patoline_caml_expr"

    let patoline_caml_struct = Gram.Entry.mk "patoline_caml_struct"

    let patoline_caml_list = Gram.Entry.mk "patoline_caml_list"

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
          let parse = glr p:patoline_paragraph STR("|>") -> p end in
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

(* Parse a caml "expr" wrapped with parentheses *)
let wrapped_caml_expr =
  dependent_sequence (locate (glr p:STR("(") end))
    (fun (_loc,_) -> caml_expr _loc)

(* Parse a caml "str_item" wrapped with parentheses *)
let wrapped_caml_str_item =
  dependent_sequence (locate (glr p:STR("(") end))
    (fun (_loc,_) -> caml_struct _loc)

(* Parse a list of caml "expr" *)
let wrapped_caml_list =
  dependent_sequence (locate (glr p:STR("[") end))
    (fun (_loc,_) -> caml_list _loc)

(****************************************************************************
 * Grammar rules for Patoline.                                              *
 ****************************************************************************)

let section = "\\(===?=?=?=?=?=?=?\\)\\|\\(---?-?-?-?-?-?-?\\)"
let op_section = "[-=]>"
let cl_section = "[-=]<"
let word_re = "[^ \t\r\n{}\\_$|/*#]+"
(* let macro = "\\\\[^ \t\r\n({|$]+" *)
let macro = "\\\\[_a-z][_a-zA-Z0-9']*"
let ident = "[_a-z][_a-zA-Z0-9']*"

let paragraph_local, set_paragraph_local = grammar_family [ true ]

let argument =
  glr
     STR("{") l:(paragraph_local true) STR("}") -> l
  || e:wrapped_caml_expr -> e
  || e:wrapped_caml_list -> e
  end

let macro_name =
  glr
    m:RE(macro) ->
       let m = String.sub m 1 (String.length m - 1) in
       if m = "Caml" then raise Give_up;
       if m = "begin" then raise Give_up;
       if m = "end" then raise Give_up;
       if m = "item" then raise Give_up;
       if m = "verb" then raise Give_up;
       m
  end
let macro =
  glr
       m:macro_name args:argument** ->
         List.fold_left (fun acc r -> <:expr@_loc_args<$acc$ $r$>>)  <:expr@_loc_m<$lid:m$>> args
    || STR("\\verb") txt:RE("{.*}") ->
        (let txt = String.sub txt 1 (String.length txt - 2) in
        <:expr<$lid:"verb"$ [tT $str:txt$]>>)
 end

let word =
  glr
    w:RE(word_re) ->
      if String.length w >= 2 && List.mem (String.sub w 0 2) ["==";"=>";"=<";"--";"->";"-<"] then
        raise Give_up;
      w
  | w:RE("\\\\[\\$|({)}]") -> String.escaped (String.sub w 1 (String.length w - 1))
  end

let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
    let x = Loc.stop_off _loc_p1 and y = Loc.start_off _loc_p2 in
(*             Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
    let bl e = if y - x >= 1 then <:expr@_loc_p1<tT" "::$e$>> else e in
    let _loc = Loc.merge _loc_p1 _loc_p2 in
    <:expr<$p1$ @ $bl p2$>>

let rec rem_hyphen = function
  | []        -> []
  | w::[]     -> w::[]
  | w1::w2::l -> let l1 = String.length w1 in
                 if w1.[l1 - 1] = '-'
                 then let w = String.sub w1 0 (l1 - 1) ^ w2
                      in w :: rem_hyphen l
                 else w1 :: rem_hyphen (w2::l)

let paragraph_elt italic =
    glr
       m:macro -> m
    || l:word++ -> <:expr@_loc_l<[tT($str:(String.concat " " (rem_hyphen l))$)]>>
    || STR("_") p1:(paragraph_local false) _e:STR("_") when italic ->
         <:expr@_loc_p1<toggleItalic $p1$>>
    || STR("//") p1:(paragraph_local false) _e:STR("//") when italic ->
         <:expr@_loc_p1<toggleItalic $p1$>>
    || STR("**") p1:(paragraph_local false) _e:STR("**") when italic ->
         <:expr@_loc_p1<bold $p1$>>
    || STR("##") p1:(paragraph_local false) _e:STR("##") when italic ->
         <:expr@_loc_p1<verb $p1$>>
    end

let _ = set_paragraph_local (fun italic ->
  change_layout (
    glr
      l:{p:(paragraph_elt italic) -> (_loc, p)}++ -> (
        match List.rev l with
          [] -> assert false
        | m:: l->
          snd (
            List.fold_left (fun (_loc_m, m) (_loc_p, p) ->
              Loc.merge _loc_p _loc_m, concat_paragraph p _loc_p m _loc_m)
              m l))
    end)
  blank1)

let paragraph_local = paragraph_local true

let _ = set_grammar patoline_paragraph paragraph_local

let item =
  glr
    STR("\\item") ->
      (let m1 = freshUid () in
       let m2 = freshUid () in
       <:str_item< module $uid:m1$ =
                   struct
                     module $uid:m2$ = $uid:"Item"$ ;;
                     let _ = $uid:m2$.do_begin_env () ;;
                     let _ = $uid:m2$.do_end_env ()
                   end>>)
  end

let verbatim_line   = "\\(^[^#\n][^\n]*\\)\\|\\(^#?#?[^#\n][^\n]*\\)"
let string_filename = "\\\"[a-zA-Z0-9-_.]*\\\""
let uid_coloring    = "[A-Z][a-zA-Z0-9]*"

let verbatim_paragraph =
  change_layout (
    glr
      RE("^###")
      lang:glr RE("[ \t]+") id:RE(uid_coloring) -> id end?
      file:glr RE("[ \t]+") fn:RE(string_filename) -> fn end?
      RE("[ \t]*\n")
      lines:glr l:RE(verbatim_line) RE("\n") -> l end++
      RE("^###\n") ->
        (* FIXME do something with "lang" and "file" *)
        let buildline l = <:str_item<let _ = newPar D.structure ~environment:verbEnv Complete.normal
                            ragged_left (lang_default $str:l$) >>
        in
        let lines = List.map (fun l -> buildline (String.escaped l)) lines in
        assert(List.length lines <> 0);
        List.fold_left (fun acc r -> <:str_item<$acc$ $r$>>)  <:str_item<>> lines
    end
  ) no_blank

let paragraph =
  glr
    verb:verbatim_paragraph -> (fun _ -> verb)
    || l:paragraph_local ->
        (fun no_indent ->
          if no_indent then
            <:str_item@_loc_l<
               let _ = newPar D.structure ~environment:(fun x -> { x with par_indent = [] })
                         Complete.normal Patoline_Format.parameters $l$>>
          else
            <:str_item@_loc_l<
               let _ = newPar D.structure Complete.normal Patoline_Format.parameters $l$ >>)
    || it:item -> (fun _ -> it)
  end

let environment =
  glr
    o2:STR("\\begin{") idb:RE(ident) c1:STR("}") ps:paragraph** o2:STR("\\end{") ide:RE(ident) c2:STR("}") ->
      (if idb <> ide then raise Give_up;
       let lpar = List.fold_left (fun acc r -> <:str_item<$acc$ $r false$>>)  <:str_item<>> ps in
       let m1 = freshUid () in
       let m2 = freshUid () in
       <:str_item< module $uid:m1$ =
                   struct
                     module $uid:m2$ = $uid:"Env_"^idb$ ;;
                     open $uid:m2$ ;;
                     let _ = $uid:m2$.do_begin_env () ;;
                     $lpar$ ;;
                     let _ = $uid:m2$.do_end_env ()
                   end>>)
  end

let text = declare_grammar ()

let text_item =
  glr
    op:RE(section) title:paragraph_local cl:RE(section) ->
      (fun no_indent lvl ->
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
       true,l,<:str_item< $!res$ let _ = $numbered$ D.structure $title$ >>)

  || op:RE(op_section) title:paragraph_local txt:text cl:RE(cl_section) ->
      (fun no_indent lvl ->
       let numbered = match op.[0], cl.[0] with
           '=', '=' -> <:expr@_loc_op<newStruct>>
         | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
         | _ -> raise Give_up
       in
       false, lvl, <:str_item< let _ = $numbered$ D.structure $title$;; $txt true (lvl+1)$;; let _ = go_up D.structure >>)

  || STR("\\Caml") s:wrapped_caml_str_item ->
      (fun no_indent lvl -> no_indent, lvl, s)

  || e:environment -> (fun no_indent lvl -> false, lvl, e)

  || l:paragraph -> (fun no_indent lvl -> false, lvl, <:str_item<$l no_indent$>>)
  end

let _ = set_grammar text (
  glr
    l:text_item** -> (fun no_indent lvl ->
      let _,_,r = List.fold_left (fun (no_indent, lvl, ast) txt ->
        let no_indent, lvl, ast' = txt no_indent lvl in
        no_indent, lvl, <:str_item<$ast$;; $ast'$>>) (no_indent, lvl, <:str_item<>>) l
      in r)
  end)

let title =
  glr
    RE("==========\\(=*\\)") t:paragraph_local
    author:{RE("----------\\(-*\\)") t:paragraph_local}??
    institute:{RE("----------\\(-*\\)") t:paragraph_local}??
    date:{RE("----------\\(-*\\)") t:paragraph_local}??
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
    title:title?? t:text EOF ->
      match title with
        | None       -> t true 0
        | Some title -> <:str_item<$title$ $t true 0$>>
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
