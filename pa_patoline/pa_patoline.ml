open Earley_core
open Unicodelib
open Patconfig
open Patutil.Extra

open Earley
open Pa_ocaml_prelude

(* Force Pa_ocaml to be linked — it registers the OCaml grammar rules
   (expression, structure, etc.) via set_expression_lvl at module init time. *)
let () = ignore (Pa_ocaml.ghost)
open Pa_ast
open Ast_helper
open Parsetree
open Asttypes
open Longident

let _ = Printexc.record_backtrace true; Sys.catch_break true

(*
 * The patoline language is implemented as an Earley OCaml syntax
 * extension. It contains:
 *   - new OCaml expressions to allow Patoline into OCaml code,
 *   - a new entry point for plain Patoline files.
 *)

(* State information + Command line arguments extension **********************)

let patoline_format   = ref "DefaultFormat"
let patoline_driver   = ref "Pdf"
let patoline_packages = ref ["Typography"]
let patoline_grammar  = ref ["DefaultGrammar"]
let debug   = ref false
let is_main = ref false

let set_patoline_format f =
  patoline_format := f

let set_patoline_driver d =
  patoline_driver := d

let add_patoline_packages ps =
  let ps = String.split_on_char ',' ps in
  patoline_packages := !patoline_packages @ ps

let no_default_grammar = ref false

(* store grammar to use before parsing starts,
   do not use once parsing is started, use add_grammar *)
let add_patoline_grammar g =
  let g = try Filename.chop_extension g with _ -> g in
  patoline_grammar := g :: !patoline_grammar

let file = ref (None : string option)

let in_ocamldep = ref false

let build_dir = ref "."
let set_build_dir d = build_dir := d

let quail_out_name=ref "" (* filename to output quail.el shortcut for emacs *)

let quail_ch =
  Lazy.from_fun (fun () ->
    let ch = open_out_bin !quail_out_name in
    ch)

let quail_out mnames unames =
  if !quail_out_name <> "" then
    begin
      let unames, others = List.partition (fun s -> UTF8.validate s && UTF8.length s = 1) unames in

      match unames with
      | [] -> ()
      | u::_ ->
       List.iter (fun name ->
         Printf.fprintf (Lazy.force quail_ch) "(\"%s\" ?%s)\n" (String.escaped name) u) mnames;
       List.iter (fun name ->
         Printf.fprintf (Lazy.force quail_ch) "(\"%s\" ?%s)\n" (String.escaped name) u) others

    end

let extra_spec =
  [ ("--driver",  Arg.String set_patoline_driver,
     "The driver against which to compile.")
  ; ("--format",  Arg.String set_patoline_format,
     "The document format to use.")
  ; ("--package", Arg.String add_patoline_packages,
     "Package to link.")
  ; ("--no-default-grammar", Arg.Set no_default_grammar,
     "do not load DefaultGrammar")
  ; ("--grammar", Arg.String add_patoline_grammar,
     "load the given grammar file.")
  ; ("--ocamldep", (Arg.Set in_ocamldep),
    "set a flag to inform parser that we are computing dependencies")
  ; ("--quail-out", (Arg.Set_string quail_out_name),
    "set a filename to output quail.el like file for emacs short cur")
  ; ("--debug-patoline" , Arg.Set debug,
    "turn on debuging mode for pa_patoline.")
  ; ("--main"  , Arg.Set is_main,
    "generate a main file.")
  ; ("--build-dir", Arg.String set_build_dir,
    "Change the build directory.")
  ]

#define LOCATE locate

(*
 * Everything is defined at the module level, accessing grammars directly
 * from Pa_ocaml_prelude. The Extension functor mechanism from earley 2.x
 * was removed in earley 3.x.
 *)

let spec = extra_spec

(* Blank functions for Patoline *********************************************)
let blank_sline buf pos =
  let open Pa_lexing in
  let ocamldoc = ref false in
  let ocamldoc_buf = Buffer.create 1024 in
  let rec fn state stack prev curr nl =
    let (buf, pos) = curr in
    let (c, buf', pos') = Input.read buf pos in
    if !ocamldoc then Buffer.add_char ocamldoc_buf c;
    let next = (buf', pos') in
    match (state, stack, c) with
    (* Basic blancs. *)
    | (`Ini      , []  , ' '     )
    | (`Ini      , []  , '\t'    )
    | (`Ini      , []  , '\r'    ) -> fn `Ini stack curr next nl
    | (`Ini      , []  , '\n'    ) -> if not nl then curr else
                                      fn `Ini stack curr next false
    (* Comment opening. *)
    | (`Ini      , _   , '('     ) -> fn (`Opn(curr)) stack curr next nl
    | (`Ini      , []  , _       ) -> curr
    | (`Opn(p)   , _   , '*'     ) ->
        begin
          let nl = true in
          if stack = [] then
            let (c, buf', pos') = Input.read buf' pos' in
            let (c',_,_) = Input.read buf' pos' in
            if c = '*' && c' <> '*' then
              begin
                ocamldoc := true;
                fn `Ini (p::stack) curr (buf',pos') nl
              end
            else fn `Ini (p::stack) curr next nl
          else fn `Ini (p::stack) curr next nl
        end
    | (`Opn(_)   , _::_, '"'     ) -> fn (`Str(curr)) stack curr next nl (*#*)
    | (`Opn(_)   , _::_, '{'     ) -> fn (`SOp([],curr)) stack curr next nl (*#*)
    | (`Opn(_)   , []  , _       ) -> prev
    | (`Opn(_)   , _   , _       ) -> fn `Ini stack curr next nl
    (* String litteral in a comment (including the # rules). *)
    | (`Ini      , _::_, '"'     ) -> fn (`Str(curr)) stack curr next nl
    | (`Str(_)   , _::_, '"'     ) -> fn `Ini stack curr next nl
    | (`Str(p)   , _::_, '\\'    ) -> fn (`Esc(p)) stack curr next nl
    | (`Esc(p)   , _::_, _       ) -> fn (`Str(p)) stack curr next nl
    | (`Str(p)   , _::_, '\255'  ) -> unclosed_comment_string p
    | (`Str(_)   , _::_, _       ) -> fn state stack curr next nl
    | (`Str(_)   , []  , _       ) -> assert false (* Impossible. *)
    | (`Esc(_)   , []  , _       ) -> assert false (* Impossible. *)
    (* Delimited string litteral in a comment. *)
    | (`Ini      , _::_, '{'     ) -> fn (`SOp([],curr)) stack curr next nl
    | (`SOp(l,p) , _::_, 'a'..'z')
    | (`SOp(l,p) , _::_, '_'     ) -> fn (`SOp(c::l,p)) stack curr next nl
    | (`SOp(_,_) , p::_, '\255'  ) -> unclosed_comment p
    | (`SOp(l,p) , _::_, '|'     ) -> fn (`SIn(List.rev l,p)) stack curr next nl
    | (`SOp(_,_) , _::_, _       ) -> fn `Ini stack curr next nl
    | (`SIn(l,p) , _::_, '|'     ) -> fn (`SCl(l,(l,p))) stack curr next nl
    | (`SIn(_,p) , _::_, '\255'  ) -> unclosed_comment_string p
    | (`SIn(_,_) , _::_, _       ) -> fn state stack curr next nl
    | (`SCl([],_), _::_, '}'     ) -> fn `Ini stack curr next nl
    | (`SCl([],b), _::_, '\255'  ) -> unclosed_comment_string (snd b)
    | (`SCl([],b), _::_, _       ) -> fn (`SIn(b)) stack curr next nl
    | (`SCl(l,b) , _::_, c       ) -> if c = List.hd l then
                                        let l = List.tl l in
                                        fn (`SCl(l, b)) stack curr next nl
                                      else
                                        fn (`SIn(b)) stack curr next nl
    | (`SOp(_,_) , []  , _       ) -> assert false (* Impossible. *)
    | (`SIn(_,_) , []  , _       ) -> assert false (* Impossible. *)
    | (`SCl(_,_) , []  , _       ) -> assert false (* Impossible. *)
    (* Comment closing. *)
    | (`Ini      , _::_, '*'     ) -> fn `Cls stack curr next nl
    | (`Cls      , _::_, '*'     ) -> fn `Cls stack curr next nl
    | (`Cls      , p::s, ')'     ) ->
       if !ocamldoc && s = [] then
         begin
           let comment = Buffer.sub ocamldoc_buf 0 (Buffer.length ocamldoc_buf - 2) in
           Buffer.clear ocamldoc_buf;
           (* FIXME: Do we want to do the same as for OCaml, skipping comments ?*)
           let lnum = Input.line_num (fst p) in
           ocamldoc_comments := (p,next,comment,lnum)::!ocamldoc_comments;
           ocamldoc := false
         end;
       fn `Ini s curr next nl
    | (`Cls      , _::_, _       ) -> fn `Ini stack curr next nl
    | (`Cls      , []  , _       ) -> assert false (* Impossible. *)
    (* Comment contents (excluding string litterals). *)
    | (`Ini     , p::_, '\255'  ) -> unclosed_comment p
    | (`Ini     , _::_, _       ) -> fn `Ini stack curr next nl
  in
  fn `Ini [] (buf, pos) (buf, pos) true

(* Intra-paragraph blanks. *)
let blank1 = blank_sline

(* Inter-paragraph blanks. *)
let blank2 = Pa_lexing.ocaml_blank

(* Code generation helpers **************************************************)

let counter = ref 1

(* Generate a fresh module names (Uid) *)
let freshUid () =
  let current = !counter in
  incr counter;
  "MOD" ^ (string_of_int current)

let caml_structure    = change_layout structure blank2
let parser wrapped_caml_structure = '(' {caml_structure | EMPTY -> []} ')'

(* Parse a caml "expr" wrapped with parentheses *)
let caml_expr         = change_layout expression blank2
let parser wrapped_caml_expr = '(' {caml_expr  | EMPTY -> exp_unit _loc} ')'

(* Parse a list of caml "expr" *)
let parser wrapped_caml_list =
  '[' {e:expression l:{ ';' e:expression }* ';'? -> e::l}?[[]] ']'

(* Parse an array of caml "expr" *)
let wrapped_caml_array =
  parser
  | "[|" l:{e:expression l:{ ';' e:expression }* ';'? -> e::l}?[[]] "|]" -> l

(****************************************************************************
 * Words.                                                                   *
 ****************************************************************************)

let uchar =
  let char_range min max = parser c:ANY ->
    let cc = Char.code c in
    if cc < min || cc > max then give_up (); c
  in
  let tl  = char_range 128 191 in
  let hd1 = char_range 0   127 in
  let hd2 = char_range 192 223 in
  let hd3 = char_range 224 239 in
  let hd4 = char_range 240 247 in
  parser
  | c0:hd1                         -> Printf.sprintf "%c" c0
  | c0:hd2 - c1:tl                 -> Printf.sprintf "%c%c" c0 c1
  | c0:hd3 - c1:tl - c2:tl         -> Printf.sprintf "%c%c%c" c0 c1 c2
  | c0:hd4 - c1:tl - c2:tl - c3:tl -> Printf.sprintf "%c%c%c%c" c0 c1 c2 c3

let char_re    = "[^ \"\t\r\n\\#*/|_$>{}-]"
let escaped_re =     "\\\\[\\#*/|_$&>{}-]"

let non_special = ['>';'*';'/';'|';'-';'_';'<';'=';'`';'\'']
let char_alone =
  black_box
    (fun str pos ->
     let c,str',pos' = Input.read str pos in
     if List.mem c non_special then
       let c',_,_ = Input.read str' pos' in
       if c = c' || ((c = '-' || c = '=') && (c' = '>' || c' = '<')) then give_up ()
       else c, str', pos'
     else
       give_up ())
    (List.fold_left Charset.add Charset.empty non_special) false
    (String.concat " | " (List.map (fun c -> String.make 1 c) non_special))

(* FIXME maybe remove the double quote? *)
let special_char =
  [ ' ' ; '"'; '\t' ; '\r' ; '\n' ; '\\' ; '#' ; '*' ; '/' ; '|' ; '_'
  ; '$' ; '<'; '>' ; '{'; '}'; '-'; '='; '&' ; '`' ; '\'']

let no_spe =
  let f buf pos =
    let (c,_,_) = Input.read buf pos in
    ((), not (List.mem c special_char))
  in
  Earley.test ~name:"no_special" Charset.full f

let parser character =
  | _:no_spe c:uchar -> c
  | s:RE(escaped_re) -> let open String in escaped (sub s 1 (length s - 1))
  | c:char_alone     -> String.make 1 c

let word =
  change_layout (parser cs:character+ -> String.concat "" cs) no_blank

let rec rem_hyphen = function
  | []        -> []
  | w::[]     -> w::[]
  | w1::w2::l -> let l1 = String.length w1 in
                 if w1.[l1 - 1] = '-'
                 then let w = String.sub w1 0 (l1 - 1) ^ w2
                      in rem_hyphen (w :: l)
                 else w1 :: rem_hyphen (w2::l)

(****************************************************************************
 * Verbatim environment / macro                                             *
 ****************************************************************************)

let parser verbatim_line = ''\(^#?#?\([^#\t\n][^\t\n]*\)?\)''
let parser mode_ident = ''[a-zA-Z0-9_']+''
let parser filename = ''[a-zA-Z0-9-_./]*''

let parser verbatim_environment =
  ''^###''
  mode:{_:''[ \t]+'' mode_ident}?
  file:{_:''[ \t]+'' "\"" filename "\""}?
  _:''[ \t]*'' "\r"? "\n"
  lines:{l:verbatim_line "\r"? "\n" -> l^"\n"}+
  ''^###'' ->
    if lines = [] then give_up ();

    (* Uniformly remove head spaces. *)
    let nb_hspaces l =
      let len = String.length l in
      let nb    = ref 0 in
      let found = ref false in
      while !nb < len && not !found do
        if l.[!nb] = ' ' then incr nb
        else found := true
      done;
      if !found then !nb else max_int
    in
    let f m l = min m (nb_hspaces l) in
    let minhsp = List.fold_left f max_int lines in
    let remhsp l =
      let len = String.length l in
      if len <= minhsp then ""
      else String.sub l minhsp (len - minhsp)
    in
    let lines =
      let f s tl = exp_cons _loc (exp_string _loc s) tl in
      List.fold_right f (List.map remhsp lines) (exp_nil _loc)
    in
    let mode = "verbs_" ^ match mode with None -> "default" | Some m -> m in
    let file =
      match file with
      | None -> exp_none _loc
      | Some f -> exp_some _loc (exp_string _loc f)
    in
    str_let_wild _loc (exp_apply2 _loc (exp_lid _loc mode) file lines)

let verbatim_environment = change_layout verbatim_environment no_blank

let verbatim_generic st forbid nd =
  let line_re = "[^\n" ^ forbid ^ "]+" in
  change_layout (
      parser
        STR(st)
        ls:{l:RE(line_re) '\r'? '\n' -> l}*
        l:RE(line_re)
            STR(nd) ->
          let lines = ls @ [l] in
          let lines = rem_hyphen lines in
          let txt = String.concat " " lines in
          exp_apply1 _loc (exp_lid _loc "verbatim") (exp_string _loc txt)
    ) no_blank

let verbatim_macro = verbatim_generic "\\verb{" "{}" "}"
let verbatim_sharp = verbatim_generic "##" "#" "##"
let verbatim_bquote = verbatim_generic "``" "`" "``"

(*************************************************************
 *   Type to control which t2t like tags are forbidden       *
 *************************************************************)
type tag_syntax =
  Italic | Bold | SmallCap | Underline | Strike | Quote

module Tag_syntax = struct
  type t = tag_syntax
  let compare = compare
end

module TagSet = Set.Make(Tag_syntax)

let addTag = TagSet.add
let allowed t f = not (TagSet.mem t f)

(****************************************************************************
 * Symbol definitions.                                                      *
 ****************************************************************************)

type math_prio =
  | AtomM | Accent | LInd | Ind | IApply | IProd | Prod
  | Sum | Operator | Rel | Neg | Conj | Impl | Punc

let math_prios = [ AtomM ; Accent ; LInd ; Ind ; IProd ; Prod ; Sum
                 ; Operator ; Rel ; Neg ; Conj ; Impl ; Punc ]

let next_prio = function
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

let parser symbol =
  | s:"\\}"             -> s
  | s:"\\{"             -> s
  | s:''[^ \t\r\n{}]+'' -> s

let symbols =
  let space_blank = Earley_str.blank_regexp ''[ ]*'' in
  change_layout (
    parser
    | "{" ss:symbol* "}" -> ss
  ) space_blank

let parser symbol_value =
  | "{" s:symbol "}"    -> SimpleSym s
  | e:wrapped_caml_expr -> CamlSym e

let parser symbol_values =
  | e:wrapped_caml_expr -> e

let parser lid = id:''[_a-z][_a-zA-Z0-9']*'' -> id
let parser uid = id:''[A-Z][_a-zA-Z0-9']*''  -> id
let parser num = n:''[0-9]+'' -> int_of_string n
let parser int = n:''-?[0-9]+'' -> int_of_string n
let parser float = n:''-?[0-9]+\(.[0-9]*\)?\([eE]-?[0-9]+\)?'' -> n

let symbol ss =
  List.partition (fun s ->
    assert (String.length s > 0);
    s.[0] = '\\') ss

(* FIXME: more entry are possible : paragraphs, ...*)
type entry = Caml | CamlStruct | String | Math | MathLine | MathMatrix
             | Text | TextLine | TextMatrix | Current | Int | Float

type arg_config =
  { entry : entry;
    filter_name : string;
  }

let default_config =
  { entry = Current;
    filter_name = "";
  }

type config = EatR | EatL | Name of string list * string | Syntax of arg_config list


let real_name _loc id cs =
  let rec find_name : config list -> string list * string = function
    | []               -> raise Not_found
    | Name(mp,id) :: _ -> (mp,id)
    | _ :: cs          -> find_name cs
  in
  let (mp, mid) = try find_name cs with Not_found -> ([], id) in
  (* FIXME: this is not exactly what we want *)
  List.fold_left (fun acc m -> exp_open _loc m acc) (exp_lid _loc mid) mp

let macro_args cs =
  let rec find_args : config list -> arg_config list option = function
    | []               -> None
    | Syntax l  :: _   -> Some l
    | _ :: cs          -> find_args cs
  in
  find_args cs

let parser arg_type =
  | s:''[a-zA-Z0-9_]+'' ->
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
      | "int"     -> Int
      | "float"   -> Float
      | "string"  -> String
      | _ -> give_up ()

let parser arg_description =
  | entry:arg_type filter_name:lid?[""] -> { entry; filter_name }

let parser arg_descriptions =
  | EMPTY -> []
  | a:arg_description l:{ _:"," arg_description }* -> a::l

let expr_filter : (string, Parsetree.expression -> Location.t -> Parsetree.expression) Hashtbl.t =
  Hashtbl.create 31

let struct_filter : (string, Parsetree.structure -> Location.t -> Parsetree.expression) Hashtbl.t =
  Hashtbl.create 31

let string_filter : (string, string -> Location.t -> Parsetree.expression) Hashtbl.t =
  Hashtbl.create 31

let apply_string_filter config s _loc =
  try Hashtbl.find string_filter config.filter_name s _loc with Not_found ->
    Printf.eprintf "Unknown string filter: %S (%a)\n%!"
      config.filter_name Pa_ocaml_prelude.print_location _loc; exit 1

let apply_expr_filter config s _loc =
  try Hashtbl.find expr_filter config.filter_name s _loc with Not_found ->
    Printf.eprintf "Unknown expression filter: %S (%a)\n%!"
      config.filter_name Pa_ocaml_prelude.print_location _loc; exit 1

let apply_struct_filter config s _loc =
  try Hashtbl.find struct_filter config.filter_name s _loc with Not_found ->
    Printf.eprintf "Unknown structure filter: %S (%a)\n%!"
      config.filter_name Pa_ocaml_prelude.print_location _loc; exit 1

let _ =
  Hashtbl.add string_filter "" (fun s _loc -> exp_string _loc s)

let _ =
  Hashtbl.add expr_filter "" (fun e _loc -> e)

let _ =
  Hashtbl.add struct_filter "" (fun s _loc ->
    exp_list _loc [exp_apply1 _loc (exp_lid _loc "bB")
      (exp_fun_s _loc "env"
        (exp_let_mod _loc "Res"
          (mod_structure _loc s)
          (exp_list _loc [exp_apply1 _loc (exp_lid _loc "Drawing")
            (exp_apply _loc (exp_dot _loc "Res" "drawing") [exp_unit _loc])])))])

let _ =
  Hashtbl.add string_filter "genumerate" (fun s _loc ->
      let pos =
        let len = String.length s in
        let rec find i =
          if i >= len - 1 then raise Not_found
          else if s.[i] = '&' && (let c = s.[i+1] in c = '1' || c = 'i' || c = 'I' || c = 'a' || c = 'A')
          then i
          else find (i+1)
        in find 0
      in
    (* let c = String.make 1 s.[pos+1] in *)
      let c = s.[pos+1] in
    let prefix = String.sub s 0 pos in
    let suffix = String.sub s (pos+2) (String.length s - pos - 2) in
    let nb_kind = begin
      match c with
      | '1' -> "Arabic"
      | 'i' -> "RomanLower"
      | 'I' -> "RomanUpper"
      | 'a' -> "AlphaLower"
      | 'A' -> "AlphaUpper"
      | _ ->   (Printf.eprintf "Invalid argument to genumerate: %c. Falling back to arabic.\n" c ;
                flush stderr ; "Arabic")
    end in
    let caml = "("^nb_kind^",(fun num_sec -> <<" ^ prefix ^ "\\caml( [tT num_sec] )" ^ suffix ^ ">>))" in
    Earley.parse_string Pa_ocaml_prelude.expression blank2 caml)

let _ =
  Hashtbl.add struct_filter "diagram" (fun s _loc ->
    let diagram_mod =
      str_module _loc "Diagram"
        (mod_apply _loc (mod_ident _loc (Lident "MakeDiagram"))
          (mod_structure _loc
            (str_let _loc "env" (exp_lid _loc "env")))) in
    let open_diagram = str_open _loc "Diagram" in
    let body_struct = diagram_mod @ open_diagram @ s in
    exp_list _loc [exp_apply1 _loc (exp_lid _loc "bB")
      (exp_fun_s _loc "env"
        (exp_let_mod _loc "Res"
          (mod_structure _loc body_struct)
          (exp_list _loc [exp_apply1 _loc (exp_lid _loc "Drawing")
            (exp_apply _loc (exp_dot2 _loc "Res" "Diagram" "make") [exp_unit _loc])])))])


let parser config =
  | "eat_right"                           -> EatR
  | "eat_left"                            -> EatL
  | "name" "=" ms:{u:uid - "." -}* id:lid -> Name (ms,id)
  | "syntax" "=" l:arg_descriptions       -> Syntax l

let parser configs = "{" cs:{config ';'} * "}"   -> cs

(****************************************************************************
 * Maths datum                                                              *
 ****************************************************************************)

type 'a indices = { up_right : 'a option; up_right_same_script: bool;
                      down_right : 'a option; up_left_same_script: bool;
                      up_left : 'a option;
                      down_left : 'a option }

let no_ind = { up_right = None; up_left = None; down_right = None; down_left = None;
                 up_right_same_script = false; up_left_same_script = false }

type infix =
  { infix_prio : math_prio;
    infix_utf8_names : string list;
    infix_macro_names : string list; (* with backslash *)
    infix_value : symbol;
    infix_space : int;
    infix_no_left_space : bool;
    infix_no_right_space : bool;
  }

let invisible_product =
  { infix_prio = Prod;
    infix_utf8_names = [];
    infix_macro_names = [];
    infix_value = Invisible;
    infix_space = 3;
    infix_no_left_space = false;
    infix_no_right_space = false;
  }

let invisible_apply =
  { infix_prio = Prod;
    infix_utf8_names = [];
    infix_macro_names = [];
    infix_value = Invisible;
    infix_space = 5;
    infix_no_left_space = false;
    infix_no_right_space = false;
  }

type prefix =
  { prefix_prio : math_prio;
    prefix_utf8_names : string list;
    prefix_macro_names : string list; (* with backslash *)
    prefix_value : symbol;
    prefix_space : int;
    prefix_no_space : bool;
  }

type postfix =
  { postfix_prio : math_prio;
    postfix_utf8_names : string list;
    postfix_macro_names : string list; (* with backslash *)
    postfix_value : symbol;
    postfix_space : int;
    postfix_no_space : bool;
  }

type atom_symbol =
  { symbol_utf8_names : string list;
    symbol_macro_names : string list; (* with backslash *)
    symbol_value : symbol;
  }

type operator_kind =
  | Limits
  | NoLimits

type operator =
  { operator_prio : math_prio;
    operator_utf8_names : string list;
    operator_macro_names : string list; (* with backslash *)
    operator_values : Parsetree.expression;
    operator_kind : operator_kind;
  }

type delimiter =
  { delimiter_utf8_names : string list;
    delimiter_macro_names : string list; (* with backslash *)
    delimiter_values : Parsetree.expression;
  }

let invisible_delimiter = {
  delimiter_utf8_names  = [];
  delimiter_macro_names = [];
  delimiter_values      = let _loc = Location.none in exp_nil _loc;
}

module PMap = PrefixTree

type grammar_state =
  { mutable verbose          : bool
  ; mutable infix_symbols    : infix PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable prefix_symbols   : prefix PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable postfix_symbols  : postfix PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable quantifier_symbols : atom_symbol PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable atom_symbols     : atom_symbol PrefixTree.t
  ; mutable any_symbols      : atom_symbol PrefixTree.t
  ; mutable accent_symbols   : atom_symbol PrefixTree.t
  ; mutable left_delimiter_symbols: delimiter PrefixTree.t
  ; mutable right_delimiter_symbols: delimiter PrefixTree.t
  ; mutable operator_symbols : operator PrefixTree.t
  ; mutable combining_symbols: string PrefixTree.t
  ; mutable reserved_symbols: unit PrefixTree.t
  ; mutable word_macros      : (string * config list) list
  ; mutable math_macros      : (string * config list) list
  ; mutable environment      : (string * config list) list }

let state =
  { verbose          = false
  ; infix_symbols    = PrefixTree.empty
  ; prefix_symbols    = PrefixTree.empty
  ; postfix_symbols    = PrefixTree.empty
  ; quantifier_symbols    = PrefixTree.empty
  ; atom_symbols     = PrefixTree.empty
  ; any_symbols     = PrefixTree.empty
  ; accent_symbols     = PrefixTree.empty
  ; left_delimiter_symbols= PrefixTree.empty
  ; right_delimiter_symbols= PrefixTree.empty
  ; operator_symbols= PrefixTree.empty
  ; combining_symbols= PrefixTree.empty
  ; reserved_symbols = PrefixTree.add "left" () (PrefixTree.add "right" () PrefixTree.empty)
  ; word_macros      = []
  ; math_macros      = []
  ; environment      = [] }

let parser mathlid = id:''[a-z][a-zA-Z0-9']*'' ->
  if PMap.mem id state.reserved_symbols then give_up ();
  id

let reserved_uid = [ "Include" ; "Caml"; "Configure_math_macro"
                   ; "Configure_word_macro"; "Configure_environment"
                   ; "Verbose_Changes"; "Save_Grammar"
                   ; "Add_relation" ; "Add_addition_like"
                   ; "Add_product_like" ; "Add_connector"
                   ; "Add_arrow" ; "Add_punctuation"
                   ; "Add_quantifier" ; "Add_prefix"
                   ; "Add_postfix" ; "Add_accent"
                   ; "Add_symbol" ; "Add_operator"
                   ; "Add_limits_operator" ; "Add_left"
                   ; "Add_right" ; "Add_combining"
                   ]
let parser macrouid = id:{uid | "item" -> "Item" } ->
  if List.mem id reserved_uid then give_up ();
  id

let empty_state =
  { verbose          = false
  ; infix_symbols    = PrefixTree.empty
  ; prefix_symbols    = PrefixTree.empty
  ; postfix_symbols    = PrefixTree.empty
  ; quantifier_symbols    = PrefixTree.empty
  ; atom_symbols     = PrefixTree.empty
  ; any_symbols     = PrefixTree.empty
  ; accent_symbols     = PrefixTree.empty
  ; left_delimiter_symbols= PrefixTree.empty
  ; right_delimiter_symbols= PrefixTree.empty
  ; operator_symbols= PrefixTree.empty
  ; combining_symbols= PrefixTree.empty
  ; reserved_symbols = PrefixTree.empty
  ; word_macros      = []
  ; math_macros      = []
  ; environment      = [] }

let local_state =
  { verbose          = false
  ; infix_symbols    = PrefixTree.empty
  ; prefix_symbols    = PrefixTree.empty
  ; postfix_symbols    = PrefixTree.empty
  ; quantifier_symbols    = PrefixTree.empty
  ; atom_symbols     = PrefixTree.empty
  ; any_symbols     = PrefixTree.empty
  ; accent_symbols     = PrefixTree.empty
  ; left_delimiter_symbols= PrefixTree.empty
  ; right_delimiter_symbols= PrefixTree.empty
  ; operator_symbols= PrefixTree.empty
  ; combining_symbols= PrefixTree.empty
  ; reserved_symbols = PrefixTree.empty
  ; word_macros      = []
  ; math_macros      = []
  ; environment      = [] }

let merge_states : grammar_state -> grammar_state -> unit = fun s1 s2 ->
  s1.verbose                 <- s1.verbose || s2.verbose;
  s1.infix_symbols           <- PrefixTree.union s1.infix_symbols s2.infix_symbols;
  s1.prefix_symbols          <- PrefixTree.union s1.prefix_symbols s2.prefix_symbols;
  s1.postfix_symbols         <- PrefixTree.union s1.postfix_symbols s2.postfix_symbols;
  s1.quantifier_symbols      <- PrefixTree.union s1.quantifier_symbols s2.quantifier_symbols;
  s1.atom_symbols            <- PrefixTree.union s1.atom_symbols s2.atom_symbols;
  s1.any_symbols             <- PrefixTree.union s1.any_symbols s2.any_symbols;
  s1.accent_symbols          <- PrefixTree.union s1.accent_symbols s2.accent_symbols;
  s1.left_delimiter_symbols  <- PrefixTree.union s1.left_delimiter_symbols s2.left_delimiter_symbols;
  s1.right_delimiter_symbols <- PrefixTree.union s1.right_delimiter_symbols s2.right_delimiter_symbols;
  s1.operator_symbols        <- PrefixTree.union s1.operator_symbols s2.operator_symbols;
  s1.combining_symbols       <- PrefixTree.union s1.combining_symbols s2.combining_symbols;
  s1.reserved_symbols        <- PrefixTree.union s1.reserved_symbols s2.reserved_symbols;
  s1.word_macros             <- s2.word_macros @ s1.word_macros;
  s1.math_macros             <- s2.math_macros @ s1.math_macros;
  s1.environment             <- s2.environment @ s1.environment

let math_prefix_symbol, set_math_prefix_symbol  = grammar_family "prefix"
let math_postfix_symbol, set_math_postfix_symbol = grammar_family "postfix"
let math_operator, set_math_operator = grammar_family "operator"
let math_infix_symbol, set_math_infix_symbol = grammar_family "infix"
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

let tree_to_grammar : ?filter:('a -> bool) -> string -> 'a PMap.tree -> 'a grammar =
  fun ?(filter=fun _ -> true) name t ->
    let PMap.Node(_,l) = t in
    let fn buf pos =
      let line = Input.line buf in
      let line = String.sub line pos (String.length line - pos) in
      try
        let (n,v) = PMap.longest_prefix ~filter line t in
        (* Printf.eprintf "Symbol found : [%s]\n%!" (String.sub line 0 n); *)
        if n > 1 && line.[0] = '\\' then
          begin
            let is_macro = ref true in
            for i = 1 to n-1 do
              is_macro := !is_macro && is_macro_char line.[i]
            done;
            let len = String.length line in
            if !is_macro && len >= n && is_macro_char line.[n] then give_up ()
          end;
        (v, buf, pos+n)
      with Not_found -> give_up ()
    in
    let charset =
      let f acc (c,_) = Charset.add acc c in
      List.fold_left f Charset.empty l
    in
    black_box fn charset false name


let build_grammar () =
  set_math_infix_symbol (fun p ->
    tree_to_grammar ~filter:(fun s -> s.infix_prio = p) "infix_symbol" state.infix_symbols);
  set_grammar math_punctuation_symbol
    (tree_to_grammar ~filter:(fun s -> s.infix_prio = Punc) "punctuation_symbol" state.infix_symbols);
  set_grammar math_relation_symbol
    (tree_to_grammar ~filter:(fun s -> s.infix_prio = Rel) "relation_symbol" state.infix_symbols);
  set_math_prefix_symbol
    (fun p -> tree_to_grammar ~filter:(fun s -> s.prefix_prio = p) "prefix_symbol" state.prefix_symbols);
  set_math_postfix_symbol
    (fun p -> tree_to_grammar ~filter:(fun s -> s.postfix_prio = p) "postfix_symbol" state.postfix_symbols);
  set_math_operator (fun p ->
    tree_to_grammar ~filter:(fun s -> s.operator_prio = p) "operator_symbol" state.operator_symbols);
  set_grammar math_quantifier_symbol (tree_to_grammar "quantifier_symbol" state.quantifier_symbols);
  set_grammar math_atom_symbol (tree_to_grammar "atom_symbol" state.atom_symbols);
  set_grammar math_any_symbol (tree_to_grammar "any_symbol" state.any_symbols);
  set_grammar math_accent_symbol (tree_to_grammar "accent_symbol" state.accent_symbols);
  set_grammar math_left_delimiter (tree_to_grammar "left_delimiter_symbol" state.left_delimiter_symbols);
  set_grammar math_right_delimiter (tree_to_grammar "right_delimiter_symbol" state.right_delimiter_symbols);
  set_grammar math_combining_symbol (tree_to_grammar "combining_symbol" state.combining_symbols)

(* add grammar now, but not build yet *)
let add_grammar g =
  let open Config in
  let (gpath, gpaths) = patoconfig.grammars_dir in
  let path = "." :: ".patobuild" :: gpath :: gpaths in
  if !no_default_grammar && g = "DefaultGrammar" then () else
    begin
      let g =
        try Filename.find_file (g ^ ".tgy") path with Not_found ->
          (*
          Printf.eprintf "Cannot find [%s.tgy] in the folders:\n%!" g;
          List.iter (Printf.eprintf " - [%s]\n%!") path;
          Printf.eprintf "(in directory [%s])\n" (Sys.getcwd ());
          *)
          raise Not_found
      in
      let ch = open_in_bin g in
      let st = input_value ch in
      merge_states state st;
      close_in ch
    end

let parser all_left_delimiter =
  | math_left_delimiter
  | _:"\\left" math_right_delimiter
  | "\\left." -> invisible_delimiter

let parser all_right_delimiter =
  | math_right_delimiter
  | _:"\\right" math_left_delimiter
  | "\\right." -> invisible_delimiter

let symbol_paragraph _loc syms names =
  (* let _ = D.structure := newPar !D.structure ~environment:(...) Complete.normal Patoline_Format.parameters [bB ...] *)
  let env_fun = exp_fun_s _loc "x"
    (exp_record _loc
      [(mkloc (Lident "par_indent") _loc, exp_nil _loc)]
      (Some (exp_lid _loc "x"))) in
  let kdraw_arg = exp_list _loc [
    exp_record _loc
      [(mkloc (Lident "mathStyle") _loc, exp_dot _loc "Mathematical" "Display")]
      (Some (exp_lid _loc "env0"))] in
  let bin_node = exp_apply _loc (maths_lid _loc "bin")
    [exp_int _loc 0;
     maths_normal _loc false (maths_node _loc (maths_glyphs _loc "⇐")) false;
     syms; names] in
  let kdraw_body = exp_apply _loc (exp_dot _loc "Maths" "kdraw") [kdraw_arg; exp_list _loc [bin_node]] in
  let bb_expr = exp_list _loc [exp_apply1 _loc (exp_lid _loc "bB") (exp_fun_s _loc "env0" kdraw_body)] in
  let newpar = Exp.apply ~loc:_loc (exp_lid _loc "newPar")
    [(Labelled "environment", env_fun);
     (Nolabel, exp_deref _loc (exp_dot _loc "D" "structure"));
     (Nolabel, exp_dot _loc "Complete" "normal");
     (Nolabel, exp_dot _loc "Patoline_Format" "parameters");
     (Nolabel, bb_expr)] in
  let assign = exp_ref_assign _loc (exp_dot _loc "D" "structure") newpar in
  str_let_wild _loc assign

let math_list _loc l =
  let merge x y =
    maths_bin _loc 0 (maths_normal _loc false (maths_node _loc (maths_glyphs _loc ",")) false) x y
  in
  List.fold_left merge (List.hd l) (List.tl l)

let dollar        = Pa_lexing.single_char '$'

let no_brace =
  Earley.test ~name:"no_brace" Charset.full (fun buf pos ->
    if Input.get buf pos <> '{' then ((), true) else ((), false))

(****************************************************************************
 * Parsing of macro arguments.                                              *
 ****************************************************************************)

let parser br_string =
  | EMPTY -> ""
  | s1:br_string s2:''[^{}]+'' -> s1^s2
  | s1:br_string '{' s2:br_string '}' -> s1 ^ "{" ^ s2 ^ "}"
  | s1:br_string '\n' -> s1 ^ "\n"

(* bool param -> can contain special text environments //...// **...** ... *)
let paragraph_basic_text, set_paragraph_basic_text = grammar_family "paragraph_basic_text"
let math_toplevel = declare_grammar "math_toplevel"

let nil = let _loc = Location.none in exp_nil _loc
let math_line = parser
  | m:math_toplevel?[nil] ls:{ _:'&' l:math_toplevel?[nil]}*
      -> exp_cons _loc m (exp_list _loc ls)

let math_matrix = parser
  | EMPTY -> exp_nil _loc
  | l:math_line ls:{ "\\\\" m:math_line }* -> exp_cons _loc l (exp_list _loc ls)

let simple_text = change_layout (paragraph_basic_text TagSet.empty) ~new_blank_after:false blank1

let text_line = parser
  | m:simple_text?[nil] ls:{ _:'&' l:simple_text?[nil]}*
      -> exp_cons _loc m (exp_list _loc ls)

let text_matrix = parser
  | EMPTY -> exp_nil _loc
  | l:text_line ls:{ "\\\\" m:text_line }* -> exp_cons _loc l (exp_list _loc ls)

let parser macro_argument config =
  | '{' m:math_toplevel '}'
      when config.entry = Math
      -> apply_expr_filter config m _loc
  | '{' m:math_line '}'
      when config.entry = MathLine
      -> apply_expr_filter config m _loc
  | '{' m:math_matrix '}'
      when config.entry = MathMatrix
      -> apply_expr_filter config m _loc
  | '{' e:simple_text '}'
      when config.entry = Text
      -> apply_expr_filter config e _loc
  | '{' e:text_line '}'
      when config.entry = TextLine
      -> apply_expr_filter config e _loc
  | '{' e:text_matrix '}'
      when config.entry = TextMatrix
      -> apply_expr_filter config e _loc
  | '{' e:int '}'
      when config.entry = Int
      -> apply_expr_filter config (exp_int _loc e) _loc
  | '{' e:float '}'
      when config.entry = Float
      -> apply_expr_filter config (exp_float _loc e) _loc
  | e:wrapped_caml_expr
      when config.entry <> CamlStruct
      -> apply_expr_filter config e _loc
  | e:wrapped_caml_array
      when config.entry <> CamlStruct
      -> apply_expr_filter config (exp_array _loc e) _loc
  | e:wrapped_caml_list
      when config.entry <> CamlStruct
      -> apply_expr_filter config (exp_list _loc e) _loc
  | s:wrapped_caml_structure
      when config.entry = CamlStruct
      -> apply_struct_filter config s _loc
  | '{' e:caml_expr '}'
      when config.entry = Caml
      -> apply_expr_filter config e _loc
  | '{' s:caml_structure '}'
      when config.entry = CamlStruct
      -> apply_struct_filter config s _loc
  | '{' s:(change_layout br_string no_blank) '}'
      when config.entry = String
      -> apply_string_filter config s _loc

let parser simple_text_macro_argument =
  | '{' l:simple_text?$ '}' ->
      (match l with Some l -> l | None -> exp_nil _loc)
  | e:wrapped_caml_expr  -> e
  | e:wrapped_caml_array -> exp_array _loc e
  | e:wrapped_caml_list  -> exp_list _loc e

let parser simple_math_macro_argument =
  | '{' m:(change_layout math_toplevel blank2) '}' -> m
  | e:wrapped_caml_expr  -> e
  | e:wrapped_caml_array -> exp_array _loc e
  | e:wrapped_caml_list  -> exp_list _loc e

let parser macro_arguments_aux l =
  | EMPTY when l = [] -> []
  | arg:(macro_argument (List.hd l)) args:(macro_arguments_aux (List.tl l)) when l <> [] -> arg::args

let macro_arguments current config =
  match macro_args config, current with
  | None, Math   -> (parser simple_math_macro_argument*$)
  | None, Text   -> (parser simple_text_macro_argument*$)
  | None, _      -> assert false
  | Some l, _    ->
     let l = List.map (fun s -> if s.entry = Current then { s with entry = current } else s) l in
     macro_arguments_aux l


let hash_sym = Hashtbl.create 1001
let count_sym = ref 0
let hash_msym = Hashtbl.create 1001
let count_msym = ref 0

let mcache_buf = ref []
let cache = ref ""
let cache_buf = ref []

let print_math_symbol _loc sym=
  let s,b =
    match sym with
      SimpleSym s -> maths_glyphs _loc s, false
    | CamlSym s   -> s, false
    | MultiSym s  -> s, true
    | Invisible   -> maths_glyphs _loc "invisible", false
  in
  if b then
    if !cache = "" then s else (* FIXME: not very clean *)
    try
      let nom = "m" ^ (!cache) in
      let index = Hashtbl.find hash_msym s in
      exp_array_get _loc (exp_lid _loc nom) (exp_int _loc index)
    with Not_found ->
      Hashtbl.add  hash_msym s !count_msym;
      mcache_buf := s::!mcache_buf;
      let res = exp_array_get _loc (exp_lid _loc ("m" ^ !cache)) (exp_int _loc !count_msym) in
      let _ = incr count_msym in
      res
  else
    if !cache = "" then s else (* FIXME: not very clean *)
    try
      let r = Hashtbl.find hash_sym s in
      exp_array_get _loc (exp_lid _loc !cache) (exp_int _loc r)
    with Not_found ->
      Hashtbl.add  hash_sym s !count_sym;
      cache_buf := s::!cache_buf;
      let res = exp_array_get _loc (exp_lid _loc !cache) (exp_int _loc !count_sym) in
      let _ = incr count_sym in
      res

let print_ordinary_math_symbol _loc sym =
  maths_ordinary_node _loc (print_math_symbol _loc sym)

let print_math_deco_sym _loc elt ind =
  if ind = no_ind then (
    maths_node _loc (print_math_symbol _loc elt)
  ) else
    begin
      let r = ref [] in
      (match ind.up_right with
        Some i ->
               if ind.up_right_same_script then
            r:= [maths_record_field _loc "super_right_same_script" (exp_bool _loc true)] @ !r;
          r:= [maths_record_field _loc "superscript_right" i] @ !r
      | _ -> ());
      (match ind.down_right with
        Some i ->
          r:= [maths_record_field _loc "subscript_right" i] @ !r
      | _ -> ());
      (match ind.up_left with
        Some i ->
               if ind.up_left_same_script then
            r:= [maths_record_field _loc "super_left_same_script" (exp_bool _loc true)] @ !r;
          r:= [maths_record_field _loc "superscript_left" i] @ !r
      | _ -> ());
      (match ind.down_left with
        Some i ->
          r:= [maths_record_field _loc "subscript_left" i] @ !r
      | _ -> ());
      loc_expr _loc (Pexp_record (!r, Some (maths_node _loc (print_math_symbol _loc elt))))
    end

let print_math_deco _loc elt ind =
  if ind = no_ind then
    elt
  else
    begin
      let r = ref [] in
      (match ind.up_right with
       | Some i ->
                  if ind.up_right_same_script then
                   r:= [maths_record_field _loc "super_right_same_script" (exp_bool _loc true)] @ !r;
                 r := [maths_record_field _loc "superscript_right" i] @ !r
       | _ -> ());
      (match ind.down_right with
       | Some i ->
           r:= [maths_record_field _loc "subscript_right" i] @ !r
       | _ -> ());
      (match ind.up_left with
       | Some i ->
           if ind.up_left_same_script then
                   r:= [maths_record_field _loc "super_left_same_script" (exp_bool _loc true)] @ !r;
           r:= [maths_record_field _loc "superscript_left" i] @ !r
       | _ -> ());
      (match ind.down_left with
       | Some i -> r:= [maths_record_field _loc "subscript_left" i] @ !r
       | _ -> ());
      let draw_fun = exp_fun_s _loc "env" (exp_fun_s _loc "st"
        (exp_apply2 _loc (exp_dot _loc "Maths" "draw")
          (exp_list _loc [exp_lid _loc "env"]) elt)) in
      let node_expr = exp_apply1 _loc (maths_lid _loc "node") draw_fun in
      let record_expr = loc_expr _loc (Pexp_record (!r, Some node_expr)) in
      exp_list _loc [maths_construct _loc "Ordinary" (Some record_expr)]
    end

let add_reserved sym_names =
  let insert map name =
    let len = String.length name in
    if len > 0 && name.[0] = '\\' then
      let name = String.sub name 1 (len - 1) in
      PrefixTree.add name () map
    else map
  in
  state.reserved_symbols <- List.fold_left insert state.reserved_symbols sym_names;
  local_state.reserved_symbols <- List.fold_left insert local_state.reserved_symbols sym_names

let new_infix_symbol _loc infix_prio sym_names infix_value =
  let infix_macro_names, infix_utf8_names = symbol sym_names in
  let infix_no_left_space = (infix_prio = Punc) in
  let infix_no_right_space =  false in
  let infix_space = match infix_prio with
    | Sum -> 2
    | Prod -> 3
    | Rel | Punc -> 1
    | Conj | Impl -> 0
    | _ -> assert false
  in
  let sym =
    { infix_prio; infix_macro_names; infix_utf8_names; infix_value
    ; infix_space; infix_no_left_space; infix_no_right_space }
  in
  quail_out infix_macro_names infix_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.infix_symbols <- List.fold_left insert state.infix_symbols sym_names;
  local_state.infix_symbols <- List.fold_left insert local_state.infix_symbols sym_names;
  let asym = { symbol_macro_names = infix_macro_names;
               symbol_utf8_names = infix_utf8_names;
               symbol_value = infix_value } in
  let insert map name = PrefixTree.add name asym map in
  state.any_symbols <- List.fold_left insert state.any_symbols sym_names;
  local_state.any_symbols <- List.fold_left insert local_state.any_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym = print_ordinary_math_symbol _loc infix_value in
    let showuname _ =
      sym (* TODO *)
      (*
        let s = maths_glyphs _loc s in
        print_ordinary_math_symbol _loc (CamlSym s)
       *)
    in
    let showmname m =
      print_ordinary_math_symbol _loc (SimpleSym m)
    in
    let unames = List.map showuname infix_utf8_names in
    let mnames = List.map showmname infix_macro_names in
    symbol_paragraph _loc sym (math_list _loc (unames @ mnames))
  else []

let new_symbol _loc sym_names symbol_value =
  let symbol_macro_names, symbol_utf8_names = symbol sym_names in
  let sym = { symbol_macro_names; symbol_utf8_names; symbol_value } in
  quail_out symbol_macro_names symbol_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.atom_symbols <- List.fold_left insert state.atom_symbols sym_names;
  local_state.atom_symbols <- List.fold_left insert local_state.atom_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = print_ordinary_math_symbol _loc symbol_value in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym symbol_macro_names @ List.map (fun _ -> sym_val) symbol_utf8_names in
    symbol_paragraph _loc sym_val (math_list _loc names)
  else []

let new_accent_symbol _loc sym_names symbol_value =
  let symbol_macro_names, symbol_utf8_names = symbol sym_names in
  let sym = { symbol_macro_names; symbol_utf8_names; symbol_value } in
  quail_out symbol_macro_names symbol_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.accent_symbols <- List.fold_left insert state.accent_symbols sym_names;
  local_state.accent_symbols <- List.fold_left insert local_state.accent_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = print_ordinary_math_symbol _loc symbol_value in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym symbol_macro_names @ List.map (fun _ -> sym_val) symbol_utf8_names in
    symbol_paragraph _loc sym_val (math_list _loc names)
  else []

(* FIXME: |- A and -x should have distinct priority and spacing *)
let new_prefix_symbol _loc sym_names prefix_value =
  let prefix_macro_names, prefix_utf8_names = symbol sym_names in
  let sym = { prefix_prio = IProd; prefix_space = 3; prefix_no_space = false;
              prefix_macro_names; prefix_utf8_names; prefix_value } in
  quail_out prefix_macro_names prefix_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.prefix_symbols <- List.fold_left insert state.prefix_symbols sym_names;
  local_state.prefix_symbols <- List.fold_left insert local_state.prefix_symbols sym_names;
  let asym = { symbol_macro_names = prefix_macro_names;
               symbol_utf8_names = prefix_utf8_names;
               symbol_value = prefix_value } in
  let insert map name = PrefixTree.add name asym map in
  state.any_symbols <- List.fold_left insert state.any_symbols sym_names;
  local_state.any_symbols <- List.fold_left insert local_state.any_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = print_ordinary_math_symbol _loc prefix_value in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym prefix_macro_names @ List.map (fun _ -> sym_val) prefix_utf8_names in
    symbol_paragraph _loc sym_val (math_list _loc names)
  else []

let new_postfix_symbol _loc sym_names postfix_value =
  let postfix_macro_names, postfix_utf8_names = symbol sym_names in
  let sym = { postfix_prio = Prod; postfix_space = 3; postfix_no_space = false; postfix_macro_names; postfix_utf8_names; postfix_value } in
  quail_out postfix_macro_names postfix_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.postfix_symbols <- List.fold_left insert state.postfix_symbols sym_names;
  local_state.postfix_symbols <- List.fold_left insert local_state.postfix_symbols sym_names;
  let asym = { symbol_macro_names = postfix_macro_names;
               symbol_utf8_names = postfix_utf8_names;
               symbol_value = postfix_value } in
  let insert map name = PrefixTree.add name asym map in
  state.any_symbols <- List.fold_left insert state.any_symbols sym_names;
  local_state.any_symbols <- List.fold_left insert local_state.any_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = print_ordinary_math_symbol _loc postfix_value in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym postfix_macro_names @ List.map (fun _ -> sym_val) postfix_utf8_names in
    symbol_paragraph _loc sym_val (math_list _loc names)
  else []

let new_quantifier_symbol _loc sym_names symbol_value =
  let symbol_macro_names, symbol_utf8_names = symbol sym_names in
  let sym = { symbol_macro_names; symbol_utf8_names; symbol_value } in
  quail_out symbol_macro_names symbol_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.quantifier_symbols <- List.fold_left insert state.quantifier_symbols sym_names;
  local_state.quantifier_symbols <- List.fold_left insert local_state.quantifier_symbols sym_names;
  state.any_symbols <- List.fold_left insert state.any_symbols sym_names;
  local_state.any_symbols <- List.fold_left insert local_state.any_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = print_ordinary_math_symbol _loc symbol_value in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym symbol_macro_names @ List.map (fun _ -> sym_val) symbol_utf8_names in
    symbol_paragraph _loc sym_val (math_list _loc names)
  else []

let new_left_delimiter _loc sym_names delimiter_values =
  let delimiter_macro_names, delimiter_utf8_names = symbol sym_names in
  let sym = { delimiter_macro_names; delimiter_utf8_names; delimiter_values } in
  quail_out delimiter_macro_names delimiter_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.left_delimiter_symbols <- List.fold_left insert state.left_delimiter_symbols sym_names;
  local_state.left_delimiter_symbols <- List.fold_left insert local_state.left_delimiter_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let syms =
      exp_list _loc [maths_construct _loc "Ordinary" (Some (exp_apply1 _loc (maths_lid _loc "node") (exp_fun_s _loc "x" (exp_fun_s _loc "y" (exp_apply1 _loc (exp_dot _loc "List" "flatten") (exp_apply3 _loc (exp_dot _loc "Maths" "multi_glyphs") delimiter_values (exp_lid _loc "x") (exp_lid _loc "y")))))))]
    in
    let sym_val =
      exp_list _loc [maths_construct _loc "Ordinary" (Some (exp_apply1 _loc (maths_lid _loc "node") (exp_fun_s _loc "x" (exp_fun_s _loc "y" (exp_apply _loc (exp_apply1 _loc (exp_dot _loc "List" "hd") delimiter_values) [exp_lid _loc "x"; exp_lid _loc "y"])))))]
    in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym delimiter_macro_names @ List.map (fun _ -> sym_val) delimiter_utf8_names in
    symbol_paragraph _loc syms (math_list _loc names)
  else []

let new_right_delimiter _loc sym_names delimiter_values =
  let delimiter_macro_names, delimiter_utf8_names = symbol sym_names in
  let sym = { delimiter_macro_names; delimiter_utf8_names; delimiter_values } in
  quail_out delimiter_macro_names delimiter_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.right_delimiter_symbols <- List.fold_left insert state.right_delimiter_symbols sym_names;
  local_state.right_delimiter_symbols <- List.fold_left insert local_state.right_delimiter_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let syms =
      exp_list _loc [maths_construct _loc "Ordinary" (Some (exp_apply1 _loc (maths_lid _loc "node") (exp_fun_s _loc "x" (exp_fun_s _loc "y" (exp_apply1 _loc (exp_dot _loc "List" "flatten") (exp_apply3 _loc (exp_dot _loc "Maths" "multi_glyphs") delimiter_values (exp_lid _loc "x") (exp_lid _loc "y")))))))]
    in
    let sym_val =
      exp_list _loc [maths_construct _loc "Ordinary" (Some (exp_apply1 _loc (maths_lid _loc "node") (exp_fun_s _loc "x" (exp_fun_s _loc "y" (exp_apply _loc (exp_apply1 _loc (exp_dot _loc "List" "hd") delimiter_values) [exp_lid _loc "x"; exp_lid _loc "y"])))))]
    in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym delimiter_macro_names @ List.map (fun _ -> sym_val) delimiter_utf8_names in
    symbol_paragraph _loc syms (math_list _loc names)
  else []

let new_operator_symbol _loc operator_kind sym_names operator_values =
  let operator_macro_names, operator_utf8_names = symbol sym_names in
  let operator_prio = Operator in
  let sym = { operator_prio; operator_kind; operator_macro_names; operator_utf8_names; operator_values } in
  quail_out operator_macro_names operator_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.operator_symbols <- List.fold_left insert state.operator_symbols sym_names;
  local_state.operator_symbols <- List.fold_left insert local_state.operator_symbols sym_names;
  let asym = { symbol_macro_names = operator_macro_names;
               symbol_utf8_names = operator_utf8_names;
               symbol_value = CamlSym (exp_apply1 _loc (exp_dot _loc "List" "hd") operator_values) } in
  let insert map name = PrefixTree.add name asym map in
  state.any_symbols <- List.fold_left insert state.any_symbols sym_names;
  local_state.any_symbols <- List.fold_left insert local_state.any_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let syms =
      exp_list _loc [maths_construct _loc "Ordinary" (Some (exp_apply1 _loc (maths_lid _loc "node") (exp_fun_s _loc "x" (exp_fun_s _loc "y" (exp_apply1 _loc (exp_dot _loc "List" "flatten") (exp_apply3 _loc (exp_dot _loc "Maths" "multi_glyphs") operator_values (exp_lid _loc "x") (exp_lid _loc "y")))))))]
    in
    let sym_val =
      exp_list _loc [maths_construct _loc "Ordinary" (Some (exp_apply1 _loc (maths_lid _loc "node") (exp_fun_s _loc "x" (exp_fun_s _loc "y" (exp_apply _loc (exp_apply1 _loc (exp_dot _loc "List" "hd") operator_values) [exp_lid _loc "x"; exp_lid _loc "y"])))))]
    in
    let sym s =
      print_ordinary_math_symbol _loc (CamlSym (maths_glyphs _loc s))
    in
    let names = List.map sym operator_macro_names @ List.map (fun _ -> sym_val) operator_utf8_names in
    symbol_paragraph _loc syms (math_list _loc names)
  else []

let new_combining_symbol _loc uchr macro =
  (* An parser for the new symbol as an atom. *)
  let _parse_sym = string uchr () in
  state.combining_symbols <- PrefixTree.add uchr macro state.combining_symbols;
  local_state.combining_symbols <- PrefixTree.add uchr macro local_state.combining_symbols;
  (* TODO *)
  (* Displaying no the document. *)
  if state.verbose then
    let sym =
      maths_ordinary_node _loc (maths_glyphs _loc uchr)
    in
    let macro = "\\" ^ macro in
    let macro =
      maths_ordinary_node _loc (maths_glyphs _loc macro)
    in
    symbol_paragraph _loc sym macro
  else []

let parser symbol_def =
  | "\\Configure_math_macro" "{" "\\"? - id:lid "}" cs:configs ->
      state.math_macros <- (id, cs) :: state.math_macros;
      local_state.math_macros <- (id, cs) :: local_state.math_macros; []
  | "\\Configure_word_macro" "{" "\\"? - id:lid "}" cs:configs ->
      state.word_macros <- (id, cs) :: state.word_macros;
      local_state.word_macros <- (id, cs) :: local_state.word_macros; []
  | "\\Configure_environment" "{" id:lid "}" cs:configs ->
      state.environment <- (id, cs) :: state.environment;
      local_state.environment <- (id, cs) :: local_state.environment; []
  | "\\Verbose_Changes" ->
      state.verbose <- true; local_state.verbose <- true; []
  | "\\Save_Grammar"    -> build_grammar (); []
  | "\\Add_relation"      ss:symbols e:symbol_value ->
      new_infix_symbol _loc Rel      ss e
  | "\\Add_addition_like" ss:symbols e:symbol_value ->
      new_infix_symbol _loc Sum      ss e
  | "\\Add_product_like"  ss:symbols e:symbol_value ->
      new_infix_symbol _loc Prod     ss e
  | "\\Add_connector"     ss:symbols e:symbol_value ->
      new_infix_symbol _loc Conj     ss e
  | "\\Add_arrow"         ss:symbols e:symbol_value ->
     new_infix_symbol _loc Impl      ss e
  | "\\Add_punctuation"   ss:symbols e:symbol_value ->
     new_infix_symbol _loc Punc   ss e (* FIXME: check *)
  | "\\Add_quantifier"    ss:symbols e:symbol_value ->
     new_quantifier_symbol _loc      ss e
  | "\\Add_prefix"        ss:symbols e:symbol_value ->
     new_prefix_symbol _loc          ss e
  | "\\Add_postfix"       ss:symbols e:symbol_value ->
     new_postfix_symbol _loc         ss e
  | "\\Add_accent"        ss:symbols e:symbol_value ->
     new_accent_symbol _loc          ss e
  | "\\Add_symbol"        ss:symbols e:symbol_value ->
     new_symbol _loc                 ss e
  (* Addition of mutliple symbols (different sizes) *)
  | "\\Add_operator"      ss:symbols e:symbol_values ->
     new_operator_symbol _loc NoLimits  ss e
  | "\\Add_limits_operator" ss:symbols e:symbol_values ->
     new_operator_symbol _loc Limits ss e
  | "\\Add_left"          ss:symbols e:symbol_values ->
     new_left_delimiter _loc         ss e
  | "\\Add_right"         ss:symbols e:symbol_values ->
     new_right_delimiter _loc        ss e
  (* Special case, combining symbol *)
  | "\\Add_combining" "{" c:uchar "}" "{" "\\" - m:lid "}" ->
     new_combining_symbol _loc       c m

type indice_height = Up | Down

let parser left_indices =
                   | "__"-> Down
                   | "^^"-> Up

let parser right_indices =
                   | "_" -> Down
                   | "^" -> Up

let parser any_symbol = sym:math_any_symbol -> sym.symbol_value

let merge_indices indices ind =
  assert(ind.down_left = None);
  assert(ind.up_left = None);
  if (indices.down_right <> None && ind.down_right <> None) ||
     (indices.up_right <> None && ind.up_right <> None) then give_up ();
  { indices with
    down_right = if ind.down_right <> None then ind.down_right else indices.down_right;
    up_right = if ind.up_right <> None then ind.up_right else indices.up_right}

let parser math_aux prio =
  | m:(math_prefix (next_prio prio)) when prio <> AtomM -> m

  | sym:math_quantifier_symbol ind:with_indices d:math_declaration p:math_punctuation_symbol? m:(math_aux prio) when prio = Operator ->
    (fun indices ->
      let indices = merge_indices indices ind in
      let inter =
        match p with
        | None   -> maths_invisible _loc
        | Some s ->
            let nsl = s.infix_no_left_space in
            let nsr = s.infix_no_right_space in
            let md  = print_math_deco_sym _loc_p s.infix_value no_ind in
            maths_normal _loc nsl md nsr
      in
      let md = print_math_deco_sym _loc_sym sym.symbol_value indices in
      exp_list _loc [exp_apply _loc (maths_lid _loc "bin")
        [exp_int _loc 3; maths_normal _loc true md true; exp_nil _loc;
         exp_list _loc [exp_apply _loc (maths_lid _loc "bin")
           [exp_int _loc 1; inter; d; m no_ind]]]])

  | op:(math_operator prio) ind:with_indices m:(math_aux prio) ->
     (fun indices ->
       let ind = merge_indices indices ind in
      match op.operator_kind with
        Limits ->
          exp_list _loc [exp_apply _loc (maths_lid _loc "op_limits") [exp_nil _loc; print_math_deco_sym _loc_op (MultiSym op.operator_values) ind; m no_ind]]
      | NoLimits ->
         exp_list _loc [exp_apply _loc (maths_lid _loc "op_nolimits") [exp_nil _loc; print_math_deco_sym _loc_op (MultiSym op.operator_values) ind; m no_ind]])

  | l:(math_aux prio) st:{ s:(math_infix_symbol prio) i:with_indices -> (s,i)
                         | BLANK when prio = IProd  -> (invisible_product, no_ind)
                         | - when prio = IApply ->  (invisible_apply  , no_ind) }
    r:(math_aux (next_prio prio)) when prio <> AtomM ->
     let s,ind = st in
     (fun indices ->
       let sp = s.infix_space in
       let nsl = s.infix_no_left_space in
       let nsr = s.infix_no_right_space in
       let indices = merge_indices indices ind in
       let l = l no_ind and r = r (if s.infix_value = Invisible then indices else no_ind) in
       if s.infix_value = SimpleSym "over" then begin
         if indices <> no_ind then give_up ();
         maths_fraction _loc l r
       end else begin
         let inter =
           if s.infix_value = Invisible then
             maths_invisible _loc
           else
             let v = print_math_deco_sym _loc_st s.infix_value indices in
             maths_normal _loc nsl v nsr
         in
         exp_list _loc [maths_construct _loc "Binary" (Some (exp_record _loc
           [exp_field _loc "Maths" "bin_priority" (exp_int _loc sp);
            exp_field _loc "Maths" "bin_drawing" inter;
            exp_field _loc "Maths" "bin_left" l;
            exp_field _loc "Maths" "bin_right" r] None))]
       end)

  (* Les règles commençant avec un { forment un conflict avec les arguments
   des macros. Je pense que c'est l'origine de nos problèmes de complexité. *)
  | '{' m:(math_aux Punc) '}' when prio = AtomM -> m
  | '{' s:any_symbol ind:with_indices '}' when prio = AtomM ->
      if s = Invisible then give_up ();
      let f indices =
        let indices = merge_indices indices ind in
        let md = print_math_deco_sym _loc_s s indices in
        exp_list _loc [maths_construct _loc "Ordinary" (Some md)]
      in f

  | l:all_left_delimiter m:(math_aux Punc) r:all_right_delimiter  when prio = AtomM ->
     (fun indices ->
       let l = print_math_symbol _loc_l (MultiSym l.delimiter_values) in
       let r = print_math_symbol _loc_r (MultiSym r.delimiter_values) in
       print_math_deco _loc (exp_list _loc [maths_construct _loc "Decoration" (Some (exp_tuple _loc [exp_apply2 _loc (maths_lid _loc "open_close") l r; m no_ind]))]) indices)

  | name:''[a-zA-Z][a-zA-Z0-9]*'' when prio = AtomM ->
     (fun indices ->
       if String.length name > 1 then
         let elt = exp_fun_s _loc "env" (exp_apply2 _loc (maths_lid _loc "glyphs") (exp_string _loc name) (exp_apply2 _loc (maths_lid _loc "change_fonts") (exp_lid _loc "env") (exp_field_access _loc (exp_lid _loc "env") (Lident "font")))) in
         exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_name (CamlSym elt) indices))]
       else
         exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_name (SimpleSym name) indices))])

  | sym:math_atom_symbol  when prio = AtomM ->
      (fun indices ->
        exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_sym sym.symbol_value indices))])

  | num:''[0-9]+\([.][0-9]+\)?''  when prio = AtomM ->
     (fun indices ->
       exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_num (SimpleSym num) indices))])

  | '\\' id:mathlid when prio = AtomM ->>
     let config = try List.assoc id state.math_macros with Not_found -> [] in
     args:(macro_arguments Math config) ->
     (fun indices ->
       let m = real_name _loc id config in
       (* TODO special macro properties to be handled. *)
       let apply acc arg = exp_apply1 _loc acc arg in
       let e = List.fold_left apply m args in
       print_math_deco _loc_id e indices
     )
  | m:(math_aux Accent) sym:math_combining_symbol when prio = Accent ->
     (fun indices -> print_math_deco _loc (exp_apply1 _loc (exp_lid _loc sym) (m no_ind)) indices)

  | m:(math_aux Accent) s:math_accent_symbol when prio = Accent ->
    let s = exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_s s.symbol_value no_ind))] in
    let rd indices =
      if indices.up_right <> None then give_up ();
      { indices with up_right = Some s; up_right_same_script = true }
    in
    (fun indices -> m (rd indices))

  | m:(math_aux Ind) s:Subsup.subscript when prio = Ind ->
    let s = exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_s (SimpleSym s) no_ind))] in
    let rd indices =
      if indices.down_right <> None then give_up ();
      { indices with down_right = Some s }
    in
    (fun indices -> m (rd indices))

  | m:(math_aux Ind) s:Subsup.superscript when prio = Ind ->
    let s = exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_s (SimpleSym s) no_ind))] in
    let rd indices =
      if indices.up_right <> None then give_up ();
      { indices with up_right = Some s }
    in
    (fun indices -> m (rd indices))

  | m:(math_aux Ind) - h:right_indices - r:(math_aux Accent) when prio = Ind ->
     (fun indices -> match h with
     | Down ->
         if indices.down_right <> None then give_up ();
        m { indices with down_right = Some (r no_ind) }
     | Up ->
        if indices.up_right <> None then give_up ();
        m { indices with up_right = Some (r no_ind) }
     )

  | m:(math_aux Ind) - h:right_indices - s:any_symbol when prio = Ind ->
     let s = print_ordinary_math_symbol _loc s in
     (fun indices -> match h with
     | Down ->
         if indices.down_right <> None then give_up ();
        m { indices with down_right = Some s }
     | Up ->
        if indices.up_right <> None then give_up ();
        m { indices with up_right = Some s }
     )

  | m:(math_aux Accent) - h:left_indices - r:(math_aux LInd) when prio = LInd ->
     (fun indices -> match h with
     | Down ->
        if indices.down_left <> None then give_up ();
        r { indices with down_left = Some (m no_ind) }
     | Up ->
        if indices.up_left <> None then give_up ();
        r { indices with up_left = Some (m no_ind) }
     )

  | s:any_symbol - h:left_indices - r:(math_aux LInd) when prio = LInd ->
     let s = print_ordinary_math_symbol _loc s in
     (fun indices -> match h with
     | Down ->
        if indices.down_left <> None then give_up ();
        r { indices with down_left = Some s }
     | Up ->
        if indices.up_left <> None then give_up ();
        r { indices with up_left = Some s }
     )

and parser math_prefix prio =
  | p:(math_postfix prio) -> p

  | sym:(math_prefix_symbol prio) ind:with_indices m:(math_prefix prio) ->
     (fun indices ->
       let indices = merge_indices indices ind in
       let psp = sym.prefix_space in
       let pnsp = sym.prefix_no_space in
       let md = print_math_deco_sym _loc_sym sym.prefix_value indices in
       maths_bin _loc psp (maths_normal _loc true md pnsp) (exp_nil _loc) (m no_ind)
     )

and parser math_postfix prio =
  | p:(math_aux prio) -> p

  | m:(math_postfix prio) sym:(math_postfix_symbol prio) ->
      (fun indices ->
        let psp = sym.postfix_space in
        let nsp = sym.postfix_no_space in
        let md  = print_math_deco_sym _loc_sym sym.postfix_value indices in
        let m = m no_ind in
        maths_bin _loc psp (maths_normal _loc nsp md true) m (exp_nil _loc))

and parser with_indices =
  | EMPTY -> no_ind

  | i:with_indices h:right_indices - r:(math_aux Accent) ->
     begin
       match h with
       | Down -> if i.down_right <> None then give_up ();
                       { i with down_right = Some (r no_ind) }
       | Up   -> if i.up_right <> None then give_up ();
                       { i with up_right = Some (r no_ind) }
     end

  | i:with_indices s:Subsup.superscript ->
      let s = exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_s (SimpleSym s) no_ind))] in
      if i.up_right <> None then give_up ();
      { i with up_right = Some s }

  | i:with_indices s:Subsup.subscript ->
      let s = exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_s (SimpleSym s) no_ind))] in
      if i.down_right <> None then give_up ();
      { i with down_right = Some s }

(*  | (m,mp):math_aux - (s,h):indices - (o,i):math_operator ->
     (* FIXME TODO: decap bug: this loops ! *)
     (* Anyway, it is a bad way to write the grammar ... a feature od decap ? *)
     if (mp >= Ind && s = Left) then give_up ();
     if (s = Right) then give_up ();
     let i = match h with
       | Down ->
          if i.down_left <> None then give_up ();
         { i with down_left = Some (m no_ind) }
       | Up ->
          if i.up_left <> None then give_up ();
         { i with up_left = Some (m no_ind) }
     in
    (o, i)*)

and parser math_punc_list =
  | m:(math_aux Ind) -> m no_ind
  | l:math_punc_list s:math_punctuation_symbol m:(math_aux Ind) ->
    let nsl = s.infix_no_left_space in
    let nsr = s.infix_no_right_space in
    let r = m no_ind in
    let inter =
      maths_normal _loc nsl (print_math_deco_sym _loc_s s.infix_value no_ind) nsr
    in
    maths_bin _loc 3 inter l r

and parser long_math_declaration =
  | m:math_punc_list -> m
  | l:long_math_declaration s:math_relation_symbol ind:with_indices r:math_punc_list ->
    let nsl = s.infix_no_left_space in
    let nsr = s.infix_no_right_space in
    let inter =
      maths_normal _loc nsl (print_math_deco_sym _loc_s s.infix_value ind) nsr
    in
    maths_bin _loc 2 inter l r

and parser math_declaration =
    | '{' m:long_math_declaration '}' -> m
    | no_brace m:(math_aux Ind) -> m no_ind
    | no_brace m:(math_aux Ind) s:math_relation_symbol ind:with_indices r:(math_aux Ind) ->
       let nsl = s.infix_no_left_space in
       let nsr = s.infix_no_right_space in
       let inter =
         maths_normal _loc nsl (print_math_deco_sym _loc_s s.infix_value ind) nsr
       in
       maths_bin _loc 2 inter (m no_ind) (r no_ind)


let _ = set_grammar math_toplevel (parser
  | m:(math_aux Punc) -> m no_ind
  | s:any_symbol i:with_indices ->
      if s = Invisible then give_up ();
      exp_list _loc [maths_construct _loc "Ordinary" (Some (print_math_deco_sym _loc_s s i))])


(****************************************************************************
 * Text content of paragraphs and macros (mutually recursive).              *
 ****************************************************************************)


(***** Patoline macros  *****)

  let reserved_macro =
    [ "begin"; "end"; "item"; "verb" ]

  let macro_name = change_layout (
    parser "\\" - m:lid ->
      if List.mem m reserved_macro then
        give_up (); m) no_blank
    (* FIXME: useless change layout, but do not work if removed !!! ?*)

  let parser macro =
    | id:macro_name ->>
       let config = try List.assoc id state.word_macros with Not_found -> [] in
       args:(macro_arguments Text config) ->
       (let fn = fun acc r -> exp_apply1 _loc acc r in
        List.fold_left fn (exp_lid _loc id) args)
    | m:verbatim_macro -> m

(****************************)

  let parser text_paragraph_elt (tags:TagSet.t) =
    | m:macro -> m

    | "//" - p:(paragraph_basic_text (addTag Italic tags)) - "//" when allowed Italic tags ->
         exp_apply1 _loc (exp_lid _loc "toggleItalic") p
    | "**" - p:(paragraph_basic_text (addTag Bold tags)) - "**" when allowed Bold tags ->
         exp_apply1 _loc (exp_lid _loc "bold") p
    | "||" - p:(paragraph_basic_text (addTag SmallCap tags)) - "||" when allowed SmallCap tags ->
         exp_apply1 _loc (exp_lid _loc "sc") p
(*    | "__" - p:(paragraph_basic_text (addTag Underline tags)) - "__" when allowed Underline tags ->
         <:expr@_loc_p<underline $p$>>
    | "--" - p:(paragraph_basic_text (addTag Strike tags)) - "--" when allowed Strike tags ->
      <:expr@_loc_p<strike $p$>>*)

(* FIXME maybe remove? *)
    | '"' p:(paragraph_basic_text (addTag Quote tags)) '"' when allowed Quote tags ->
        (let opening = "“" in (* TODO adapt with the current language*)
         let closing = "”" in (* TODO adapt with the current language*)
         exp_infix _loc "@" (exp_cons _loc (exp_apply1 _loc (exp_lid _loc "tT") (exp_string _loc opening)) p) (exp_list _loc [exp_apply1 _loc (exp_lid _loc "tT") (exp_string _loc closing)]))

    | "``" p:(paragraph_basic_text (addTag Quote tags)) "''" when allowed Quote tags ->
        (let opening = "“" in (* TODO adapt with the current language*)
         let closing = "”" in (* TODO adapt with the current language*)
         exp_infix _loc "@" (exp_cons _loc (exp_apply1 _loc (exp_lid _loc "tT") (exp_string _loc opening)) p) (exp_list _loc [exp_apply1 _loc (exp_lid _loc "tT") (exp_string _loc closing)]))

    | v:verbatim_sharp  -> v
    | v:verbatim_bquote  -> v

    | dollar m:math_toplevel dollar ->
        let style_rec = exp_record _loc [(mkloc (Lident "mathStyle") _loc, exp_field_access _loc (exp_lid _loc "env0") (Lident "mathStyle"))] (Some (exp_lid _loc "env0")) in
        let kdraw_body = exp_apply2 _loc (exp_dot _loc "Maths" "kdraw") (exp_list _loc [style_rec]) m in
        exp_list _loc [exp_apply1 _loc (exp_lid _loc "bB") (exp_fun_s _loc "env0" kdraw_body)]
    | "\\(" m:math_toplevel "\\)" ->
        let style_rec = exp_record _loc [(mkloc (Lident "mathStyle") _loc, exp_field_access _loc (exp_lid _loc "env0") (Lident "mathStyle"))] (Some (exp_lid _loc "env0")) in
        let kdraw_body = exp_apply2 _loc (exp_dot _loc "Maths" "kdraw") (exp_list _loc [style_rec]) (exp_apply1 _loc (exp_lid _loc "displayStyle") m) in
        exp_list _loc [exp_apply1 _loc (exp_lid _loc "bB") (exp_fun_s _loc "env0" kdraw_body)]

    | ws:word+$ ->
       exp_list _loc [exp_apply1 _loc (exp_lid _loc "tT") (exp_string _loc (String.concat " " ws))]

    | '{' p:(paragraph_basic_text TagSet.empty) '}' -> p

  let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
    let x,y = Lexing.((_loc_p1.Location.loc_end).pos_cnum, (_loc_p2.Location.loc_start).pos_cnum) in
    (*Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
    let _loc = _loc_p2 in
    let bl e = if y - x >= 1 then exp_cons _loc (exp_apply1 _loc (exp_lid _loc "tT") (exp_string _loc " ")) e else e in
    let _loc = Pa_ast.merge2 _loc_p1 _loc_p2 in
    exp_infix _loc "@" p1 (bl p2)

  let _ = set_paragraph_basic_text (fun tags ->
             parser
               l:{p:(text_paragraph_elt tags) -> (_loc, p)}+$ ->
                 match List.rev l with
                 | []   -> assert false
                 | m::l ->
                    let fn = fun (_loc_m, m) (_loc_p, p) ->
                      (Pa_ast.merge2 _loc_p _loc_m
                      , concat_paragraph p _loc_p m _loc_m)
                    in snd (List.fold_left fn m l))

  let oparagraph_basic_text = paragraph_basic_text
  let parser paragraph_basic_text =
      p:(oparagraph_basic_text TagSet.empty) ->
    (fun indented ->
         if indented then
           str_let_wild _loc (exp_ref_assign _loc (exp_dot _loc "D" "structure")
             (exp_apply _loc (exp_lid _loc "newPar")
               [exp_deref _loc (exp_dot _loc "D" "structure");
                exp_dot _loc "Complete" "normal";
                exp_dot _loc "Patoline_Format" "parameters"; p]))
         else
           let env_fun = exp_fun_s _loc "x" (exp_record _loc [(mkloc (Lident "par_indent") _loc, exp_nil _loc)] (Some (exp_lid _loc "x"))) in
           str_let_wild _loc (exp_ref_assign _loc (exp_dot _loc "D" "structure")
             (Exp.apply ~loc:_loc (exp_lid _loc "newPar")
               [(Labelled "environment", env_fun);
                (Nolabel, exp_deref _loc (exp_dot _loc "D" "structure"));
                (Nolabel, exp_dot _loc "Complete" "normal");
                (Nolabel, exp_dot _loc "Patoline_Format" "parameters");
                (Nolabel, p)]))
        )

(****************************************************************************
 * Paragraphs                                                               *
 ****************************************************************************)

  let paragraph = declare_grammar "paragraph"
  let paragraphs = declare_grammar "paragraphs"

  let nb_includes = ref 0

  let parser paragraph_elt =
    | verb:verbatim_environment -> (fun _ -> verb)
    (* FIXME some of the macro below could be defined using the new configure_word_macro *)
    | "\\Caml" s:wrapped_caml_structure -> (fun _ -> s)
    | "\\Include" '{' id:uid '}' -> (fun _ ->
         incr nb_includes;
         (try add_grammar id; build_grammar () with Not_found -> ());
         let temp_id = Printf.sprintf "TEMP%d" !nb_includes in
         str_module _loc temp_id (mod_apply _loc (mod_apply _loc (mod_ident _loc (Ldot (Lident id, "Document"))) (mod_ident _loc (Lident "Patoline_Output"))) (mod_ident _loc (Lident "D")))
         @ str_open _loc temp_id)
    | "\\" id:macrouid ts:simple_text_macro_argument*$ -> (fun _ ->
         let m1 = freshUid () in
         if ts <> [] then
           let m2 = freshUid () in
           let str = List.flatten (List.map (fun t -> str_let _loc "arg1" t) ts) in
           str_module _loc m2 (mod_structure _loc str)
           @ str_module _loc m1 (mod_apply _loc (mod_ident _loc (Lident id)) (mod_ident _loc (Lident m2)))
           @ str_let_wild _loc (exp_apply _loc (exp_dot _loc m1 "do_begin_env") [exp_unit _loc])
           @ str_let_wild _loc (exp_apply _loc (exp_dot _loc m1 "do_end_env") [exp_unit _loc])
         else
           str_module _loc m1 (mod_ident _loc (Lident id))
           @ str_let_wild _loc (exp_apply _loc (exp_dot _loc m1 "do_begin_env") [exp_unit _loc])
           @ str_let_wild _loc (exp_apply _loc (exp_dot _loc m1 "do_end_env") [exp_unit _loc]))
    | "\\begin" '{' idb:lid '}' ->>
        let config = try List.assoc idb state.environment with Not_found -> [] in
          args:(macro_arguments Text config)
          ps:(change_layout paragraphs blank2)
          "\\end" '{' STR(idb) '}'
      ->
         (fun indent_first ->
           let m1 = freshUid () in
           let m2 = freshUid () in
           let arg =
             if args = [] then [] else
               let gen i e =
                 let id = Printf.sprintf "arg%i" (i+1) in
                 str_let _loc id e
               in
               let args = List.mapi gen args in
               let args = List.fold_left (@) [] args in
               str_module _loc ("Arg_"^m2) (mod_structure _loc args)
           in
           let def =
             let name = "Env_" ^ idb in
             let argname = "Arg_" ^ m2 in
             if args = [] then
               str_module _loc m2 (mod_ident _loc (Lident name))
             else
               str_module _loc m2 (mod_apply _loc (mod_ident _loc (Lident name)) (mod_ident _loc (Lident argname)))
           in
           let body = arg @ def @ str_open _loc m2
             @ str_let_wild _loc (exp_apply _loc (exp_dot _loc m2 "do_begin_env") [exp_unit _loc])
             @ ps indent_first
             @ str_let_wild _loc (exp_apply _loc (exp_dot _loc m2 "do_end_env") [exp_unit _loc]) in
           str_module _loc m1 (mod_structure _loc body))
    | m:{ "\\[" math_toplevel "\\]" | "$$" math_toplevel "$$" } ->
         (fun _ ->
           let env_fun = exp_fun_s _loc "x" (exp_record _loc [(mkloc (Lident "par_indent") _loc, exp_nil _loc)] (Some (exp_lid _loc "x"))) in
           let style_rec = exp_record _loc [(mkloc (Lident "mathStyle") _loc, exp_dot _loc "Mathematical" "Display")] (Some (exp_lid _loc "env0")) in
           let kdraw_body = exp_apply2 _loc (exp_dot _loc "Maths" "kdraw") (exp_list _loc [style_rec]) m in
           let bb = exp_list _loc [exp_apply1 _loc (exp_lid _loc "bB") (exp_fun_s _loc "env0" kdraw_body)] in
           str_let_wild _loc (exp_ref_assign _loc (exp_dot _loc "D" "structure")
             (Exp.apply ~loc:_loc (exp_lid _loc "newPar")
               [(Labelled "environment", env_fun);
                (Nolabel, exp_deref _loc (exp_dot _loc "D" "structure"));
                (Nolabel, exp_dot _loc "Complete" "normal");
                (Nolabel, exp_lid _loc "displayedFormula");
                (Nolabel, bb)])))
    | l:paragraph_basic_text -> l
    | s:symbol_def -> fun _ -> s

  let _ = set_grammar paragraph
                      (change_layout
                         (parser e:paragraph_elt l:paragraph_elt*
                            -> fun i -> List.flatten (e i :: List.map (fun r -> r false) l)
                         ) ~new_blank_after:false blank1)

  let _ = set_grammar paragraphs (
    parser p:paragraph ps:paragraph* ->
      let ps = List.flatten (List.map (fun r -> r true) ps) in
      fun indent_first -> p indent_first @ ps)

(****************************************************************************
 * Sections, layout of the document.                                        *
 ****************************************************************************)

(* Returns a couple (numbered, in_toc). *)
let numbered op cl =
  match (op.[0], cl.[0]) with
  | ('=', '=') -> (true , true )
  | ('-', '-') -> (false, true )
  | ('_', '_') -> (false, false)
  | _          -> give_up ()

let sect lvl = parser
  | "==" when lvl = 0
  | "===" when lvl = 1
  | "====" when lvl = 2
  | "=====" when lvl = 3
  | "======" when lvl = 4
  | "=======" when lvl = 5
  | "========" when lvl = 6
  | "=========" when lvl = 7
let usect lvl = parser
  | "--" when lvl = 0
  | "---" when lvl = 1
  | "----" when lvl = 2
  | "-----" when lvl = 3
  | "------" when lvl = 4
  | "-------" when lvl = 5
  | "--------" when lvl = 6
  | "---------" when lvl = 7

let parser text_item lvl =
  | op:''[-=_]>'' title:simple_text txt:(topleveltext (lvl+1)) cl:''[-=_]<'' when lvl < 8 ->
    let (num, in_toc) = numbered op cl in
    (fun _ lvl' ->
      assert(lvl' = lvl);
      let code =
        let new_struct = Exp.apply ~loc:_loc (exp_lid _loc "newStruct")
          [(Labelled "in_toc", exp_bool _loc in_toc);
           (Labelled "numbered", exp_bool _loc num);
           (Nolabel, exp_deref _loc (exp_dot _loc "D" "structure"));
           (Nolabel, title)] in
        str_let_wild _loc (exp_ref_assign _loc (exp_dot _loc "D" "structure") new_struct)
        @ txt false (lvl+1)
        @ str_let_wild _loc (exp_apply1 _loc (exp_lid _loc "go_up") (exp_dot _loc "D" "structure"))
      in
      (true, lvl, code))

  | (num,title):{_:(sect lvl)  title:simple_text _:(sect lvl) -> true, title
                |_:(usect lvl) title:simple_text _:(usect lvl)-> false, title }
      txt:(topleveltext (lvl+1))$ when lvl < 8 ->
     (fun _ lvl' ->
       assert (lvl' >= lvl);
      let code =
        let new_struct = Exp.apply ~loc:_loc (exp_lid _loc "newStruct")
          [(Labelled "numbered", exp_bool _loc num);
           (Nolabel, exp_deref _loc (exp_dot _loc "D" "structure"));
           (Nolabel, title)] in
        str_let_wild _loc (exp_ref_assign _loc (exp_dot _loc "D" "structure") new_struct)
        @ txt false (lvl+1)
        @ str_let_wild _loc (exp_apply1 _loc (exp_lid _loc "go_up") (exp_dot _loc "D" "structure"))
      in
      (true, lvl, code))

  | ps:paragraph when lvl < 8 ->
    (fun indent lvl -> (true, lvl, ps indent))


and parser topleveltext lvl = l:(text_item lvl)* ->
  (fun indent lvl ->
    let fn (indent, lvl, ast) txt =
      let indent, lvl, ast' = txt indent lvl in
      (indent, lvl, (ast @ ast'))
    in
    let _,_,r = List.fold_left fn (indent, lvl, []) l in r)


and parser text = txt:(topleveltext 0) -> txt true 0

(* Header, title, main Patoline entry point *********************************)

let patoline_config : unit grammar =
  change_layout (
    parser
    | "#FORMAT " f:uid ->
       set_patoline_format f;
       (try add_grammar f with _ -> ())
    | "#DRIVER " d:uid ->
       set_patoline_driver d
    | "#PACKAGES " ps:''[,a-zA-Z]+'' ->
       add_patoline_packages ps
    | "#GRAMMAR " g:''[a-zA-Z]+''    ->
       add_grammar g
  ) no_blank

let parser header = _:patoline_config*$ ->
  fun () -> List.iter add_grammar !patoline_grammar; build_grammar ()

let parser title =
  | RE("==========\\(=*\\)")
      title:simple_text
      (auth,inst,date):{
        auth:{_:RE("----------\\(-*\\)") simple_text}
        (inst,date):{
          inst:{_:RE("----------\\(-*\\)") simple_text}
          date:{_:RE("----------\\(-*\\)") simple_text}? -> (Some inst, date)
        }?[None,None] -> (Some auth, inst, date)
      }?[None,None,None]
    RE("==========\\(=*\\)") ->

      let date =
        match date with
        | None   -> exp_nil _loc
        | Some t -> exp_list _loc [exp_tuple _loc [exp_string _loc "Date"; exp_apply1 _loc (exp_lid _loc "string_of_contents") t]]
      in
      let inst =
        match inst with
        | None   -> exp_nil _loc
        | Some t -> exp_list _loc [exp_tuple _loc [exp_string _loc "Institute"; exp_apply1 _loc (exp_lid _loc "string_of_contents") t]]
      in
      let auth =
        match auth with
        | None   -> exp_nil _loc
        | Some t -> exp_list _loc [exp_tuple _loc [exp_string _loc "Author"; exp_apply1 _loc (exp_lid _loc "string_of_contents") t]]
      in
      let tags = exp_infix _loc "@" auth (exp_infix _loc "@" inst date) in
      str_let_wild _loc (Exp.apply ~loc:_loc (exp_dot _loc "Patoline_Format" "title")
        [(Nolabel, exp_dot _loc "D" "structure");
         (Labelled "extra_tags", tags);
         (Nolabel, title)])

let wrap basename _loc ast =
  let opens = List.concat [
    str_open_lid _loc (Lident "Patoraw");
    str_open_lid _loc (Lident "Typography");
    str_open_lid _loc (Ldot (Lident "Typography", "Box"));
    str_open_lid _loc (Ldot (Lident "Typography", "Document"));
    str_open_lid _loc (Ldot (Lident "Typography", "Maths"));
    str_open_lid _loc (Lident "RawContent");
    str_open_lid _loc (Lident "Color");
    str_open_lid _loc (Lident "Driver");
    str_open_lid _loc (Lident "DefaultMacros")] in
  let cache_let = str_let _loc ("cache_"^basename) (exp_array _loc (List.rev !cache_buf)) in
  let mcache_let = str_let _loc ("mcache_"^basename) (exp_array _loc (List.rev !mcache_buf)) in
  let format_mod = str_module _loc "Patoline_Format"
    (mod_apply _loc (mod_ident _loc (Ldot (Lident !patoline_format, "Format"))) (mod_ident _loc (Lident "D"))) in
  let open_format = str_open _loc !patoline_format @ str_open _loc "Patoline_Format" in
  let temp1_let = str_let _loc "temp1"
    (exp_apply2 _loc (exp_dot _loc "List" "map") (exp_lid _loc "fst")
      (exp_apply1 _loc (exp_lid _loc "snd") (exp_deref _loc (exp_dot _loc "D" "structure")))) in
  let follow_let = str_let_wild _loc (exp_ref_assign _loc (exp_dot _loc "D" "structure")
    (exp_apply2 _loc (exp_lid _loc "follow")
      (exp_apply1 _loc (exp_lid _loc "top") (exp_deref _loc (exp_dot _loc "D" "structure")))
      (exp_apply1 _loc (exp_dot _loc "List" "rev") (exp_lid _loc "temp1")))) in
  let functor_body = cache_let @ mcache_let @ format_mod @ open_format @ temp1_let @ ast @ follow_let in
  let doc_mod = str_module _loc "Document"
    (Mod.functor_ ~loc:_loc (Named (mkloc (Some "Patoline_Output") _loc, Mty.ident ~loc:_loc (mkloc (Ldot (Lident "DefaultFormat", "Output")) _loc)))
      (Mod.functor_ ~loc:_loc (Named (mkloc (Some "D") _loc, Mty.ident ~loc:_loc (mkloc (Lident "DocumentStructure") _loc)))
        (mod_structure _loc functor_body))) in
  opens @ doc_mod

let parser full_text = f:header ->>
  let _ = f () in
  let file = match !file with None -> "" | Some f -> f in
  let (_,base,_) = Filename.decompose file in
  let _ = cache := "cache_" ^ base in
  t:{tx1:text t:title}? tx2:text EOF ->
    let t = match t with None -> [] | Some (tx1,t) -> tx1 @ t in
    wrap base _loc (t @ tx2)

(* Extension of Ocaml's grammar *********************************************)

let parser directive =
  | '#' n:uid a:uid ->
    ((match n with
       | "FORMAT"  -> patoline_format := a
       | "DRIVER"  -> patoline_driver := a
       | "PACKAGE" -> patoline_packages := a :: !patoline_packages
       | _ -> give_up ());
    ([] : Parsetree.structure_item list))
(* let extra_structure = [directive] (* unused - Extension removed *) *)

let parser patoline_quotations ((_,lvl) : Pa_ocaml_prelude.alm * Pa_ocaml_prelude.expression_prio) =
  | "<<" par:simple_text     ">>" when lvl <= Atom -> par
  | "<$" mat:math_toplevel "$>" when lvl <= Atom -> mat

let _ =
  let reserved = ["<<"; ">>"; "<$"; "$>"; "<<$"; "$>>"] in
  List.iter Pa_lexing.add_reserved_symb reserved

(* let extra_expressions = [patoline_quotations] (* unused - Extension removed *) *)

(* Entry points and extension creation **************************************)

(* Adding the new entry points *)

let parse_ml =
  parser f:header ->>
    let _ =
      try f () with e ->
        Printf.eprintf "Exception: %s\nTrace:\n%!" (Printexc.to_string e)
    in
    structure

let parse_mli =
  parser f:header ->>
    let _ =
      try f () with e ->
        Printf.eprintf "Exception: %s\nTrace:\n%!" (Printexc.to_string e)
    in
    signature

(* End of grammar/parser definitions *)

(* Generator for the main file. *)
let write_main_file driver form build_dir dir name =
  let full = Filename.concat dir name in
  let fullb = Filename.concat build_dir name in
  let file = fullb ^ "_.ml" in
  let oc = open_out file in
  let dcache = fullb ^ ".tdx" in
  let fmt = Format.formatter_of_out_channel oc in
  let _loc = Location.none in
  let m =
    let c  = (String.make 1 (Char.uppercase_ascii name.[0])) in
    let cs = String.sub name 1 (String.length name - 1) in
    c ^ cs
  in
  let ast =
    let opens = List.concat [
      str_open_lid _loc (Lident "Patoraw");
      str_open_lid _loc (Lident "Typography");
      str_open_lid _loc (Ldot (Lident "Typography", "Box"));
      str_open_lid _loc (Ldot (Lident "Typography", "Document"));
      str_open_lid _loc (Lident "RawContent");
      str_open_lid _loc (Lident "Color")] in
    let read_cache = str_let_wild _loc (exp_apply1 _loc (exp_dot _loc "Distance" "read_cache") (exp_string _loc dcache)) in
    let d_struct_body = str_let _loc "structure"
      (exp_apply1 _loc (exp_lid _loc "ref")
        (exp_tuple _loc [
          exp_apply1 _loc (exp_lid _loc "Node")
            (exp_record _loc [(mkloc (Lident "node_tags") _loc,
              exp_list _loc [exp_tuple _loc [exp_string _loc "intoc"; exp_string _loc ""]])]
              (Some (exp_lid _loc "empty")));
          exp_nil _loc])) in
    let d_mod = str_module _loc "D"
      (Mod.constraint_ ~loc:_loc (mod_structure _loc d_struct_body)
        (Mty.ident ~loc:_loc (mkloc (Lident "DocumentStructure") _loc))) in
    let driver_mod = str_module _loc "Driver" (mod_ident _loc (Lident driver)) in
    let parse_argv = str_let_wild _loc (exp_apply _loc (exp_dot _loc "Arg" "parse_argv")
      [exp_apply1 _loc (exp_dot _loc "Driver" "filter_options") (exp_dot _loc "Sys" "argv");
       exp_infix _loc "@" (exp_dot _loc "Driver" "driver_options") (exp_dot _loc "DefaultFormat" "spec");
       exp_lid _loc "ignore"; exp_string _loc "Usage :"]) in
    let format0_mod = str_module _loc "Patoline_Format0"
      (mod_apply _loc (mod_ident _loc (Ldot (Lident form, "Format"))) (mod_ident _loc (Lident "D"))) in
    let open_f0 = str_open _loc "Patoline_Format0" in
    let format_mod = str_module _loc "Patoline_Format" (mod_ident _loc (Lident "Patoline_Format0")) in
    let output_mod = str_module _loc "Patoline_Output"
      (mod_apply _loc (mod_ident _loc (Ldot (Lident "Patoline_Format0", "Output"))) (mod_ident _loc (Lident "Driver"))) in
    let tmp_mod = str_module _loc "TMP"
      (mod_apply _loc (mod_apply _loc (mod_ident _loc (Ldot (Lident m, "Document"))) (mod_ident _loc (Lident "Patoline_Output"))) (mod_ident _loc (Lident "D"))) in
    let open_tmp = str_open _loc "TMP" in
    let output_call = str_let_wild _loc (exp_apply _loc (exp_dot _loc "Patoline_Output" "output")
      [exp_dot _loc "Patoline_Output" "outputParams";
       exp_apply1 _loc (exp_lid _loc "fst") (exp_apply1 _loc (exp_lid _loc "top") (exp_deref _loc (exp_dot _loc "D" "structure")));
       exp_apply _loc (exp_dot _loc "List" "fold_left")
         [exp_fun_s _loc "acc" (exp_fun_s _loc "f" (exp_apply1 _loc (exp_lid _loc "f") (exp_lid _loc "acc")));
          exp_dot _loc "Patoline_Format" "defaultEnv";
          exp_deref _loc (exp_lid _loc "init_env_hook")];
       exp_string _loc full]) in
    let write_cache = str_let_wild _loc (exp_apply1 _loc (exp_dot _loc "Distance" "write_cache") (exp_string _loc dcache)) in
    opens @ read_cache @ d_mod @ driver_mod @ parse_argv @ format0_mod @ open_f0
    @ format_mod @ output_mod @ tmp_mod @ open_tmp @ output_call @ write_cache
  in
  Format.fprintf fmt "%a\n%!" Pprintast.structure ast;
  close_out oc;
  if !debug then Printf.eprintf "Written main file %s\n%!" file

(* Creating and running the parser - standalone main *)
let _ =
  try
    let usage = Printf.sprintf "usage: %s [options] file" Sys.argv.(0) in
    let file_ref = file in
    Arg.parse (spec @ [
      ("--debug", Arg.Set_int Earley.debug_lvl,
        "Sets the value of \"Earley.debug_lvl\".");
      ("--ascii", Arg.Unit ignore,
        "Output ASCII (always on, kept for compatibility).")
    ]) (fun s -> file_ref := Some s) usage;
    let (filename, ic) =
      match !file_ref with
      | None       -> ("stdin", stdin)
      | Some file  -> (file, open_in file)
    in
    let ext = try Filename.extension filename with _ -> ".txp" in
    let (grammar, blank) =
      match ext with
      | ".txp" -> (full_text, blank2)
      | _      -> (parse_ml, blank2)
    in
    let ast =
      Earley.handle_exception
        (Earley.parse_channel ~filename grammar blank) ic
    in
    Format.printf "%a\n%!" Pprintast.structure ast;
    match !file, !in_ocamldep with
    | Some s, false ->
       let (dir, base, _) = Filename.decompose s in
       let name = base ^ ".tgy" in
       let build_dir = !build_dir in
       let name = Filename.concat build_dir name in
       if !debug then Printf.eprintf "Writing grammar %s\n%!" name;
       (* Check if the build directory needs to be created *)
       if not (Sys.file_exists build_dir)
       then Unix.mkdir build_dir 0o700;
       if local_state <> empty_state then begin
         (* Now write the grammar *)
         let ch = open_out_bin name in
         output_value ch local_state;
         close_out ch;
         if !debug then Printf.eprintf "Written grammar %s\n%!" name;
       end;
       (* Writing the main file. *)
       if !is_main then
         let (drv, fmt) = (!patoline_driver, !patoline_format) in
         write_main_file drv fmt build_dir dir base
    | _ -> ()

  with e ->
    Printf.eprintf "Exception: %s\nTrace:\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
