open Decap
open UsualMake
open FilenameExtra
open Pa_ocaml_prelude

let _ = Sys.catch_break true

(*
 * The patoline language is implemented as a DeCaP OCaml syntax extension. It
 * contains:
 *   - new OCaml expressions to allow Patoline into OCaml code,
 *   - a new entry point for plain Patoline files.
 *)

(* State information + Comand line arguments extension **********************)

let patoline_format   = ref "DefaultFormat"
let patoline_driver   = ref "Pdf"
let patoline_packages = ref ["Typography"]
let patoline_grammar  = ref ["DefaultGrammar"]

let set_patoline_format f =
  patoline_format := f

let set_patoline_driver d =
  patoline_driver := d

let add_patoline_packages ps =
  let ps = Util.split ',' ps in
  patoline_packages := !patoline_packages @ ps

let add_patoline_grammar g =
  patoline_grammar := g :: !patoline_grammar

let no_default_grammar = ref false

let in_ocamldep = ref false

let quail_out_name=ref "" (* filename to output quail.el shortcut for emacs *)

let quail_ch =
  Lazy.from_fun (fun () ->
    let ch = open_out_bin !quail_out_name in
    ch)

let quail_out mnames unames =
  if !quail_out_name <> "" then
    begin
      let unames = List.filter (fun s -> UTF8.validate s && UTF8.length s = 1) unames in
      match unames with
      | [] -> ()
      | u::_ ->
       List.iter (fun name ->
	 Printf.fprintf (Lazy.force quail_ch) "(\"%s\" ?%s)\n" (String.escaped name) u) mnames
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
  ]

#define LOCATE locate

(*
 * Everything is wrapped into the functor, this is standard procedur to write
 * syntax extensions using DeCaP. The argument of the functor is included
 * straight away, so that extensions can be composed.
 *)
module Ext = functor(In:Extension) -> struct
include In

let spec = extra_spec @ spec

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
    | (`SCl([],b), _::_, '}'     ) -> fn `Ini stack curr next nl
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
           ocamldoc_comments := (p,next,comment)::!ocamldoc_comments;
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
  let parser wrapped_caml_structure = '(' {caml_structure | EMPTY -> <:struct<>>} ')'

  (* Parse a caml "expr" wrapped with parentheses *)
  let caml_expr         = change_layout expression blank2
  let parser wrapped_caml_expr = '(' {caml_expr  | EMPTY -> <:expr<()>>} ')'

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

  let char_re    = "[^ \"\t\r\n\\#*/|_$>{}-]"
  let escaped_re =     "\\\\[\\#*/|_$>{}-]"

  let non_special = ['>';'*';'/';'|';'-';'_']
  let char_alone =
    black_box
      (fun str pos ->
       let c,str',pos' = Input.read str pos in
       if List.mem c non_special then
         let c',_,_ = Input.read str' pos' in
         if c' = c then give_up ""
         else c, str', pos'
       else
         give_up "")
      (List.fold_left Charset.add Charset.empty_charset non_special) false
      (String.concat " | " (List.map (fun c -> String.make 1 c) non_special))

  let character =
    parser
    | c:RE(char_re) -> c
    | s:RE(escaped_re) ->
      String.escaped (String.sub s 1 (String.length s - 1))
    | c:char_alone -> String.make 1 c

  let word =
    change_layout (
        parser
        | cs:character+ ->
             let w = String.concat "" cs in
             if String.length w >= 2 &&
                  List.mem (String.sub w 0 2) ["==";"=>";"=<";"--";"->";"-<";">>";"$>";]
             then give_up (w ^ " is not a word");
             w
      ) no_blank

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
  _:''[ \t]*'' "\n"
  lines:{l:verbatim_line "\n"}+
  ''^###'' ->
    if lines = [] then give_up "Empty verbatim environment.";

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
      let f s tl = <:expr<$string:s$ :: $tl$>> in
      List.fold_right f (List.map remhsp lines) <:expr<[]>>
    in
    let mode = "verb_" ^ match mode with None -> "default" | Some m -> m in
    let file =
      match file with
      | None -> <:expr<None>>
      | Some f -> <:expr<Some $string:f$>>
    in
    <:struct<
      let _ = $lid:mode$ $file$ $lines$
    >>

let verbatim_environment = change_layout verbatim_environment no_blank


(*
  let verbatim_line =
      "\\(^#?#?\\([^#\t\n][^\t\n]*\\)?\\)"

  let string_filename = "\\\"\\([a-zA-Z0-9-_.]*\\)\\\""
  let uid_coloring    = "[A-Z][_'a-zA-Z0-9]*"

  let files_contents = Hashtbl.create 31

  let verbatim_environment =
    change_layout (
        parser
          RE("^###")
          lang:{_:RE("[ \t]+") id:RE(uid_coloring)}?
          filename:{_:RE("[ \t]+") fn:RE(string_filename)[groupe 1]}?
          RE("[ \t]*") '\n' lines:{l:RE(verbatim_line) '\n'}+
          RE("^###") -> (
                    let lang = match lang with
                        None -> <:expr<lang_default>>
                      | Some s -> <:expr<$lid:("lang_"^s)$>>
                    in
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
                    let blank = Str.regexp "[ ]+" in
                    let rec split acc pos line =
                      try
                        let start_blank = Str.search_forward blank line pos in
                        let end_blank = Str.match_end () in
                        let acc =
                          String.sub line start_blank (end_blank - start_blank)::
                            String.sub line pos (start_blank - pos):: acc
                        in
                        split acc end_blank line
                      with
                        Not_found ->
                          if pos >= String.length line then List.rev acc else
                          List.rev (String.sub line pos (String.length line - pos)::acc)
                    in
                    let lines = List.map (fun s ->
                                          let s = try String.sub s sps (String.length s - sps)
                                                  with Invalid_argument _ -> "" in
                                          split [] 0 s) lines
                    in
                    begin
                      match filename with
                      | Some name when not !in_ocamldep ->
                         let pos =
                           try Hashtbl.find files_contents name
                           with Not_found -> -1
                         in
                         let new_pos = (start_pos _loc).Lexing.pos_cnum in
                         if new_pos > pos then
                           begin
                             Hashtbl.add files_contents name new_pos;
                             let mode,msg = if pos >= 0 then
                                          [Open_creat; Open_append ], "Appending to"
                                        else
                                          [Open_creat; Open_trunc; Open_wronly ], "Creating"
                             in
                             let name = if Filename.is_relative name then
                                          match !file with
                                            None -> name
                                          | Some file ->
                                             Filename.concat (Filename.dirname file) name
                                        else name
                             in
                             Printf.eprintf "%s file: %s\n%!" msg name;
                             let ch = open_out_gen mode 0o600 name in
                             (* FIXME: not all language accept the next line: *)
                             Printf.fprintf ch "#%d \"%s\"\n" (start_pos _loc_lines).Lexing.pos_lnum (start_pos _loc_lines).Lexing.pos_fname;
                             List.iter (Printf.fprintf ch "%a\n" (fun ch -> List.iter (Printf.fprintf ch "%s"))) lines;
                             close_out ch
                           end;
                      | _ -> ()
                    end;
                    let lines =
                      List.map
                        (fun line ->
                         let line:Parsetree.expression list =
                           List.map (fun s -> <:expr< $string:s$ >>) line
                         in
                         <:expr< String.concat "" $list:line$ >>) lines
                    in
                    let line_with_num =
                      (* FIXME: the fact to have a line name and line number are orthogonal *)
                      match filename with
                      | None ->
                         <:expr< line >>
                      | Some name ->
                         <:expr<verb_counter $string:("verb_file_"^name)$ @ line >>
                    in
                    <:struct<let _ =
                     List.iter (fun line ->
                       newPar D.structure ~environment:verbEnv Complete.normal ragged_left $line_with_num$) ($lang$ $list:lines$)>>)
                  ) no_blank
  *)

  let verbatim_generic st forbid nd =
    let line_re = "[^\n" ^ forbid ^ "]+" in
    change_layout (
        parser
          STR(st)
          ls:{l:RE(line_re) '\n'}*
          l:RE(line_re)
              STR(nd) ->
            let lines = ls @ [l] in
            let lines = rem_hyphen lines in
            let txt = String.concat " " lines in
            <:expr< ($lid:"verbatim"$)
                     $string:txt$ >>
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
  | AtomM | Accent | LInd | Ind | IProd | Prod
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
  | IProd -> Ind
  | Ind -> LInd
  | LInd -> Accent
  | Accent -> AtomM
  | AtomM -> assert false

type symbol =
  | Invisible
  | SimpleSym of string
  | MultiSym of Parsetree.expression
  | CamlSym of Parsetree.expression
  | ComplexSym of string

let parser symbol =
  | s:"\\}"             -> s
  | s:"\\{"             -> s
  | s:''[^ \t\r\n{}]+'' -> s

let symbols =
  let space_blank = blank_regexp ''[ ]*'' in
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

let uchar =
  let char_range min max = parser c:ANY ->
    let cc = Char.code c in
    if cc < min || cc > max then give_up "Char not in range..."; c
  in
  let tl  = char_range 128 191 in
  let hd1 = char_range 0   127 in
  let hd2 = char_range 192 223 in
  let hd3 = char_range 224 239 in
  let hd4 = char_range 240 247 in
  parser
  | c0:hd1                         -> Printf.sprintf "%c" c0
  | c0:hd2 - c1:tl                 -> Printf.sprintf "%c%c" c0 c1
  | c0:hd3 - c1:tl-  c2:tl         -> Printf.sprintf "%c%c%c" c0 c1 c2
  | c0:hd4 - c1:tl - c2:tl - c3:tl -> Printf.sprintf "%c%c%c%c" c0 c1 c2 c3

let symbol ss =
  List.partition (fun s ->
    assert (String.length s > 0);
    s.[0] = '\\') ss

(* FIXME: more entry are possible : paragraphs, ...*)
type entry = Caml | CamlStruct | String | Math | Text | Current

type arg_config =
  { entry : entry;
    filter_name : string;
  }

let default_config =
  { entry = Current;
    filter_name = "";
  }

type config = EatR | EatL | Name of string list * string | Syntax of arg_config list


let real_name id cs =
  let rec find_name : config list -> string list * string = function
    | []               -> raise Not_found
    | Name(mp,id) :: _ -> (mp,id)
    | _ :: cs          -> find_name cs
  in
  let (mp, mid) = try find_name cs with Not_found -> ([], id) in
  let _loc = Location.none in
  (* FIXME: this is not exactly what we want *)
  List.fold_left (fun acc m -> <:expr<$uid:m$.($acc$)>>) <:expr<$lid:mid$>> mp

let macro_args id cs =
  let rec find_args : config list -> arg_config list option = function
    | []               -> None
    | Syntax l  :: _   -> Some l
    | _ :: cs          -> find_args cs
  in
  find_args cs

let parser arg_type =
  | "math" -> Math
  | "text" -> Text
  | "caml" -> Caml
  | "struct" -> CamlStruct
  | "string" -> String
  | "current" -> Current

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
    Printf.eprintf "Unknown string filter: %S\n%!" config.filter_name; exit 1

let apply_expr_filter config s _loc =
  try Hashtbl.find expr_filter config.filter_name s _loc with Not_found ->
    Printf.eprintf "Unknown expression filter: %S\n%!" config.filter_name; exit 1

let apply_struct_filter config s _loc =
  try Hashtbl.find struct_filter config.filter_name s _loc with Not_found ->
    Printf.eprintf "Unknown structure filter: %S\n%!" config.filter_name; exit 1

let _ =
  Hashtbl.add string_filter "" (fun s _loc -> <:expr<$string:s$ >>)

let _ =
  Hashtbl.add expr_filter "" (fun e _loc -> e)

let _ =
  Hashtbl.add struct_filter "" (fun s _loc ->
    <:expr<
          [bB (fun env ->
            let module Res =
              struct
                $struct:s$ ;;
              end
             in [ Drawing (Res.drawing ()) ])]>>)

let _ =
  Hashtbl.add string_filter "genumerate" (fun s _loc ->
      let pos = Str.search_forward (Str.regexp "&\\([1iIaA]\\)") s 0 in
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
    Decap.parse_string Pa_ocaml_prelude.expression blank2 caml)

let _ =
  Hashtbl.add struct_filter "diagram" (fun s _loc ->
    <:expr<
          [bB (fun env ->
            let module Res =
              struct
                module EnvDiagram = Env_Diagram (struct let env = env end) ;;
                open EnvDiagram ;;
                $struct:s$ ;;
              end
             in [ Drawing (Res.EnvDiagram.make ()) ])]>>)

let parser config =
  | "eat_right"                           -> EatR
  | "eat_left"                            -> EatL
  | "name" "=" ms:{u:uid - "."}* - id:lid -> Name (ms,id)
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
  delimiter_values      = let _loc = Location.none in <:expr< [] >>;
}

module PMap = PrefixTree

type grammar_state =
  { mutable verbose          : bool
  ; mutable infix_symbols    : infix PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable prefix_symbols   : prefix PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable postfix_symbols  : postfix PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable quantifier_symbols : atom_symbol PrefixTree.t (* key are macro_names or utf8_names mixed *)
  ; mutable atom_symbols     : atom_symbol PrefixTree.t
  ; mutable accent_symbols   : atom_symbol PrefixTree.t
  ; mutable left_delimiter_symbols: delimiter PrefixTree.t
  ; mutable right_delimiter_symbols: delimiter PrefixTree.t
  ; mutable operator_symbols : operator PrefixTree.t
  ; mutable combining_symbols: string PrefixTree.t
  ; mutable reserved_symbols: unit PrefixTree.t
  ; mutable word_macros      : (string * config list) list
  ; mutable math_macros      : (string * config list) list
  ; mutable environment      : (string * config list) list }

let new_grammar str =
  let res = declare_grammar str in set_grammar res (fail ("empty grammar: " ^str)); res

let state =
  { verbose          = false
  ; infix_symbols    = PrefixTree.empty
  ; prefix_symbols    = PrefixTree.empty
  ; postfix_symbols    = PrefixTree.empty
  ; quantifier_symbols    = PrefixTree.empty
  ; atom_symbols     = PrefixTree.empty
  ; accent_symbols     = PrefixTree.empty
  ; left_delimiter_symbols= PrefixTree.empty
  ; right_delimiter_symbols= PrefixTree.empty
  ; operator_symbols= PrefixTree.empty
  ; combining_symbols= PrefixTree.empty
  ; reserved_symbols = PrefixTree.empty
  ; word_macros      = []
  ; math_macros      = []
  ; environment      = [] }

let parser mathlid = id:''[a-z][a-zA-Z0-9']*'' ->
  if PMap.mem id state.reserved_symbols then give_up "symbol";
  id

let empty_state =
  { verbose          = false
  ; infix_symbols    = PrefixTree.empty
  ; prefix_symbols    = PrefixTree.empty
  ; postfix_symbols    = PrefixTree.empty
  ; quantifier_symbols    = PrefixTree.empty
  ; atom_symbols     = PrefixTree.empty
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
let math_atom_symbol = new_grammar "atom_symbol"
let math_quantifier_symbol = new_grammar "quantifier"
let math_accent_symbol = new_grammar "accent"
let math_left_delimiter = new_grammar "left delimiter"
let math_right_delimiter = new_grammar "right delimiter"
let math_combining_symbol = new_grammar "combining"
let math_punctuation_symbol = new_grammar "punctuation"
let math_relation_symbol = new_grammar "relation"

let tree_to_grammar : ?filter:('a -> bool) -> string -> 'a PMap.tree -> 'a grammar =
  fun ?(filter=fun x -> true) name t ->
    let PMap.Node(_,l) = t in
    let fn buf pos =
      let line = Input.line buf in
      let line = String.sub line pos (String.length line - pos) in
      try
	let (n,v) = PMap.longest_prefix ~filter line t in
	(v, buf, pos+n)
      with Not_found -> give_up "Not a valid symbol."
    in
    let charset =
      let f acc (c,_) = Charset.add acc c in
      List.fold_left f Charset.empty_charset l
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
  set_grammar math_accent_symbol (tree_to_grammar "accent_symbol" state.accent_symbols);
  set_grammar math_left_delimiter (tree_to_grammar "left_delimiter_symbol" state.left_delimiter_symbols);
  set_grammar math_right_delimiter (tree_to_grammar "rigt_delimiter_symbol" state.right_delimiter_symbols);
  set_grammar math_combining_symbol (tree_to_grammar "combining_symbol" state.combining_symbols)

let parser all_left_delimiter =
  | math_left_delimiter
  | _:"\\left" math_right_delimiter
  | "\\left." -> invisible_delimiter

let parser all_right_delimiter =
  | math_right_delimiter
  | _:"\\right" math_left_delimiter
  | "\\right." -> invisible_delimiter

let before_parse_hook () =
  In.before_parse_hook ();
  let path = "." :: "_patobuild" :: !Config2.grammarspath in
  let add_grammar g =
    if !no_default_grammar && g = "DefaultGrammar" then () else
    let g = findPath (g ^ ".tgy") path in
    Printf.eprintf "Reading grammar %s\n%!" g;
    let ch = open_in_bin g in
    let st = input_value ch in
    merge_states state st;
    close_in ch;
    Printf.eprintf "Done with grammar %s\n%!" g
  in
  List.iter add_grammar !patoline_grammar;
  build_grammar ()

let symbol_paragraph _loc syms names =
  <:struct<
    let _ = newPar D.structure
      ~environment:(fun x -> {x with par_indent = []})

      Complete.normal Patoline_Format.parameters
      [bB (fun env0 -> Maths.kdraw
        [ { env0 with mathStyle = Mathematical.Display } ] [
        Maths.bin 0 (Maths.Normal(false,Maths.noad (Maths.glyphs "⇐"),false))
        $syms$ $names$
      ])]
  >>

let math_list _loc l =
  let merge x y =
    <:expr<[Maths.bin 0
      (Maths.Normal(false,Maths.noad (Maths.glyphs ","),false))
      $x$ $y$]>>
  in
  List.fold_left merge (List.hd l) (List.tl l)

let dollar        = Pa_lexing.single_char '$'

let no_brace =
  Decap.test ~name:"no_brace" Charset.full_charset (fun buf pos ->
    let c,buf,pos = Input.read buf pos in
    if c <> '{' then ((), true) else ((), false))

(****************************************************************************
 * Parsing of macro arguments.                                              *
 ****************************************************************************)

let parser br_string =
  | EMPTY -> ""
  | s1:br_string s2:''[^{}]*'' -> s1^s2
  | s1:br_string '{' s2:br_string '}' -> s1 ^ "{" ^ s2 ^ "}"
  | s1:br_string '\n' -> s1 ^ "\n"

(* bool param -> can contain special text environments //...// **...** ... *)
let paragraph_basic_text, set_paragraph_basic_text = grammar_family "paragraph_basic_text"
let math_toplevel = declare_grammar "math_toplevel"

let parser macro_argument config =
  | '{' m:(math_toplevel) '}' when config.entry = Math
			      -> apply_expr_filter config m _loc
  | '{' e:(change_layout (paragraph_basic_text TagSet.empty) blank1) '}' when config.entry = Text
			      -> apply_expr_filter config e _loc
  | e:wrapped_caml_expr when config.entry <> CamlStruct
			      -> apply_expr_filter config e _loc
  | e:wrapped_caml_array when config.entry <> CamlStruct
                              -> apply_expr_filter config <:expr<$array:e$>> _loc
  | e:wrapped_caml_list when config.entry <> CamlStruct
                              -> apply_expr_filter config <:expr<$list:e$>> _loc
  | s:wrapped_caml_structure when config.entry = CamlStruct
			      -> apply_struct_filter config s _loc
  | '{' e:caml_expr '}' when config.entry = Caml
			      -> apply_expr_filter config e _loc
  | '{' s:caml_structure '}' when config.entry = CamlStruct
			      -> apply_struct_filter config s _loc
  | '{' s:(change_layout br_string no_blank) '}' when config.entry = String
			      -> apply_string_filter config s _loc

let parser simple_text_macro_argument =
  | '{' l:(change_layout (paragraph_basic_text TagSet.empty) blank1) '}' -> l
  | e:wrapped_caml_expr  -> e
  | e:wrapped_caml_array -> <:expr<$array:e$>>
  | e:wrapped_caml_list  -> <:expr<$list:e$>>

let parser simple_math_macro_argument =
  | '{' m:(change_layout math_toplevel blank2) '}' -> m
  | e:wrapped_caml_expr  -> e
  | e:wrapped_caml_array -> <:expr<$array:e$>>
  | e:wrapped_caml_list  -> <:expr<$list:e$>>

let parser macro_arguments_aux l =
  | EMPTY when l = [] -> []
  | arg:(macro_argument (List.hd l)) args:(macro_arguments_aux (List.tl l)) when l <> [] -> arg::args

let macro_arguments id current config =
  match macro_args id config, current with
  | None, Math   -> (parser simple_math_macro_argument*#)
  | None, Text   -> (parser simple_text_macro_argument*#)
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
      SimpleSym s -> <:expr<Maths.glyphs $string:s$>>, false
    | CamlSym s   -> s, false
    | MultiSym s  -> s, true
    | Invisible   -> <:expr<Maths.glyphs "invisible">>, false
    | _ -> failwith "a faire ds Pa_patoline.print_math_symbol.\n"
  in
  if b then
    if !cache = "" then s else (* FIXME: not very clean *)
    try
      let nom = "m" ^ (!cache) in
      let index = Hashtbl.find hash_msym s in
      <:expr< ! $lid:nom$.($int:index$) >>
    with Not_found ->
      Hashtbl.add  hash_msym s !count_msym;
      mcache_buf := s::!mcache_buf;
      let res = <:expr< ! $lid:("m" ^ !cache)$.($int:(!count_msym)$) >> in
      let _ = incr count_msym in
      res
  else
    if !cache = "" then s else (* FIXME: not very clean *)
    try
      let r = Hashtbl.find hash_sym s in
      <:expr< ! $lid:(!cache)$.($int:r$) >>
    with Not_found ->
      Hashtbl.add  hash_sym s !count_sym;
      cache_buf := s::!cache_buf;
      let res = <:expr< ! $lid:(!cache)$.($int:(!count_sym)$) >> in
      let _ = incr count_sym in
      res

let print_math_deco_sym _loc elt ind =
  if ind = no_ind then (
    <:expr< Maths.noad $print_math_symbol _loc elt$ >>
  ) else
    begin
      let r = ref [] in
      (match ind.up_right with
	Some i ->
     	  if ind.up_right_same_script then
	    r:= <:record<Maths.super_right_same_script = true>> @ !r;
	  r:= <:record<Maths.superscript_right = $i$ >> @ !r
      | _ -> ());
      (match ind.down_right with
	Some i ->
	  r:= <:record<Maths.subscript_right = $i$ >> @ !r
      | _ -> ());
      (match ind.up_left with
	Some i ->
     	  if ind.up_left_same_script then
	    r:= <:record<Maths.super_left_same_script = true>> @ !r;
	  r:= <:record<Maths.superscript_left = $i$ >> @ !r
      | _ -> ());
      (match ind.down_left with
	Some i ->
	  r:= <:record<Maths.subscript_left = $i$ >> @ !r
      | _ -> ());
      Pa_ast.loc_expr _loc (Parsetree.Pexp_record (!r, Some <:expr<Maths.noad $print_math_symbol _loc elt$>>))
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
	           r:= <:record<Maths.super_right_same_script = true>> @ !r;
	         r := <:record<Maths.superscript_right = $i$ >> @ !r
       | _ -> ());
      (match ind.down_right with
       | Some i ->
           r:= <:record<Maths.subscript_right = $i$ >> @ !r
       | _ -> ());
      (match ind.up_left with
       | Some i ->
           if ind.up_left_same_script then
	           r:= <:record<Maths.super_left_same_script = true>> @ !r;
           r:= <:record<Maths.superscript_left = $i$ >> @ !r
       | _ -> ());
      (match ind.down_left with
       | Some i -> r:= <:record<Maths.subscript_left = $i$ >> @ !r
       | _ -> ());
      <:expr<
       [Maths.Ordinary $Pa_ast.loc_expr _loc (Parsetree.Pexp_record (!r, Some <:expr< Maths.noad (fun env st -> Maths.draw [env] $elt$)>>))$]>>
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
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc infix_value$)]>> in
    let showuname u =
      sym (* TODO *)
    in
    let showmname m =
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (SimpleSym m)$)]>>
    in
    let unames = List.map showuname infix_utf8_names in
    let mnames = List.map showmname infix_macro_names in
    symbol_paragraph _loc sym (math_list _loc (unames @ mnames))
(*
    let sym_val = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc infix_value$)]>> in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
    in
    let names = List.map sym infix_macro_names @ List.map (fun _ -> sym_val) infix_utf8_names in
    symbol_paragraph _loc sym_val (math_list _loc names)
*)
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
    let sym_val = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc symbol_value$)]>> in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
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
    let sym_val = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc symbol_value$)]>> in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
    in
    let names = List.map sym symbol_macro_names @ List.map (fun _ -> sym_val) symbol_utf8_names in
    symbol_paragraph _loc sym_val (math_list _loc names)
  else []

(* FIXME: |- A and -x should have distinct priority and spacing *)
let new_prefix_symbol _loc sym_names prefix_value =
  let prefix_macro_names, prefix_utf8_names = symbol sym_names in
  let sym = { prefix_prio = IProd; prefix_space = 3; prefix_no_space = false; prefix_macro_names; prefix_utf8_names; prefix_value } in
  quail_out prefix_macro_names prefix_utf8_names;
  let insert map name = PrefixTree.add name sym map in
  state.prefix_symbols <- List.fold_left insert state.prefix_symbols sym_names;
  local_state.prefix_symbols <- List.fold_left insert local_state.prefix_symbols sym_names;
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc prefix_value$)]>> in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
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
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc postfix_value$)]>> in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
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
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let sym_val = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc symbol_value$)]>> in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
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
      <:expr<[Maths.Ordinary (Maths.noad
        (fun x y -> List.flatten (Maths.multi_glyphs $delimiter_values$ x y)))]>>
    in
    let sym_val =
      <:expr<[Maths.Ordinary (Maths.noad
        (fun x y -> List.hd $delimiter_values$ x y))]>>
    in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
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
      <:expr<[Maths.Ordinary (Maths.noad
        (fun x y -> List.flatten (Maths.multi_glyphs $delimiter_values$ x y)))]>>
    in
    let sym_val =
      <:expr<[Maths.Ordinary (Maths.noad
        (fun x y -> List.hd $delimiter_values$ x y))]>>
    in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
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
  add_reserved sym_names;
  (* Displaying no the document. *)
  if state.verbose then
    let syms =
      <:expr<[Maths.Ordinary (Maths.noad
        (fun x y -> List.flatten (Maths.multi_glyphs $operator_values$ x y)))]>>
    in
    let sym_val =
      <:expr<[Maths.Ordinary (Maths.noad
        (fun x y -> List.hd $operator_values$ x y))]>>
    in
    let sym s =
      let s = <:expr<Maths.glyphs $string:s$>> in
      <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc (CamlSym s)$)]>>
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
      <:expr<[Maths.Ordinary (Maths.noad (Maths.glyphs $string:uchr$))]>>
    in
    let macro = "\\" ^ macro in
    let macro =
      <:expr<[Maths.Ordinary (Maths.noad (Maths.glyphs $string:macro$))]>>
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

let parser any_symbol =
  | sym:                        math_quantifier_symbol              -> sym.symbol_value
  | sym:(alternatives (List.map math_infix_symbol      math_prios)) -> sym.infix_value
  | sym:(alternatives (List.map math_prefix_symbol     math_prios)) -> sym.prefix_value
  | sym:(alternatives (List.map math_postfix_symbol    math_prios)) -> sym.postfix_value

let merge_indices indices ind =
  assert(ind.down_left = None);
  assert(ind.up_left = None);
  if (indices.down_right <> None && ind.down_right <> None) ||
     (indices.up_right <> None && ind.up_right <> None) then give_up "doubles indices";
  { indices with
    down_right = if ind.down_right <> None then ind.down_right else indices.down_right;
    up_right = if ind.up_right <> None then ind.up_right else indices.up_right}

let parser math_aux prio =
  | m:(math_aux (next_prio prio)) when prio <> AtomM -> m

  | sym:(math_prefix_symbol prio) ind:with_indices m:(math_aux prio) ->
     (fun indices ->
       let indices = merge_indices indices ind in
       let psp = sym.prefix_space in
       let pnsp = sym.prefix_no_space in
       let md = print_math_deco_sym _loc_sym sym.prefix_value indices in
       <:expr<[Maths.bin $int:psp$ (Maths.Normal(true,$md$,$bool:pnsp$)) [] $m no_ind$]>>
     )

  | sym:math_quantifier_symbol ind:with_indices d:math_declaration p:math_punctuation_symbol? m:(math_aux prio) when prio = Operator ->
    (fun indices ->
      let indices = merge_indices indices ind in
      let inter =
        match p with
        | None   -> <:expr<Maths.Invisible>>
        | Some s ->
            let nsl = s.infix_no_left_space in
            let nsr = s.infix_no_right_space in
            let md  = print_math_deco_sym _loc_p s.infix_value no_ind in
            <:expr<Maths.Normal($bool:nsl$, $md$, $bool:nsr$)>>
      in
      let md = print_math_deco_sym _loc_sym sym.symbol_value indices in
      <:expr<[Maths.bin 3 (Maths.Normal(true,$md$,true)) []
                    [Maths.bin 1 $inter$ $d$ $m no_ind$]]>>)

  | op:(math_operator prio) ind:with_indices m:(math_aux prio) ->
     (fun indices ->
       let ind = merge_indices indices ind in
      match op.operator_kind with
	Limits ->
	  <:expr<[Maths.op_limits [] $print_math_deco_sym _loc_op (MultiSym op.operator_values) ind$ $m no_ind$]>>
      | NoLimits ->
	 <:expr<[Maths.op_nolimits [] $print_math_deco_sym _loc_op (MultiSym op.operator_values) ind$ $m no_ind$]>>)

  | m:(math_aux prio) sym:(math_postfix_symbol prio) ->
      (fun indices ->
        let psp = sym.postfix_space in
        let nsp = sym.postfix_no_space in
        let md  = print_math_deco_sym _loc_sym sym.postfix_value indices in
        let m = m no_ind in
        <:expr<[Maths.bin $int:psp$ (Maths.Normal($bool:nsp$,$md$,true)) $m$ []] >>)

  | l:(math_aux prio) st:{ s:(math_infix_symbol prio) i:with_indices -> (s,i)
			 | s:(empty invisible_product) when prio = IProd -> (s,no_ind) }
    r:(math_aux (next_prio prio)) when prio <> AtomM ->
     let s,ind = st in
     (fun indices ->
         let indices = merge_indices indices ind in
	 let nsl = s.infix_no_left_space in
	 let nsr = s.infix_no_right_space in
	 let sp = s.infix_space in
	 let l = l no_ind and r = r (if s.infix_value = Invisible then indices else no_ind) in
	 if s.infix_value = SimpleSym "over" then begin
	   if indices <> no_ind then give_up "indices on fraction";
	   <:expr< [Maths.fraction $l$ $r$] >>
	 end else begin
	   let inter =
	     if s.infix_value = Invisible then
	       <:expr<Maths.Invisible>>
	     else
         let v = print_math_deco_sym _loc_st s.infix_value indices in
	       <:expr<Maths.Normal ($bool:nsl$, $v$, $bool:nsr$)>>
	   in
	   <:expr<[Maths.Binary { bin_priority= $int:sp$ ; bin_drawing = $inter$
                          ; bin_left = $l$ ; bin_right= $r$ }]>>
	 end)

  (* Les règles commençant avec un { forment un conflict avec les arguments
   des macros. Je pense que c'est l'origine de nos problèmes de complexité. *)
  | '{' m:(math_aux Punc) '}' when prio = AtomM -> m
  | '{' s:any_symbol ind:with_indices '}' when prio = AtomM ->
      if s = Invisible then give_up "";
      let f indices =
        let indices = merge_indices indices ind in
        let md = print_math_deco_sym _loc_s s indices in
        <:expr<[Maths.Ordinary $md$]>>
      in f

  | l:all_left_delimiter m:(math_aux Punc) r:all_right_delimiter  when prio = AtomM ->
     (fun indices ->
       let l = print_math_symbol _loc_l (MultiSym l.delimiter_values) in
       let r = print_math_symbol _loc_r (MultiSym r.delimiter_values) in
       print_math_deco _loc <:expr<[Maths.Decoration ((Maths.open_close $l$ $r$), $m no_ind$)]>> indices)

  | name:''[a-zA-Z][a-zA-Z0-9]*'' when prio = AtomM ->
     (fun indices ->
       if String.length name > 1 then
	 let elt = <:expr<fun env -> Maths.glyphs $string:name$ (Maths.change_fonts env env.font)>> in
	 <:expr<[Maths.Ordinary $print_math_deco_sym _loc_name (CamlSym elt) indices$] >>
       else
	 <:expr<[Maths.Ordinary $print_math_deco_sym _loc_name (SimpleSym name) indices$] >>)

  | sym:math_atom_symbol  when prio = AtomM ->
      (fun indices ->
	<:expr<[Maths.Ordinary $print_math_deco_sym _loc_sym sym.symbol_value indices$] >>)

  | num:''[0-9]+\([.][0-9]+\)?''  when prio = AtomM ->
     (fun indices ->
       <:expr<[Maths.Ordinary $print_math_deco_sym _loc_num (SimpleSym num) indices$] >>)

  (* macro non déclarée *)
  | '\\' id:mathlid when prio = AtomM ->>
     let config = try List.assoc id state.math_macros with Not_found -> [] in
     args:(macro_arguments id Math config) ->
     (fun indices ->
       let m = real_name id config in
       (* TODO special macro properties to be handled. *)
       let apply acc arg = <:expr<$acc$ $arg$>> in
       let e = List.fold_left apply <:expr<$m$>> args in
       print_math_deco _loc_id e indices
     )
  | m:(math_aux Accent) sym:math_combining_symbol when prio = Accent ->
    (fun indices -> <:expr<$lid:sym$ $m indices$>>)

  | m:(math_aux Accent) s:math_accent_symbol when prio = Accent ->
    let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s s.symbol_value no_ind$] >> in
    let rd indices =
      if indices.up_right <> None then give_up "double indices";
      { indices with up_right = Some s; up_right_same_script = true }
    in
    (fun indices -> m (rd indices))

  | m:(math_aux Ind) s:Subsup.subscript when prio = Ind ->
    let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s (SimpleSym s) no_ind$] >> in
    let rd indices =
      if indices.down_right <> None then give_up "double indices";
      { indices with down_right = Some s }
    in
    (fun indices -> m (rd indices))

  | m:(math_aux Ind) s:Subsup.superscript when prio = Ind ->
    let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s (SimpleSym s) no_ind$] >> in
    let rd indices =
      if indices.up_right <> None then give_up "double indices";
      { indices with up_right = Some s }
    in
    (fun indices -> m (rd indices))

  | m:(math_aux Ind) - h:right_indices - r:(math_aux Accent) when prio = Ind ->
     (fun indices -> match h with
     | Down ->
 	if indices.down_right <> None then give_up "double indices";
        m { indices with down_right = Some (r no_ind) }
     | Up ->
	if indices.up_right <> None then give_up "double indices";
	m { indices with up_right = Some (r no_ind) }
     )

  | m:(math_aux Ind) - h:right_indices - s:any_symbol when prio = Ind ->
     let s = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc s$) ]>> in
     (fun indices -> match h with
     | Down ->
 	if indices.down_right <> None then give_up "double indices";
        m { indices with down_right = Some s }
     | Up ->
	if indices.up_right <> None then give_up "double indices";
	m { indices with up_right = Some s }
     )

  | m:(math_aux Accent) - h:left_indices - r:(math_aux LInd) when prio = LInd ->
     (fun indices -> match h with
     | Down ->
	if indices.down_left <> None then give_up "double indices";
        r { indices with down_left = Some (m no_ind) }
     | Up ->
	if indices.up_left <> None then give_up "double indices";
        r { indices with up_left = Some (m no_ind) }
     )

  | s:any_symbol - h:left_indices - r:(math_aux LInd) when prio = LInd ->
     let s = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc s$) ]>> in
     (fun indices -> match h with
     | Down ->
	if indices.down_left <> None then give_up "double indices";
        r { indices with down_left = Some s }
     | Up ->
	if indices.up_left <> None then give_up "double indices";
        r { indices with up_left = Some s }
     )

and with_indices =
  | EMPTY -> no_ind

  | i:with_indices h:right_indices - r:(math_aux Accent) ->
     begin
       match h with
       | Down -> if i.down_right <> None then give_up "double indices";
	               { i with down_right = Some (r no_ind) }
       | Up   -> if i.up_right <> None then give_up "double indices";
	               { i with up_right = Some (r no_ind) }
     end

  | i:with_indices s:Subsup.superscript ->
      let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s (SimpleSym s) no_ind$] >> in
      if i.up_right <> None then give_up "double indices";
      { i with up_right = Some s }

  | i:with_indices s:Subsup.subscript ->
      let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s (SimpleSym s) no_ind$] >> in
      if i.down_right <> None then give_up "double indices";
      { i with down_right = Some s }

(*  | (m,mp):math_aux - (s,h):indices - (o,i):math_operator ->
     (* FIXME TODO: decap bug: this loops ! *)
     (* Anyway, it is a bad way to write the grammar ... a feature od decap ? *)
     if (mp >= Ind && s = Left) then give_up "can not be used as indice";
     if (s = Right) then give_up "No indice/exponent allowed here";
     let i = match h with
       | Down ->
	  if i.down_left <> None then give_up "double indices";
	 { i with down_left = Some (m no_ind) }
       | Up ->
	  if i.up_left <> None then give_up "double indices";
	 { i with up_left = Some (m no_ind) }
     in
    (o, i)*)

and math_punc_list =
  | m:(math_aux Ind) -> m no_ind
  | l:math_punc_list s:math_punctuation_symbol m:(math_aux Ind) ->
    let nsl = s.infix_no_left_space in
    let nsr = s.infix_no_right_space in
    let r = m no_ind in
    let inter =
      <:expr<
                         Maths.Normal( $bool:nsl$,
                           $print_math_deco_sym _loc_s s.infix_value no_ind$,
                           $bool:nsr$) >>
    in
    <:expr<[Maths.bin 3 $inter$ $l$ $r$]>>

and long_math_declaration =
  | m:math_punc_list -> m
  | l:long_math_declaration s:math_relation_symbol ind:with_indices r:math_punc_list ->
    let nsl = s.infix_no_left_space in
    let nsr = s.infix_no_right_space in
    let inter =
      <:expr<Maths.Normal( $bool:nsl$,
                           $print_math_deco_sym _loc_s s.infix_value ind$,
                           $bool:nsr$) >>
    in
    <:expr<[Maths.bin 2 $inter$ $l$ $r$] >>

and math_declaration =
    | '{' m:long_math_declaration '}' -> m
    | no_brace m:(math_aux Ind) -> m no_ind
    | no_brace m:(math_aux Ind) s:math_relation_symbol ind:with_indices r:math_punc_list ->
       let nsl = s.infix_no_left_space in
       let nsr = s.infix_no_right_space in
       let inter =
	 <:expr<Maths.Normal( $bool:nsl$,
                              $print_math_deco_sym _loc_s s.infix_value ind$,
                              $bool:nsr$) >>
       in
       <:expr<[Maths.bin 2 $inter$ $m no_ind$ $r$] >>


let _ = set_grammar math_toplevel (parser
  | m:(math_aux Punc) -> m no_ind
  | s:any_symbol i:with_indices ->
      if s = Invisible then give_up "...";
      <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s s i$]>>)


(****************************************************************************
 * Text content of paragraphs and macros (mutually recursive).              *
 ****************************************************************************)


(***** Patoline macros  *****)

  let reserved_macro =
    [ "begin"; "end"; "item"; "verb" ]

  let macro_name = change_layout (
    parser "\\" - m:lid ->
      if List.mem m reserved_macro then
        give_up (m ^ " is a reserved macro"); m
    ) no_blank

  let parser macro =
    | id:macro_name ->>
       let config = try List.assoc id state.word_macros with Not_found -> [] in
       args:(macro_arguments id Text config) ->
       (let fn = fun acc r -> <:expr<$acc$ $r$>> in
        List.fold_left fn <:expr<$lid:id$>> args)
    | m:verbatim_macro -> m

(****************************)

  let parser text_paragraph_elt (tags:TagSet.t) =
    | m:macro -> m

    | "//" - p:(paragraph_basic_text (addTag Italic tags)) - "//" when allowed Italic tags ->
         <:expr<toggleItalic $p$>>
    | "**" - p:(paragraph_basic_text (addTag Bold tags)) - "**" when allowed Bold tags ->
         <:expr<bold $p$>>
    | "||" - p:(paragraph_basic_text (addTag SmallCap tags)) - "||" when allowed SmallCap tags ->
         <:expr<sc $p$>>
(*    | "__" - p:(paragraph_basic_text (addTag Underline tags)) - "__" when allowed Underline tags ->
         <:expr@_loc_p<underline $p$>>
    | "--" - p:(paragraph_basic_text (addTag Strike tags)) - "--" when allowed Strike tags ->
      <:expr@_loc_p<strike $p$>>*)

    | v:verbatim_bquote -> <:expr<$v$>>
    | v:verbatim_sharp  -> <:expr<$v$>>

    | '"' p:(paragraph_basic_text (addTag Quote tags)) '"' when allowed Quote tags ->
        (let opening = "``" in (* TODO addapt with the current language*)
         let closing = "''" in (* TODO addapt with the current language*)
         <:expr<tT $string:opening$ :: $p$ @ [tT $string:closing$]>>)

    | dollar m:math_toplevel dollar ->
        <:expr<[bB (fun env0 -> Maths.kdraw
                        [ { env0 with mathStyle = env0.mathStyle } ]
                          $m$)]>>
    | "\\(" m:math_toplevel "\\)" ->
        <:expr<[bB (fun env0 -> Maths.kdraw
                        [ { env0 with mathStyle = env0.mathStyle } ]
                        (displayStyle $m$))]>>

    | ws:word+# ->
       <:expr<[tT $string:String.concat " " ws$]>>

    | '{' p:(paragraph_basic_text TagSet.empty) '}' -> p

  let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
    let x,y = Lexing.((end_pos _loc_p1).pos_cnum, (start_pos _loc_p2).pos_cnum) in
    (*Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
    let _loc = _loc_p2 in
    let bl e = if y - x >= 1 then <:expr<tT" "::$e$>> else e in
    let _loc = Pa_ast.merge2 _loc_p1 _loc_p2 in
    <:expr<$p1$ @ $bl p2$>>

  let _ = set_paragraph_basic_text (fun tags ->
             parser
               l:{p:(text_paragraph_elt tags) -> (_loc, p)}+ ->
                 match List.rev l with
                 | []   -> assert false
                 | m::l ->
                    let fn = fun (_loc_m, m) (_loc_p, p) ->
                      (Pa_ast.merge2 _loc_p _loc_m
                      , concat_paragraph p _loc_p m _loc_m)
                    in snd (List.fold_left fn m l))

  let text_only = change_layout (paragraph_basic_text TagSet.empty) blank1

  let oparagraph_basic_text = paragraph_basic_text
  let parser paragraph_basic_text =
      p:(oparagraph_basic_text TagSet.empty) ->
        (fun indented ->
         if indented then
           <:struct<
             let _ = newPar D.structure
                            Complete.normal Patoline_Format.parameters $p$ >>
         else
           <:struct<
             let _ = newPar D.structure
                            ~environment:(fun x -> { x with par_indent = [] })
                            Complete.normal Patoline_Format.parameters $p$>>
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
    | "\\Caml" s:(change_layout wrapped_caml_structure blank2) -> (fun _ -> s)
    | "\\Title" t:simple_text_macro_argument -> (fun _ ->
         let m1 = freshUid () in
         let m2 = freshUid () in
         <:struct<
           module $uid:m1$ =
             struct
               let arg1 = $t$
             end
           module $uid:m2$ = Title($uid:m1$)
           let _ = $uid:m2$.do_begin_env ()
           let _ = $uid:m2$.do_end_env ()
         >>)

    | "\\Include" '{' id:uid '}' -> (fun _ ->
         incr nb_includes;
         let temp_id = Printf.sprintf "TEMP%d" !nb_includes in
         <:struct< module $uid:temp_id$ =$uid:id$.Document(Patoline_Output)(D)
                   open $uid:temp_id$>>)
    | "\\TableOfContents" -> (fun _ ->
         let m = freshUid () in
         <:struct<
           module $uid:m$ = TableOfContents
           let _ = $uid:m$.do_begin_env ()
           let _ = $uid:m$.do_end_env ()
         >>)
    | "\\item" -> (fun _ ->
         let m1 = freshUid () in
         let m2 = freshUid () in
         let _Item = "Item" in
         <:struct< module $uid:m1$ =
                     struct
                       module $uid:m2$ = $uid:_Item$
                       let _ = $uid:m2$.do_begin_env ()
                       let _ = $uid:m2$.do_end_env ()
                     end>>)
    | "\\begin{" idb:lid '}' ->>
       let config = try List.assoc idb state.environment with Not_found -> [] in
        args:(macro_arguments idb Text config)
        ps:(change_layout paragraphs blank2)
       "\\end{" ide:lid '}' ->
         (fun indent_first ->
           if idb <> ide then give_up "Non-matching begin / end";
           let m1 = freshUid () in
           let m2 = freshUid () in
           let arg =
             if args = [] then <:struct<>> else
               let gen i e =
                 let id = Printf.sprintf "arg%i" (i+1) in
                 <:struct<let $lid:id$ = $e$>>
               in
               let args = List.mapi gen args in
               let args = List.fold_left (@) [] args in
               <:struct<
                 module $uid:"Arg_"^m2$ =
                   struct
                     $struct:args$
                   end
               >>
           in
           let def =
             let name = "Env_" ^ idb in
             let argname = "Arg_" ^ m2 in
             if args = [] then
               <:struct<module $uid:m2$ = $uid:name$>>
             else
               <:struct<module $uid:m2$ = $uid:name$($uid:argname$)>>
           in
           <:struct< module $uid:m1$ =
                       struct
                         $struct:arg$
                         $struct:def$
                         open $uid:m2$
                         let _ = $uid:m2$ . do_begin_env ()
                         $struct:ps indent_first$
                         let _ = $uid:m2$ . do_end_env ()
                        end>>)
    | m:{ "\\[" m:math_toplevel "\\]" | "$$" m:math_toplevel "$$" } ->
         (fun _ ->
           <:struct<let _ = newPar D.structure
                        ~environment:(fun x -> {x with par_indent = []})
                        Complete.normal displayedFormula
                        [bB (fun env0 -> Maths.kdraw
                          [ { env0 with mathStyle = Mathematical.Display } ]
                          $m$)]>>)
    | l:paragraph_basic_text -> l
    | s:symbol_def -> fun _ -> s

  let _ = set_grammar paragraph (change_layout paragraph_elt blank1)

  let _ = set_grammar paragraphs (
                        parser
                          p:paragraph ps:paragraph* ->
                                               let ps = List.flatten (List.map (fun r -> r true) ps) in
                                         fun indent_first -> p indent_first @ ps
                      )

(****************************************************************************
 * Sections, layout of the document.                                        *
 ****************************************************************************)

  let text = declare_grammar "text"

  let section = "\\(===?=?=?=?=?=?=?\\)\\|\\(---?-?-?-?-?-?-?\\)"
  let op_section = "[-=]>"
  let cl_section = "[-=]<"

  let parser text_item =
      op:RE(op_section) title:text_only txt:text cl:RE(cl_section) ->
        (fun _ lvl ->
         let numbered = match op.[0], cl.[0] with
             '=', '=' -> <:expr<newStruct>>
           | '-', '-' -> <:expr<newStruct ~numbered:false>>
           | _ -> give_up "Non-matching relative section markers"
         in
         true, lvl, <:struct< let _ = $numbered$ D.structure $title$
                              $struct:txt false (lvl+1)$
                              let _ = go_up D.structure >>)

    | op:RE(section) title:text_only cl:RE(section) txt:text ->
        (fun _ lvl ->
         if String.length op <> String.length cl then
	   give_up "Non-matching absolute section marker";
         let numbered = match op.[0], cl.[0] with
             '=', '=' -> <:expr<newStruct>>
           | '-', '-' -> <:expr<newStruct ~numbered:false>>
           | _ -> give_up "Non-mathing section marker"
         in
         let l = String.length op - 1 in
         if l > lvl + 1 then failwith "Illegal level skip";
         let res = ref [] in
         for i = 0 to lvl - l do
           res := !res @ <:struct<let _ = go_up D.structure>>
         done;
         true, lvl, <:struct< $struct:!res$
                              let _ = ($numbered$) D.structure $title$
                              $struct:txt false l$>>)

    | ps:paragraph ->
         (fun indent lvl -> indent, lvl, ps indent)

  let _ = set_grammar text (
     parser
       l:text_item* ->
         (fun indent lvl ->
          let fn = fun (indent, lvl, ast) txt ->
            let indent, lvl, ast' = txt indent lvl in
            indent, lvl, (ast @ ast')
          in
          let _,_,r = List.fold_left fn (indent, lvl, []) l in
          r)
      )

  let otext = text
  let parser text = txt:otext -> txt true 0




(* Header, title, main Patoline entry point *********************************)

let patoline_config : unit grammar =
  change_layout (
    parser
    | "#FORMAT " f:uid -> set_patoline_driver f
    | "#DRIVER " d:uid -> set_patoline_driver d
    | "#PACKAGES " ps:''[,a-zA-Z]+'' ->
        add_patoline_packages ps
    | "#GRAMMAR " g:''[a-zA-Z]+''    -> add_patoline_grammar g
  ) no_blank

let parser header = _:patoline_config*

let parser title =
  | RE("==========\\(=*\\)")
      title:text_only
      (auth,inst,date):{
	auth:{_:RE("----------\\(-*\\)") text_only}
	(inst,date):{
	  inst:{_:RE("----------\\(-*\\)") text_only}
	  date:{_:RE("----------\\(-*\\)") text_only}? -> (Some inst, date)
	}?[None,None] -> (Some auth, inst, date)
      }?[None,None,None]
    RE("==========\\(=*\\)") ->

      let date =
        match date with
        | None   -> <:expr<[]>>
        | Some t -> <:expr<["Date", string_of_contents $t$]>>
      in
      let inst =
        match inst with
        | None   -> <:expr<[]>>
        | Some t -> <:expr<["Institute", string_of_contents $t$]>>
      in
      let auth =
        match auth with
        | None   -> <:expr<[]>>
        | Some t -> <:expr<["Author", string_of_contents $t$]>>
      in
      <:struct<
        let _ = Patoline_Format.title D.structure
                  ~extra_tags:($auth$ @ $inst$ @ $date$) $title$
      >>

let wrap basename _loc ast =
  <:struct<
    open Typography
    open Util
    open Typography.Box
    open Typography.Config
    open Typography.Document
    open Typography.Maths
    open RawContent
    open Color
    open Driver
    open DefaultFormat.MathsFormat
    let $lid:("cache_"^basename)$ =
      ref ([||] : (environment -> Mathematical.style -> box list) array)
    let $lid:("mcache_"^basename)$ =
      ref ([||] : (environment -> Mathematical.style -> box list) list array)
    module Document = functor(Patoline_Output:DefaultFormat.Output)
      -> functor(D:DocumentStructure)->struct
    module Patoline_Format = $uid:!patoline_format$ .Format(D)
    open $uid:!patoline_format$
    open Patoline_Format
    let temp1 = List.map fst (snd !D.structure)
    $struct:ast$
    let _ = D.structure:=follow (top !D.structure) (List.rev temp1)
    end
    let _ = $lid:("cache_"^basename)$  := $array:(List.rev !cache_buf)$
    let _ = $lid:("mcache_"^basename)$ := $array:(List.rev !mcache_buf)$
  >>

let parser init =
  EMPTY -> (fun () ->
    let file = match !file with
                 | None -> ""
                 | Some f -> f
    in
    let basename = chop_extension' (Filename.basename file) in
    cache := "cache_" ^ basename;
    basename)

let parser full_text =
  | h:header basename:{basename:init -> basename ()} t:{tx1:text t:title}? tx2:text EOF ->
     begin
       let t = match t with
         | None   -> []
         | Some (tx1,t) -> t @ tx1
       in
       let ast = t @ tx2 in
       wrap basename _loc ast
     end

(* Extension of Ocaml's grammar *********************************************)

let parser directive =
  | '#' n:uid a:uid ->
    ((match n with
       | "FORMAT"  -> patoline_format := a
       | "DRIVER"  -> patoline_driver := a
       | "PACKAGE" -> patoline_packages := a :: !patoline_packages
       | _ -> give_up ("Unknown directive #"^n));
    [])
let extra_structure = directive :: extra_structure

let parser patoline_quotations _ =
  | "<<" par:text_only     ">>" -> par
  | "<$" mat:math_toplevel "$>" -> mat

let _ = List.iter Pa_lexing.add_reserved_symb ["<<"; ">>"; "<$"; "$>"]
let extra_expressions = patoline_quotations :: extra_expressions

(* Entry points and extension creation **************************************)

(* Adding the new entry points *)

let entry_points =
    (".txp", Implementation (full_text, blank2)) ::
    (".typ", Implementation (full_text, blank2)) ::
    (".mlp", Implementation (structure, Pa_lexing.ocaml_blank)) ::
    entry_points

end (* of the functor *)

(* Creating and running the extension *)
let _ =
  try
    let _ = Printexc.record_backtrace true in
    let module ParserExt = Pa_parser.Ext(Pa_ocaml_prelude.Initial) in
    let module PaExt = Ext(ParserExt) in
    let module PatolineDefault = Pa_ocaml.Make(PaExt) in
    let module M = Pa_main.Start(PatolineDefault) in
    let open PaExt in
    match !Pa_ocaml_prelude.file, !in_ocamldep, local_state = empty_state with
    | Some s, false, false ->
       let dir = Filename.dirname s in
       let name = Filename.basename s in
       let name = chop_extension' name ^ ".tgy" in
       let patobuild_dir = Filename.concat dir "_patobuild" in
       let name = Filename.concat patobuild_dir name in
       Printf.eprintf "Writing grammar %s\n%!" name;
       (* Check if _patobuild/ needs to be created *)
       if not (Sys.file_exists patobuild_dir)
       then Unix.mkdir patobuild_dir 0o700;
       (* Now write the grammar *)
       let ch = open_out_bin name in
       output_value ch local_state;
       close_out ch;
       Printf.eprintf "Written grammar %s\n%!" name
    | _ -> ()

  with e ->
    Printf.eprintf "Exception: %s\nTrace:\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1
