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

let set_patoline_format f =
  patoline_format := f

let set_patoline_driver d =
  patoline_driver := d

let add_patoline_package p =
  let ps = Util.split ',' p in
  patoline_packages := !patoline_packages @ ps

let no_default_grammar = ref false

let spec =
  [ ("--driver",  Arg.String set_patoline_driver,
     "The driver against which to compile.")
  ; ("--format",  Arg.String set_patoline_format,
     "The document format to use.")
  ; ("--package", Arg.String add_patoline_package,
     "Package to link.")
  ; ("--no-default-grammar", Arg.Set no_default_grammar, "do not load DefaultGrammar")
  ]

let _ = extend_cl_args spec

#define LOCATE locate

(*
 * Everything is wrapped into the functor, this is standard procedur to write
 * syntax extensions using DeCaP. The argument of the functor is included
 * straight away, so that extensions can be composed.
 *)
module Ext = functor(In:Extension) -> struct
include In

(* Blank functions for Patoline *********************************************)

exception Unclosed_comments of Location.t list

(*
 * The position of comment openings is stored in a stack.
 * When a closing is reached we pop this stack so that in the end it contains
 * comments that have not been closed. There might be several, but there is at
 * least one.
 *)
let cstack = Stack.create ()
let get_unclosed () =
  let rec f l =
    if Stack.is_empty cstack then l
    else f (Stack.pop cstack :: l)
  in f [];

let patocomment = declare_grammar "patocomment"

let any_not_closing =
  black_box (fun str pos ->
    let (c, str', pos') = Input.read str pos in
    match c with
    | '\255' -> let locs = get_unclosed () in
                raise (Unclosed_comments locs)
    | '*'    -> let (c', _, _) = Input.read str' pos' in
                if c' = ')' then
                  raise (Give_up "Not the place to close a comment")
                else
                  ((), str', pos')
    | _      -> ((), str', pos')
  ) Charset.full_charset None "ANY"

let comment_content =
  parser
  | | _:patocomment
  | | _:string_literal
  | | _:any_not_closing

let _ = set_grammar patocomment
  (change_layout (
    parser
      {"(*" -> Stack.push _loc cstack}
      comment_content**
      {"*)" -> Stack.pop cstack}
  ) no_blank)

let patocomments =
  parser _:{patocomment}**

let blank_grammar_sline =
  parser _:''[ \t\r]''** _:{'\n' _:''[ \t\r]''**}??

let blank_grammar_mline =
  parser _:''[ \t\r]''** _:{'\n' _:''[ \t\r]''**}**

let blank_sline = blank_grammar blank_grammar_sline no_blank
let blank_mline = blank_grammar blank_grammar_mline no_blank

let blank1 = blank_grammar patocomments blank_sline
let blank2 = blank_grammar patocomments blank_mline

(* Code generation helpers **************************************************)

let counter = ref 1

(* Generate a fresh module names (Uid) *)
let freshUid () =
  let current = !counter in
  incr counter;
  "MOD" ^ (string_of_int current)

  let wrapped_caml_structure =
    parser
    | | '(' l:structure ')' -> l

  (* Parse a caml "expr" wrapped with parentheses *)
  let wrapped_caml_expr =
    parser
    | | '(' e:expression ')' -> e

  (* Parse a list of caml "expr" *)
  let wrapped_caml_list =
    parser
    | | '[' l:{e:expression l:{ ';' e:expression }** ';'?? -> e::l}??[[]] ']' -> l

  (* Parse an array of caml "expr" *)
  let wrapped_caml_array =
    parser
    | | "[|" l:{e:expression l:{ ';' e:expression }** ';'?? -> e::l}??[[]] "|]" -> l

(****************************************************************************
 * Words.                                                                   *
 ****************************************************************************)

  let char_re    = "[^ \t\r\n\\{}()#*/|_$>-]"
  let escaped_re =     "\\\\[\\{}()#*/|_$>-]"
  let special_re =             "[()#]"
  let non_special = ['>';'*';'/';'|';'-';'_']
  let char_alone =
    black_box
      (fun str pos ->
       let c,str',pos' = Input.read str pos in
       if List.mem c non_special then
         let c',_,_ = Input.read str' pos' in
         if c' = c then raise (Give_up "") (* FIXME *)
         else c, str', pos'
       else
         raise (Give_up "")) (* FIXME *)
      (List.fold_left Charset.add Charset.empty_charset non_special) None
      (String.concat " | " (List.map (fun c -> String.make 1 c) non_special))

  let character =
    parser
    | | c:RE(char_re) -> c
    | | s:RE(escaped_re) ->
        String.escaped (String.sub s 1 (String.length s - 1))
    | | c:char_alone -> String.make 1 c

  let special = parser
      s:RE(special_re) -> s

  let word =
    change_layout (
        parser
        | | cs:character++ ->
             let w = String.concat "" cs in
             if String.length w >= 2 &&
                  List.mem (String.sub w 0 2) ["==";"=>";"=<";"--";"->";"-<";">>";"$>";]
             then raise (Give_up (w ^ "is not a word"));
             w
        | | c:special -> c
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

  let verbatim_line =
      "\\(^#?#?\\([^#\t\n][^\t\n]*\\)?\\)"

  let string_filename = "\\\"\\([a-zA-Z0-9-_.]*\\)\\\""
  let uid_coloring    = "[A-Z][_'a-zA-Z0-9]*"

  let files_contents = Hashtbl.create 31

  let verbatim_environment =
    change_layout (
        parser
          RE("^###")
          lang:{_:RE("[ \t]+") id:RE(uid_coloring)}??
          filename:{_:RE("[ \t]+") fn:RE(string_filename)[groupe 1]}??
          RE("[ \t]*") '\n' lines:{l:RE(verbatim_line) '\n'}++
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
                    <:structure<let _ =
                     List.iter (fun line ->
                       newPar D.structure ~environment:verbEnv Complete.normal ragged_left $line_with_num$) ($lang$ $list:lines$)>>)
                  ) no_blank

  let verbatim_generic st forbid nd =
    let line_re = "[^\n" ^ forbid ^ "]+" in
    change_layout (
        parser
          STR(st)
          ls:{l:RE(line_re) '\n'}**
          l:RE(line_re)
              STR(nd) ->
            let lines = ls @ [l] in
            let lines = rem_hyphen lines in
            let txt = String.concat " " lines in
            <:expr@_loc< ($lid:"verb"$) [tT $string:txt$] >>
      ) no_blank

  let verbatim_macro = verbatim_generic "\\verb{" "{}" "}"
  let verbatim_sharp = verbatim_generic "##" "#" "##"
  let verbatim_bquote = verbatim_generic "``" "`" "``"

(****************************************************************************
 * Symbol definitions.                                                      *
 ****************************************************************************)

type math_prio =
  | Macro | AtomM | Accent | Ind | Fun | Prod
  | Sum | Operator | Rel | Neg | Conj | Impl | Punc

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
    if cc < min || cc > max then raise (Give_up "Char not in range..."); c
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

type config = EatR | EatL | Name of string list * string | Arity of int
  | GivePos | Arg of int * string | ArgNoPar of int | IsIdentity

let parser config =
  | "eat_right"                           -> EatR
  | "eat_left"                            -> EatL
  | "name" "=" ms:{u:uid - "."}* - id:lid -> Name (ms,id)
  | "arity" "=" n:num                     -> Arity n
  | "give_position"                       -> GivePos
  | "arg_" - n:num "=" id:lid             -> Arg (n,id)
  | "arg_" - n:num "no_parenthesis"       -> ArgNoPar n
  | "is_identity"                         -> IsIdentity
let parser configs = "{" cs:config* "}" -> cs

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
  ; mutable word_macros      : (string * config list) list
  ; mutable math_macros      : (string * config list) list
  ; mutable paragraph_macros : (string * config list) list
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
  ; word_macros      = []
  ; math_macros      = []
  ; paragraph_macros = []
  ; environment      = [] }

let math_atom_symbol = new_grammar "atom_symbol"
let math_prefix_symbol = new_grammar "prefix"
let math_postfix_symbol = new_grammar "postfix"
let math_quantifier_symbol = new_grammar "postfix"
let math_accent_symbol = new_grammar "accent"
let math_left_delimiter = new_grammar "left delimiter"
let math_right_delimiter = new_grammar "right delimiter"
let math_operator_symbol = new_grammar "operator"
let math_combining_symbol = new_grammar "combining"
let math_infix_symbol' = new_grammar "infix"
let parser math_infix_symbol =
    | (empty invisible_product)
    | math_infix_symbol'
let math_punctuation_symbol = new_grammar "punctuation"
let math_relation_symbol = new_grammar "relation"

let tree_to_grammar : ?filter:('a -> bool) -> 'a PMap.tree -> 'a grammar = fun ?(filter=fun x -> true) t ->
  let PMap.Node(_,l) = t in
  let fn buf pos =
    let line = Input.line buf in
    let line = String.sub line pos (String.length line - pos) in
    try
      let (n,v) = PMap.longest_prefix ~filter line t in
      (v, buf, pos+n)
    with Not_found -> raise (Give_up "Not a valid symbol.")
  in
  let charset =
    let f acc (c,_) = Charset.add acc c in
    List.fold_left f Charset.empty_charset l
  in
  black_box fn charset None "symbol"

let build_grammar () =
  set_grammar math_infix_symbol' (tree_to_grammar state.infix_symbols);
  set_grammar math_punctuation_symbol (tree_to_grammar ~filter:(fun s -> s.infix_prio = Punc) state.infix_symbols);
  set_grammar math_relation_symbol (tree_to_grammar ~filter:(fun s -> s.infix_prio = Rel) state.infix_symbols);
  set_grammar math_prefix_symbol (tree_to_grammar state.prefix_symbols);
  set_grammar math_postfix_symbol (tree_to_grammar state.postfix_symbols);
  set_grammar math_quantifier_symbol (tree_to_grammar state.quantifier_symbols);
  set_grammar math_atom_symbol (tree_to_grammar state.atom_symbols);
  set_grammar math_accent_symbol (tree_to_grammar state.accent_symbols);
  set_grammar math_left_delimiter (tree_to_grammar state.left_delimiter_symbols);
  set_grammar math_right_delimiter (tree_to_grammar state.right_delimiter_symbols);
  set_grammar math_operator_symbol (tree_to_grammar state.operator_symbols);
  set_grammar math_combining_symbol (tree_to_grammar state.combining_symbols)


let before_parse_hook () =
  In.before_parse_hook ();
  if not !no_default_grammar then begin
    let path = "." :: !Config2.grammarspath in
    let gram = findPath "DefaultGrammar.tgy" path in
    let ch = open_in_bin gram in
    Printf.eprintf "Reading default grammar %s\n%!" gram;
    state.infix_symbols <- input_value ch;
    state.prefix_symbols <- input_value ch;
    state.postfix_symbols <- input_value ch;
    state.quantifier_symbols <- input_value ch;
    state.atom_symbols <- input_value ch;
    state.accent_symbols <- input_value ch;
    state.left_delimiter_symbols <- input_value ch;
    state.right_delimiter_symbols <- input_value ch;
    state.operator_symbols <- input_value ch;
    state.combining_symbols <- input_value ch;
    build_grammar ();
    Printf.eprintf "Read default grammar\n%!";
    close_in ch;
  end

let symbol_paragraph _loc syms names =
  <:structure<
    let _ = newPar D.structure
      ~environment:(fun x -> {x with par_indent = []})

      Complete.normal Patoline_Format.parameters
      [bB (fun env0 -> Maths.kdraw
        [ { env0 with mathStyle = Mathematical.Display } ] [
        Maths.bin 0 (Maths.Normal(false,Maths.noad (Maths.glyphs "⇐"),false))
        $syms$ $names$
      ])];;
  >>

let math_list _loc l =
  let merge x y =
    <:expr<[Maths.bin 0
      (Maths.Normal(false,Maths.noad (Maths.glyphs ","),false))
      $x$ $y$]>>
  in
  List.fold_left merge (List.hd l) (List.tl l)

(****************************************************************************
 * Maths.                                                                   *
 ****************************************************************************)

type 'a indices = { up_right : 'a option; up_right_same_script: bool;
		      down_right : 'a option; up_left_same_script: bool;
		      up_left : 'a option;
		      down_left : 'a option }

let no_ind = { up_right = None; up_left = None; down_right = None; down_left = None;
		 up_right_same_script = false; up_left_same_script = false }


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

     <:expr< { (Maths.noad $print_math_symbol _loc elt$) with $(!r)$ } >>
    end

let print_math_deco _loc elt ind =
  if ind = no_ind then elt else begin
    let s = CamlSym <:expr<fun env st -> Maths.draw [env] $elt$>> in
    <:expr<[Maths.Ordinary $print_math_deco_sym _loc s ind$]>>
  end

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
  state.infix_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.infix_symbols sym_names;
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
  state.atom_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.atom_symbols sym_names;
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
  state.accent_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.accent_symbols sym_names;
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

let new_prefix_symbol _loc sym_names prefix_value =
  let prefix_macro_names, prefix_utf8_names = symbol sym_names in
  let sym = { prefix_prio = Prod; prefix_space = 3; prefix_no_space = false; prefix_macro_names; prefix_utf8_names; prefix_value } in
  state.prefix_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.prefix_symbols sym_names;
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
  state.postfix_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.postfix_symbols sym_names;
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
  state.quantifier_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.quantifier_symbols sym_names;
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
  state.left_delimiter_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.left_delimiter_symbols sym_names;
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
  state.right_delimiter_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.right_delimiter_symbols sym_names;
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
  state.operator_symbols <-
    List.fold_left (fun map name -> PrefixTree.add name sym map)
    state.operator_symbols sym_names;
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
      state.math_macros <- (id, cs) :: state.math_macros; []
  | "\\Configure_word_macro" "{" "\\"? - id:lid "}" cs:configs ->
      state.word_macros <- (id, cs) :: state.word_macros; []
  | "\\Configure_paragraph_macro" "{" "\\"? - id:lid "}" cs:configs ->
      state.paragraph_macros <- (id, cs) :: state.paragraph_macros; []
  | "\\Configure_environment" "{" id:lid "}" cs:configs ->
      state.environment <- (id, cs) :: state.environment; []
  | "\\Verbose_Changes" -> state.verbose <- true; []
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

let pred : math_prio -> math_prio = function
  | Macro -> Macro
  | p -> Obj.magic (Obj.magic p - 1)

type indice_height = Up | Down
type indice_side = Left | Right

let parser indices =
		   | "_" -> Right,Down
		   | "__"-> Left,Down
		   | "^" -> Right,Up
		   | "^^"-> Left,Up

let no_blank_list g = change_layout ( parser g* ) no_blank

let parser any_symbol =
  | sym:math_infix_symbol   -> sym.infix_value
  | sym:math_atom_symbol    -> sym.symbol_value
  | sym:math_prefix_symbol  -> sym.prefix_value
  | sym:math_postfix_symbol -> sym.postfix_value

let parser math_aux : ((Parsetree.expression indices -> Parsetree.expression) * math_prio) Decap.grammar =
  | '{' (m,_):math_aux '}' -> (m,AtomM)
  | '{' s:any_symbol '}' ->
      if s = Invisible then raise (Give_up "...");
      (fun indices ->
        <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s s indices$]>>
      ), AtomM

  | l:math_left_delimiter (m,_):math_aux r:math_right_delimiter ->
     (fun indices ->
       let l = print_math_symbol _loc_l (MultiSym l.delimiter_values) in
       let r = print_math_symbol _loc_r (MultiSym r.delimiter_values) in
       print_math_deco _loc <:expr<[Maths.Decoration (Maths.open_close $l$ $r$, $m no_ind$)]>> indices
     ),AtomM

  | name:''[a-zA-Z][a-zA-Z0-9]*'' ->
     (fun indices ->
       if String.length name > 1 then
	 let elt = <:expr<fun env -> Maths.glyphs $string:name$ (Maths.change_fonts env env.font)>> in
	 <:expr<[Maths.Ordinary $print_math_deco_sym _loc_name (CamlSym elt) indices$] >>
       else
	 <:expr<[Maths.Ordinary $print_math_deco_sym _loc_name (SimpleSym name) indices$] >>), AtomM

  | num:''[0-9]+\([.][0-9]+\)?'' ->
     (fun indices ->
       <:expr<[Maths.Ordinary $print_math_deco_sym _loc_num (SimpleSym num) indices$] >>), AtomM

  | sym:math_atom_symbol ->
      (fun indices ->
	<:expr<[Maths.Ordinary $print_math_deco_sym _loc_sym sym.symbol_value indices$] >>), AtomM

  | sym:math_prefix_symbol (m,mp):math_aux ->
     if mp > sym.prefix_prio then give_up "bad prefix priority";
      (fun indices ->
	<:expr<[Maths.bin $int:sym.prefix_space$ (Maths.Normal(true,$print_math_deco_sym _loc_sym sym.prefix_value indices$,$bool:sym.prefix_no_space$)) [] $m no_ind$] >>), sym.prefix_prio

  | sym:math_quantifier_symbol d:math_declaration p:math_punctuation_symbol? (m,mp):math_aux ->
     if mp > Operator then give_up "bad prefix priority";
    (fun indices ->
      let inter = match p with
	  None -> <:expr<Maths.Invisible>>
	| Some s ->
	   let nsl = s.infix_no_left_space in
	   let nsr = s.infix_no_right_space in
                <:expr<
                         Maths.Normal( $bool:nsl$,
                           $print_math_deco_sym _loc_p s.infix_value no_ind$,
                           $bool:nsr$) >>
      in
	<:expr<[Maths.bin 3
                    (Maths.Normal(true,$print_math_deco_sym _loc_sym sym.symbol_value indices$,true))
                    []
                    [Maths.bin 1 $inter$ $d$ $m no_ind$]]>>), Operator

  | op:math_operator (m,mp):math_aux ->
     let sym,ind = op in
     if mp > sym.operator_prio then give_up "bad operator priority";
    (fun indices ->
      if indices.down_right <> None || indices.up_right <> None then give_up "illegal indices for applied operators";
      let ind = { indices with down_right = ind.down_right; up_right = ind.up_right } in
      match sym.operator_kind with
	Limits ->
	  <:expr<[Maths.op_limits [] $print_math_deco_sym _loc_op (MultiSym sym.operator_values) ind$ $m no_ind$]>>
      | NoLimits ->
	 <:expr<[Maths.op_nolimits [] $print_math_deco_sym _loc_op (MultiSym sym.operator_values) ind$ $m no_ind$]>>), sym.operator_prio

  | (m,mp):math_aux sym:math_combining_symbol  ->
    if mp > AtomM then give_up "bad post priority";
    (fun indices -> <:expr<$lid:sym$ $m indices$>>),Accent

  | (m,mp):math_aux sym:math_postfix_symbol  ->
     if mp > sym.postfix_prio then give_up "bad post priority";
      (fun indices ->
	<:expr<[Maths.bin $int:sym.postfix_space$ (Maths.Normal($bool:sym.postfix_no_space$,$print_math_deco_sym _loc_sym sym.postfix_value indices$,true)) $m no_ind$ []] >>), sym.postfix_prio

  | '\\' - id:lid - args:(no_blank_list (change_layout math_macro_argument blank1)) ->
     (fun indices ->
       let config =
         try List.assoc id state.math_macros with Not_found -> []
       in
       (* TODO special macro properties to be handled. *)
       let apply acc arg = <:expr<$acc$ $arg$>> in
       let e = List.fold_left apply <:expr<$lid:id$>> args in
       print_math_deco _loc_id e indices
     ), AtomM

  | (m,mp):math_aux s:Subsup.subscript ->
     if mp > Ind then give_up "No indice/exponent allowed here";
    let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s (SimpleSym s) no_ind$] >> in
    let rd indices =
      if indices.down_right <> None then give_up "double indices";
      { indices with down_right = Some s }
    in
    (fun indices -> m (rd indices)), Ind

  | (m,mp):math_aux s:Subsup.superscript ->
     if mp > Ind then give_up "No indice/exponent allowed here";
    let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s (SimpleSym s) no_ind$] >> in
    let rd indices =
      if indices.up_right <> None then give_up "double indices";
      { indices with up_right = Some s }
    in
    (fun indices -> m (rd indices)), Ind

  | (m,mp):math_aux s:math_accent_symbol ->
     if mp > Ind then give_up "No indice/exponent allowed here";
    let s = <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s s.symbol_value no_ind$] >> in
    let rd indices =
      if indices.up_right <> None then give_up "double indices";
      { indices with up_right = Some s; up_right_same_script = true }
    in
    (fun indices -> m (rd indices)), Ind

  | (m,mp):math_aux - (s,h):indices - (r,rp):math_aux ->
     if (mp >= Ind && s = Left) then give_up "can not be used as indice";
     if (rp >= Ind && s = Right) then give_up "can not be used as indice";
     if (mp > Ind && s = Right) then give_up "No indice/exponent allowed here";
     if (rp > Ind && s = Left) then give_up "No indice/exponent allowed here";
     let rd indices = function
       | Left,Down ->
	  if indices.down_left <> None then give_up "double indices";
	 { indices with down_left = Some (m no_ind) }
       | Right,Down ->
	  if indices.down_right <> None then give_up "double indices";
	 { indices with down_right = Some (r no_ind) }
       | Left,Up ->
	  if indices.up_left <> None then give_up "double indices";
	 { indices with up_left = Some (m no_ind) }
       | Right,Up ->
	  if indices.up_right <> None then give_up "double indices";
	 { indices with up_right = Some (r no_ind) }
     in
     (fun indices -> (if s = Left then r else m) (rd indices (s,h))), Ind

  | (l,lp):math_aux s:math_infix_symbol (r,rp):math_aux ->
     if lp > s.infix_prio || rp >= s.infix_prio then give_up "bad infix priority";
       (fun indices ->
	 let nsl = s.infix_no_left_space in
	 let nsr = s.infix_no_right_space in
	 let sp = s.infix_space in
	 let l = l no_ind and r = r no_ind in
	 if s.infix_value = SimpleSym "over" then begin
	   if indices <> no_ind then give_up "indices on fraction";
	   <:expr< [Maths.fraction $l$ $r$] >>
	 end else begin
	   let inter =

	     if s.infix_value = Invisible then
	       <:expr< Maths.Invisible >>
	     else
	       <:expr<
                         Maths.Normal( $bool:nsl$,
                           $print_math_deco_sym _loc_s s.infix_value indices$,
                           $bool:nsr$) >>
	   in
	   <:expr<[Maths.Binary { bin_priority= $int:sp$;
                                bin_drawing = $inter$;
                                bin_left= $l$;
                                bin_right= $r$ } ] >>
	 end), s.infix_prio

and math_macro_argument =
  | '{' (m,_):math_aux '}' -> m no_ind
  | e:wrapped_caml_expr    -> e

and math_operator =
  | o:math_operator_symbol ->
     (o, no_ind)

  | (o,i):math_operator - (s,h):indices - (r,rp):math_aux ->
     if (rp >= Ind && s = Right) then give_up "can not be used as indice";
     if (s = Left) then give_up "No indice/exponent allowed here";
     let i = match h with
       | Down ->
	  if i.down_right <> None then give_up "double indices";
	 { i with down_right = Some (r no_ind) }
       | Up ->
	  if i.up_right <> None then give_up "double indices";
	 { i with up_right = Some (r no_ind) }
     in
     (o, i)

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
  | (m,p):math_aux -> if p > Ind then give_up "too hight math priority"; m no_ind
  | l:math_punc_list s:math_punctuation_symbol (m,p):math_aux ->
     if p > Ind then give_up "too hight math priority";
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

and math_declaration =
  | m:math_punc_list -> m
  | l:math_declaration s:math_relation_symbol r:math_punc_list ->
    let nsl = s.infix_no_left_space in
    let nsr = s.infix_no_right_space in
    let inter =
      <:expr<
                         Maths.Normal( $bool:nsl$,
                           $print_math_deco_sym _loc_s s.infix_value no_ind$,
                           $bool:nsr$) >>
    in
    <:expr<[Maths.bin 2 $inter$ $l$ $r$] >>


let parser math_toplevel =
  | (m,_):math_aux -> m no_ind
  | s:any_symbol   ->
      if s = Invisible then raise (Give_up "...");
      <:expr<[Maths.Ordinary $print_math_deco_sym _loc_s s no_ind$]>>



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
 * Text content of paragraphs and macros (mutually recursive).              *
 ****************************************************************************)

(* bool param -> can contain special text environments //...// **...** ... *)
  let paragraph_basic_text, set_paragraph_basic_text = grammar_family "paragraph_basic_text"

(***** Patoline macros  *****)
  let parser macro_argument =
    | '{' l:(paragraph_basic_text TagSet.empty) '}' -> l
    | e:wrapped_caml_expr  -> e
    | e:wrapped_caml_array -> <:expr<$array:e$>>
    | e:wrapped_caml_list  -> <:expr<$list:e$>>

  let reserved_macro = [ "Caml"; "begin"; "end"; "item"; "verb"; "diagram" ]

  let macro_name = change_layout (
    parser "\\" - m:lid ->
      if List.mem m reserved_macro then
        raise (Give_up (m ^ " is a reserved macro")); m
    ) no_blank

  let macro =
    parser
    | m:macro_name args:macro_argument** ->
                        (let fn = fun acc r -> <:expr@_loc_args<$acc$ $r$>> in
                         List.fold_left fn <:expr@_loc_m<$lid:m$>> args)
    | m:verbatim_macro -> m

    | "\\diagram" s:(change_layout wrapped_caml_structure blank2) ->
        <:expr<
          [bB (fun env ->
            let module Res =
              struct
                module EnvDiagram = Env_Diagram (struct let env = env end) ;;
                $s$ ;;
              end
             in [ Drawing (Res.EnvDiagram.make ()) ])]>>

(****************************)

  let text_paragraph_elt (tags:TagSet.t) =
    parser
      m:macro -> m

    | | "//" - p:(paragraph_basic_text (addTag Italic tags)) - "//" when allowed Italic tags ->
         <:expr@_loc_p<toggleItalic $p$>>
    | | "**" - p:(paragraph_basic_text (addTag Bold tags)) - "**" when allowed Bold tags ->
         <:expr@_loc_p<bold $p$>>
    | | "||" - p:(paragraph_basic_text (addTag SmallCap tags)) - "||" when allowed SmallCap tags ->
         <:expr@_loc_p<sc $p$>>
(*    | | "__" - p:(paragraph_basic_text (addTag Underline tags)) - "__" when allowed Underline tags ->
         <:expr@_loc_p<underline $p$>>
    | | "--" - p:(paragraph_basic_text (addTag Strike tags)) - "--" when allowed Strike tags ->
      <:expr@_loc_p<strike $p$>>*)

    | | v:verbatim_bquote -> <:expr@_loc_v<$v$>>
    | | v:verbatim_sharp  -> <:expr@_loc_v<$v$>>

    | | '(' p:(paragraph_basic_text tags) ')' ->
         <:expr@_loc_p<tT $string:"("$ :: $p$ @ [tT $string:")"$]>>

    | | '"' p:(paragraph_basic_text (addTag Quote tags)) '"' when allowed Quote tags ->
        (let opening = "``" in (* TODO addapt with the current language*)
         let closing = "''" in (* TODO addapt with the current language*)
         <:expr@_loc_p<tT($string:opening$) :: $p$ @ [tT($string:closing$)]>>)

    | | '$' m:math_toplevel '$' ->
        <:expr@_loc_m<[bB (fun env0 -> Maths.kdraw
                        [ { env0 with mathStyle = env0.mathStyle } ]
                          $m$)]>>
    | | "[$" m:math_toplevel "$]" ->
        <:expr@_loc_m<[bB (fun env0 -> Maths.kdraw
                        [ { env0 with mathStyle = env0.mathStyle } ]
                        (displayStyle $m$))]>>

    | | l:word -> <:expr@_loc_l<[tT $string:l$]>>


  let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
    let x,y = Lexing.((end_pos _loc_p1).pos_cnum, (start_pos _loc_p2).pos_cnum) in
    (*Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
    let bl e = if y - x >= 1 then <:expr@_loc_p1<tT" "::$e$>> else e in
    let _loc = merge2 _loc_p1 _loc_p2 in
    <:expr@_loc<$p1$ @ $(bl p2)$>>

  let _ = set_paragraph_basic_text (fun tags ->
             parser
               l:{p:(text_paragraph_elt tags) -> (_loc, p)}++ ->
                 match List.rev l with
                 | []   -> assert false
                 | m::l ->
                    let fn = fun (_loc_m, m) (_loc_p, p) ->
                      (merge2 _loc_p _loc_m
                      , concat_paragraph p _loc_p m _loc_m)
                    in snd (List.fold_left fn m l))

  let text_only = change_layout (paragraph_basic_text TagSet.empty) blank1

  let paragraph_basic_text =
    parser
      p:(paragraph_basic_text TagSet.empty) ->
        (fun indented ->
         if indented then
           <:structure@_loc_p<
             let _ = newPar D.structure
                            Complete.normal Patoline_Format.parameters $p$ >>
         else
           <:structure@_loc_p<
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

  let paragraph_elt =
    parser
    | | verb:verbatim_environment -> (fun _ -> verb)
    | | "\\Caml" s:(change_layout wrapped_caml_structure blank2) -> (fun _ -> s)
    | | "\\Include" '{' id:capitalized_ident '}' -> (fun _ ->
         incr nb_includes;
         let temp_id = Printf.sprintf "TEMP%d" !nb_includes in
         <:structure< module $uid:temp_id$ =$uid:id$.Document(Patoline_Output)(D)
                      open $uid:temp_id$>>)
    | | "\\item" -> (fun _ ->
         let m1 = freshUid () in
         let m2 = freshUid () in
         let _Item = "Item" in
         <:structure< module $uid:m1$ =
                     struct
                       module $uid:m2$ = $uid : _Item$ ;;
                       let _ = $uid:m2$.do_begin_env () ;;
                       let _ = $uid:m2$.do_end_env ()
                     end>>)
    | | "\\begin{" idb:lid '}' args:macro_argument**
       ps:(change_layout paragraphs blank2)
       "\\end{" ide:lid '}' ->
         (fun indent_first ->
           if idb <> ide then raise (Give_up "Non-matching begin / end");
           let m1 = freshUid () in
           let m2 = freshUid () in
           let arg =
             if args = [] then <:structure<>> else
               let gen i e =
                 let id = Printf.sprintf "arg%i" (i+1) in
                 <:structure<let $lid:id$ = $e$ ;;>>
               in
               let args = List.mapi gen args in
               let combine acc e = <:structure<$acc$ ;; $e$ ;;>> in
               let args = List.fold_left combine <:structure<>> args in
               <:structure<
                 module $uid:("Arg_"^m2)$ =
                   struct
                     $args$ ;;
                   end
               >>
           in
           let def =
             let name = "Env_" ^ idb in
             let argname = "Arg_" ^ m2 in
             if args = [] then
               <:structure<module $uid:m2$ = $uid:name$ ;;>>
             else
               <:structure<module $uid:m2$ = $uid:name$($uid:argname$) ;;>>
           in
           <:structure< module $uid:m1$ =
                       struct
                         $arg$ ;;
                         $def$ ;;
                         open $uid:m2$ ;;
                         let _ = $uid:m2$ . do_begin_env () ;;
                         $(ps indent_first)$ ;;
                         let _ = $uid:m2$ . do_end_env ()
                        end>>)
    | | "$$" m:math_toplevel "$$" ->
         (fun _ ->
           <:structure<let _ = newPar D.structure
                        ~environment:(fun x -> {x with par_indent = []})
                        Complete.normal displayedFormula
                        [bB (fun env0 -> Maths.kdraw
                          [ { env0 with mathStyle = Mathematical.Display } ]
                          $m$)];;>>)
    | | l:paragraph_basic_text -> l
    | | s:symbol_def -> fun _ -> s

  let _ = set_grammar paragraph (
                        change_layout (
                            parser
                              e:paragraph_elt es:{es:paragraph_elt}** ->
                                                 let es = List.flatten (List.map (fun r -> r false) es) in
                                                 fun indent -> e indent @ es
                          ) blank1)

  let _ = set_grammar paragraphs (
                        parser
                          p:paragraph ps:paragraph** ->
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

  let text_item =
    parser
      op:RE(op_section) title:text_only txt:text cl:RE(cl_section) ->
        (fun _ lvl ->
         let numbered = match op.[0], cl.[0] with
             '=', '=' -> <:expr@_loc_op<newStruct>>
           | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
           | _ -> raise (Give_up "Non-matching relative section markers")
         in
         true, lvl, <:structure< let _ = $numbered$ D.structure $title$;;
                     $(txt false (lvl+1))$;;
                     let _ = go_up D.structure >>)

    | | op:RE(section) title:text_only cl:RE(section) txt:text ->
        (fun _ lvl ->
         if String.length op <> String.length cl then
     raise (Give_up "Non-matching absolute section marker");
         let numbered = match op.[0], cl.[0] with
             '=', '=' -> <:expr@_loc_op<newStruct>>
           | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
           | _ -> raise (Give_up "Non-mathing section marker")
         in
         let l = String.length op - 1 in
         if l > lvl + 1 then failwith "Illegal level skip";
         let res = ref [] in
         for i = 0 to lvl - l do
           res := !res @ <:structure@_loc_op<let _ = go_up D.structure>>
         done;
         true, lvl, <:structure< $!res$ let _ = ($numbered$) D.structure $title$;;
                     $(txt false l)$>>)

    | | ps:paragraphs ->
         (fun indent lvl -> indent, lvl, ps indent)

  let _ = set_grammar text (
     parser
       l:text_item** ->
         (fun indent lvl ->
          let fn = fun (indent, lvl, ast) txt ->
            let indent, lvl, ast' = txt indent lvl in
            indent, lvl, <:structure<$ast$;; $ast'$>>
          in
          let _,_,r = List.fold_left fn (indent, lvl, []) l in
          r)
      )

  let text =
    parser
      txt:text -> txt true 0




(* Header, title, main Patoline entry point *********************************)

let patoline_config : unit grammar =
  change_layout (
    parser
    | "#" n:capitalized_ident ' ' a:capitalized_ident ->
          (match n with
           | "FORMAT"  -> patoline_format := a
           | "DRIVER"  -> patoline_driver := a
           | "PACKAGE" -> patoline_packages := a :: !patoline_packages
           | _         -> raise (Give_up ("Unknown directive #"^n)))
  ) no_blank

let header = parser _:patoline_config**

let title =
  parser
  | RE("==========\\(=*\\)") title:text_only
    auth:{_:RE("----------\\(-*\\)") t:text_only}??
    inst:{_:RE("----------\\(-*\\)") t:text_only}??
    date:{_:RE("----------\\(-*\\)") t:text_only}??
    RE("==========\\(=*\\)") ->

      let date =
        match date with
        | None   -> <:expr@_loc_date<[]>>
        | Some t -> <:expr@_loc_date<["Date", string_of_contents $t$]>>
      in
      let inst =
        match inst with
        | None   -> <:expr@_loc_inst<[]>>
        | Some t -> <:expr@_loc_inst<["Institute", string_of_contents $t$]>>
      in
      let auth =
        match auth with
        | None   -> <:expr@_loc_auth<[]>>
        | Some t -> <:expr@_loc_auth<["Author", string_of_contents $t$]>>
      in
      <:structure@_loc<
        let _ = Patoline_Format.title D.structure
                  ~extra_tags:($auth$ @ $inst$ @ $date$) $title$
      >>

let wrap basename _loc ast =
  <:structure<
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
    let temp1 = List.map fst (snd !D.structure);;
    $ast$
    let _ = D.structure:=follow (top !D.structure) (List.rev temp1)
    end;;
    let _ = $lid:("cache_"^basename)$  := $array:(List.rev !cache_buf)$;;
    let _ = $lid:("mcache_"^basename)$ := $array:(List.rev !mcache_buf)$;;
  >>

let init =
  parser EMPTY -> (fun () ->
    let file = match !file with
                 | None -> ""
                 | Some f -> f
    in
    let basename = chop_extension' file in
    cache := "cache_" ^ basename;
    basename)

let full_text =
  parser
  | h:header basename:init ->>
    let basename = basename () in
    t:title?? txt:text EOF ->
      begin
        let t = match t with
                | None   -> <:structure<>>
                | Some t -> t
        in
        let ast = <:structure<$t$;; $txt$>> in
        wrap basename _loc ast
      end

(* Extension of Ocaml's grammar *********************************************)

let directive =
  parser
  | '#' n:capitalized_ident a:capitalized_ident ->
    ((match n with
       | "FORMAT"  -> patoline_format := a
       | "DRIVER"  -> patoline_driver := a
       | "PACKAGE" -> patoline_packages := a :: !patoline_packages
       | _ -> raise (Give_up ("Unknown directive #"^n)));
    <:structure<>>)
let extra_structure = directive :: extra_structure

let patoline_quotations _ =
    parser
    | "<<" par:text_only     ">>" -> Atom, par
    | "<$" mat:math_toplevel "$>" -> Atom, mat
let _ = add_reserved_symb "<<"
let _ = add_reserved_symb ">>"
let _ = add_reserved_symb "<$"
let _ = add_reserved_symb "$>"
let extra_expressions = patoline_quotations :: extra_expressions

(* Entry points and extension creation **************************************)

(* Adding the new entry points *)

let _ =
  entry_points :=
    (".txp", Implementation (full_text, blank2)) ::
    (".typ", Implementation (full_text, blank2)) ::
    (".mlp", Implementation (structure, blank)) ::
    !entry_points

end (* of the functor *)

(* Creating and running the extension *)
let _ =
  try
    let _ = Printexc.record_backtrace true in
    let module ParserExt = Pa_parser.Ext(Pa_ocaml_prelude.Initial) in
    let module PaExt = Ext(ParserExt) in
    let module PatolineDefault = Pa_ocaml.Make(PaExt) in
    let module M = Pa_main.Start(PatolineDefault) in
    match !Pa_ocaml_prelude.file, !in_ocamldep with
    | Some s, false ->
       let name = chop_extension' s ^ ".tgy" in
       let ch = open_out_bin name in
       Printf.eprintf "Writing default grammar\n%!";
       let open PaExt in
       output_value ch state.infix_symbols;
       output_value ch state.prefix_symbols;
       output_value ch state.postfix_symbols;
       output_value ch state.quantifier_symbols;
       output_value ch state.atom_symbols;
       output_value ch state.accent_symbols;
       output_value ch state.left_delimiter_symbols;
       output_value ch state.right_delimiter_symbols;
       output_value ch state.operator_symbols;
       output_value ch state.combining_symbols;
       Printf.eprintf "Written default grammar\n%!";
       close_out ch
    | _ -> Printf.eprintf "Not writing default grammar, no filename\n%!";

  with e ->
    Printf.eprintf "Exception: %s\nTrace:\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stderr
