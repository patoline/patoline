open Decap
open UsualMake
open FilenameExtra
open Pa_ocaml_prelude

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
  | MultiSym of string
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
  let tail  = char_range 128 191 in
  let head1 = char_range 0   127 in
  let head2 = char_range 192 223 in
  let head3 = char_range 224 239 in
  let head4 = char_range 240 247 in
  parser
  | c0:head1                               ->
      String.make 1 c0
  | c0:head2 - c1:tail                     ->
      let s = String.make 2 c0 in s.[1] <- c1; s
  | c0:head3 - c1:tail-  c2:tail           ->
      let s = String.make 3 c0 in s.[1] <- c1; s.[2] <- c2; s
  | c0:head4 - c1:tail - c2:tail - c3:tail ->
      let s = String.make 4 c0 in s.[1] <- c1; s.[2] <- c2; s.[3] <- c3; s

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
  | Quantifier
  | Limit
  | Sum

type operator =
  { operator_prio : math_prio;
    operator_utf8_names : string list;
    operator_macro_names : string list; (* with backslash *)
    operator_value : Parsetree.expression;
    operator_space : int;
    operator_no_space : bool;
    operator_kind : operator_kind;
  }

type grammar_state =
  { mutable verbose          : bool
  ; mutable infix_symbols    : infix StrMap.t (* key are macro_names or utf8_names mixed *)
  ; mutable infix_grammar    : string grammar
  ; mutable atom_symbols     : atom_symbol StrMap.t
  ; mutable atom_grammar     : string grammar
  ; mutable other_symbols    : StrSet.t
  ; mutable word_macros      : (string * config list) list
  ; mutable math_macros      : (string * config list) list
  ; mutable paragraph_macros : (string * config list) list
  ; mutable environment      : (string * config list) list }

let state =
  { verbose          = false
  ; infix_symbols    = StrMap.empty
  ; infix_grammar    = fail "no infix yet"
  ; atom_symbols     = StrMap.empty
  ; atom_grammar     = fail "no symbol yet"
  ; other_symbols    = StrSet.empty
  ; word_macros      = []
  ; math_macros      = []
  ; paragraph_macros = []
  ; environment      = [] }

let build_grammar () =
  let cmp s1 s2 = String.length s2 - String.length s1 in
  let map_to_grammar m =
    Decap.alternatives (List.map (fun s -> Decap.string s s)
			  (List.sort cmp (List.map fst
					    (StrMap.bindings m))))
  in
  state.infix_grammar <- map_to_grammar state.infix_symbols;
  state.atom_grammar <-map_to_grammar state.atom_symbols

let before_parse_hook () =
  In.before_parse_hook ();
  if not !no_default_grammar then begin
    let path = "." :: !Config2.grammarspath in
    let gram = findPath "DefaultGrammar.tgy" path in
    let ch = open_in_bin gram in
    Printf.eprintf "Reading default grammar %s\n%!" gram;
    state.infix_symbols <- input_value ch;
    state.atom_symbols <- input_value ch;
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
      SimpleSym s-> <:expr<Maths.glyphs $string:s$>>, false
    | CamlSym s-> s, false
    | _ -> failwith "a faire ds Pa_patoline.print_math_symbol.\n"
      (* | MultiSym s -> <:expr<$lid:s$>>, true *)
      (* | ComplexSym s -> Printf.bprintf buf "(%s)" s; raise Exit *)
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

let print_math_deco _loc elt ind =
  let gn name ind =
    match ind with
      None -> assert false
    | Some m ->
      <:record< $lid:name$ = $m$ >>
  in
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

let new_infix_symbol _loc infix_prio sym_names infix_value =
  let infix_macro_names, infix_utf8_names = symbol sym_names in
  let infix_no_left_space = false in
  let infix_no_right_space = (infix_prio = Punc) in
  let infix_space = match infix_prio with
    | Sum -> 2
    | Prod -> 3
    | Rel | Punc -> 1
    | Conj | Impl -> 0
    | _ -> assert false
  in
  let sym = { infix_prio; infix_macro_names; infix_utf8_names; infix_value;
	      infix_space; infix_no_left_space; infix_no_right_space } in
  state.infix_symbols <-
    List.fold_left (fun map name -> StrMap.add name sym map)
    state.infix_symbols sym_names;
  build_grammar ();
  (* Displaying no the document. *)
  if state.verbose then
    let sym = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc infix_value$)]>> in
    let f s = sym in (* TODO *)
    let names = List.map f sym_names in
    symbol_paragraph _loc sym (math_list _loc names)
  else []

let parser math_infix_symbol =
    | "*"? -> invisible_product
    | r:(black_box (fun buf pos ->
      let name,buf,pos =
	internal_parse_buffer state.infix_grammar blank buf pos
      in
      try
	let sym = StrMap.find name state.infix_symbols in
	sym,buf,pos
      with Not_found -> give_up "Not an infix symbol")
	Charset.full_charset None "Not an infix symbol") -> r

let new_symbol _loc sym_names symbol_value =
  let symbol_macro_names, symbol_utf8_names = symbol sym_names in
  let sym = { symbol_macro_names; symbol_utf8_names; symbol_value } in
  state.atom_symbols <-
    List.fold_left (fun map name -> StrMap.add name sym map)
    state.atom_symbols sym_names;
  build_grammar ();
  (* Displaying no the document. *)
  if state.verbose then
    let sym = <:expr<[Maths.Ordinary (Maths.noad $print_math_symbol _loc symbol_value$)]>> in
    let f s = sym in (* TODO *)
    let names = List.map f sym_names in
    symbol_paragraph _loc sym (math_list _loc names)
  else []

let math_atom_symbol =
  black_box (fun buf pos ->
    let name,buf,pos =
      internal_parse_buffer state.atom_grammar blank buf pos
    in
    try
      let sym = StrMap.find name state.atom_symbols in
      sym,buf,pos
    with Not_found -> give_up "Not an atom symbol")
    Charset.full_charset None "Not an atom symbol"

let new_multi_symbol _loc kind is_infix syms value =
  (* An parser for the new symbol as an atom. *)
  let parse_sym = symbol syms in
  (* TODO *)
  (* Displaying no the document. *)
  if state.verbose then
    let syms =
      <:expr<[Maths.Ordinary (Maths.noad
        (fun x y -> List.flatten (Maths.multi_glyphs $value$ x y)))]>>
    in
    let names = [syms] in (* TODO *)
    symbol_paragraph _loc syms (math_list _loc names)
  else []

let new_combining_symbol _loc uchr macro =
  (* An parser for the new symbol as an atom. *)
  let parse_sym = string uchr () in
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

let save_grammar () =
  (* TODO do something ? *)
  ()

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
  | "\\Save_Grammar"    -> save_grammar (); []
  (* Addition of single symbols *)
  | "\\Add_relation"      ss:symbols e:symbol_value ->
      new_infix_symbol _loc Rel      ss e
  | "\\Add_addition_like" ss:symbols e:symbol_value ->
      new_infix_symbol _loc Sum ss e
  | "\\Add_product_like"  ss:symbols e:symbol_value ->
      new_infix_symbol _loc Prod  ss e
  | "\\Add_connector"     ss:symbols e:symbol_value ->
      new_infix_symbol _loc Conj     ss e
  | "\\Add_arrow"         ss:symbols e:symbol_value ->
     new_infix_symbol _loc Impl          ss e
  | "\\Add_punctuation"   ss:symbols e:symbol_value ->
     new_infix_symbol _loc Punc   ss e (* FIXME: check *)
  | "\\Add_negation"      ss:symbols e:symbol_value ->
     [] (* new_symbol _loc Negation      false ss e*)
  | "\\Add_quantifier"    ss:symbols e:symbol_value ->
     [] (* new_symbol _loc Quantifier    false ss e*)
  | "\\Add_prefix"        ss:symbols e:symbol_value ->
     [] (* new_symbol _loc Prefix        true  ss e*)
  | "\\Add_postfix"       ss:symbols e:symbol_value ->
     [] (* new_symbol _loc Postfix       true  ss e*)
  | "\\Add_accent"        ss:symbols e:symbol_value ->
     [] (* new_symbol _loc Accent        true  ss e*)
  | "\\Add_symbol"        ss:symbols e:symbol_value ->
     new_symbol _loc ss e
  (* Addition of mutliple symbols (different sizes) *)
  | "\\Add_operator"        ss:symbols e:symbol_values ->
      new_multi_symbol _loc Operator       false  ss e
  | "\\Add_limits_operator" ss:symbols e:symbol_values ->
     [] (*new_multi_symbol _loc Limit_operator false  ss e*)
  | "\\Add_left"            ss:symbols e:symbol_values ->
     [] (* new_multi_symbol _loc Left           false  ss e*)
  | "\\Add_right"           ss:symbols e:symbol_values ->
     [] (*new_multi_symbol _loc Right          false  ss e*)
  (* Special case, combining symbol *)
  | "\\Add_combining" "{" c:uchar "}" "{" "\\" - m:lid "}" ->
      new_combining_symbol _loc c m

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

let parser math_aux : ((Parsetree.expression indices -> Parsetree.expression) * math_prio) Decap.grammar =
  | '{' (m,_):math_aux '}' -> (m,AtomM)

  | var:''[a-zA-Z]'' ->
     (fun indices ->
       <:expr<[Maths.Ordinary $print_math_deco _loc_var (SimpleSym var) indices$] >>), AtomM

  | num:''[0-9]+\([.][0-9]+\)?'' ->
     (fun indices ->
       <:expr<[Maths.Ordinary $print_math_deco _loc_num (SimpleSym num) indices$] >>), AtomM

  | '\\' - id:lid - args:(no_blank_list (change_layout math_macro_argument blank1)) ->
     (fun indices ->
       (* TODO special macro properties to be handled. *)
       let apply acc arg = <:expr<$acc$ $arg$>> in
       List.fold_left apply <:expr<$lid:id$>> args
     ), AtomM

  | sym:math_atom_symbol ->
      (fun indices ->
	<:expr<[Maths.Ordinary $print_math_deco _loc_sym sym.symbol_value indices$] >>), AtomM

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
                           $print_math_deco _loc_s s.infix_value indices$,
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

let math_toplevel = parser
  (m,_):math_aux -> m no_ind



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

  let reserved_macro = [ "Caml"; "begin"; "end"; "item"; "verb" ]

  let macro_name = change_layout (
    parser "\\" - m:lid ->
      if List.mem m reserved_macro then
        raise (Give_up (m ^ "is a reserved macro")); m
    ) no_blank

  let macro =
    parser
    | m:macro_name args:macro_argument** ->
                        (let fn = fun acc r -> <:expr@_loc_args<$acc$ $r$>> in
                         List.fold_left fn <:expr@_loc_m<$lid:m$>> args)
    | m:verbatim_macro -> m

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
(*    | | "__" - p:(paragraph_basic_text ("__"::tags)) - "__" when not (List.mem "__" tags) ->
         <:expr@_loc_p<underline $p$>>
    | | "--" - p:(paragraph_basic_text ("--"::tags)) - "--" when not (List.mem "--" tags) ->
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
    | | "\\Caml" s:wrapped_caml_structure -> (fun _ -> s)
    | | "\\Include" '{' id:capitalized_ident '}' -> (fun _ ->
         incr nb_includes;
         let temp_id = Printf.sprintf "TEMP%d" !nb_includes in
         <:structure< module $uid:temp_id$ =$uid:id$.Document(Patoline_Output)(D)
                      open $uid:temp_id$>>)
    | | "\\item" -> (fun _ ->
        (let m1 = freshUid () in
         let m2 = freshUid () in
         let _Item = "Item" in
         <:structure< module $uid:m1$ =
                     struct
                       module $uid:m2$ = $uid : _Item$ ;;
                       let _ = $uid:m2$.do_begin_env () ;;
                       let _ = $uid:m2$.do_end_env ()
                     end>>))
    | | "\\begin{" idb:lid '}'
       ps:(change_layout paragraphs blank2)
       "\\end{" ide:lid '}' ->
         (fun indent_first ->
           if idb <> ide then raise (Give_up "Non-matching begin / end");
           let m1 = freshUid () in
           let m2 = freshUid () in
           let _Env = "Env_" in
           <:structure< module $uid:m1$ =
                       struct
                         module $uid:m2$ = $uid:(_Env^idb)$ ;;
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
module ParserExt = Pa_parser.Ext(Pa_ocaml_prelude.Initial)
module PaExt = Ext(ParserExt)
module PatolineDefault = Pa_ocaml.Make(PaExt)
module M = Pa_main.Start(PatolineDefault)

let _ =
  match !Pa_ocaml_prelude.file, !in_ocamldep with
  | Some s, false ->
     let name = chop_extension' s ^ ".tgy" in
     let ch = open_out_bin name in
     Printf.eprintf "Writing default grammar\n%!";
     output_value ch PaExt.state.infix_symbols;
     output_value ch PaExt.state.atom_symbols;
     Printf.eprintf "Written default grammar\n%!";
     close_out ch
  | _ -> Printf.eprintf "Not writing default grammar, no filename\n%!";
