(* ocamlc -pp ../pa_glr -I .. -I +camlp4 dynlink.cma camlp4lib.cma str.cma umap.cmo glr.cmo -o pato pato.ml
    ocamlfind ocamlopt -pp ./pato -package Typography,Typography.Pdf -linkpkg  -impl toto.txt
*)

open Input
open Glr
open Charset
open FilenameExtra
open Pa_ocaml_prelude

(****************************************************************************
 * Some state information (Driver in use, ...) + Code generation helpers    *
 ****************************************************************************)

let patoline_format   = ref "DefaultFormat"
let patoline_driver   = ref "Pdf"
let patoline_packages = ref ["Typography"]

(****************************************************************************
 * Command-line argument parsing.                                           *
 ****************************************************************************)

let _ = spec := !spec @
		  [ ("--driver",  Arg.String (fun d -> patoline_driver := d),
		     "The driver against which to compile.")
		  ; ("--format",  Arg.String (fun f -> patoline_format := f),
		     "The document format to use.")
		  ; ("--package", Arg.String (fun p -> let pkgs = p :: !patoline_packages
						       in patoline_packages := pkgs),
		     "Package to link.")
		  ]


let _ = parser_locate locate merge

(****************************************************************************
 * Things that have to do with comments and things to be ignored            *
 ****************************************************************************)

exception Unclosed_comment of int * int

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
let pato_blank mline str pos =
  let rec fn nb lvl state prev (str, pos as cur) =
    if is_empty str then (if lvl > 0 then raise (Unclosed_comment (line_num str, pos)) else cur)
    else
      let c,str',pos' = read str pos in 
      let next =str', pos' in
      match state, c with
      | `Ini , '('               -> fn nb lvl `Opn cur next
      | `Opn , '*'               -> fn nb (lvl + 1) `Ini cur next
      | `Opn , _  when lvl = 0   -> prev
      | `Opn , _                 -> fn nb lvl `Ini cur next
      | `Ini , '*' when lvl = 0  -> cur
      | `Ini , '*'               -> fn nb lvl `Cls cur next
      | `Cls , '*'               -> fn nb lvl `Cls cur next
      | `Cls , ')'               -> fn 0 (lvl - 1) `Ini cur next
      | `Cls , _                 -> fn nb lvl `Ini cur next

      | _    , '\n' when lvl > 0 -> fn nb lvl `Ini cur next
      | _    , '\n' when nb > 0  -> cur
      | _    , '\n'              -> fn (nb + 1) lvl `Ini cur next

      | `Str , '"'               -> fn nb lvl `Ini cur next
      | _    , '"' when lvl > 0  -> (try fn nb lvl `Str cur next with
                                      Unclosed_comment _ ->
                                        fn nb lvl `Ini cur next)
      | `Str , '\\'              -> fn nb lvl `Esc cur next
      | `Esc , _                 -> fn nb lvl `Str cur next
      | `Str , _                 -> fn nb lvl `Str cur next

      | _    , (' '|'\t'|'\r')   -> fn nb lvl `Ini cur next
      | _    , _ when lvl > 0    -> fn nb lvl `Ini cur next
      | _    , _                 -> cur
  in fn 0 0 `Ini (str, pos) (str, pos)

let blank1 = pato_blank false
let blank2 = pato_blank true

(* Function for geting fresh module names (Uid) *)
let counter = ref 1
let freshUid () =
  let current = !counter in
  incr counter;
  "MOD" ^ (string_of_int current)

module Ext = functor(In:Extension) -> 
struct
  include In


  let wrapped_caml_structure = 
    parser
    | CHR('(') l:structure CHR(')') -> l

  (* Parse a caml "expr" wrapped with parentheses *)
  let wrapped_caml_expr = 
    parser
    | CHR('(') e:expression CHR(')') -> e

  (* Parse a list of caml "expr" *)
  let wrapped_caml_list =
    parser
    | CHR('[') l:{e:expression l:{ CHR(';') e:expression }* CHR(';')? -> e::l}?[[]] CHR(']') -> l

  (* Parse an array of caml "expr" *)
  let wrapped_caml_array =
    parser
    | STR("[|") l:{e:expression l:{ CHR(';') e:expression }* CHR(';')? -> e::l}?[[]] STR("|]") -> l

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
		     let c,str',pos' = read str pos in
		     if List.mem c non_special then
		       let c',_,_ = read str' pos' in
		       if c' = c then raise Give_up
		       else c, str', pos'
		     else
		       raise Give_up)
		    (List.fold_left Charset.add Charset.empty_charset non_special) false
		    (String.concat " | " (List.map (fun c -> String.make 1 c) non_special))

  let character =
    parser
    | c:RE(char_re) -> c
    | s:RE(escaped_re) ->
	String.escaped (String.sub s 1 (String.length s - 1))
    | c:char_alone -> String.make 1 c

  let special = parser
      s:RE(special_re) -> s
			       
  let word =
    change_layout (
	parser
        | cs:character+ ->
             let w = String.concat "" cs in
             if String.length w >= 2 &&
		  List.mem (String.sub w 0 2) ["==";"=>";"=<";"--";"->";"-<";">>";"$>";]
             then raise Give_up;
             w
	| c:special -> c
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
      "\\(^[^# \t][^\t]*\\)\\|\\(^#?#?[^#\t][^\t]*\\)\\|\\(^[\t]*\\)"

(*    "^#?#?\\([^#\t\n][^\t\n]*\\)?" ??? *)
      
  let string_filename = "\\\"\\([a-zA-Z0-9-_.]*\\)\\\""
  let uid_coloring    = "[A-Z][_'a-zA-Z0-9]*"

  let files_contents = Hashtbl.create 31 

  let verbatim_environment =
    change_layout (
	parser
	  RE("^###")
	  lang:{RE("[ \t]+") id:RE(uid_coloring)}?
	  filename:{RE("[ \t]+") fn:RE(string_filename)[groupe 1]}?
	  RE("[ \t]*") CHR('\n')
	  lines:{l:RE(verbatim_line) CHR('\n')}+ 
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
			     let mode = if pos >= 0 then
					  [Open_creat; Open_append ]
					else
					  [Open_creat; Open_trunc; Open_wronly ]
			     in
			     let name = if Filename.is_relative name then
					  match !file with
					    None -> name
					  | Some file ->
					     Filename.concat (Filename.dirname file) name
					else name
			     in
			     Printf.eprintf "Creating file: %s\n%!" name;
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
			 <:expr<verb_counter "" @ line >>
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
	  ls:{l:RE(line_re) STR("\n")}*
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
 * Maths.                                                                   *
 ****************************************************************************)

  let math_toplevel =
    parser
    | STR("x") -> <:expr@_loc<Maths.noad (Maths.glyphs "x")>>
		    (* TODO *)

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
  let macro_argument =
    parser
      STR("{") l:(paragraph_basic_text TagSet.empty) STR("}") -> l
    | e:wrapped_caml_expr  -> e
    | e:wrapped_caml_array -> <:expr<$array:e$>>
    | e:wrapped_caml_list  -> <:expr<$list:e$>>

  let lident = "[_a-z][_a-zA-Z0-9']*"

  let reserved_macro = [ "Caml"; "begin"; "end"; "item"; "verb" ]

  let macro_name =
    change_layout (
	parser
	  STR("\\") m:RE(lident) ->
        if List.mem m reserved_macro then raise Give_up; m
      ) no_blank

  let macro =
    parser
    | m:macro_name args:macro_argument* ->
			(let fn = fun acc r -> <:expr@_loc_args<$acc$ $r$>> in
			 List.fold_left fn <:expr@_loc_m<$lid:m$>> args)
    | m:verbatim_macro -> m

(****************************)

  let text_paragraph_elt (tags:TagSet.t) =
    parser
      m:macro -> m
		   
    | STR("//") - p:(paragraph_basic_text (addTag Italic tags)) - STR("//") when allowed Italic tags ->
         <:expr@_loc_p<toggleItalic $p$>>
    | STR("**") - p:(paragraph_basic_text (addTag Bold tags)) - STR("**") when allowed Bold tags ->
         <:expr@_loc_p<bold $p$>>
    | STR("||") - p:(paragraph_basic_text (addTag SmallCap tags)) - STR("||") when allowed SmallCap tags ->
         <:expr@_loc_p<sc $p$>>
(*    | STR("__") - p:(paragraph_basic_text ("__"::tags)) - STR("__") when not (List.mem "__" tags) ->
         <:expr@_loc_p<underline $p$>>
    | STR("--") - p:(paragraph_basic_text ("--"::tags)) - STR("--") when not (List.mem "--" tags) ->
         <:expr@_loc_p<strike $p$>>*)

    | v:verbatim_bquote -> <:expr@_loc_v<$v$>>
    | v:verbatim_sharp  -> <:expr@_loc_v<$v$>>

    | STR("(") p:(paragraph_basic_text tags) STR(")") ->
         <:expr@_loc_p<tT $string:"("$ :: $p$ @ [tT $string:")"$]>>

    | STR("\"") p:(paragraph_basic_text (addTag Quote tags)) STR("\"") when allowed Quote tags ->
        (let opening = "``" in (* TODO addapt with the current language*)
         let closing = "''" in (* TODO addapt with the current language*)
         <:expr@_loc_p<tT($string:opening$) :: $p$ @ [tT($string:closing$)]>>)

    | STR("$") m:math_toplevel STR("$") ->
        <:expr@_loc_m<[bB (fun env0 -> Maths.kdraw
                        [ { env0 with mathStyle = env0.mathStyle } ]
                        [Maths.Ordinary $m$])]>>
    | STR("[$") m:math_toplevel STR("$]") ->
        <:expr@_loc_m<[bB (fun env0 -> Maths.kdraw
                        [ { env0 with mathStyle = env0.mathStyle } ]
                        (displayStyle [Maths.Ordinary $m$]))]>>

    | l:word -> <:expr@_loc_l<[tT $string:l$]>>


  let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
    let x,y = Lexing.((end_pos _loc_p1).pos_cnum, (start_pos _loc_p2).pos_cnum) in
    (*Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
    let bl e = if y - x >= 1 then <:expr@_loc_p1<tT" "::$e$>> else e in
    let _loc = merge2 _loc_p1 _loc_p2 in
    <:expr@_loc<$p1$ @ $(bl p2)$>>

  let _ = set_paragraph_basic_text (fun tags ->
	     parser
	       l:{p:(text_paragraph_elt tags) -> (_loc, p)}+ ->
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

  let paragraph_elt =
    parser
    | verb:verbatim_environment -> (fun _ -> verb)
    | STR("\\Caml") s:wrapped_caml_structure -> (fun _ -> s)
    | STR("\\item") -> (fun _ ->
        (let m1 = freshUid () in
         let m2 = freshUid () in
	 let _Item = "Item" in
         <:structure< module $uid:m1$ =
                     struct
                       module $uid:m2$ = $uid : _Item $ ;;
                       let _ = $uid:m2$.do_begin_env () ;;
                       let _ = $uid:m2$.do_end_env ()
                     end>>))
    | STR("\\begin{") idb:RE(lident) STR("}")
       ps:(change_layout paragraphs blank2)
       STR("\\end{") ide:RE(lident) STR("}") ->
         (fun indent_first ->
           if idb <> ide then raise Give_up;
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
    | STR("$$") m:math_toplevel STR("$$") ->
         (fun _ ->
           <:structure<let _ = newPar D.structure
                        ~environment:(fun x -> {x with par_indent = []})
                        Complete.normal displayedFormula
                        [bB (fun env0 -> Maths.kdraw
                          [ { env0 with mathStyle = Mathematical.Display } ]
                          [Maths.Ordinary $m$])];;>>)
    | l:paragraph_basic_text -> l

  let _ = set_grammar paragraph (
			change_layout (
			    parser
			      e:paragraph_elt es:paragraph_elt* ->
						 let es = List.flatten (List.map (fun r -> r false) es) in
						 fun indent -> e indent @ es
			  ) blank1)

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

  let text_item =
    parser
      op:RE(op_section) title:text_only txt:text cl:RE(cl_section) ->
	(fun _ lvl ->
	 let numbered = match op.[0], cl.[0] with
	     '=', '=' -> <:expr@_loc_op<newStruct>>
           | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
           | _ -> raise Give_up
         in
         true, lvl, <:structure< let _ = $numbered$ D.structure $title$;;
                     $(txt false (lvl+1))$;;
                     let _ = go_up D.structure >>)
	  
    | op:RE(section) title:text_only cl:RE(section) txt:text ->
	(fun _ lvl ->
	 if String.length op <> String.length cl then raise Give_up;
         let numbered = match op.[0], cl.[0] with
             '=', '=' -> <:expr@_loc_op<newStruct>>
           | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
           | _ -> raise Give_up
         in
         let l = String.length op - 1 in
         if l > lvl + 1 then failwith "Illegal level skip";
         let res = ref [] in
         for i = 0 to lvl - l do
           res := !res @ <:structure@_loc_op<let _ = go_up D.structure>>
         done;
         true, lvl, <:structure< $!res$ let _ = ($numbered$) D.structure $title$;;
                     $(txt false l)$>>)
	  
    | ps:paragraphs -> 
	 (fun indent lvl -> indent, lvl, ps indent)

  let _ = set_grammar text (
     parser
       l:text_item* ->
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
		      
(****************************************************************************
 * Header, Title, Document body.                                            *
 ****************************************************************************)

  let uident = "[A-Z][a-zA-Z0-9_']*"
		 
  let patoline_config : unit grammar =
    change_layout (
	parser
	  STR("#") n:RE(uident) STR(" ") a:RE(uident) ->
        ((match n with
          | "FORMAT"  -> patoline_format := a
          | "DRIVER"  -> patoline_driver := a
          | "PACKAGE" -> patoline_packages := a :: !patoline_packages;
          | _         -> raise Give_up);
         ())
      ) no_blank

  let header =
    parser
      patoline_config* -> []

  let title =
    parser
      RE("==========\\(=*\\)") t:text_only
      author:{RE("----------\\(-*\\)") t:text_only}?
      institute:{RE("----------\\(-*\\)") t:text_only}?
      date:{RE("----------\\(-*\\)") t:text_only}?
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
	   <:structure@_loc_t<
	     let _ = Patoline_Format.title D.structure ~extra_tags:($extras$) $t$>>

(****************************************************************************
 * Helper function for top level ast.                                       *
 ****************************************************************************)

  let wrap basename _loc ast =
    <:structure<
     open Typography
     open Util
     open Typography.Box
     open Typography.Config
     open Typography.Document
     open Typography.OutputCommon
     open DefaultFormat.MathsFormat
     let $lid:("cache_"^basename)$  =
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
     let _ = $lid:("cache_"^basename)$ :=[||];;
     let _ = $lid:("mcache_"^basename)$ :=[||];; >>

  let full_text =
    parser
      h:header t:title? txt:text EOF ->
			    let t = match t with
			      | None   -> []
			      | Some t -> t
			    in
			    let ast = <:structure<$h$;; $t$;; $txt$>> in
			    let file = match !file with None -> assert false
						      | Some f -> f
			    in
			    let basename = chop_extension' file in
			    wrap basename _loc ast

(****************************************************************************
 * Extension to Ocaml's grammar.                                            *
 ****************************************************************************)

  let directive =
    parser
      CHR('#') n:capitalized_ident a:capitalized_ident ->
          ((match n with
             | "FORMAT"  -> patoline_format := a
             | "DRIVER"  -> patoline_driver := a
             | "PACKAGE" -> patoline_packages := a :: !patoline_packages;
             | _ -> raise Give_up);
          <:structure<>>)

  let extra_structure = directive :: extra_structure	    

  let patoline_quotations =
      parser
	STR("<<") par:text_only STR(">>") -> Atom, par
      | STR("<$") mat:math_toplevel STR("$>") -> Atom, mat

  let _ = reserved_symbols := "<<" :: "<$" :: !reserved_symbols

  let extra_expressions = patoline_quotations :: extra_expressions

 (****************************************************************************
  * Register file extension                                                  *
  ****************************************************************************)

 let _ = 
    entry_points:= 
      (".txp", `Impl full_text) ::
	(".typ", `Impl full_text) ::
	  (".mlp", `Impl structure ) :: !entry_points

end

let _ = register_extension (module Ext : FExt)
