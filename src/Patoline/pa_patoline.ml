(* ocamlc -pp ../pa_glr -I .. -I +camlp4 dynlink.cma camlp4lib.cma str.cma umap.cmo glr.cmo -o pato pato.ml
    ocamlfind ocamlopt -pp ./pato -package Typography,Typography.Pdf -linkpkg  -impl toto.txt
*)

open Input
open Glr
open Charset
open FilenameExtra
open Pa_ocaml_prelude

let _ = glr_locate locate merge

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

(****************************************************************************
 * Some state information (Driver in use, ...) + Code generation helpers    *
 ****************************************************************************)

let patoline_format   = ref "DefaultFormat"
let patoline_driver   = ref "Pdf"
let patoline_packages = ref ["Typography"]

(* Function for geting fresh module names (Uid) *)
let counter = ref 1
let freshUid () =
  let current = !counter in
  incr counter;
  "MOD" ^ (string_of_int current)

let _ = glr_locate locate merge

module Ext = functor(In:Extension) -> 
struct
  include In


  let wrapped_caml_structure = 
    glr
    | CHR('(') l:structure CHR(')') -> l
    end

  (* Parse a caml "expr" wrapped with parentheses *)
  let wrapped_caml_expr = 
    glr
    | CHR('(') e:expression CHR(')') -> e
    end

  (* Parse a list of caml "expr" *)
  let wrapped_caml_list =
    glr
    | CHR('[') l:{e:expression l:{ CHR(';') e:expression }* CHR(';')? -> e::l}?[[]] CHR(']') -> l
    end

(* Parse an array of caml "expr" *)
let wrapped_caml_array =
    glr
    | STR("[|") l:{e:expression l:{ CHR(';') e:expression }* CHR(';')? -> e::l}?[[]] STR("|]") -> Array.of_list l
    end

(****************************************************************************
 * Words.                                                                   *
 ****************************************************************************)

let char_re    = "[^][ `\t\r\n{}()\\_$|/*#\"]"
let escaped_re = "\\\\[][\\$|({)}/*#\"`]"

let character =
  glr
    c:RE(char_re) -> String.escaped c
  | s:RE(escaped_re) ->
     String.escaped (String.sub s 1 (String.length s - 1))
  end

let word =
  change_layout (
    glr
      cs:character+ ->
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
    ws:word+ -> String.concat " " (rem_hyphen ws)
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
      lines:glr l:RE(verbatim_line) RE("\n") -> l end+
      RE("^###") -> (
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
          <:structure<let _ = newPar D.structure ~environment:verbEnv
                              Complete.normal ragged_left
                              (lang_default $string:l$)>>
        in
        let fn = fun l -> buildline (String.escaped l) in
        List.flatten (List.map fn lines))
    end
  ) no_blank

let verbatim_generic st forbid nd =
  let line_re = "[^\n" ^ forbid ^ "]+" in
  change_layout (
    glr
      STR(st)
      ls:glr l:RE(line_re) STR("\n") -> l end*
      l:RE(line_re)
      STR(nd) ->
        let lines = ls @ [l] in
        let lines = rem_hyphen lines in
        let txt = String.concat " " lines in
        <:expr@_loc< ($lid:"verb"$) [tT $string:txt$] >>
    end
  ) no_blank

let verbatim_macro = verbatim_generic "\\verb{" "{}" "}"
let verbatim_sharp = verbatim_generic "##" "#" "##"
let verbatim_bquote = verbatim_generic "``" "`" "``"


(****************************************************************************
 * Maths.                                                                   *
 ****************************************************************************)

let math_toplevel =
  glr
    STR("x") -> <:expr@_loc<Maths.noad (Maths.glyphs "x")>>
    (* TODO *)
  end

(****************************************************************************
 * Text content of paragraphs and macros (mutually recursive).              *
 ****************************************************************************)

(* bool param -> can contain special text environments //...// **...** ... *)
let paragraph_basic_text, set_paragraph_basic_text = grammar_family "paragraph_basic_text"

(***** Patoline macros  *****)
let macro_argument =
  glr
     STR("{") l:(paragraph_basic_text true) STR("}") -> l
  | e:wrapped_caml_expr  -> e
(* FIXME: need $list:e$ and $array:e$ *)
(*  | e:wrapped_caml_array -> e
  | e:wrapped_caml_list  -> e*)
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
     m:macro_name args:macro_argument* ->
       (let fn = fun acc r -> <:expr@_loc_args<$acc$ $r$>> in
        List.fold_left fn <:expr@_loc_m<$lid:m$>> args)
  | m:verbatim_macro -> m
  end
(****************************)

let text_paragraph_elt allowed =
    glr
       m:macro -> m
    | l:words -> <:expr@_loc_l<[tT $string:l$]>>

    | STR("//") p:(paragraph_basic_text false) _e:STR("//") when allowed ->
         <:expr@_loc_p<toggleItalic $p$>>
    | STR("**") p:(paragraph_basic_text false) _e:STR("**") when allowed ->
         <:expr@_loc_p<bold $p$>>
    | v:verbatim_bquote -> <:expr@_loc_v<$v$>>
    (*
    | STR("__") p:(paragraph_basic_text false) _e:STR("__") when allowed ->
         <:expr@_loc_p<underline $p$>>
    | STR("--") p:(paragraph_basic_text false) _e:STR("--") when allowed ->
         <:expr@_loc_p<strike $p$>>
    *)

    | v:verbatim_sharp -> <:expr@_loc_v<$v$>>
    | STR("||") p:(paragraph_basic_text false) _e:STR("||") when allowed ->
         <:expr@_loc_p<sc $p$>>

    | STR("(") p:(paragraph_basic_text allowed) STR(")") ->
         <:expr@_loc_p<tT $string:"("$ :: $p$ @ [tT $string:")"$]>>
    | STR("\"") p:(paragraph_basic_text false) _e:STR("\"") when allowed ->
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
    end

let concat_paragraph p1 _loc_p1 p2 _loc_p2 =
(* FIXME ... what is the best way ? *)
(*    let x = _loc_p1.end_pos.lnum  and y = Loc.start_off _loc_p2 in*)
    (*Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
  let y = 1 and x = 0 in
    let bl e = if y - x >= 1 then <:expr@_loc_p1<tT" "::$e$>> else e in
    let _loc = merge _loc_p1 _loc_p2 in
    <:expr@_loc<$p1$ @ $(bl p2)$>>

let _ = set_paragraph_basic_text
  (fun spec_allowed ->
    glr
      l:{p:(text_paragraph_elt spec_allowed) -> (_loc, p)}+ ->
        match List.rev l with
          | []   -> assert false
          | m::l ->
              let fn = fun (_loc_m, m) (_loc_p, p) ->
                         (merge _loc_p _loc_m
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
            <:structure@_loc_p<
               let _ = newPar D.structure
                         Complete.normal Patoline_Format.parameters $p$ >>
        else
              <:structure@_loc_p<
               let _ = newPar D.structure
                         ~environment:(fun x -> { x with par_indent = [] })
                         Complete.normal Patoline_Format.parameters $p$>>)
  end

(****************************************************************************
 * Paragraphs                                                               *
 ****************************************************************************)

let paragraph = declare_grammar "paragraph"
let paragraphs = declare_grammar "paragraphs"

let paragraph_elt =
  glr
       verb:verbatim_environment -> (fun _ -> verb)
    | STR("\\Caml") s:wrapped_caml_structure -> (fun _ -> s)
    | l:paragraph_basic_text -> l
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
  end

let _ = set_grammar paragraph (
  change_layout (
    glr
      e:paragraph_elt es:paragraph_elt* ->
			 let es = List.flatten (List.map (fun r -> r false) es) in
			 fun indent -> e indent @ es
    end
  ) blank1)

let _ = set_grammar paragraphs (
  glr
    p:paragraph ps:paragraph* ->
      		   let ps = List.flatten (List.map (fun r -> r true) ps) in
		   fun indent_first -> p indent_first @ ps
  end)

(****************************************************************************
 * Sections, layout of the document.                                        *
 ****************************************************************************)

let text = declare_grammar "text"

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
  end

let _ = set_grammar text (
  glr
    l:text_item* ->
      (fun indent lvl ->
        let fn = fun (indent, lvl, ast) txt ->
                   let indent, lvl, ast' = txt indent lvl in
                   indent, lvl, <:structure<$ast$;; $ast'$>>
        in
        let _,_,r = List.fold_left fn (indent, lvl, []) l in
        r)
  end)

let text =
  glr
    txt:text -> txt true 0
  end

(****************************************************************************
 * Header, Title, Document body.                                            *
 ****************************************************************************)

let uident = "[A-Z][a-zA-Z0-9_']*"

let patoline_config : unit grammar =
  change_layout (
    glr
      STR("#") n:RE(uident) STR(" ") a:RE(uident) ->
        ((match n with
            | "FORMAT"  -> patoline_format := a
            | "DRIVER"  -> patoline_driver := a
            | "PACKAGE" -> patoline_packages := a :: !patoline_packages;
            | _         -> raise Give_up);
        ())
    end
  ) no_blank

let header =
  glr
    patoline_config* -> []
  end

let title =
  glr
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
 end

let full_text =
  glr
    h:header t:title? txt:text EOF ->
      let t = match t with
                | None   -> []
                | Some t -> t
      in
      <:structure<$h$;; $t$;; $txt$>>
  end


(****************************************************************************
 * Helper function for top level ast.                                       *
 ****************************************************************************)

let wrap basename _loc ast = <:structure<
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

let _ = 
  entry_points := 
    (".txp", `Impl full_text) ::
      (".typ", `Impl full_text) ::
	(".mlp", `Impl structure ) :: !entry_points

(* FIXME: combine this with pa_ocaml options parsing *)
let _ =
  let anon_args = ref [] in
  Arg.parse spec (fun a -> anon_args := a :: !anon_args) "Usage:";

end

let _ = register_extension (module Ext)
