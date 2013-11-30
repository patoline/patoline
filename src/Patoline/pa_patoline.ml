(* ocamlc -pp ../pa_glr -I .. -I +camlp4 dynlink.cma camlp4lib.cma str.cma umap.cmo glr.cmo -o pato pato.ml 
    ocamlfind ocamlopt -pp ./pato -package Typography,Typography.Pdf -linkpkg  -impl toto.txt
*)

open Glr
open Charset
open Camlp4.PreCast
open Syntax

module Id : Camlp4.Sig.Id =
    struct
      let name = "pa_patoline"
      let version = "0.1"
    end

module Extension (Syntax : Camlp4.Sig.Camlp4Syntax) =
	struct
include Syntax

let patoline_caml_expr = Gram.Entry.mk "patoline_caml_expr"
let patoline_caml_struct = Gram.Entry.mk "patoline_caml_struct"

EXTEND Gram
     patoline_caml_expr: [ [
       e = expr LEVEL "top"; ")" -> e
     ] ];
     patoline_caml_struct: [ [
       e = str_items; ")" -> e
     ] ];
END;;
	end

module M0 = Camlp4OCamlRevisedParser.Make(Syntax)
module M1 = Camlp4OCamlParser.Make(M0)
module M = Extension(M1)
open M

let fname = ref ""

let bol = Hashtbl.create 1001

let find_pos str n = 
  let rec fn i =
      (*      Printf.fprintf stderr "str: %s, i: %d\n%!" str i;*)
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
  Lexing.({ pos_fname = !fname; pos_lnum = lnum; pos_bol = bol; pos_cnum = n })

let locate g =
  filter_position g (fun str pos pos' ->
    let s = find_pos str pos in
    let e = find_pos str pos' in
    Loc.(merge (of_lexing_position s) (of_lexing_position e)))

let _ = glr_locate locate Loc.merge

let caml_expr _loc = 
  let fn str pos = 
    let last_pos = ref pos in
    let len = String.length str in
    let cs = Stream.from (fun n ->
(*            Printf.fprintf stderr "n: %d, pos: %d, c: %c, len %d\n%!" n pos str.[n+pos] len;*)
      last_pos := max !last_pos (n+pos);
      if n+pos >= len then None else Some str.[n+pos]
      ) 
    in
    let r =
      Gram.parse patoline_caml_expr _loc cs
    in
    r, !last_pos
  in
  black_box fn (singleton '(') false

let caml_struct _loc = 
  let fn str pos = 
    let last_pos = ref pos in
    let len = String.length str in
    let cs = Stream.from (fun n ->
      (* Printf.fprintf stderr "n: %d, pos: %d, c: %c, len %d\n%!" n pos str.[n+pos] len;*)
      last_pos := max !last_pos (n+pos);
      if n+pos >= len then None else Some str.[n+pos]
      ) 
    in
    let r =
      Gram.parse patoline_caml_struct _loc cs
    in 
    r, !last_pos
  in
  black_box fn (singleton '(') false
 
let blank1 = blank_regexp (Str.regexp "[ \t\r]*\\([\n][ \t\r]*\\)?")
let blank2 = blank_regexp (Str.regexp "[ \n\t\r]*")
let section = "\\(==+\\)\\|\\(--+\\)"
let op_section = "[=-]>"
let cl_section = "[=-]<"
let word_re = "\\([^ \t\r\n{}\\]\\|\\([\\][\\{}]\\)\\)+"
let macro = "\\\\[^ \t\r\n({]+"

let paragraph_local = declare_grammar ()

let argument =
  glr
     STR("{") l:paragraph_local STR("}") -> l
  || e:(dependant_sequence (locate (glr p:STR("(") end)) (fun (_loc,_) -> caml_expr _loc)) -> e
  end

let macro =
  glr
     m:RE(macro) args:argument** ->
       let m = String.sub m 1 (String.length m - 1) in
       List.fold_left (fun acc r -> <:expr@_loc_args<$acc$ $r$>>)  <:expr@_loc_m<$lid:m$>> args
 end


let _ = set_grammar paragraph_local (
  change_layout
    glr
       m:macro p:paragraph_local?? -> 
           (match p with None -> m | Some (p:Ast.expr) -> 
	     let x = Loc.stop_off _loc_m and y = Loc.start_off _loc_p in
(*	     Printf.fprintf stderr "x: %d, y: %d\n%!" x y;*)
	   let bl = if y - x >= 1 then <:expr@_loc_m<[tT" "]>> else <:expr@_loc_m<[]>> in
	   let _loc = Loc.merge _loc_m _loc_p in
           <:expr<$m$ @ $bl$ @ $p$>>)
    || l:{w:RE(word_re) ->
	    if String.length w >= 2 && List.mem (String.sub w 0 2) ["==";"=<";"--";"->"] then
	      raise Give_up;
	    w }++
          -> <:expr@_loc_l<[tT($str:(String.concat " " l)$)]>>
    end
  blank1)

let paragraph =
    glr
      l:paragraph_local ->
        fun no_indent ->
	  if no_indent then
	    <:str_item@_loc_l<
              let _ =
		newPar D.structure ~environment:(fun x -> { x with par_indent = [] }) Complete.normal 
		  Patoline_Format.parameters $l$
                >>
	  else
	    <:str_item@_loc_l<
              let _ =
		newPar D.structure Complete.normal Patoline_Format.parameters $l$
		>>
    end 

let text = declare_grammar ()
let _ = set_grammar text
  glr
    op:RE(section) title:paragraph_local cl:RE(section) txt:text ->
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
       <:str_item< $!res$ let _ = $numbered$ D.structure $title$;; $txt true l$ >>)

  || op:RE(op_section) title:paragraph_local txt:text cl:RE(cl_section) txt2:text ->
      (fun no_indent lvl -> 
       let numbered = match op.[0], cl.[0] with
	   '=', '=' -> <:expr@_loc_op<newStruct>>
         | '-', '-' -> <:expr@_loc_op<newStruct ~numbered:false>>
	 | _ -> raise Give_up
       in
       <:str_item< let _ = $numbered$ D.structure $title$;; $txt true (lvl+1)$;; let _ = go_up D.structure;; $txt2 false lvl$ >>)

  || STR("\\Caml") s:(dependant_sequence (locate (glr p:STR("(") end)) (fun (_loc,_) -> caml_struct _loc))
	txt:text -> (fun no_indent lvl -> <:str_item<$s$ $txt no_indent lvl$>>)

  || l:paragraph txt:text  -> (fun no_indent lvl -> <:str_item<$l no_indent$ $txt false lvl$>>)

  || (empty ()) -> (fun _ _ -> <:str_item<>>)
  end

let full_text = 
  glr
    t:text EOF -> t false 0
  end 

(* #FORMAT DefaultFormat *)
(* #DRIVER Pdf *)
(* #PACKAGES Typography *)

let format = Sys.argv.(1)
let driver = Sys.argv.(2)
let filename = Sys.argv.(3)
let basename = Filename.chop_extension filename

let _ =
      try 
	let ch = open_in filename in
	fname := filename;
	let n = in_channel_length ch in
	let str = String.create n in
	really_input ch str 0 n;
       try
	let l = parse_string full_text blank2 str in
	let _loc = Loc.(of_lexing_position (find_pos str 0)) in 
	let all = <:str_item<
	  open Typography
	  open Typography.Util
	  open Typography.Box
	  open Typography.Config
	  open Typography.Document
	  open Typography.OutputCommon
	  open DefaultFormat.MathsFormat
	  let $lid:"cache_"^basename$ = ref ([||] : (environment -> Mathematical.style -> box list) array)
	  let $lid:"mcache_"^basename$ = ref ([||] : (environment -> Mathematical.style -> box list) list array)
	  module Document=functor(Patoline_Output:DefaultFormat.Output) -> functor(D:DocumentStructure)->struct
	    module Patoline_Format = $uid:format$.Format(D)
	    open $uid:format$
	    open Patoline_Format
            let temp1 = List.map fst (snd !D.structure);;
	    $l$
           let _ = D.structure:=follow (top !D.structure) (List.rev temp1)
         end;;
         let _ = $lid:"cache_"^basename$ :=[||];;
         let _ = $lid:"mcache_"^basename$ :=[||];;
        >>
	 in
	Printers.OCaml.print_implem all
(*	Printers.DumpOCamlAst.print_implem all*)
      with
	Parse_error n -> 
	  let _loc = find_pos str n in 
	  Loc.raise Loc.(of_lexing_position _loc) Stream.Failure
      | Ambiguity(n,p) -> 
	let _loc1 = find_pos str n in 
	let _loc2 = find_pos str p in 
	Loc.raise Loc.(merge (of_lexing_position _loc1) (of_lexing_position _loc2)) 
	  (Stream.Error "Ambiguous expression")
      with

      | exc -> Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exc; exit 1
