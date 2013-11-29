(* ocamlc -pp ../pa_glr -I .. -I +camlp4 dynlink.cma camlp4lib.cma str.cma umap.cmo glr.cmo -o pato pato.ml 
    ocamlfind ocamlopt -pp ./pato -package Typography,Typography.Pdf -linkpkg  -impl toto.txt
*)

open Glr
open Charset
open Camlp4.PreCast
open Syntax
open Loc

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

let raise = Pervasives.raise

let caml_expr = 
  let fn str pos = 
    let last_pos = ref pos in
    let len = String.length str in
    let cs = Stream.from (fun n ->
      (*      Printf.fprintf stderr "n: %d, pos: %d, c: %c, len %d\n%!" n pos str.[n+pos] len;*)
      last_pos := max !last_pos (n+pos);
      if n+pos >= len then None else Some str.[n+pos]
      ) 
    in
    let r =
      try 
	Gram.parse patoline_caml_expr ghost cs
      with
	Loc.Exc_located _ -> raise (Parse_error !last_pos)
    in
    r, !last_pos
  in
  black_box fn (singleton '(') false

let caml_struct = 
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
      try
	Gram.parse patoline_caml_struct ghost cs
      with
	Loc.Exc_located _ -> raise (Parse_error !last_pos)
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

let paragraph_local1 = declare_grammar ()
let paragraph_local2 = declare_grammar ()

let argument =
  glr
     STR("{") l:{w:RE(word_re) -> w} STR("}") -> <:expr@ghost<[tT($str:(String.concat " " [l])$)]>>
  || STR("(") e:caml_expr -> e
  end

let macro =
  glr
     m:RE(macro) args:argument** ->
       let m = String.sub m 1 (String.length m - 1) in
       List.fold_left (fun acc r -> <:expr@ghost<$acc$ $r$>>)  <:expr@ghost<$lid:m$>> args
 end

let _ = set_grammar paragraph_local1 (
  change_layout
    glr
       m:(position macro) p:(position paragraph_local1)? -> 
          (let (_,x,m) = m in
          match p with None -> m | Some (y,_,p) -> 
	    let bl = if y - x >= 1 then <:expr@ghost<[tT" "]>> else <:expr@ghost<[]>> in
            <:expr@ghost<$m$ @ $bl$ @ $p$>>)
    ||  l:{w:RE(word_re) -> w}+ -> <:expr@ghost<[tT($str:(String.concat " " l)$)]>>
      
    end
  blank1)

let _ = set_grammar paragraph_local2 (
  change_layout
    glr
       m:(position macro) p:(position paragraph_local2)?? -> 
          (let (_,x,m) = m in
           match p with None -> m | Some (y,_,p) -> 
	     Printf.fprintf stderr "x: %d, y: %d\n%!" x y;
	   let bl = if y - x >= 1 then <:expr@ghost<[tT" "]>> else <:expr@ghost<[]>> in
           <:expr@ghost<$m$ @ $bl$ @ $p$>>)
    || l:{w:RE(word_re) -> if w = "=<" or w = "-<" then raise Give_up; w}++  -> <:expr@ghost<[tT($str:(String.concat " " l)$)]>>
    end
  blank1)

let paragraph =
    glr
      l:paragraph_local2 -> 
        fun no_indent ->
	  if no_indent then
	    <:str_item@ghost<
              let _ =
		newPar D.structure ~environment:(fun x -> { x with par_indent = [] }) Complete.normal 
		  Patoline_Format.parameters $l$
                >>
	  else
	    <:str_item@ghost<
              let _ =
		newPar D.structure Complete.normal Patoline_Format.parameters $l$
		>>
    end 

let text = declare_grammar ()
let _ = set_grammar text
  glr
    op:RE(section) title:paragraph_local1 cl:RE(section) txt:text ->
      (fun no_indent lvl -> 
       if String.length op <> String.length cl then raise Give_up;
       let numbered = match op.[0], cl.[0] with
	   '=', '=' -> <:expr@ghost<newStruct>>
         | '-', '-' -> <:expr@ghost<newStruct ~numbered:false>>
	 | _ -> raise Give_up
       in
       let l = String.length op - 1 in
       if l > lvl + 1 then failwith "Illegal level skip";
       let res = ref <:str_item@ghost<>> in
       for i = 0 to lvl - l do
	 res := <:str_item@ghost< $!res$ let _ = go_up D.structure>>
       done;
       <:str_item@ghost< $!res$ let _ = $numbered$ D.structure $title$;; $txt true l$ >>)

  || op:RE(op_section) title:paragraph_local2 txt:text cl:RE(cl_section) txt2:text ->
      (fun no_indent lvl -> 
       let numbered = match op.[0], cl.[0] with
	   '=', '=' -> <:expr@ghost<newStruct>>
         | '-', '-' -> <:expr@ghost<newStruct ~numbered:false>>
	 | _ -> raise Give_up
       in
       <:str_item@ghost< let _ = $numbered$ D.structure $title$;; $txt true (lvl+1)$;; let _ = go_up D.structure;; $txt2 false lvl$ >>)

  || STR("\\Caml") STR("(") s:caml_struct  txt:text -> (fun no_indent lvl -> <:str_item@ghost<$s$ $txt no_indent lvl$>>)

  || l:paragraph txt:text  -> (fun no_indent lvl -> <:str_item@ghost<$l no_indent$ $txt false lvl$>>)

  || (empty ()) -> (fun _ _ -> <:str_item@ghost<>>)
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
	let l = parse_channel full_text blank2 ch in
	let all = <:str_item@ghost<
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
	Parse_error n -> Printf.fprintf stderr "Parse error after char %d\n%!" n; exit 1
      | Ambiguity(n,p) -> Printf.fprintf stderr "Ambiguous expression from %d to %d\n%!" n p; exit 1

