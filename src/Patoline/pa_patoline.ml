(* ocamlc -pp ../pa_glr -I .. -I +camlp4 dynlink.cma camlp4lib.cma str.cma umap.cmo glr.cmo -o pato pato.ml 
    ocamlfind ocamlopt -pp ./pato -package Typography,Typography.Pdf -linkpkg  -impl toto.txt
*)

open Glr
open Camlp4.PreCast
open Loc

let blank1 = blank_regexp (Str.regexp "[ \t\r]*\\([\n][ \t\r]*\\)?")
let blank2 = blank_regexp (Str.regexp "[ \n\t\r]*")

let word_re = "[^ \t\r\n]+"
 
let paragraph =
  change_layout
    glr
      l:{w:RE(word_re) -> w}++  ->  <:str_item@ghost<
         let _ =
	   newPar D.structure ~environment:(fun x -> { x with par_indent = [] }) Complete.normal 
	     Patoline_Format.parameters ( ( [tT($str:(String.concat " " l)$) ] ) )
                >>
    end 
  blank1

let text =
    glr
      ll:{l:paragraph -> l}* EOF -> ll
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
	let l = parse_channel text blank2 ch in
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
	    $list:l$
         let _ = D.structure:=follow (top !D.structure) (List.rev temp1)
         end;;
         let _ = $lid:"cache_"^basename$ :=[||];;
         let _ = $lid:"mcache_"^basename$ :=[||];;
        >>
	 in
(*	Printf.printf "=> %d paragraphs\n" (List.length l)*)
	Printers.DumpOCamlAst.print_implem all
      with
	Parse_error n -> Printf.fprintf stderr "Parse error after char %d\n%!" n
      | Ambiguity(n,p) -> Printf.fprintf stderr "Ambiguous expression from %d to %d\n%!" n p

