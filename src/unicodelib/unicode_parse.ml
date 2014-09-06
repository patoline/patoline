open Pa_ocaml_prelude

let _ = parser_locate locate merge

module Ext = functor(In:Extension) -> 
struct
  include In

let code = parser
  | c:RE("[0-9A-F]+") -> c, <:expr< $int:(UChar.of_int (int_of_string ("0x" ^ c)))$>>

let name = parser
  | n:RE("[A-Za-z0-9-]+") -> 
      let n = String.map (function '-' -> '_' | c -> c) n in
      <:expr<$string:n$ >>
  | s:RE("<[^>\n]*>") -> <:expr<$string:s$>>
  | CHR('(') n:RE("[A-Za-z0-9-]+") CHR(')') ->
      <:expr<$string:n$ >>

let category = parser
  | c:RE("[A-Z][a-z]") -> <:expr<$uid:c$ >>

let combining_class = parser 
  | c:RE("[0-9]+") -> <:expr<Unicode_type.combining_class_of_int $int:(int_of_string c)$>>

let bidirectional_mapping = parser 
  | c:RE("[A-Z]+") -> <:expr<$uid:c$>>

let decomposition_tag = parser
  | CHR('<') t:RE("[a-zA-Z]+") CHR('>') -> <:expr<decomposition_tag_of_string $string:t$>>

let decomposition = parser
  | l:{ t:decomposition_tag -> <:expr<Tag $t$>> | (_,c):code -> <:expr<Char $c$>> }* -> <:expr<$list:l$>>

let int = parser
  | c:RE("[+-]?[0-9]+") -> <:expr<$int:(int_of_string c)$>>

let fraction = parser
  | n:int d:{ CHR('/') d:int }? ->
	    let d = match d with None -> <:expr<1>> 
			       | Some d -> d
	    in <:expr<($n$,$d$)>>
 
let mirrored = parser
  | CHR('Y') -> <:expr<true>> | CHR('N') -> <:expr<false>>

let line = parser
  | (h, cod):code CHR(';')
    name:name+ CHR(';')
    category:category CHR(';')	   
    combining_class:combining_class CHR(';')
    bidirectional_mapping:bidirectional_mapping CHR(';')
    decomposition:decomposition CHR(';')
    decimal:int? CHR(';')
    digit:int? CHR(';')
    numeric:fraction? CHR(';')
    mirrored:mirrored CHR(';')
    oldName:name* CHR(';')
    comments:RE("[^;\n]*") CHR(';')
    uppercase:code? CHR(';')
    lowercase:code? CHR(';')
    titlecase:code? CHR('\n') ->
	Printf.eprintf "\r%s%!" h;
	let f = function None -> <:expr<None>> | Some x -> <:expr<Some $x$>> in
	let f' = function None -> <:expr<None>> | Some (_,x) -> <:expr<Some $x$>> in
	let decimal = f decimal in
	let digit = f digit in
	let numeric = f numeric in
	let uppercase = f' uppercase in
	let lowercase = f' lowercase in
	let titlecase = f' titlecase in
	 <:structure< let _ = Hashtbl.add unicode_table $cod$
	 { code=$cod$;
	   name=$list:name$;
	   category=$category$;
	   combining_class=$combining_class$;
	   bidirectional_mapping=$bidirectional_mapping$;
	   decomposition=$decomposition$;
	   decimal=$decimal$;
	   digit=$digit$;
	   numeric=$numeric$;
	   mirrored=$mirrored$;
	   oldName=$list:oldName$;
	   comments=$string:comments$;
	   uppercase=$uppercase$;
	   lowercase=$lowercase$;
	   titlecase=$titlecase$;
	 }>>

let unicodeData = Glr.change_layout (parser
  ls:{l:line}* ->   <:structure<
  open Unicode_type
  let unicode_table = Hashtbl.create 10001;;
  $(List.flatten ls)$>>) (Glr.blank_regexp (Str.regexp "[ \t]*"))


 let _ = 
    entry_points:= 
      (".txt", `Impl unicodeData) :: !entry_points
				     
end

let _ = register_extension (module Ext : FExt)
