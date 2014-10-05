open Pa_ocaml_prelude
open Unicode_type

#define LOCATE locate

module Ext = functor(In:Extension) -> 
struct
  include In

let code = parser
  | c:RE("[0-9A-F]+") -> int_of_string ("0x" ^ c) 

let string_table = Hashtbl.create 1001 

let hash_cons str =
  try Hashtbl.find string_table str
  with Not_found -> 
    Hashtbl.add string_table str str;
    str

let name = parser
  | n:RE("[A-Za-z0-9-]+") -> 
      <:expr<$string:(hash_cons n)$ >>
  | s:RE("<[^>\n]*>") -> <:expr<$string:(hash_cons s)$>>
  | CHR('(') n:RE("[A-Za-z0-9-]+") CHR(')') ->
      <:expr<$string:(hash_cons n)$ >>

let category = parser
  | c:RE("[A-Z][a-z]") -> c

let combining_class = parser 
  | c:RE("[0-9]+") -> 
      let n = int_of_string c in
      if n >= 10 && n <= 199 then
	<:expr<Fixed_position $int:n$>>
      else
	<:expr<$uid:(cci n)$>>

let bidirectional_mapping = parser 
  | c:RE("[A-Z]+") -> c

let decomposition_tag = parser
  | CHR('<') t:RE("[a-zA-Z]+") CHR('>') -> t

let decomposition = parser
  | l:{ t:decomposition_tag -> <:expr<Tag (dts $string:t$)>> | c:code -> <:expr<Char $int:c$>> }* -> <:expr< $list:l$ >>

let int = parser
  | c:RE("[+-]?[0-9]+") -> int_of_string c

let fraction = parser
  | n:int d:{ CHR('/') d:int }? ->
	    let d = match d with None -> 1
			       | Some d -> d
	    in n,d
 
let mirrored = parser
  | CHR('Y') -> <:expr<true>> | CHR('N') -> <:expr<false>>

let line = parser
  | code:code CHR(';')
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
        if code mod 16 = 0 then Printf.eprintf "\r%x%!" code;
	let fi = function None -> <:expr<None>> | Some x -> <:expr<Some $int:x$>> in
	let ff = function None -> <:expr<None>> | Some (n,d) -> <:expr<Some ($int:n$,$int:d$)>> in
	let decimal = fi decimal in
	let digit = fi digit in
	let numeric = ff numeric in
	let uppercase = fi uppercase in
	let lowercase = fi lowercase in
	let titlecase = fi titlecase in
	<:structure< let _ = fn
	 $int:code$ $list:name$ $uid:category$ $combining_class$
	 $uid:bidirectional_mapping$ $decomposition$
	 $decimal$ $digit$ $numeric$ $mirrored$
	 $list:oldName$ $string:comments$
	 $uppercase$ $lowercase$ $titlecase$>> 

let unicodeData = Decap.change_layout (parser
  ls:{l:line}* ->   <:structure<
  open Unicode_type
  let unicode_table = Hashtbl.create 10001;;
  let fn a b c d e f g h i j k l m n o
      = Hashtbl.add unicode_table a {
	   code=a;
	   name=b;
	   category=c;
	   combining_class=d;
	   bidirectional_mapping=e;
	   decomposition=f;
	   decimal=g;
	   digit=h;
	   numeric=i;
	   mirrored=j;
	   oldName=k;
	   comments=l;
	   uppercase=m;
	   lowercase=n;
	   titlecase=o;
	 };;
  $(List.flatten ls)$>>) (Decap.blank_regexp (Str.regexp "[ \t]*"))


 let _ = 
    entry_points:= 
      (".txt", `Impl unicodeData) :: !entry_points
				     
end

let _ = register_extension (module Ext : FExt)
