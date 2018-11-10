open Earley_core
open Earley_ocaml
open Pa_ocaml_prelude
open UCharInfo

#define LOCATE locate

let general_category_of_string = function
  | "Lu" -> Lu | "Ll" -> Ll | "Lt" -> Lt | "Mn" -> Mn | "Mc" -> Mc
  | "Me" -> Me | "Nd" -> Nd | "Nl" -> Nl | "No" -> No | "Zs" -> Zs
  | "Zl" -> Zl | "Zp" -> Zp | "Cc" -> Cc | "Cf" -> Cf | "Cs" -> Cs
  | "Co" -> Co | "Cn" -> Cn
  | "Lm" -> Lm | "Lo" -> Lo | "Pc" -> Pc | "Pd" -> Pd | "Ps" -> Ps
  | "Pe" -> Pe | "Pi" -> Pi | "Pf" -> Pf | "Po" -> Po | "Sm" -> Sm
  | "Sc" -> Sc | "Sk" -> Sk | "So" -> So
  | s -> Printf.eprintf "Missing: %s\n%!" s; assert false

let combining_class_of_int = function
  | n when n >= 10 && n <= 199
        -> Fixed_position n
  | 0   -> Spacing_split_enclosing_reordrant_and_Tibetan_subjoined
  | 1   -> Overlays_and_interior
  | 7   -> Nuktas
  | 8   -> Hiragana_Katakana_voicing_marks
  | 9   -> Viramas
  | 200 -> Below_left_attached
  | 202 -> Below_attached
  | 204 -> Below_right_attached
  | 208 -> Left_attached
  | 210 -> Right_attached
  | 212 -> Above_left_attached
  | 214 -> Above_attached
  | 216 -> Above_right_attached
  | 218 -> Below_left
  | 220 -> Below
  | 222 -> Below_right
  | 224 -> Left
  | 226 -> Right
  | 228 -> Above_left
  | 230 -> Above
  | 232 -> Above_right
  | 233 -> Double_below
  | 234 -> Double_above
  | 240 -> Below_iota_subscript
  | i -> Printf.eprintf "Missing: %i\n%!" i; assert false


let bidirectional_mapping_of_string = function
  | "L"   -> L
  | "LRE" -> LRE
  | "LRO" -> LRO
  | "LRI" -> LRI
  | "R"   -> R
  | "AL"  -> AL
  | "RLE" -> RLE
  | "RLO" -> RLO
  | "RLI" -> RLI
  | "PDF" -> PDF
  | "PDI" -> PDI
  | "FSI" -> FSI
  | "EN"  -> EN
  | "ES"  -> ES
  | "ET"  -> ET
  | "AN"  -> AN
  | "CS"  -> CS
  | "NSM" -> NSM
  | "BN"  -> BN
  | "B"   -> B
  | "S"   -> S
  | "WS"  -> WS
  | "ON"  -> ON
  | s -> Printf.eprintf "Missing: %s\n%!" s; assert false

let decomposition_atom_of_string = function
  | "font"     -> Font
  | "noBreak"  -> NoBreak
  | "initial"  -> Initial
  | "medial"   -> Medial
  | "final"    -> Final
  | "isolated" -> Isolated
  | "circle"   -> Circle
  | "super"    -> Super
  | "sub"      -> Sub
  | "vertical" -> Vertical
  | "wide"     -> Wide
  | "narrow"   -> Narrow
  | "small"    -> Small
  | "square"   -> Square
  | "fraction" -> Fraction
  | "compat"   -> Compat
  | s -> Printf.eprintf "Missing: %s\n%!" s; assert false

let code =
  parser
  | c:''[0-9A-F]+'' -> int_of_string ("0x" ^ c)

let integer =
  parser
  | c:''[+-]?[0-9]+'' -> int_of_string c

let integer64 =
  parser
  | c:''[+-]?[0-9]+'' -> Int64.of_string c

let fraction =
  parser
  | n:integer64 d:{'/' d:integer}?[1]
 
let category =
  parser
  | c:''[A-Z][a-z]'' -> general_category_of_string c

let bidirectional_mapping =
  parser 
  | c:''[A-Z]+'' -> bidirectional_mapping_of_string c

let combining_class =
  parser 
  | c:''[0-9]+'' -> combining_class_of_int (int_of_string c)

let mirrored =
  parser
  | 'Y' -> true
  | 'N' -> false

let decomposition =
  let decomposition_tag =
    parser
    | '<' t:''[a-zA-Z]+'' '>' -> decomposition_atom_of_string t
    | c:code -> Char c
  in parser | decomposition_tag*

let name =
  parser
  | n:''[()A-Za-z0-9-]+''
  | n:"<control>"
(*  | '(' n:"<control>" ')' *)

let old_name =
  parser | n:''[A-Za-z0-9 ()-]*'' -> n

type kind = Single of int * char_description
          | Range  of int * int * (int -> char_description)

let single =
  parser
  | code:code ';'
    name:name+ ';'
    gen_cat:category ';'    
    c_cl:combining_class ';'
    bid_map:bidirectional_mapping ';'
    dec:decomposition ';'
    decimal:integer? ';'
    digit:integer? ';'
    numeric:fraction? ';'
    mirrored:mirrored ';'
    oldName:old_name ';'
    comments:''[^;\n]*'' ';'
    uppercase:code? ';'
    lowercase:code? ';' 
    titlecase:code? '\r'? '\n' ->
      let desc =
        { code                  = code
        ; name                  = name
        ; general_category      = gen_cat
        ; combining_class       = c_cl
        ; bidirectional_mapping = bid_map
        ; decomposition         = dec
        ; decimal_digit_value   = decimal
        ; digit_value           = digit
        ; numeric_value         = numeric
        ; mirrored              = mirrored
        ; oldName               = oldName
        ; comments              = comments
        ; uppercase             = uppercase
        ; lowercase             = lowercase
        ; titlecase             = titlecase
        }
      in Single (code, desc)

let range =
  parser
  | firstcode:code ';'
    '<' gname:name+ ", First>" ';'
    gen_cat:category ';'    
    c_cl:combining_class ';'
    bid_map:bidirectional_mapping ';'
    dec:decomposition ';'
    decimal:integer? ';'
    digit:integer? ';'
    numeric:fraction? ';'
    mirrored:mirrored ';'
    oldName:old_name ';'
    comments:''[^;\n]*'' ';'
    uppercase:code? ';'
    lowercase:code? ';' 
    titlecase:code? '\r'? '\n'
    lastcode:code ';'
    '<' _:name+ ", Last>" ';'
    _:category ';'    
    _:combining_class ';'
    _:bidirectional_mapping ';'
    _:decomposition ';'
    _:integer? ';'
    _:integer? ';'
    _:fraction? ';'
    _:mirrored ';'
    _:old_name ';'
    _:''[^;\n]*'' ';'
    _:code? ';'
    _:code? ';' 
    _:code? '\r'? '\n' ->
      let build_desc c =
        if c < firstcode || c > lastcode then assert false;
        { code                  = c
        ; name                  = gname
        ; general_category      = gen_cat
        ; combining_class       = c_cl
        ; bidirectional_mapping = bid_map
        ; decomposition         = dec
        ; decimal_digit_value   = decimal
        ; digit_value           = digit
        ; numeric_value         = numeric
        ; mirrored              = mirrored
        ; oldName               = oldName
        ; comments              = comments
        ; uppercase             = uppercase
        ; lowercase             = lowercase
        ; titlecase             = titlecase
        }
      in Range (firstcode, lastcode, build_desc)

let file_contents =
  parser
  | l:{single | range }* EOF

let blank = Earley_str.blank_regexp "[ \t]*"
let parse = Earley.parse_file file_contents blank

let flatten_data ld =
  let rec flatten_data ld acc =
    match ld with
    | []                   -> acc
    | Single (k,v) :: ls   -> flatten_data ls ((k,v) :: acc)
    | Range (f,l,bf) :: ls ->
        if f > l then flatten_data ls acc
        else flatten_data (Range (f+1, l, bf) :: ls) ((f, bf f) :: acc)
  in flatten_data ld []

let _ =
  (* Command line args *)
  if Array.length Sys.argv != 3 then
    begin
      let pn = Sys.argv.(0) in
      Printf.eprintf "Usage: %s <path_to_UnicodeData.txt> <output_file>" pn;
      exit 1
    end;
  let infile = Sys.argv.(1) in
  let outfile = Sys.argv.(2) in

  (* Parsing and preparing the data *)
  let data = Earley.handle_exception parse infile in
  let data = flatten_data data in

  (* Adding the data to the permanent map *)
  PermanentMap.new_map outfile; (* Fails if file exists *)
  let m = PermanentMap.open_map outfile in
  PermanentMap.add_many m data;

  (* Compacting *)
  PermanentMap.compact m;
  PermanentMap.close_map m
