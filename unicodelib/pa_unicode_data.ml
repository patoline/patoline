open Unicodelib
open Earley_core
open UCharInfo
let general_category_of_string =
  function
  | "Lu" -> Lu
  | "Ll" -> Ll
  | "Lt" -> Lt
  | "Mn" -> Mn
  | "Mc" -> Mc
  | "Me" -> Me
  | "Nd" -> Nd
  | "Nl" -> Nl
  | "No" -> No
  | "Zs" -> Zs
  | "Zl" -> Zl
  | "Zp" -> Zp
  | "Cc" -> Cc
  | "Cf" -> Cf
  | "Cs" -> Cs
  | "Co" -> Co
  | "Cn" -> Cn
  | "Lm" -> Lm
  | "Lo" -> Lo
  | "Pc" -> Pc
  | "Pd" -> Pd
  | "Ps" -> Ps
  | "Pe" -> Pe
  | "Pi" -> Pi
  | "Pf" -> Pf
  | "Po" -> Po
  | "Sm" -> Sm
  | "Sc" -> Sc
  | "Sk" -> Sk
  | "So" -> So
  | s -> (Printf.eprintf "Missing: %s\n%!" s; assert false)
let combining_class_of_int =
  function
  | n when (n >= 10) && (n <= 199) -> Fixed_position n
  | 0 -> Spacing_split_enclosing_reordrant_and_Tibetan_subjoined
  | 1 -> Overlays_and_interior
  | 7 -> Nuktas
  | 8 -> Hiragana_Katakana_voicing_marks
  | 9 -> Viramas
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
  | i -> (Printf.eprintf "Missing: %i\n%!" i; assert false)
let bidirectional_mapping_of_string =
  function
  | "L" -> L
  | "LRE" -> LRE
  | "LRO" -> LRO
  | "LRI" -> LRI
  | "R" -> R
  | "AL" -> AL
  | "RLE" -> RLE
  | "RLO" -> RLO
  | "RLI" -> RLI
  | "PDF" -> PDF
  | "PDI" -> PDI
  | "FSI" -> FSI
  | "EN" -> EN
  | "ES" -> ES
  | "ET" -> ET
  | "AN" -> AN
  | "CS" -> CS
  | "NSM" -> NSM
  | "BN" -> BN
  | "B" -> B
  | "S" -> S
  | "WS" -> WS
  | "ON" -> ON
  | s -> (Printf.eprintf "Missing: %s\n%!" s; assert false)
let decomposition_atom_of_string =
  function
  | "font" -> Font
  | "noBreak" -> NoBreak
  | "initial" -> Initial
  | "medial" -> Medial
  | "final" -> Final
  | "isolated" -> Isolated
  | "circle" -> Circle
  | "super" -> Super
  | "sub" -> Sub
  | "vertical" -> Vertical
  | "wide" -> Wide
  | "narrow" -> Narrow
  | "small" -> Small
  | "square" -> Square
  | "fraction" -> Fraction
  | "compat" -> Compat
  | s -> (Printf.eprintf "Missing: %s\n%!" s; assert false)
let code =
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"[0-9A-F]+" "[0-9A-F]+" (fun group -> group 0))
    (Earley_core.Earley.empty (fun c -> int_of_string ("0x" ^ c)))
let integer =
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"[+-]?[0-9]+" "[+-]?[0-9]+"
       (fun group -> group 0))
    (Earley_core.Earley.empty (fun c -> int_of_string c))
let integer64 =
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"[+-]?[0-9]+" "[+-]?[0-9]+"
       (fun group -> group 0))
    (Earley_core.Earley.empty (fun c -> Int64.of_string c))
let fraction =
  Earley_core.Earley.fsequence integer64
    (Earley_core.Earley.fsequence
       (Earley_core.Earley.option 1
          (Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '/' '/')
             (Earley_core.Earley.fsequence integer
                (Earley_core.Earley.empty (fun d -> d)))))
       (Earley_core.Earley.empty (fun d -> fun n -> (n, d))))
let category =
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"[A-Z][a-z]" "[A-Z][a-z]" (fun group -> group 0))
    (Earley_core.Earley.empty (fun c -> general_category_of_string c))
let bidirectional_mapping =
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"[A-Z]+" "[A-Z]+" (fun group -> group 0))
    (Earley_core.Earley.empty (fun c -> bidirectional_mapping_of_string c))
let combining_class =
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"[0-9]+" "[0-9]+" (fun group -> group 0))
    (Earley_core.Earley.empty
       (fun c -> combining_class_of_int (int_of_string c)))
let mirrored =
  Earley_core.Earley.alternatives
    (List.cons
       (Earley_core.Earley.fsequence_ignore (Earley_core.Earley.char 'N' 'N')
          (Earley_core.Earley.empty false))
       (List.cons
          (Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char 'Y' 'Y')
             (Earley_core.Earley.empty true)) []))
let decomposition =
  let decomposition_tag =
    Earley_core.Earley.alternatives
      (List.cons
         (Earley_core.Earley.fsequence code
            (Earley_core.Earley.empty (fun c -> Char c)))
         (List.cons
            (Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.char '<' '<')
               (Earley_core.Earley.fsequence
                  (Earley_str.regexp ~name:"[a-zA-Z]+" "[a-zA-Z]+"
                     (fun group -> group 0))
                  (Earley_core.Earley.fsequence_ignore
                     (Earley_core.Earley.char '>' '>')
                     (Earley_core.Earley.empty
                        (fun t -> decomposition_atom_of_string t))))) [])) in
  Earley_core.Earley.apply (fun f -> f [])
    (Earley_core.Earley.fixpoint' (fun l -> l) decomposition_tag
       (fun x -> fun f -> fun l -> f (List.cons x l)))
let name =
  Earley_core.Earley.alternatives
    (List.cons (Earley_core.Earley.string "<control>" "<control>")
       (List.cons
          (Earley_str.regexp ~name:"[()A-Za-z0-9-]+" "[()A-Za-z0-9-]+"
             (fun group -> group 0)) []))
let old_name =
  Earley_str.regexp ~name:"[A-Za-z0-9 ()-]*" "[A-Za-z0-9 ()-]*"
    (fun group -> group 0)
type kind =
  | Single of int * char_description 
  | Range of int * int * (int -> char_description) 
let single =
  Earley_core.Earley.fsequence code
    (Earley_core.Earley.fsequence_ignore (Earley_core.Earley.char ';' ';')
       (Earley_core.Earley.fsequence
          (Earley_core.Earley.apply (fun f -> f [])
             (Earley_core.Earley.fixpoint1' (fun l -> l) name
                (fun x -> fun f -> fun l -> f (List.cons x l))))
          (Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char ';' ';')
             (Earley_core.Earley.fsequence category
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char ';' ';')
                   (Earley_core.Earley.fsequence combining_class
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char ';' ';')
                         (Earley_core.Earley.fsequence bidirectional_mapping
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.char ';' ';')
                               (Earley_core.Earley.fsequence decomposition
                                  (Earley_core.Earley.fsequence_ignore
                                     (Earley_core.Earley.char ';' ';')
                                     (Earley_core.Earley.fsequence
                                        (Earley_core.Earley.option None
                                           (Earley_core.Earley.apply
                                              (fun x -> Some x) integer))
                                        (Earley_core.Earley.fsequence_ignore
                                           (Earley_core.Earley.char ';' ';')
                                           (Earley_core.Earley.fsequence
                                              (Earley_core.Earley.option None
                                                 (Earley_core.Earley.apply
                                                    (fun x -> Some x) integer))
                                              (Earley_core.Earley.fsequence_ignore
                                                 (Earley_core.Earley.char ';'
                                                    ';')
                                                 (Earley_core.Earley.fsequence
                                                    (Earley_core.Earley.option
                                                       None
                                                       (Earley_core.Earley.apply
                                                          (fun x -> Some x)
                                                          fraction))
                                                    (Earley_core.Earley.fsequence_ignore
                                                       (Earley_core.Earley.char
                                                          ';' ';')
                                                       (Earley_core.Earley.fsequence
                                                          mirrored
                                                          (Earley_core.Earley.fsequence_ignore
                                                             (Earley_core.Earley.char
                                                                ';' ';')
                                                             (Earley_core.Earley.fsequence
                                                                old_name
                                                                (Earley_core.Earley.fsequence_ignore
                                                                   (Earley_core.Earley.char
                                                                    ';' ';')
                                                                   (Earley_core.Earley.fsequence
                                                                    (Earley_str.regexp
                                                                    ~name:"[^;\\n]*"
                                                                    "[^;\n]*"
                                                                    (fun
                                                                    group ->
                                                                    group 0))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    (Earley_core.Earley.char
                                                                    '\r' '\r')))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '\n' '\n')
                                                                    (Earley_core.Earley.empty
                                                                    (fun
                                                                    _default_0
                                                                    ->
                                                                    fun
                                                                    titlecase
                                                                    ->
                                                                    fun
                                                                    lowercase
                                                                    ->
                                                                    fun
                                                                    uppercase
                                                                    ->
                                                                    fun
                                                                    comments
                                                                    ->
                                                                    fun
                                                                    oldName
                                                                    ->
                                                                    fun
                                                                    mirrored
                                                                    ->
                                                                    fun
                                                                    numeric
                                                                    ->
                                                                    fun digit
                                                                    ->
                                                                    fun
                                                                    decimal
                                                                    ->
                                                                    fun dec
                                                                    ->
                                                                    fun
                                                                    bid_map
                                                                    ->
                                                                    fun c_cl
                                                                    ->
                                                                    fun
                                                                    gen_cat
                                                                    ->
                                                                    fun name
                                                                    ->
                                                                    fun code
                                                                    ->
                                                                    let desc
                                                                    =
                                                                    {
                                                                    code;
                                                                    name;
                                                                    general_category
                                                                    = gen_cat;
                                                                    combining_class
                                                                    = c_cl;
                                                                    bidirectional_mapping
                                                                    = bid_map;
                                                                    decomposition
                                                                    = dec;
                                                                    decimal_digit_value
                                                                    = decimal;
                                                                    digit_value
                                                                    = digit;
                                                                    numeric_value
                                                                    = numeric;
                                                                    mirrored;
                                                                    oldName;
                                                                    comments;
                                                                    uppercase;
                                                                    lowercase;
                                                                    titlecase
                                                                    } in
                                                                    Single
                                                                    (code,
                                                                    desc)))))))))))))))))))))))))))))))))
let range =
  Earley_core.Earley.fsequence code
    (Earley_core.Earley.fsequence_ignore (Earley_core.Earley.char ';' ';')
       (Earley_core.Earley.fsequence_ignore (Earley_core.Earley.char '<' '<')
          (Earley_core.Earley.fsequence
             (Earley_core.Earley.apply (fun f -> f [])
                (Earley_core.Earley.fixpoint1' (fun l -> l) name
                   (fun x -> fun f -> fun l -> f (List.cons x l))))
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.string ", First>" ", First>")
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.char ';' ';')
                   (Earley_core.Earley.fsequence category
                      (Earley_core.Earley.fsequence_ignore
                         (Earley_core.Earley.char ';' ';')
                         (Earley_core.Earley.fsequence combining_class
                            (Earley_core.Earley.fsequence_ignore
                               (Earley_core.Earley.char ';' ';')
                               (Earley_core.Earley.fsequence
                                  bidirectional_mapping
                                  (Earley_core.Earley.fsequence_ignore
                                     (Earley_core.Earley.char ';' ';')
                                     (Earley_core.Earley.fsequence
                                        decomposition
                                        (Earley_core.Earley.fsequence_ignore
                                           (Earley_core.Earley.char ';' ';')
                                           (Earley_core.Earley.fsequence
                                              (Earley_core.Earley.option None
                                                 (Earley_core.Earley.apply
                                                    (fun x -> Some x) integer))
                                              (Earley_core.Earley.fsequence_ignore
                                                 (Earley_core.Earley.char ';'
                                                    ';')
                                                 (Earley_core.Earley.fsequence
                                                    (Earley_core.Earley.option
                                                       None
                                                       (Earley_core.Earley.apply
                                                          (fun x -> Some x)
                                                          integer))
                                                    (Earley_core.Earley.fsequence_ignore
                                                       (Earley_core.Earley.char
                                                          ';' ';')
                                                       (Earley_core.Earley.fsequence
                                                          (Earley_core.Earley.option
                                                             None
                                                             (Earley_core.Earley.apply
                                                                (fun x ->
                                                                   Some x)
                                                                fraction))
                                                          (Earley_core.Earley.fsequence_ignore
                                                             (Earley_core.Earley.char
                                                                ';' ';')
                                                             (Earley_core.Earley.fsequence
                                                                mirrored
                                                                (Earley_core.Earley.fsequence_ignore
                                                                   (Earley_core.Earley.char
                                                                    ';' ';')
                                                                   (Earley_core.Earley.fsequence
                                                                    old_name
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_str.regexp
                                                                    ~name:"[^;\\n]*"
                                                                    "[^;\n]*"
                                                                    (fun
                                                                    group ->
                                                                    group 0))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    (Earley_core.Earley.char
                                                                    '\r' '\r')))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '\n' '\n')
                                                                    (Earley_core.Earley.fsequence
                                                                    code
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '<' '<')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.apply
                                                                    (fun f ->
                                                                    f [])
                                                                    (Earley_core.Earley.fixpoint1'
                                                                    (fun l ->
                                                                    l) name
                                                                    (fun x ->
                                                                    fun f ->
                                                                    fun l ->
                                                                    f
                                                                    (List.cons
                                                                    x l))))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.string
                                                                    ", Last>"
                                                                    ", Last>")
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    category
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    combining_class
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    bidirectional_mapping
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    decomposition
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    integer))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    integer))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    fraction))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    mirrored
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    old_name
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_str.regexp
                                                                    ~name:"[^;\\n]*"
                                                                    "[^;\n]*"
                                                                    (fun
                                                                    group ->
                                                                    group 0))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    ';' ';')
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    code))
                                                                    (Earley_core.Earley.fsequence
                                                                    (Earley_core.Earley.option
                                                                    None
                                                                    (Earley_core.Earley.apply
                                                                    (fun x ->
                                                                    Some x)
                                                                    (Earley_core.Earley.char
                                                                    '\r' '\r')))
                                                                    (Earley_core.Earley.fsequence_ignore
                                                                    (Earley_core.Earley.char
                                                                    '\n' '\n')
                                                                    (Earley_core.Earley.empty
                                                                    (fun
                                                                    _default_0
                                                                    ->
                                                                    fun
                                                                    lastcode
                                                                    ->
                                                                    fun
                                                                    _default_1
                                                                    ->
                                                                    fun
                                                                    titlecase
                                                                    ->
                                                                    fun
                                                                    lowercase
                                                                    ->
                                                                    fun
                                                                    uppercase
                                                                    ->
                                                                    fun
                                                                    comments
                                                                    ->
                                                                    fun
                                                                    oldName
                                                                    ->
                                                                    fun
                                                                    mirrored
                                                                    ->
                                                                    fun
                                                                    numeric
                                                                    ->
                                                                    fun digit
                                                                    ->
                                                                    fun
                                                                    decimal
                                                                    ->
                                                                    fun dec
                                                                    ->
                                                                    fun
                                                                    bid_map
                                                                    ->
                                                                    fun c_cl
                                                                    ->
                                                                    fun
                                                                    gen_cat
                                                                    ->
                                                                    fun gname
                                                                    ->
                                                                    fun
                                                                    firstcode
                                                                    ->
                                                                    let build_desc
                                                                    c =
                                                                    if
                                                                    (c <
                                                                    firstcode)
                                                                    ||
                                                                    (c >
                                                                    lastcode)
                                                                    then
                                                                    assert
                                                                    false;
                                                                    {
                                                                    code = c;
                                                                    name =
                                                                    gname;
                                                                    general_category
                                                                    = gen_cat;
                                                                    combining_class
                                                                    = c_cl;
                                                                    bidirectional_mapping
                                                                    = bid_map;
                                                                    decomposition
                                                                    = dec;
                                                                    decimal_digit_value
                                                                    = decimal;
                                                                    digit_value
                                                                    = digit;
                                                                    numeric_value
                                                                    = numeric;
                                                                    mirrored;
                                                                    oldName;
                                                                    comments;
                                                                    uppercase;
                                                                    lowercase;
                                                                    titlecase
                                                                    } in
                                                                    Range
                                                                    (firstcode,
                                                                    lastcode,
                                                                    build_desc))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
let file_contents =
  Earley_core.Earley.fsequence
    (Earley_core.Earley.apply (fun f -> f [])
       (Earley_core.Earley.fixpoint' (fun l -> l)
          (Earley_core.Earley.alternatives
             (List.cons range (List.cons single [])))
          (fun x -> fun f -> fun l -> f (List.cons x l))))
    (Earley_core.Earley.fsequence_ignore (Earley_core.Earley.eof ())
       (Earley_core.Earley.empty (fun l -> l)))
let blank = Earley_str.blank_regexp "[ \t]*"
let parse = Earley.parse_file file_contents blank
let flatten_data ld =
  let rec flatten_data ld acc =
    match ld with
    | [] -> acc
    | (Single (k, v))::ls -> flatten_data ls ((k, v) :: acc)
    | (Range (f, l, bf))::ls ->
        if f > l
        then flatten_data ls acc
        else
          flatten_data ((Range ((f + 1), l, bf)) :: ls) ((f, (bf f)) :: acc) in
  flatten_data ld []
let _ =
  if (Array.length Sys.argv) != 3
  then
    (let pn = Sys.argv.(0) in
     Printf.eprintf "Usage: %s <path_to_UnicodeData.txt> <output_file>" pn;
     exit 1);
  (let infile = Sys.argv.(1) in
   let outfile = Sys.argv.(2) in
   let data = Earley.handle_exception parse infile in
   let data = flatten_data data in
   Permap.new_map outfile;
   (let m = Permap.open_map outfile in
    Permap.add_many m data; Permap.compact m; Permap.close_map m))
