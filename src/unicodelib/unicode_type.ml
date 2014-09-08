type category = 
(* normative *)
  | Lu | Ll | Lt | Mn | Mc | Me | Nd | Nl | No
  | Zs | Zl | Zp | Cc | Cf | Cs | Co | Cn
(* informative *)
  | Lm | Lo | Pc | Pd | Ps | Pe | Pi | Pf | Po
  | Sm | Sc | Sk | So

type combining_class =
| 	Spacing_split_enclosing_reordrant_and_Tibetan_subjoined
| 	Overlays_and_interior
| 	Nuktas
| 	Hiragana_Katakana_voicing_marks
| 	Viramas
| 	Fixed_position of int
| 	Below_left_attached
| 	Below_attached
| 	Below_right_attached
| 	Left_attached
| 	Right_attached
| 	Above_left_attached
| 	Above_attached
| 	Above_right_attached
| 	Below_left
| 	Below
| 	Below_right
| 	Left
| 	Right
| 	Above_left
| 	Above
| 	Above_right
| 	Double_below
| 	Double_above
| 	Below_iota_subscript

let combining_class_of_int = function
    | 0 -> 	Spacing_split_enclosing_reordrant_and_Tibetan_subjoined
    | 1 -> 	Overlays_and_interior
    | 7 -> 	Nuktas
    | 8 -> 	Hiragana_Katakana_voicing_marks
    | 9 -> 	Viramas
    | n when n >= 10 && n <= 199 -> Fixed_position n
    | 200 -> 	Below_left_attached
    | 202 -> 	Below_attached
    | 204 -> 	Below_right_attached
    | 208 -> 	Left_attached
    | 210 -> 	Right_attached
    | 212 -> 	Above_left_attached
    | 214 -> 	Above_attached
    | 216 -> 	Above_right_attached
    | 218 -> 	Below_left
    | 220 -> 	Below
    | 222 -> 	Below_right
    | 224 -> 	Left
    | 226 -> 	Right
    | 228 -> 	Above_left
    | 230 -> 	Above
    | 232 ->	Above_right
    | 233 -> 	Double_below
    | 234 -> 	Double_above
    | 240 -> 	Below_iota_subscript
    | _ -> assert false

let cci = function
    | 0 -> 	"Spacing_split_enclosing_reordrant_and_Tibetan_subjoined"
    | 1 -> 	"Overlays_and_interior"
    | 7 -> 	"Nuktas"
    | 8 -> 	"Hiragana_Katakana_voicing_marks"
    | 9 -> 	"Viramas"
    | n when n >= 10 && n <= 199 -> "Fixed_position"
    | 199 -> 	"End_of_fixed_position_classes"
    | 200 -> 	"Below_left_attached"
    | 202 -> 	"Below_attached"
    | 204 -> 	"Below_right_attached"
    | 208 -> 	"Left_attached"
    | 210 -> 	"Right_attached"
    | 212 -> 	"Above_left_attached"
    | 214 -> 	"Above_attached"
    | 216 -> 	"Above_right_attached"
    | 218 -> 	"Below_left"
    | 220 -> 	"Below"
    | 222 -> 	"Below_right"
    | 224 -> 	"Left"
    | 226 -> 	"Right"
    | 228 -> 	"Above_left"
    | 230 -> 	"Above"
    | 232 ->	"Above_right"
    | 233 -> 	"Double_below"
    | 234 -> 	"Double_above"
    | 240 -> 	"Below_iota_subscript"
    | _ -> assert false

let combining_class_of_string s =
  combining_class_of_int (int_of_string s)

type bidirectional_mapping =
| L (*Left-to-Right*)
| LRE (*Left-to-Right Embedding*)
| LRO (*Left-to-Right Override*)
| LRI (*Left-to-Right Isolate*)
| R (*Right-to-Left*)
| AL (*Right-to-Left Arabic*)
| RLE (*Right-to-Left Embedding*)
| RLO (*Right-to-Left Override*)
| RLI (*Right-to-Left Isolate*)
| PDF (*Pop Directional Format*)
| PDI (*Pop Directional Isolate*)
| FSI (*??? Isolate*)
| EN (*European Number*)
| ES (*European Number Separator*)
| ET (*European Number Terminator*)
| AN (*Arabic Number*)
| CS (*Common Number Separator*)
| NSM (*Non-Spacing Mark*)
| BN (*Boundary Neutral*)
| B (*Paragraph Separator*)
| S (*Segment Separator*)
| WS (*Whitespace*)
| ON (*Other Neutral*)

let bidirectional_mapping_of_string = function
    | "L" -> L (*Left-to-Right*)
    | "LRE" -> LRE (*Left-to-Right Embedding*)
    | "LRO" -> LRO (*Left-to-Right Override*)
    | "LRI" -> LRI (*Left-to-Right Isolate*)
    | "R" -> R (*Right-to-Left*)
    | "AL" -> AL (*Right-to-Left Arabic*)
    | "RLE" -> RLE (*Right-to-Left Embedding*)
    | "RLO" -> RLO (*Right-to-Left Override*)
    | "RLI" -> RLI (*Right-to-Left Isolate*)
    | "PDF" -> PDF (*Pop Directional Format*)
    | "EN" -> EN (*European Number*)
    | "ES" -> ES (*European Number Separator*)
    | "ET" -> ET (*European Number Terminator*)
    | "AN" -> AN (*Arabic Number*)
    | "CS" -> CS (*Common Number Separator*)
    | "NSM" -> NSM (*Non-Spacing Mark*)
    | "BN" -> BN (*Boundary Neutral*)
    | "B" -> B (*Paragraph Separator*)
    | "S" -> S (*Segment Separator*)
    | "WS" -> WS (*Whitespace*)
    | "ON" -> ON (*Other Neutral*)
    | _ -> assert false

type decomposition_tag =
    | Font   	(*A font variant (e.g. a blackletter form).*)
    | NoBreak  	(*A no-break version of a space or hyphen.*)
    | Initial   (*An initial presentation form (Arabic).*)
    | Medial   	(*A medial presentation form (Arabic).*)
    | Final   	(*A final presentation form (Arabic).*)
    | Isolated  (*An isolated presentation form (Arabic).*)
    | Circle   	(*An encircled form.*)
    | Super   	(*A superscript form.*)
    | Sub   	(*A subscript form.*)
    | Vertical  (*A vertical layout presentation form.*)
    | Wide   	(*A wide (or zenkaku) compatibility character.*)
    | Narrow   	(*A narrow (or hankaku) compatibility character.*)
    | Small   	(*A small variant form (CNS compatibility).*)
    | Square   	(*A CJK squared font variant.*)
    | Fraction  (*A vulgar fraction form.*)
    | Compat   	(*Otherwise unspecified compatibility character.*)
    | Canonical

type decomposition_atom = Char of int | Tag of decomposition_tag

type decomposition = decomposition_atom list

let decomposition_tag_of_string = function
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
   | _ -> assert false

let dts = decomposition_tag_of_string

type 'a char_description =
    { code : UChar.t; 
      name : 'a list;
      category : category;
      combining_class : combining_class;
      bidirectional_mapping : bidirectional_mapping;
      decomposition : decomposition;
      decimal : int option;
      digit : int option;
      numeric : (int * int) option; (* use Num.t ? *)
      mirrored : bool;
      oldName : 'a list;
      comments : string;
      uppercase : UChar.t option;
      lowercase : UChar.t option;
      titlecase : UChar.t option;
    }
