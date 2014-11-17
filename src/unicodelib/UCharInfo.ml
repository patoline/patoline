type general_category =
  (* normative *)
  | Lu | Ll | Lt | Mn | Mc | Me | Nd | Nl | No
  | Zs | Zl | Zp | Cc | Cf | Cs | Co | Cn
  (* informative *)
  | Lm | Lo | Pc | Pd | Ps | Pe | Pi | Pf | Po
  | Sm | Sc | Sk | So

type combining_class =
  | Spacing_split_enclosing_reordrant_and_Tibetan_subjoined
  | Overlays_and_interior
  | Nuktas
  | Hiragana_Katakana_voicing_marks
  | Viramas
  | Fixed_position of int
  | Below_left_attached
  | Below_attached
  | Below_right_attached
  | Left_attached
  | Right_attached
  | Above_left_attached
  | Above_attached
  | Above_right_attached
  | Below_left
  | Below
  | Below_right
  | Left
  | Right
  | Above_left
  | Above
  | Above_right
  | Double_below
  | Double_above
  | Below_iota_subscript

type bidirectional_mapping =
  | L   (* Left-to-Right *)
  | LRE (* Left-to-Right Embedding *)
  | LRO (* Left-to-Right Override *)
  | LRI (* Left-to-Right Isolate *)
  | R   (* Right-to-Left *)
  | AL  (* Right-to-Left Arabic *)
  | RLE (* Right-to-Left Embedding *)
  | RLO (* Right-to-Left Override *)
  | RLI (* Right-to-Left Isolate *)
  | PDF (* Pop Directional Format *)
  | PDI (* Pop Directional Isolate *)
  | FSI (* ??? Isolate *)
  | EN  (* European Number *)
  | ES  (* European Number Separator *)
  | ET  (* European Number Terminator *)
  | AN  (* Arabic Number *)
  | CS  (* Common Number Separator *)
  | NSM (* Non-Spacing Mark *)
  | BN  (* Boundary Neutral *)
  | B   (* Paragraph Separator *)
  | S   (* Segment Separator *)
  | WS  (* Whitespace *)
  | ON  (* Other Neutral *)

type decomposition_atom =
  | Font        (* A font variant (e.g. a blackletter form). *)
  | NoBreak     (* A no-break version of a space or hyphen. *)
  | Initial     (* An initial presentation form (Arabic). *)
  | Medial      (* A medial presentation form (Arabic). *)
  | Final       (* A final presentation form (Arabic). *)
  | Isolated    (* An isolated presentation form (Arabic). *)
  | Circle      (* An encircled form. *)
  | Super       (* A superscript form. *)
  | Sub         (* A subscript form. *)
  | Vertical    (* A vertical layout presentation form. *)
  | Wide        (* A wide (or zenkaku) compatibility character. *)
  | Narrow      (* A narrow (or hankaku) compatibility character. *)
  | Small       (* A small variant form (CNS compatibility). *)
  | Square      (* A CJK squared font variant. *)
  | Fraction    (* A vulgar fraction form. *)
  | Compat      (* Otherwise unspecified compatibility character. *)
  | Canonical
  | Char of int

type char_description =
  { code                  : UChar.t
  ; name                  : string list
  ; general_category      : general_category
  ; combining_class       : combining_class
  ; bidirectional_mapping : bidirectional_mapping
  ; decomposition         : decomposition_atom list
  ; decimal_digit_value   : int option
  ; digit_value           : int option
  ; numeric_value         : (int * int) option
  ; mirrored              : bool
  ; oldName               : string
  ; comments              : string
  ; uppercase             : UChar.t option
  ; lowercase             : UChar.t option
  ; titlecase             : UChar.t option
  }

let get_char_descr_from_file : UChar.t -> char_description = fun u ->
  let m = PermanentMap.open_map !UnicodeLibConfig.datafile in
  let d = PermanentMap.get m u in
  PermanentMap.close_map m; d

let cache : (UChar.t, char_description) Hashtbl.t = Hashtbl.create 2048
let get_char_descr : UChar.t -> char_description = fun u ->
  try
    Hashtbl.find cache u
  with Not_found ->
    begin
      let d = get_char_descr_from_file u in
      Hashtbl.add cache u d; d
    end

let general_category : UChar.t -> general_category = fun u ->
  let d = get_char_descr u in
  d.general_category

let unicode_name : UChar.t -> string = fun u ->
  let d = get_char_descr u in
  String.concat " " d.name

let to_lower : UChar.t -> UChar.t option = fun u ->
  let d = get_char_descr u in
  d.lowercase

let to_upper : UChar.t -> UChar.t option = fun u ->
  let d = get_char_descr u in
  d.uppercase
