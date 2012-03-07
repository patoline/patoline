(** Types de l'optimiseur (boîtes), et fonctions de manipulation *)
open Drivers

(** Parametres dont se sert l'optimiseur pour connaître le placement
    des boîtes sur la ligne, le nombre de lignes sur la page, etc. *)
type parameters = {
  lead : float;
  measure : float;
  lines_by_page : int;
  left_margin : float;
  local_optimization : int; (** Arité sortante maximale du nœud du graphe courant*)
  min_height_before:int;    (** Espace vertical minimal avant la ligne courante *)
  min_height_after:int;     (** Espace vertical minimal après la ligne courante *)
  min_page_diff:int;        (** Nombre de pages minimal avant la ligne courante *)
  allow_widows:bool;        (** Considérer les fin de paragraphe sur la première ligne d'une page *)
  allow_orphans:bool        (** Considérer les débuts de paragraphe sur la dernière ligne d'une page *)
}
val default_params:parameters

val print_parameters : parameters -> unit
type line = {
  paragraph : int;                      (** Paragraphe de cette ligne *)
  lastFigure : int;                     (** Dernière figure placée avant cette ligne *)
  lineEnd : int;                        (** Fin de la ligne  *)
  lineStart : int;                      (** Début de la ligne  *)
  hyphenStart : int;                    (** indice de césure de la première boîte (si c'est une HyphenBox)  *)
  hyphenEnd : int;                      (** indice de césure de la dernière boîte (si c'est une HyphenBox)  *)
  isFigure : bool;                      (** Cette ligne est-elle une figure ? Si c'est le cas, elle a l'indice lastFigure dans le tableau des figures *)
  mutable height : int;                 (** Hauteur sur la page, en lignes  *)
  paragraph_height : int;               (** Numéro de la ligne dans le paragraphe *)
  mutable page_height:int;              (** Numéro de la ligne sur la page *)
  mutable page : int;                   (** Numéro de la page  *)
  min_width : float;                    (** Largeur minimale du matériel de cette ligne  *)
  nom_width : float;
  max_width : float                     (** Largeur maximale du matériel de cette ligne  *)
}
module Line : sig type t = line val compare : 'a -> 'a -> int end
module LineMap :
  sig
    type key = Line.t
    type 'a t = 'a New_map.Make(Line).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type 'a kerningBox = 'a Fonts.FTypes.kerningBox

type drawingBox = {
  drawing_min_width : float;
  drawing_nominal_width : float;
  drawing_max_width : float;
  drawing_y0 : float;
  drawing_y1 : float;
  drawing_badness : float -> float;
  drawing_contents : float -> Drivers.contents list;
}
and hyphenBox = {
  hyphen_normal : box array;
  hyphenated : (box array * box array) array;
}
and box =
    GlyphBox of Drivers.glyph
  | Kerning of box kerningBox
  | Glue of drawingBox
  | Drawing of drawingBox
  | Hyphen of hyphenBox
  | Empty

(** Encapsule proprement des dessins, en calculant la plus petite
    boîte qui le contient. Le paramètre est le décalage par rapport à
    la ligne de base. *)
val drawing : ?offset:float -> Drivers.contents list -> drawingBox
type error_log = Overfull_line of line | Widow of line | Orphan of line

val fold_left_line : box array array -> ('a -> box -> 'a) -> 'a -> line -> 'a

(** Dernière boîte de la ligne *)
val last_line : box array array -> line -> box

(** Première boîte de la ligne *)
val first_line : box array array -> line -> box

(** Affichage du type ligne *)
val print_line : line -> unit
val print_box : box -> unit
val print_box_type : box -> unit
val print_text_line : box array array -> line -> unit
val is_glyph : box -> bool
val is_glue : box -> bool
val is_hyphen : box -> bool

(** Largeur d'une boîte, en ajustant le paramètre variable entre 0. et 1. *)
val box_width : float -> box -> float

(** Les largeurs minimale, nominale et maximale d'une boîte *)
val box_interval : box -> float * float * float

(** Les largeurs minimale, nominale et maximale d'un tableau de boîtes *)
val boxes_interval : box array -> float * float * float

val draw_boxes : box list -> Drivers.contents list

(** Bas de la boîte, en tenant compte de la largeur *)
val lower_y : box -> float -> float
(** Haut de la boîte, en tenant compte de la largeur *)
val upper_y : box -> float -> float

(** Hauteur maximale au-dessus et en-dessous de la ligne de base *)
val line_height : box array array -> line -> float * float

val comp :
  box array array -> float -> int -> int -> int -> int -> int -> float

(** Paramètre entre 0. et 1. nécessaire pour que la ligne tienne dans
    la mesure qu'on lui donne en paramètres *)
val compression : box array array -> parameters * line -> float

(** Hash-consing sur les glyphs du document *)
val glyphCache : Fonts.font -> Fonts.FTypes.glyph_id -> glyph
val glyph_of_string :
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font -> float -> CamomileLibrary.UTF8.t -> box list
val hyphenate :
  (string -> (string*string) array) ->
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font -> float -> CamomileLibrary.UTF8.t -> box list

(** Exemple de fonction de badness utilisable pour les glueBox *)
val knuth_h_badness : float -> float -> float

(** Multiplier la taille des boîtes *)
val resize:float -> box -> box
