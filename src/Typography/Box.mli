open Line
(** Le type des boîtes, des lignes, et des fonctions de manipulation *)

(** {3 Boîtes} *)
(** Boîtes *)
type 'a box =
    GlyphBox of OutputCommon.glyph
  | Kerning of 'a box kerningBox
  | Glue of drawingBox
  | Drawing of drawingBox
  | Hyphen of 'a hyphenBox
  | User of 'a
  | BeginFigure of int
  | FlushFigure of int
  | Empty
and 'a kerningBox = 'a Fonts.FTypes.kerningBox
and drawingBox = {
  drawing_min_width : float;
  drawing_nominal_width : float;
  drawing_max_width : float;
  drawing_y0 : float;
  drawing_y1 : float;
  drawing_badness : float -> float;
  drawing_contents : float -> OutputCommon.contents list;
}
and 'a hyphenBox = {
  hyphen_normal : 'a box array;
  hyphenated : ('a box array * 'a box array) array;
}

(** Fabrique une boîte de dessin, en calculant la bounding box. Le
paramètre offset correspond à la "profondeur" en TeX *)
val drawing : ?offset:float -> OutputCommon.contents list -> drawingBox

(** Colle un dessin à une position dans un autre dessin, en ajustant
les tailles des boîtes si besoin *)
val drawing_blit : drawingBox -> float -> float -> drawingBox -> drawingBox

val is_glyph : 'a box -> bool
val is_glue : 'a box -> bool
val is_hyphen : 'a box -> bool

(** Taille de l'em de cette boîte *)
val box_size : 'a box -> float

(** Largeur d'une boîte, pour une compression donnée *)
val box_width : float -> 'a box -> float

(** Largeurs possibles d'une boîte, i.e largeur minimale, normale, maximale *)
val box_interval : 'a box -> float * float * float

(** Largeurs possibles d'un tableau de boîtes *)
val boxes_interval : 'a box array -> float * float * float

(** Dessiner le contenu d'une boîte, en prenant leur largeur normale pour les espacer *)
val draw_boxes : 'a box list -> OutputCommon.contents list

(** Bas d'une boîte *)
val lower_y : 'a box -> 'b -> float

(** Haut d'une boîte *)
val upper_y : 'a box -> 'b -> float

(** Fabrique une glue avec les paramètres normaux, pour les largeurs données. *)
val glue : float -> float -> float -> 'a box

(** Fonction de badness de Knuth pour les boîtes normales. [knuth_h_badness normal_width width] *)
val knuth_h_badness : float->float->float

(** Redimensionner une boîte *)
val resize : float -> 'a box -> 'a box

(** Un fold_left sur les lignes. Le premier paramètre est le tableau
global des paragraphes dans le document. Les césures sont
transparentes pour la fonction passée en paramètres. *)
val fold_left_line :
  'a box array array -> ('b -> 'a box -> 'b) -> 'b -> line -> 'b

(** Première boîte de la ligne *)
val first_line : 'a box array array -> line -> 'a box

(** Dernière boîte de la ligne *)
val last_line : 'a box array array -> line -> 'a box

(** Extremas de la hauteur d'une ligne *)
val line_height : 'a box array array -> line -> float * float

(** Compression (entre 0 et 1) nécessaire pour faire tenir la ligne
dans une ligne de mesure le deuxième argument (version sans record) *)
val comp :
  'a box array array -> float -> int -> int -> int -> int -> int -> float

(** Pareil que {!comp}, mais avec le record paramètres au lieu d'une version deboxée *)
val compression : 'a box array array -> parameters -> line -> float

(** Fonction de chargement des glyphs, avec mise en cache pour éviter
de recalculer des bounding boxes et des maps de caractères à chaque
boîte, et partager la mémoire. *)
val glyphCache : Fonts.font -> Fonts.FTypes.glyph_id -> OutputCommon.glyph

(** Transformation simple d'une chaîne de caractères en liste de glyphs *)
val glyph_of_string :
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font ->
  float -> OutputCommon.color -> CamomileLibrary.UTF8.t -> 'a box list

(** Pareil que {!glyph_of_string}, mais en appliquant une fonction de césure avant *)
val hyphenate :
  (CamomileLibrary.UTF8.t ->
   (CamomileLibrary.UTF8.t * CamomileLibrary.UTF8.t) array) ->
  (Fonts.FTypes.glyph_id list -> Fonts.FTypes.glyph_id list) ->
  (Fonts.FTypes.glyph_ids list -> Fonts.FTypes.glyph_ids list) ->
  Fonts.font ->
  float -> OutputCommon.color -> CamomileLibrary.UTF8.t -> 'a box list


(** {3 Fonctions de démouchage} *)

val print_box : out_channel -> 'a box -> unit
val print_box_type : out_channel -> 'a box -> unit
val print_text_line : 'a box array array -> line -> unit

val text_box : 'a box -> string
val text_line : 'a box array array -> line -> string
