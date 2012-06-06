(** Fonctions de poids standards *)


(** Un exemple de fonction de poids. L'ordre des arguments est : tous
    les paragraphes, les figures, puis deux fois (une ligne, le
    tableau de boîtes correspondant, l'indice maximal dans ce tableau,
    les paramètres de la ligne, la compression entre 0. et 1.).
    
    L'idée est de factoriser au maximum le calcul de la compression et
    du tableau de boîtes (où les césures sont toutes expansées à leur
    apparence définitive).
*)
val badness :
  'a Box.box array array ->
  Box.drawingBox array ->
  Break.figurePosition Util.IntMap.t->
  Line.line ->
  'c Box.box array ->
  int ->
  Line.parameters ->
  float ->
  Line.line -> 'd Box.box array -> int -> Line.parameters -> float -> float
