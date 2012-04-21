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
  'a Boxes.box array array ->
  ?figures:'b array ->
  Boxes.line ->
  'c Boxes.box array ->
  int ->
  Boxes.parameters ->
  float ->
  Boxes.line -> 'd Boxes.box array -> int -> Boxes.parameters -> float -> float
