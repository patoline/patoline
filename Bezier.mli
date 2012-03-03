(** Le type d'une courbe de Bézier à deux dimensions. Certaines
    fonctions utilisent le fait que les deux composantes soient de
    même taille *)
type curve = float array * float array

(** Coordonée du i-eme point de contrôle *)
val curve : 'a array * 'b array -> int -> 'a * 'b

(** O(n) Dérivée d'un polynôme de Bernstein *)
val derivee : float array -> float array

(** O(n^2) Subdivision de De Casteljau, sur l'intervalle [x;1] *)
val casteljau_right : float array -> float -> float array

(** O(n^2) Subdivision de De Casteljau, sur l'intervalle [0;x] *)
val casteljau_left : float array -> float -> float array

(** Version combinée de casteljau_left et casteljau_right *)
val restrict : float array -> float -> float -> float array

(** O(n^2) Evaluation d'une courbe de Bézier en un point, avec l'algo
    de De Casteljau *)
val eval : float array -> float -> float

(** Trouve les racines d'un polynôme avec la règle de Descartes *)
val bernstein_solve : float array -> float list

(** Les extremas d'un polynôme *)
val bernstein_extr : float array -> float * float

(** Les coins diagonaux de la plus petite boîte rectangulaire
    définissable comme ça qui contient la courbe *)
val bounding_box :
  float array * float array -> (float * float) * (float * float)

(** Cet algorithme est une combinaison de l'algo naif sur les morceaux
    convexes des courbes, et d'un calcul de resultant pour verifer la
    sortie *)
val intersect :
  float array * float array -> float array * float array -> (float * float) list
