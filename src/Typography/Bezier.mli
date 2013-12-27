type curve = float array * float array

(** Coordonée du i-eme point de contrôle *)
val curve : 'a array * 'b array -> int -> 'a * 'b

(** O(n) Retournement *)
val rev : 'a array * 'b array -> 'a array * 'b array

(** O(n) Dérivée d'un polynôme de Bernstein *)
val derivee : float array -> float array

(** Dérivée en 0 d'un polynôme de Bernstein *)
val derivee_start : float array -> float 

(** Dérivée en 1 d'un polynôme de Bernstein *)
val derivee_end : float array -> float 

(** Dérivée approximative : la corde ! *)
val derivee_approx : float array -> float 

(** O(n^2) Subdivision de De Casteljau, sur l'intervalle [x;1] *)
val casteljau_right : float array -> float -> float array

(** O(n^2) Subdivision de De Casteljau, sur l'intervalle [0;x] *)
val casteljau_left : float array -> float -> float array

(** Version combinée de casteljau_left et casteljau_right *)
val restrict : float array -> float -> float -> float array

(** Division d'une courbe en trois selon deux temps *)
val split2 : float array -> float -> float -> (float array * float array * float array)

(** Divise une courbe en n > 0 selon le temps *)
val divide : float array -> int -> (float array) list

(** O(n^2) Evaluation d'une courbe de Bézier en un point, avec l'algo
    de De Casteljau *)
val eval : float array -> float -> float

(** Trouve les racines d'un polynôme avec la règle de Descartes *)
val bernstein_solve_int : float array -> float -> (float*float) list

(** Trouve les racines d'un polynôme avec la règle de Descartes *)
val bernstein_solve : float array -> float -> float list

(** Les extremas d'un polynôme *)
val bernstein_extr : float array -> float * float

(** Les coins diagonaux de la plus petite boîte rectangulaire
    définissable comme ça qui contient la courbe *)
val bounding_box :
  float array * float array -> (float * float) * (float * float)

(** Cet algorithme est une combinaison de l'algo naif sur les morceaux
    convexes des courbes, et d'un calcul de resultant pour verifer la
    sortie *)
val extremity : float array * float array -> float list
val intersect' :
  float array * float array -> float list -> float array * float array -> float list -> (float * float) list

val intersect :
  float array * float array -> float array * float array -> (float * float) list

val plus :
  float array -> float array -> float array
val minus :
  float array -> float array -> float array
val times :
  float array -> float array -> float array

val plus2 :
  float array array -> float array array -> float array array
val minus2 :
  float array array -> float array array -> float array array
val times2 :
  float array array -> float array array -> float array array

val solve2 :
  float array array -> float array array -> (float*float*float*float) list
val distance :
  (float array *float array)-> (float array*float array)-> float

val distance1 :
  (float *float)-> (float array*float array)-> float

val subdivise :
  float -> (float array*float array) -> (float array*float array) list

(* orientation des courbes de Beziers:
   - on oriente chaque courba par l'aire (>= 0 = sens direct de rotation)
   - on regarde le module 2 du nombre de courbe qui contiennent chaque courbe
   - on garde true pour sens directe avec module 2 = 0
*)
val oriente :
  (float array*float array) list list -> ((float array*float array) list * bool) list

val length : (float array*float array)-> float
val partial_length : float -> (float array*float array)-> float
