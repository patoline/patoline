(** Quelques constantes et fonctions utiles *)

open Config

exception File_not_found of (string*string list)
(** Chercher un fichier dans un chemin *)
let findPath f path=
  let rec findPath f=function
      []->raise (File_not_found (f,path))
    | h::s when Sys.file_exists (Filename.concat h f)->(Filename.concat h f)
    | h::s -> (findPath f s)
  in
    findPath f path
(** Chercher un fichier dans le chemin des polices *)
let findFont f=findPath f ("."::(!fontsdir))
(** Chercher un fichier dans le chemin des grammaires *)
let findGrammar f=findPath f ("." :: (!grammarsdir))
(** Chercher un fichier dans le chemin des dictionnaires de césures *)
let findHyph f=findPath f ("."::(!hyphendir))

(** Convertir en points Adobe une longueur en millimètres *)
let pt_of_mm x=(72.*.x)/.25.4
(** Convertir en millimètres une longueur en points Adobe *)
let mm_of_pt x=(25.4*.x)/.72.

let a4=(210.,297.)
let phi=(1.+.(sqrt 5.))/.2.
