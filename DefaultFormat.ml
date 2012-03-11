open Typography
open Parameters
open Fonts.FTypes
open Util
open Fonts
open Drivers

let _=Random.self_init ()

let ragged_left a b c line=
  let par=parameters a b c line in
  { par with measure=line.nom_width }

let ragged_right a b c line=
  let par=parameters a b c line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width }

let title is_last str =
  let mcenter a b c l =
    { (center a b c l) with
      min_height_after = if is_last then 4 else 2 }
  in
  newPar (normal 150.) mcenter [size 10. str ]

let author is_last str =
  let mcenter a b c l =
    { (center a b c l) with
      min_height_after = if is_last then 3 else 1 }
  in
  newPar (normal 150.) mcenter [size 6. str ]

let institute is_last str =
  let mcenter a b c l =
    { (center a b c l) with
      min_height_after = if is_last then 3 else 1 }
  in
  newPar (normal 150.) mcenter [size 4. str ]

let textWidth = normal 150.

let lang_OCaml s = [T s]

