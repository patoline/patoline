open Typography
open Parameters
open Fonts.FTypes
open Util
open Fonts
open Drivers

let _=Random.self_init ()

let ragged_left a b c d e line=
  let par=parameters a b c d e line in
  { par with measure=line.nom_width }

let ragged_right a b c d e line=
  let par=parameters a b c d e line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width }

let in_text_figure a b c d e line=
  let par=parameters a b c d e line in
  { par with
    measure=line.nom_width;
    left_margin=par.left_margin+.par.measure-.line.nom_width;
    next_acceptable_height=(fun node h->h+.5.) }

let title is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        min_height_before=0.;
        next_acceptable_height=(fun node h->max (node.height+.20.) (h+.5.)) }
  in
  newPar (Typography.C.normal 150.) mcenter [size 10. str ]

let author is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        next_acceptable_height=(fun node h->max (node.height) (h+.5.)) }
  in
  newPar (Typography.C.normal 150.) mcenter [size 6. str ]

let institute is_last str =
  let mcenter a b c d e l =
    { (center a b c d e l) with
        min_height_before=11.;
        next_acceptable_height=(fun node h->max (node.height+.10.) (h+.5.)) }
  in
  newPar (Typography.C.normal 150.) mcenter [size 4. str ]

let textWidth : Typography.user Typography.C.completion= Typography.C.normal 150.

let lang_OCaml s = [T s]
