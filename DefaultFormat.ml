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

let author str =
  newPar (normal 150.) center [size 7. [T str] ]

let institute str =
  newPar (normal 150.) center [size 4. [T str] ]

let textWidth = normal 150.
