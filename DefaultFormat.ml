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

let title str =
  let mcenter a b c l =
    { (center a b c l) with
      min_height_after = 3 }
  in
  newPar (normal 150.) mcenter [size 10. [T str] ]

let author str =
  let mcenter a b c l =
    { (center a b c l) with
      min_height_after = 1 }
  in
  newPar (normal 150.) mcenter [size 6. [T str] ]

let institute str =
  let mcenter a b c l =
    { (center a b c l) with
      min_height_after = 1 }
  in
  newPar (normal 150.) mcenter [size 4. [T str] ]

let textWidth = normal 150.
