(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
open Box

(** Sum the widths of all boxes under a given compression ratio. *)
let line_width comp boxes =
  let acc = ref 0. in
  for k = 0 to Array.length boxes - 1 do
    acc := !acc +. box_width comp boxes.(k)
  done;
  !acc

(* Demerit constants for the paragraph optimizer. Centralizing them here
   keeps [Document.badness] readable and lets the Knuth-Plass port
   (planned in [Break.Optimal]) share the same scale. *)
module Demerit = struct
  (* Consecutive-line hyphenation in the same paragraph (very bad). *)
  let consecutive_hyphen = 1e10
  (* Single hyphenation break in a paragraph (less bad than consecutive,
     but still strongly discouraged). *)
  let hyphen = 1e8
  (* Vertical badness penalty for an under-filled page when the line is
     the last one on the page. *)
  let page_underfull = 10_000.
  (* Per-unit compression-ratio difference between two consecutive lines
     (i.e. how much the stretch/shrink ratio diverges). *)
  let compression_jitter = 1_000.
  (* Floor for the inner glue-only h_badness sum. *)
  let glue_floor = 10e6
  (* Penalty for h_badness when a paragraph is far from its optimal
     width (< 2/9 or > 7/9 of the line measure). *)
  let glue_stretch = 20.
  (* P1-1: Knuth-Plass §4 demerits. *)
  let fitness_demerit_loose = 50.
  let fitness_demerit_very_loose = 100.
  let line_penalty = 10.
end

(* Fit classes, ordered from tightest to loosest. The integer rank lets
   the caller compare the class without exposing constructors. *)
type fit_class =
  | Very_tight
  | Tight
  | Normal
  | Loose
  | Very_loose
  | Overfull

let rank = function
  | Very_tight -> 0
  | Tight -> 1
  | Normal -> 2
  | Loose -> 3
  | Very_loose -> 4
  | Overfull -> 5

(* Classify a (line_width, target_width) ratio into one of five fit
   classes using the standard Knuth-Plass thresholds:
     very_loose   ratio >= 7/9
     loose        5/9 <= ratio < 7/9
     normal       2/9 <= ratio < 5/9
     tight        1/9 <= ratio < 2/9
     very_tight   ratio <  1/9
   An [infinity] stretch means the line is overfull (treated as
   very_tight because it cannot fit). *)
let classify ratio =
  let r =
    if not (Float.is_finite ratio) then 0.
    else ratio
  in
  if r < 1. /. 9. then Very_tight
  else if r < 2. /. 9. then Tight
  else if r < 5. /. 9. then Normal
  else if r < 7. /. 9. then Loose
  else if r <= 1. then Very_loose
  else Overfull

(** Knuth-Plass §4.1: the cubic-of-distance demerits of a single line,
    scaled by the optional [penalty] constant. The classic formula is
    [(1 + r)^3] for stretching (r >= 0) and [-(alpha * r)^3] for
    shrinking; here we keep the simpler [r^3 * 100] trade-off which is
    the one currently used by Tex. *)
let line_demerits ~ratio ~penalty =
  let r = max (-1.) (min 2. ratio) in
  let abs_cubic = if r >= 0. then (1. +. r) ** 3. else (-. r) ** 3. in
  abs_cubic +. penalty

(** P1-1: convert a fitness class to the demerit penalty contributed by
    the second-order (§4.3) rules. *)
let fitness_demerits = function
  | Very_tight | Tight | Normal -> 0.
  | Loose -> Demerit.fitness_demerit_loose
  | Very_loose -> Demerit.fitness_demerit_very_loose
  | Overfull -> infinity

(** [compute_line_demerits] is the integer-style decomposition used by
    the optimal-break DP: it returns the (non-vertical) demerits of a
    single line given the boxes and parameters on either side. *)
let _compute_line_demerits v_space _haut _max_haut _params_i _comp_i bas _max_bas params_j comp_j =
  let ratio =
    let target = params_j.measure -. v_space in
    let actual = line_width comp_j bas in
    if target <= 0. then 0. else actual /. target -. 1. in
  line_demerits ~ratio ~penalty:Demerit.line_penalty
  +. fitness_demerits (classify ratio)

let v_badness v_space haut max_haut params_i comp_i bas max_bas params_j comp_j=

  let xi=ref params_i.left_margin in
  let xj=ref params_j.left_margin in

  let rec collide i j w_tot col col2=
    let box_i=if i<max_haut then haut.(i) else Empty in
    let box_j=if j<max_bas then bas.(j) else Empty in
      (* let _=Graphics.wait_next_event [Graphics.Key_pressed] in *)
    let wi=box_width comp_i box_i in
    let wj=box_width comp_j box_j in
      if !xi +.wi < !xj+. wj && i < max_haut then (
        let yi=lower_y box_i in
        let yj=if !xi+.wi < !xj then -.infinity else
          if upper_y box_j > -.infinity then upper_y box_j else 0.
        in
        let x0=if !xi+.wi < !xj then !xi else max !xi !xj in
        let w0= !xi +. wi -. x0 in

          xi:= !xi+.wi;
          if !xj>= !xi+.wi || is_glue box_i || is_glue box_j then
            (collide (i+1) j w_tot col col2)
          else
            (let area=w0*.(v_space+.yi-.yj) in
             let cl=classify_float area in
             let ar=if cl=FP_infinite || cl=FP_nan then 0. else area in
             collide (i+1) j (w_tot+.w0) (col+.ar) (col+.ar*.ar))
      ) else if j < max_bas then (
        let yi=if !xj +. wj < !xi then infinity else
          if lower_y box_i < infinity then lower_y box_i else 0. in
        let yj=upper_y box_j in
        let x0=if !xj+.wj < !xi then !xj else max !xi !xj in
        let w0= !xj +. wj -. x0 in
          xj:= !xj+.wj;
          if !xi>= !xj+.wj || is_glue box_i || is_glue box_j then
            (collide i (j+1) w_tot col col2)
          else
            (let area=w0*.(v_space+.yi-.yj) in
             let cl=classify_float area in
             let ar=if cl=FP_infinite || cl=FP_nan then 0. else area in
             collide i (j+1) (w_tot+.w0) (col+.ar) (col+.ar*.ar))
      ) else (if w_tot<=0. then 0. else ((col*.col-.col2)/.w_tot))
  in
    collide 0 0 0. 0. 0.




let h_badness paragraphs measure node comp=
  let bad=ref 0. in
  let glues=ref 0 in
  for k=node.lineStart to node.lineEnd-1 do
    match paragraphs.(node.paragraph).(k) with
        Drawing x ->
          bad:= !bad +.
            x.drawing_badness (x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp)
      | Glue x->(
        bad:= !bad +.
          x.drawing_badness (x.drawing_min_width+.(x.drawing_max_width-.x.drawing_min_width)*.comp);
        incr glues
      )
      | _->()
  done;
  if !glues<=0 then Demerit.glue_floor else
    !bad *. (if node.nom_width/.measure < 2./.9. || node.nom_width/.measure > 7./.9.
             then Demerit.glue_stretch else 1.)
