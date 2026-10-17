module Demerit : sig
  val consecutive_hyphen : float
  val hyphen : float
  val page_underfull : float
  val compression_jitter : float
  val glue_floor : float
  val glue_stretch : float
  (* P1-1: Knuth-Plass constants. These are the integer demerit scale
     used by the optimal-break DP. The names follow Knuth & Plass
     1981 §4 ("Optimal Breaking"). *)
  val fitness_demerit_loose : float
  val fitness_demerit_very_loose : float
  val line_penalty : float
end

type fit_class =
  | Very_tight
  | Tight
  | Normal
  | Loose
  | Very_loose
  | Overfull

val rank : fit_class -> int

val classify : float -> fit_class

(** Sum the widths of all boxes under a given compression ratio. *)
val line_width : float -> Box.box array -> float

val _compute_line_demerits :
  float ->
  Box.box array ->
  int ->
  Box.parameters ->
  float ->
  Box.box array ->
  int ->
  Box.parameters ->
  float -> float

(** Knuth-Plass style line demerits given the stretch (positive) or
    shrink (negative) ratio and the line penalty. Mirrors §4.1 of
    "Breaking Paragraphs into Lines" by Knuth & Plass. *)
val line_demerits : ratio:float -> penalty:float -> float

(** Convert a ratio to a fitness-class penalty as per §4.3: a ratio
    outside [2/9, 5/9] of the line width costs extra demerits. *)
val fitness_demerits : fit_class -> float

val v_badness :
  float ->
  Box.box array ->
  int ->
  Box.parameters ->
  float -> Box.box array -> int -> Box.parameters -> float -> float
val h_badness : Box.box array array -> float -> Box.line -> float -> float
