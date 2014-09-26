(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

(** A module implementing character sets efficiently. *)

(** {2 Type} *)

(** The abstract type for a character set. *)
type charset

(** {2 Constant charsets} *)

(** The empty character set. *)
val empty_charset :charset

(** The full character set. *)
val full_charset :charset

(** {2 Manipulating charsets} *)

(** [mem cs c] tests whether the character [c] appears in the charset [cs]. *)
val mem : charset -> char -> bool

(** [addq cs c] adds the character [c] to the charset [cs]. *)
val addq : charset -> char -> unit

(** [add cs c] adds the character [c] to the charset [cs] and returns the new
   charset. *)
val add : charset -> char -> charset

(** [delq cs c] deletes the character [c] in the charset [cs] if it is
   appears. *)
val delq : charset -> char -> unit

(** [delq cs c] deletes the character [c] in the charset [cs] if it is
   appears, and returns the new charset. *)
val del : charset -> char -> charset

(** [union cs1 cs2] returns a charset that is the union of charset [cs1] and
   charset [cs2]. *)
val union: charset -> charset -> charset

(** [singleton c] returns a charset containing only character [c]. *)
val singleton : char -> charset

(** [copy cs] make a copy of the charset [cs]. *)
val copy : charset -> charset

(** [print_charset oc cs] prints the charset [cs] on the output channel [oc].
   If no charset is provided, ["None"] is printed. *)
val print_charset : out_channel -> charset -> unit

(** [list_of_charset cs] transforms a charset into a list of strings, each
   containing an escaped character. *)
val list_of_charset : charset -> string list