(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

(** Cairo rendering backend for Pango.
  For more details please see:
  [http://developer.gnome.org/doc/API/2.0/pango/pango-Cairo-Rendering.html]
  *)

type font_map

module FontMap : sig
  val new_ : unit -> font_map
  val get_default : unit -> font_map
  val create_context : font_map -> Pango.context
end

val update_context : Cairo.t -> Pango.context -> unit
val create_layout : Cairo.t -> Pango.layout
val update_layout : Cairo.t -> Pango.layout -> unit
val show_layout : Cairo.t -> Pango.layout -> unit
