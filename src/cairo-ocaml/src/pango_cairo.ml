(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

open Gaux
open Gobject

type font_map = [`pangocairofontmap] obj

module FontMap = struct
  external new_ : unit -> font_map
    = "ml_pango_cairo_font_map_new"
  external get_default : unit -> font_map
    = "ml_pango_cairo_font_map_get_default"
  external create_context : font_map -> Pango.context
    = "ml_pango_cairo_font_map_create_context"
end

external update_context : Cairo.t -> Pango.context -> unit
  = "ml_pango_cairo_update_context"
external create_layout : Cairo.t -> Pango.layout
  = "ml_pango_cairo_create_layout"
external update_layout : Cairo.t -> Pango.layout -> unit
  = "ml_pango_cairo_update_layout"
external show_layout : Cairo.t -> Pango.layout -> unit
  = "ml_pango_cairo_show_layout"
