/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#define CAML_NAME_SPACE

#include "ml_cairo.h"

#include <pango/pangocairo.h>

#include <wrappers.h>
#include <ml_glib.h>
#include <ml_gobject.h>
#include <ml_pango.h>

#define PangoCairoFontMap_val(val) check_cast(PANGO_CAIRO_FONT_MAP, val)
#define Val_PangoCairoFontMap Val_GAnyObject

wML_0(pango_cairo_font_map_new, Val_PangoCairoFontMap)
wML_0(pango_cairo_font_map_get_default, Val_PangoCairoFontMap)
wML_1(pango_cairo_font_map_create_context, PangoCairoFontMap_val, Val_PangoContext)
wML_2(pango_cairo_update_context, cairo_t_val, PangoContext_val, Unit)
wML_1(pango_cairo_create_layout, cairo_t_val, Val_PangoLayout)
wML_2(pango_cairo_update_layout, cairo_t_val, PangoLayout_val, Unit)
wML_2(pango_cairo_show_layout, cairo_t_val, PangoLayout_val, Unit)
