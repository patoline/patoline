/**************************************************************************/
/*  cairo-ocaml -- Objective Caml bindings for Cairo                      */
/*  Copyright © 2004-2005 Olivier Andrieu                                 */
/*                                                                        */
/*  This code is free software and is licensed under the terms of the     */
/*  GNU Lesser General Public License version 2.1 (the "LGPL").           */
/**************************************************************************/

#include "ml_cairo.h"

#include <caml/bigarray.h>

/* This prototype is only exported since OCaml 3.11 */
uintnat caml_ba_byte_size (struct caml_ba_array *);

CAMLprim value
ml_bigarray_byte_size (value b)
{
  return Val_long (caml_ba_byte_size (Caml_ba_array_val (b)));
}

CAMLprim value
ml_bigarray_kind_float (value v)
{
  struct caml_ba_array *ba = Caml_ba_array_val (v);

  switch (ba->flags & CAML_BA_KIND_MASK)
    {
    case CAML_BA_FLOAT32:
    case CAML_BA_FLOAT64:
    case CAML_BA_COMPLEX32:
    case CAML_BA_COMPLEX64:
      return Val_true;
    default:
      return Val_false;
    }
}

CAMLprim value
ml_cairo_image_surface_create_for_data (value img, value fmt, value w, value h, value stride)
{
  cairo_surface_t *surf;
  surf = cairo_image_surface_create_for_data (Caml_ba_data_val (img),
					      cairo_format_t_val (fmt),
					      Int_val (w),
					      Int_val (h),
					      Int_val (stride));
  ml_cairo_surface_set_image_data (surf, img);

  return Val_cairo_surface_t (surf);
}

/* cairo_image_surface_get_data */
