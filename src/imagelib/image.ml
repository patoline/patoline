(****************************************************************************
 * Data types used by the image input / output fonctions                    *
 ****************************************************************************)

exception Wrong_image_type

type component = int

type rgb = {
  r : component;
  g : component;
  b : component;
}

type pixmap = RGB of rgb array array
            | GreyL of component array array

type image = {
  size    : int * int ;
  max_val : component;
  pixels  : pixmap ;
  alpha   : component array array option ;
}

module type ReadImage =
  sig
    exception Corrupted_Image of string
    val extensions : string list
    val size : string -> int * int
    val openfile : string -> image
  end
