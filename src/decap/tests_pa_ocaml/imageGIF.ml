open Pervasives
open ImageUtil
open Image

type gif_header_data = {
  version            : string ;
  image_size         : int * int ;
  global_color_table : bool ;
  color_resolution   : int ;
  sort               : bool ;
  size_glob_col_tbl  : int ;
  bg_color_index     : int ;
  pix_aspect_ratio   : int
}

module ReadGIF : ReadImage = struct
  exception Corrupted_Image of string

  let extensions = ["gif"]

  (* Read signature and header *)
  let read_header ich =
    let magic = get_bytes ich 3 in
    if magic <> "GIF" then
      raise (Corrupted_Image "GIF signature expected...");
    let version = get_bytes ich 3 in
    if version <> "87a" && version <> "89a" then
      raise (Corrupted_Image "Version of GIF not supported...");
    let width  = get_bytes ich 2 in
    let height = get_bytes ich 2 in
    let packed = input_byte ich in
    let bgcol  = input_byte ich in
    let pixar  = input_byte ich in
    let w = ((int_of_char width.[1]) lsl 8) lor (int_of_char width.[0]) in
    let h = ((int_of_char height.[1]) lsl 8) lor (int_of_char height.[0]) in
    {
      version            = version ;
      image_size         = w , h ;
      global_color_table = (packed lsr 7 = 1) ;
      color_resolution   = (packed lsr 4) mod 8 ;
      sort               = ((packed lsr 3) mod 2 = 1) ;
      size_glob_col_tbl  = packed mod 8 ;
      bg_color_index     = bgcol ;
      pix_aspect_ratio   = pixar
     }

  (* Read the size of a GIF image.
   * Arguments:
   *   - fn : filename.
   * Returns a couble (width, height).
   * Note: the image is not checked for inconsistency, only the signature and
   * header are checked.
   *)
  let size fn =
    let ich = open_in_bin fn in
    let hdr = read_header ich in
    close_in ich;
    hdr.image_size

  let openfile fn =
    raise (Not_yet_implemented "openfile") (* TODO  *)
end
