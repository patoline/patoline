open Pervasives
open ImageUtil
open Image

let xcf_signature = "gimp xcf "

type xcf_header_data = {
  version            : int ;
  image_size         : int * int ;
  colour_type        : int
}

module ReadXCF : ReadImage = struct
  exception Corrupted_Image of string

  let extensions = ["xcf"]

  let version_num s =
    match s with
    | "file\000" -> 0
    | "v001\000" -> 1
    | "v002\000" -> 2
    | _          -> raise (Corrupted_Image "Unknown version number...")

  let read_header ich =
    let magic     = get_bytes ich 9 in
    let version   = get_bytes ich 5 in
    let width     = get_bytes ich 4 in
    let height    = get_bytes ich 4 in
    let base_type = get_bytes ich 4 in
    if magic <> xcf_signature then
      raise (Corrupted_Image "Corrupted header...");
    let w  = int_of_str4 width in
    let h  = int_of_str4 height in
    let ct = int_of_str4 base_type in
    if ct < 0 || ct > 2 then
      raise (Corrupted_Image "Unknown color type...");
    {
      version     = version_num version ;
      image_size  = w , h ;
      colour_type = ct
    }

  (* Read the size of a XCF image.
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
