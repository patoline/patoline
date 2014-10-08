open Pervasives
open ImageUtil
open Image

module ReadJPG : ReadImage = struct
  exception Corrupted_Image of string

  let extensions = ["jpg"; "jpeg"; "jpe"; "jif"; "jfif"; "jfi"]

  let read_marker ich =
    let ff = input_byte ich in
    if ff <> 0xff then
      raise (Corrupted_Image "Expected marker...");
    let rec read_first_not_ff ich =
      let c = input_byte ich in
      if c = 0xff then read_first_not_ff ich else c
    in
    let c = read_first_not_ff ich in
    if c = 0 then
      raise (Corrupted_Image "0xFF00 is not a valid marker...");
    c

  let read_header_data ich =
    (* Read magic number (i.e. marker 0xffd8) *)
    let magic = read_marker ich in
    if magic <> 0xd8 then
      raise (Corrupted_Image "First marker should be SOI...");

    (* Read other header sections *)
    let rec read_header_sections ich acc =
      let mrk = read_marker ich in
      let sz = get_bytes ich 2 in
      let size = ((int_of_char sz.[0]) lsl 8) lor (int_of_char sz.[1]) in
      let data = get_bytes ich (size - 2) in
      if mrk = 0xda then (* SOS reached *)
        List.rev ((mrk, data) :: acc)
      else
        read_header_sections ich ((mrk, data) :: acc)
    in
    read_header_sections ich []

  (* Read the size of a XCF image.
   * Arguments:
   *   - fn : filename.
   * Returns a couble (width, height).
   * Note: the image is not checked for inconsistency, only the signature and
   * header are checked.
   *)
  let size fn =
    let ich = open_in_bin fn in
    let headers = read_header_data ich in
    close_in ich;

    let rec find_SOF ls =
      match ls with
      | []        -> raise (Corrupted_Image "No SOFn marker...");
      | (m,d)::bs -> if m >= 0xc0 && m <= 0xcf && m <> 0xc4 && m <> 0xcc
                     then (m - 0xc0, d)
                     else find_SOF bs
    in
    let (n, sof) = find_SOF headers in
    (*
    Printf.fprintf stderr "SOF%i: " n;
    show_string_hex sof;
    *)
    let height = ((int_of_char sof.[1]) lsl 8) lor (int_of_char sof.[2]) in
    let width = ((int_of_char sof.[3]) lsl 8) lor (int_of_char sof.[4]) in
    width, height

  let openfile fn =
    raise (Not_yet_implemented "openfile") (* TODO  *)
end
