open Pervasives
open ImageUtil
open Image

let debug = ref false

let png_signature = "\137PNG\013\010\026\010"

type chunck = {
  chunck_type : string;
  chunck_data : string;
}

type ihdr_data = {
  image_size         : int * int ;
  bit_depth          : int ;
  colour_type        : int ;
  compression_method : int ;
  filter_method      : int ;
  interlace_method   : int
}

(****************************************************************************
 * Zlib compression functions relying on ocaml-zip                          *
 ****************************************************************************)
module PNG_Zlib = struct
  exception PNG_Zlib_error of string
 
  let uncompress_string inputstr =
    let len = String.length inputstr in
    let inputpos = ref 0 in
    let output = ref [] in
  
    let refill strbuf =
      let buflen = String.length strbuf in
      let remaining = len - !inputpos in
      let tocopy = min remaining buflen in
      String.blit inputstr !inputpos strbuf 0 tocopy;
      inputpos := !inputpos + tocopy;
      tocopy
    in
  
    let flush strbuf len =
      let str = String.sub strbuf 0 len in
      output := str :: !output;
    in
  
    (try Zlib.uncompress refill flush with
      _ -> raise (PNG_Zlib_error "Zlib.uncompress failed..."));
  
    String.concat "" (List.rev !output)
  
  let compress_string inputstr =
    let len = String.length inputstr in
    let inputpos = ref 0 in
    let output = ref [] in
  
    let refill strbuf =
      let buflen = String.length strbuf in
      let remaining = len - !inputpos in
      let tocopy = min remaining buflen in
      String.blit inputstr !inputpos strbuf 0 tocopy;
      inputpos := !inputpos + tocopy;
      tocopy
    in
  
    let flush strbuf len =
      let str = String.sub strbuf 0 len in
      output := str :: !output;
    in
  
    (try Zlib.compress refill flush with
      _ -> raise (PNG_Zlib_error "Zlib.compress failed..."));
  
    String.concat "" (List.rev !output)
end

open PNG_Zlib


(****************************************************************************
 * CRC-related things.                                                      *
 ****************************************************************************)
module PNG_CRC = struct
  let (>>) = Int32.shift_right_logical
  let (&)  = Int32.logand
  let (^)  = Int32.logxor
  
  let crc_table =
    let elem n =
      let c = ref (Int32.of_int n) in
      for k = 0 to 7 do
        c := (!c >> 1) ^ (0xedb88320l & (Int32.succ (Int32.lognot (!c & 1l))))
      done; !c
    in Array.init 256 elem
  
  let update_crc crc buf len =
    let c = ref crc in
    for n = 0 to len - 1 do
      let e = Int32.of_int (int_of_char buf.[n]) in
      c := crc_table.(Int32.to_int ((!c ^ e) & 0xffl)) ^ (!c >> 8)
    done; !c
  
  let png_crc buf len =
    Int32.lognot (update_crc 0xffffffffl buf len)
end

open PNG_CRC

(****************************************************************************
 * Core PNG function                                                        *
 ****************************************************************************)
module ReadPNG : ReadImage = struct
  exception Corrupted_Image of string

  let extensions = ["png"]

  (* Checks for the PNG signature
   * Arguments:
   *   - ich : input channel.
   *)
  let read_signature ich =
    let hdr = get_bytes ich 8 in
    if String.sub hdr 1 3 = "PNG" then
      (if hdr <> png_signature then
        raise (Corrupted_Image "Corrupted header..."))
    else raise Wrong_image_type
  
  (* Read one PNG chunck, and check the CRC.
   * Arguments:
   *   - ich : input channel
   * Returns chunck data.
   *)
  let read_chunck ich =
    let length = int32_of_str4 (get_bytes ich 4) in
    if length > 2147483647l then
      raise (Corrupted_Image "Size of chunck greater that 2^31 - 1...");
    let length = Int32.to_int length in (* FIXME unsafe for large chunks *)
    let data = get_bytes ich (length + 4) in
    let str_crc = get_bytes ich 4 in
    let expected_crc = int32_of_str4 str_crc in
    let crc = png_crc data (length + 4) in
    if expected_crc <> crc then
      raise (Corrupted_Image "CRC error...");
    { chunck_type = String.sub data 0 4 ;
      chunck_data = String.sub data 4 length }

  (* Read data form the IHDR header.
   * Arguments:
   *   - s : string containing the data of the IHDR chunck.
   * Returns IHDR data.
   *)
  let data_from_ihdr s =
    (* FIXME problem with very wide images (more that 2^30 - 1 pixels) *)
    let image_width        = int_of_str4(String.sub s 0 4) in
    let image_height       = int_of_str4(String.sub s 4 4) in
    let bit_depth          = int_of_char s.[8] in
    let colour_type        = int_of_char s.[9] in
    let valid = match colour_type, bit_depth with
                  | 0, 1 | 0, 2 | 0, 4 | 0, 8 | 0, 16
                  | 2, 8 | 2, 16
                  | 3, 1 | 3, 2 | 3, 4 | 3, 8
                  | 4, 8 | 4, 16
                  | 6, 8 | 6, 16 -> true
                  | _ -> false
    in
    if not valid then begin
      let msg = Printf.sprintf
            "Unsupported combination of colour type %X with bit depth %X..."
            colour_type bit_depth in
      raise (Corrupted_Image msg)
    end; 
    let compression_method = int_of_char s.[10] in
    if compression_method <> 0 then begin
      let msg = Printf.sprintf
            "Unsupported compression method %X (only code %X is standard)..."
            compression_method 0 in
      raise (Corrupted_Image msg)
    end;
    let filter_method      = int_of_char s.[11] in
    if filter_method <> 0 then begin
      let msg = Printf.sprintf
            "Unsupported filter method %X (only code %X is standard)..."
            compression_method 0 in
      raise (Corrupted_Image msg)
    end;
    let interlace_method   = int_of_char s.[12] in
    if interlace_method <> 0 && interlace_method <> 1 then begin
      let msg = Printf.sprintf
            "Unsupported interlace method %X (only %X and %X are standard)..."
            interlace_method 0 1 in
      raise (Corrupted_Image msg)
    end;
    { image_size         = (image_width, image_height) ;
      bit_depth          = bit_depth ;
      colour_type        = colour_type ;
      compression_method = compression_method;
      filter_method      = filter_method;
      interlace_method   = interlace_method }

  (* Read the size of a PNG image.
   * Arguments:
   *   - fn : filename.
   * Returns a couble (width, height).
   * Note: the image is not checked for inconsistency, only the signature and
   * header are checked.
   *)
  let size fn =
    let ich = open_in_bin fn in
    read_signature ich;
    let ihdr_chunck = read_chunck ich in
    if ihdr_chunck.chunck_type <> "IHDR" then
      raise (Corrupted_Image "First chunck sould be of type IHDR...");
    let ihdr = data_from_ihdr ihdr_chunck.chunck_data in
    close_in ich;
    ihdr.image_size

  (* Removes the filter on a scanline.
   * Arguments:
   *   - ftype : filter type.
   *   - bpp : number of bytes per pixels.
   *   - scanline : the scanline to filter.
   *   - prev_scanline : previous scanline with filter removed.
   * Returns the unfiltered scanline.
   *)
  let unfilter ftype bpp scanline prev_scanline =
    let paeth_predictor a b c =
      let p = a + b - c in
      let pa = abs (p - a) in
      let pb = abs (p - b) in
      let pc = abs (p - c) in
      if pa <= pb && pa <= pc
      then a
      else (if pb <= pc then b else c)
    in
  
    let slen = String.length scanline in
    let unfiltered = String.create slen in
  
    for x = 0 to slen - 1 do
      let filtx = int_of_char scanline.[x] in
      let recona =
        let j = x - bpp in
        if j < 0 then 0 else int_of_char unfiltered.[j]
      in
      let reconb =
        match prev_scanline with
         | None     -> 0
         | Some psl -> int_of_char psl.[x]
      in
      let reconc =
        let j = x - bpp in
        if j < 0 then 0 else
        match prev_scanline with
         | None     -> 0
         | Some psl -> int_of_char psl.[j]
      in
      let recon =
        match ftype with
         | 0 -> filtx
         | 1 -> (filtx + recona) mod 256
         | 2 -> (filtx + reconb) mod 256
         | 3 -> (filtx + ((recona + reconb) / 2)) mod 256
         | 4 -> (filtx + paeth_predictor recona reconb reconc) mod 256
         | _ -> let msg = Printf.sprintf "Unknown filter type (%i)..." ftype in
                raise (Corrupted_Image msg)
      in
      String.set unfiltered x (char_of_int recon)
    done;
    unfiltered
  
  (*
   * Pass extraction function.
   * Arguments :
   *   - s : the string containing the data.
   *   - pl_bit : number of bits per pixel.
   *   - w, h : width and height of the image in pixels.
   * Returns an array of strings containing image rows.
   *)
  let extract_pass s pl_bit w h =
    let starting_row  = [| 0; 0; 4; 0; 2; 0; 1 |] in
    let starting_col  = [| 0; 4; 0; 2; 0; 1; 0 |] in
    let row_increment = [| 8; 8; 8; 4; 4; 2; 2 |] in
    let col_increment = [| 8; 8; 4; 4; 2; 2; 1 |] in
    (*let block_height  = [| 8; 8; 4; 4; 2; 2; 1 |] in*)
    (*let block_width   = [| 8; 4; 4; 2; 2; 1; 1 |] in*)
  
    let rowsize_bit = w * pl_bit in
    let rowsize = rowsize_bit / 8 + if rowsize_bit mod 8 <> 0 then 1 else 0 in
    let zchar = char_of_int 0 in
    let output = Array.init h (fun _ -> String.make rowsize zchar) in
  
    let input_byte = ref 0 in
    let input_bit = ref 0 in
  
    let read_byte () =
      assert (!input_bit = 0);
      let c = String.get s !input_byte in
      incr input_byte; int_of_char c
    in
  
    let read_pix str pixnum =
      if pl_bit mod 8 = 0
      then begin
        let bpp = pl_bit / 8 in
        let offset = pixnum * bpp in
        String.sub str offset bpp
      end else begin
        (*Printf.fprintf stderr "pl_bit = %i, strlen = %i\n%!" pl_bit (String.length str);*)
        let bitoffset = pl_bit * pixnum in
        (*Printf.fprintf stderr "byte offset = %i, bit offset = %i\n%!" (bitoffset / 8) (bitoffset mod 8);*)
        let byte = if bitoffset / 8 >= (String.length str)
                   then (Printf.fprintf stderr "Warning: out of bound...\n%!"; 255)
                   else int_of_char str.[bitoffset / 8] in
        (*let byte = int_of_char str.[bitoffset / 8] in*) (* FIXME *)
        let bitpos = bitoffset mod 8 in
        let mask = (ones pl_bit) lsl (8 - pl_bit) in
        let pix = (byte lsl bitpos) land mask in
        let res = String.make 1 (char_of_int pix) in
        (*Printf.fprintf stderr "Leaving read_pix...\n%!";*)
        res
      end
    in
  
    let read_pixel () =
      if pl_bit mod 8 = 0
      then begin
        let bpp = pl_bit / 8 in
        let res = String.sub s !input_byte bpp in
        input_byte := !input_byte + bpp; res
      end else begin
        let byte = int_of_char s.[!input_byte] in
        let mask = (ones pl_bit) lsl (8 - pl_bit) in
        let pix = (byte lsl !input_bit) land mask in
        input_bit := !input_bit + pl_bit;
        if !input_bit > 7 then begin
          input_bit := 0;
          incr input_byte
        end;
        String.make 1 (char_of_int pix)
      end
    in
  
    let flush_end_of_byte () =
      if !input_bit <> 0
      then begin
        input_bit := 0;
        incr input_byte
      end
    in
  
    (* Writes the pixel pix at pixel position pos in the string str. *)
    let output_pixel pix pos str =
      if pl_bit mod 8 = 0
      then begin
        let bpp = pl_bit / 8 in
        let offset = pos * bpp in
        String.blit pix 0 str offset bpp
      end else begin
        let pixv = int_of_char pix.[0] in
        let bitpos = pos * pl_bit in
        let byte = bitpos / 8 in
        let bit = bitpos mod 8 in
        let content = int_of_char str.[byte] in
        let mask = lnot (((ones pl_bit) lsl (8 - pl_bit)) lsr bit) in
        let newcontent = (content land mask) lor (pixv lsr bit) in
        str.[byte] <- char_of_int newcontent;
        (* DEBUG FIXME *)
        (*
        Printf.fprintf stderr "Writing %i in byte %i at pos %i: "
          (pixv lsr 7) byte bit;
        print_byte content;
        Printf.fprintf stderr " -> ";
        print_byte (int_of_char str.[byte]);
        Printf.fprintf stderr "\n%!";
        *)
        (* DEBUG FIXME *)
      end
    in
  
    let sl = String.make (w * 8) zchar in (* ugly... (2bytes x 4 component) *)
    let slpos = ref 0 in
  
    for pass = 0 to 6 do
      let prevsl = ref None in
  
      let row = ref starting_row.(pass) in
      while !row < h do
        let ft = ref (-1) in
  
        slpos := 0;
        let col = ref starting_col.(pass) in
        while !col < w do
          if !ft < 0 then ft := read_byte ();
  
          let pix = read_pixel () in
          (*
          let pval = (int_of_char pix.[0]) lsr 7 in
          Printf.fprintf stderr "reading pixel at pos: (x = %i, y = %i, val = %i)\n%!" !col !row pval;
          *)
  
          output_pixel pix !slpos sl;
          incr slpos;
  
          col := !col + col_increment.(pass)
        done;
        flush_end_of_byte ();
(*        Printf.fprintf stderr "scanline : ";
        for blibli = 0 to (!slpos - 1) / 8 do
          print_byte (int_of_char sl.[blibli]);
        done;
        Printf.fprintf stderr "\n%!";*)
  
        if !ft >= 0 then begin
          let bitlen = !slpos * pl_bit in
          let sllen = bitlen / 8 + if bitlen mod 8 = 0 then 0 else 1 in
          if bitlen mod 8 <> 0 then begin
            let nbbits = bitlen mod 8 in
            let mask = ones nbbits lsl (8 - nbbits) in
            let last = int_of_char sl.[sllen - 1] in
            sl.[sllen - 1] <- char_of_int (last land mask)
          end;
          let sl = String.sub sl 0 sllen in
          let bpp = max (pl_bit / 8) 1 in
          let slunfilt = unfilter !ft bpp sl !prevsl in
          prevsl := Some slunfilt;
  
          col := starting_col.(pass);
          slpos := 0;
          while !col < w do
            let pix = read_pix slunfilt !slpos in
            incr slpos;
            output_pixel pix !col output.(!row);
  
            col := !col + col_increment.(pass)
          done;
        end;
  
        row := !row + row_increment.(pass)
      done
    done;
(*
    for y = 0 to Array.length output - 1 do
      for x = 0 to String.length output.(y) - 1 do
        print_byte (int_of_char output.(y).[x])
      done;
      Printf.fprintf stderr "\n%!";
    done;*)
    output

  let openfile fn =
    let ich = open_in_bin fn in
    read_signature ich;
  
    let curr_chunck = ref (read_chunck ich) in
    let read_chuncks = ref [] in
  
    let only_once ctype =
      if List.mem ctype !read_chuncks
      then begin
        let msg = Printf.sprintf
                    "Chunck %s should not appear more than once..." ctype
        in
        close_in ich;
        raise (Corrupted_Image msg)
      end
    in
  
    let only_before ctype ctype' =
      if List.mem ctype' !read_chuncks
      then begin
        let msg = Printf.sprintf
                    "Chunck %s should appear before chunck %s..." ctype ctype'
        in
        close_in ich;
        raise (Corrupted_Image msg)
      end
    in
  
    let only_after ctype' ctype =
      if not (List.mem ctype' !read_chuncks)
      then begin
        let msg = Printf.sprintf
                    "Chunck %s should appear after chunck %s..." ctype ctype'
        in
        close_in ich;
        raise (Corrupted_Image msg)
      end
    in
  
    let is_first_chunck ctype =
      if ([] <> !read_chuncks)
      then begin
        let msg = Printf.sprintf
                    "Chunck %s can only be the first chunck..." ctype
        in
        close_in ich;
        raise (Corrupted_Image msg)
      end
    in
  
    let is_not_first_chunck ctype =
      if ([] = !read_chuncks)
      then begin
        let msg = Printf.sprintf
                    "Chunck %s cannot be the first chunck..." ctype
        in
        close_in ich;
        raise (Corrupted_Image msg)
      end
    in
  
    let is_not_compatible_with ctype ctype' =
      if List.mem ctype' !read_chuncks
      then begin
        let msg = Printf.sprintf
                    "Chunck %s is not compatible with chunck %s..." ctype ctype'
        in
        close_in ich;
        raise (Corrupted_Image msg)
      end
    in
  
    let last_chunck () =
      match !read_chuncks with
        | []   -> "NONE"
        | x::_ -> x
    in
  
    let has_read_chunck ctype =
      List.mem ctype !read_chuncks
    in
  
    let not_after ctype' ctype =
      if List.mem ctype' !read_chuncks
      then begin
        let msg = Printf.sprintf
                    "Chunck %s cannot appear after chunck %s..." ctype ctype'
        in
        close_in ich;
        raise (Corrupted_Image msg)
      end
    in
  
    let empty_ihdr = {
      image_size         = -1 , -1;
      bit_depth          = -1;
      colour_type        = -1;
      compression_method = -1;
      filter_method      = -1;
      interlace_method   = -1
    } in
    let ihdr = ref empty_ihdr in
    let palette = ref [||] in
    let raw_idat = ref "" in
    let aspect_ratio = ref None in
    let pixel_size = ref None in

    begin
    try
      while !curr_chunck.chunck_type <> "IEND" do
        let curr_ctype = !curr_chunck.chunck_type in
        (match curr_ctype with
           (* Critical chunks *)
           | "IHDR" ->
               only_once curr_ctype;
               is_first_chunck curr_ctype;
               ihdr := data_from_ihdr !curr_chunck.chunck_data;
               if !debug then begin
                 Printf.fprintf stderr "IHDR content:\n%!";
                 let w, h = !ihdr.image_size in
                 Printf.fprintf stderr
                   "  - image size:         %ix%i\n%!" w h;
                 Printf.fprintf stderr
                   "  - bit depth:          %i\n%!" !ihdr.bit_depth;
                 Printf.fprintf stderr
                   "  - colour type:        %i\n%!" !ihdr.colour_type;
                 Printf.fprintf stderr
                   "  - compression_method: %i\n%!" !ihdr.compression_method;
                 Printf.fprintf stderr
                   "  - filter_method:      %i\n%!" !ihdr.filter_method;
                 Printf.fprintf stderr
                   "  - interlace_method:   %i\n%!" !ihdr.interlace_method
               end
           | "PLTE" ->
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "IDAT";
               only_once curr_ctype;
               not_after "tRNS" curr_ctype;
               not_after "bKGD" curr_ctype;
  
               let ct = !ihdr.colour_type in
               if ct = 0 || ct = 4
               then begin
                 let msg = Printf.sprintf
                       "Chunck PLTE is forbiden for greyscale mode (%i)..." ct
                 in raise (Corrupted_Image msg);
               end;
  
               let bytes_palette = String.length !curr_chunck.chunck_data in
               if bytes_palette mod 3 <> 0
               then raise (Corrupted_Image "Invalid palette size...");
               let palette_length = bytes_palette / 3 in
               if palette_length > pow_of_2 !ihdr.bit_depth
               then begin
                 let msg = Printf.sprintf
                       "Maximum palette size is %i for bit depth %i.\n%!"
                       (pow_of_2 !ihdr.bit_depth) !ihdr.bit_depth
                 in raise (Corrupted_Image msg)
               end;
               palette := Array.init palette_length
                 (fun i ->
                   { r = int_of_char !curr_chunck.chunck_data.[i * 3];
                     g = int_of_char !curr_chunck.chunck_data.[i * 3 + 1];
                     b = int_of_char !curr_chunck.chunck_data.[i * 3 + 2] });
               if !debug then begin
                 Printf.fprintf stderr "PLTE with %i lines\n%!" palette_length
               end
           | "IDAT" ->
               is_not_first_chunck curr_ctype;
               if has_read_chunck curr_ctype && last_chunck () <> curr_ctype
               then raise (Corrupted_Image
                            "Chuncks IDAT should be consecutive...");
               raw_idat := String.concat "" [!raw_idat; !curr_chunck.chunck_data];
               if !debug then begin
                 Printf.fprintf stderr "IDAT (raw data is now %i bytes long)\n%!"
                   (String.length !raw_idat)
               end
           (* IEND cannot occur (end condition of the loop) *)
           (* | "IEND" -> *)
  
           (* Ancillary chunks *)
           | "cHRM" ->
               only_once curr_ctype;
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "PLTE";
               only_before curr_ctype "IDAT";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "cHRM chunck ignored\n%!"
               end
           | "gAMA" ->
               only_once curr_ctype;
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "PLTE";
               only_before curr_ctype "IDAT";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "gAMA chunck ignored\n%!"
               end
           | "iCCP" ->
               only_once curr_ctype;
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "PLTE";
               only_before curr_ctype "IDAT";
               is_not_compatible_with curr_ctype "sRGB";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "iCPP chunck ignored\n%!"
               end
           | "sBIT" ->
               only_once curr_ctype;
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "PLTE";
               only_before curr_ctype "IDAT";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "sBIT chunck ignored\n%!"
               end
           | "sRGB" ->
               only_once curr_ctype;
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "PLTE";
               only_before curr_ctype "IDAT";
               is_not_compatible_with curr_ctype "iCPP";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "sRGB chunck ignored\n%!"
               end
           | "bKGD" ->
               only_once curr_ctype;
               only_before curr_ctype "IDAT";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "bKGD chunck ignored\n%!"
               end
           | "hIST" ->
               only_once curr_ctype;
               only_after "PLTE" curr_ctype;
               only_before curr_ctype "IDAT";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "hIST chunck ignored\n%!"
               end
           | "tRNS" ->
               only_once curr_ctype;
               only_before curr_ctype "IDAT";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "tRNS chunck ignored\n%!"
               end
           | "pHYs" ->
               only_once curr_ctype;
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "IDAT";

               let data = !curr_chunck.chunck_data in
               let size_x = int_of_str4 (String.sub data 0 4) in
               let size_y = int_of_str4 (String.sub data 3 4) in
               (match int_of_char (String.get data 8) with
                | 0 -> if !debug then
                         Printf.fprintf stderr "Aspect ratio is %i / %i\n" size_x size_y;
                       aspect_ratio := Some (size_x , size_y) (* Unknown unit *)
                | 1 -> if !debug then begin
                         Printf.fprintf stderr "Pixel size X axis: %i px/m\n" size_x;
                         Printf.fprintf stderr "Pixel size Y axis: %i px/m\n" size_y
                       end;
                       pixel_size   := Some (size_x, size_y)  (* Unit is pixel / metre *)
                | _ ->  raise (Corrupted_Image
                                 "Unit not supported in pHYs chunk..."))
           | "sPLT" ->
               is_not_first_chunck curr_ctype;
               only_before curr_ctype "IDAT";
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "sPLT chunck ignored\n%!"
               end
           | "tIME" ->
               only_once curr_ctype;
               is_not_first_chunck curr_ctype;
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "tIME chunck ignored\n%!"
               end
           | "iTXt" ->
               is_not_first_chunck curr_ctype;
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "iTXt chunck ignored\n%!"
               end
           | "tEXt" ->
               is_not_first_chunck curr_ctype;
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "tEXt chunck ignored\n%!";
                 (* Printf.fprintf stderr "  \"%s\"\n%!" !curr_chunck.chunck_data *)
               end
           | "zTXt" ->
               is_not_first_chunck curr_ctype;
               (* TODO *)
               if !debug then begin
                 Printf.fprintf stderr "zTXt chunck ignored\n%!"
               end
           | ch_ty  ->
               let msg = Printf.sprintf "Unknown chunck type \"%s\"..." ch_ty in
               raise (Corrupted_Image msg));
        read_chuncks := !curr_chunck.chunck_type :: !read_chuncks;
        curr_chunck := read_chunck ich
      done;
      read_chuncks := "IEND" :: !read_chuncks;
      if !debug then begin
        Printf.fprintf stderr "IEND reached\n%!"
      end;
    with
      End_of_file ->
        (close_in ich;
         raise (Corrupted_Image "End of file reached before chunck end..."))
    end;
  
    (* Check for trailing bytes... *)
    if (try let _ = input_char ich in true with End_of_file -> false)
    then raise (Corrupted_Image "Data after the IEND chunck...");
    close_in ich;
  
    let uncomp_idat = uncompress_string !raw_idat in
  
    let w, h = !ihdr.image_size in
    let bd = !ihdr.bit_depth in
    let ct = !ihdr.colour_type in
    let im = !ihdr.interlace_method in
  
    (* Computing number of component and byte per pixel *)
    let nb_comp = match ct with
                   | 0 -> 1 | 2 -> 3 | 3 -> 1 | 4 -> 2 | 6 -> 4
                   | _ -> -1
    in
    let bpp = match bd with
               | 8  -> nb_comp
               | 16 -> 2 * nb_comp
               | _  -> 1
    in
  
    let unfiltered =
      match im with
       | 0 ->
         (* Finding the lenght of the scanline *)
         let sl_bit = w * nb_comp * bd in
         let slen = sl_bit / 8 + if sl_bit mod 8 <> 0 then 1 else 0 in
         if !debug then
           Printf.fprintf stderr "No interlace, scanline length = %i\n%!" slen;
  
         (* Building the scanlines *)
         let scanlines = Array.init h
           (fun y ->
             let ind = y * (slen + 1) in
             let ft = int_of_char uncomp_idat.[ind] in
             let sl = String.sub uncomp_idat (ind + 1) slen in
             (ft, sl)
           )
         in
       
         (* Removing the filter on the scanlines *)
         let prev_scanline = ref None in
         Array.mapi
           (fun y (ftype, scanline) ->
             let output = unfilter ftype bpp scanline !prev_scanline in
             prev_scanline := Some output; output
           ) scanlines
       | 1 ->
         if !debug then Printf.fprintf stderr "Interlace method 1.\n%!";
         let pixlen_bit = nb_comp * bd in
         extract_pass uncomp_idat pixlen_bit w h
       | _ -> assert false
    in
  
    (* Conversion of the array of string into an array of array of int *)
    let unfiltered_int = Array.init h (fun y -> Array.make (w * nb_comp) 0) in
  
    for y = 0 to h - 1 do
      for x = 0 to (w * nb_comp) - 1 do
        match bd with
         | 16 -> let b1 = int_of_char unfiltered.(y).[2 * x] in
                 let b2 = int_of_char unfiltered.(y).[2 * x + 1] in
                 let v = (b1 lsl 8) + b2 in
                 Array.set unfiltered_int.(y) x v
         | 8  -> Array.set unfiltered_int.(y) x (int_of_char unfiltered.(y).[x])
         | 4  -> let ind = x / 2 in
                 let partb = x mod 2 in
                 let b = int_of_char unfiltered.(y).[ind] in
                 let v = if partb == 0 then b lsr 4 else b mod 16 in
                 Array.set unfiltered_int.(y) x v
         | 2  -> let ind = x / 4 in
                 let partb = x mod 4 in
                 let b = int_of_char unfiltered.(y).[ind] in
                 let v = (b mod (pow_of_2 (2 * (4 - partb)))) lsr (2 * (3 - partb)) in
                 Array.set unfiltered_int.(y) x v
         | 1  -> let ind = x / 8 in
                 let partb = x mod 8 in
                 let b = int_of_char unfiltered.(y).[ind] in
                 let v = (b mod (pow_of_2 (8 - partb))) lsr (7 - partb) in
                 Array.set unfiltered_int.(y) x v
         | _  -> assert false
      done;
    done;
  
    (* Output *)
    if !debug then Printf.fprintf stderr "Building image structure...\n%!";
    match ct with
    | 0 -> 
      let image = create_grey ~max_val:(ones bd) w h in
      for y = 0 to h - 1 do
	for x = 0 to w - 1 do
	  write_grey_pixel image x y unfiltered_int.(y).(x)
	done
      done;
      image

     | 2 ->
       let image = create_rgb ~max_val:(ones bd) w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
           let r = unfiltered_int.(y).(3 * x) in
           let g = unfiltered_int.(y).(3 * x + 1) in
           let b = unfiltered_int.(y).(3 * x + 2) in
	   write_rgb_pixel image x y r g b
	 done
       done;
       image

     | 3 ->
       let image = create_rgb ~max_val:255 w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
           let index = unfiltered_int.(y).(x) in
           let index = (* FIXME *)
             if index >= Array.length !palette
             then (Printf.fprintf stderr "Palette index too big...\n%!"; 0)
             else index 
           in
	   let p = !palette.(index) in
           write_rgb_pixel image x y p.r p.g p.b
	 done
       done;
       image

     | 4 ->
       let image = create_grey ~alpha:true ~max_val:(ones bd) w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
	   write_greya_pixel image x y 
	     unfiltered_int.(y).(2 * x)
	     unfiltered_int.(y).(2 * x + 1);
	 done
       done;
       image

     | 6 ->
       let image = create_rgb ~alpha:true ~max_val:(ones bd) w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
           let r = unfiltered_int.(y).(4 * x) in
           let g = unfiltered_int.(y).(4 * x + 1) in
           let b = unfiltered_int.(y).(4 * x + 2) in
           let a = unfiltered_int.(y).(4 * x + 3) in
	   write_rgba_pixel image x y r g b a
	 done
       done;
       image

     | _ -> assert false
end

(****************************************************************************
 * PNG writing function                                                     *
 ****************************************************************************)
let write_signature och =
  output_string och png_signature

let write_chunk och chunck =
  let len = String.length chunck.chunck_data in
  output_string och (int_to_str4 len);
  output_string och chunck.chunck_type;
  output_string och chunck.chunck_data;
  let type_and_data = String.concat ""
    [chunck.chunck_type; chunck.chunck_data] in
  let crc = png_crc type_and_data (len + 4) in
  let crc3 = Int32.to_int ((crc >> 24) & 0xFFl) in
  let crc2 = Int32.to_int ((crc >> 16) & 0xFFl) in
  let crc1 = Int32.to_int ((crc >> 8) & 0xFFl) in
  let crc0 = Int32.to_int (crc & 0xFFl) in
  output_char och (char_of_int crc3);
  output_char och (char_of_int crc2);
  output_char och (char_of_int crc1);
  output_char och (char_of_int crc0)

let ihdr_to_string ihdr =
  let s = String.create 13 in
  String.blit (int_to_str4 (fst ihdr.image_size)) 0 s 0 4;
  String.blit (int_to_str4 (snd ihdr.image_size)) 0 s 4 4;
  s.[8] <- char_of_int ihdr.bit_depth;
  s.[9] <- char_of_int ihdr.colour_type;
  s.[10] <- char_of_int ihdr.compression_method;
  s.[11] <- char_of_int ihdr.filter_method;
  s.[12] <- char_of_int ihdr.interlace_method;
  s

let write_png fn img =
  let och = open_out_bin fn in
  write_signature och;

  let maxv = img.max_val in
  let bd =
    let rec find_bd i =
      if ones i >= maxv then i else find_bd (i + 1)
    in find_bd 1
  in

  let ct = match img.pixels, img.alpha with
            | GreyL _, None   -> 0
            | GreyL _, Some _ -> 4
            | RGB _,   None   -> 2
            | RGB _,   Some _ -> 6
  in

  let w = img.width and h = img.height in

  let ihdr = { image_size         = (w, h);
               bit_depth          = bd;
               colour_type        = ct;
               compression_method = 0;
               filter_method      = 0;
               interlace_method   = 0
             }
  in
  let ihdr = { chunck_type = "IHDR" ; chunck_data = ihdr_to_string ihdr } in
  write_chunk och ihdr;

  let buf = Buffer.create 4096 in
  let byte0 () = Buffer.add_char buf (char_of_int 0) in
  let add_byte i = Buffer.add_char buf (char_of_int i) in
  (match img.pixels, img.alpha, bd with
    | GreyL _, None     , 1  ->
      let byte = ref 0 in
      let numb = ref 0 in
      let add_bit i =
        (match !numb with
          | 0 -> byte := !byte lor (i lsl 7)
          | 1 -> byte := !byte lor (i lsl 6)
          | 2 -> byte := !byte lor (i lsl 5)
          | 3 -> byte := !byte lor (i lsl 4)
          | 4 -> byte := !byte lor (i lsl 3)
          | 5 -> byte := !byte lor (i lsl 2)
          | 6 -> byte := !byte lor (i lsl 1)
          | 7 -> byte := !byte lor i
          | _ -> assert false);
        incr numb;
        if !numb = 8 then begin
          add_byte !byte;
          byte := 0;
          numb := 0
        end
      in
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
          read_grey_pixel img x y (fun ~g -> add_bit g);
        done
      done;
      if !numb <> 0 then add_byte !byte
    | GreyL _, None     , 2  ->
      let byte = ref 0 in
      let numb = ref 0 in
      let add_quarter_byte i =
        (match !numb with
          | 0 -> byte := !byte lor (i lsl 6)
          | 1 -> byte := !byte lor (i lsl 4)
          | 2 -> byte := !byte lor (i lsl 2)
          | 3 -> byte := !byte lor i
          | _ -> assert false);
        incr numb;
        if !numb = 4 then begin
          add_byte !byte;
          byte := 0;
          numb := 0
        end
      in
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
          read_grey_pixel img x y (fun ~g -> add_quarter_byte g)
        done
      done;
      if !numb <> 0 then add_byte !byte
    | GreyL _, None     , 4  ->
      let byte = ref 0 in
      let numb = ref 0 in
      let add_half_byte i =
        (match !numb with
          | 0 -> byte := i lsl 4
          | 1 -> byte := !byte lor i
          | _ -> assert false);
        incr numb;
        if !numb = 2 then begin
          add_byte !byte;
          byte := 0;
          numb := 0
        end
      in
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
          read_grey_pixel img x y (fun ~g -> add_half_byte g)
        done
      done;
      if !numb <> 0 then add_byte !byte
    | GreyL _, None     , 8  ->
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
          read_grey_pixel img x y (fun ~g -> add_byte g)
        done
      done
    | GreyL _, None     , 16 ->
      let mask = ones 8 in
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
	  read_grey_pixel img x y (fun ~g -> 
            add_byte (g lsr 8);
            add_byte (g land mask));
        done
      done
    | GreyL _, Some als , 8  ->
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
 	  read_greya_pixel img x y (fun ~g ~a -> 
            add_byte g;
            add_byte a);
        done
      done
    | GreyL _, Some als , 16 ->
      let mask = ones 8 in
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
	  read_greya_pixel img x y (fun ~g ~a -> 
            add_byte (g lsr 8);
            add_byte (g land mask);
            add_byte (a lsr 8);
            add_byte (a land mask));
        done
      done
    | RGB _,   None     , 8  ->
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
	  read_rgb_pixel img x y (fun ~r ~g ~b -> 
            add_byte r;
            add_byte g;
            add_byte b);
        done
      done
    | RGB _,   None     , 16 ->
      let mask = ones 8 in
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
          read_rgb_pixel img x y (fun ~r ~g ~b -> 
	    add_byte (r lsr 8);
            add_byte (r land mask);
            add_byte (g lsr 8);
            add_byte (g land mask);
            add_byte (b lsr 8);
            add_byte (b land mask));
        done
      done
    | RGB _,   Some als , 8  ->
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
          read_rgba_pixel img x y (fun ~r ~g ~b ~a ->
	    add_byte r;
            add_byte g;
            add_byte b;
            add_byte a);
        done
      done
    | RGB _,   Some als , 16 ->
      let mask = ones 8 in
      for y = 0 to h - 1 do
        byte0 (); (* Scanline with no filter *)
        for x = 0 to w - 1 do
          read_rgba_pixel img x y (fun ~r ~g ~b ~a ->
	    add_byte (r lsr 8);
            add_byte (r land mask);
            add_byte (g lsr 8);
            add_byte (g land mask);
            add_byte (b lsr 8);
            add_byte (b land mask);
            add_byte (a lsr 8);
            add_byte (a land mask));
        done
      done
    | _ -> Printf.fprintf stderr "%i\n%!" bd; assert false
  );
  let data = Buffer.contents buf in
  let data = compress_string data in

  let datalen = String.length data in
  let max_idat_len = 1048576 in (* 2^20 sould be enough *)
  (* FIXME constant too big for 32 bit architectures...*)
  (*let max_idat_len = 2147483647 - 4 in (* 2^31 - 1 - 4 *)*)
  let rec output_idat_from pos =
    if datalen - pos < max_idat_len
    then begin
      let idat = String.sub data pos (datalen - pos) in
      let idat = { chunck_type = "IDAT" ; chunck_data = idat } in
      write_chunk och idat
    end else begin
      let idat = String.sub data pos max_idat_len in
      let idat = { chunck_type = "IDAT" ; chunck_data = idat } in
      write_chunk och idat;
      output_idat_from (pos + max_idat_len)
    end
  in output_idat_from 0;

  let iend = { chunck_type = "IEND" ; chunck_data = "" } in
  write_chunk och iend;

  close_out och
