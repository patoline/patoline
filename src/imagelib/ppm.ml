open Pervasives
open ImageUtil
open Image

module ReadPPM : ReadImage = struct
  exception Corrupted_Image of string
  
  (*
   * The list of standard extenisons for the PPM format.
   *   - ppm : portable pixmap format
   *   - pgm : portable graymap format
   *   - pbm : portable bitmap format
   *   - pnm : portable anymap format
   *)
  let extensions = ["ppm"; "pgm"; "pbm"; "pnm"]
  
  (*
   * Reads the size of a PPM image from a file.
   * Does not check if the file is correct. Only goes as far as the header.
   * Returns a couple (width, height)
   *)
  let size fn =
    let lines = lines_from_file fn in
  
    if List.mem "" lines then
      raise (Corrupted_Image "Empty line in the file...");
  
    let lines = List.filter (fun s -> s.[0] <> '#') lines in
    let content = String.concat " " lines in
  
    let size = ref (-1, -1) in
  
    Scanf.sscanf content
      "%s%[\t\n ]%u%[\t\n ]%u"
      (fun magic _ w _ h ->
        if not (List.mem magic ["P1"; "P2"; "P3"; "P4"; "P5"; "P6"]) then
          raise (Corrupted_Image "Invalid magic number...");
        size := (w, h)
      );
  
    !size
  
  (*
   * Read a PPM format image file.
   * Arguments:
   *   - fn : the path to the file.
   * Raise the exception Corrupted_Image if the file is not valid.
   *)
  let openfile fn =
    let lines = lines_from_file fn in
  
    if List.mem "" lines then
      raise (Corrupted_Image "Empty line in the file...");
  
    let lines = List.filter (fun s -> s.[0] <> '#') lines in
    let content = String.concat " " lines in
    let c_len = String.length content in
  
    let magic = ref "" in
    let size = ref (-1, -1) in
    let max_val = ref 1 in
    let data = ref "" in
  
    Scanf.sscanf content "%s%[\t\n ]" (fun mn _ -> magic := mn);
    if not (List.mem !magic ["P1"; "P2"; "P3"; "P4"; "P5"; "P6"]) then
      raise (Corrupted_Image "Invalid magic number...");
  
    if List.mem !magic ["P1"; "P4"]
    then begin
      Scanf.sscanf content
        "%s%[\t\n ]%u%[\t\n ]%u%[\t\n ]%n"
        (fun _ _ w _ h _ n ->
          size := (w, h);
          data := String.sub content n (c_len - n)
        )
    end else begin
      Scanf.sscanf content
        "%s%[\t\n ]%u%[\t\n ]%u%[\t\n ]%u%[\t\n ]%n"
        (fun _ _ w _ h _ mv _ n ->
          size := (w, h);
          max_val := mv;
          data := String.sub content n (c_len - n)
        )
    end;
  
    let w, h = !size in
  
    match !magic with
     | "P1" | "P2" ->
       let values = int_array_from_string !data in
       let pixels = init_matrix !size (fun (x, y) -> values.(y * w + x)) in
       { size = !size ;
         max_val = !max_val ;
         pixels = GreyL pixels ;
         alpha = None }
     | "P3" ->
       let values = int_array_from_string !data in
       let pixels = init_matrix !size
         (fun (x,y) ->
           let indr = y * w * 3 + x * 3 in
           let r = values.(indr) in
           let g = values.(indr + 1) in
           let b = values.(indr + 2) in
           { r = r ; g = g ; b = b }
         )
       in
       { size = !size ;
         max_val = !max_val ;
         pixels = RGB pixels ;
         alpha = None }
     | "P4" ->
       let row_byte_size = if w mod 8 = 0 then w else w + 1 in
       let pixels = init_matrix !size
         (fun (x,y) ->
           let byte_num = y * row_byte_size + x / 8 in
           let byte_pos = x mod 8 in
           let byte = int_of_char !data.[byte_num] in
           (byte lsr (7 - byte_pos)) land 1
         )
       in
       { size = !size ;
         max_val = 1 ;
         pixels = GreyL pixels ;
         alpha = None }
     | "P5" ->
       let values = byte_array_from_string !data in
       let pixels = init_matrix !size
         (fun (x,y) ->
           if !max_val <= 255
           then values.(y * w + x)
           else begin
             let indb1 = y * 2 * w + 2 * x in
             let b1 = values.(indb1) in
             let b0 = values.(indb1 + 1) in
             (b1 lsl 8) + b0
           end
         )
       in
       { size = !size ;
         max_val = !max_val ;
         pixels = GreyL pixels ;
         alpha = None }
     | "P6" ->
       let values = byte_array_from_string !data in
       let pixels = init_matrix !size
         (fun (x,y) ->
           if !max_val <= 255
           then begin
             let indr = y * w * 3 + x * 3 in
             let r = values.(indr) in
             let g = values.(indr + 1) in
             let b = values.(indr + 2) in
             { r = r ; g = g ; b = b }
           end else begin
             let indr1 = y * w * 3 * 2 + x * 3 * 2 in
             let r1 = values.(indr1) in
             let r0 = values.(indr1 + 1) in
             let g1 = values.(indr1 + 2) in
             let g0 = values.(indr1 + 3) in
             let b1 = values.(indr1 + 4) in
             let b0 = values.(indr1 + 5) in
             let r = (r1 lsl 8) + r0 in
             let g = (g1 lsl 8) + g0 in
             let b = (b1 lsl 8) + b0 in
             { r = r ; g = g ; b = b }
           end
         )
       in
       { size = !size ;
         max_val = !max_val ;
         pixels = RGB pixels ;
         alpha = None }
     | _ ->
       raise (Corrupted_Image "Invalid magic number...");
end

(*
 * PPM encoding of files (Binary or ASCII)
 *)
type ppm_mode = Binary | ASCII

(*
 * Write a PPM format image to a file.
 * Arguments:
 *   - fn : the path to the file.
 *   - img : the image.
 *   - mode : the image mode (Binary or ASCII).
 * Warning: the alpha channel is ignored since it is not supported by the PPM
 * image format.
 *)
let ppm_write fn img mode =
  let och = open_out_bin fn in
  let w, h = img.size in

  (match img.pixels, mode, img.max_val with
   | RGB pixmap,    Binary, mv ->
     let header = Printf.sprintf "P6\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         let rgb = pixmap.(x).(y) in
         let pixel =
           if mv < 256
           then begin
             let r = char_of_int rgb.r in
             let g = char_of_int rgb.g in
             let b = char_of_int rgb.b in
             Printf.sprintf "%c%c%c" r g b
           end else begin
             let r0 = char_of_int (rgb.r mod 256) in
             let r1 = char_of_int (rgb.r lsr 8) in
             let g0 = char_of_int (rgb.g mod 256) in
             let g1 = char_of_int (rgb.g lsr 8) in
             let b0 = char_of_int (rgb.b mod 256) in
             let b1 = char_of_int (rgb.b lsr 8) in
             Printf.sprintf "%c%c%c%c%c%c" r1 r0 g1 g0 b1 b0
           end
         in output_string och pixel
       done
     done
   | RGB pixmap,    ASCII,  mv ->
     let header = Printf.sprintf "P3\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         let rgb = pixmap.(x).(y) in
         let pixel = Printf.sprintf "%i %i %i\n" rgb.r rgb.g rgb.b in
         output_string och pixel
       done;
     done
   | GreyL greymap, Binary, 1  ->
     let header = Printf.sprintf "P4\n%i %i\n" w h in
     output_string och header;
     for y = 0 to h - 1 do
       let byte = ref 0 in
       let pos = ref 0 in

       let output_bit b =
         let bitmask = b lsl (7 - !pos) in
         byte := !byte lor bitmask;
         incr pos;
         if !pos = 8 then begin
           output_char och (char_of_int !byte);
           byte := 0;
           pos := 0;
         end
       in

       let flush_byte () =
         if !pos <> 0 then output_char och (char_of_int !byte)
       in

       for x = 0 to w - 1 do
         output_bit greymap.(x).(y)
       done;

       flush_byte ()
     done
   | GreyL greymap, ASCII,  1  ->
     let header = Printf.sprintf "P1\n%i %i\n" w h in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         let gl = greymap.(x).(y) in
         let pixel = Printf.sprintf "%i\n" gl in
         output_string och pixel
       done;
     done
   | GreyL greymap, Binary, mv ->
     let header = Printf.sprintf "P5\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         let gl = greymap.(x).(y) in
         let pixel =
           if mv < 256
           then Printf.sprintf "%c" (char_of_int gl)
           else begin
             let gl0 = char_of_int (gl mod 256) in
             let gl1 = char_of_int (gl lsr 8) in
             Printf.sprintf "%c%c" gl1 gl0
           end
         in output_string och pixel
       done;
     done
   | GreyL greymap, ASCII,  mv ->
     let header = Printf.sprintf "P2\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         let gl = greymap.(x).(y) in
         let pixel = Printf.sprintf "%i\n" gl in
         output_string och pixel
       done;
     done
  );

  close_out och
;;
