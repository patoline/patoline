open Pervasives
open ImageUtil
open Image
open Bigarray

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

  let count_line str = 
    let r = ref 0 in
    for i = 0 to String.length str - 1 do
      if str.[i] = '\n' then incr r
    done;
    !r

  let read_header content =
    let magic = ref "" in
    let width = ref (-1) and height = ref (-1) in
    let max_val = ref 1 in
    let header_line_num = ref 0 in
    let count s =
      header_line_num := !header_line_num + count_line s;
    in
    let rec pass_comments () =
      (try
	 Scanf.fscanf content "#%[^\n\r]%[\t\n\r]" (fun s1 s2 ->
	   count s1; count s2);
	 (fun () -> pass_comments ())
     with _ -> (fun () -> ())) ()
    in
    Scanf.fscanf content "%s%[\t\n ]" (fun mn s -> magic := mn; 
      count s);
    pass_comments ();
    
    if not (List.mem !magic ["P1"; "P2"; "P3"; "P4"; "P5"; "P6"]) then
      raise (Corrupted_Image "Invalid magic number...");
    
    if List.mem !magic ["P1"; "P4"] then (
      Scanf.fscanf content "%u%[\t\n ]" (fun w s -> width := w; count s);
      pass_comments ();
      Scanf.fscanf content "%u%1[\t\n ]" (fun h s -> height := h; count s))
    else (
      Scanf.fscanf content "%u%[\t\n ]" (fun w s -> width := w; count s);
      pass_comments ();
      Scanf.fscanf content "%u%[\t\n ]" (fun h s -> height := h; count s);
      pass_comments ();
      Scanf.fscanf content "%u%1[\t\n ]" (fun mv s -> max_val := mv; count s));
      
    !magic,!width,!height,!max_val,!header_line_num

  (*
   * Reads the size of a PPM image from a file.
   * Does not check if the file is correct. Only goes as far as the header.
   * Returns a couple (width, height)
   *)
  let size fn =
    let content = open_in_bin fn in
    let _,w,h,_,_ = read_header content in
    w, h
  
  (*
   * Read a PPM format image file.
   * Arguments:
   *   - fn : the path to the file.
   * Raise the exception Corrupted_Image if the file is not valid.
   *)
  let openfile fn = 
    try
      let content = open_in_bin fn in
      let magic,w,h,max_val,hline_num = read_header content in
      seek_in content 0;
      for i = 1 to hline_num do
	ignore (input_line content)
      done;
 
      match magic with
      | "P1" | "P2" ->
       let image = create_grey ~max_val:max_val w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
	   Scanf.fscanf content "%d%[\t\n ]" (fun v _ ->
	     write_grey_pixel image x y v)
	 done
       done;
       image

     | "P3" ->
       let image = create_rgb ~max_val:max_val w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
	   Scanf.fscanf content "%d%[\t\n ]%d%[\t\n ]%d%[\t\n ]"
	     (fun r _ g _ b _ ->
	       write_rgb_pixel image x y r g b)
	 done
       done;
       image

     | "P4" ->
       let image = create_grey ~max_val:1 w h in
       for y = 0 to h - 1 do
	 let x = ref 0 in
	 let byte = ref 0 in
	 while !x < w do
	   if !x mod 8 = 0 then
	     byte := int_of_char (input_char content);
	   let byte_pos = !x mod 8 in
	   let v = (!byte lsr (7 - byte_pos)) land 1 in
	   write_grey_pixel image !x y v;
	   incr x
	 done
       done;
       image  

     | "P5" ->
       let image = create_grey ~max_val:max_val w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
	   if max_val <= 255 then (
	     let b0 = int_of_char (input_char content) in
	     write_grey_pixel image x y b0)
	   else (
	     let b0 = int_of_char (input_char content) in
	     let b1 = int_of_char (input_char content) in
	     write_grey_pixel image x y ((b0 lsl 8) + b1))
	 done
       done;
       image

     | "P6" ->
       let image = create_rgb ~max_val:max_val w h in
       for y = 0 to h - 1 do
	 for x = 0 to w - 1 do
	   if max_val <= 255 then (
	     let r = int_of_char (input_char content) in
	     let g = int_of_char (input_char content) in
	     let b = int_of_char (input_char content) in
	     write_rgb_pixel image x y r g b)
	   else (
	     let r1 = int_of_char (input_char content) in
	     let r0 = int_of_char (input_char content) in
	     let g1 = int_of_char (input_char content) in
	     let g0 = int_of_char (input_char content) in
	     let b1 = int_of_char (input_char content) in
	     let b0 = int_of_char (input_char content) in
             let r = (r1 lsl 8) + r0 in
             let g = (g1 lsl 8) + g0 in
             let b = (b1 lsl 8) + b0 in
	     write_rgb_pixel image x y r g b)
	 done
       done;
       image

     | _ ->
       raise (Corrupted_Image "Invalid magic number...");
    with End_of_file ->
      raise (Corrupted_Image "Truncated file");

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
let write_ppm fn img mode =
  let och = open_out_bin fn in
  let w = img.width and h = img.height in

  (match img.pixels, mode, img.max_val with
   | RGB _,    Binary, mv ->
     Printf.fprintf och "P6\n%i %i %i\n" w h mv;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
	 read_rgb_pixel img x y (fun ~r ~g ~b ->
           if mv < 256
           then begin
             Printf.fprintf och "%c%c%c"
	       (char_of_int r) (char_of_int g) (char_of_int b)
           end else begin
             let r0 = char_of_int (r mod 256) in
             let r1 = char_of_int (r lsr 8) in
             let g0 = char_of_int (g mod 256) in
             let g1 = char_of_int (g lsr 8) in
             let b0 = char_of_int (b mod 256) in
             let b1 = char_of_int (b lsr 8) in
             Printf.fprintf och "%c%c%c%c%c%c" r1 r0 g1 g0 b1 b0
           end)
       done
     done
   | RGB _,    ASCII,  mv ->
     Printf.fprintf och "P3\n%i %i %i\n" w h mv;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
 	 read_rgb_pixel img x y (fun ~r ~g ~b ->
         Printf.fprintf och "%i %i %i\n" r g b)
       done;
     done
   | GreyL _, Binary, 1  ->
     Printf.fprintf och "P4\n%i %i\n" w h;
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
	 read_grey_pixel img x y (fun ~g ->
         output_bit g)
       done;

       flush_byte ()
     done

   | GreyL greymap, ASCII,  1  ->
     let header = Printf.sprintf "P1\n%i %i\n" w h in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_grey_pixel img x y (fun ~g ->
         Printf.fprintf och "%i\n" g)
       done;
     done

   | GreyL greymap, Binary, mv ->
     let header = Printf.sprintf "P5\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
        read_grey_pixel img x y (fun ~g ->
          if mv < 256
          then Printf.fprintf och "%c" (char_of_int g)
          else begin
            let gl0 = char_of_int (g mod 256) in
            let gl1 = char_of_int (g lsr 8) in
            Printf.fprintf och "%c%c" gl1 gl0
          end)
       done;
     done

   | GreyL greymap, ASCII,  mv ->
     let header = Printf.sprintf "P2\n%i %i %i\n" w h mv in
     output_string och header;
     for y = 0 to h - 1 do
       for x = 0 to w - 1 do
         read_grey_pixel img x y (fun ~g ->
         Printf.fprintf och "%i\n" g)
       done;
     done
  );

  close_out och
;;
