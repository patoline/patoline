open Pervasives

(*
 * Reads all the lines in the channel by calling input_line.
 * Returns a list of strings.
 *)
let lines_from_channel ich =
  let lines = ref [] in

  let rec intfun () =
    try
      let l = input_line ich in
      lines := l :: !lines;
      intfun ();
    with
     | _ -> ()
  in
  intfun ();
  List.rev !lines
;;

(*
 * Same as lines_from_channel but from a file.
 *)
let lines_from_file fn =
  let ich = open_in_bin fn in
  let ls = lines_from_channel ich in
  close_in ich; ls
;;

(*
 * Test whether a character is a digit.
 *)
let is_digit c =
  List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
;;

(*
 * Reads a list of positive integers written in decimal from a string.
 * The integers can be separated by anything.
 *)
let int_array_from_string str =
  let words = ref [] in

  let wpos = ref (-1) in

  let len = String.length str in
  for i = 0 to len - 1 do
    if is_digit str.[i]
    then begin
      if !wpos < 0 then wpos := i;
      if i = len - 1
      then begin
        let w = String.sub str !wpos (len - !wpos) in
        words := (int_of_string w) :: !words;
      end
    end else begin
      if !wpos >= 0
      then begin
        let w = String.sub str !wpos (i - !wpos) in
        words := (int_of_string w) :: !words;
        wpos := -1;
      end
    end
  done;

  Array.of_list (List.rev !words)
;;

(*
 * Read a string and returns the integer value of every character in an
 * array.
 *)
let byte_array_from_string str =
  let len = String.length str in
  Array.init len (fun i -> int_of_char str.[i])
;;

(*
 * Computes the n-th power of 2.
 *)
let rec pow_of_2 n =
  if n = 0 then 1
  else (2 * pow_of_2 (n - 1))
;;

(*
 * Same as Array.init but for dimension 2.
 * Arguments:
 *   - (w,h) : width / height couple.
 *   - f : initialization function.
 *)
let init_matrix (w,h) f =
  Array.init w (fun x -> Array.init h (fun y -> f (x,y)))
;;

(*
 * Displays the hexadecimal representation of a string in the same way as in
 * an hexadecimal editor.
 * Arguments:
 *   - s : the string.
 *)
let show_string_hex s =
  let len = String.length s in
  let count = ref 0 in
  for i = 0 to len - 1 do
    let v = int_of_char s.[i] in
    Printf.fprintf stderr "%s%x" (if v < 16 then "0" else "") v;
    incr count;
    Printf.fprintf stderr "%s"
      (if !count mod 16 = 0 then "\n" else " ")
  done;
  if !count mod 16 <> 0 then Printf.fprintf stderr "\n";
  Printf.fprintf stderr "%!"
;;

(*
 * Fetch bytes on an input channel and store them in a string.
 * Arguments:
 *   - ich : the input channel.
 *   - n : number of bytes to fecth
 * Returns a string of length n.
 *)
let get_bytes ich n =
  let str = String.create n in
  really_input ich str 0 n; str
;;

(*
 * Builds an integer which byte reprentation contains a given number of ones
 * starting form the right.
 * Arguments:
 *   - i : the number of ones.
 * Returns an integer.
 *)
let rec ones i = if i == 1 then 1 else ((ones (i-1)) lsl 1) lor 1 ;;

(*
 * Converts a string of size 4 into an integer WITHOUT taking care of
 * overflow...
 * Arguments:
 *   - s : the string.
 * Returns an integer.
 *)
let int_of_str4 s =
  int_of_char s.[0] lsl 24 +
  int_of_char s.[1] lsl 16 +
  int_of_char s.[2] lsl 8 +
  int_of_char s.[3]
;;

(*
 * Converts a string of size 4 into an Int32.
 * Arguments:
 *   - s : the string (should have size 4 at least).
 * Returns an Int32.
 *)
let int32_of_str4 s =
  let (<<) = Int32.shift_left in
  let (++) = Int32.add in
  ((Int32.of_int (int_of_char s.[0])) << 24) ++
  ((Int32.of_int (int_of_char s.[1])) << 16) ++
  ((Int32.of_int (int_of_char s.[2])) << 8) ++
  (Int32.of_int (int_of_char s.[3]))

(*
 * Converts an integer into a string of length 4.
 * Arguments:
 *   - i : the integer
 * Returns a string.
 *)
let int_to_str4 i =
  let s = String.create 4 in
  let mask = ones 8 in
  s.[0] <- char_of_int (i lsr 24);
  s.[1] <- char_of_int ((i lsr 16) land mask);
  s.[2] <- char_of_int ((i lsr 8) land mask);
  s.[3] <- char_of_int (i land mask);
  s
;;
