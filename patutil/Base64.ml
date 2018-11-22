(** [encode str] encodes [str] using Base 64 Encoding (RFC4648). *)
let encode : string -> string = fun str ->
  let pad = (3 - (String.length str mod 3)) mod 3 in
  let str = str ^ String.make pad '\x00' in
  let buf = Buffer.create (String.length str * 2) in
  let encode64 x =
    let y = x land 0x3f in
    if y < 26 then char_of_int (y + int_of_char 'A') else
    if y < 52 then char_of_int (y - 26 + int_of_char 'a') else
    if y < 62 then char_of_int (y - 52 + int_of_char '0') else
    if y = 62 then '+' else '/'
  in
  for pos = 0 to String.length str / 3 - 1 do
    let off = pos * 3 in
    let i0 = int_of_char str.[off]   in 
    let i1 = int_of_char str.[off+1] in
    let i2 = int_of_char str.[off+2] in
    let x = (((i0 lsl 8) lor i1) lsl 8) lor i2 in
    Buffer.add_char buf (encode64 (x lsr 18));
    Buffer.add_char buf (encode64 (x lsr 12));
    Buffer.add_char buf (encode64 (x lsr 6 ));
    Buffer.add_char buf (encode64 x);
  done;
  let str = Buffer.to_bytes buf in
  if pad > 0 then Bytes.set str (Bytes.length str - 1) '=';
  if pad > 1 then Bytes.set str (Bytes.length str - 2) '=';
  Bytes.unsafe_to_string str

(** [decode str] decodes [str] according to Base 64 Encoding (RFC4648). If the
 * input string contains invalid characters [Invalid_argument] is raised. *)
let decode : string -> string = fun str ->
  let len = String.length str in
  if len mod 4 <> 0 then invalid_arg "Base64.decode - not properly padded";
  let buf = Buffer.create len in
  let decode64 c =
    match c with
    | 'A'..'Z' -> (int_of_char c) - (int_of_char 'A')
    | 'a'..'z' -> 26 + (int_of_char c) - (int_of_char 'a')
    | '0'..'9' -> 52 + (int_of_char c) - (int_of_char '0')
    | '+'      -> 62
    | '/'      -> 63
    | '='      -> 64
    | _        -> invalid_arg "Base64.decode - invalid character"
  in
  for pos = 0 to (len / 4) - 1 do
    let off = pos * 4 in
    let v0 = decode64 str.[off]   in
    let v1 = decode64 str.[off+1] in
    let v2 = decode64 str.[off+2] in
    let v3 = decode64 str.[off+3] in
    if v3 = 64 then
      if v2 = 64 then
        let x = (v0 lsl 6) lor v1 in
        Buffer.add_char buf (char_of_int ((x lsr 4) land 0xff));
      else
        let x = (((v0 lsl 6) lor v1) lsl 6) lor v2 in
        Buffer.add_char buf (char_of_int ((x lsr 10) land 0xff));
        Buffer.add_char buf (char_of_int ((x lsr 2) land 0xff));
    else
      let x= (((((v0 lsl 6) lor v1) lsl 6) lor v2) lsl 6) lor v3 in
      Buffer.add_char buf (char_of_int ((x lsr 16) land 0xff));
      Buffer.add_char buf (char_of_int ((x lsr 8) land 0xff));
      Buffer.add_char buf (char_of_int (x land 0xff));
  done;
  Buffer.contents buf
