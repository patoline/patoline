(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)

open Patutil.Extra

let readInt f n0 =
  let rec aux n x =
    if n = n0 then x
    else aux (n+1) ((x lsl 8) + (input_byte f))
  in
  aux 0 0

let buf = Bytes.make 4 ' '

let readInt2 f =
  really_input f buf 0 2;
  let c0 = int_of_char (Bytes.get buf 0) in
  let c1 = int_of_char (Bytes.get buf 1) in
  (c0 lsl 8) lor c1

let sreadInt2 f =
  let d = readInt2 f in
  if d >= 0x8000 then d - 0x10000 else d

let strInt2 s i x=
  Bytes.set s i (char_of_int ((x lsr 8) land 0xff));
  Bytes.set s (i+1) (char_of_int (x land 0xff))

let strInt4 s i x=
  let a=Int32.shift_right x 8 in
  let b=Int32.shift_right a 8 in
  let c=Int32.shift_right b 8 in
  Bytes.set s i     (char_of_int (Int32.to_int (Int32.logand c 0xffl)));
  Bytes.set s (i+1) (char_of_int (Int32.to_int (Int32.logand b 0xffl)));
  Bytes.set s (i+2) (char_of_int (Int32.to_int (Int32.logand a 0xffl)));
  Bytes.set s (i+3) (char_of_int (Int32.to_int (Int32.logand x 0xffl)))

let strInt4_int s i x=
  let a=x lsr 8 in
  let b=a lsr 8 in
  let c=b lsr 8 in
  Bytes.set s (i)   (char_of_int (c land 0xff));
  Bytes.set s (i+1) (char_of_int (b land 0xff));
  Bytes.set s (i+2) (char_of_int (a land 0xff));
  Bytes.set s (i+3) (char_of_int (x land 0xff))

let buf2 = Bytes.make 2 ' '

let writeInt2 f x =
  strInt2 buf2 0 x;
  output_bytes f buf2

let buf4 = Bytes.make 4 ' '

let writeInt4 f x=
  strInt4 buf4 0 x;
  output_bytes f buf4


let bufInt1 b x=
  Buffer.add_char b (char_of_int (x land 0xff))

let bufInt2 b x=
  Buffer.add_char b (char_of_int ((x lsr 8) land 0xff));
  Buffer.add_char b (char_of_int (x land 0xff))

let bufInt4 b x=
  let u=Int32.shift_right x 8 in
  let v=Int32.shift_right u 8 in
  let w=Int32.shift_right v 8 in
  Buffer.add_char b (char_of_int (Int32.to_int (Int32.logand w 0xffl)));
  Buffer.add_char b (char_of_int (Int32.to_int (Int32.logand v 0xffl)));
  Buffer.add_char b (char_of_int (Int32.to_int (Int32.logand u 0xffl)));
  Buffer.add_char b (char_of_int (Int32.to_int (Int32.logand x 0xffl)))

let bufInt4_int b x=
  let u=x lsr 8 in
  let v=u lsr 8 in
  let w=v lsr 8 in
  Buffer.add_char b (char_of_int (w land 0xff));
  Buffer.add_char b (char_of_int (v land 0xff));
  Buffer.add_char b (char_of_int (u land 0xff));
  Buffer.add_char b (char_of_int (x land 0xff))

let int32_of_char x=Int32.of_int (int_of_char x)
let readInt4 f=
  really_input f buf 0 4;
  let (a,b,c,d) = Bytes.(get buf 0, get buf 1, get buf 2, get buf 3) in
  let a = Int32.shift_left (int32_of_char a) 8 in
  let b = Int32.shift_left (Int32.logor a (int32_of_char b)) 8 in
  let c = Int32.shift_left (Int32.logor b (int32_of_char c)) 8 in
  let d = Int32.logor c (int32_of_char d) in d

let readInt4_int f=
  really_input f buf 0 4;
  let (a,b,c,d) = Bytes.(get buf 0, get buf 1, get buf 2, get buf 3) in
  let a = (int_of_char a) lsl 8 in
  let b = (a lor (int_of_char b)) lsl 8 in
  let c = (b lor (int_of_char c)) lsl 8 in
  let d = c lor (int_of_char d) in d

let int16 x=if x<0x8000 then x else x-0x10000

let bin_cache:in_channel StrMap.t ref=ref StrMap.empty
let cache:in_channel StrMap.t ref=ref StrMap.empty

let close_in_cache () =
  StrMap.iter (fun _ f -> close_in f) !cache;
  StrMap.iter (fun _ f -> close_in f) !bin_cache;
  cache := StrMap.empty;
  bin_cache := StrMap.empty

let open_in_bin_cached f=
  if not (StrMap.mem f !bin_cache) then (
    if StrMap.mem f !cache then (
      close_in (StrMap.find f !cache);
      cache:=StrMap.remove f !cache
    );
    bin_cache:=StrMap.add f (open_in_bin f) !bin_cache
  );
  let ch = StrMap.find f !bin_cache in
  seek_in ch 0; ch

let open_in_cached = open_in_bin_cached
