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

open Extra

(** [pt_of_mm l] converts [l] from Adobe points to millimeters. *)
let pt_of_mm : float -> float = fun x -> (72.0 *. x) /. 25.4

(** [mm_of_pt l] converts [l] from millimeters to Adobe points. *)
let mm_of_pt : float -> float = fun x -> (25.4 *. x) /. 72.0

(** Width and height of the A4 page format (in millimeters). *)
let a4 : float * float = (210.0, 297.0)

(** Golden ratio. *)
let phi : float = (1.0 +. sqrt 5.0) /. 2.0

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

#ifdef INT32

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
#else

let strInt4 s i x=
  let a=x lsr 8 in
  let b=a lsr 8 in
  let c=b lsr 8 in
  Bytes.set s (i)   (char_of_int (c land 0xff));
  Bytes.set s (i+1) (char_of_int (b land 0xff));
  Bytes.set s (i+2) (char_of_int (a land 0xff));
  Bytes.set s (i+3) (char_of_int (x land 0xff))
let strInt4_int=strInt4

#endif

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

#ifdef INT32

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

#else

let bufInt4 b x=
  let u=x lsr 8 in
  let v=u lsr 8 in
  let w=v lsr 8 in
  Buffer.add_char b (char_of_int (w land 0xff));
  Buffer.add_char b (char_of_int (v land 0xff));
  Buffer.add_char b (char_of_int (u land 0xff));
  Buffer.add_char b (char_of_int (x land 0xff))

let bufInt4_int=bufInt4

#endif

#ifdef INT32
let int32_of_char x=Int32.of_int (int_of_char x)
let readInt4 f=
  really_input f buf 0 4;
  let a=Int32.shift_left (int32_of_char buf.[0]) 8 in
  let b=Int32.shift_left (Int32.logor a (int32_of_char buf.[1])) 8 in
  let c=Int32.shift_left (Int32.logor b (int32_of_char buf.[2])) 8 in
  let d=Int32.logor c (int32_of_char buf.[3]) in
  d
let readInt4_int f=
  really_input f buf 0 4;
  let a=(int_of_char buf.[0]) lsl 8 in
  let b=(a lor (int_of_char buf.[1])) lsl 8 in
  let c=(b lor (int_of_char buf.[2])) lsl 8 in
  let d=c lor (int_of_char buf.[3]) in
    d
#else
let readInt4 f=
  really_input f buf 0 4;
  let r = (int_of_char (Bytes.get buf 0)) lsl 8 in
  let r = (r lor (int_of_char (Bytes.get buf 1))) lsl 8 in
  let r = (r lor (int_of_char (Bytes.get buf 2))) lsl 8 in
  r lor (int_of_char (Bytes.get buf 3))

let readInt4_int=readInt4
#endif

let int16 x=if x<0x8000 then x else x-0x10000

let is_space x0=
  let x=UChar.code x0 in
    (x>=0x0009 && x<=0x000d)
  || x=0x0020
  || x=0x00a0
  || x=0x1680
  || x=0x180e
  || (x>=0x2000 && x<=0x200a)
  || x=0x202f
  || x=0x205f
  || x=0x3000

let unspace s=
  let rec rem0 i=
    if is_space (UTF8.look s i) then
      rem0 (UTF8.next s i)
    else
      i
  in
  let rec rem1 j=
    if is_space (UTF8.look s j) then
      rem1 (UTF8.prev s j)
    else
      UTF8.next s j
  in
  let a=rem0 0 and b=rem1 (UTF8.last s) in
  String.sub s a (b-a)

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

let open_in_cached f=
#ifdef WIN32
  if not (StrMap.mem f !cache) then (
    if StrMap.mem f !bin_cache then (
      close_in (StrMap.find f !bin_cache);
      bin_cache:=StrMap.remove f !bin_cache
    );
    cache:=StrMap.add f (open_in f) !cache
  );
  let ch = StrMap.find f !cache in
  seek_in ch 0; ch
#else
  open_in_bin_cached f
#endif

let copy_file a b=
  let fa = open_in a in
  let fb = open_out b in
  let s = Bytes.create 1000 in
  let rec copy () =
    let x = input fa s 0 1000 in
    if x > 0 then (output fb s 0 x; copy ())
  in
  copy ();
  close_in fa;
  close_out fb

let btimer=ref StrMap.empty
let timer name f=
  let t0=Sys.time () in
  let x=f () in
  let t1=Sys.time () in
  let t=try StrMap.find name !btimer with Not_found->0. in
  btimer:= StrMap.add name (t+.t1-.t0) !btimer;
  StrMap.iter (fun k a->Printf.fprintf stderr "time %S: %g\n" k a) !btimer;flush stderr;
  x

(* a lighter split that calling str *)
let split char str =
  let len = String.length str in
  let rec fn beg pos acc =
    if pos >= len then List.rev (String.sub str beg (pos - beg)::acc)
    else if str.[pos] = char then fn (pos+1) (pos+1)  (String.sub str beg (pos - beg)::acc)
    else fn beg (pos+1) acc
  in
  fn 0 0 []

(* alternative implementation
let split c s =
  let rec split s acc =
    try
      let i = String.index s c in
      let e = String.sub s 0 i in
      let s' = String.sub s (i+1) (String.length s - i - 1) in
      split s' (e :: acc)
    with _ -> List.rev (s :: acc)
  in
  split s []
*)

(* A type needed both by Db and RawContent *)
type visibility = Private | Group | Public
let vis_to_string = function
  | Private -> "Private"
  | Group -> "Group"
  | Public -> "Public"
