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
open CamomileLibrary
open Config

(** Convertir en points Adobe une longueur en millimètres *)
let pt_of_mm x=(72.*.x)/.25.4
(** Convertir en millimètres une longueur en points Adobe *)
let mm_of_pt x=(25.4*.x)/.72.


let a4=(210.,297.)
let phi=(1.+.(sqrt 5.))/.2.


let readInt f n0=
  let rec readInt_ n x=
    if n=n0 then x else
      readInt_ (n+1) ((x lsl 8) + (input_byte f))
  in
    readInt_ 0 0

let buf="    "

let readInt2 f=
  really_input f buf 0 2;
  let d=((int_of_char buf.[0]) lsl 8) lor (int_of_char buf.[1]) in
    d
let sreadInt2 f=
  really_input f buf 0 2;
  let d=((int_of_char buf.[0]) lsl 8) lor (int_of_char buf.[1]) in
  if d>=0x8000 then d-0x10000 else d

let strInt2 s i x=
  s.[i]<-char_of_int ((x lsr 8) land 0xff);
  s.[i+1]<-char_of_int (x land 0xff)

#ifdef INT32

let strInt4 s i x=
  let a=Int32.shift_right x 8 in
  let b=Int32.shift_right a 8 in
  let c=Int32.shift_right b 8 in
  s.[i]<-char_of_int (Int32.to_int (Int32.logand c 0xffl));
  s.[i+1]<-char_of_int (Int32.to_int (Int32.logand b 0xffl));
  s.[i+2]<-char_of_int (Int32.to_int (Int32.logand a 0xffl));
  s.[i+3]<-char_of_int (Int32.to_int (Int32.logand x 0xffl))

let strInt4_int s i x=
  let a=x lsr 8 in
  let b=a lsr 8 in
  let c=b lsr 8 in
  s.[i]<-char_of_int (c land 0xff);
  s.[i+1]<-char_of_int (b land 0xff);
  s.[i+2]<-char_of_int (a land 0xff);
  s.[i+3]<-char_of_int (x land 0xff)
#else

let strInt4 s i x=
  let a=x lsr 8 in
  let b=a lsr 8 in
  let c=b lsr 8 in
  s.[i]<-char_of_int (c land 0xff);
  s.[i+1]<-char_of_int (b land 0xff);
  s.[i+2]<-char_of_int (a land 0xff);
  s.[i+3]<-char_of_int (x land 0xff)
let strInt4_int=strInt4

#endif

let buf2="  "
let writeInt2 f x=
  strInt2 buf2 0 x;
  output_string f buf2

let buf4="    "
let writeInt4 f x=
  strInt4 buf4 0 x;
  output_string f buf4


let bufInt1 b x=
  Rbuffer.add_char b (char_of_int (x land 0xff))

let bufInt2 b x=
  Rbuffer.add_char b (char_of_int ((x lsr 8) land 0xff));
  Rbuffer.add_char b (char_of_int (x land 0xff))

#ifdef INT32

let bufInt4 b x=
  let u=Int32.shift_right x 8 in
  let v=Int32.shift_right u 8 in
  let w=Int32.shift_right v 8 in
  Rbuffer.add_char b (char_of_int (Int32.to_int (Int32.logand w 0xffl)));
  Rbuffer.add_char b (char_of_int (Int32.to_int (Int32.logand v 0xffl)));
  Rbuffer.add_char b (char_of_int (Int32.to_int (Int32.logand u 0xffl)));
  Rbuffer.add_char b (char_of_int (Int32.to_int (Int32.logand x 0xffl)))

let bufInt4_int b x=
  let u=x lsr 8 in
  let v=u lsr 8 in
  let w=v lsr 8 in
  Rbuffer.add_char b (char_of_int (w land 0xff));
  Rbuffer.add_char b (char_of_int (v land 0xff));
  Rbuffer.add_char b (char_of_int (u land 0xff));
  Rbuffer.add_char b (char_of_int (x land 0xff))

#else

let bufInt4 b x=
  let u=x lsr 8 in
  let v=u lsr 8 in
  let w=v lsr 8 in
  Rbuffer.add_char b (char_of_int (w land 0xff));
  Rbuffer.add_char b (char_of_int (v land 0xff));
  Rbuffer.add_char b (char_of_int (u land 0xff));
  Rbuffer.add_char b (char_of_int (x land 0xff))

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
  let a=(int_of_char buf.[0]) lsl 8 in
  let b=(a lor (int_of_char buf.[1])) lsl 8 in
  let c=(b lor (int_of_char buf.[2])) lsl 8 in
  let d=c lor (int_of_char buf.[3]) in
    d
let readInt4_int=readInt4
#endif

let int16 x=if x<0x8000 then x else x-0x10000

let round x=
  let c=ceil x in
    if (c-.x) < 0.5 && (c-.x)> -0.5 then int_of_float c else
      if c-.x=0.5 || c-.x=(-0.5) then
        if int_of_float (floor x) mod 2=0 then int_of_float (floor x) else int_of_float c
      else
        int_of_float (floor x)

let round_float x=float_of_int (round x)


let rec span f=function
    []->([],[])
  | h::s when f h->let (a,b)=span f s in (h::a, b)
  | l->([],l)

let rec break f=function
    []->([],[])
  | h::s when not (f h)->let (a,b)=break f s in (h::a, b)
  | l->([],l)

let rec take x l=
  if x<=0 then [] else
    match l with
        []->[]
      | h::s->h::(take (x-1) s)

let rec drop x l=
  if x<=0 then l else
    match l with
        []->[]
      | _::s->drop (x-1) s

let rec last=function
    [h]->h
  | _::s->last s
  | _-> raise Not_found

let rec init=function
    [_] | [] ->[]
  | h::s->h::init s

let is_space x0=
  let x=UChar.uint_code x0 in
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

let compose f g x=f (g x)



module IntMap=Map.Make (struct type t=int let compare=compare end)
module StrMap=Map.Make (String)
module IntSet=Set.Make (struct type t=int let compare=compare end)
module IntListMap=Map.Make(struct type t=int list let compare=compare end)


type 'a tree=
    N of ('a tree) IntMap.t
  | L of 'a


let bin_cache:in_channel StrMap.t ref=ref StrMap.empty
let cache:in_channel StrMap.t ref=ref StrMap.empty

let open_in_bin_cached f=
  if not (StrMap.mem f !bin_cache) then (
    if StrMap.mem f !cache then (
      close_in (StrMap.find f !cache);
      cache:=StrMap.remove f !cache
    );
    bin_cache:=StrMap.add f (open_in_bin f) !bin_cache
  );
  StrMap.find f !bin_cache


let open_in_cached f=
#ifdef WIN32
  if not (StrMap.mem f !cache) then (
    if StrMap.mem f !bin_cache then (
      close_in (StrMap.find f !bin_cache);
      bin_cache:=StrMap.remove f !bin_cache
    );
    cache:=StrMap.add f (open_in f) !cache
  );
  StrMap.find f !cache
#else
  open_in_bin_cached f
#endif

let copy_file a b=
  let fa=open_in a in
  let fb=open_out b in
  let s=String.create 1000 in
  let rec copy ()=
    let x=input fa s 0 (String.length s) in
    if x>0 then (
      output fb s 0 x;
      copy ()
    )
  in
  copy ();
  close_in fa;
  close_out fb
