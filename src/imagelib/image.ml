(****************************************************************************
 * Data types used by the image input / output fonctions                    *
 ****************************************************************************)

open Bigarray

exception Not_yet_implemented of string
exception Wrong_image_type

type component = int

type pixmap = 
(* 8 bits per componants *)
| Bits8 of (int, int8_unsigned_elt, c_layout) Array2.t
(* 16 bits per componants *)
| Bits16 of (int, int16_unsigned_elt, c_layout) Array2.t

type cpixmap = RGB of pixmap * pixmap * pixmap (*r, g, b*) 
            | GreyL of pixmap

type image = {
  width   : int;
  height  : int;
  max_val : component;
  pixels  : cpixmap ;
  alpha   : pixmap option ;
}

type pixel = { r : int; g : int; b : int }

let create_pixmap8 width height =
  Bits8(Array2.create int8_unsigned c_layout width height)

let create_pixmap16 width height =
  Bits16(Array2.create int16_unsigned c_layout width height)

let create_rgb 
    ?(alpha=false)
    ?(max_val=255)
    width
    height =
  assert (max_val <= 65535);
  let create = if max_val <= 255 then create_pixmap8 
    else create_pixmap16
  in
{
  width;
  height;
  max_val;
  pixels = RGB(create width height,
	       create width height,
	       create width height);
  alpha = match alpha with
    false -> None
  | true -> Some(create width height)
}

let create_grey 
    ?(alpha=false)
    ?(max_val=255)
    width
    height =
  assert (max_val <= 65535);
  let create = if max_val <= 255 then create_pixmap8 
    else create_pixmap16
  in
{
  width;
  height;
  max_val;
  pixels = GreyL(create width height);
  alpha = match alpha with
    false -> None
  | true -> Some(create width height)
}

module type ReadImage =
  sig
    exception Corrupted_Image of string
    val extensions : string list
    val size : string -> int * int
    val openfile : string -> image
  end

let read_pixmap p i j = match p with
| Bits8 p -> Array2.get p i j 
| Bits16 p -> Array2.get p i j 

let write_pixmap p i j v = match p with
| Bits8 p -> Array2.set p i j v
| Bits16 p -> Array2.set p i j v

let read_rgba_pixel i x y fn =
  let r,g, b = match i.pixels with
      RGB(r,g,b) ->
	let r = read_pixmap r x y in
	let g = read_pixmap g x y in
	let b = read_pixmap b x y in
	r,g,b
    | GreyL(g) ->
      	let g = read_pixmap g x y in
	g,g,g
  in
  let a = match i.alpha with
    | None -> i.max_val
    | Some p -> read_pixmap p x y
  in
  fn  ~r ~g ~b ~a

let read_rgb_pixel i x y fn =
  let r,g, b = match i.pixels with
      RGB(r,g,b) ->
	let r = read_pixmap r x y in
	let g = read_pixmap g x y in
	let b = read_pixmap b x y in
	r,g,b
    | GreyL(g) ->
      	let g = read_pixmap g x y in
	g,g,g
  in
  fn ~r ~g ~b

let read_greya_pixel i x y fn =
  let g = match i.pixels with
      RGB(r,g,b) ->
	let r = read_pixmap r x y in
	let g = read_pixmap g x y in
	let b = read_pixmap b x y in
	(r+g+b) / 3
    | GreyL(g) ->
      	let g = read_pixmap g x y in
	g
  in
  let a = match i.alpha with
    | None -> i.max_val
    | Some p -> read_pixmap p x y
  in
  fn ~g ~a

let read_grey_pixel i x y fn =
  let g = match i.pixels with
      RGB(r,g,b) ->
	let r = read_pixmap r x y in
	let g = read_pixmap g x y in
	let b = read_pixmap b x y in
	(r+g+b) / 3
    | GreyL(g) ->
      	let g = read_pixmap g x y in
	g
  in
  fn ~g

let write_rgba_pixel i x y r g b a =
  (match i.pixels with
      RGB(pr,pg,pb) ->
	write_pixmap pr x y r;
	write_pixmap pg x y g;
	write_pixmap pb x y b;
    | GreyL(pg) ->
      let g = (r + g + b) / 3 in
      write_pixmap pg x y g);
  match i.alpha with
  | None -> ()
  | Some p -> write_pixmap p x y a

let write_rgb_pixel i x y r g b =
  match i.pixels with
      RGB(pr,pg,pb) ->
	write_pixmap pr x y r;
	write_pixmap pg x y g;
	write_pixmap pb x y b;
    | GreyL(pg) ->
      let g = (r + g + b) / 3 in
      write_pixmap pg x y g

let write_greya_pixel i x y g a =
  (match i.pixels with
  | RGB(pr,pg,pb) ->
    write_pixmap pr x y g;
    write_pixmap pg x y g;
    write_pixmap pb x y g;
  | GreyL(pg) ->
    write_pixmap pg x y g);
  match i.alpha with
  | None -> ()
  | Some p -> write_pixmap p x y a
    
let write_grey_pixel i x y g =
  match i.pixels with
  | RGB(pr,pg,pb) ->
    write_pixmap pr x y g;
    write_pixmap pg x y g;
    write_pixmap pb x y g;
  | GreyL(pg) ->
    write_pixmap pg x y g

