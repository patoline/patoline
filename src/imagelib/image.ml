(****************************************************************************
 * Data types used by the image input / output fonctions                    *
 ****************************************************************************)

exception Wrong_image_type

type component = int

type rgb = {
  r : component;
  g : component;
  b : component;
}

type pixmap = RGB of rgb array array
            | GreyL of component array array

type image = {
  size    : int * int ;
  max_val : component;
  pixels  : pixmap ;
  alpha   : component array array option ;
}

module type ReadImage =
  sig
    exception Corrupted_Image of string
    val extensions : string list
    val size : string -> int * int
    val openfile : string -> image
  end

let read_pixel i x y =
  let (<<)  = Int32.shift_left in
  let (|||) = Int32.logor in
  match i.pixels with
  | RGB ps   -> let {r;g;b} = ps.(x).(y) in
                let r = Int32.of_int r in
                let g = Int32.of_int g in
                let b = Int32.of_int b in
                let al = (match i.alpha with
                          | None     -> Int32.zero
                          | Some als -> Int32.of_int als.(x).(y))
                in
                ((((al << 8) ||| r) ||| g) << 8) ||| b
  | GreyL ps -> let g = Int32.of_int ps.(x).(y) in
                let al = (match i.alpha with
                          | None     -> Int32.zero
                          | Some als -> Int32.of_int als.(x).(y))
                in
                ((((al << 8) ||| g) ||| g) << 8) ||| g

let write_pixel i x y p =
  let (>>) = Int32.shift_right in
  let (&&) = Int32.logand in
  (match i.pixels with
   | RGB ps   -> let r  = Int32.to_int ((p >> 16) && 255l) in
                 let g  = Int32.to_int ((p >> 8) && 255l) in
                 let b  = Int32.to_int (p && 255l) in
                 ps.(x).(y) <- {r;g;b}
   | GreyL ps -> let g = Int32.to_int (p && 255l) in
                 ps.(x).(y) <- g);
  let al = Int32.to_int (p && 255l) in
  match i.alpha with
  | None     -> () (* NOTE create a table if value of al is not 0? *)
  | Some als -> als.(x).(y) <- al
