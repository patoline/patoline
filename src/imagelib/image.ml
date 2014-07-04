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

let create_rgb 
    ?(background={ r = 0; g = 0; b = 0 })
    ?(alpha=None)
    ?(max_val=255)
    width
    height =
{
  size = (width, height);
  max_val;
  pixels = RGB(Array.create_matrix width height background);
  alpha = match alpha with
    None -> None
  | Some default -> Some(Array.create_matrix width height default)
}

let create_grey 
    ?(background=0)
    ?(alpha=None)
    ?(max_val=255)
    width
    height =
{
  size = (width, height);
  max_val;
  pixels = GreyL(Array.create_matrix width height background);
  alpha = match alpha with
    None -> None
  | Some default -> Some(Array.create_matrix width height default)
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
                let al = (match i.alpha with
                          | None     -> i.max_val
                          | Some als -> als.(x).(y))
                in
		let r,g,b,al =
		  if i.max_val <> 255 then
		    (r * 256) / i.max_val,
		    (g * 256) / i.max_val,
		    (b * 256) / i.max_val,
		    (al * 256) / i.max_val
		  else r,g,b,al
		in		  
                let r = Int32.of_int r in
                let g = Int32.of_int g in
                let b = Int32.of_int b in
                let al = Int32.of_int al in
                (((((al << 8) ||| b) << 8) ||| g) << 8) ||| r
  | GreyL ps -> let g = ps.(x).(y) in
                let al = (match i.alpha with
                          | None     -> i.max_val
                          | Some als -> als.(x).(y))
                in
 		let g,al =
		  if i.max_val <> 255 then
		    (g * 256) / i.max_val,
		    (al * 256) / i.max_val
		  else g,al
		in		  
                let g = Int32.of_int g in
                let al = Int32.of_int al in
                (((((al << 8) ||| g) << 8) ||| g) << 8) ||| g

let write_pixel i x y p =
  let (>>) = Int32.shift_right in
  let (&&) = Int32.logand in
  (match i.pixels with
   | RGB ps   -> let b  = Int32.to_int ((p >> 16) && 255l) in
                 let g  = Int32.to_int ((p >> 8) && 255l) in
                 let r  = Int32.to_int (p && 255l) in
		 let r,g,b =
		   if i.max_val <> 255 then
		     (r * i.max_val) / 256,
		     (g * i.max_val) / 256,
		     (b * i.max_val) / 256
		   else r,g,b
		 in		  
                 ps.(x).(y) <- {r;g;b}
   | GreyL ps -> let g = Int32.to_int (p && 255l) in
		 let g =
		   if i.max_val <> 255 then
		     (g * i.max_val) / 256
		   else g
		 in		  
                 ps.(x).(y) <- g);
  let al = Int32.to_int ((p >> 24) && 255l) in
  let al =
    if i.max_val <> 255 then
      (al * i.max_val) / 256
    else al
  in		  
  match i.alpha with
  | None     -> () (* NOTE create a table if value of al is not 0? *)
  | Some als -> als.(x).(y) <- al
