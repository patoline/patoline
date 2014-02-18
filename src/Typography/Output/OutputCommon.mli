type lineCap = Butt_cap | Round_cap | Proj_square_cap
type lineJoin = Miter_join | Round_join | Bevel_join
type rgb = { red : float; green : float; blue : float; }
type color = RGB of rgb
val rgb : float -> float -> float -> color
val mix : float -> color -> color -> color
val black : color
val white : color
val blue : color
val green : color
val red : color
val orange : color
val purple : color
val pink : color
val yellow : color
val gray : color
val grey : color
val hsv : float -> float -> float -> color
type metadata =
    Contributor
  | Coverage
  | Creator
  | Date
  | Description
  | Format
  | Identifier
  | Language
  | Publisher
  | Relation
  | Rights
  | Source
  | Subject
  | Title
  | Type
type path_parameters = {
  path_order : int;
  close : bool;
  strokingColor : color option;
  fillColor : color option;
  lineCap : lineCap;
  lineJoin : lineJoin;
  lineWidth : float;
  dashPattern : float list;
}
val default : path_parameters
type glyph = {
  glyph_x : float;
  glyph_kx : float;
  glyph_y : float;
  glyph_ky : float;
  glyph_order : int;
  glyph_color : color;
  glyph_size : float;
  glyph : Fonts.glyph;
}
type image = {
  image_file : string;
  image_x : float;
  image_y : float;
  image_order : int;
  image_height : float;
  image_width : float;
  image_pixel_width : int;
  image_pixel_height : int;
}
type video = {
  video_file : string;
  video_x : float;
  video_y : float;
  video_order : int;
  video_height : float;
  video_width : float;
  video_pixel_width : int;
  video_pixel_height : int;
}
type button_kind = Clickable | Dragable | Editable of string * string
type link_kind =
    Extern of string
  | Intern of (string * int * float * float)
  | Button of button_kind * string * string list
type link = {
  mutable link_x0 : float;
  mutable link_y0 : float;
  mutable link_x1 : float;
  mutable link_y1 : float;
  mutable link_closed : bool;
  link_order : int;
  link_kind : link_kind;
  mutable link_contents : raw list;
}
and states = {
  states_contents : raw list;
  states_states : int list;
  states_order : int;
}
and animation = {
  anim_contents : raw list array;
  anim_default : int;
  anim_step : float;
  anim_duration : float;
  anim_mirror : bool;
  anim_order : int;
}
and event =
    Click of string
  | Drag of string * (float * float)
  | Edit of string * string
and action = Unchanged | Private | Public
and 'a dynamic = {
  dyn_label : string;
  dyn_contents : unit -> 'a;
  dyn_sample : 'a;
  dyn_react : event -> action;
  dyn_order : int;
  dyn_father : string option;
}
and affine = {
  affine_matrix : float array array;
  affine_contents : raw list;
  affine_order : int
}
and raw =
    Glyph of glyph
  | Path of path_parameters * Bezier.curve array list
  | Affine of affine
  | Link of link
  | Image of image
  | Video of video
  | States of states
  | Animation of animation
  | Dynamic of raw list dynamic
type page = {
  mutable pageFormat : float * float;
  mutable pageContents : raw list;
}

val states : int list -> raw list -> raw
val defaultPage : page
val translate : float -> float -> raw -> raw
val resize : float -> raw -> raw
type bounding_box_opt = {
  ignore_negative_abcisse : bool;
  ignore_after_glyphWidth : bool;
  ignore_under_base_line : bool;
}
val print_raw : out_channel -> raw -> unit
val bounding_box_opt :
  bounding_box_opt -> raw list -> float * float * float * float
val bounding_box : raw list -> float * float * float * float
val bounding_box_kerning : raw list -> float * float * float * float
val bounding_box_full : raw list -> float * float * float * float
val rectangle : 'a * 'b -> 'a * 'b -> ('a array * 'b array) array
val line : 'a * 'b -> 'a * 'b -> 'a array * 'b array
val circle : float -> (float array * float array) array
type structure = {
  mutable name : string;
  mutable metadata : (metadata * string) list;
  mutable displayname : raw list;
  mutable tags : (string * string) list;
  mutable page : int;
  mutable struct_x : float;
  mutable struct_y : float;
  mutable substructures : structure array;
}
val empty_structure : structure
val print_structure : structure -> unit
val output_to_prime :
  (?structure:structure -> page array -> 'b -> 'c) ->
  ?structure:structure -> page array array -> 'b -> 'c
val output_from_prime :
  (?structure:structure -> 'a array array -> 'b -> 'c) ->
  ?structure:structure -> 'a array -> 'b -> 'c
val in_order : int -> raw -> raw
val drawing_order : raw -> int
val drawing_sort : raw list -> raw list
val sort_raw : raw list -> raw list
