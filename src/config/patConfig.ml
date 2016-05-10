open ConfigRC

module Spec = struct
  open Data

  include PatDefault

  let home =
    try Sys.getenv "XDG_DATA_HOME"
    with Not_found -> Sys.getenv "HOME"

  (* Default Patoline configuration. *)
  let spec =
    [ ("fonts_dir"          , of_string fonts_dir   )
    ; ("plugins_dir"        , of_string plugins_dir )
    ; ("grammars_dir"       , of_string grammars_dir)
    ; ("hyphen_dir"         , of_string hyphen_dir  )

    ; ("extra_fonts_dirs"   , of_list String extra_fonts_dir   )
    ; ("extra_plugins_dirs" , of_list String extra_plugins_dir )
    ; ("extra_grammars_dirs", of_list String extra_grammars_dir)
    ; ("extra_hyphen_dirs"  , of_list String extra_hyphen_dir  )

    ; ("drivers"    , of_list String drivers)
    ; ("formats"    , of_list String formats)
    ; ("has_patonet", of_bool has_patonet   )
    ; ("max_iter"   , of_int 4              )
    ; ("user_dir"   , of_string home        ) ]

  let name = "Patoline"
  let path = ("/etc/patolinerc", [home ^ "/.patolinerc"; "./patolinerc"])
end

module Conf = Make(Spec)

type patoconfig =
  { mutable fonts_dir    : string * string list
  ; mutable plugins_dir  : string * string list
  ; mutable grammars_dir : string * string list
  ; mutable hyphen_dir   : string * string list
  ; mutable drivers      : string list
  ; mutable formats      : string list
  ; mutable has_patonet  : bool
  ; mutable max_iter     : int
  ; mutable user_dir     : string }

let debug = false

let patoconfig : patoconfig =
  if debug then Printf.eprintf "########## patoconfig creation !\n%!";
  let cfg = Conf.get_config () in
  let open Data in
  let get_s k = to_string (Config.get k cfg) in
  let get_b k = to_bool (Config.get k cfg) in
  let get_i k = to_int (Config.get k cfg) in
  let get_l k = to_list String (Config.get k cfg) in
  { fonts_dir    = (get_s "fonts_dir"   , get_l "extra_fonts_dirs"   ) 
  ; plugins_dir  = (get_s "plugins_dir" , get_l "extra_plugins_dirs" )
  ; grammars_dir = (get_s "grammars_dir", get_l "extra_grammars_dirs")
  ; hyphen_dir   = (get_s "hyphen_dir"  , get_l "extra_hyphen_dirs"  )
  ; drivers      = get_l "drivers"
  ; formats      = get_l "formats"
  ; has_patonet  = get_b "has_patonet"
  ; max_iter     = get_i "max_iter"
  ; user_dir     = get_s "user_dir" }

exception File_not_found of string

let findPath (path, paths) fname =
  if debug then Printf.eprintf "########## Looking for %S in:\n%!" fname;
  if debug then List.iter (Printf.eprintf " - %S\n%!") (path :: paths);
  let rec find = function
    | []    -> raise (File_not_found fname)
    | p::ps -> let path = Filename.concat p fname in
               if Sys.file_exists path then path else find ps
  in find (List.rev (path :: paths))

let findFont    fn = findPath patoconfig.fonts_dir fn
let findPlugin  fn = findPath patoconfig.plugins_dir fn
let findGrammar fn = findPath patoconfig.grammars_dir fn
let findHyphen  fn = findPath patoconfig.hyphen_dir fn

let add_fonts_dir d =
  if debug then Printf.eprintf "########## Adding fonts dir %S\n%!" d;
  let (p, ps) = patoconfig.fonts_dir in
  patoconfig.fonts_dir <- (p, ps @ [d])

let add_plugins_dir d =
  if debug then Printf.eprintf "########## Adding plugins dir %S\n%!" d;
  let (p, ps) = patoconfig.plugins_dir in
  patoconfig.plugins_dir <- (p, ps @ [d])

let add_grammars_dir d =
  if debug then Printf.eprintf "########## Adding grammars dir %S\n%!" d;
  let (p, ps) = patoconfig.grammars_dir in
  patoconfig.grammars_dir <- (p, ps @ [d])

let add_hyphen_dir d =
  if debug then Printf.eprintf "########## Adding hyphen dir %S\n%!" d;
  let (p, ps) = patoconfig.hyphen_dir in
  patoconfig.hyphen_dir <- (p, ps @ [d])

let print_config = Conf.print_config
