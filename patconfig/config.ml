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
    ; ("grammars_dir"       , of_string grammars_dir)
    ; ("hyphen_dir"         , of_string hyphen_dir  )

    ; ("extra_fonts_dirs"   , of_list String extra_fonts_dir   )
    ; ("extra_grammars_dirs", of_list String extra_grammars_dir)
    ; ("extra_hyphen_dirs"  , of_list String extra_hyphen_dir  )

    ; ("drivers"    , of_list String drivers)
    ; ("formats"    , of_list String formats)
    ; ("max_iter"   , of_int 4              )
    ; ("user_dir"   , of_string home        ) ]

  let name = "Patoline"
  let path = ("/etc/patolinerc", [home ^ "/.patolinerc"; "./patolinerc"])
end

module Conf = Make(Spec)

type patoconfig =
  { mutable fonts_dir      : string * string list
  ; mutable grammars_dir   : string * string list
  ; mutable hyphen_dir     : string * string list
  ; mutable drivers        : string list
  ; mutable formats        : string list
  ; mutable max_iter       : int
  ; mutable user_dir       : string }

let debug = false

let patoconfig : patoconfig =
  if debug then Printf.eprintf "########## patoconfig creation !\n%!";
  let cfg = Conf.get_config () in
  let open Data in
  let get_s k = to_string (Config.get k cfg) in
  let get_i k = to_int (Config.get k cfg) in
  let get_l k = to_list String (Config.get k cfg) in
  { fonts_dir      = (get_s "fonts_dir"   , get_l "extra_fonts_dirs"   )
  ; grammars_dir   = (get_s "grammars_dir", get_l "extra_grammars_dirs")
  ; hyphen_dir     = (get_s "hyphen_dir"  , get_l "extra_hyphen_dirs"  )
  ; drivers        = get_l "drivers"
  ; formats        = get_l "formats"
  ; max_iter       = get_i "max_iter"
  ; user_dir       = get_s "user_dir" }

let findPath ?(subdir=false) (path, paths) fname =
  if debug then Printf.eprintf "########## Looking for %S in:\n%!" fname;
  if debug then List.iter (Printf.eprintf " - %S\n%!") (path :: paths);
  let rec search base =
    let path = Filename.concat base fname in
    if Sys.file_exists path then path else
      let dirs = Sys.readdir base in
      let rec fn i =
        if i < 0 then raise Not_found;
        let dname = Filename.concat base dirs.(i) in
        if Sys.is_directory dname then search dname
        else fn (i-1)
      in
      if subdir then fn (Array.length dirs - 1)
      else raise Not_found
  in
  let rec find = function
    | []    -> raise Not_found
    | p::ps -> try search p with Not_found -> find ps
  in find (List.rev (path :: paths))

let findFont    fn = findPath ~subdir:true patoconfig.fonts_dir fn
let findGrammar fn = findPath patoconfig.grammars_dir fn
let findHyphen  fn = findPath patoconfig.hyphen_dir fn

let add_fonts_dir d =
  if debug then Printf.eprintf "########## Adding fonts dir %S\n%!" d;
  let (p, ps) = patoconfig.fonts_dir in
  patoconfig.fonts_dir <- (p, ps @ [d])

let add_grammars_dir d =
  if debug then Printf.eprintf "########## Adding grammars dir %S\n%!" d;
  let (p, ps) = patoconfig.grammars_dir in
  patoconfig.grammars_dir <- (p, ps @ [d])

let add_hyphen_dir d =
  if debug then Printf.eprintf "########## Adding hyphen dir %S\n%!" d;
  let (p, ps) = patoconfig.hyphen_dir in
  patoconfig.hyphen_dir <- (p, ps @ [d])

let print_config = Conf.print_config
