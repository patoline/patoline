open ConfigRC

let home =
  try Sys.getenv "XDG_DATA_HOME"
  with Not_found -> Sys.getenv "HOME"

include PatDefault

module Spec = struct
  open Data

  (* Default Patoline configuration. *)
  let spec =
    [ ("fonts_dir"          , of_string fonts_dir   )
    ; ("plugins_dir"        , of_string plugins_dir )
    ; ("grammars_dir"       , of_string grammars_dir)
    ; ("hyphen_dir"         , of_string grammars_dir)

    ; ("extra_fonts_dirs"   , of_list String extra_fonts_dir   )
    ; ("extra_plugins_dirs" , of_list String extra_plugins_dir )
    ; ("extra_grammars_dirs", of_list String extra_grammars_dir)
    ; ("extra_hyphen_dirs"  , of_list String extra_hyphen_dir  ) ]

  let name = "Patoline"
  let path = ("/etc/patolinerc", [home ^ "/.patolinerc"; "./patolinerc"])
end

module Conf = Make(Spec)

type patoconfig =
  { fonts_dir    : string * string list
  ; plugins_dir  : string * string list
  ; grammars_dir : string * string list
  ; hyphen_dir   : string * string list }

let get_patoconfig : unit -> patoconfig = fun () ->
  let cfg = Conf.get_config () in
  let open Data in
  let get_s k = to_string (Config.get k cfg) in
  let get_l k = to_list String (Config.get k cfg) in
  { fonts_dir    = (get_s "fonts_dir"   , get_l "extra_fonts_dirs"   ) 
  ; plugins_dir  = (get_s "plugins_dir" , get_l "extra_plugins_dirs" )
  ; grammars_dir = (get_s "grammars_dir", get_l "extra_grammars_dirs")
  ; hyphen_dir   = (get_s "hyphen_dir"  , get_l "extra_hyphen_dirs"  ) }
