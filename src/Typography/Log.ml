open Util

type error_log=
    Overfull_line of line
  | Widow of line
  | Orphan of line
  | Normal

let logf out=function
    Normal -> ()
  | Widow line->Printf.fprintf out "Veuve ";print_linef out line
  | Orphan line->Printf.fprintf out "Orphelin ";print_linef out line
  | Overfull_line line->Printf.fprintf out "Ligne trop pleine ";print_linef out line
