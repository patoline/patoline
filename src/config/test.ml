open ConfigRC

module MySpec = struct
  open Data

  let spec =
    [ ("filename", of_string "default"                       )
    ; ("maxthds" , of_int 4                                  )
    ; ("testnone", of_option Int None                        )
    ; ("testsome", of_option Int (Some 42)                   )
    ; ("path0"   , of_list String []                         )
    ; ("path1"   , of_list String ["."]                      )
    ; ("path2"   , of_list String ["."; "/usr/bin"]          )
    ; ("path3"   , of_list String ["."; "/usr/bin"; "blabla"]) ]

  let name = "test"
  let path = ("/etc/testrc", ["/home/rodolphe/.testrc"; "./testrc"])
end

module Conf = Make(MySpec)


let _ =
  Config.write stdout (Conf.get_config ())
