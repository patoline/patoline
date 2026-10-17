open Driver

let drivers : (string, (module OutputDriver)) Hashtbl.t = Hashtbl.create 37

let dependencies = ["Image",["DriverGL"];"Patonet",["SVG"]]

let rec load_driver driverdir name =
  Printf.fprintf stderr "Loading driver %S.\n%!" name;
  let _ =
    try List.iter (load_driver driverdir) (List.assoc name dependencies)
    with Not_found -> ()
  in
  let name = name^".cmxs" in
  let rec fn = function
    | []     -> failwith (Printf.sprintf "Driver %S not found." name)
    | dir::l ->
        let path = Filename.concat dir name in
        if not (Sys.file_exists path) then fn l
        else
          try
            Dynlink.loadfile path;
            Printf.fprintf stderr "Driver %s loaded.\n%!" name
          with
          | Dynlink.Error e ->
            Printf.fprintf stderr "Dynlink error: %s\n"
              (Dynlink.error_message e);
            exit 1
  in fn driverdir

