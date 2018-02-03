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
        try
          Dynlink.loadfile (Filename.concat dir name);
          Printf.fprintf stderr "Driver %s loaded.\n%!" name
        with 
        | Dynlink.Error (Dynlink.File_not_found _) -> fn l
        | Dynlink.Error s ->
            Printf.fprintf stderr "Dynlink error: %s\n"
              (Dynlink.error_message s);
            exit 1
  in fn driverdir


