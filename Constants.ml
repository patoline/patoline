let destdir="/usr/local/texprime"

let makePath path=
  List.map (fun x->Filename.concat x path)
    ("."::(try Str.split (Str.regexp ";") (Sys.getenv "TS_PATH") with Not_found -> [])@[destdir])

let rec findPath f=function
    []->raise Not_found
  | h::s when Sys.file_exists (Filename.concat h f)->(Filename.concat h f)
  | _::s -> findPath f s

let findFont f=try
  findPath f (makePath "Otf")
with
    Not_found ->(
      Printf.printf "%s : Not found. Path :\n" f;
      List.iter (Printf.printf "\t%s\n");
      exit 1
    )
let findGramar f=findPath f (makePath "Grammars")

let pt_of_mm x=(72.*.x)/.25.4
let mm_of_pt x=(25.4*.x)/.72.

let a4=(210.,297.)
let phi=(1.+.(sqrt 5.))/.2.
