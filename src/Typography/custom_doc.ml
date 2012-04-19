open Odoc_info

let _=Odoc_info.Args.charset:="UTF-8"
let my_generator = new Odoc_html.html
let _ = Odoc_args.set_doc_generator (Some (my_generator :> Odoc_args.doc_generator))
