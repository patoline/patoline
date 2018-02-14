open Util
let _=
  Build.macros:=
    StrMap.add "bibliography" (fun x->
      let f=Filename.temp_file "biblio" ".bib" in
      let f_out="biblio.bibi" in
      let fo=open_out f in
      output_string fo x;
      close_out fo;
      let _=Sys.command (Printf.sprintf "bibi -o %s %s" f_out f) in
      "\nopen Bibi\nlet _=Bibi.bibfile \"biblio.bibi\"\n"
    ) !Build.macros
