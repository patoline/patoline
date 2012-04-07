let prefix=ref "/usr/local"
let bin_prefix=ref ""
let fonts_prefix=ref ""
let grammars_prefix=ref ""
let hyphen_prefix=ref ""

open Arg
let rec escape s=
  try
    let i=String.index s ' ' in
      String.sub s 0 i ^ "\\ " ^ (escape (String.sub s (i+1) (String.length s-i-1)))
  with
      Not_found -> s


let _=
  parse [
    ("--prefix", Set_string prefix, "prefix");
    ("--bin-prefix", Set_string fonts_prefix, "prefix for the binaries");
    ("--fonts-prefix", Set_string fonts_prefix, "prefix for the fonts");
    ("--grammars-prefix", Set_string grammars_prefix, "prefix for the grammars");
    ("--hyphenation-prefix", Set_string hyphen_prefix, "prefix for the hyphenation dictionaries")
  ] ignore "Usage :";
  if !bin_prefix="" then bin_prefix:=Filename.concat !prefix "bin";
  if !fonts_prefix="" then fonts_prefix:=Filename.concat !prefix "share/texprime/fonts";
  if !grammars_prefix="" then grammars_prefix:=Filename.concat !prefix "share/texprime/grammars";
  if !hyphen_prefix="" then hyphen_prefix:=Filename.concat !prefix "share/texprime/hyphenation";
  let out=open_out "Makefile" in
  let config=open_out "src/Config.ml" in

  let fonts_dir="Otf" in
  let grammars_dir="src" in
  let hyphen_dir="Hyphenation" in
  let fontsdirs = List.filter (fun x->Sys.is_directory (Filename.concat fonts_dir x)) (Array.to_list (Sys.readdir fonts_dir)) in

    Printf.fprintf out "all:\n\tmake -C src all\n";

    Printf.fprintf out "install:\n";
    List.iter (fun dir ->
                 Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s\n" (escape (Filename.concat !fonts_prefix dir));
                 List.iter (fun f->
                              if Filename.check_suffix f ".otf" then
                                Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)/%s\n"
                                  (escape (Filename.concat (Filename.concat fonts_dir dir) f))
                                  (escape (Filename.concat (Filename.concat !fonts_prefix dir) f))
                           ) (Array.to_list (Sys.readdir (Filename.concat fonts_dir dir)))
              ) fontsdirs;
    (* Grammars *)
    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s\n" (escape !grammars_prefix);
    List.iter (fun x->
                 if Filename.check_suffix x ".tgo" || Filename.check_suffix x ".tgx" then
                   Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)/%s\n" (escape (Filename.concat grammars_dir x)) (escape !grammars_prefix)
              ) (Array.to_list (Sys.readdir grammars_dir));

    (* Hyphenation *)
    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s\n" (escape !hyphen_prefix);
    List.iter (fun x->
                 if Filename.check_suffix x ".hdict" then
                   Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)/%s\n" (escape (Filename.concat hyphen_dir x)) (escape !hyphen_prefix)
              ) (Array.to_list (Sys.readdir hyphen_dir));
    (* binaries *)
    Printf.fprintf out "\tinstall -m 755 src/texprime $(DESTDIR)/%s\n" (escape !bin_prefix);

    Printf.fprintf out "\tinstall -m 755 -d $(shell ocamlfind ocamlc -where)/site-lib/texprime\n";
    Printf.fprintf out "\tinstall -m 644 src/texprime.cma src/texprime.cmxa $(shell ocamlfind ocamlc -where)\n";
    Printf.fprintf out "\tinstall -m 644 src/META src/texprime.cma src/texprime.cmxa $(shell ocamlfind ocamlc -where)/site-lib/texprime\n";

    Printf.fprintf config "let fontsdir=ref [\"%s\"]\nlet bindir=ref [\"%s\"]\nlet grammarsdir=ref [\"%s\"]\nlet hyphendir=ref [\"%s\"]\n"
      !fonts_prefix !bin_prefix !grammars_prefix !hyphen_prefix;
    Printf.fprintf out "clean:\n\tmake -C src clean\n";
    close_out out;
    close_out config;
