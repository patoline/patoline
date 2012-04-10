let prefix=ref "/usr/local"
let bin_prefix=ref ""
let fonts_prefix=ref ""
let ocaml_prefix=ref ""
let fonts_dir=ref []
let grammars_prefix=ref []
let hyphen_prefix=ref []

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
    ("--bin-prefix", Set_string bin_prefix, "prefix for the binaries");
    ("--ocaml-libs", Set_string ocaml_prefix, "prefix for the caml libraries (use ocamlc -where)");
    ("--fonts-prefix", Set_string fonts_prefix, "prefix for the fonts");
    ("--fonts-dir", String (fun pref->fonts_dir:=pref:: !fonts_dir), "prefix for the fonts");
    ("--grammars-prefix", String (fun pref->grammars_prefix:=pref:: !grammars_prefix), "prefix for the grammars");
    ("--hyphenation-prefix", String (fun pref->hyphen_prefix:=pref:: !hyphen_prefix), "prefix for the hyphenation dictionaries")
  ] ignore "Usage :";
  if !bin_prefix="" then bin_prefix:=Filename.concat !prefix "bin";
  if !fonts_prefix="" then fonts_prefix:=Filename.concat !prefix "share/texprime/fonts";
  if !ocaml_prefix="" then ocaml_prefix:=Filename.concat !prefix "lib/ocaml";
  fonts_dir:=List.rev (!fonts_prefix :: !fonts_dir);
  grammars_prefix:=List.rev ((Filename.concat !prefix "share/texprime/grammars"):: !grammars_prefix);
  hyphen_prefix:=List.rev ((Filename.concat !prefix "share/texprime/hyphenation"):: !hyphen_prefix);
  let out=open_out "Makefile" in
  let config=open_out "src/Config.ml" in

  let fonts_src_dir="Otf" in
  let grammars_dir="src" in
  let hyphen_dir="Hyphenation" in
  let fontsdirs = List.filter (fun x->Sys.is_directory (Filename.concat fonts_src_dir x)) (Array.to_list (Sys.readdir fonts_src_dir)) in

    Printf.fprintf out "all:\n\tmake -C src all\n";

    Printf.fprintf out "install:\n";
    List.iter (fun dir ->
                 Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s\n" (escape (Filename.concat !fonts_prefix dir));
                 List.iter (fun f->
                              if Filename.check_suffix f ".otf" then
                                Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)/%s\n"
                                  (escape (Filename.concat (Filename.concat fonts_src_dir dir) f))
                                  (escape (Filename.concat (Filename.concat !fonts_prefix dir) f))
                           ) (Array.to_list (Sys.readdir (Filename.concat fonts_src_dir dir)))
              ) fontsdirs;
    (* Grammars *)
    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s\n" (escape (List.hd !grammars_prefix));
    List.iter (fun x->
                 if Filename.check_suffix x ".tgo" || Filename.check_suffix x ".tgx" then
                   Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)/%s\n" (escape (Filename.concat grammars_dir x)) (escape (List.hd !grammars_prefix))
              ) ("texprimeDefault.tgo"::"texprimeDefault.tgx"::Array.to_list (Sys.readdir grammars_dir));

    (* Hyphenation *)
    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s\n" (escape (List.hd !hyphen_prefix));
    List.iter (fun x->
                 if Filename.check_suffix x ".hdict" then
                   Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)/%s\n" (escape (Filename.concat hyphen_dir x)) (escape (List.hd !hyphen_prefix))
              ) (Array.to_list (Sys.readdir hyphen_dir));
    (* binaries *)
    Printf.fprintf out "\tinstall -m 755 src/texprime $(DESTDIR)/%s\n" (escape !bin_prefix);

    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s/site-lib/texprime\n" (escape !ocaml_prefix);
    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)/%s/site-lib/stublibs\n" (escape !ocaml_prefix);
    Printf.fprintf out "\tinstall -m 644 src/Typography.cma src/Typography.cmxa $(DESTDIR)/%s\n" (escape !ocaml_prefix);
    Printf.fprintf out "\tinstall -m 644 src/META src/Typography.cma src/Typography.cmxa src/Typography.a $(DESTDIR)/%s/site-lib/texprime\n" (escape !ocaml_prefix);
    Printf.fprintf out "\tinstall -m 644 src/Typography.cmi $(DESTDIR)/%s/site-lib/texprime/Typography.cmi\n" (escape !ocaml_prefix);

    Printf.fprintf config "let fontsdir=ref [%s]\nlet bindir=ref [\"%s\"]\nlet grammarsdir=ref [%s]\nlet hyphendir=ref [%s]\n"
      (String.concat ";" (List.map (fun s->"\""^s^"\"") !fonts_dir))
      !bin_prefix
      (String.concat ";" (List.map (fun s->"\""^s^"\"") !grammars_prefix))
      (String.concat ";" (List.map (fun s->"\""^s^"\"") !hyphen_prefix));
    Printf.fprintf out "clean:\n\tmake -C src clean\n";
    close_out out;
    close_out config;
