let prefix=ref "/usr/local/bin"
let fonts_prefix=ref "/usr/local/share/texprime/fonts"
let grammars_prefix=ref "/usr/local/share/texprime/grammars"
let hyphen_prefix=ref "/usr/local/share/texprime/hyphen"

open Arg
let rec escape s=
  try
    let i=String.index s ' ' in
      String.sub s 0 i ^ "\\ " ^ (escape (String.sub s (i+1) (String.length s-i-1)))
  with
      Not_found -> s


let _=
  parse [
    ("--prefix", Set_string prefix, "prefix for binary installation");
    ("--fonts-prefix", Set_string fonts_prefix, "prefix for the fonts");
    ("--grammars-prefix", Set_string grammars_prefix, "prefix for the grammars");
    ("--hyphenation-prefix", Set_string hyphen_prefix, "prefix for the hyphenation dictionaries")
  ] ignore "Usage :";
  let out=open_out "Makefile" in
  let config=open_out "src/Config.ml" in

  let fonts_dir="Otf" in
  let grammars_dir="src" in
  let hyphen_dir="Hyphenation" in
  let fontsdirs = List.filter (fun x->Sys.is_directory (Filename.concat fonts_dir x)) (Array.to_list (Sys.readdir fonts_dir)) in

    Printf.fprintf out "all:\n\tmake -C src all\n";

    Printf.fprintf out "install:\n";
    List.iter (fun dir ->
                 Printf.fprintf out "\tinstall -m 755 -d %s\n" (escape (Filename.concat !fonts_prefix dir));
                 List.iter (fun f->
                              if Filename.check_suffix f ".otf" then
                                Printf.fprintf out "\tinstall -m 644 %s %s\n"
                                  (escape (Filename.concat (Filename.concat fonts_dir dir) f))
                                  (escape (Filename.concat (Filename.concat !fonts_prefix dir) f))
                           ) (Array.to_list (Sys.readdir (Filename.concat fonts_dir dir)))
              ) fontsdirs;
    (* Grammars *)
    Printf.fprintf out "\tinstall -m 755 -d %s\n" (escape !grammars_prefix);
    List.iter (fun x->
                 if Filename.check_suffix x ".tgo" || Filename.check_suffix x ".tgx" then
                   Printf.fprintf out "\tinstall -m 644 %s %s\n" (escape (Filename.concat grammars_dir x)) (escape !grammars_prefix)
              ) (Array.to_list (Sys.readdir grammars_dir));

    (* Hyphenation *)
    Printf.fprintf out "\tinstall -m 755 -d %s\n" (escape !hyphen_prefix);
    List.iter (fun x->
                 if Filename.check_suffix x ".hdict" then
                   Printf.fprintf out "\tinstall -m 644 %s %s\n" (escape (Filename.concat hyphen_dir x)) (escape !hyphen_prefix)
              ) (Array.to_list (Sys.readdir hyphen_dir));
    (* binaries *)
    Printf.fprintf out "\tinstall -m 755 src/texprime %s\n" (escape !prefix);

    Printf.fprintf out "\tocamlfind remove texprime\n\tocamlfind install texprime src/META src/texprime.cma src/texprime.cmxa\n";

    Printf.fprintf config "let fontsdir=ref [\"%s\"]\nlet bindir=ref [\"%s\"]\nlet grammarsdir=ref [\"%s\"]\nlet hyphendir=ref [\"%s\"]\n"
      !fonts_prefix !prefix !grammars_prefix !hyphen_prefix;
    Printf.fprintf out "clean:\n\tmake -C src clean\n";
    close_out out;
    close_out config;
