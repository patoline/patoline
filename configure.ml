let prefix=ref "/usr/local/"
let bin_dir=ref ""
let fonts_dir=ref ""
let grammars_dir=ref ""
let hyphen_dir=ref ""
let ocaml_lib_dir=ref ""
let ocamlfind_dir=ref ""
let fonts_dirs=ref []
let grammars_dirs=ref []
let hyphen_dirs=ref []
let opt_only=ref true
let camlzip=ref ""
let camlimages=ref ""
let lang=ref "FR"

let avail_lang=
  let f=open_in "src/Typography/Language.ml" in
  let buf=String.create (in_channel_length f) in
    really_input f buf 0 (in_channel_length f);
    close_in f;
    let rec make_str i res=
      if i>String.length buf-7 then res else
        make_str (i+1)
          (if String.sub buf i 5="LANG_" then
             String.sub buf (i+5) 2::res
           else res)
    in
      make_str 0 []

open Arg
let rec escape s=
  try
    let i=String.index s ' ' in
      String.sub s 0 i ^ "\\ " ^ (escape (String.sub s (i+1) (String.length s-i-1)))
  with
      Not_found -> s


let _=
  parse [
    ("--prefix", Set_string prefix, "  prefix (/usr/local/ by default)");
    ("--bin-prefix", Set_string bin_dir, "  directory for the binaries ($PREFIX/bin/ by default)");
    ("--ocaml-libs", Set_string ocaml_lib_dir, "  directory for the caml libraries ($PREFIX/lib/ocaml/ by default; `ocamlc -where` is another sensible choice)");
    ("--ocamlfind-dir", Set_string ocamlfind_dir, "  directory for the caml libraries ($PREFIX/lib/ocaml/ by default; `ocamlc -where` is another sensible choice)");
    ("--fonts-dir", Set_string fonts_dir, "  directory for the fonts ($PREFIX/share/patoline/fonts/ by default)");
    ("--grammars-dir", Set_string grammars_dir, "  directory for the grammars ($PREFIX/lib/patoline/grammars/ by default)");
    ("--hyphen-dir", Set_string hyphen_dir, "  directory for the hyphenation dictionnaries ($PREFIX/share/patoline/hyphen/ by default)");
    ("--extra-fonts-dir", String (fun pref->fonts_dirs:=pref:: !fonts_dirs), "  additional directories patoline should scan for fonts");
    ("--extra-grammars-dir", String (fun pref->grammars_dirs:=pref:: !grammars_dirs), "  additional directories patoline should scan for grammars");
    ("--extra-hyphen-dir", String (fun pref->hyphen_dirs:=pref:: !hyphen_dirs), "  additional directories patoline should scan for hyphenation dictionaries");
    ("--byte", Unit (fun ()->opt_only:=false), "  compile bytecode version (only native code is compiled by default)");
    ("--lang", Set_string lang, Printf.sprintf "  language of the error messages (english by default), available : %s"
       (String.concat ", " (List.rev avail_lang)))
  ] ignore "Usage:";
  if !bin_dir="" then bin_dir:=Filename.concat !prefix "bin/";
  if !ocaml_lib_dir="" then ocaml_lib_dir:=Filename.concat !prefix "lib/ocaml";
  if !fonts_dir="" then fonts_dir:=Filename.concat !prefix "share/patoline/fonts";
  if !grammars_dir="" then grammars_dir:=Filename.concat !prefix "lib/patoline/grammars";
  if !hyphen_dir="" then hyphen_dir:=Filename.concat !prefix "share/patoline/hyphen";

  fonts_dirs:= !fonts_dir ::(!fonts_dirs);
  grammars_dirs:= !grammars_dir ::(!grammars_dirs);
  hyphen_dirs:= !hyphen_dir ::(!hyphen_dirs);

  if Sys.command "ocamlfind query zip" = 0
  then camlzip := "zip"
  else if Sys.command "ocamlfind query camlzip" = 0
  then camlzip := "camlzip";

  if Sys.command "ocamlfind query camlimages" = 0
  then camlimages := "camlimages.all_formats";

  let out=open_out "Makefile" in
  let config=open_out "src/Typography/Config.ml" in
  let config'=open_out "src/Patoline/PatolineConfig.ml" in

  let fonts_src_dir="Fonts" in
  let grammars_src_dir="src" in
  let hyphen_src_dir="Hyphenation" in

    Printf.fprintf out "all:\n\tmake -C src %s\n" (if !opt_only then "native" else "all");
    Printf.fprintf out "binary:all\nbuild:all\n";
    Printf.fprintf out "doc:\n\tmake -C src doc\n";
    Printf.fprintf out "install:\n";
    Printf.fprintf out "\t#fonts\n";
    let rec read_fonts dir =
      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s\n" (escape (Filename.concat !fonts_dir dir));
      List.iter (fun f->
        let f = Filename.concat dir f in
        if Sys.is_directory f
        then read_fonts f
        else if Filename.check_suffix f ".otf" then
          Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)%s\n"
            (escape (Filename.concat fonts_src_dir f))
            (escape (Filename.concat !fonts_dir f))
            ) (Array.to_list (Sys.readdir dir))
    in
    let cdir = Sys.getcwd () in
      Sys.chdir fonts_src_dir;
      read_fonts "./";
      Sys.chdir cdir;

    (* Grammars *)
    Printf.fprintf out "\t#grammars\n";
    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s\n" (escape !grammars_dir);
    Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)%s\n" 
      (escape (Filename.concat grammars_src_dir "DefaultGrammar.tgx")) 
      (escape (List.hd !grammars_dirs));

    (* Hyphenation *)
    Printf.fprintf out "\t#hyphenation\n";
    Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s\n" (escape !hyphen_dir);
    List.iter (fun x->
                 if Filename.check_suffix x ".hdict" then
                   Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)%s\n" (escape (Filename.concat hyphen_src_dir x)) (escape !hyphen_dir)
              ) (Array.to_list (Sys.readdir hyphen_src_dir));

    let tags=open_out "src/Typography/_tags" in
      Printf.fprintf tags
        "<**/*.ml> or <**/*.mli>: package(camomile)%s%s,pp(cpp -w %s%s%s)
<**/*.{cmo,cmx}> and not <Typography.*>:for-pack(Typography)
<Fonts> or <Output> or <Output/Drivers>:include\n"
        (if !camlzip <> "" then ",package("^(!camlzip)^")" else "")
        (if !camlimages <> "" then ",package("^(!camlimages)^")" else "")
        (if !camlzip <> "" then "-DCAMLZIP " else "")
        (if !camlimages <> "" then "-DCAMLIMAGES " else "")
        (if String.uppercase !lang <> "EN" then ("-DLANG_"^String.uppercase !lang) else "");
      close_out tags;

    let tags=open_out "src/_tags" in
      Printf.fprintf tags
        "<**/*>: pp(cpp -w),package(camomile)%s%s
<Format/*.{ml,mli}>: use_Typography
<proof/proof.{byte,native}>: package(camomile)%s%s
<Patoline/*>:pp(cpp -w %s),package(dyp),use_str,use_dynlink
\"Typography\":include\n"
        (if !camlzip <> "" then ",package("^(!camlzip)^")" else "")
        (if !camlimages <> "" then ",package("^(!camlimages)^")" else "")
        (if !camlzip <> "" then ",package("^(!camlzip)^")" else "")
        (if !camlimages <> "" then ",package("^(!camlimages)^")" else "")
        (if String.uppercase !lang <> "EN" then ("-DLANG_"^String.uppercase !lang) else "");

      close_out tags;

    (* binaries *)
    Printf.fprintf out "\t#binaries\n";
    Printf.fprintf out "\tinstall -d $(DESTDIR)%s\n" (escape !bin_dir);
    Printf.fprintf out "\tinstall -m 755 src/_build/Patoline/Main.native $(DESTDIR)%s/patoline\n" (escape !bin_dir);

    let sources=
      "src/_build/Typography/Typography.cmxa src/_build/Typography/Typography.a src/_build/Typography/Typography.cmi "^
        "src/_build/Format/*Format*.cmxa src/_build/Format/*Format*.a src/_build/Format/*Format*.cmi "^
        "src/_build/DefaultGrammar.cmx src/_build/DefaultGrammar.cmi"^
        (if not !opt_only then
           " src/_build/Typography/Typography.cma src/_build/Format/*Format*.cma"
         else "")
    in
      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/Typography\n" (escape !ocaml_lib_dir);
      Printf.fprintf out "\tinstall -m 644 %s $(DESTDIR)%s/Typography\n" sources (escape !ocaml_lib_dir);

      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/Typography\n" (escape !ocaml_lib_dir);


      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/Typography\n" (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);
      Printf.fprintf out "\tinstall -m 644 src/Typography/META %s $(DESTDIR)%s/Typography\n" sources (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);

      (* proof *)
      Printf.fprintf out "\tinstall -m 755 src/_build/proof/proof.native $(DESTDIR)%s/proof\n" (escape !bin_dir);

      (* Ecriture de la configuration *)
      let conf=if Sys.os_type= "Win32" then (
        let path_var="PATOLINE_PATH" in
          Printf.sprintf "(** Configuration locale (chemins de recherche des fichiers) *)\nlet path=try Sys.getenv %S with _->\"\"\n(** Chemin des polices de caractères *)\nlet fontsdir=ref [%s]\n(** Chemin de l'éxécutable Patoline *)\nlet bindir=%S\n(** Chemin des grammaires *)\nlet grammarsdir=ref [%s]\n(** Chemin des dictionnaires de césures *)\nlet hyphendir=ref [%s]\n"
            path_var
            (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !fonts_dirs)))
            !bin_dir
            (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !grammars_dirs)))
            (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !hyphen_dirs)))
      ) else (
        Printf.sprintf "(** Configuration locale (chemins de recherche des fichiers) *)\n(** Chemin des polices de caractères *)\nlet fontsdir=ref [%s]\n(** Chemin de l'éxécutable Patoline *)\nlet bindir=%S\n(** Chemin des grammaires *)\nlet grammarsdir=ref [%s]\n(** Chemin des dictionnaires de césures *)\nlet hyphendir=ref [%s]\n"
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !fonts_dirs)))
          !bin_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !grammars_dirs)))
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !hyphen_dirs)))
      )
      in
        Printf.fprintf config "%s" conf;
        Printf.fprintf config' "%s" conf;
        Printf.fprintf out "clean:\n\tmake -C src clean\n";
        close_out out;
        close_out config;
        close_out config';

        let meta=open_out "src/Typography/META" in
          Printf.fprintf meta
            "name=\"Typography\"\nversion=\"1.0\"\ndescription=\"Typography library\"\nrequires=\"camomile%s%s\"\n" (if !camlzip="" then "" else (","^(!camlzip)))
            (if !camlimages="" then "" else (","^(!camlimages)));
          Printf.fprintf meta "archive(native)=\"Typography.cmxa, DefaultFormat.cmxa\"\n";
          Printf.fprintf meta "archive(byte)=\"Typography.cma, DefaultFormat.cma\"\n";
          close_out meta
