(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)

#load "unix.cma"

let prefix=ref "/usr/local/"
let bin_dir=ref ""
let fonts_dir=ref ""
let grammars_dir=ref ""
let hyphen_dir=ref ""
let ocaml_lib_dir=ref ""
let ocamlfind_dir=ref ""
let fonts_dirs=ref []
let grammars_dirs=ref []
let plugins_dir=ref ""
let plugins_dirs=ref []
let hyphen_dirs=ref []
let lang=ref "FR"
let ban_comic_sans=ref false
let int32=ref (Sys.word_size=32)

let avail_lang=
  let f=open_in "src/Typography/TypoLanguage.ml" in
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

let is_substring s1 s0=
  let rec sub i j=
    if i>String.length s0-String.length s1 then false else
      if j>=String.length s1 then true else
        if s0.[i+j]=s1.[j] then sub i (j+1) else
          sub (i+1) 0
  in
    sub 0 0

let configure_environment=Unix.environment ()

(* Querying ocamlfind packages, and caching results.
 * Returns type is (bool * string) where the boolean indicates whether
 * ocamlfound the package, and string is its actual name (a package may have
 * different names depending on the system). *)
let ocamlfind_query =
  (* Some packages may have different names, which we try to find if the first
   * package name yields no result. *)
  let ocamlfind_aliases =
    [
      ["zip"; "camlzip"];
      ["cairo"; "ocaml-cairo"];
      ["lablgl"; "lablGL"];
      ["lablgl.glut"; "lablGL.glut"]
    ]
  and checked = Hashtbl.create 10 in
  function pack ->
    try
      (* Easy case, when package has already been looked up *)
      Hashtbl.find checked pack
    with
      Not_found ->
        let liste=
            (
              try List.find (fun l -> List.hd l = pack) ocamlfind_aliases
              with Not_found -> [pack]
            )
        in
        (match liste with
            h::_->Printf.printf "Looking for package %s..." h
          | _->());
        let res =
          List.fold_left
           (fun res pack_alias ->
             let ci,ci'=Unix.pipe () in
             let co,co'=Unix.pipe () in
             let ce,ce'=Unix.pipe () in
             let i=Unix.create_process "ocamlfind" [|"ocamlfind";"query";pack_alias|] ci' co ce in
             Unix.close co;
             Unix.close ce;
             Unix.close ci';
             let _,st=Unix.waitpid [] i in
             if fst res || st <> (Unix.WEXITED 0) then (
               res
             ) else
               (true, pack_alias)
           )
            (false, "")
            liste
        in
        Hashtbl.add checked pack res;
        if (fst res) then
          Printf.printf " found (%s)\n" (snd res)
        else
          Printf.printf " not found\n";
        res

(* Is a package ocamlfindable? *)
let ocamlfind_has pack = fst (ocamlfind_query pack)

(* Listing Patoline drivers with their corresponding dependancies.
 * A driver may depend on some package found using ocamlfind, or on some other
 * driver. *)
type driver_needs =
  | Package of string
  | Driver of driver
and driver =
  { name: string;
    needs: driver_needs list;
    suggests: driver_needs list;
    internals: driver_needs list;
  }

let ocamlnet_needs =
  [Package "netstring"; Package "netsys"; Package "unix"; Package "cryptokit"]

let patoline_driver_gl =
  { name = "DriverGL";
    needs =(Package "str")::(Package "camlimages.all_formats")::
      (Package "lablgl")::(Package "lablgl.glut")::ocamlnet_needs;
    suggests = [];
    internals = [] (* [Package "Typography.GL"] *)
  }
(*
let patoline_driver_gl2 =
  { patoline_driver_gl with
    name = "DriverGL2";
    needs = (Package "str")::(Package "camlimages.all_formats")::
      (Package "lablgl")::(Package "lablgtk2")::ocamlnet_needs;
  }
*)
let patoline_driver_image =
  { name = "Image"; needs = [Package "camlimages.all_formats"; Driver
    patoline_driver_gl]; suggests = []; internals = [] }

(* List of all Patoline drivers.
 * Add yours to this list in order to build it. *)
let r_patoline_drivers = ref
  [
    { name = "None"; needs = []; suggests = []; internals = [] };
    { name = "Pdf"; needs = []; suggests = [Package "zip"]; internals = [] };
    { name = "Bin"; needs = []; suggests = []; internals = [] };
    { name = "Html"; needs = []; suggests = []; internals = [] };
    { name = "SVG"; needs = []; suggests = []; internals = [] };
    { name = "DriverCairo"; needs = [Package "cairo"]; suggests = []; internals = [] };
    patoline_driver_gl;
(*    patoline_driver_gl2;*)
    { name = "Net"; needs = ocamlnet_needs; suggests = []; internals = [Package "Typography.SVG"] };
    patoline_driver_image;
  ]

(* Checks whether we can build a given driver.
 * This certainly won't check that a driver doesn't somehow reference itself:
 * expect infinite loops if you do not care. *)
let rec can_build_driver d =
  let check_need = function
    | Package p -> ocamlfind_has p
    | Driver d' -> can_build_driver d'
  in List.iter (fun a -> ignore (check_need a)) d.suggests;
  List.exists (fun x->x.name==d.name) !r_patoline_drivers
    && List.for_all check_need d.needs

(* Generates contents for a -package option for ocamlfind, using the argument
 * needs.
 * This function does not require all packages in "needs" to be present. If some
 * of them are missing, they simply won't appear in the returned string.
 *)
let gen_pack_line ?(query=true) needs =
  let rec aux_gen = function
  | [] -> []
  | (Package p) :: needs ->
      (if query then (snd (ocamlfind_query p)) else p) :: (aux_gen needs)
  | (Driver d) :: needs ->
      (aux_gen d.needs) @ (aux_gen d.suggests) @ (aux_gen needs)
  in (List.filter ((<>) "") (aux_gen needs))

let _=
  parse [
    ("--prefix", Set_string prefix, "  prefix (/usr/local/ by default)");
    ("--bin-prefix", Set_string bin_dir, "  directory for the binaries ($PREFIX/bin/ by default)");
    ("--ocaml-libs", Set_string ocaml_lib_dir, "  directory for the caml libraries ($PREFIX/lib/ocaml/ by default; `ocamlc -where` is another sensible choice)");
    ("--ocamlfind-dir", Set_string ocamlfind_dir, "  directory for the caml libraries ($PREFIX/lib/ocaml/ by default; `ocamlc -where` is another sensible choice)");
    ("--fonts-dir", Set_string fonts_dir, "  directory for the fonts ($PREFIX/share/patoline/fonts/ by default)");
    ("--grammars-dir", Set_string grammars_dir, "  directory for the grammars ($PREFIX/lib/patoline/grammars/ by default)");
    ("--plugins-dir", Set_string plugins_dir, "  directory for the plugins ($PREFIX/lib/patoline/plugins/ by default)");
    ("--hyphen-dir", Set_string hyphen_dir, "  directory for the hyphenation dictionnaries ($PREFIX/share/patoline/hyphen/ by default)");
    ("--extra-fonts-dir", String (fun pref->fonts_dirs:=pref:: !fonts_dirs), "  additional directories patoline should scan for fonts");
    ("--extra-grammars-dir", String (fun pref->grammars_dirs:=pref:: !grammars_dirs), "  additional directories patoline should scan for grammars");
    ("--extra-hyphen-dir", String (fun pref->hyphen_dirs:=pref:: !hyphen_dirs), "  additional directories patoline should scan for hyphenation dictionaries");
    ("--ban-comic-sans", Set ban_comic_sans, " disallows the use of a font with name '*comic*sans*'. Robust to filename changes.");
    ("--lang", Set_string lang, Printf.sprintf "  language of the error messages (english by default), available : %s"
       (String.concat ", " (List.rev avail_lang)));
    ("--without", String (fun str->
      r_patoline_drivers:=List.filter (fun x->
        x.name<>str
      ) !r_patoline_drivers
     )," remove a driver from the list")
  ] ignore "Usage:";
  if !bin_dir="" then bin_dir:=Filename.concat !prefix "bin/";
  if !ocaml_lib_dir="" then ocaml_lib_dir:=Filename.concat !prefix "lib/ocaml";
  if !fonts_dir="" then fonts_dir:=Filename.concat !prefix "share/patoline/fonts";
  if !grammars_dir="" then grammars_dir:=Filename.concat !prefix "lib/patoline/grammars";
  if !hyphen_dir="" then hyphen_dir:=Filename.concat !prefix "share/patoline/hyphen";
  if !plugins_dir="" then plugins_dir:=Filename.concat !prefix "lib/patoline/plugins";

  fonts_dirs:= !fonts_dir ::(!fonts_dirs);
  grammars_dirs:= !grammars_dir ::(!grammars_dirs);
  hyphen_dirs:= !hyphen_dir ::(!hyphen_dirs);
  plugins_dirs:= !plugins_dir ::(!plugins_dirs);
  let patoline_drivers= !r_patoline_drivers in
  let has_dypgen=
    let ci,ci'=Unix.pipe () in
    let co,co'=Unix.pipe () in
    let ce,ce'=Unix.pipe () in
    let i=Unix.create_process "dypgen" [|"dypgen"|] ci' co ce in
    Unix.close co;
    Unix.close ce;
    Unix.close ci';
    let _,st=Unix.waitpid [] i in
    if (ocamlfind_has "dyp") && st = (Unix.WEXITED 0) then (
      true
    ) else false
  in

  if not (ocamlfind_has "camomile") then (
    Printf.fprintf stderr "error: package camomile missing.\n";
    exit 1
  );
  let has_sqlite3=ocamlfind_has "sqlite3" in

  let out=open_out "Makefile" in
  let config=open_out "src/Typography/Config.ml" in
  let config'=open_out "src/Patoline/Config.ml" in

  let fonts_src_dir="Fonts" in
  let grammars_src_dir="src" in
  let hyphen_src_dir="Hyphenation" in
  let emacsdir = Filename.concat !prefix "share/emacs/site-lisp/patoline" in

    Printf.fprintf out "# DO NOT EDIT THIS FILE\n";
    Printf.fprintf out "# generated by 'ocaml configure.ml'\n\n";
    Printf.fprintf out "all:src/Typography/Config.ml\n\tmake -C src all\n\tmake emacs\n";
    Printf.fprintf out "src/Typography/Config.ml:configure.ml\n\tocaml configure.ml\n";
    Printf.fprintf out "binary:all\nbuild:all\n";
    Printf.fprintf out "doc:\n\tmake -C src doc\n\n";
    Printf.fprintf out "Patoline.pdf: Patoline.txp src/Patoline/patoline src/DefaultGrammar.tgx src/Rbuffer/rbuffer.cmxa src/Typography/Typography.cmxa src/Format/DefaultFormat.cmxa src/Drivers/Pdf.cmxa
	./src/Patoline/patoline --recompile --extra-hyph-dir ./Hyphenation --extra-fonts-dir ./Fonts --ml -I src Patoline.txp
	ocamlfind ocamlopt -package %s -linkpkg -I src -I src/Rbuffer rbuffer.cmxa -I src/Typography src/Typography/Typography.cmxa -I src/Drivers Pdf.cmxa -I src/Format src/Format/DefaultFormat.cmxa -o Patoline.tmx src/DefaultGrammar.cmx -impl Patoline.tml
	./Patoline.tmx --extra-fonts-dir Fonts
"  (String.concat "," (gen_pack_line [Package "camomile"; Package "zip"; Package "camlimages.all_formats"]));
    Printf.fprintf out "check:\n\tmake -C tests articles\n";
    Printf.fprintf out ".PHONY: emacs\nemacs:\n\tmake -C emacs\n";

    Printf.fprintf out "install:\n";
    Printf.fprintf out "\t#fonts\n";
    let rec read_fonts dir =
      Printf.fprintf out "\tinstall -p -m 755 -d $(DESTDIR)%s\n" (escape (Filename.concat !fonts_dir dir));
      List.iter (fun f->
        let f = Filename.concat dir f in
        if Sys.is_directory f
        then read_fonts f
        else if Filename.check_suffix f ".otf" || Filename.check_suffix f ".ttf" then
          Printf.fprintf out "\tinstall -p -m 644 %s $(DESTDIR)%s\n"
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
    Printf.fprintf out "\tinstall -p -m 755 -d $(DESTDIR)%s\n" (escape !grammars_dir);
    Printf.fprintf out "\tinstall -p -m 644 %s %s $(DESTDIR)%s\n" 
      (escape (Filename.concat grammars_src_dir "DefaultGrammar.txp")) 
      (escape (Filename.concat grammars_src_dir "DefaultGrammar.tgx")) 
      (escape (List.hd !grammars_dirs));

    (* Hyphenation *)
    Printf.fprintf out "\t#hyphenation\n";
    Printf.fprintf out "\tinstall -p -m 755 -d $(DESTDIR)%s\n" (escape !hyphen_dir);
    List.iter (fun x->
                 if Filename.check_suffix x ".tex" then (
                   Printf.fprintf out "\tsrc/cesure/cesure %s\n"
                     (escape (Filename.concat hyphen_src_dir x));

                   Printf.fprintf out "\tinstall -p -m 644 %s $(DESTDIR)%s\n"
                     (escape (Filename.concat hyphen_src_dir
                                (Filename.chop_extension x^".hdict")))
                     (escape !hyphen_dir)
                 )
              ) (Array.to_list (Sys.readdir hyphen_src_dir));

    let make=open_out "src/Makefile.config" in
    Printf.fprintf make "CPP='cpp -C -ffreestanding -w %s%s%s%s%s%s'\n"
      (if Sys.os_type="Win32" then "-D__WINDOWS__ " else "")
      (if Sys.word_size=32 || !int32 then "-DINT32 " else "")
      (if ocamlfind_has "zip" then "-DCAMLZIP " else "")
      (if ocamlfind_has "camlimages.all_formats" then "-DCAMLIMAGES " else "")
      (if !ban_comic_sans then "-DBAN_COMIC_SANS " else "")
      (if String.uppercase !lang <> "EN" then ("-DLANG_"^String.uppercase !lang) else "");
    (if has_dypgen then
        (Printf.fprintf make "PATOLINE=Patoline/patoline\n";
        Printf.fprintf make "PACKAGE_DYP=-package dyp\n")
     else
        (Printf.fprintf make "PATOLINE=\n";
        Printf.fprintf make "PACKAGE_DYP=\n")
    );
    Printf.fprintf make "PACK=-package %s\n"
      (String.concat "," (gen_pack_line [Package "camomile"; Package "zip";
                                         Package "camlimages.all_formats"; Package "cairo"]));


    (* Write out the list of enabled drivers *)
    let ok_drivers = List.filter can_build_driver patoline_drivers in
    Printf.fprintf make "DRIVERS=%s\n"
      (String.concat " "
        (List.map (fun d -> "Drivers/" ^ d.name ^ ".cmxa") ok_drivers)
      );

    (* Output -package option for enabled drivers *)
    List.iter
    (fun d ->
      Printf.fprintf make "PACK_DRIVER_%s=%s\n"
        d.name
        (String.concat "," (gen_pack_line (d.needs @ d.suggests)))
    )
    ok_drivers;

    (* Enable compilation of ocaml-bibi if sqlite3 is installed *)
    if has_sqlite3 then (
      Printf.fprintf make "BIBI=ocaml-bibi/bibi.cmxa ocaml-bibi/bibi.cma\n"
    );
    close_out make;

    let tags_typography=open_out "src/Typography/_tags" in
    Printf.fprintf tags_typography "<**/*.ml{i,}>:use_rbuffer,pp(cpp -C -ffreestanding -w %s%s%s%s%s%s)%s,for-pack(Typography)
<Fonts> or <Output> or <Fonts/Sfnt>: include
<Fonts/unicode_ranges.cm{i,x,o}>:unicode_ranges
<Break.ml>:rectypes
<Fonts/Sfnt/make_unicode_ranges.*>:use_str
"
      (if Sys.os_type="Win32" then "-D__WINDOWS__ " else "")
      (if Sys.word_size=32 || !int32 then "-DINT32 " else "")
      (if ocamlfind_has "zip" then "-DCAMLZIP " else "")
      (if ocamlfind_has "camlimages.all_formats" then "-DCAMLIMAGES " else "")
      (if !ban_comic_sans then "-DBAN_COMIC_SANS " else "")
      (if String.uppercase !lang <> "EN" then ("-DLANG_"^String.uppercase !lang) else "")

      (let pack=String.concat ","
         (List.map (fun x->Printf.sprintf "package(%s)" x)
            (gen_pack_line [Package "camomile"; Package "zip";
                            Package "camlimages.all_formats"; Package "cairo"]))
       in
       if pack="" then "" else ","^pack);
    close_out tags_typography;

    (* binaries *)
    Printf.fprintf out "\t#binaries\n";
    Printf.fprintf out "\tmake -C src/Rbuffer install DESTDIR=$(DESTDIR)\n";
    Printf.fprintf out "\trm -f src/Drivers/Typography.cmi\n";
    Printf.fprintf out "\tinstall -d $(DESTDIR)%s\n" (escape !bin_dir);
    Printf.fprintf out "\tinstall -p -m 755 src/Patoline/patoline $(DESTDIR)%s/patoline\n" (escape !bin_dir);
    Printf.fprintf out "\tinstall -p -m 755 src/cesure/cesure $(DESTDIR)%s/cesure\n" (escape !bin_dir);
    if can_build_driver patoline_driver_gl then
      Printf.fprintf out "\tinstall -p -m 755 src/Patoline/PatolineGL $(DESTDIR)%s/patolineGL\n" (escape !bin_dir);
(*    if can_build_driver patoline_driver_gl2 then
      Printf.fprintf out "\tinstall -p -m 755 src/Patoline/PatolineGL2 $(DESTDIR)%s/patolineGL2\n" (escape !bin_dir);*)

    let sources=
      "src/Typography/_build/Typography.cmxa src/Typography/_build/Typography.a src/Typography/_build/Typography.cmi "^
      "src/Typography/_build/ParseMainArgs.cmx src/Typography/_build/ParseMainArgs.o src/Typography/_build/ParseMainArgs.cmi "^
        "src/Format/*Format*.cmxa src/Format/*Format*.a src/Format/*Format*.cmi "^
        "src/Drivers/*.cmxa src/Drivers/*.a src/Drivers/*.cmi "^
        "src/Patoline/Build.cmi src/Patoline/Util.cmi "^
        "src/Pdf/pdf_parser.cmxa src/Pdf/pdf_parser.a  "^
        "src/Pdf/pdf_parser.cmi "
        (* "src/Pdf/pdf_parser.cmxa src/Pdf/pdf_parser.a  "^ *)
        (* "src/Pdf/pdf_parser.cmi src/Pdf/pdf_parser.p.cmxa" *)
    in
      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/Typography\n" (escape !ocaml_lib_dir);
      Printf.fprintf out "\tinstall -p -m 644 %s $(DESTDIR)%s/Typography\n" sources (escape !ocaml_lib_dir);

      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/Typography\n" (escape !ocaml_lib_dir);


      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/Typography\n" (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);
      Printf.fprintf out "\tinstall -p -m 644 src/Typography/META %s $(DESTDIR)%s/Typography\n" sources (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);

      (* ocaml-bibi *)
      if has_sqlite3 then (
        let bibi_sources="src/ocaml-bibi/bibi.cmxa src/ocaml-bibi/bibi.cmi src/ocaml-bibi/bibi.a"
        in
        Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/bibi\n" (escape !ocaml_lib_dir);
        Printf.fprintf out "\tinstall -p -m 644 %s $(DESTDIR)%s/bibi\n" bibi_sources (escape !ocaml_lib_dir);
        Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/bibi\n" (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);
        Printf.fprintf out "\tinstall -p -m 644 src/ocaml-bibi/META %s $(DESTDIR)%s/bibi\n" bibi_sources (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir)
      );

      (* patoplot *)
      let plot_sources="src/plot/plot.cmxa src/plot/plot.cmi src/plot/plot.a"
      in
      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/patoplot\n" (escape !ocaml_lib_dir);
      Printf.fprintf out "\tinstall -p -m 644 %s $(DESTDIR)%s/patoplot\n" plot_sources (escape !ocaml_lib_dir);
      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/patoplot\n" (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);
      Printf.fprintf out "\tinstall -p -m 644 src/plot/META %s $(DESTDIR)%s/patoplot\n" plot_sources (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);

      (* proof *)
      Printf.fprintf out "\tinstall -p -m 755 src/proof/proof $(DESTDIR)%s/proof\n" (escape !bin_dir);

      (* Plugins *)

      (* Librairie d'écriture de plugins du parser *)
      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s/patoline\n" (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);
      Printf.fprintf out "\tinstall -p -m 755 src/Patoline/META src/Patoline/Build.cmi src/Patoline/Util.cmi $(DESTDIR)%s/patoline\n" (if !ocamlfind_dir="" then "$(shell ocamlfind printconf destdir)" else escape !ocamlfind_dir);

      Array.iter (fun plugin->
        if Filename.check_suffix plugin ".cmxs" then (
          Printf.fprintf out "\tinstall -p -m 644 src/plugins/%s $(DESTDIR)%s\n" plugin (escape !plugins_dir);
        )
      ) (Sys.readdir "src/plugins");


      Printf.fprintf out "\tinstall -m 755 -d $(DESTDIR)%s\n" (escape !plugins_dir);
      Printf.fprintf out "\tinstall -p -m 644 src/Drivers/patonet.ml $(DESTDIR)%s/patonet.ml\n" (escape !plugins_dir);

      (* emacs *)
      Printf.fprintf out "\tcd emacs; install -m 755 -d $(DESTDIR)%s\n" emacsdir;
      Printf.fprintf out "\tcd emacs; install -p -m 644 *.el $(DESTDIR)%s/\n" emacsdir;

      (* Generate a .META file for the driver n with internal / external dependency intd / extd *)
      let driver_generated_metas = ref [] in
      let gen_meta_driver drv =
        let meta_name = "src/Drivers/" ^ drv.name ^ ".META" in
        driver_generated_metas := meta_name :: !driver_generated_metas;
        let f = open_out meta_name in
          Printf.fprintf f "package \"%s\" (\n" drv.name;
          Printf.fprintf f "archive(byte)=\"%s.cma\"\n" drv.name;
          Printf.fprintf f "archive(native)=\"%s.cmxa\"\n" drv.name;
          let alldep = (gen_pack_line ~query:false (Package "Typography" :: drv.internals)) @ (gen_pack_line [Driver drv]) in
          Printf.fprintf f "requires=\"%s\"\n" (String.concat "," alldep);
          Printf.fprintf f ")\n";
          close_out f
      in

      let _ =
        if can_build_driver patoline_driver_gl
        then gen_meta_driver patoline_driver_gl in

      let _ =
        if can_build_driver patoline_driver_image
        then gen_meta_driver patoline_driver_image in

      let meta=open_out "src/Typography/META" in
        Printf.fprintf meta
          "name=\"Typography\"\nversion=\"0.1\"\ndescription=\"Typography library\"\nrequires=\"rbuffer,%s\"\n"
          (String.concat "," (gen_pack_line [Package "str"; Package "camomile";
                                             Package "zip"; Package "camlimages.all_formats"]));
        Printf.fprintf meta "archive(native)=\"Typography.cmxa, DefaultFormat.cmxa, ParseMainArgs.cmx\"\n";
        Printf.fprintf meta "archive(byte)=\"Typography.cma, DefaultFormat.cma, ParseMainArgs.cmo\"\n";

      let check_name file=
        let valid=ref (String.length file>0) in
        for i=0 to String.length file-1 do
          valid:= !valid && ((file.[i]>='a' && file.[i]<='z')
                             || (file.[i]>='A' && file.[i]<='Z')
                             || (file.[i]>='0' && file.[i]<='9'))
        done;
        !valid
      in
      let make_meta_part dir file =
        if Filename.check_suffix file ".ml" && check_name (Filename.chop_extension file)
        then (
          let base_file = Filename.chop_extension file in
          let custom_meta = Filename.concat dir (base_file^".META") in
          try
            let custom_meta_fd = open_in custom_meta in
            let buf = String.create (in_channel_length custom_meta_fd) in
            really_input custom_meta_fd buf 0 (in_channel_length custom_meta_fd);
            close_in custom_meta_fd;
            Printf.fprintf meta "%s\n" buf
          with Sys_error _ ->
            Printf.fprintf meta
              "package \"%s\" (\nrequires=\"Typography\"\narchive(native)=\"%s\"\narchive(byte)=\"%s\"\n)\n"
              base_file (base_file^".cmxa") (base_file^".cma")
        )
      in
      Array.iter
        (fun file ->
          if is_substring "Format" file && file <> "DefaultFormat.ml"
          then make_meta_part "src/Format" file) (Sys.readdir "src/Format");
      Array.iter (make_meta_part "src/Drivers") (Sys.readdir "src/Drivers");
      close_out meta;

      (* Ecriture de la configuration *)
      let conf=if Sys.os_type= "Win32" then (
        let path_var="PATOLINE_PATH" in
        Printf.sprintf "(** Configuration locale (chemins de recherche des fichiers) *)\nlet path=try Sys.getenv %S with _->\"\"\n(** Chemin des polices de caractères *)\nlet fontsdir=%S\nlet fontspath=ref [%s]\n(** Chemin de l'éxécutable Patoline *)\nlet bindir=%S\n(** Chemin des grammaires *)\nlet grammarsdir=%S\nlet grammarspath=ref [%s]\n(** Chemin des dictionnaires de césures *)\nlet hyphendir=%S\nlet hyphenpath=ref [%s]\n(** Chemin des plugins de compilation *)\nlet pluginsdir=%S\nlet pluginspath=ref [%s]\nlet local_path:string list ref=ref []\n"
          path_var
          !fonts_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !fonts_dirs)))
          !bin_dir
          !grammars_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !grammars_dirs)))
          !hyphen_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !hyphen_dirs)))
          !plugins_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !plugins_dirs)))
      ) else (
        Printf.sprintf "(** Configuration locale (chemins de recherche des fichiers) *)\n(** Chemin des polices de caractères *)\nlet fontsdir=%S\nlet fontspath=ref [%s]\n(** Chemin de l'éxécutable Patoline *)\nlet bindir=%S\n(** Chemin des grammaires *)\nlet grammarsdir=%S\nlet grammarspath=ref [%s]\n(** Chemin des dictionnaires de césures *)\nlet hyphendir=%S\nlet hyphenpath=ref [%s]\n(** Chemin des plugins de compilation *)\nlet pluginsdir=%S\nlet pluginspath=ref [%s]\nlet local_path:string list ref=ref []\n"
          !fonts_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !fonts_dirs)))
          !bin_dir
          !grammars_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !grammars_dirs)))
          !hyphen_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !hyphen_dirs)))
          !plugins_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !plugins_dirs)))
      )
      in
        Printf.fprintf config "%s" conf;
        Printf.fprintf config' "%s" conf;
        Printf.fprintf out "clean:\n\trm -Rf _build\n\tmake -C src clean\n";
        Printf.fprintf out "distclean: clean\n\trm -f Makefile src/Typography/Config.ml src/Patoline/Config.ml src/Typography/META src/Makefile.config %s\n" (String.concat " " !driver_generated_metas);
        close_out out;
        close_out config;
        close_out config';

          Printf.printf "\nGood news: you can use Patoline !\n
Now build it by doing:
	make
	make install

And optionally
	make doc
	make check

"
