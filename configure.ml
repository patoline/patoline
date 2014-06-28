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

let prefix=ref "/usr/local/"
let bin_dir=ref ""
let fonts_dir=ref ""
let grammars_dir=ref ""
let hyphen_dir=ref ""
let ocaml_lib_dir=ref (Findlib.default_location())
let ocaml_dlls_dir=ref (Filename.concat !ocaml_lib_dir "stublibs")
let fonts_dirs=ref []
let grammars_dirs=ref []
let plugins_dir=ref ""
let plugins_dirs=ref []
let hyphen_dirs=ref []
let driver_dir=ref ""
let lang=ref "FR"
let ban_comic_sans=ref false
let pdf_type3_only=ref false
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

type findlib_package =
  {
    pack_name : string;
    (* Some packages may have different names, which we try to find if
     * the first package name yields no result. *)
    known_aliases : string list;
    additional_check : string -> string -> bool;
    additional_check_string : string (* a message to report additional check failure *)
  }

let package_no_check _ _ = true

let package_min_version min_version package alias =
  (* parse_version splits a string representing a version number to a
   * tuple which can be compared to other version numbers using OCaml
   * regular < operator.
   *
   * For example, it maps the string "2.3.17~20131103" to the tuple:
       ([2; 3; 17], 20131103)
   *
   * This function parses for the moment version numbers like:

       epoch:ver.rev.level~add-patch

     where each placeholder can be an integer. The only mandatory part
     is the dot-separated list in the middle (which may have any
     arbitrary length).

     The resulting tuple would be:

       (epoch, [ver; rev; level], add, patch)
   *)
  let parse_version v =
    (* Get a matched group, or return a default value if the group does
     * not exist. *)
    let def_group def n s =
      try Str.matched_group n s
      with Not_found -> def
    in

    let rever = Str.regexp "^\\(\\([0-9]+\\):\\)?\\([0-9.]+\\)\\(~\\([0-9]+\\)\\)?\\(-\\([0-9]+\\)\\)?$" in
    let _ = Str.string_match rever v 0 in
    let epoch = def_group "0" 2 v
    and main  = def_group ""  3 v
    and additional = def_group "0" 5 v
    and last = def_group "0" 7 v in

    (* Actually build the version tuple *)
    (
      (* Epoch *)
      int_of_string epoch,
      (* Main part of the version number: we have a dot-separated list
       * of integers. *)
      List.map int_of_string (Str.split (Str.regexp "\\.") main),
      (* Additional part after ~ *)
      int_of_string additional,
      (* Last part, after - *)
      int_of_string last
    )
  in
  let installed = Findlib.package_property [] alias "version" in
  let min_ver_list = parse_version min_version
  and ins_ver_list = parse_version installed in
  if min_ver_list <= ins_ver_list then
    true
  else
    (
      Printf.printf "%s is too old: version %s requested, but %s has been found\n"
        package min_version installed;
      false
    )

let patoline_uses_packages =
  let res = Hashtbl.create 10 in
  List.iter (fun p -> Hashtbl.add res p.pack_name p)
  [
    {
      pack_name = "camomile";
      known_aliases = [];
      additional_check = package_min_version "0.8.3";
      additional_check_string = "version >= 0.8.3";
    };
    {
      pack_name = "zip";
      known_aliases = ["camlzip"];
      additional_check = package_no_check;
      additional_check_string = "";
    };
    {
      pack_name = "cairo";
      known_aliases = ["ocaml-cairo"];
      additional_check = package_no_check;
      additional_check_string = "";
    };
    {
      pack_name = "lablgl";
      known_aliases = ["lablGL"];
      additional_check = package_no_check;
      additional_check_string = "";
    };
    {
      pack_name = "lablgl.glut";
      known_aliases = ["lablGL.glut"];
      additional_check = package_no_check;
      additional_check_string = "";
    };
    {
      pack_name = "fontconfig";
      known_aliases = [];
      additional_check = (fun package alias ->
        let fontconfig_min_version = "0~20131103" in
        if Findlib.package_property [] alias "version" = "0.1" then
          (
            Printf.printf "Version 0.1 of fontconfig is outdated, update fontconfig to version %s\n"
              fontconfig_min_version;
            false
          )
        else package_min_version fontconfig_min_version package alias);
      additional_check_string = "version >= 0~20131103";
    };
  ];
  res

let ocamlfind_query =
  let checked = Hashtbl.create 10 in
  function pack ->
    try
      (* Easy case, when package has already been looked up *)
      Hashtbl.find checked pack
    with
      Not_found ->
        Printf.printf "Looking for package %s..." pack;
        let liste, additional_check =
          (try
            let p = Hashtbl.find patoline_uses_packages pack in
            (p.pack_name :: p.known_aliases, p.additional_check) 
            with Not_found -> [pack], package_no_check
          ) in
        let res =
          List.fold_left
           (fun res pack_alias ->
             if fst res then
               res
             else
               try
                 let _ = Findlib.package_directory pack_alias in
                 if additional_check pack pack_alias
                 then (true, pack_alias)
                 else res
               with
               _ -> res
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
  {
    (* Driver module name *)
    name: string;
    (* List of required packages to build the driver *)
    needs: driver_needs list;
    (* List of optional packages, which improve the driver when present *)
    suggests: driver_needs list;
    (* List of Patoline own packages (= not installed system-wide while building
     * Patoline) needed to output a document using this driver. The code package
     * does not need to be specified, as it is always added. *)
    internals: driver_needs list;
    (* Says whether configure.ml should generate the driver's META file from
     * this record, or rely on a preexisting META in the source tree. *)
    autometa: bool;
  }

let patoline_driver_gl =
  { name = "DriverGL";
    needs =(Package "str")::(Package "camlimages.all_formats")::
      (Package "lablgl")::(Package "lablgl.glut")::[];
    suggests = [];
    internals = []; (* [Package "Typography.GL"] *)
    autometa = true;
  }
let patoline_driver_image =
  { name = "Image";
    needs = [Package "camlimages.all_formats"; Package "lablgl"; Package "lablgl.glut"];
    suggests = [];
    internals = [Driver patoline_driver_gl];
    autometa = true }

let svg_driver =
    { name = "SVG"; needs = []; suggests = []; internals = []; autometa = true }

(* List of all Patoline drivers.
 * Add yours to this list in order to build it. *)
let r_patoline_drivers = ref
  [
    { name = "None"; needs = []; suggests = []; internals = []; autometa = true };
    { name = "Pdf"; needs = []; suggests = [Package "zip"]; internals = []; autometa = false };
    { name = "Bin"; needs = []; suggests = []; internals = []; autometa = true };
    { name = "Html"; needs = []; suggests = []; internals = []; autometa = true };
    { name = "Patonet"; needs = [Package "cryptokit"]; suggests = []; internals = [Driver svg_driver]; autometa = true };
    { name = "DriverCairo"; needs = [Package "cairo"]; suggests = []; internals = [] ; autometa = true };
    svg_driver; patoline_driver_gl;
    { name = "Net"; needs = []; suggests = []; internals = [Driver svg_driver]; autometa = true };
    { name = "Web"; needs = []; suggests = []; internals = [Driver svg_driver]; autometa = true };
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
  let found, missing = List.partition check_need d.needs in
  if List.exists (fun x->x.name==d.name) !r_patoline_drivers
    && missing = [] then
    true
  else (
    Printf.eprintf "Warning: driver %s not build because %s are missing\n"
      d.name
      (String.concat ", " (List.map (function
      | Package pack  -> 
	let n = Hashtbl.find patoline_uses_packages pack in
	n.pack_name ^ " " ^ n.additional_check_string
      | Driver d -> d.name ^ " driver") missing));
    false)

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
      ("Typography."^d.name) :: (aux_gen d.needs) @ (aux_gen d.suggests) @ (aux_gen needs)
  in (List.filter ((<>) "") (aux_gen needs))

let _=
  parse [
    ("--prefix", Set_string prefix, "  prefix (/usr/local/ by default)");
    ("--bin-prefix", Set_string bin_dir, "  directory for the binaries ($PREFIX/bin/ by default)");
    ("--ocaml-libs", Set_string ocaml_lib_dir, "  directory for the caml libraries (`ocamlfind printconf destdir` by default; `ocamlc -where` is another sensible choice)");
    ("--ocaml-dlls", Set_string ocaml_dlls_dir, "  directory for the dll.so caml file (`ocamlfind printconf destdir`/stublibs by default; the distination should be liste in ocaml's ld.conf usually in /usr/lib/ocaml/ld.conf)");
    ("--fonts-dir", Set_string fonts_dir, "  directory for the fonts ($PREFIX/share/patoline/fonts/ by default)");
    ("--grammars-dir", Set_string grammars_dir, "  directory for the grammars ($PREFIX/lib/patoline/grammars/ by default)");
    ("--plugins-dir", Set_string plugins_dir, "  directory for the plugins ($PREFIX/lib/patoline/plugins/ by default)");
    ("--driver-dir", Set_string hyphen_dir, "  directory for the drivers ($PREFIX/lib/Typography/ by default)");
    ("--hyphen-dir", Set_string hyphen_dir, "  directory for the hyphenation dictionnaries ($PREFIX/share/patoline/hyphen/ by default)");
    ("--extra-fonts-dir", String (fun pref->fonts_dirs:=pref:: !fonts_dirs), "  additional directories patoline should scan for fonts");
    ("--extra-grammars-dir", String (fun pref->grammars_dirs:=pref:: !grammars_dirs), "  additional directories patoline should scan for grammars");
    ("--extra-hyphen-dir", String (fun pref->hyphen_dirs:=pref:: !hyphen_dirs), "  additional directories patoline should scan for hyphenation dictionaries");
    ("--ban-comic-sans", Set ban_comic_sans, " disallows the use of a font with name '*comic*sans*'. Robust to filename changes.");
    ("--pdf-type3-only", Set pdf_type3_only, " converts all fonts to vector graphics in PDFs. Improves compatibility, does not change the file size significatively. May worsen font rasterizing in some readers.");
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
  if !driver_dir="" then driver_dir:=Filename.concat !ocaml_lib_dir "Typography";

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
    (ocamlfind_has "dyp") && st = (Unix.WEXITED 0)
  in

  if not (ocamlfind_has "camomile") then (
    Printf.eprintf "Error: package camomile missing.\n";
    exit 1
  );

  let has_mysql = ocamlfind_has "mysql" in
  if not has_mysql then (
    Printf.eprintf "Warning: package mysql missing, Patonet will not support Mysql storage\n";
  );


  let has_sqlite3=ocamlfind_has "sqlite3" in
  if not has_sqlite3 then (
    Printf.eprintf "Warning: package sqlite3 missing, Patonet and Bibi will not support sqlite storage\n";
  );

  let has_glr = ocamlfind_has "glr" in
  if not has_glr then 
    Printf.eprintf "Warning: glr is missing: the new EXPERIMENTAL parser of patoline will not be installed\n";

  let has_fontconfig = ocamlfind_has "fontconfig" in
  if not has_fontconfig then 
    Printf.eprintf "Warning: fontconfig is missing, patoline will not use it to search for fonts\n";

  let config=open_out "src/Typography/Config.ml" in
  let config'=open_out "src/Patoline/Config2.ml" in

  let emacsdir = Filename.concat !prefix "share/emacs/site-lisp/patoline" in

  let make=open_out "src/Makefile.config" in
  Printf.fprintf make "OCPP := cpp -C -ffreestanding -w %s%s%s%s%s%s%s%s%s\n"
    (if Sys.os_type="Win32" then "-D__WINDOWS__ " else "")
    (if Sys.word_size=32 || !int32 then "-DINT32 " else "")
    (if ocamlfind_has "zip" then "-DCAMLZIP " else "")
    (if has_mysql then "-DMYSQL " else "")
    (if has_sqlite3 then "-DSQLITE3 " else "")
    (if ocamlfind_has "camlimages.all_formats" then "-DCAMLIMAGES " else "")
    (if !ban_comic_sans then "-DBAN_COMIC_SANS " else "")
    (if !pdf_type3_only then "-DPDF_TYPE3_ONLY " else "")
    (if String.uppercase !lang <> "EN" then ("-DLANG_"^String.uppercase !lang) else "");
  (if has_dypgen then
      (Printf.fprintf make "PATOLINE := src/Patoline/patoline\n";
      Printf.fprintf make "PACKAGE_DYP := -package dyp\n")
   else
      (Printf.fprintf make "PATOLINE :=\n";
      Printf.fprintf make "PACKAGE_DYP :=\n")
  );
  (if ocamlfind_has "zip" then
      Printf.fprintf make "PACKAGE_ZIP := -package %s\n" (snd (ocamlfind_query "zip"))
   else
      Printf.fprintf make "PACKAGE_ZIP :=\n"
  );
  Printf.fprintf make "PACK := -package %s\n"
    (String.concat "," (gen_pack_line [Package "camomile"; Package "zip"; Package "mysql";
                                       Package "camlimages.all_formats";
                                       Package "cairo"; Package "fontconfig"]));

  Printf.fprintf make "INSTALL_FONT_DIR := %s\n" !fonts_dir;
  Printf.fprintf make "INSTALL_GRAMMARS_DIR := %s\n" !grammars_dir;
  Printf.fprintf make "INSTALL_HYPHEN_DIR := %s\n" !hyphen_dir;
  Printf.fprintf make "INSTALL_TYPOGRAPHY_DIR := %s/Typography\n" !ocaml_lib_dir;
  Printf.fprintf make "INSTALL_DRIVERS_DIR := %s\n" !driver_dir;
  Printf.fprintf make "INSTALL_DLLS_DIR := %s\n" !ocaml_dlls_dir;
  Printf.fprintf make "INSTALL_EMACS_DIR := %s\n" emacsdir;
  Printf.fprintf make "INSTALL_RBUFFER_DIR := %s/rbuffer\n" !ocaml_lib_dir;
  Printf.fprintf make "INSTALL_UTIL_DIR := %s/util\n" !ocaml_lib_dir;
  Printf.fprintf make "INSTALL_IMGLIB_DIR := %s/imagelib\n" !ocaml_lib_dir;
  Printf.fprintf make "INSTALL_LIBFONTS_DIR := %s/fonts\n" !ocaml_lib_dir;
  Printf.fprintf make "INSTALL_BIBI_DIR := %s/bibi\n" !ocaml_lib_dir;
  Printf.fprintf make "INSTALL_PATOPLOT_DIR := %s/patoplot\n" !ocaml_lib_dir;
  Printf.fprintf make "INSTALL_PLUGINS_DIR := %s\n" !plugins_dir;

  Printf.fprintf make "INSTALL_BIN_DIR := %s\n" !bin_dir;


  (* Write out the list of enabled drivers *)
  let ok_drivers = List.filter can_build_driver patoline_drivers in
  Printf.fprintf make "DRIVERS := %s\n"
    (String.concat " "
      (List.map (fun d -> d.name) ok_drivers)
    );

  let has_patonet = List.exists (fun d -> d.name = "Patonet") ok_drivers in

  (* Output -package option for enabled drivers *)
  List.iter
  (fun d ->
    Printf.fprintf make "PACK_DRIVER_%s := %s\n"
      d.name
      (String.concat "," (gen_pack_line (d.needs @ d.suggests)))
  )
  ok_drivers;

  (* Enable compilation of ocaml-bibi if sqlite3 is installed *)
  Printf.fprintf make "OCAML_BIBI := %s\n" (if has_sqlite3 then "ocaml-bibi" else "");

  (* Enable compilation of pa_patoline if glr is installed *)
  Printf.fprintf make "PA_PATOLINE := %s\n" (if has_glr then "src/Patoline/pa_patoline" else "");

  (* Tell make which ConfigFindFont (fontconfig or not) should be linked while
   * building Typograhy.cmxa. *)
  Printf.fprintf make "FINDFONT := %s\n" (if has_fontconfig then "ConfigFindFontFC" else "ConfigFindFontLeg");
  close_out make;

  (* Generate a .META file for the driver n with internal / external dependency intd / extd *)
  let driver_generated_metas = ref [] in
  let gen_meta_driver drv =
    if can_build_driver drv && drv.autometa then begin
      let meta_name = "src/Drivers/" ^ drv.name ^ "/" ^ drv.name ^ ".META" in
      driver_generated_metas := meta_name :: !driver_generated_metas;
      let f = open_out meta_name in
        Printf.fprintf f "package \"%s\" (\n" drv.name;
        Printf.fprintf f "archive(byte)=\"%s.cma\"\n" drv.name;
        Printf.fprintf f "archive(native)=\"%s.cmxa\"\n" drv.name;
        let alldep = (gen_pack_line ~query:false (Package "Typography" :: drv.internals)) @ (gen_pack_line drv.needs) @ (gen_pack_line drv.suggests) in
        Printf.fprintf f "requires=\"%s\"\n" (String.concat "," alldep);
        Printf.fprintf f ")\n";
        close_out f
    end
  in
  List.iter gen_meta_driver !r_patoline_drivers;

  (* Generate the META file for Typography, which details package information
   * for Typography.cmxa/Typography.cma as well as subpackages for each format.
   * Each format in the source tree can be shipped with a custom FormatName.META
   * file, which is included as is. Otherwise, a generic entry is generated for
   * the format.
   *
   * DefaultFormat.ml is excluded, since it is already embedded inside
   * Typography.
   *)
  let meta=open_out "src/Typography/META" in
    Printf.fprintf meta
      "name=\"Typography\"\nversion=\"0.1\"\ndescription=\"Typography library\"\nrequires=\"util,fonts,%s\"\n"
      (String.concat "," (gen_pack_line [Package "str"; Package "camomile"; Package "mysql";
                                         Package "zip";
                                         Package "camlimages.all_formats";
                                         Package "fontconfig"]));
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
        let custom_meta_len = in_channel_length custom_meta_fd in
        let buf = String.create custom_meta_len in
        really_input custom_meta_fd buf 0 custom_meta_len;
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
      if (is_substring "Format" file || file = "Interactive.ml") && file <> "DefaultFormat.ml"
      then make_meta_part "src/Format" file) (Sys.readdir "src/Format");
  List.iter (fun drv ->
    make_meta_part (Filename.concat "src/Drivers" drv.name) (drv.name ^ ".ml")
  ) !r_patoline_drivers;
  close_out meta;

      (* Ecriture de la configuration *)
      let conf=if Sys.os_type= "Win32" then (
        let path_var="PATOLINE_PATH" in
        Printf.sprintf "(** Configuration locale (chemins de recherche des fichiers) *)\nlet path=try Sys.getenv %S with _->\"\"\n(** Chemin des polices de caractères *)\nlet fontsdir=%S\nlet fontspath=ref [%s]\n(** Chemin de l'éxécutable Patoline *)\nlet bindir=%S\n(** Chemin des grammaires *)\nlet grammarsdir=%S\nlet grammarspath=ref [%s]\n(** Chemin des dictionnaires de césures *)\nlet hyphendir=%S\nlet hyphenpath=ref [%s]\n(** Chemin des plugins de compilation *)\nlet driverdir=[%S]\nlet pluginsdir=%S\nlet pluginspath=ref [%s]\nlet local_path:string list ref=ref []\nlet user_dir=(try Filename.concat (Sys.getenv \"APPDATA\") \"patoline\" with Not_found->\"\")\n"
          path_var
          !fonts_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !fonts_dirs)))
          !bin_dir
          !grammars_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !grammars_dirs)))
          !hyphen_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !hyphen_dirs)))
	  !driver_dir
          !plugins_dir
          (String.concat ";" ("\".\""::List.map (Printf.sprintf "Filename.concat path %S") (List.rev !plugins_dirs)))
      ) else (
        Printf.sprintf "(** Configuration locale (chemins de recherche des fichiers) *)\n(** Chemin des polices de caractères *)\nlet fontsdir=%S\nlet fontspath=ref [%s]\n(** Chemin de l'éxécutable Patoline *)\nlet bindir=%S\n(** Chemin des grammaires *)\nlet grammarsdir=%S\nlet grammarspath=ref [%s]\n(** Chemin des dictionnaires de césures *)\nlet hyphendir=%S\nlet hyphenpath=ref [%s]\n(** Chemin des plugins de compilation *)\nlet driverdir=ref [%S]\nlet pluginsdir=%S\nlet pluginspath=ref [%s]\nlet local_path:string list ref=ref []\nlet user_dir=Filename.concat (try Sys.getenv \"XDG_DATA_HOME\" with Not_found -> Filename.concat (Sys.getenv \"HOME\") \".local/share\" ) \"patoline\"\n"
          !fonts_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !fonts_dirs)))
          !bin_dir
          !grammars_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !grammars_dirs)))
          !hyphen_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !hyphen_dirs)))
	  !driver_dir
          !plugins_dir
          (String.concat ";" (List.map (Printf.sprintf "%S") (List.rev !plugins_dirs)))
      )
      in
        Printf.fprintf config "%s" conf;
        Printf.fprintf config "(** Module used to query font paths *)\nlet findFont=%s.findFont fontspath\n" (if ocamlfind_has "fontconfig" then "ConfigFindFontFC" else "ConfigFindFontLeg");
	Printf.fprintf config "let atmost = ref 3\nlet input_bin = ref (None : string option) (* if Some str, the file a a .bin that should be read instead of producing the structure/pages *)\nlet driver=ref (None:string option)\n";

        Printf.fprintf config' "%slet has_patonet = %b\n" conf has_patonet;

        close_out config;
        close_out config';

        (*
        let _=
          if Sys.os_type="Unix" then (
            let i=Unix.open_process_in "uname" in
            let uname=input_line i in
            close_in i;
            let o=open_out "Accessoires/Rules.mk" in
            if uname="Darwin" then (
              Printf.fprintf o "d := $(if $(d),$(d)/,)$(mod)\n";
              Printf.fprintf o "all:$(d)/patomote\n";
              Printf.fprintf o "$(d)/patomote:$(d)/patomote_macos.m\n";
              Printf.fprintf o "\tgcc -o $@ -framework cocoa -framework iokit $<\n";
              Printf.fprintf o "install:\n";
              Printf.fprintf o "\tinstall -m 755 $(d)/patomote $(DESTDIR)/$(INSTALL_BIN_DIR)\n"
            ) else (
            );
            close_out o;
          ) else (

          )
        in
        *)
        (* Generate Rules.clean which tells GNUmakefile which files we've
         * generated, and have to be removed. *)
        let rules_clean = open_out "Rules.clean" in
        Printf.fprintf rules_clean "DISTCLEAN += Rules.clean src/Typography/Config.ml src/Patoline/Config2.ml src/Typography/META src/Makefile.config %s\n"
        (String.concat " " !driver_generated_metas);
        close_out rules_clean;

          Printf.printf "\nGood news: you can use Patoline !\n
Now build it by doing:
	make
	make install

And optionally
	make doc
	make check

"
