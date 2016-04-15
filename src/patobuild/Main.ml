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

open BuildDir
open PatConfig

let local_path = ref []

let output=ref ""
let files=ref []
let compile = ref true
let run = ref true
let no_grammar=ref false
let deps_only=ref false
let extras = ref []
let extras_top = ref []
let extras_pp = ref []
let cmd_line_format = ref false
let cmd_line_driver= ref false
let formats=ref []
let driver = ref "Pdf"
let dynlink = ref false
let dirs = ref []
let package_list = ref ["rawlib"; "db"; "Typography"]
let patoline=ref (Sys.argv.(0))
let ocamlopt=ref "ocamlopt"
let recompile=ref false
let quiet=ref 1
let main_ml=ref false
let edit_link=ref false
let shortDrivers = List.map (fun name ->
  let len = String.length name in
  if len > 6 && String.sub name 0 6 = "Driver" then
    String.sub name 6 (len - 6) else name) patoconfig.drivers
let aliasDriver=
  List.filter (fun (c, c') -> c <> c')
  (List.combine shortDrivers patoconfig.drivers)

(* Regexp for pragma-style things. *)
let set_format   = Str.regexp "^[ \t(*]*[ \t]*#FORMAT[ \t]+\\([^ \t]+\\)[ )*\t\r]*$"
let set_driver   = Str.regexp "^[ \t(*]*[ \t]*#DRIVER[ \t]+\\([^ \t]+\\)[ )*\t\r]*$"
let set_grammar  = Str.regexp "^[ \t(*]*[ \t]*#GRAMMAR[ \t]+\\([^ \t]+\\)[ )*\t\r]*$"
let set_noamble  = Str.regexp "^[ \t(*]*[ \t]*#NOAMBLE[ )*\t\r]*$"
let link         = Str.regexp "^[ \t(*]*[ \t]*#LINK[ \t]+\\([^ \t]+\\)[ )*\t\r]*$"
let depends      = Str.regexp "^[ \t(*]*[ \t]*#DEPENDS[ \t]+\\([^ \t]+\\)[ )*\t\r]*$"
let add_comp_opt = Str.regexp "^[ \t(*]*[ \t]*#COMPILATION[ \t]+\\(.+\\)[ )*\t\r]*$"
let add_package  = Str.regexp "^[ \t(*]*[ \t]*#PACKAGES[ \t]+\\([^ \t]+\\(,[ \t]*[^ \t]+\\)*\\)[ )*\t\r]*$"
let add_dir      = Str.regexp "^[ \t(*]*[ \t]*#DIRECTORIES[ \t]+\\([^ \t]+\\(,[ \t]*[^ \t]+\\)*\\)[ )*\t\r]*$"

open Language

       let getopts str=
  let restr="\\('\\(\\'\\|[^\']\\)*\'\\)\\|\\(\"\\(\\\"\\|[^\"]\\)*\"\\)\\|\\([^ \t\n\"]+\\)" in
  let re=Str.regexp restr in
  let rec getopts i res=
    let next,j=
      try
        let _=Str.search_forward re str i in
        let i'=Str.match_beginning () in
        let j=Str.match_end () in
        let gr=String.sub str i' (j-i') in
        gr, j
      with
          Not_found->"",(-1)
    in
    if j<0 then List.rev res else getopts (max (i+1) j) (next::res)
  in
  getopts 0 []

let spec =
  [("--extra-fonts-dir",Arg.String (fun x->extras_top:=["--extra-fonts-dir";x]::(!extras_top)),
    message (Cli Extra_fonts));
   ("--extra-hyph-dir",Arg.String (fun x->extras_top:=["--extra-hyph-dir";x]::(!extras_top)),
    message (Cli Extra_hyph));
   ("--font-filter",Arg.String (fun x->extras_top:=["--font-filter";x]::(!extras_top)),
    message (Cli Font_filter));
   ("--build-dir",Arg.String (fun x-> build_dir := x; extras_top:=["--build-dir";x]::(!extras_top)),
    message (Cli Build_dir));
   ("--no-build-dir",Arg.Unit (fun ()-> build_dir := ""; extras_top:=["--no-build-dir"]::(!extras_top)),
    message (Cli No_build_dir));
   ("--main-ml",Arg.Set main_ml, message (Cli MainMl));
   ("--ml",Arg.Tuple [Arg.Clear compile; Arg.Clear run], message (Cli Ml));
   ("-I",Arg.String (fun x-> local_path := x :: !local_path; dirs := x :: !dirs),
    message (Cli Dirs));
   ("--recompile",Arg.Set recompile,message (Cli Recompile));
   ("--no-grammar",Arg.Unit (fun ()-> extras_pp := ["--no-default-grammar"] :: !extras_pp), message (Cli No_grammar));
   ("--format",Arg.String
     (fun f ->formats := Filename.basename f:: !formats), message (Cli Format));
   ("--dynlink",Arg.Unit(fun () -> dynlink:=true),message (Cli Dynlink));
   ("--driver",Arg.String
     (fun f ->driver := f; cmd_line_driver := true), message (Cli Driver) ^ (String.concat ", " shortDrivers));
   ("-package",Arg.String (fun s-> package_list:= s::(!package_list)),
    message (Cli Package));
   ("-o",Arg.Set_string output, message (Cli Output));
   ("--bin",Arg.Tuple [Arg.Set compile; Arg.Clear run], message (Cli Bin));
   ("--edit-link", Arg.Set edit_link, message (Cli Edit_link));
   ("--patoline",Arg.String (fun s->patoline:=s), message (Cli Patoline));
   ("--ocamlopt",Arg.String (fun s->ocamlopt:=s), message (Cli Ocamlopt));
   ("-j",Arg.Int (fun s->Build.j:=max !Build.j s), message (Cli Parallel));
   ("--quiet", Arg.Int (fun x->quiet:=x), message (Cli Quiet));
   ("--",Arg.Rest (fun s -> extras := [s]:: !extras), message (Cli Remaining));
   ("--topopts",Arg.String (fun s -> extras_top := getopts s:: !extras_top), message (Cli TopOpts));
   ("--ccopts",Arg.String (fun s -> extras := getopts s :: !extras), message (Cli CCOpts));
   ("--ppopts",Arg.String (fun s -> extras_pp := getopts s :: !extras_pp), message (Cli PPOpts));
   ("--debug-parser", Arg.Int (fun i -> extras_pp := ["--debug"; string_of_int i] :: !extras_pp), message (Cli Debug_parser));
  ]


let mlname_of f = if !output="" then chg_ext f ".tml" else !output
let binname_of f = chg_ext f ".tmx"
let execname_of f = if Filename.is_implicit f then "./"^(binname_of f) else (binname_of f)

let write_main_file dynlink where formats driver suppl main_mod outfile=
  let formats=match formats with []->["DefaultFormat"] | _->formats in
  let cache_name = chg_ext outfile ".tdx" in
  Printf.fprintf where
    "(* #FORMAT %s *)
(* #DRIVER %s *)
%s
open Typography
open Typography.Box
open Typography.Document
open RawContent
open Color
open DefaultFormat.MathsFormat

let _ = Distance.read_cache \"%s\"
module D=(struct let structure=ref (Node { empty with node_tags=[\"intoc\",\"\"] },[]) end:DocumentStructure)\n"
    (List.hd formats)
    driver
    suppl
    cache_name;

  let i=ref 0 in
  if dynlink then
    Printf.fprintf where "let driver = match !Driver.driver with
  None -> %S
| Some s -> s
let _ = DynDriver.load_driver driver
module Driver = (val Hashtbl.find DynDriver.drivers driver:Driver:OutputDriver);;\n"
      driver
  else
    Printf.fprintf where "module Driver = %s;;\n" driver;

  Printf.fprintf where
    "let _ = Arg.parse_argv (Driver.filter_options Sys.argv) (Driver.driver_options @ DefaultFormat.spec) ignore \"Usage :\"\n";

  List.iter (fun x->
    Printf.fprintf where "module Patoline_Format%d=%s.Format(D);;\nopen Patoline_Format%d;;\n" !i x !i;
    incr i
  ) formats;
  Printf.fprintf where "module Patoline_Format=Patoline_Format%d\n" (!i-1);

  Printf.fprintf where "module Patoline_Output=Patoline_Format%d.Output(Driver);;\n" (!i-1) ;

  let buf=Buffer.create 100 in
  Printf.bprintf buf
    "module TMP = %s.Document(Patoline_Output)(D);;\nopen TMP;;\n" main_mod;
  Buffer.output_buffer where buf;
  Printf.fprintf where
    "let _ =Patoline_Output.output Patoline_Output.outputParams (fst (top !D.structure))
      (List.fold_left (fun acc f -> f acc) Patoline_Format.defaultEnv !init_env_hook) %S\n" outfile;
  Printf.fprintf where "let _ = Distance.write_cache \"%s\"\n" cache_name



(************************************************)
open UsualMake
open FilenameExtra
let dynlinked=(Mutex.create (), ref StrMap.empty)

type options={
  deps:string list;
  formats:string list;
  grammar:string option;
  driver:string;
  comp_opts:string list;
  packages:string list;
  directories:string list;
  noamble:bool
}

module StringSet = Set.Make(Str_)
let compact l =
  let s = List.fold_right StringSet.add l StringSet.empty in
  StringSet.elements s

module StrGraph = Graph.Make(Str_)

(* A function that creates a [StrGraph.t] out of a [string list StrMap.t] *)
let strGraph_of_strMap m =
  let open StrGraph in
  (* We create a mutable map [nodes] of `type' string ~> node which we'll gradually update *)
  (* the idea being to maintain a bijection between strings and nodes *)
  let nodes = ref StrMap.empty in
  (* We also create a list of nodes which should also be kept up to date. This is useful because the list of keys of [nodes]
   may not contain all involved nodes, as nodes may have no dependencies. *)
  let nodelist : graph ref = ref [] in
  (* The next function attemps to associate a node to some argument string [s] via [nodes], and,
   if it fails, creates a new node with underlying string [s], which it readily adds to [nodes] *)
  let nodeOfString s =
    try StrMap.find s !nodes
    with Not_found -> let res = {  id = s ; sons = [] ; color = Virgin } in
                      let _ = nodes := StrMap.add s res !nodes in
                      let _ = nodelist := res :: !nodelist in
                      res
  in
  (* We brutally add all the dependencies mentioned by the given [StrMap.t], [m].  *)
  (* Because [m] only adds dependencies to a node once, we could probably get rid of the [@] below. *)
  let _ = StrMap.iter (fun s sons ->
               let node = nodeOfString s in
               let nsons = List.map nodeOfString sons in
               node.sons <- node.sons @ nsons)
                      m
  in !nodelist

let last_options_used file=
  let fread=open_in file in
  let _formats=ref "" in
  let _driver=ref "" in
  let rec pump () =
    let s = input_line fread in
    if Str.string_match set_format s 0 then (
      _formats:=Str.matched_group 1 s;
      if String.length !_driver=0 then pump ()
    )
    else if Str.string_match set_driver s 0 then (
      _driver:=Str.matched_group 1 s;
      if String.length !_formats=0 then pump ()
    )
    else pump ()
  in
  (try
     pump ()
   with End_of_file -> ());
  close_in fread;
  !_formats, !_driver


(* FIXME we sould not have to list formats here... *)
let default_formats =
  [ "FormatArticle" ; "FormatLetter" ; "FormatLivre" ; "FormatMemoire"
  ; "FormatSlides" ; "FormatThese" ; "FormatWeb" ; "Interactive"
  ; "LMFormat" ]

let add_format opts =
  let packages=
    List.fold_left (fun pac format->
      if not (List.mem ("Typography." ^ format) opts.packages) &&
        format<>"DefaultFormat" &&
        List.mem format default_formats &&
        (try
           let _=findPath (format ^ ".ml") (".":: !local_path) in
           false
         with _->true)
      then
        ("Typography."^format)::pac
      else
        pac
    ) opts.packages opts.formats
  in
(* No need to compile with the driver ...
  let packages=
    (if (not (List.mem ("Typography." ^ opts.driver) opts.packages)) &&
        (try
           let _=findPath (opts.driver ^ ".ml") (".":: !local_path) in
           false
         with _->true)
     then
        ("Typography." ^ opts.driver) :: packages
     else packages)
  in
*)
  { opts with packages = packages }

let rec breadth_first m h l=match h with
    []->l
  | _::_->
    let rec make_next h0 l0 h=match h with
        []->h0,l0
      | a::b->
        let next=try StrMap.find a !m with Not_found->[] in
        let h1=List.fold_left (fun h2 x->x::h2) h0 next in
        let l1=List.fold_left (fun l2 x->x::(List.filter (fun y->y<>x) l2)) l0 next in
        make_next h1 l1 b
    in
    let h1,l1=make_next [] l h in
    let h2=List.filter (fun x->not (List.mem x h) && not (List.mem x l)) h1 in
    breadth_first m h2 l1

let rec read_options_from_source_file f fread =
  let deps=ref [] in
  let formats=ref !formats in
  let grammar=ref (Some "DefaultGrammar") in
  let driver=ref !driver in
  let noamble=ref false in
  let comp_opts=ref [] in
  let packages=ref !package_list in
  let directories = ref [] in

  let nothing = Str.regexp "^[ \t]*\\((\\*.*\\*)\\)?[ \t\r]*$" in
  let blank=Str.regexp "^[ \t]*$" in
  let rec pump () =
    let s = input_line fread in
    if Str.string_match link s 0 then (
      let n=
        let n=Str.matched_group 1 s in
        chg_ext n ".cmo"
      in
      let name=
        try
          let name=FilenameExtra.findPath (chg_ext n ".ml") ("." :: !local_path) in
          let objects=(Mutex.create (), ref StrMap.empty) in
          Build.build_with_rule (patoline_rule objects) (Dynlink.adapt_filename (chg_ext name ".cmo")::f);
          Dynlink.adapt_filename (chg_ext name ".cmo")
        with
            No_matching_path _->FilenameExtra.findPath
              (Dynlink.adapt_filename (chg_ext n ".cmo"))
              ((fst patoconfig.plugins_dir) :: (snd patoconfig.plugins_dir))
      in
      let m,n=dynlinked in
      Mutex.lock m;
      if not (StrMap.mem name !n) then (
        n:=StrMap.add name () !n;
        try
          Dynlink.loadfile name;
        with
            Dynlink.Error e->(
              output_string stderr (Language.message (Language.Dynlink_error (Dynlink.error_message e)));
              exit 1
            )
      );
      Mutex.unlock m;
      pump ()
    )
    else if Str.string_match depends s 0 then (
      deps := Str.matched_group 1 s:: !deps;
      pump ()
    )
    else if Str.string_match set_grammar s 0 then (
      if Str.matched_group 1 s="" then grammar:=None else grammar:=Some (Str.matched_group 1 s);
      pump ()
    )
    else if Str.string_match set_format s 0 then (
      formats := Str.matched_group 1 s:: !formats;
      pump ()
    )
    else if Str.string_match set_driver s 0 then (
      if not !cmd_line_driver then driver := Str.matched_group 1 s;
      pump ()
    )
    else if Str.string_match set_noamble s 0 then (
      noamble:=true;
      pump ()
    )
    else if Str.string_match add_comp_opt s 0 then (
      let str=Str.matched_group 1 s in
      let opts=getopts str in
      comp_opts := opts@(!comp_opts);
      pump ())
    else if Str.string_match add_package s 0 then (
      packages := (Str.split (Str.regexp ",[ \t]*") (Str.matched_group 1 s)) @ (!packages);
      pump ())

    else if Str.string_match add_dir s 0 then
      (let dirs_ = Str.split (Str.regexp ",[ \t]*") (Str.matched_group 1 s)
       in
       directories := dirs_ ;
       dirs := (compact (dirs_ @ !dirs)) ;
       pump ())

    else if Str.string_match nothing s 0
        || Str.string_match blank s 0
    then pump ()
  in
  seek_in fread 0;
  (try
     pump ()
   with End_of_file -> ());
  seek_in fread 0;
  {
    deps= !deps;
    grammar= !grammar;
    formats= !formats;
    driver= (try List.assoc !driver aliasDriver with Not_found -> !driver);
    comp_opts= !comp_opts;
    packages= !packages ;
    directories = !directories;
    noamble = !noamble
  }

and make_deps pre source=
  Build.sem_down Build.sem;
  let in_s=open_in source in
  let opts=read_options_from_source_file pre in_s in
  close_in in_s;

  let dirs_=str_dirs (!dirs@opts.directories) in

  let dep_filename = chg_ext source (
    if Filename.check_suffix source ".ttml" then ".tdep"
    else if Filename.check_suffix source ".tml" then "_.tdep" else ".dep") in

  let date_dep =
    try (Unix.stat dep_filename).Unix.st_mtime with Unix.Unix_error _ -> -. infinity
  in
  let date_source =
    try (Unix.stat source).Unix.st_mtime with Unix.Unix_error _ -> infinity
  in

  let in_deps, do_dep =
    if !recompile || date_source > date_dep then begin
      let cmd=Printf.sprintf
        "ocamlfind ocamldep %s %s %s %s -ml-synonym .tml -ml-synonym .ttml -ml-synonym .txp -ml-synonym .txp '%s'"
        (let pack=String.concat "," (List.rev opts.packages) in
         if pack<>"" then "-package "^pack else "")
        (if Filename.check_suffix source ".txp" then
          Printf.sprintf "-pp 'pa_patoline --ocamldep %s'" (String.concat " " (List.flatten !extras_pp)) else "")
        (String.concat " " dirs_)
        (includes_opt source)
        source
      in
      Mutex.lock Build.mstdout;
      (match !quiet with
        0 -> ()
      | 1 -> Printf.fprintf stdout "[DEP] %s -> %s\n%!" source dep_filename
      | _ -> Printf.fprintf stdout "%s\n%!" cmd);
      Mutex.unlock Build.mstdout;
      Unix.open_process_in cmd, true
    end else
      open_in dep_filename, false
  in
  let buf=Buffer.create 1000 in
  let s=Bytes.create 1000 in
  let rec read_all ()=
    let i=input in_deps s 0 (String.length s) in
    if i>0 then (
      Buffer.add_substring buf s 0 i;
      read_all ()
    )
  in
  read_all ();
  close_in in_deps;
  let cont = Buffer.contents buf in
  let ldeps =
    let str=Str.regexp ("^"^(Filename.chop_extension source)^"[.]cmx[ \t]*:[ \t]*\\(\\(.\\|[\n]\\)*\\)") in
    try
       let _=Str.search_forward str cont 0 in
       Str.split (Str.regexp "[\n\t\r\\ ]+") (Str.matched_group 1 cont);
     with
       Not_found->[]
  in
  let ldeps = List.map refine_ext ldeps in
  if do_dep then begin
    let ch = open_out dep_filename in
    Printf.fprintf ch "%s.cmx: " (Filename.chop_extension source);
    List.iter (fun s -> Printf.fprintf ch "%s " s) ldeps;
    Printf.fprintf ch "\n%!";
    close_out ch
  end;
  Build.sem_up Build.sem;
  ldeps

and compilation_needed sources targets=
  if !recompile then true else (
    let max_sources=List.fold_left (fun m f->
      if Sys.file_exists f then max m (Unix.stat f).Unix.st_mtime else infinity
    ) (-.infinity) (Sys.executable_name::sources)
    in
    let min_targets=List.fold_left (fun m f->
      if Sys.file_exists f then min m (Unix.stat f).Unix.st_mtime else (-.infinity)
    ) (infinity) targets
    in
    (* Printf.fprintf stderr "\n\nCompilation of [%s] %sneeded %g %g (diff %g)\n\n" *)
    (*   (String.concat ";" targets) *)
    (*   (if max_sources>min_targets then "" else "not ") *)
    (*   max_sources min_targets (min_targets-.max_sources); *)
    (* List.iter (fun f-> *)
    (*   let y=if Sys.file_exists f then (Unix.stat f).Unix.st_mtime else infinity in *)
    (*   Printf.fprintf stderr "Source %S (diff %g)\n" *)
    (*     f *)
    (*     (min_targets-.y) *)
    (* ) sources; *)

    max_sources>min_targets
  )

and check_source r =
  let source = chg_ext r ".txp" in
  if Sys.file_exists source then (".cmx", source) else raise Not_found

and patoline_rule objects (builddir:string) (hs:string list) =
  match hs with
  | []            -> true
  | h::pretargets ->
      Mutex.lock Build.mstdout;
      if !quiet > 1 then Printf.fprintf stdout "checking for %s\n%!" h;
      Mutex.unlock Build.mstdout;
      begin
        if Filename.check_suffix h ".tml" then (
          let r0=chop_extension h in
          let extension, source=check_source r0 in

          if Sys.file_exists source then (
            let in_s=open_in source in
            let opts=read_options_from_source_file (source::hs) in_s in
            close_in in_s;
            (match opts.grammar with
                Some def when def<>"DefaultGrammar" ->
                  Build.build_with_rule (patoline_rule objects) ((chg_ext def ".tgy") :: hs)
              | _->()
            );

            let modu=chg_ext r0 extension in

            List.iter (fun x->
              if x<>h then Build.build_with_rule (patoline_rule objects) (x::hs)
            ) (modu::opts.deps);

            let options_have_changed=
              (not (Sys.file_exists h)) ||
                (
                  let last_format,last_driver=last_options_used h in
                  (last_format<> "" || last_driver<> !driver)
                )
            in
            if options_have_changed || compilation_needed [source] [h] then (
              let o=open_out h in
              let main_mod=Filename.chop_extension (Filename.basename modu) in
              Bytes.set main_mod 0 (Char.uppercase main_mod.[0]);
              write_main_file !dynlink o opts.formats opts.driver "" main_mod (ori_name r0);
              close_out o;
            );
            true
          ) else false
        )
        else if Filename.check_suffix h ".ml" || Filename.check_suffix h ".mli" then (
          Sys.file_exists h;
        )
        else if Filename.check_suffix h ".cmx" || Filename.check_suffix h ".cmi" then (

          let pref=Filename.chop_extension h in
          let is_main = pref.[String.length pref - 1] = '_' in
          let source, prepro=
            if Filename.check_suffix h ".cmi" && Sys.file_exists (chg_ext ~compile:true pref ".mli") then (
              assert (not is_main); chg_ext ~compile:true pref ".mli", [])
            else if Sys.file_exists (chg_ext ~compile:true pref ".ml") then
              (assert (not is_main); chg_ext ~compile:true pref ".ml", [])
            else if not is_main && Sys.file_exists (chg_ext pref ".txp") then (
              let source = chg_ext pref ".txp" in
              let ch = open_in source in
              let opts= add_format (read_options_from_source_file hs ch) in
              let format = match opts.formats with
                  [] -> ""
                | f::_ -> (* FIXME: what to do with multiple formats ? Compose them ? *)
                   "--format " ^ f
              in
              close_in ch;
               source, ["-pp"; "pa_patoline " ^ format ^ " --driver " ^ opts.driver ^ " " ^ (String.concat " " (List.flatten !extras_pp))])
            else
              (chg_ext pref (if is_main then "_.tml" else ".ttml"), [])
          in
          (if prepro = [] then ignore (Build.build_with_rule (patoline_rule objects) (source::hs)));
          let deps0=make_deps hs source in

          let mut,m=objects in
          Mutex.lock mut;
          m:=StrMap.add h deps0 !m;
          Mutex.unlock mut;
          let tar=List.map
            (fun x->Thread.create (Build.build_with_rule (patoline_rule objects)) (x::hs))
            (List.filter (fun x->not (List.mem x hs)) deps0)
          in
          List.iter Thread.join tar;

          let cmis=List.map (fun x-> chg_ext x ".cmi") deps0 in
          if compilation_needed (source::cmis) [chg_ext ~compile:true pref ".cmx";chg_ext ~compile:true pref ".cmi"] then (
            let i=open_in source in
            let opts=
              let opts0=(read_options_from_source_file hs i) in
              add_format (if opts0.formats=[] then { opts0 with formats=["DefaultFormat"] } else opts0)
            in
            let dirs_=str_dirs (!dirs@opts.directories) in
            close_in i;

            let cmd="ocamlfind" in

            let comp_opts=
              List.map (fun x->if x.[0]='"' then String.sub x 1 (String.length x-2) else x) opts.comp_opts
            in
            let args_l=List.filter (fun x->x<>"")
              ([ cmd;
                 !ocamlopt]@prepro@
                  (List.concat (List.rev !extras))@
                  comp_opts@
                  (let pack=String.concat "," (List.rev (opts.packages)) in
                   if pack<>"" then ["-package";pack] else [])@
                  dirs_@includes_opts source@
                  ["-c";"-o";h;
                   if Filename.check_suffix h ".cmi" then "-intf" else "-impl";
                   source ])
            in
            let args=Array.of_list (args_l) in
            Mutex.lock Build.mstdout;
            (match !quiet with
              0 -> ()
            | 1 -> Printf.fprintf stdout "[OPT] %s -> %s\n%!" source h
            | _ ->
              Printf.fprintf stdout "%s\n%!"
                (String.concat " "
                   (List.map (fun x->if String.contains x ' ' then Printf.sprintf "\"%s\"" x else x) args_l)));
            Mutex.unlock Build.mstdout;
            let err=Build.command cmd args in
            if err<>0 then (
              exit err
            )
          );
          true
        )
        else if Filename.check_suffix h ".cmxs" then (
          let raw_h=(Filename.chop_extension h) in
          let source=chg_ext ~compile:true raw_h ".ml"in

          let in_s=open_in source in
          let opts=read_options_from_source_file hs in_s in
          let opts={ opts with packages="patoline"::opts.packages } in
          close_in in_s;
          List.iter (fun x->
            Build.build_with_rule (patoline_rule objects) (x::hs)
          ) opts.deps;
          Build.build_with_rule (patoline_rule objects) (source::hs);

          (* A ce niveau, toutes les dépendances sont indépendantes entre
             elles. On peut les placer sur le même niveau. *)
          let deps0=make_deps hs source in
          let mut,m=objects in
          Mutex.lock mut;
          m:=StrMap.add h deps0 !m;
          Mutex.unlock mut;
          let tar=List.map
            (fun dep->Thread.create (Build.build_with_rule (patoline_rule objects)) (dep::hs))
            deps0
          in
          List.iter Thread.join tar;

          let mut,m=objects in
          Mutex.lock mut;
          let objs=breadth_first m [h] [] in
          if compilation_needed (source::objs) [h] then (
            let dirs_=str_dirs (!dirs@opts.directories) in
            let cmd="ocamlfind" in
            let comp_opts=
              List.map (fun x->if x.[0]='"' then String.sub x 1 (String.length x-2) else x) opts.comp_opts
            in
            let args_l=List.filter (fun x->x<>"")
              ([cmd;
                !ocamlopt]@
                  (List.concat (List.rev !extras))@
                  comp_opts@
                  (let pack=String.concat "," (List.rev (opts.packages)) in
                   if pack<>"" then ["-package";pack] else [])@
                  dirs_@includes_opts source@
                  ["-shared";"-o";h]@
                  (List.filter (fun f->Filename.check_suffix f ".cmx") objs)@
                  ["-impl";source])
            in
            Mutex.unlock mut;
            Mutex.lock Build.mstdout;
            (match !quiet with
              0 -> ()
            | 1 -> Printf.fprintf stdout "[SHR] %s -> %s\n%!" source h
            | _ ->
              Printf.fprintf stdout "%s\n%!"
                (String.concat " "
                   (List.map (fun x->if String.contains x ' ' then Printf.sprintf "\"%s\"" x else x) args_l)));
            Mutex.unlock Build.mstdout;

            let err=Build.command cmd (Array.of_list args_l) in
            if err<>0 then (
              exit err
            )
          );
          true
        ) else if Filename.check_suffix h ".tmx" then (
          let raw_h=Filename.chop_extension (ori_name h) in
          let main_obj= chg_ext raw_h "_.cmx" in
          let source= chg_ext raw_h "_.tml" in
          let source_txp= snd (check_source raw_h) in
          let in_s=open_in source_txp in
          let opts=add_format (read_options_from_source_file hs in_s) in
          close_in in_s;
          List.iter (fun x->
            Build.build_with_rule (patoline_rule objects) (x::source::hs)
          ) opts.deps;
          Build.build_with_rule (patoline_rule objects) (source::hs);

          if not opts.noamble then begin

    (* A ce niveau, toutes les dépendances sont indépendantes entre
       elles. On peut les placer sur le même niveau. *)
            let deps0=make_deps hs source in
            let mut,m=objects in
            Mutex.lock mut;
            m:=StrMap.add h deps0 !m;
            Mutex.unlock mut;
            let tar=List.map
              (fun x->Thread.create (Build.build_with_rule (patoline_rule objects)) (x::hs)) (main_obj::deps0)
            in
            List.iter Thread.join tar;

            let mut,m=objects in
            Mutex.lock mut;
            (* let objs=breadth_first m [h] [] in *)

            let objs =
              let graph = strGraph_of_strMap !m in
              begin match StrGraph.total_order graph with
                      (* On enleve la tete de liste, qui hopefully n'est autre que le .tmx *)
                      h :: deps -> List.rev deps
                    | _ -> []
              end
            in

            if compilation_needed (main_obj::objs) [h] then (
              let dirs_=str_dirs (!dirs@opts.directories) in
              let cmd="ocamlfind" in
              let comp_opts=
                List.map (fun x->if x.[0]='"' then String.sub x 1 (String.length x-2) else x) opts.comp_opts
              in
              let args_l=List.filter (fun x->x<>"")
                ([cmd;
                  !ocamlopt]@
                    (List.concat (List.rev !extras))@
                    comp_opts@
                    (let pack=String.concat ","
                       ((if !dynlink then ["dynlink"] else ["dynlink";"Typography."^opts.driver])@
                           (if patoconfig.has_patonet && not (Filename.check_suffix h ".cmxs") then "cryptokit" else "")::
                           List.rev opts.packages) in
                     if pack<>"" then ["-package";pack] else [])@
                    dirs_@includes_opts source@
                    [(if Filename.check_suffix h ".cmxs" then "-shared" else "-linkpkg");
                     "-o";h]@
                    (List.filter (fun f->Filename.check_suffix f ".cmx") objs)@
                    (if !dynlink then ["-linkall"] else [])@
                    [main_obj])
              in
              Mutex.unlock mut;
              Mutex.lock Build.mstdout;
              (match !quiet with
                0 -> ()
              | 1 -> Printf.fprintf stdout "[LNK] ... %s -> %s\n%!" main_obj h
              | _ ->
                Printf.fprintf stdout "%s\n%!"
                  (String.concat " "
                     (List.map (fun x->if String.contains x ' ' then Printf.sprintf "\"%s\"" x else x) args_l)));
              Mutex.unlock Build.mstdout;
              let err=Build.command cmd (Array.of_list args_l) in
              if err<>0 then (
                exit err
              )
            );
          end;
          true
        )
          else false
      end

(* Gestion de la compilation de plusieurs fichiers patolines différents. *)

let objects=(Mutex.create (), ref StrMap.empty)
let process_each_file l=
  if l=[] then (
    Mutex.lock Build.mstderr;
    Printf.fprintf stderr "%s\n%!" (Language.message No_input_file);
    Mutex.unlock Build.mstderr;
    exit 1
  ) else
    List.iter (fun f->
      if !main_ml then (
        let in_s = open_in f in
        let opts=read_options_from_source_file [f] in_s in
        close_in in_s;
        let main_mod=Filename.chop_extension (Filename.basename f) in
        let o=open_out (chg_ext f "_.tml") in
        write_main_file !dynlink o opts.formats opts.driver "" main_mod (chop_extension f);
        close_out o;
      ) else if !compile then (
        if Sys.file_exists f then (
          let cmd= chg_ext f ".tmx" in
          Build.sem_set Build.sem !Build.j;
          Build.build_with_rule (patoline_rule objects) [cmd];
          if !run && Sys.file_exists cmd then (
            let extras_top =List.concat (List.rev !extras_top) in
            Mutex.lock Build.mstdout;
            Printf.fprintf stdout "[RUN] %s %s\n%!" cmd (String.concat " " extras_top);
            Mutex.unlock Build.mstdout;
            let pid=Unix.create_process
              (if Filename.is_relative cmd then (Filename.concat (Sys.getcwd ()) cmd) else cmd)
              (Array.of_list (List.filter (fun x->x<>"") (cmd:: extras_top)))
              Unix.stdin
              Unix.stdout
              Unix.stderr
            in
            let transmitted = [ Sys.sighup; Sys.sigterm ] in
            let saved = List.map (fun n -> Sys.signal n (Sys.Signal_handle (fun n ->
              Unix.kill pid n))) transmitted in
            let rec fn () =
              try
                let _ = Unix.waitpid [] pid in
                List.iter2 (fun n s -> Sys.set_signal n s) transmitted saved
              with
                Unix.Unix_error(Unix.EINTR,"waitpid",_) -> fn ()
              | e ->
                List.iter2 (fun n s -> Sys.set_signal n s) transmitted saved;
                raise e
            in Unix.handle_unix_error fn ()
          )
        ) else (
          Mutex.lock Build.mstderr;
          Printf.fprintf stderr "%s\n%!" (Language.message (Inexistent_file f));
          Mutex.unlock Build.mstderr;
          exit 1
        )
      ) else (
        if not (Sys.file_exists f) then
          begin
            Mutex.lock Build.mstderr;
            Printf.fprintf stderr "%s\n%!"
              (Language.message (Language.Inexistent_file f));
            Mutex.unlock Build.mstderr;
            exit 1
          end
      )
    ) l

(************************************************)




let _ =
  if Array.length Sys.argv < 2 then (
    Mutex.lock Build.mstderr;
    Printf.fprintf stderr "%s\n%!" (Language.message No_input_file);
    Mutex.unlock Build.mstderr;
  ) else (
    match Sys.argv.(1) with
        "destdir"->(
          if Array.length Sys.argv<3 then (
            Mutex.lock Build.mstderr;
            Printf.fprintf stderr "%s\n%!" (Language.message Usage);
            List.iter (Printf.fprintf stderr "  patoline destdir %s\n")
              ["fonts";"plugins";"grammars";"hyphendir"];
            Mutex.unlock Build.mstderr;
            exit 1
          ) else (
            Mutex.lock Build.mstdout;
            (match Sys.argv.(2) with
                "fonts"->
                  Printf.fprintf stdout "%s\n%!" (fst patoconfig.fonts_dir)
              | "plugins"->
                Printf.fprintf stdout "%s\n%!" (fst patoconfig.plugins_dir)
              | "grammars"->
                Printf.fprintf stdout "%s\n%!" (fst patoconfig.grammars_dir)
              | "hyphens"->
                Printf.fprintf stdout "%s\n%!" (fst patoconfig.hyphen_dir)
              | _->(
                Mutex.unlock Build.mstdout;
                Mutex.lock Build.mstderr;
                Printf.fprintf stderr "%s\n%!" (Language.message (Unknown_command Sys.argv.(2)));
                Mutex.unlock Build.mstderr;
                exit 1
              )
            );
            Mutex.unlock Build.mstdout;
          );

        )
      | _->(
        Arg.parse spec (fun x->files := x::(!files)) (Language.message Usage);
        process_each_file (List.rev !files)
      )
  )
