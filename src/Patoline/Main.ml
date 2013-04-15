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
(* let amble=ref Generateur.Main *)
let output=ref ""
let files=ref []
let compile = ref true
let run = ref true
let cmd_line = ref []
let no_grammar=ref false
let deps_only=ref false
let extras = ref []
let extras_top = ref []
let cmd_line_format = ref false
let cmd_line_driver= ref false
let format=ref "DefaultFormat"
let driver = ref "Pdf"
let dirs = ref []
let package_list = ref ["Typography"]
let patoline=ref (Sys.argv.(0))
let ocamlopt=ref "ocamlopt"
let recompile=ref false
let quiet=ref false
open Language
let spec =
  [("--extra-fonts-dir",Arg.String (fun x->cmd_line:=("--extra-fonts-dir "^x)::(!cmd_line)),
    message (Cli Extra_fonts));
   ("--extra-hyph-dir",Arg.String (fun x->cmd_line:=("--extra-hyph-dir "^x)::(!cmd_line)),
    message (Cli Extra_hyph));

   ("-I",Arg.String (fun x->
     Config.local_path:=x::(!Config.local_path);
     Config.grammarspath:=x::(!Config.grammarspath);
     dirs := x :: !dirs),
    message (Cli Dirs));
   ("--recompile",Arg.Unit(fun ()->recompile:=true),message (Cli Recompile));
   ("--no-grammar",Arg.Unit (fun ()->Parser.grammar:=None), message (Cli No_grammar));
   ("--format",Arg.String
     (fun f ->format := Filename.basename f; cmd_line_format := true), message (Cli Format));
   ("--driver",Arg.String
     (fun f ->driver := f; cmd_line_driver := true), message (Cli Driver));
   (*
   ("-c",Arg.Unit (fun ()->
     compile:=false;run:= false;
     if !amble<> Generateur.Noamble then
       amble:=Generateur.Separate
    ), message (Cli Separately));
   ("--noamble",Arg.Unit (fun ()->amble:=Generateur.Noamble), message (Cli Noamble));
   *)
   ("-package",Arg.String (fun s-> package_list:= s::(!package_list)),
    message (Cli Package));
   ("--ml",Arg.Unit (fun () -> compile:=false; run:= false), message (Cli Ml));
   ("-o",Arg.Set_string output, message (Cli Output));
   ("--bin",Arg.Unit (fun () -> compile:=true; run:= false), message (Cli Bin));
   ("--edit-link", Arg.Unit (fun () -> Generateur.edit_link:=true), message (Cli Edit_link));
   ("--patoline",Arg.String (fun s->patoline:=s), message (Cli Patoline));
   ("--ocamlopt",Arg.String (fun s->ocamlopt:=s), message (Cli Ocamlopt));
   ("-j",Arg.Int (fun s->Build.j:=max !Build.j s), message (Cli Parallel));
   ("--quiet", Arg.Unit (fun ()->quiet:=true), message (Cli Quiet));
   ("--",Arg.Rest (fun s -> extras := s:: !extras), message (Cli Remaining));
   ("--topopts",Arg.String (fun s -> extras_top := s:: !extras_top), message (Cli TopOpts));
   ("--ccopts",Arg.String (fun s -> extras := s:: !extras), message (Cli CCOpts));
   ("--no-line-directive", Arg.Unit (fun () -> Generateur.line_directive:=false), message (Cli No_line_directive));
   ("--debug-parser", Arg.Unit (fun () -> Parser.debug_parser:=true), message (Cli Debug_parser));
  ]


let mlname_of f = if !output="" then (Filename.chop_extension f)^".tml" else !output
let binname_of f = (Filename.chop_extension f)^".tmx"
let execname_of f = if Filename.is_implicit f then "./"^(binname_of f) else (binname_of f)

(************************************************)
open Util
let dynlinked=(Mutex.create (), ref StrMap.empty)

type options={
  deps:string list;
  format:string;
  grammar:string option;
  driver:string;
  comp_opts:string list;
  packages:string list;
  directories:string list;
  noamble:bool
}

let str_dirs optsdirs =
  let rec make_dirs l res=match l with
      []->List.rev res
    | h::s->make_dirs s (h::"-I"::res)
  in
  make_dirs optsdirs []

let getopts str=
  let restr="\\(\"\\(\\\"\\|[^\"]\\)*\"\\)\\|\\([^ \t\n\"]+\\)" in
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

module StringSet = Set.Make(struct type t = string let compare = compare end)
let compact l =
  let s = List.fold_right StringSet.add l StringSet.empty in
  StringSet.elements s


let last_options_used file=
  let fread=open_in file in
  let _format=ref "" in
  let _driver=ref "" in
  let set_format = Str.regexp "^[ \t]*(\\*[ \t]*#FORMAT[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let set_driver = Str.regexp "^[ \t]*(\\*[ \t]*#DRIVER[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let rec pump () =
    let s = input_line fread in
    if Str.string_match set_format s 0 then (
      _format:=Str.matched_group 1 s;
      if String.length !_driver=0 then pump ()
    )
    else if Str.string_match set_driver s 0 then (
      _driver:=Str.matched_group 1 s;
      if String.length !_format=0 then pump ()
    )
    else pump ()
  in
  (try
     pump ()
   with End_of_file -> ());
  close_in fread;
  !_format, !_driver



let add_format opts =
  let packages=
    (if opts.format <> "DefaultFormat" &&
        (not (List.mem ("Typography." ^ opts.format) opts.packages)) &&
        (try
           let _=findPath (opts.format ^ ".ml") (".":: !Config.local_path) in
           false
         with _->true)
     then
        ("Typography." ^ opts.format) :: opts.packages
     else opts.packages)
  in
  let packages=
    (if (not (List.mem ("Typography." ^ opts.driver) opts.packages)) &&
        (try
           let _=findPath (opts.driver ^ ".ml") (".":: !Config.local_path) in
           false
         with _->true)
     then
        ("Typography." ^ opts.driver) :: packages
     else packages)
  in
  { opts with packages = packages }


let rec read_options_from_source_file fread =
  let deps=ref [] in
  let format=ref !format in
  let grammar=ref (Some "DefaultGrammar") in
  let driver=ref !driver in
  let noamble=ref false in
  let comp_opts=ref [] in
  let packages=ref !package_list in
  let directories = ref [] in

  let nothing = Str.regexp "^[ \t]*\\((\\*.*\\*)\\)?[ \t\r]*$" in
  let blank=Str.regexp "^[ \t]*$" in
  let set_format = Str.regexp "^[ \t]*(\\*[ \t]*#FORMAT[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let set_driver = Str.regexp "^[ \t]*(\\*[ \t]*#DRIVER[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let set_grammar = Str.regexp "^[ \t]*(\\*[ \t]*#GRAMMAR[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let set_noamble = Str.regexp "^[ \t]*(\\*[ \t]*#NOAMBLE[ \t]*\\*)[ \t\r]*$" in
  let link = Str.regexp "^[ \t]*(\\*[ \t]*#LINK[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let depends = Str.regexp "^[ \t]*(\\*[ \t]*#DEPENDS[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let add_compilation_option = Str.regexp "^[ \t]*(\\*[ \t]*#COMPILATION[ \t]+\\(.+\\)[ \t]*\\*)[ \t\r]*$" in
  let add_package = Str.regexp "^[ \t]*(\\*[ \t]*#PACKAGES[ \t]+\\([^ \t]+\\(,[ \t]*[^ \t]+\\)*\\)[ \t]*\\*)[ \t\r]*$" in
  let add_directories = Str.regexp "^[ \t]*(\\*[ \t]*#DIRECTORIES[ \t]+\\([^ \t]+\\(,[ \t]*[^ \t]+\\)*\\)[ \t]*\\*)[ \t\r]*$" in
  let rec pump () =
    let s = input_line fread in
    if Str.string_match link s 0 then (
      let n=
        let n=Str.matched_group 1 s in
        (try Filename.chop_extension n with _->n)^".cmo"
      in
      let name=
        try
          let name=Util.findPath ((Filename.chop_extension n)^".ml") ("."::!Config.local_path) in
          let objects=(Mutex.create (), ref StrMap.empty) in
          Build.build_with_rule (patoline_rule objects) (Dynlink.adapt_filename (Filename.chop_extension name^".cmo"));
          Dynlink.adapt_filename (Filename.chop_extension name^".cmo")
        with
            File_not_found _->Util.findPath
              (Dynlink.adapt_filename (Filename.chop_extension n^".cmo"))
              !Config.pluginspath
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
      if not !cmd_line_format then format := Str.matched_group 1 s;
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
    else if Str.string_match add_compilation_option s 0 then (
      let str=Str.matched_group 1 s in
      let opts=getopts str in
      comp_opts := opts@(!comp_opts);
      pump ())
    else if Str.string_match add_package s 0 then (
      packages := (Str.split (Str.regexp ",[ \t]*") (Str.matched_group 1 s)) @ (!packages);
      pump ())

    else if Str.string_match add_directories s 0 then
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
    format= !format;
    driver= !driver;
    comp_opts= !comp_opts;
    packages= !packages ;
    directories = !directories;
    noamble = !noamble
  }

and make_deps source=
  let ldeps=ref [] in
  Build.sem_down Build.sem;
  let in_s=open_in source in
  let opts=read_options_from_source_file in_s in
  close_in in_s;

  let dirs_=str_dirs (!dirs@opts.directories) in

  let dep_filename = (Filename.chop_extension source) ^ if Filename.check_suffix source ".ttml" then ".tdep" else ".dep" in

  let date_dep =
    try (Unix.stat dep_filename).Unix.st_mtime with Unix.Unix_error _ -> -. infinity
  in
  let date_source =
    try (Unix.stat source).Unix.st_mtime with Unix.Unix_error _ -> infinity
  in

  let in_deps, do_dep =
    if !recompile || date_source > date_dep then begin
      let cmd=Printf.sprintf
	"ocamlfind ocamldep %s %s -ml-synonym .tml -ml-synonym .ttml -ml-synonym .txp '%s'"
	(let pack=String.concat "," (List.rev opts.packages) in
	 if pack<>"" then "-package "^pack else "")
	(String.concat " " dirs_)
	source
      in
      if not !quiet then (Printf.fprintf stdout "%s\n" cmd;flush stdout);
      Unix.open_process_in cmd, true
    end else 
      open_in dep_filename, false
  in

  let buf=Buffer.create 1000 in
  let s=String.create 1000 in
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
  if do_dep then begin
    let ch = open_out dep_filename in
    output_string ch cont;
    close_out ch
  end;

  let str=Str.regexp ("^"^(Filename.chop_extension source)^".cmx[ \t]*:[ \t]*\\(\\(.*\\\n\\)*\\)$") in
  (try
     let _=Str.search_forward str cont 0 in
     ldeps:=(Str.split (Str.regexp "[\n\t\r\\ ]+") (Str.matched_group 1 cont))@(!ldeps);
   with
       Not_found->());
  Build.sem_up Build.sem;
  !ldeps


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

and patoline_rule objects h=
  if Filename.check_suffix h ".ttml" || Filename.check_suffix h Parser.gram_ext then
    (
      let source=
        let r=Filename.chop_extension h in
        let source=(r^".txp") in
        if Sys.file_exists source then
          source
        else (
          if r.[String.length r-1]='_' then
            String.sub r 0 (String.length r-1)^".txp"
          else
            r
        )
      in
      if Sys.file_exists source then (
        let in_s=open_in source in
        let opts=read_options_from_source_file in_s in
        close_in in_s;

        (match opts.grammar with
            Some def when def<>"DefaultGrammar"->
              Build.build (def^Parser.gram_ext)
          | _->()
        );

        List.iter (fun x->
          if x<>h then Build.build x
        ) opts.deps;

        let options_have_changed=not (Sys.file_exists h)
        in
        if options_have_changed || compilation_needed [source] [h] then (
	  let dirs_=str_dirs (!dirs@opts.directories) in
          let cmd= !patoline in
          let args_l=List.filter (fun x->x<>"")
            (cmd::
               dirs_@
               [(if opts.format<>"DefaultFormat" then "--format" else "");
                (if opts.format<>"DefaultFormat" then opts.format else "");
                "--ml";
                (if opts.grammar=None then "--no-grammar" else "");
	        (if !Generateur.edit_link then "--edit-link" else "");
	        "--driver";opts.driver;
	        "-o";h;
                source]
            )
          in
          if not !quiet then (Printf.fprintf stdout "%s\n"
                                (String.concat " "
                                   (List.map (fun x->if String.contains x ' ' then Printf.sprintf "\"%s\"" x else x) args_l));
                              flush stdout);
          let err=Build.command cmd (Array.of_list args_l) in
          if err<>0 then (
            exit err
          )
        );
        true
      ) else (
        Filename.check_suffix h Parser.gram_ext
      )
    )
  else if Filename.check_suffix h ".tml" then (
    let r0=(Filename.chop_extension h) in
    let r1=String.sub r0 0 (String.length r0-1) in
    let source=r1^".txp" in
    if Sys.file_exists source then (
      let in_s=open_in source in
      let opts=read_options_from_source_file in_s in
      close_in in_s;
      (match opts.grammar with
          Some def when def<>"DefaultGrammar"->
            Build.build (def^Parser.gram_ext)
        | _->()
      );

      let modu=Printf.sprintf "%s.ttml" r1 in

      List.iter (fun x->
        if x<>h then Build.build x
      ) (modu::opts.deps);

      let options_have_changed=
        (not (Sys.file_exists h)) ||
          (
            let last_format,last_driver=last_options_used h in
            (last_format<> !format || last_driver<> !driver)
          )
      in
      if options_have_changed || compilation_needed [source] [h] then (
        let o=open_out h in
        let main_mod=Filename.chop_extension (Filename.basename modu) in
        main_mod.[0]<-Char.uppercase main_mod.[0];
        Generateur.write_main_file o opts.format opts.driver "" main_mod r1;
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
    let source=
      if Filename.check_suffix h ".cmi" && Sys.file_exists (pref^".mli") then pref^".mli" else
        if Sys.file_exists (pref^".ml") then (pref^".ml") else
          pref^".ttml"
    in
    Build.build source;

    let deps0=make_deps source in

    let mut,m=objects in
    Mutex.lock mut;
    m:=StrMap.add h deps0 !m;
    Mutex.unlock mut;

    let tar=List.map (Thread.create Build.build) deps0 in
    List.iter (Thread.join) tar;
    let cmis=List.map (fun x->Filename.chop_extension x^".cmi") deps0 in
    if compilation_needed (source::cmis) [pref^".cmx";pref^".cmi"] then (
      let i=open_in source in
      let opts= add_format (read_options_from_source_file i) in
      let dirs_=str_dirs (!dirs@opts.directories) in
      close_in i;

      let cmd="ocamlfind" in

      let comp_opts=
        List.map (fun x->if x.[0]='"' then String.sub x 1 (String.length x-2) else x) opts.comp_opts
      in
      let args_l=List.filter (fun x->x<>"")
        ([ cmd;
           !ocamlopt]@
            (List.concat (List.map getopts !extras))@
            comp_opts@
            (let pack=String.concat "," (List.rev (opts.packages)) in
             if pack<>"" then ["-package";pack] else [])@
            dirs_@
            ["-c";"-o";h;
             "-impl";source ])
      in
      let args=Array.of_list (args_l) in

      if not !quiet then (Printf.fprintf stdout "%s\n"
                              (String.concat " "
                                 (List.map (fun x->if String.contains x ' ' then Printf.sprintf "\"%s\"" x else x) args_l));

                          flush stdout);
      let err=Build.command cmd args in
      if err<>0 then (
        exit err
      )
    );
    true
  )
  else if Filename.check_suffix h ".cmxs" then (
    let raw_h=(Filename.chop_extension h) in
    let source=raw_h^".ml"in

    let in_s=open_in source in
    let opts=read_options_from_source_file in_s in
    let opts={ opts with packages="patoline"::opts.packages } in
    close_in in_s;
    List.iter (fun x->
      Build.build x
    ) opts.deps;
    Build.build source;

    (* A ce niveau, toutes les dépendances sont indépendantes entre
       elles. On peut les placer sur le même niveau. *)
    let deps0=make_deps source in
    let mut,m=objects in
    Mutex.lock mut;
    m:=StrMap.singleton h deps0;
    Mutex.unlock mut;
    let tar=List.map (Thread.create Build.build) deps0 in
    List.iter (Thread.join) tar;

    let mut,m=objects in
    Mutex.lock mut;

    let rec breadth_first h l=match h with
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
        breadth_first h1 l1
    in
    let objs=breadth_first [h] [] in
    if compilation_needed (source::objs) [h] then (
      let dirs_=str_dirs (!dirs@opts.directories) in
      let cmd="ocamlfind" in
      let comp_opts=
        List.map (fun x->if x.[0]='"' then String.sub x 1 (String.length x-2) else x) opts.comp_opts
      in
      let args_l=List.filter (fun x->x<>"")
        ([cmd;
          !ocamlopt]@
            (List.concat (List.map getopts !extras))@
            comp_opts@
            (let pack=String.concat "," (List.rev (opts.packages)) in
             if pack<>"" then ["-package";pack] else [])@
            dirs_@
            [(if Filename.check_suffix h ".cmxs" then "-shared" else "-linkpkg");
             "-o";h]@
            objs@
            ["-impl";source])
      in
      Mutex.unlock mut;

      if not !quiet then (Printf.fprintf stdout "%s\n"
                            (String.concat " "
                               (List.map (fun x->if String.contains x ' ' then Printf.sprintf "\"%s\"" x else x) args_l));
                          flush stdout);
      let err=Build.command cmd (Array.of_list args_l) in
      if err<>0 then (
        exit err
      )
    );
    true
  ) else if Filename.check_suffix h ".tmx" then (
    let raw_h=(Filename.chop_extension h) in
    let source=if Filename.check_suffix h ".tmx" then raw_h^"_.tml" else raw_h^".ml"in
    let source_txp=if Filename.check_suffix h ".tmx" then raw_h^".txp" else source in

    let in_s=open_in source_txp in
    let opts=add_format (read_options_from_source_file in_s) in
    close_in in_s;
    List.iter (fun x->
      Build.build x
    ) opts.deps;
    Build.build source;

    if not opts.noamble then begin

    (* A ce niveau, toutes les dépendances sont indépendantes entre
       elles. On peut les placer sur le même niveau. *)
    let deps0=make_deps source in
    let mut,m=objects in
    Mutex.lock mut;
    m:=StrMap.singleton h deps0;
    Mutex.unlock mut;
    let tar=List.map (Thread.create Build.build) deps0 in
    List.iter (Thread.join) tar;

    let mut,m=objects in
    Mutex.lock mut;

    let rec breadth_first h l=match h with
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
        breadth_first h1 l1
    in
    let objs=breadth_first [h] [] in
    if compilation_needed (source::objs) [h] then (
      let dirs_=str_dirs (!dirs@opts.directories) in
      let cmd="ocamlfind" in
      let comp_opts=
        List.map (fun x->if x.[0]='"' then String.sub x 1 (String.length x-2) else x) opts.comp_opts
      in
      let args_l=List.filter (fun x->x<>"")
        ([cmd;
          !ocamlopt]@
            (List.concat (List.map getopts !extras))@
            comp_opts@
            (let pack=String.concat "," (List.rev (opts.packages)) in
             if pack<>"" then ["-package";pack] else [])@
            dirs_@
            [(if Filename.check_suffix h ".cmxs" then "-shared" else "-linkpkg");
             "-o";h]@
            objs@
            ["-impl";source])
      in
      Mutex.unlock mut;

      if not !quiet then (Printf.fprintf stdout "%s\n"
                              (String.concat " "
                                 (List.map (fun x->if String.contains x ' ' then Printf.sprintf "\"%s\"" x else x) args_l));
                          flush stdout);
      let err=Build.command cmd (Array.of_list args_l) in
      if err<>0 then (
        exit err
      )
    );
    end;
    true
  )
  else false


(* Gestion de la compilation de plusieurs fichiers patolines différents. *)

and process_each_file l=
  if l=[] then (
    Printf.fprintf stderr "%s\n" (Language.message No_input_file);
    exit 1
  ) else
    List.iter (fun f->
      if !compile then (
        if Sys.file_exists f then (
          let cmd=(Filename.chop_extension f)^".tmx" in
          Build.sem_set Build.sem !Build.j;
          Build.build cmd;
          if !run && Sys.file_exists cmd then (
            extras_top:=List.rev !extras_top;
            Printf.fprintf stdout "%s %s\n" cmd (String.concat " " !extras_top);flush stdout;
            let pid=Unix.create_process
              (Filename.concat (Sys.getcwd ()) cmd)
              (Array.of_list (List.filter (fun x->x<>"") (cmd:: !extras_top)))
              Unix.stdin
              Unix.stdout
              Unix.stderr
            in
            let _=Unix.waitpid [] pid in
            ()
          )
        ) else (
          Printf.fprintf stderr "%s\n" (Language.message (Inexistent_file f));
          exit 1
        )
      ) else (
        if Sys.file_exists f then (
          let fread = open_in f in
          let where_ml = open_out (mlname_of f) in
          Parser.out_grammar_name:=Filename.chop_extension f;
          if Filename.check_suffix f "txt" then (
            SimpleGenerateur.gen_ml !format SimpleGenerateur.Main f fread (mlname_of f) where_ml (Filename.chop_extension f)
          ) else (

            let opts=read_options_from_source_file fread in
            Parser.grammar:=opts.grammar;
            (match opts.grammar with
                Some def when def<>"DefaultGrammar"->(
                  Build.build (def^Parser.gram_ext)
                )
              | _->()
            );
            List.iter (fun x->
              if x<>f then Build.build x
            ) opts.deps;

            let suppl=
              Printf.sprintf "(* #PACKAGES %s *)%s%s%s\n"
	        (String.concat "," opts.packages)
	        (if opts.directories <> [] then
		    (Printf.sprintf "\n(* #DIRECTORIES %s *)" (String.concat "," opts.directories))
	         else "")
	        (if opts.comp_opts <> [] then
		    (Printf.sprintf "\n(* #COMPILATION %s *)" (String.concat " " opts.comp_opts))
	         else "")
	        (if opts.noamble then
		    Printf.sprintf "\n(* #NOAMBLE *)"
	         else "")
            in
            Generateur.gen_ml opts.format opts.driver suppl f fread (mlname_of f) where_ml
              (Filename.chop_extension f)
          );
          close_out where_ml;
          close_in fread;
        ) else (
          Printf.fprintf stderr "%s\n"
            (Language.message (Language.Inexistent_file f));
          exit 1
        )
      )
    ) l

(************************************************)




let _ =
  if Array.length Sys.argv < 2 then (
    Printf.fprintf stderr "%s\n" (Language.message No_input_file);flush stderr;
  ) else (
    match Sys.argv.(1) with
        "destdir"->(
          if Array.length Sys.argv<3 then (
            Printf.fprintf stderr "%s\n" (Language.message Usage);
            List.iter (Printf.fprintf stderr "  patoline destdir %s\n")
              ["fonts";"plugins";"grammars";"hyphendir"];flush stderr;
            exit 1
          ) else (
            match Sys.argv.(2) with
                "fonts"->
                  Printf.fprintf stdout "%s\n" Config.fontsdir
              | "plugins"->
                Printf.fprintf stdout "%s\n" Config.pluginsdir
              | "grammars"->
                Printf.fprintf stdout "%s\n" Config.grammarsdir
              | "hyphens"->
                Printf.fprintf stdout "%s\n" Config.hyphendir
              | _->(
                Printf.fprintf stderr "%s\n" (Language.message (Unknown_command Sys.argv.(2)));
                exit 1
              )
          )
        )
      | _->(
        let objects=(Mutex.create (), ref StrMap.empty) in
        Build.append_rule (patoline_rule objects);
        Arg.parse spec (fun x->files := x::(!files)) (Language.message Usage);
        process_each_file (List.rev !files)
      )
  )
