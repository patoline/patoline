let amble=ref Generateur.Main
let files=ref []
let compile = ref true
let run = ref true
let cmd_line = ref []
let no_grammar=ref false
let deps_only=ref false
let extras = ref ""
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
   ("--no-grammar",Arg.Unit (fun ()->Config.grammarspath:=[]), message (Cli No_grammar));
   ("--format",Arg.String
     (fun f ->format := Filename.basename f; cmd_line_format := true), message (Cli Format));
   ("--driver",Arg.String
     (fun f ->driver := f; cmd_line_driver := true), message (Cli Driver));
   ("-c",Arg.Unit (fun ()->amble:=Generateur.Separate), message (Cli Separately));
   ("--noamble",Arg.Unit (fun ()->amble:=Generateur.Noamble), message (Cli Noamble));
   ("-package",Arg.String (fun s-> package_list:= s::(!package_list)),
    message (Cli Package));
   ("--ml",Arg.Unit (fun () -> compile:=false; run:= false), message (Cli Ml));
   ("--bin",Arg.Unit (fun () -> compile:=true; run:= false), message (Cli Bin));
   ("--edit-link", Arg.Unit (fun () -> Generateur.edit_link:=true), message (Cli Edit_link));
   ("--patoline",Arg.String (fun s->patoline:=s), message (Cli Patoline));
   ("--ocamlopt",Arg.String (fun s->ocamlopt:=s), message (Cli Ocamlopt));
   ("-j",Arg.Int (fun s->Build.j:=max !Build.j s), message (Cli Parallel));
   ("--quiet", Arg.Unit (fun ()->quiet:=true), message (Cli Quiet));
   ("--",Arg.Rest (fun s -> extras := !extras ^ " " ^s), message (Cli Remaining));

   ("--no-line-directive", Arg.Unit (fun () -> Generateur.line_directive:=false), message (Cli No_line_directive))
  ]


let mlname_of f = (Filename.chop_extension f)^(if !amble<>Generateur.Main then ".ttml" else ".tml")
let binname_of f = (Filename.chop_extension f)^".tmx"
let execname_of f = if Filename.is_implicit f then "./"^(binname_of f) else (binname_of f)

(************************************************)
open Util
let dynlinked=(Mutex.create (), ref StrMap.empty)

type options={
  deps:string list;
  format:string;
  driver:string;
  comp_opts:string list;
  packages:string list;
  directories:string list
}

let str_dirs opts = (String.concat " " (List.map (fun dir -> ("-I " ^ dir)) (!dirs (* @ opts.directories *))))
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
  if opts.format <> "DefaultFormat" &&
    (not (List.mem ("Typography." ^ opts.format) opts.packages)) &&
    (try
       let _=findPath (opts.format ^ ".ml") (".":: !Config.local_path) in
       false
     with _->true)
  then
    { opts with packages = ("Typography." ^ opts.format) :: opts.packages }
  else opts 


let rec read_options_from_source_file fread =
  let deps=ref [] in
  let format=ref !format in
  let driver=ref !driver in
  let comp_opts=ref [] in
  let packages=ref !package_list in
  let directories = ref [] in

  let nothing = Str.regexp "^[ \t]*\\((\\*.*\\*)\\)?[ \t\r]*$" in
  let set_format = Str.regexp "^[ \t]*(\\*[ \t]*#FORMAT[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let set_driver = Str.regexp "^[ \t]*(\\*[ \t]*#DRIVER[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
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
        Dynlink.loadfile name;
      );
      Mutex.unlock m;
      pump ()
    )
    else if Str.string_match depends s 0 then (
      deps := Str.matched_group 1 s:: !deps;
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
    else if Str.string_match add_compilation_option s 0 then (comp_opts := (Str.matched_group 1 s)::(!comp_opts); pump ())
    else if Str.string_match add_package s 0 then (packages := (Str.split (Str.regexp ",[ \t]*") (Str.matched_group 1 s)) @ (!packages); pump ())
    else if Str.string_match add_directories s 0 then 
      (let dirs_ = Str.split (Str.regexp ",[ \t]*") (Str.matched_group 1 s)
       in 
       directories := dirs_ ; 
       dirs := (compact (dirs_ @ !dirs)) ;
       pump ())
    else if Str.string_match nothing s 0 then pump ()
  in
  seek_in fread 0;
  (try
     pump ()
   with End_of_file -> ());
  seek_in fread 0;
  {
    deps= !deps;
    format= !format;
    driver= !driver;
    comp_opts= !comp_opts;
    packages= !packages ;
    directories = !directories
  }
and make_deps source=
  let ldeps=ref [] in
  Build.sem_down Build.sem;
  let in_s=open_in source in
  let opts=read_options_from_source_file in_s in
  close_in in_s;

  let dirs_=str_dirs opts in
  let cmd=Printf.sprintf
    "ocamlfind ocamldep %s %s -ml-synonym .tml -ml-synonym .ttml -ml-synonym .txp '%s'"
    (let pack=String.concat "," (List.rev opts.packages) in
     if pack<>"" then "-package "^pack else "")
    dirs_
    source
  in
  if not !quiet then (Printf.fprintf stdout "%s\n" cmd;flush stdout);
  let in_deps=Unix.open_process_in cmd in
  let buf=Buffer.create 100 in
  let s=String.create 100 in
  (try
     while true do
       let i=input in_deps s 0 (String.length s) in
       Buffer.add_substring buf s 0 i;
       if i<String.length s then raise End_of_file
     done
   with
       End_of_file->());
  let cont=Buffer.contents buf in
  let str=Str.regexp ("^"^(Filename.chop_extension source)^".cmx:\\(\\(.*\\\n\\)*\\(.*$\\)\\)") in
  (try
     let _=Str.search_forward str cont 0 in
     ldeps:=(Str.split (Str.regexp "[\n\t\r\\ ]+") (Str.matched_group 1 cont))@(!ldeps);
   with
       Not_found->());
  close_in in_deps;
  Build.sem_up Build.sem;
  !ldeps


and compilation_needed sources targets=
  if !recompile then true else (
    let max_sources=List.fold_left (fun m f->
      if Sys.file_exists f then max m (Unix.stat f).Unix.st_mtime else infinity
    ) (-.infinity) sources
    in
    let min_targets=List.fold_left (fun m f->
      if Sys.file_exists f then min m (Unix.stat f).Unix.st_mtime else (-.infinity)
    ) (infinity) targets
    in
    max_sources>=min_targets
  )

and patoline_rule objects h=
  if Filename.check_suffix h ".ttml" || Filename.check_suffix h ".tml" then
    (
      let source=(Filename.chop_extension h)^".txp" in
      let in_s=open_in source in
      let opts=read_options_from_source_file in_s in
      close_in in_s;
      List.iter Build.build opts.deps;

      let options_have_changed=
        if Sys.file_exists h then (
          let last_format,last_driver=last_options_used h in
          (last_format<> !format || last_driver<> !driver)
        ) else false
      in
      if options_have_changed || compilation_needed [source;Sys.argv.(0)] [h] then (
	let dirs_=str_dirs opts in
        let cmd=Printf.sprintf "%s %s --ml%s%s%s --driver %s%s '%s'"
          !patoline
          dirs_
          (if opts.format<>"DefaultFormat" then " --format " else "")
          (if opts.format<>"DefaultFormat" then opts.format else "")
	  (if !Generateur.edit_link then " --edit-link" else "")
	  opts.driver
          (if Filename.check_suffix h ".ttml" then " -c" else "")
          source
        in
        if not !quiet then (Printf.fprintf stdout "%s\n" cmd;flush stdout);
        let err=Build.command cmd in
        if err<>0 then (
          failwith ("error : "^h)
        )
      );
      true
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
    if compilation_needed (source::cmis) [h;pref^".cmi"] then (
      let i=open_in source in
      let opts= add_format (read_options_from_source_file i) in
      let dirs_=str_dirs opts in
      close_in i;
      let cmd=Printf.sprintf "ocamlfind %s %s %s -c -o '%s' -impl '%s'"
        !ocamlopt
        (let pack=String.concat "," (List.rev opts.packages) in
         if pack<>"" then "-package "^pack else "")
        dirs_
        h
        source
      in

      if not !quiet then (Printf.fprintf stdout "%s\n" cmd;flush stdout);
      let err=Build.command cmd in
      if err<>0 then (
        failwith ("error : "^h)
      )
    );
    true
  )
  else if Filename.check_suffix h ".tmx" || Filename.check_suffix h ".cmxs" then (
    let raw_h=(Filename.chop_extension h) in
    let source=if Filename.check_suffix h ".tmx" then raw_h^".tml" else raw_h^".ml"in
    let source_txp=if Filename.check_suffix h ".tmx" then raw_h^".txp" else source in
    Build.build source;

    let in_s=open_in source_txp in
    let opts=add_format (read_options_from_source_file in_s) in
    close_in in_s;
    List.iter Build.build opts.deps;

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
      let dirs_=str_dirs opts in
      let cmd=Printf.sprintf "ocamlfind %s %s %s %s -linkpkg -o '%s' %s -impl '%s'"
        !ocamlopt
        (let pack=String.concat "," (List.rev opts.packages) in
         if pack<>"" then "-package "^pack else "")
        dirs_
        (if Filename.check_suffix h ".cmxs" then "-shared" else "")
        h
        (String.concat " " objs)
        source
      in
      Mutex.unlock mut;

      if not !quiet then (Printf.fprintf stdout "%s\n" cmd;flush stdout);
      let err=Build.command cmd in
      if err<>0 then (
        failwith ("error : "^h)
      )
    );
  true
  )
  else false


(* Gestion de la compilation de plusieurs fichiers patolines différents. *)

and process_each_file l=
  if l=[] then
    Printf.fprintf stderr "%s\n" (Language.message No_input_file)
  else
    List.iter (fun f->
      if !compile then (
        package_list := ("Typography."^ !driver):: !package_list;

        let cmd= (Filename.concat
                    (Sys.getcwd ()) ((Filename.chop_extension f)^".tmx")) in
        Build.sem_set Build.sem !Build.j;
        Build.build cmd;
        if !run then (
          Printf.fprintf stdout "'%s'\n" cmd;flush stdout;
          let _=Build.command ("'"^cmd^"'") in
          ()
        )
      ) else (
        let where_ml = open_out (mlname_of f) in
        let fread = open_in f in
        Parser.out_grammar_name:=Filename.chop_extension f;
        if Filename.check_suffix f "txt" then (
          SimpleGenerateur.gen_ml !format SimpleGenerateur.Main f fread (mlname_of f) where_ml (Filename.chop_extension f)
        ) else (
          let opts=read_options_from_source_file fread in
          Parser.fprint_caml_buf :=
            (fun ld gr buf s e txps opos ->
              let pos = pos_in fread in
              Generateur.print_caml_buf (Parser.pp ()) ld gr (Generateur.Source.of_in_channel fread) buf s e txps opos;
              seek_in fread pos);
          let suppl=Printf.sprintf "(* #PACKAGES %s *)%s\n"
	    (String.concat "," opts.packages)
	    (if opts.directories <> [] then
		(Printf.sprintf "\n(* #DIRECTORIES %s *)" (String.concat "," opts.directories))
	     else "")
          in
          Generateur.gen_ml opts.format opts.driver suppl !amble f fread (mlname_of f) where_ml (Filename.chop_extension f)
        );
        close_out where_ml;
        close_in fread;
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
