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

let quiet=ref false

let spec =
  [("--extra-fonts-dir",Arg.String (fun x->cmd_line:=("--extra-fonts-dir "^x)::(!cmd_line)), "Adds directories to the font search path");
   ("--extra-hyph-dir",Arg.String (fun x->cmd_line:=("--extra-hyph-dir "^x)::(!cmd_line)), "Adds directories to the hyphenation search path");

   ("-I",Arg.String (fun x->
     Config.local_path:=x::(!Config.local_path);
     Config.grammarspath:=x::(!Config.grammarspath);
     dirs := ("-I " ^ x) :: !dirs),
    "Adds directories to the search path");

   ("--no-grammar",Arg.Unit (fun ()->Config.grammarspath:=[]), "Empty grammar search path");
   ("--format",Arg.String
     (fun f ->format := Filename.basename f; cmd_line_format := true), "Change the default document format");
   ("--driver",Arg.String
     (fun f ->driver := f; cmd_line_driver := true), "Change the default document driver");
   ("-c",Arg.Unit (fun ()->amble:=Generateur.Separate), "Compile separately");
   ("--noamble",Arg.Unit (fun ()->amble:=Generateur.Noamble), "Compile separately");
   ("-package",Arg.String (fun s-> package_list:= s::(!package_list)), "Use package given as argument when compiling");
   ("--caml",Arg.String (fun arg -> (dirs := arg :: !dirs)), "Add the given arguments to the OCaml command line");
   ("--ml",Arg.Unit (fun () -> compile:=false; run:= false), "Only generates OCaml code");
   ("--bin",Arg.Unit (fun () -> compile:=true; run:= false), "Generates OCaml code and compiles it");
   ("--pdf",Arg.Unit (fun () -> compile:=true; run:= true), "Generates OCaml code, compiles it and runs it");
   ("--edit-link", Arg.Unit (fun () -> Generateur.edit_link:=true), "Generated uri link of the form \"edit:filename@line");
   ("--no-line-directive", Arg.Unit (fun () -> Generateur.line_directive:=false), "Disable generation of \"# line\" directive in the generated ml (for debugging the generator)");
   ("--patoline",Arg.String (fun s->patoline:=s), "Changes the patoline compiler");
   ("--ocamlopt",Arg.String (fun s->ocamlopt:=s), "Changes the ocamlopt compiler");
   ("-j",Arg.Int (fun s->Build.j:=max !Build.j s), "Maximum number of processes");
   ("--quiet", Arg.Unit (fun ()->quiet:=true), "Do not output the commands");
   ("--",Arg.Rest (fun s -> extras := !extras ^ " " ^s), "Remaining arguments are passed to the OCaml executable")
  ]

let str_dirs () = (String.concat " " !dirs)

let pdfname_of f = (Filename.chop_extension f)^".pdf"
let mlname_of f = (Filename.chop_extension f)^(if !amble==Generateur.Separate then ".ttml" else ".tml")
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
  packages:string list
}


let rec read_options_from_source_file fread =
  let deps=ref [] in
  let format=ref !format in
  let driver=ref !driver in
  let comp_opts=ref !dirs in
  let packages=ref !package_list in

  let nothing = Str.regexp "^[ \t]*\\((\\*.*\\*)\\)?[ \t\r]*$" in
  let set_format = Str.regexp "^[ \t]*(\\*[ \t]*#FORMAT[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let set_driver = Str.regexp "^[ \t]*(\\*[ \t]*#DRIVER[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let link = Str.regexp "^[ \t]*(\\*[ \t]*#LINK[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let depends = Str.regexp "^[ \t]*(\\*[ \t]*#DEPENDS[ \t]+\\([^ \t]+\\)[ \t]*\\*)[ \t\r]*$" in
  let add_compilation_option = Str.regexp "^[ \t]*(\\*[ \t]*#COMPILATION[ \t]+\\(.+\\)[ \t]*\\*)[ \t\r]*$" in
  let add_package = Str.regexp "^[ \t]*(\\*[ \t]*#PACKAGES[ \t]+\\([^ \t]+\\(,[ \t]*[^ \t]+\\)*\\)[ \t]*\\*)[ \t\r]*$" in
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
          let objects=(Mutex.create (), ref []) in
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
    else if Str.string_match nothing s 0 then pump ()
  in
  (try
     pump ()
   with End_of_file -> ());
  seek_in fread 0;
  {
    deps= !deps;
    format= !format;
    driver= !driver;
    comp_opts= !comp_opts;
    packages= !packages
  }
and make_deps source=
  let ldeps=ref [] in
  Build.sem_down Build.sem;
  let in_s=open_in source in
  let opts=read_options_from_source_file in_s in
  close_in in_s;

  let dirs_=String.concat " " !dirs in
  let cmd=Printf.sprintf
    "ocamlfind ocamldep %s %s -ml-synonym .tml -ml-synonym .ttml -ml-synonym .txp %s"
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

and patoline_rule objects h=
  if Filename.check_suffix h ".ttml" then
    (
      let source=(Filename.chop_suffix h ".ttml")^".txp" in
      let in_s=open_in source in
      let opts=read_options_from_source_file in_s in
      close_in in_s;
      List.iter Build.build opts.deps;

      let age_h=if Sys.file_exists h then (Unix.stat h).Unix.st_mtime else -.infinity in
      let age_source=if Sys.file_exists h then (Unix.stat source).Unix.st_mtime else infinity in
      if age_h<age_source then (
        let dirs_=String.concat " " !dirs in
        let cmd=Printf.sprintf "%s %s --ml%s%s --driver %s -c %s"
          !patoline
          dirs_
          (if !format<>"DefaultFormat" then " --format " else "")
          (if !format<>"DefaultFormat" then !format else "")
	  opts.driver
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
  else if Filename.check_suffix h ".tml" then
    (
      let source=(Filename.chop_suffix h ".tml")^".txp" in
      let in_s=open_in source in
      let opts=read_options_from_source_file in_s in
      close_in in_s;
      List.iter Build.build opts.deps;

      let age_h=if Sys.file_exists h then (Unix.stat h).Unix.st_mtime else -.infinity in
      let age_source=if Sys.file_exists h then (Unix.stat source).Unix.st_mtime else infinity in
      if age_h<age_source then (
        let dirs_=String.concat " " !dirs in
        let cmd=Printf.sprintf "%s %s --ml%s%s --driver %s %s"
          !patoline
          dirs_
          (if !format<>"DefaultFormat" then " --format " else "")
          (if !format<>"DefaultFormat" then !format else "")
	  opts.driver
          source
        in
        if not !quiet then (Printf.fprintf stdout "%s\n" cmd; flush stdout);
        let err=Build.command cmd in
        if err<>0 then (
          failwith ("error : "^h);
        )
      );
      true
    )
  else if Filename.check_suffix h ".ml" then (
    Sys.file_exists h;
  )
  else if Filename.check_suffix h ".cmx" then (
    Mutex.lock (fst objects);
    (if not (List.mem h !(snd objects)) then
        (snd objects):=h::(!(snd objects)));
    Mutex.unlock (fst objects);
    let pref=Filename.chop_extension h in
    let source_ml=pref^".ml" in
    let source=
      if Sys.file_exists source_ml then source_ml else (
        pref^".ttml"
      )
    in
    Build.build source;

    let deps0=make_deps source in
    let tar=List.map (Thread.create Build.build) deps0 in
    List.iter (Thread.join) tar;

    let age_h=if Sys.file_exists h then (Unix.stat h).Unix.st_mtime else -.infinity in
    let age_source=if Sys.file_exists h then (Unix.stat source).Unix.st_mtime else infinity in
    if age_h<age_source then (
      let dirs_=String.concat " " !dirs in
      let cmd=Printf.sprintf "ocamlfind %s %s %s -c -o %s -impl %s"
        !ocamlopt
        (let pack=String.concat "," (List.rev !package_list) in
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
    Build.build source;

    let in_s=open_in source in
    let opts=read_options_from_source_file in_s in
    close_in in_s;
    List.iter Build.build opts.deps;

    let deps0=make_deps source in
    let tar=List.map (Thread.create Build.build) deps0 in
    List.iter (Thread.join) tar;

    let age_h=if Sys.file_exists h then (Unix.stat h).Unix.st_mtime else -.infinity in
    let age_source=if Sys.file_exists h then (Unix.stat source).Unix.st_mtime else infinity in
    if age_h<age_source then (
      Mutex.lock (fst objects);
      let dirs_=String.concat " " !dirs in
      let cmd=Printf.sprintf "ocamlfind %s %s %s %s -linkpkg -o %s %s -impl %s"
        !ocamlopt
        (let pack=String.concat "," (List.rev !package_list) in
         if pack<>"" then "-package "^pack else "")
        dirs_
        (if Filename.check_suffix h ".cmxs" then "-shared" else "")

        h (String.concat " " (!(snd objects))) source
      in
      Mutex.unlock (fst objects);

      if not !quiet then (Printf.fprintf stdout "%s\n" cmd;flush stdout);
      let err=Build.command cmd in
      if err<>0 then (
        failwith ("error : "^h)
      )
    );
    true
  )
  else false
and process_each_file l=
  if l=[] then
    Printf.fprintf stderr "no input files\n"
  else
    List.iter (fun f->
      if !compile then (
        if !format <> "DefaultFormat" &&
          (not (List.mem ("Typography." ^ !format) !package_list)) then
          package_list := ("Typography." ^ !format) :: !package_list;
        package_list := ("Typography."^ !driver):: !package_list;

        let cmd= (Filename.concat
                    (Sys.getcwd ())
                    (Filename.chop_extension f)^".tmx") in
        Build.sem_set Build.sem !Build.j;
        Build.build cmd;
        if !run then (
          Printf.fprintf stdout "%s\n" cmd;flush stdout;
          let _=Build.command cmd in
          ()
        )
      ) else (
        let where_ml = open_out (mlname_of f) in
        let fread = open_in f in
        Parser.out_grammar_name:=Filename.chop_extension f;
        if Filename.check_suffix f "txt" then (
          SimpleGenerateur.gen_ml !format SimpleGenerateur.Main f fread (mlname_of f) where_ml (pdfname_of f);
        ) else (
          let opts=read_options_from_source_file fread in
          Parser.fprint_caml_buf :=
            (fun ld gr buf s e txps opos ->
              let pos = pos_in fread in
              Generateur.print_caml_buf (Parser.pp ()) ld gr (Generateur.Source.of_in_channel fread) buf s e txps opos;
              seek_in fread pos);
          Generateur.gen_ml opts.format opts.driver !amble f fread (mlname_of f) where_ml (pdfname_of f);
        );
        close_out where_ml;
        close_in fread;
      )
    ) l


let _=
  let objects=(Mutex.create (), ref []) in
  Build.append_rule (patoline_rule objects)


(************************************************)




let _ =
  Arg.parse spec (fun x->files := x::(!files)) "Usage :";
  process_each_file (List.rev !files)
