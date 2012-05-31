open Ocamlbuild_plugin;;
open Command;;

let bibfile = ref []
let main=ref None

let print_info f =
  Format.fprintf Format.std_formatter
    "@[<hv 2>Tags for file %s:@ %a@]@." 
    (Pathname.to_string f)
    Tags.print (tags_of_pathname f)

let tag_to_string tag =
  let buf = Buffer.create 80 in
  let _ = Format.bprintf buf
    "%a" 
    Tags.print tag
  in
  Buffer.contents buf

exception Found of int
let rec find_str str0 str1_=try
  let str1=str1_^"(" in
  let s0=try
    for i=0 to String.length str0-String.length str1 do
      try
        for j=0 to String.length str1-1 do
          if str0.[i+j]<>str1.[j] then raise Not_found
        done;
        raise (Found i)
      with
          Not_found->()
    done;
    raise Not_found
  with
      Found i->i
  in
  let s1=String.index_from str0 s0 ')' in
    (String.sub str0 (s0+String.length str1) (s1-s0-String.length str1))::
      (find_str (String.sub str0 (s1+1) (String.length str0-s1-1)) str1_)
with
Not_found->[]


let _ = dispatch begin function
  | After_options ->

      (*c This enforces the creation of symbolic links to the build directory. *)
      Options.make_links := true

  | After_rules ->

      ocaml_lib "Typography/Typography";
      ocaml_lib "Format/DefaultFormat";



      rule "patoline: txp -> dep"
        ~prods:["%.txp.deps"]
        ~deps:["%.txp"]
        begin fun env _build->
          Seq[Cmd(S([A"patoline";A"--deps";A"--ml";P(env "%.txp")]));
              Cmd(S[A"rm";P(env "%.tml")])]
        end;
      rule "patoline: tml -> ml"
        ~prods:["%.ml"]
        ~deps:["%.tml"]
      begin fun env _build ->
        cp (env "%.tml") (env "%.ml")
      end;

      rule "patoline: txp -> ml"
        ~prods:["%.tml"]
        ~deps:["%.txp";"%.txp.deps"]
      begin fun env _build ->
	let txp = env "%.txp" in
        let deps=
          let i=open_in (env "%.txp.deps") in
          let rec deps ()=try
            let _=_build [[input_line i]] in
              deps ()
          with End_of_file->()
          in
            deps ()
        in

	let tags = tag_to_string (tags_of_pathname txp) in
	let format =match find_str tags "format" with
            []->"DefaultFormat"
          | h::_->h
	in
        let is_main=(match !main with
                         Some a->a=txp
                       | None->(main:=Some txp; true))
        in
          if is_main then tag_file (env "%.txp") ["main"];
          let grammar=match find_str tags "grammar" with
              []->()
            | l->let _=_build [(List.map (fun filename->(try Filename.chop_extension filename with _->filename) ^ ".tgx") l)] in
                ()
          in
          let bibi=match find_str tags "bibi" with
              []->()
            | l->(
                bibfile:=l@(!bibfile);
                List.iter (fun bibfilename->tag_file (env "%.pdf") [ "bibi("^bibfilename^")" ]) l
              )
          in
            Seq[Cmd(S([A"patoline";
                       A"--ml";
		       A"--format";A format]@
                        (if is_main then [] else [A"-c"])@
		       [P(env "%.txp")]));
	       ]
      end ;

      rule "patoline: txp -> tgx"
        ~prods:["%.tgx";"%.tml"]
        ~dep:"%.txp"
      begin fun env _build ->
        Cmd(S[A"patoline";A"--noamble";A"--ml"; P(env "%.txp")])
      end ;

      rule "ocaml: native -> pdf"
        ~prods:["%.pdf"]
        ~dep:"%.native"
      begin fun env _build ->
	let _ = _build [!bibfile] in
        let cmd=Seq[Cmd(S[A"exec";A(env (".."/"_build"/"%.native"));A"--extra-fonts-dir";A".."]);
	            ln_s(env ("_build" / "%.pdf")) (env "../%.pdf")]
        in
          cmd
      end ;

  | _ -> ()
end
