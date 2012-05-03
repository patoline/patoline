open Ocamlbuild_plugin;;
open Command;;

let texprime = A"texprime";;
let mmml = Sh" --ml"
let noamble = Sh"--noamble";;

let bibfile = ref []

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

let regexp_format = Str.regexp ".*format(\\([^)]*\\)).*" 
let regexp_bibi = Str.regexp ".*bibi(\\([^)]*\\)).*" 

let _ = dispatch begin function
  | After_options ->

      (*c This enforces the creation of symbolic links to the build directory. *)
      Options.make_links := true

  | After_rules ->

      ocaml_lib ~dir:"/usr/local/lib/ocaml" "Typography/Typography";
      ocaml_lib ~dir:"/usr/local/lib/ocaml" "Format/DefaultFormat";

      rule "texprime: txp -> ml"
        ~prods:["%.ml"]
        ~deps:["%.txp"]
      begin fun env _build ->
	let txp = env "%.txp" in
	let tags = tag_to_string (tags_of_pathname txp) in
	let format = 
	  if Str.string_match regexp_format tags 0 then
	    Str.matched_group 1 tags
	  else "DefaultFormat"
	in
	let _ = 
	  if Str.string_match regexp_bibi tags 0 then begin
	    let bibfilename = Str.matched_group 1 tags in
	    bibfile := bibfilename :: !bibfile ;
	    tag_file (env "%.pdf") [ "bibi("^bibfilename^")" ] ;
	    Printf.fprintf stderr
	      "bibi file: %s\n" (Str.matched_group 1 tags) ;
	    flush stderr
	  end
	in
	let _ = print_info txp in
        Seq[Cmd(S[texprime;mmml; 
		  A"--format";A format;
		  P(env "%.txp")]);
	    cp (env "%.tml") (env "%.ml") ]
      end ;

      rule "texprime: txp -> tgx"
        ~prods:["%.tgx"]
        ~dep:"%.txp"
      begin fun env _build ->
        Cmd(S[texprime;noamble;mmml; P(env "%.txp")])
      end ;

      rule "ocaml: native -> pdf"
        ~prods:["%.pdf"]
        ~dep:"%.native"
      begin fun env _build ->
	let _ = _build [!bibfile] in
        Seq[Cmd(A(env "%.native"));
	    ln_s (env ("_build" / "%.pdf")) (env "../%.pdf")]
      end ;

  | _ -> ()
end
