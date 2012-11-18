open Ocamlbuild_plugin;;

dispatch begin function
  | After_options ->(
    (rule "unicode_ranges"
        ~prod:"Fonts/Sfnt/unicode_ranges.ml"
        ~deps:["Fonts/Sfnt/make_unicode_ranges.ml";"Fonts/Sfnt/unicode"]
        begin fun env _build->
          let _=_build [["Fonts/Sfnt/make_unicode_ranges.byte"]] in
          Cmd(S[A"Fonts/Sfnt/make_unicode_ranges.byte";A"Fonts/Sfnt/unicode"])
        end);

    (* flag ["ocaml";"compile";"rectypes"] & A"-rectypes"; *)
    flag ["ocaml";"compile"] & S[A"-I";A"../../Rbuffer"];
    flag ["ocaml";"compile";"rectypes"] & A"-rectypes";
    flag ["ocaml";"pack"] & S[A"-linkall"];
    flag ["ocaml";"link"] & S[A"-linkall"];
    (* flag ["ocaml"] & S[A"-pp";A"cpp"]; *)
  )
  | _ -> ()
end
