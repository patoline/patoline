open Ocamlbuild_plugin;;

dispatch begin function
  | After_options ->(
    flag ["ocaml";"compile"] & S[A"-I";A"../../Typography/_build"];
  )
  | _ -> ()
end
