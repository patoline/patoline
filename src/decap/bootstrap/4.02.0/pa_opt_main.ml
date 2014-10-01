open Pa_ocaml_prelude
open Pa_ocaml
open Decap
open Format
open Pa_compose
let entry =
  match ((!entry), (!file)) with
  | (FromExt ,Some s) ->
      let rec fn =
        function
        | (ext,res)::l -> if Filename.check_suffix s ext then res else fn l
        | [] -> (eprintf "Don't know what to do with file %s\n%!" s; exit 1) in
      fn (!Main.entry_points)
  | (FromExt ,None ) -> `Top
  | (Intf ,_) -> `Intf Main.signature
  | (Impl ,_) -> `Impl Main.structure
  | (Toplvl ,_) -> `Top
let _ =
  if entry = `Top
  then
    (Printf.eprintf "native toplevel not supported by pa_ocaml.\n%!"; exit 1)
let ast =
  let (filename,ch) =
    match !file with
    | None  -> ("stdin", stdin)
    | Some name -> (name, (open_in name)) in
  try
    match entry with
    | `Impl g -> `Struct (parse_channel ~filename g blank ch)
    | `Intf g -> `Sig (parse_channel ~filename g blank ch)
    | `Top -> assert false
  with | Decap.Parse_error _ as e -> (Decap.print_exception e; exit 1)
let _ =
  if !ascii
  then
    ((match ast with
      | `Struct ast -> Pprintast.structure Format.std_formatter ast
      | `Sig ast -> Pprintast.signature Format.std_formatter ast);
     Format.print_newline ())
  else
    (let magic =
       match ast with
       | `Struct _ -> Config.ast_impl_magic_number
       | `Sig _ -> Config.ast_intf_magic_number in
     output_string stdout magic;
     output_value stdout (match !file with | None  -> "" | Some name -> name);
     (match ast with
      | `Struct ast -> output_value stdout ast
      | `Sig ast -> output_value stdout ast);
     close_out stdout)
