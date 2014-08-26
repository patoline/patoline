open Pa_ocaml
open Glr
open Format
let _ =
  if entry = Top
  then
    (Printf.eprintf "native toplevel not supported by pa_ocaml.\n%!"; exit 1)
let ast =
  let (name,ch) =
    match !file with
    | None  -> ("stdin", stdin)
    | Some name -> (name, (open_in name)) in
  try
    if entry = Impl
    then `Struct (parse_channel structure blank name ch)
    else `Sig (parse_channel signature blank name ch)
  with
  | Parse_error (fname,l,n,msgs) ->
      let msgs = String.concat " | " msgs in
      (Printf.eprintf
         "File %S, line %d, characters %d:\nError: Syntax error, %s expected\n"
         fname l n msgs;
       exit 1)
let _ =
  if !ascii
  then
    ((match ast with
      | `Struct ast -> Printast.implementation Format.std_formatter ast
      | `Sig ast -> Printast.interface Format.std_formatter ast);
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
