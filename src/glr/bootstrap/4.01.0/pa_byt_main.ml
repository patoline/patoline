open Pa_ocaml_prelude
open Pa_ocaml
open Glr
open Format
let anon_fun s = file := (Some s)
let _ =
  Arg.parse (!spec) anon_fun
    (Printf.sprintf "usage: %s [options] file" (Sys.argv.(0)))
module Final = (val
  List.fold_left
    (fun ((module Acc)  : (module Extension))  ->
       fun ((module Ext)  : (module FExt))  -> (module Ext(Acc))) (module
    Initial) (List.rev (!extensions_mod)))
module Main = Make(Final)
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
  then printf "Pa_ocaml top level using OCaml %s%!" Sys.ocaml_version;
  Toploop.initialize_toplevel_env ();
  (let rec loop () =
     Format.printf "\n> %!";
     (try
        let buffer = Input.buffer_from_channel "stdin" stdin in
        let phrases = partial_parse_buffer Main.top_phrase blank buffer 0 in
        match phrases with
        | (buffer,pos,ph)::[] ->
            ignore (Toploop.execute_phrase true Format.std_formatter ph)
        | _ -> assert false
      with | Main.Top_Exit  -> raise Main.Top_Exit
      | Glr.Parse_error (_,line,col,msgs) ->
          let msgs = String.concat " | " msgs in
          Printf.eprintf
            "line %d, characters %d:\nError: Syntax error, %s expected\n%!"
            line col msgs
      | e -> Errors.report_error Format.std_formatter e);
     loop () in
   try loop () with | Main.Top_Exit  -> exit 0)
let ast =
  let (name,ch) =
    match !file with
    | None  -> ("stdin", stdin)
    | Some name -> (name, (open_in name)) in
  try
    match entry with
    | `Impl g -> `Struct (parse_channel g blank name ch)
    | `Intf g -> `Sig (parse_channel g blank name ch)
    | `Top -> assert false
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
