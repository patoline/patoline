open Pa_ocaml_prelude
open Pa_ocaml
open Glr
open Format

module Final = (val
		  List.fold_left (fun (module Acc:Extension) (module Ext:FExt) -> 
                      (module (Ext(Acc)))) (module Initial) (List.rev !extensions_mod))

let _ = Arg.parse !spec anon_fun (Printf.sprintf "usage: %s [options] file" Sys.argv.(0)) 
  
module Main = Make(Final)

let entry =
  match !entry, !file with
    FromExt, Some s -> 
    let rec fn = function
	(ext, res)::l -> if Filename.check_suffix s ext then res else fn l
      | [] -> eprintf "Don't know what to do with file %s\n%!" s; exit 1
    in
    fn !Main.entry_points
  | FromExt, None -> `Top
  | Intf, _ -> `Intf Main.signature
  | Impl, _ -> `Impl Main.structure
  | Toplvl, _  -> `Top

#ifdef BYTE
let _ = if entry = `Top then
  printf "Pa_ocaml top level using OCaml %s%!" (Sys.ocaml_version);
  Toploop.initialize_toplevel_env ();
  let rec loop () =
    Format.printf "\n> %!";
    (try
	(* the buffer is recreated for each phrase in case of parse error ! *)
	let buffer = Input.buffer_from_channel "stdin" stdin in
	let phrases = partial_parse_buffer Main.top_phrase blank buffer 0 in
	match phrases with
	  [buffer,pos,ph] ->
	  ignore (Toploop.execute_phrase true Format.std_formatter ph)
	| _ -> assert false
      with
      | Main.Top_Exit -> 
	 raise Main.Top_Exit
      | Glr.Parse_error(_,line,col,msgs) ->
	 let msgs = String.concat " | " msgs in
	 Printf.eprintf "line %d, characters %d:\n\
			 Error: Syntax error, %s expected\n%!"
			line col msgs;
      | e ->  
	 Errors.report_error Format.std_formatter e);
    loop ()

  in
  try
    loop ()
  with
  | Main.Top_Exit -> exit 0
#else
let _ = 
  if entry = `Top then (
    Printf.eprintf "native toplevel not supported by pa_ocaml.\n%!" ; exit 1)
#endif

let ast =
  (* read the whole file with a buffer ...
     to be able to read stdin *)
  let name, ch = match !file with
      None -> "stdin", stdin
    | Some name -> 
(*       let buffer = Input.buffer_from_file name in
       List.iter (fun line ->
		  Printf.eprintf "%s\n" line.Input.contents) buffer;*)
       name, open_in name
  in
  try
    match entry with
      `Impl g -> `Struct (parse_channel g blank name ch)
    | `Intf g -> `Sig (parse_channel g blank name ch)
    | `Top -> assert false
  with
    Parse_error (fname,l,n,msgs) ->
    let msgs = String.concat " | " msgs in
    Printf.eprintf "File %S, line %d, characters %d:\n\
                    Error: Syntax error, %s expected\n"
                   fname l n msgs;
    exit 1

let _ = 
  if !ascii then begin
    begin
#ifversion >= 4.01
      match ast with 
      | `Struct ast -> Pprintast.structure Format.std_formatter ast;
      | `Sig ast -> Pprintast.signature Format.std_formatter ast;
#else
      match ast with 
      | `Struct ast -> Printast.implementation Format.std_formatter ast;
      | `Sig ast -> Printast.interface Format.std_formatter ast;
#endif
    end;
    Format.print_newline ()
  end else begin
    let magic = match ast with 
      | `Struct _ -> Config.ast_impl_magic_number
      | `Sig _ -> Config.ast_intf_magic_number
    in
    output_string stdout magic;
    output_value stdout (match !file with None -> "" | Some name -> name);
    begin
      match ast with 
      | `Struct ast -> output_value stdout ast
      | `Sig ast -> output_value stdout ast
    end;
    close_out stdout
  end
