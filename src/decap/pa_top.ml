(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

open Pa_ocaml_prelude
open Pa_ocaml
open Decap
open Format

module ParserExt = Pa_parser.Ext(Pa_ocaml_prelude.Initial)
module Default = Pa_ocaml.Make(ParserExt)

module type Final = sig
  include Extension

  exception Top_Exit
  val top_phrase : Parsetree.toplevel_phrase Decap.grammar

end

module Start = functor (Main : Final) -> struct
  let anon_fun s = file := Some s
  let _ = Arg.parse !spec anon_fun (Printf.sprintf "usage: %s [options] file" Sys.argv.(0))

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
  let _ = if entry = `Top then (
    printf "Pa_ocaml top level using OCaml %s%!" (Sys.ocaml_version);
    Toploop.initialize_toplevel_env ();
    let rec loop () =
      Format.printf "\n> %!";
      (try
  	(* the buffer is recreated for each phrase in case of parse error ! *)
  	let buffer = Input.buffer_from_channel ~filename:"stdin" stdin in
  	let (buffer,pos,ph) = partial_parse_buffer Main.top_phrase blank buffer 0 in
  	ignore (Toploop.execute_phrase true Format.std_formatter ph)
        with
        | Main.Top_Exit -> 
  	 raise Main.Top_Exit
        | Decap.Parse_error _ as e ->
  	 Decap.print_exception e;
  	 exit 1
        | e ->  
  	 Errors.report_error Format.std_formatter e);
      loop ()
  
    in
    try
      loop ()
    with
    | Main.Top_Exit -> exit 0)
#else
  let _ = 
    if entry = `Top then (
      Printf.eprintf "native toplevel not supported by pa_ocaml.\n%!" ; exit 1)
#endif
  
  let ast =
    (* read the whole file with a buffer ...
       to be able to read stdin *)
    let filename, ch = match !file with
        None -> "stdin", stdin
      | Some name -> 
  (*       let buffer = Input.buffer_from_file name in
         List.iter (fun line ->
  		  Printf.eprintf "%s\n" line.Input.contents) buffer;*)
         name, open_in name
    in
    try
      match entry with
        `Impl g -> `Struct (parse_channel ~filename g blank ch)
      | `Intf g -> `Sig (parse_channel ~filename g blank ch)
      | `Top -> assert false
    with
      Decap.Parse_error _ as e ->
      Decap.print_exception e;
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
end


