open Pa_ocaml_prelude
open Pa_ocaml
open Decap
open Format
module type Final  =
  sig
    include Extension
    exception Top_Exit
    val top_phrase : Parsetree.toplevel_phrase Decap.grammar
  end
module Start(Main:Final) =
  struct
    let anon_fun s = file := (Some s)
    let _ =
      Arg.parse (!spec) anon_fun
        (Printf.sprintf "usage: %s [options] file" (Sys.argv.(0)))
    let _ = Main.before_parse_hook ()
    let entry =
      match ((!entry), (!file)) with
      | (FromExt ,Some s) ->
          let rec fn =
            function
            | (ext,res)::l ->
                if Filename.check_suffix s ext then res else fn l
            | [] ->
                (eprintf "Don't know what to do with file %s\n%!" s; exit 1) in
          fn (!Main.entry_points)
      | (FromExt ,None ) -> Implementation (Main.structure, blank)
      | (Intf ,_) -> Interface (Main.signature, blank)
      | (Impl ,_) -> Implementation (Main.structure, blank)
    let ast =
      let (filename,ch) =
        match !file with
        | None  -> ("stdin", stdin)
        | Some name -> (name, (open_in name)) in
      try
        match entry with
        | Implementation (g,blank) ->
            `Struct (parse_channel ~filename g blank ch)
        | Interface (g,blank) -> `Sig (parse_channel ~filename g blank ch)
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
         output_value stdout
           (match !file with | None  -> "" | Some name -> name);
         (match ast with
          | `Struct ast -> output_value stdout ast
          | `Sig ast -> output_value stdout ast);
         close_out stdout)
  end
