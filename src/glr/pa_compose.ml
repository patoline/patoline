open Pa_ocaml_prelude
open Pa_ocaml
open Decap
open Format

let anon_fun s = file := Some s

let _ = Arg.parse !spec anon_fun (Printf.sprintf "usage: %s [options] file" Sys.argv.(0)) 
  
module Final = (val
  List.fold_left
    (fun acc ext ->
     let module Acc = (val acc : Extension) in
     let module Ext = (val ext : FExt) in
       (module Ext(Acc) : Extension))
    (module Initial : Extension) (List.rev (!extensions_mod)) : Extension)

module Main = Make(Final)
