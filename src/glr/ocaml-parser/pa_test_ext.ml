open Pa_ocaml_prelude

module Ext = functor(In:Extension) -> 
struct
  include In
  (* can not use both pa_glr and pa_ocaml extension yet! Waiting for bootstrap ! *)
  (* need Glr.apply to avoid to parse the quotation when the grammar is not ready.
     will be automatic when using a syntax for parser *)
  let test = Glr.apply (fun f -> f ()) (Glr.string "++0++" (fun () -> (Atom, <expr:"0">)))  
  let extra_expressions = test::extra_expressions

  let test2 = Glr.apply (fun f -> f ()) (Glr.string "++unit++" (fun () -> <type:"unit -> unit">))  
  let extra_types = test2::extra_types
end

let _ = register_extension (module Ext)
