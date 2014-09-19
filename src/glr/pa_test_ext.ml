open Pa_ocaml_prelude

module Ext = functor(In:Extension) -> 
struct
  include In
  (* can not use both pa_glr and pa_ocaml extension yet! Waiting for bootstrap ! *)
  (* need Decap.apply to avoid to parse the quotation when the grammar is not ready.
     will be automatic when using a syntax for parser *)
  let test = glr STR("++0++") -> (Atom, <:expr<0>>) end
  let extra_expressions = test::extra_expressions

  let test2 = glr STR("++unit++") -> <:type<unit -> unit>> end
  let extra_types = test2::extra_types
end

let _ = register_extension (module Ext)
