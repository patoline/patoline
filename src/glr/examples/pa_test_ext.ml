open Pa_ocaml_prelude

let _ = parser_locate locate (*merge*) locate2

module Ext = functor(In:Extension) -> 
struct
  include In
  (* can not use both pa_glr and pa_ocaml extension yet! Waiting for bootstrap ! *)
  (* need Decap.apply to avoid to parse the quotation when the grammar is not ready.
     will be automatic when using a syntax for parser *)

  let test = parser
  | STR("let") STR("try") b:let_binding STR("in") e:(expr) STR("with") c:(match_cases Top) ->
      let c = List.map (fun (pat, e) -> (pat, <:expr< fun () -> $e$ >>)) c in
      (Let, <:expr<(try let $bindings:b$ in fun () -> $e$ with $cases:c$) ()>>)
  | STR("do") e:(expr) STR("where") r:STR("rec")? b:let_binding ->
      (Let, if r<>None then <:expr<let rec $bindings:b$ in $e$>> else <:expr<let $bindings:b$ in $e$>>)
								     
  let extra_expressions = test::extra_expressions
  let _ = reserved_ident := "where"::!reserved_ident

end

let _ = register_extension (module Ext)
