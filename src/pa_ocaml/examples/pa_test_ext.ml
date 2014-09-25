open Pa_ocaml_prelude

#define LOCATE locate

module Ext = functor(In:Extension) -> 
struct
  include In
  (* can not use both pa_glr and pa_ocaml extension yet! Waiting for bootstrap ! *)
  (* need Decap.apply to avoid to parse the quotation when the grammar is not ready.
     will be automatic when using a syntax for parser *)

  let test = parser
  | STR("let") STR("try") b:let_binding STR("in") e:(expr) STR("with") c:(match_cases Top) ->
(* missing quotation in pattern yet *)
#ifversion >= 4.02
      let c = Parsetree.(List.map (fun ({ pc_rhs = e; _ } as b) ->  { b with pc_rhs = <:expr< fun () -> $e$ >> }) c) in
#else
      let c = List.map (fun (pat, e) -> (pat, <:expr< fun () -> $e$ >>)) c in
#endif
      (Let, <:expr<(try let $bindings:b$ in fun () -> $e$ with $cases:c$) ()>>)
  | STR("do") e:(expr) STR("where") r:STR("rec")? b:let_binding ->
      (Let, if r<>None then <:expr<let rec $bindings:b$ in $e$>> else <:expr<let $bindings:b$ in $e$>>)
								     
  let extra_expressions = test::extra_expressions
  let _ = add_reserved_id "where"

end

let _ = register_extension (module Ext)
