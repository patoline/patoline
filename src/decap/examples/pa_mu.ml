open Pa_ocaml_prelude
open Parsetree
 
#define LOCATE locate

(*
ocamlopt -pp pa_ocaml -I .. -I +compiler-libs ocamlcommon.cmxa str.cmxa unix.cmxa decap.cmxa decap_ocaml.cmxa pa_mu5.ml -o pa_mu
*)
module Ext = functor(In:Extension) -> 
struct
  include In

  let cont = "kk"

  let gen_sym =
    let cont = ref 0 in
    (fun name ->
     let c = !cont in
     cont := c+1;
     name ^ string_of_int c)

  type 'a classical =
    | MuLambda of pattern list * 'a classical
    | MuApp of 'a classical * 'a classical list
    | Return of 'a
    | MuLet of pattern * 'a classical * 'a classical
    | Star of 'a classical * string
    | Mu of string * 'a classical
    | MuIf of 'a * 'a classical * 'a classical
    | MuCase of 'a * (pattern * 'a classical) list


  let rec translate head _loc =
    let mkh head e =
      if head then e else <:expr< ($lid:cont$ $e$)>> in
    function
    | Return e -> mkh head e
    | Mu(id,e) ->
       if head then raise (Decap.Give_up "Illegal classical definition");
       let e = translate false _loc e in
       <:expr< let $lid:id$ = $lid:cont$ in $e$ >>
    | Star(e,id) ->
       let e = translate false _loc e in
       <:expr< let $lid:cont$ = $lid:id$ in $e$ >>
    | MuLambda(ids, e) ->
       let e = translate false _loc e in
       let r = List.fold_right (fun id acc -> <:expr<fun $id$ -> $acc$>>) ids <:expr<fun $lid:cont$ -> $e$>> in
       mkh head r
    | MuApp(f, args) ->
       if head then raise (Decap.Give_up "Illegal classical definition");
       let rec fn acc = function
	   [] -> assert false
	 | [Return arg] ->
	    let v = gen_sym "v" in
	    (match f with
	      Return f -> <:expr< $acc <:expr<$f$ $arg$>>$ $lid:cont$ >>
	     | f -> let f = translate false _loc f in
		    <:expr<let $lid:v$ = $arg$ in let $lid:cont$ = fun f -> $acc <:expr<f $lid:v$>>$ $lid:cont$
		     in $f$>>)
	 | [arg] ->
	    let arg = translate false _loc arg in
	    let v = gen_sym "v" in
	    (match f with
	      Return f -> <:expr< let $lid:cont$ = fun $lid:v$ -> $acc <:expr<$f$ $lid:v$>>$  $lid:cont$
		    in $arg$ >>
	     | f -> let f = translate false _loc f in
		    let fn = gen_sym "f" in
		    <:expr< let $lid:cont$ = fun $lid:v$ ->
                     let $lid:cont$ = (fun $lid:fn$ -> $acc <:expr<$lid:fn$ $lid:v$>>$ $lid:cont$)
	             in $f$
		     in $arg$ >>)
	 | Return arg::args ->
	    let v = gen_sym "v" in
	    let acc f = <:expr< $acc <:expr<$f$ $lid:v$>>$ >> in
	    <:expr<let $lid:v$ = $arg$ in $fn acc args$>>

	 | arg::args ->
	    let v = gen_sym "v" in
	    let arg = translate false _loc arg in
	    let acc f = <:expr< $acc <:expr<$f$ $lid:v$ >>$ >> in
	    <:expr< let $lid:cont$ = fun $lid:v$ -> $fn acc args$ in $arg$ >>
       in fn (fun f -> f) (List.rev args)
    | MuLet(id, Return e1, e2) ->
       let e2 = translate head _loc e2 in
       <:expr< let $id$ = $e1$ in $e2$ >>
    | MuLet(id, e1, e2) ->
       let e1 = translate false _loc e1 and e2 = translate head _loc e2 in
       <:expr< let $lid:cont$ = fun $id$ -> $e2$  in $e1$ >>
    | MuIf(e,e1,e2) ->
       let e1 = translate head _loc e1 and e2 = translate head _loc e2 in
       <:expr< if $e$ then $e1$ else $e2$ >>
    | MuCase(e,ls) ->
       let ls = List.map (fun (p,e) -> (p, translate head _loc e)) ls in
       <:expr< match $e$ with $cases:ls$ >>
  
  let classical_term, set_term = Decap.grammar_family "classical"

  let match_case = parser
		     STR("|") p:pattern STR("->") e:(classical_term true) -> (p,e)

  let args = parser e:(classical_term false) es:{STR("<") e:(classical_term false)}* -> e::es

  let parameters = parser e:pattern es:{STR("<") e:pattern}* -> e::es
    
  let _ = set_term (fun lvl -> parser
    | (key_word "fun") ids:parameters* STR("->") e:(classical_term true) when lvl ->
	List.fold_right (fun ids acc -> MuLambda(ids,acc)) ids e
    | (key_word "mu")  id:lowercase_ident STR("->") e:(classical_term true) when lvl -> Mu(id,e)
    | (key_word "let") id:pattern ls:parameters* STR("=") e1:(classical_term true) (key_word "in") e2:(classical_term true)  when lvl ->
	let e1 = List.fold_right (fun ids acc -> MuLambda(ids,acc)) ls e1 in		    
	MuLet(id,e1,e2)
    | "[" id:lowercase_ident "]" e:(classical_term false) -> Star(e,id)

    | f:(classical_term false) la:args* when lvl -> List.fold_left (fun f a -> MuApp(f,a)) f la
    | "(" a:(classical_term true) ")" -> a
    | (key_word "if") e:expr (key_word "then") e1:(classical_term true) (key_word "else") e2:(classical_term lvl) ->
       MuIf(e,e1,e2)
    | (key_word "match") e:expr (key_word "with") cases:match_case* -> MuCase(e,cases)
    | "(!" e:expr "!)" -> Return(e)
    | e:(expression_lvl (next_exp App)) -> Return(e)

   )		  

  let expr_extension lvl = parser
    | "(?" e:(classical_term true) "?)" -> Atom, <:expr< Classical.run (fun kk -> $translate false _loc e$) >>

  let type_suit_extension lvl' lvl = parser
  | STR("=>") te':(typexpr_lvl Arr) when lvl' > Arr && lvl <= Arr ->
      (Arr, fun te _loc_te -> <:type< $te$ -> $te'$ Classical.neg Classical.neg>>)

  let str_extension = parser
    | "let" "classical" name:pattern ls:parameters* "=" e:(classical_term true) ->
	let e = List.fold_right (fun ids acc -> MuLambda(ids,acc)) ls e in
        <:structure< let rec $name$ = $translate true _loc_e e$ >>

  let extra_expressions = expr_extension::extra_expressions
  let extra_structure = str_extension::extra_structure
  let extra_type_suits = type_suit_extension::extra_type_suits
  let _ = add_reserved_id "mu"
  let _ = add_reserved_id "classical"

end
module M = Pa_main.Start(Pa_ocaml.Make(Ext(Initial)))
