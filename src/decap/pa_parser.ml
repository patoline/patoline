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

open Asttypes
open Parsetree
open Longident
open Pa_ocaml_prelude
open Pa_ast

#define LOCATE locate

type action =
  | Default
  | Normal of expression
  | DepSeq of ((expression -> expression) * expression option * expression)

let find_locate () =
  try
    let l = Sys.getenv "LOCATE" in
    Some(exp_ident Location.none l)
  with Not_found -> None

let mkpatt _loc (id, p) = match p, find_locate () with
    None, _ -> pat_ident _loc id
  | Some p, None -> ppat_alias _loc p id
  | Some p, Some _ ->
     ppat_alias _loc (loc_pat _loc (Ppat_tuple[loc_pat _loc Ppat_any; p])) id

let mkpatt' _loc (id,p) =  match p with
    None -> pat_ident _loc id
  | Some p -> ppat_alias _loc p id

let filter _loc visible r =
  match find_locate (), visible with
  | Some(f2), true ->
     let f =
       exp_fun _loc "x" (
	 exp_fun _loc "str" (
	   exp_fun _loc "pos" (
	     exp_fun _loc "str'" (
	       exp_fun _loc "pos'" (
		 exp_tuple _loc [
  		   exp_apply _loc f2
		     [exp_ident _loc "str";
		      exp_ident _loc "pos";
		      exp_ident _loc "str'";
		      exp_ident _loc "pos'"];
		   exp_ident _loc "x"])))))
     in
     exp_apply _loc (exp_glr_fun _loc "apply_position") [f; r]
  | _ -> r


let rec build_action _loc occur_loc ids e =
  let e = match find_locate (), occur_loc with
    | Some(locate2), true ->
       exp_fun _loc "__loc__start__buf" (
	 exp_fun _loc "__loc__start__pos" (
	   exp_fun _loc "__loc__end__buf" (
	     exp_fun _loc "__loc__end__pos" (
	       loc_expr _loc (Pexp_let(Nonrecursive, [
	         value_binding _loc (pat_ident _loc "_loc")
			       (exp_apply _loc locate2 [exp_ident _loc "__loc__start__buf";
							exp_ident _loc "__loc__start__pos";
							exp_ident _loc "__loc__end__buf";
							exp_ident _loc "__loc__end__pos"])], e))))))
    | _ -> e
  in
  List.fold_left (fun e ((id,x),visible) ->
    match find_locate (), visible with
    | Some(_), true ->
      loc_expr _loc (
	pexp_fun("", None,
	  mkpatt _loc (id,x),
	  loc_expr _loc (Pexp_let(Nonrecursive,
	    [value_binding _loc (loc_pat _loc (Ppat_tuple([
		loc_pat _loc (Ppat_var (id_loc ("_loc_"^id) _loc));
		loc_pat _loc (Ppat_var (id_loc id _loc))])))
	     (loc_expr _loc (Pexp_ident((id_loc (Lident id) _loc))))],
	    e))))
    | _ ->
      loc_expr _loc (pexp_fun("", None, mkpatt' _loc (id,x), e))
  ) e (List.rev ids)

let apply_option _loc opt visible e =
  filter _loc visible (match opt with
    `Once -> e
  | `Option(greedy,d) ->
     let f = if greedy then "option'" else "option" in
    (match d with
       None ->
       exp_apply _loc (exp_glr_fun _loc f)
	  [exp_None _loc;
	   exp_apply _loc (exp_glr_fun _loc "apply")
	     [exp_Some_fun _loc; e]]
     | Some d ->
	exp_apply _loc (exp_glr_fun _loc f) [d; e])
  | `Fixpoint(greedy,d) ->
     let f = if greedy then "fixpoint'" else "fixpoint" in
    (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "apply")
	  [exp_list_fun _loc "rev";
	   exp_apply _loc (exp_glr_fun _loc f)
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
		[exp_Cons_fun _loc; e]]]
    | Some d ->
       exp_apply _loc (exp_glr_fun _loc f) [d; e])
  | `Fixpoint1(greedy,d) ->
     let f = if greedy then "fixpoint1'" else "fixpoint1" in
    (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "apply")
	  [exp_list_fun _loc "rev";
	   exp_apply _loc (exp_glr_fun _loc f)
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
		[exp_Cons_fun _loc; e]]]
    | Some d ->
       exp_apply _loc (exp_glr_fun _loc f) [d; e])
   )

let default_action _loc l =
  let l = List.filter (function `Normal((("_",_),false,_,_,_)) -> false | `Ignore -> false | _ -> true) l in
  let l = List.map (function `Normal((id,_),_,_,_,_) -> exp_ident _loc id | _ -> assert false) l in
  let rec fn = function
    | [] -> exp_unit _loc
    | [x] -> x
    | _::_ as l ->
       exp_tuple _loc l
  in
  fn l

module Ext = functor(In:Extension) ->
struct
  include In

  let glr_rules = Decap.declare_grammar "glr_rules"
  let glr_rule = Decap.declare_grammar "glr_rule"

  let location_name_re = {|_loc\([a-zA-Z0-9_']*\)|}

  let parser_prefix = parser e:cached_kw? parser_kw -> e <> None

  let glr_parser lvl =
    parser
    | cached:parser_prefix p:glr_rules -> (Atom, if cached then exp_apply _loc (exp_glr_fun _loc "cache") [p] else p)
    | cached:parser_prefix CHR('*') p:glr_rules -> (Atom, exp_apply _loc (exp_glr_fun _loc "lists") [if cached then exp_apply _loc (exp_glr_fun _loc "cache") [p] else p])

  let glr_binding = Decap.declare_grammar "glr_binding"
  let _ = Decap.set_grammar glr_binding (parser
     name:lowercase_ident arg:(pattern_lvl AsPat)? ty:{':' typexpr}? '=' r:glr_rules l:{_:and_kw glr_binding}?[[]] ->
										       (name,arg,ty,r)::l)
  let glr_struct_item =
    parser
    | let_kw cached:parser_prefix l:glr_binding ->
		  let rec fn = function
		      [] -> [], []
		    | (name,arg,ty,r)::l ->
		       let str1, str2 = fn l in
		       let pname = match ty, arg with
			   None, _-> <:pat< $lid:name$ >>
			 | Some ty, None -> <:pat< $lid:name$ : $ty$ >>
			 | Some ty, Some _ -> <:pat< $lid:name$ : ('type_of_arg -> $ty$) >>
		       in
		       let r = if cached then exp_apply _loc (exp_glr_fun _loc "cache") [r] else r in
		       (match arg with
			| None ->
			   <:structure< let $pname$ = Decap.declare_grammar $string:name$>> @ str1,
			   <:structure< let _ = Decap.set_grammar $lid:name$ $r$ >> @ str2
			| Some arg ->
			   let set_name = name ^ "__set__grammar" in
			   <:structure< let $pname$, $lid:set_name$ = Decap.grammar_family $string:name$>> @ str1,
			   <:structure< let _ = $lid:set_name$ (fun $arg$ -> $r$) >> @ str2)
		  in
		  let str1, str2 = fn l in
		  str1 @ str2

  let extra_expressions = glr_parser::extra_expressions
  let extra_structure = glr_struct_item::extra_structure

  let _ = add_reserved_id "parser"

  let glr_opt_expr =
    parser
      e:{ CHR('[') e:expression CHR(']') }? -> e

  let is_greedy c =
    let greedy = try
	ignore (Sys.getenv "GREEDY");
	true
      with Not_found -> false
    in
    parser
      CHR('~') -> false
    | CHR(c)   -> true
    | EMPTY -> greedy

  let fs = is_greedy '*'
  let fp = is_greedy '+'
  let fq = is_greedy '?'

  let glr_option =
    parser
    | CHR('*') s:fs e:glr_opt_expr -> `Fixpoint(s,e)
    | CHR('+') s:fp e:glr_opt_expr -> `Fixpoint1(s,e)
    | CHR('?') s:fq e:glr_opt_expr -> `Option(s,e)
    | EMPTY -> `Once

  let glr_sequence =
    parser
    | CHR('{') r:glr_rules CHR('}') -> true, r
    | STR("EOF") opt:glr_opt_expr ->
       let e = match opt with None -> exp_unit _loc | Some e -> e in
       (opt <> None, exp_apply _loc (exp_glr_fun _loc "eof") [e])
    | STR("EMPTY") opt:glr_opt_expr ->
       let e = match opt with None -> exp_unit _loc | Some e -> e in
       (opt <> None, exp_apply _loc (exp_glr_fun _loc "empty") [e])
    | STR("FAIL") e:(expression_lvl (next_exp App)) ->
       (false, exp_apply _loc (exp_glr_fun _loc "fail") [e])
    | STR("DEBUG") e:(expression_lvl (next_exp App)) ->
       (false, exp_apply _loc (exp_glr_fun _loc "debug") [e])
    | STR("ANY") ->
       (true, exp_glr_fun _loc "any")
    | STR("CHR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
       let o = match opt with None -> e | Some e -> e in
       (opt <> None, exp_apply _loc (exp_glr_fun _loc "char") [e; o])
    | c:char_literal opt:glr_opt_expr ->
       let e = loc_expr _loc_c (Pexp_constant (Const_char c)) in
       let o = match opt with None -> e | Some e -> e in
       (opt <> None, exp_apply _loc (exp_glr_fun _loc "char") [e; o])
    | STR("STR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
       let o = match opt with None -> e | Some e -> e in
       (opt <> None, exp_apply _loc (exp_glr_fun _loc "string") [e; o])
    | s:string_literal opt:glr_opt_expr ->
       (opt <> None,
        (if String.length s = 0 then
	  Decap.give_up "Empty string litteral in rule.";
	let e = loc_expr _loc_s (Pexp_constant (const_string s)) in
	let opt = match opt with None -> e | Some e -> e in
	exp_apply _loc (exp_glr_fun _loc "string") [e; opt]))
    | e:{ STR("RE") e:(expression_lvl (next_exp App))
        | s:regexp_literal -> loc_expr _loc_s (Pexp_constant (const_string s))}
       opt:glr_opt_expr ->
       let opt = match opt with
	 | None -> exp_apply _loc (exp_ident _loc "groupe") [exp_int _loc 0]
	 | Some e -> e
       in
       (match e.pexp_desc with
#ifversion >= 4.00
	  Pexp_ident { txt = Lident id } ->
#else
	  Pexp_ident (Lident id) ->
#endif
	  let id =
	    let l = String.length id in
	    if l > 3 && String.sub id (l - 3) 3 = "_re" then String.sub id 0 (l - 3) else id
	  in
	  (true, exp_lab_apply _loc (exp_glr_fun _loc "regexp") ["name", exp_string _loc id; "", e; "", exp_fun _loc "groupe" opt])
	| _ ->
	  (true, exp_apply _loc (exp_glr_fun _loc "regexp") [e; exp_fun _loc "groupe" opt]))

    | e:(expression_lvl Atom) -> (true, e)

  let glr_ident =
    parser
    | p:(pattern_lvl ConstrPat) CHR(':') ->
	(match p.ppat_desc with
#ifversion >= 4.00
	 | Ppat_alias(p, { txt = id }) -> (Some true, (id, Some p))
	 | Ppat_var { txt = id } -> (Some (id <> "_"), (id, None))
#else
	 | Ppat_alias(p, id ) -> (Some true, (id, Some p))
	 | Ppat_var id -> (Some (id <> "_"), (id, None))
#endif
         | Ppat_any -> (Some false, ("_", None))
	 | _ -> (Some true, ("_", Some p)))
    | EMPTY -> (None, ("_", None))

  let dash = Decap.black_box
    (fun str pos ->
       let c,str',pos' = Input.read str pos in
       if c = '-' then
         let c',_,_ = Input.read str' pos' in
         if c' = '>' then Decap.give_up "\'-\' expected"
         else (), str', pos'
      else
        Decap.give_up "\'-\' expexted"
    ) (Charset.singleton '-') None ("-")

  let glr_left_member =
    let f x y = match x with Some x -> x | None -> y in
    parser
    | i:{(cst',id):glr_ident (cst,s):glr_sequence opt:glr_option -> `Normal(id, (f cst' (opt <> `Once || cst)),s,opt)}
      l:{(cst',id):glr_ident (cst,s):glr_sequence opt:glr_option -> `Normal(id, (f cst' (opt <> `Once || cst)),s,opt) | dash -> `Ignore }* -> i::l

  let glr_let = Decap.declare_grammar "glr_let"
  let _ = Decap.set_grammar glr_let (
    parser
    | STR("let") r:rec_flag lbs:let_binding STR("in") l:glr_let -> (fun x -> loc_expr _loc (Pexp_let(r,lbs,l x)))
    | EMPTY -> (fun x -> x)
  )

  let glr_cond =
    parser
    | STR("when") e:expression -> Some e
    | EMPTY -> None

  let build_rule (_loc,occur_loc,def, l, condition, action) =
          let iter, action = match action with
	| Normal a -> false, a
	| Default -> false, default_action _loc l
	| DepSeq(def, cond, a) ->
           true, match cond with
		 | None -> def a
		 | Some cond ->
		    def (loc_expr _loc (Pexp_ifthenelse(cond,a,Some (exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]))))
      in

      let rec fn first ids l = match l with
	  [] -> assert false
	| `Ignore::ls -> assert false
	| `Normal(id,cst,e,opt,oc)::`Ignore::ls ->
	   let e =  exp_apply _loc (exp_glr_fun _loc "ignore_next_blank") [e] in
	   fn first ids (`Normal(id,cst,e,opt,oc)::ls)
	| [`Normal(id,_,e,opt,occur_loc_id)] ->
	   let e = apply_option _loc opt occur_loc_id e in
	   let f = match find_locate (), first && occur_loc with
	     | Some _, true -> "apply_position"
	     | _ -> "apply"
	   in
	   (match action.pexp_desc with
#ifversion >= 4.00
		Pexp_ident({ txt = Lident id'}) when fst id = id' && f = "apply" -> e
#else
		Pexp_ident(Lident id') when fst id = id' && f = "apply" -> e
#endif
	   | _ ->
	      exp_apply _loc (exp_glr_fun _loc f) [build_action _loc occur_loc ((id,occur_loc_id)::ids) action; e])
	| [`Normal(id,_,e,opt,occur_loc_id); `Normal(id',_,e',opt',occur_loc_id') ] ->
	   let e = apply_option _loc opt occur_loc_id e in
	   let e' = apply_option _loc opt' occur_loc_id' e' in
	   let f = match find_locate (), first && occur_loc with
	     | Some _, true -> "sequence_position"
	     | _ -> "sequence"
	   in
	   exp_apply _loc (exp_glr_fun _loc f) [e; e'; build_action _loc occur_loc
								    ((id,occur_loc_id)::(id',occur_loc_id')::ids) action]
	| `Normal(id,_,e,opt,occur_loc_id) :: ls ->
	   let e = apply_option _loc opt occur_loc_id e in
	   let f = match find_locate (), first && occur_loc with
	     | Some _, true -> "fsequence_position"
	     | _ -> "fsequence"
	   in
	   exp_apply _loc (exp_glr_fun _loc f) [e; fn false ((id,occur_loc_id)::ids) ls]
      in
      let res = fn true [] l in
      let res = if iter then exp_apply _loc (exp_glr_fun _loc "iter") [res] else res in
      def, condition, res

  let glr_action =
    parser
    | STR("->>") r:glr_rule -> let (a,b,c) = build_rule r in DepSeq (a,b,c)
    | arrow_re action:expression -> Normal action
    | EMPTY -> Default

  let _ = Decap.set_grammar glr_rule (
    parser
      | def:glr_let l:glr_left_member condition:glr_cond ->> let _ = push_frame () in action:glr_action ->
      let l = fst (List.fold_right (fun x (res,i) ->
	match x with
	  `Normal(("_",a),true,c,d) ->
	  (`Normal(("_default_"^string_of_int i,a),true,c,d,false)::res, i+1)
	| `Normal(id, b,c,d) ->
	   let occur_loc_id = fst id <> "_" && pop_location (fst id) in
	   (`Normal(id, b,c,d,occur_loc_id)::res, i)
	| `Ignore -> (`Ignore::res,i)) l ([], 0))
      in
      let occur_loc = pop_location "" in
      pop_frame ();
      (_loc, occur_loc, def, l, condition, action)
     )

  let apply_def_cond _loc arg =
    let (def,cond,e) = build_rule arg in
    match cond with
      None -> def e
    | Some c ->
      def (loc_expr _loc (Pexp_ifthenelse(c,e,Some (exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]))))

  let eq_expression_opt e1 e2 = match e1, e2 with
    | Some e1, Some e2 -> Compare.eq_expression e1 e2
    | None, None -> true
    | _ -> false

  let eq_pattern_opt e1 e2 = match e1, e2 with
    | Some e1, Some e2 -> Compare.eq_pattern e1 e2
    | None, None -> true
    | _ -> false

  let eq_option opt opt' = match opt, opt' with
    | `Once, `Once -> true
    | `Option(g,d), `Option(g',d') -> g = g' && eq_expression_opt d d'
    | `Fixpoint(g,d), `Fixpoint(g',d') -> g = g' && eq_expression_opt d d'
    | `Fixpoint1(g,d), `Fixpoint1(g',d') -> g = g' && eq_expression_opt d d'
    | _ -> false

  let factorisable elt1 elt2 = match elt1, elt2 with
    | (_loc,occur_loc,def, (`Normal(id,cst,e,opt,occur_loc_id)::_), condition, action),
      (_loc',occur_loc',def', (`Normal(id',cst',e',opt',occur_loc_id')::_), condition', action') ->
	  fst id = fst id' && cst = cst' &&
            Compare.eq_expression e e' &&
            eq_pattern_opt (snd id) (snd id') && eq_option opt opt' &&
	      Compare.eq_expression (def (exp_ident _loc "@")) (def' (exp_ident _loc "@"))
    | _ -> false

  let build_alternatives _loc comb ls =
    match ls with
    | [] -> exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]
    | [r] -> apply_def_cond _loc r
    | _::_ ->
      (match ls with
	elt1::elt2::_ ->
	  (*	  Format.eprintf "comparing\n  %a\nwith\n  %a\n%!" Pprintast.expression e Pprintast.expression e';*)
	  if factorisable elt1 elt2 then
	    Printf.eprintf "can left factorise\n%!"
      | _ -> ());
      let l = List.fold_right (fun r y ->
	let (def,cond,e) = build_rule r in
	match cond with
	  None ->
	    def (exp_Cons _loc e y)
        | Some c ->
	  def (loc_expr _loc (Pexp_let(Nonrecursive,[value_binding _loc (pat_ident _loc "y") y],
				       loc_expr _loc (Pexp_ifthenelse(c,exp_Cons _loc
					 e (exp_ident _loc "y"), Some (exp_ident _loc "y"))))))
      ) ls (exp_Nil _loc) in
      exp_apply _loc (exp_glr_fun _loc comb) [l]

  let alt = parser '|' x:'|'? -> x = None

  let _ = Decap.set_grammar glr_rules (
    parser
    | a:alt? r:glr_rule rs:{ a:alt r:glr_rule -> (a,r)}** ->
      let rec fn a ls = match a, ls with
	  None, (a,_)::ls -> fn (Some a) ls
	| Some a', (a,_)::ls when a <> a' -> Decap.give_up "Inhomogenous alternatives"
	| _, _::ls -> fn a ls
	| _, [] -> a
      in
      let a = fn a rs in
      let ls = r::List.map snd rs in
      let f = if a = Some false then "alternatives'" else "alternatives" in
      build_alternatives _loc f ls)


end
