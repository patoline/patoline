open Asttypes
open Parsetree
open Longident
open Pa_ocaml_prelude

let _ = parser_locate locate merge

type ('a,'b) action =
  | Default 
  | Normal of 'a
  | DepSeq of 'b

let do_locate = ref None

let exp_int _loc n =
  loc_expr _loc (Pexp_constant (Const_int n))

let exp_string _loc n =
  loc_expr _loc (Pexp_constant (const_string n))

let exp_None _loc =
  let cnone = { txt = Lident "None"; loc = _loc } in
  loc_expr _loc (pexp_construct(cnone, None))

let exp_Some _loc a =
  let csome = { txt = Lident "Some"; loc = _loc } in
  loc_expr _loc (pexp_construct(csome, Some a))

let exp_unit _loc =
  let cunit = { txt = Lident "()"; loc = _loc } in
  loc_expr _loc (pexp_construct(cunit, None))

let exp_tuple _loc l = 
  loc_expr _loc (Pexp_tuple l)

let exp_Nil _loc =
  let cnil = { txt = Lident "[]"; loc = _loc } in
  loc_expr _loc (pexp_construct(cnil, None))

let exp_Cons _loc a l =
  loc_expr _loc (pexp_construct({ txt = Lident "::"; loc = _loc}, Some (exp_tuple _loc [a;l])))

let exp_list _loc l =
  List.fold_right (exp_Cons _loc) l (exp_Nil _loc)

let exp_ident _loc id =
  loc_expr _loc (Pexp_ident { txt = Lident id; loc = _loc })

let pat_ident _loc id =
  loc_pat _loc (Ppat_var { txt = id; loc = _loc })

let exp_apply _loc f l = 
  loc_expr _loc (Pexp_apply(f, List.map (fun x -> "", x) l))

let exp_lab_apply _loc f l = 
  loc_expr _loc (Pexp_apply(f, l))

let exp_Some_fun _loc =
  loc_expr _loc (pexp_fun("", None, pat_ident _loc "x", (exp_Some _loc (exp_ident _loc "x"))))

let exp_fun _loc id e =
  loc_expr _loc (pexp_fun("", None, pat_ident _loc id, e))

let exp_glr_fun _loc f =
  loc_expr _loc (Pexp_ident({ txt = Ldot(Lident "Glr",f); loc = _loc } ))

let exp_list_fun _loc f =
  loc_expr _loc (Pexp_ident({ txt = Ldot(Lident "List",f); loc = _loc } ))

let exp_str_fun _loc f =
  loc_expr _loc (Pexp_ident({ txt = Ldot(Lident "Str",f); loc = _loc } ))

let exp_Cons_fun _loc =
  exp_fun _loc "x" (exp_fun _loc "l" (exp_Cons _loc (exp_ident _loc "x") (exp_ident _loc "l")))

let exp_Cons_rev_fun _loc =
  exp_fun _loc "x" (exp_fun _loc "l" (exp_Cons _loc (exp_ident _loc "x") (exp_apply _loc (exp_list_fun _loc "rev") [exp_ident _loc "l"])))

let mkpatt _loc (id, p) = match p, !do_locate with 
    None, _ -> pat_ident _loc id
  | Some p, None -> loc_pat _loc (Ppat_alias (p, { txt = id; loc = _loc }))
  | Some p, Some _ -> 
     loc_pat _loc (Ppat_alias (loc_pat _loc (Ppat_tuple[loc_pat _loc Ppat_any; p]) , { txt = id; loc = _loc }))

let rec apply _loc ids e =
  let ids = List.mapi (fun i (id,x) ->
		       ((if id = "_" then "_unnamed_" ^ string_of_int i else id), x)) ids in
  let e = match !do_locate with
      None -> e
    | Some(_,merge) ->
      match ids with
      | [] -> e
      | [id,_] ->
	 loc_expr _loc (Pexp_let(Nonrecursive, [
	   value_binding _loc (pat_ident _loc "_loc") (exp_ident _loc ("_loc_"^id))], e))
      | ids ->
	 let all_loc = List.map (fun (id, _) -> exp_ident _loc ("_loc_"^id)) ids in
	loc_expr _loc (Pexp_let(Nonrecursive, [
	  value_binding _loc (pat_ident _loc "_loc")
	  (loc_expr _loc (Pexp_apply(merge, [
	    "", exp_list _loc all_loc])))], e))
  in
  List.fold_left (fun e id -> 
    match !do_locate with
      None ->
      loc_expr _loc (pexp_fun("", None, mkpatt _loc id, e))
    | Some(_) ->  
      loc_expr _loc (
	pexp_fun("", None,
	  mkpatt _loc id,
	  loc_expr _loc (Pexp_let(Nonrecursive, 
	    [value_binding _loc (loc_pat _loc (Ppat_tuple([
		loc_pat _loc (Ppat_var { txt = "_loc_"^fst id; loc = _loc });
		loc_pat _loc (Ppat_var { txt = fst id; loc = _loc })])))
	     (loc_expr _loc (Pexp_ident({ txt = Lident (fst id); loc = _loc})))], 
	    e))))
  ) e (List.rev ids)

let filter _loc r =
  match !do_locate with
    None -> r
  | Some(f,_) -> 
     loc_expr _loc (Pexp_apply(f,["", r]))


let apply_option _loc opt e = 
  filter _loc (match opt with
    `Once -> e
  | `Option d ->
    (match d with 
       None ->
       exp_apply _loc (exp_glr_fun _loc "option")
	  [exp_None _loc;
	   exp_apply _loc (exp_glr_fun _loc "apply")
	     [exp_Some_fun _loc; e]]
     | Some d ->
	exp_apply _loc (exp_glr_fun _loc "option") [d; e])
  | `OptionPrime d ->
    (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "option'")
	  [exp_None _loc;
	   exp_apply _loc (exp_glr_fun _loc "apply")
	     [exp_Some_fun _loc; e]]
    | Some d ->
	exp_apply _loc (exp_glr_fun _loc "option") [d; e])
  | `Fixpoint d ->
    (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "apply")
	  [exp_list_fun _loc "rev";
	   exp_apply _loc (exp_glr_fun _loc "fixpoint")
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
		[exp_Cons_fun _loc; e]]]
    | Some d ->
       exp_apply _loc (exp_glr_fun _loc "fixpoint") [d; e])
  | `FixpointPrime d ->
    (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "apply")
	  [exp_list_fun _loc "rev";
	   exp_apply _loc (exp_glr_fun _loc "fixpoint'")
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
		[exp_Cons_fun _loc; e]]]
    | Some d ->
       exp_apply _loc (exp_glr_fun _loc "fixpoint") [d; e])
  | `Fixpoint1 d ->
   (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "sequence")
	  [e;
	   exp_apply _loc (exp_glr_fun _loc "fixpoint")
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
		[exp_Cons_fun _loc; e]];
	   exp_Cons_rev_fun _loc]
   | Some d ->
      exp_apply _loc (exp_glr_fun _loc "dependent_sequence")
	 [e;
	  exp_fun _loc "x"
	    (exp_apply _loc (exp_glr_fun _loc "fixpoint") 
	       [exp_apply _loc (exp_ident _loc "x") [d];
		e])])
  | `Fixpoint1Prime d ->
   (match d with None ->
      exp_apply _loc (exp_glr_fun _loc "sequence")
        [e;
	 exp_apply _loc (exp_glr_fun _loc "fixpoint'")
	   [exp_Nil _loc;
	    exp_apply _loc (exp_glr_fun _loc "apply")
		      [exp_Cons_fun _loc; e]];
	 exp_Cons_rev_fun _loc]
   | Some d ->
      exp_apply _loc (exp_glr_fun _loc "dependent_sequence")
		[e;
		 exp_fun _loc "x"
			 (exp_apply _loc (exp_glr_fun _loc "fixpoint'") 
				    [exp_apply _loc (exp_ident _loc "x") [d];
				     e])])
   )

let apply_list_option _loc opt e = 
  filter _loc (match opt with
    `Once -> e
  | `Option d ->
    (match d with None ->
      exp_apply _loc (exp_glr_fun _loc "option") [exp_Nil _loc; e]
    | Some d ->
      exp_apply _loc (exp_glr_fun _loc "option") [exp_list _loc [d]; e])
  | `OptionPrime d ->
    (match d with None ->
      exp_apply _loc (exp_glr_fun _loc "option'") [exp_Nil _loc; e]
    | Some d ->
      exp_apply _loc (exp_glr_fun _loc "option'") [exp_list _loc [d]; e])
  | `Fixpoint d ->
    (match d with None ->
      exp_apply _loc (exp_apply _loc (exp_list_fun _loc "map") [exp_list_fun _loc "rev"])
	 [exp_apply _loc (exp_glr_fun _loc "list_fixpoint")
            [exp_Nil _loc;
	     exp_apply _loc (exp_glr_fun _loc "apply")
	        [exp_fun _loc "x" (exp_fun _loc "l" (exp_apply _loc (exp_list_fun _loc "map") [
		    exp_fun _loc "y" (exp_Cons _loc (exp_ident _loc "y") (exp_ident _loc "l"));
		     exp_ident _loc "x"]));
		 e]]]
    | Some d ->
      exp_apply _loc (exp_glr_fun _loc "list_fixpoint") [d; e])
  | `FixpointPrime d ->
    (match d with None ->
      exp_apply _loc (exp_apply _loc (exp_list_fun _loc "map") [exp_list_fun _loc "rev"])
	 [exp_apply _loc (exp_glr_fun _loc "list_fixpoint'")
            [exp_Nil _loc;
	     exp_apply _loc (exp_glr_fun _loc "apply")
	        [exp_fun _loc "x" (exp_fun _loc "l" (exp_apply _loc (exp_list_fun _loc "map") [
		    exp_fun _loc "y" (exp_Cons _loc (exp_ident _loc "y") (exp_ident _loc "l"));
		     exp_ident _loc "x"]));
		 e]]]
    | Some d ->
      exp_apply _loc (exp_glr_fun _loc "list_fixpoint'") [d; e])
 | `Fixpoint1 d ->
   (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "list_sequence")
	  [e;
	   exp_apply _loc (exp_glr_fun _loc "list_fixpoint")
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
	        [exp_fun _loc "x" (exp_fun _loc "l" (exp_apply _loc (exp_list_fun _loc "map") [
		   exp_fun _loc "y" (exp_Cons _loc (exp_ident _loc "y") (exp_ident _loc "l"));
		   exp_ident _loc "x"]));
		 e]];
	   exp_Cons_rev_fun _loc]
   | Some d ->
      exp_apply _loc (exp_glr_fun _loc "list_dependent_sequence")
	 [e;
	  exp_fun _loc "x"
	    (exp_apply _loc (exp_glr_fun _loc "list_fixpoint") 
	       [exp_apply _loc (exp_ident _loc "x") [d];
		e])])
 | `Fixpoint1Prime d ->
   (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "list_sequence")
	  [e;
	   exp_apply _loc (exp_glr_fun _loc "list_fixpoint'")
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
	        [exp_fun _loc "x" (exp_fun _loc "l" (exp_apply _loc (exp_list_fun _loc "map") [
		   exp_fun _loc "y" (exp_Cons _loc (exp_ident _loc "y") (exp_ident _loc "l"));
		   exp_ident _loc "x"]));
		 e]];
	   exp_Cons_rev_fun _loc]
   | Some d ->
      exp_apply _loc (exp_glr_fun _loc "list_dependent_sequence")
	 [e;
	  exp_fun _loc "x"
	    (exp_apply _loc (exp_glr_fun _loc "list_fixpoint'") 
	       [exp_apply _loc (exp_ident _loc "x") [d];
		e])])
	      )

let default_action _loc l =
  let l = List.filter (function (("_",_),_,_) -> false | _ -> true) l in
  let l = List.map (fun ((id,_),_,_) -> exp_ident _loc id) l in
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

  let glr_rules = Glr.declare_grammar "glr_rules"
  let glr_list_rules = Glr.declare_grammar "glr_list_rules"
  let glr_rule = Glr.declare_grammar "glr_rule"
  let glr_list_rule = Glr.declare_grammar "glr_list_rule"

  let glr_parser = 
    parser
    | STR("parser_locate") filter2:(expression_lvl (next_exp App))
         merge2:(expression_lvl (next_exp App)) ->
      (do_locate := Some(filter2,merge2); (Atom, exp_unit _loc))
    | parser_kw p:glr_rules -> (Atom, p)
    | parser_kw CHR('*') p:glr_list_rules -> (Atom, p)

  let extra_expressions = glr_parser::extra_expressions

  let glr_opt_expr = 
    parser
      e:{ CHR('[') e:expression CHR(']') }? -> e

  let glr_option =
    parser
    | CHR('*') CHR('*') e:glr_opt_expr -> `FixpointPrime e
    | CHR('*') e:glr_opt_expr -> `Fixpoint e
    | CHR('+') CHR('+') e:glr_opt_expr -> `Fixpoint1Prime e
    | CHR('+') e:glr_opt_expr -> `Fixpoint1 e
    | CHR('?') CHR('?') e:glr_opt_expr -> `OptionPrime e
    | CHR('?') e:glr_opt_expr -> `Option e
    | EMPTY -> `Once

  let glr_sequence =
    parser
    | CHR('{') r:glr_rules CHR('}') -> r
    | STR("EOF") opt:glr_opt_expr ->
       let e = match opt with None -> exp_unit _loc | Some e -> e in
       exp_apply _loc (exp_glr_fun _loc "eof") [e]
    | STR("EMPTY") opt:glr_opt_expr ->
       let e = match opt with None -> exp_unit _loc | Some e -> e in
       exp_apply _loc (exp_glr_fun _loc "empty") [e]
    | STR("FAIL") e:(expression_lvl (next_exp App)) ->
       exp_apply _loc (exp_glr_fun _loc "fail") [e]
    | STR("DEBUG") e:(expression_lvl (next_exp App)) ->
       exp_apply _loc (exp_glr_fun _loc "debug") [e]
    | STR("CHR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
       let opt = match opt with None -> exp_unit _loc | Some e -> e in
       exp_apply _loc (exp_glr_fun _loc "char") [e; opt]
    | STR("STR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
       let opt = match opt with None -> exp_unit _loc | Some e -> e in
       exp_apply _loc (exp_glr_fun _loc "string") [e; opt]
    | STR("RE") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
       let opt = match opt with
	 | None -> exp_apply _loc (exp_ident _loc "groupe") [exp_int _loc 0]
	 | Some e -> e
       in
       (match e.pexp_desc with
	  Pexp_ident { txt = Lident id } ->
	  let id = 
	    let l = String.length id in
	    if l > 3 && String.sub id (l - 3) 3 = "_re" then String.sub id 0 (l - 3) else id
	  in
	  exp_lab_apply _loc (exp_glr_fun _loc "regexp") ["name", exp_string _loc id; "", e; "", exp_fun _loc "groupe" opt] 
	| _ -> 
	   exp_apply _loc (exp_glr_fun _loc "regexp") [e; exp_fun _loc "groupe" opt])
	 
    | e:(expression_lvl Atom) -> e

let glr_list_sequence =
  parser
  | CHR('{') r:glr_list_rules CHR('}') -> r
  | STR("EOF") opt:glr_opt_expr ->
     let e = match opt with None -> exp_unit _loc | Some e -> e in
     exp_apply _loc (exp_glr_fun _loc "list_eof") [e]
  | STR("EMPTY") opt:glr_opt_expr ->
     let e = match opt with None -> exp_unit _loc | Some e -> e in
     exp_apply _loc (exp_glr_fun _loc "list_empty") [e]
  | STR("FAIL") e:(expression_lvl (next_exp App)) ->
     exp_apply _loc (exp_glr_fun _loc "list_fail") [e]
  | STR("DEBUG") e:(expression_lvl (next_exp App)) ->
     exp_apply _loc (exp_glr_fun _loc "list_debug") [e]
  | STR("CHR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
     let opt = match opt with None -> exp_unit _loc | Some e -> e in
     exp_apply _loc (exp_glr_fun _loc "list_char") [e; opt]
  | STR("STR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
     let opt = match opt with None -> exp_unit _loc | Some e -> e in
     exp_apply _loc (exp_glr_fun _loc "list_string") [e; opt]
  | STR("RE") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
      let opt = match opt with
	| None -> exp_apply _loc (exp_ident _loc "groupe") [exp_int _loc 0]
	| Some e -> e
      in
      (match e.pexp_desc with
	Pexp_ident { txt = Lident id } ->
	let id = 
	  let l = String.length id in
	  if l > 3 && String.sub id (l - 3) 3 = "_re" then String.sub id 0 (l - 3) else id
	in 
	exp_lab_apply _loc (exp_glr_fun _loc "list_regexp") ["name", exp_string _loc id; "", e; "", exp_fun _loc "groupe" opt] 
      | _ -> 
	exp_apply _loc (exp_glr_fun _loc "list_regexp") [e; exp_fun _loc "groupe" opt])

  | e:(expression_lvl Atom) -> e

  let glr_ident =
    parser
    | p:(pattern_lvl ConstrPat) CHR(':') ->
	(match p.ppat_desc with
	 | Ppat_alias(p, { txt = id }) -> id, Some p
	 | Ppat_var { txt = id } -> id, None
	 | _ -> "_", Some p)
    | EMPTY -> ("_", None)

  let glr_left_member =
    parser
    | l:{ id: glr_ident s:glr_sequence opt:glr_option }+ -> l

  let glr_list_left_member =
    parser
    | l:{ id: glr_ident s:glr_list_sequence opt:glr_option }+ -> l

  let glr_let = Glr.declare_grammar "glr_let" 
  let _ = Glr.set_grammar glr_let (
    parser
    | STR("let") r:rec_flag lbs:let_binding STR("in") l:glr_let -> (fun x -> loc_expr _loc (Pexp_let(r,lbs,l x)))
    | EMPTY -> (fun x -> x)
  )
 
  let glr_cond =
    parser
    | STR("when") e:expression -> Some e
    | EMPTY -> None

  let glr_action =
    parser
    | STR("->>") (def, cond, r):glr_rule -> DepSeq (def, cond, r)
    | STR("->") action:expression -> Normal action
    | EMPTY -> Default

  let glr_list_action =
    parser
    | STR("->>") (def, cond, r):glr_list_rule -> DepSeq (def, cond, r)
    | STR("->") action:(expression_lvl (next_exp Disj)) -> Normal action
    | EMPTY -> Default

  let _ = Glr.set_grammar glr_rule (
    parser
    | def:glr_let l:glr_left_member condition:glr_cond action:glr_action ->
	let iter, action = match action with
	Normal a -> false, a
    | Default -> false, default_action _loc l
    | DepSeq(def, cond, a) ->
        true, match cond with
		 None -> def a 
	       | Some cond -> def (loc_expr _loc (Pexp_ifthenelse(cond,a,Some (exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]))))
    in
    let rec fn ids l = match l with
      [] -> assert false
    | [id,e,opt] ->
      let e = apply_option _loc opt e in
      exp_apply _loc (exp_glr_fun _loc "apply") [apply _loc (id::ids) action; e]
    | [ (id,e,opt); (id',e',opt') ] ->
      let e = apply_option _loc opt e in
      let e' = apply_option _loc opt' e' in
      exp_apply _loc (exp_glr_fun _loc "sequence") [e';e;apply _loc (id'::id::ids) action]
    | (id,e,opt) :: ls ->
      let e = apply_option _loc opt e in      
      exp_apply _loc (exp_glr_fun _loc "sequence") [fn (id::ids) ls; e; exp_fun _loc "x" (exp_ident _loc "x")]
    in
    let res = fn [] (List.rev l) in
    let res = if iter then exp_apply _loc (exp_glr_fun _loc "iter") [res] else res in
    def, condition, res
    )

  let _ = Glr.set_grammar glr_list_rule (
    parser
    | def:glr_let l:glr_list_left_member condition:glr_cond action:glr_list_action ->
    let iter, action = match action with
	Normal a -> false, a
      | Default -> false, default_action _loc l
      | DepSeq(def, cond, a) ->
	 true, match cond with
		 None -> def a 
	       | Some cond -> def (loc_expr _loc (Pexp_ifthenelse(cond,a,Some (exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]))))
    in
    let rec fn ids l = match l with
      [] -> assert false
    | [id,e,opt] ->
      let e = apply_list_option _loc opt e in
      exp_apply _loc (exp_glr_fun _loc "apply") [apply _loc (id::ids) action; e]
    | [ (id,e,opt); (id',e',opt') ] ->
      let e = apply_list_option _loc opt e in
      let e' = apply_list_option _loc opt' e' in
      exp_apply _loc (exp_glr_fun _loc "list_sequence") [e';e;apply _loc (id'::id::ids) action]
    | (id,e,opt) :: ls ->
      let e = apply_list_option _loc opt e in      
      exp_apply _loc (exp_glr_fun _loc "list_sequence") [fn (id::ids) ls; e; exp_fun _loc "x" (exp_ident _loc "x")]
    in
    let res = fn [] (List.rev l) in
    let res = if iter then exp_apply _loc (exp_glr_fun _loc "iter_list") [res] else res in
    def, condition, res
    )

  let glr_rules_aux = 
    parser
    | CHR('|')? r:glr_rule rs:{ CHR('|') r:glr_rule}* -> 
      (match rs with
      | [] -> r
      | l ->
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def (exp_Cons _loc x y)
          | Some c -> 
	      def (loc_expr _loc (Pexp_let(Nonrecursive,[value_binding _loc (pat_ident _loc "y") y], 
		   loc_expr _loc (Pexp_ifthenelse(c,exp_Cons _loc 
			 x (exp_ident _loc "y"), Some (exp_ident _loc "y"))))))
	) (r::l) (exp_Nil _loc) in
	(fun x -> x), None, (exp_apply _loc (exp_glr_fun _loc "alternatives") [l]))

 let glr_list_rules_aux =
   parser
     CHR('|')? r:glr_list_rule rs:{ CHR('|') r:glr_list_rule}* -> 
      (match rs with
      | [] -> r
      | l ->
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def (exp_Cons _loc x y)
          | Some c -> 
	      def (loc_expr _loc (Pexp_let(Nonrecursive,[value_binding _loc (pat_ident _loc "y") y], 
		   loc_expr _loc (Pexp_ifthenelse(c,exp_Cons _loc 
			 x (exp_ident _loc "y"), Some (exp_ident _loc "y"))))))
	) (r::l) (exp_Nil _loc) in
	(fun x -> x), None, (exp_apply _loc (exp_glr_fun _loc "list_alternatives") [l]))
 
  (* FIXME: use only | | after bootstrap *)
  let _ = Glr.set_grammar glr_rules (
    parser
    { CHR('|') CHR('|') | else_kw}? r:glr_rules_aux rs:{ { CHR('|') CHR('|') | else_kw } r:glr_rules_aux}* -> 
      (match r,rs with
      | (def,cond,e),  [] ->
	(match cond with
	  None -> def e
        | Some c -> 
	  loc_expr _loc (Pexp_ifthenelse(c,e,Some (exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]))))
      | r, l ->
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def (exp_Cons _loc x y)
          | Some c -> 
	      def (loc_expr _loc (Pexp_let(Nonrecursive,[value_binding _loc (pat_ident _loc "y") y], 
		   loc_expr _loc (Pexp_ifthenelse(c,exp_Cons _loc 
			 x (exp_ident _loc "y"), Some (exp_ident _loc "y"))))))
	) (r::l) (exp_Nil _loc) in
	exp_apply _loc (exp_glr_fun _loc "alternatives'") [l])
  )

  let _ = Glr.set_grammar glr_list_rules (
    parser
    { CHR('|') CHR('|') }? r:glr_list_rules_aux rs:{ { CHR('|') CHR('|') } r:glr_list_rules_aux}* -> 
      (match r,rs with
      | (def,cond,e),  [] ->
	(match cond with
	  None -> def e
        | Some c -> 
	  loc_expr _loc (Pexp_ifthenelse(c,e,Some (exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]))))
      | r, l ->
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def (exp_Cons _loc x y)
          | Some c -> 
	      def (loc_expr _loc (Pexp_let(Nonrecursive,[value_binding _loc (pat_ident _loc "y") y], 
		   loc_expr _loc (Pexp_ifthenelse(c,exp_Cons _loc 
			 x (exp_ident _loc "y"), Some (exp_ident _loc "y"))))))
	) (r::l) (exp_Nil _loc) in
	exp_apply _loc (exp_glr_fun _loc "list_alternatives'") [l])
  )

end

let _ = register_extension (module Ext)
