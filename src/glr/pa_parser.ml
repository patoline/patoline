open Asttypes
open Parsetree
open Longident
open Pa_ocaml_prelude

let _ = parser_locate locate (*merge*) locate2

type action =
  | Default 
  | Normal of expression
  | DepSeq of ((expression -> expression) * expression option * expression)

let do_locate = ref None

let exp_int _loc n =
  loc_expr _loc (Pexp_constant (Const_int n))

let exp_string _loc n =
  loc_expr _loc (Pexp_constant (const_string n))

let exp_None _loc =
  let cnone = id_loc (Lident "None") _loc in
  loc_expr _loc (pexp_construct(cnone, None))

let exp_Some _loc a =
  let csome = id_loc (Lident "Some") _loc in
  loc_expr _loc (pexp_construct(csome, Some a))

let exp_unit _loc =
  let cunit = id_loc (Lident "()") _loc in
  loc_expr _loc (pexp_construct(cunit, None))

let exp_tuple _loc l = 
  loc_expr _loc (Pexp_tuple l)

let exp_Nil _loc =
  let cnil = id_loc (Lident "[]") _loc in
  loc_expr _loc (pexp_construct(cnil, None))

let exp_Cons _loc a l =
  loc_expr _loc (pexp_construct(id_loc (Lident "::") _loc, Some (exp_tuple _loc [a;l])))

let exp_list _loc l =
  List.fold_right (exp_Cons _loc) l (exp_Nil _loc)

let exp_ident _loc id =
  loc_expr _loc (Pexp_ident (id_loc (Lident id) _loc ))

let pat_ident _loc id =
  loc_pat _loc (Ppat_var (id_loc id _loc))

let exp_apply _loc f l = 
  loc_expr _loc (Pexp_apply(f, List.map (fun x -> "", x) l))

let exp_lab_apply _loc f l = 
  loc_expr _loc (Pexp_apply(f, l))

let exp_Some_fun _loc =
  loc_expr _loc (pexp_fun("", None, pat_ident _loc "x", (exp_Some _loc (exp_ident _loc "x"))))

let exp_fun _loc id e =
  loc_expr _loc (pexp_fun("", None, pat_ident _loc id, e))

let exp_app _loc =
  exp_fun _loc "x" (exp_fun _loc "y" (exp_apply _loc (exp_ident _loc "y") [exp_ident _loc "x"]))

let exp_glr_fun _loc f =
  loc_expr _loc (Pexp_ident((id_loc (Ldot(Lident "Decap",f)) _loc) ))

let exp_list_fun _loc f =
  loc_expr _loc (Pexp_ident((id_loc (Ldot(Lident "List",f)) _loc) ))

let exp_str_fun _loc f =
  loc_expr _loc (Pexp_ident((id_loc (Ldot(Lident "Str",f)) _loc) ))

let exp_Cons_fun _loc =
  exp_fun _loc "x" (exp_fun _loc "l" (exp_Cons _loc (exp_ident _loc "x") (exp_ident _loc "l")))

let exp_Cons_rev_fun _loc =
  exp_fun _loc "x" (exp_fun _loc "l" (exp_Cons _loc (exp_ident _loc "x") (exp_apply _loc (exp_list_fun _loc "rev") [exp_ident _loc "l"])))

let ppat_alias _loc p id =
  if id = "_" then p else
    loc_pat _loc (Ppat_alias (p, (id_loc (id) _loc)))

let mkpatt _loc (id, p) = match p, !do_locate with 
    None, _ -> pat_ident _loc id
  | Some p, None -> ppat_alias _loc p id
  | Some p, Some _ -> 
     ppat_alias _loc (loc_pat _loc (Ppat_tuple[loc_pat _loc Ppat_any; p])) id

let mkpatt' _loc (id,p) =  match p with 
    None -> pat_ident _loc id
  | Some p -> ppat_alias _loc p id

let filter _loc visible r =
  match !do_locate, visible with
  | Some(f,_), true -> 
     loc_expr _loc (Pexp_apply(f,["", r]))
  | _ -> r


let rec build_action _loc occur_loc ids e =
  let e = match !do_locate, occur_loc with
    | Some(_,locate2), true ->
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
    match !do_locate, visible with
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
  | `Option(strict,d) ->
     let f = if strict then "option'" else "option" in
    (match d with 
       None ->
       exp_apply _loc (exp_glr_fun _loc f)
	  [exp_None _loc;
	   exp_apply _loc (exp_glr_fun _loc "apply")
	     [exp_Some_fun _loc; e]]
     | Some d ->
	exp_apply _loc (exp_glr_fun _loc f) [d; e])
  | `Fixpoint(strict,d) ->
     let f = if strict then "fixpoint'" else "fixpoint" in
    (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "apply")
	  [exp_list_fun _loc "rev";
	   exp_apply _loc (exp_glr_fun _loc f)
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
		[exp_Cons_fun _loc; e]]]
    | Some d ->
       exp_apply _loc (exp_glr_fun _loc f) [d; e])
  | `Fixpoint1(strict,d) ->
     let f = if strict then "fixpoint'" else "fixpoint" in
   (match d with None ->
       exp_apply _loc (exp_glr_fun _loc "sequence")
	  [e;
	   exp_apply _loc (exp_glr_fun _loc f)
	     [exp_Nil _loc;
	      exp_apply _loc (exp_glr_fun _loc "apply")
		[exp_Cons_fun _loc; e]];
	   exp_Cons_rev_fun _loc]
   | Some d ->
      exp_apply _loc (exp_glr_fun _loc "dependent_sequence")
	 [e;
	  exp_fun _loc "x"
	    (exp_apply _loc (exp_glr_fun _loc f) 
	       [exp_apply _loc (exp_ident _loc "x") [d];
		e])])
   )

let default_action _loc l =
  let l = List.filter (function `Normal(("_",_),_,_) -> false | `Ignore -> false | _ -> true) l in
  let l = List.map (function `Normal((id,_),_,_) -> exp_ident _loc id | _ -> assert false) l in
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

  let glr_parser = 
    parser
    | STR("parser_locate") filter2:(expression_lvl (next_exp App))
         merge2:(expression_lvl (next_exp App)) ->
      (do_locate := Some(filter2,merge2); (Atom, exp_unit _loc))
    | parser_kw p:glr_rules -> (Atom, p)
    | parser_kw CHR('*') p:glr_rules -> (Atom, exp_apply _loc (exp_glr_fun _loc "lists") [p])

  let extra_expressions = glr_parser::extra_expressions

  let glr_opt_expr = 
    parser
      e:{ CHR('[') e:expression CHR(']') }? -> e

  let glr_option =
    parser
    | CHR('*') strict:CHR('*')? e:glr_opt_expr -> `Fixpoint(strict<>None,e)
    | CHR('+') strict:CHR('+')? e:glr_opt_expr -> `Fixpoint1(strict<>None,e)
    | CHR('?') strict:CHR('?')? e:glr_opt_expr -> `Option(strict<>None,e)
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
    | STR("ANY") ->
       exp_glr_fun _loc "any"
    | STR("CHR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
       let opt = match opt with None -> e | Some e -> e in
       exp_apply _loc (exp_glr_fun _loc "char") [e; opt]
    | STR("STR") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
       let opt = match opt with None -> e | Some e -> e in
       exp_apply _loc (exp_glr_fun _loc "string") [e; opt]
    | STR("RE") e:(expression_lvl (next_exp App)) opt:glr_opt_expr ->
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
	  exp_lab_apply _loc (exp_glr_fun _loc "regexp") ["name", exp_string _loc id; "", e; "", exp_fun _loc "groupe" opt] 
	| _ -> 
	   exp_apply _loc (exp_glr_fun _loc "regexp") [e; exp_fun _loc "groupe" opt])
	 
    | e:(expression_lvl Atom) -> e

  let glr_ident =
    parser
    | p:(pattern_lvl ConstrPat) CHR(':') ->
	(match p.ppat_desc with
#ifversion >= 4.00
	 | Ppat_alias(p, { txt = id }) -> id, Some p
	 | Ppat_var { txt = id } -> id, None
#else
	 | Ppat_alias(p, id ) -> id, Some p
	 | Ppat_var id -> id, None
#endif
	 | _ -> "_", Some p)
    | EMPTY -> ("_", None)

  let dash = Decap.black_box 
    (fun str pos ->
       let c,str',pos' = Input.read str pos in
       if c = '-' then
         let c',_,_ = Input.read str' pos' in
         if c' = '>' then raise (Decap.Give_up "\'-\' expected")
         else (), str', pos'
      else
        raise (Decap.Give_up "\'-\' expexted")
    ) (Charset.singleton '-') false ("-")

  let glr_left_member =
    parser
    | i:{id: glr_ident s:glr_sequence opt:glr_option -> `Normal(id,s,opt)} 
      l:{ id: glr_ident s:glr_sequence opt:glr_option -> `Normal(id,s,opt) | dash -> `Ignore }* -> i::l

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

  let glr_action =
    parser
    | STR("->>") (def, cond, r):glr_rule -> DepSeq (def, cond, r)
    | STR("->") action:expression -> Normal action
    | EMPTY -> Default

  let _ = Decap.set_grammar glr_rule (
    parser
    | def:glr_let l:glr_left_member condition:glr_cond ->> let _ = push_frame () in action:glr_action ->
      let iter, action = match action with
	| Normal a -> false, a
	| Default -> false, default_action _loc l
	| DepSeq(def, cond, a) ->
           true, match cond with
		 | None -> def a 
		 | Some cond ->
		    def (loc_expr _loc (Pexp_ifthenelse(cond,a,Some (exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]))))
      in
      let occur_loc = pop_location "" in
      let rec fn first ids l = match l with
	  [] -> assert false
	| `Ignore::ls -> assert false
	| `Normal(id,e,opt)::`Ignore::ls -> 
	   let e =  exp_apply _loc (exp_glr_fun _loc "ignore_next_blank") [e] in
	   fn first ids (`Normal(id,e,opt)::ls)
	| [`Normal(id,e,opt)] ->
	   let occur_loc_id = fst id <> "_" && pop_location (fst id) in
	   let e = apply_option _loc opt occur_loc_id e in
	   let f = match !do_locate, first && occur_loc with
	     | Some _, true -> "apply_position"
	     | _ -> "apply"
	   in
	   exp_apply _loc (exp_glr_fun _loc f) [build_action _loc occur_loc ((id,occur_loc_id)::ids) action; e]
	| [`Normal(id,e,opt); `Normal(id',e',opt') ] ->
	   let occur_loc_id = fst id <> "_" && pop_location (fst id) in
	   let occur_loc_id' = fst id' <> "_" && pop_location (fst id') in
	   let e = apply_option _loc opt occur_loc_id e in
	   let e' = apply_option _loc opt' occur_loc_id' e' in
	   let f = match !do_locate, first && occur_loc with
	     | Some _, true -> "sequence_position"
	     | _ -> "sequence"
	   in
	   exp_apply _loc (exp_glr_fun _loc f) [e; e'; build_action _loc occur_loc 
								    ((id,occur_loc_id)::(id',occur_loc_id')::ids) action]
	| `Normal(id,e,opt) :: ls ->
	   let occur_loc_id = fst id <> "_" && pop_location (fst id) in
	   let e = apply_option _loc opt occur_loc_id e in 
	   let f = match !do_locate, first && occur_loc with
	     | Some _, true -> "fsequence_position"
	     | _ -> "fsequence"
	   in
	   exp_apply _loc (exp_glr_fun _loc f) [e; fn false ((id,occur_loc_id)::ids) ls]
      in
      let res = fn true [] l in
      pop_frame ();
      let res = if iter then exp_apply _loc (exp_glr_fun _loc "iter") [res] else res in
      def, condition, res
     )

  let glr_rules_aux = 
    parser
    | {CHR('|') CHR('|')}? r:glr_rule rs:{ CHR('|') CHR('|') r:glr_rule}* -> 
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
	(fun x -> x), None, (exp_apply _loc (exp_glr_fun _loc "alternatives'") [l]))

  let _ = Decap.set_grammar glr_rules (
    parser
    { CHR('|') }? r:glr_rules_aux rs:{ { CHR('|') } r:glr_rules_aux}* -> 
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
	exp_apply _loc (exp_glr_fun _loc "alternatives") [l])
  )

end

let _ = register_extension (module Ext : FExt)

