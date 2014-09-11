open Asttypes
open Parsetree
open Longident
open Pa_ocaml_prelude
let _ = ()
type action =
  | Default
  | Normal of expression
  | DepSeq of (expression -> expression)* expression option* expression
let do_locate = ref None
let exp_int _loc n = loc_expr _loc (Pexp_constant (Const_int n))
let exp_string _loc n = loc_expr _loc (Pexp_constant (const_string n))
let exp_None _loc =
  let cnone = id_loc (Lident "None") _loc in
  loc_expr _loc (pexp_construct (cnone, None))
let exp_Some _loc a =
  let csome = id_loc (Lident "Some") _loc in
  loc_expr _loc (pexp_construct (csome, (Some a)))
let exp_unit _loc =
  let cunit = id_loc (Lident "()") _loc in
  loc_expr _loc (pexp_construct (cunit, None))
let exp_tuple _loc l = loc_expr _loc (Pexp_tuple l)
let exp_Nil _loc =
  let cnil = id_loc (Lident "[]") _loc in
  loc_expr _loc (pexp_construct (cnil, None))
let exp_Cons _loc a l =
  loc_expr _loc
    (pexp_construct
       ((id_loc (Lident "::") _loc), (Some (exp_tuple _loc [a; l]))))
let exp_list _loc l = List.fold_right (exp_Cons _loc) l (exp_Nil _loc)
let exp_ident _loc id = loc_expr _loc (Pexp_ident (id_loc (Lident id) _loc))
let pat_ident _loc id = loc_pat _loc (Ppat_var (id_loc id _loc))
let exp_apply _loc f l =
  loc_expr _loc (Pexp_apply (f, (List.map (fun x  -> ("", x)) l)))
let exp_lab_apply _loc f l = loc_expr _loc (Pexp_apply (f, l))
let exp_Some_fun _loc =
  loc_expr _loc
    (pexp_fun
       ("", None, (pat_ident _loc "x"), (exp_Some _loc (exp_ident _loc "x"))))
let exp_fun _loc id e =
  loc_expr _loc (pexp_fun ("", None, (pat_ident _loc id), e))
let exp_app _loc =
  exp_fun _loc "x"
    (exp_fun _loc "y"
       (exp_apply _loc (exp_ident _loc "y") [exp_ident _loc "x"]))
let exp_glr_fun _loc f =
  loc_expr _loc (Pexp_ident (id_loc (Ldot ((Lident "Glr"), f)) _loc))
let exp_list_fun _loc f =
  loc_expr _loc (Pexp_ident (id_loc (Ldot ((Lident "List"), f)) _loc))
let exp_str_fun _loc f =
  loc_expr _loc (Pexp_ident (id_loc (Ldot ((Lident "Str"), f)) _loc))
let exp_Cons_fun _loc =
  exp_fun _loc "x"
    (exp_fun _loc "l"
       (exp_Cons _loc (exp_ident _loc "x") (exp_ident _loc "l")))
let exp_Cons_rev_fun _loc =
  exp_fun _loc "x"
    (exp_fun _loc "l"
       (exp_Cons _loc (exp_ident _loc "x")
          (exp_apply _loc (exp_list_fun _loc "rev") [exp_ident _loc "l"])))
let mkpatt _loc (id,p) =
  match (p, (!do_locate)) with
  | (None ,_) -> pat_ident _loc id
  | (Some p,None ) -> loc_pat _loc (Ppat_alias (p, (id_loc id _loc)))
  | (Some p,Some _) ->
      loc_pat _loc
        (Ppat_alias
           ((loc_pat _loc (Ppat_tuple [loc_pat _loc Ppat_any; p])),
             (id_loc id _loc)))
let rec apply _loc ids e =
  let ids =
    List.mapi
      (fun i  ->
         fun (id,x)  ->
           ((if id = "_" then "_unnamed_" ^ (string_of_int i) else id), x))
      ids in
  let e =
    match !do_locate with
    | None  -> e
    | Some (_,merge) ->
        (match ids with
         | [] -> e
         | (id,_)::[] ->
             loc_expr _loc
               (Pexp_let
                  (Nonrecursive,
                    [value_binding _loc (pat_ident _loc "_loc")
                       (exp_ident _loc ("_loc_" ^ id))], e))
         | id1::id2::ids ->
             let all_loc =
               List.map (fun (id,_)  -> exp_ident _loc ("_loc_" ^ id))
                 (List.rev (id2 :: id1 :: ids)) in
             loc_expr _loc
               (Pexp_let
                  (Nonrecursive,
                    [value_binding _loc (pat_ident _loc "_loc")
                       (loc_expr _loc
                          (Pexp_apply
                             (merge, [("", (exp_list _loc all_loc))])))], e))) in
  List.fold_left
    (fun e  ->
       fun id  ->
         match !do_locate with
         | None  -> loc_expr _loc (pexp_fun ("", None, (mkpatt _loc id), e))
         | Some _ ->
             loc_expr _loc
               (pexp_fun
                  ("", None, (mkpatt _loc id),
                    (loc_expr _loc
                       (Pexp_let
                          (Nonrecursive,
                            [value_binding _loc
                               (loc_pat _loc
                                  (Ppat_tuple
                                     [loc_pat _loc
                                        (Ppat_var
                                           (id_loc ("_loc_" ^ (fst id)) _loc));
                                     loc_pat _loc
                                       (Ppat_var (id_loc (fst id) _loc))]))
                               (loc_expr _loc
                                  (Pexp_ident (id_loc (Lident (fst id)) _loc)))],
                            e)))))) e (List.rev ids)
let filter _loc r =
  match !do_locate with
  | None  -> r
  | Some (f,_) -> loc_expr _loc (Pexp_apply (f, [("", r)]))
let apply_option _loc opt e =
  filter _loc
    (match opt with
     | `Once -> e
     | `Option d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "option")
                [exp_None _loc;
                exp_apply _loc (exp_glr_fun _loc "apply")
                  [exp_Some_fun _loc; e]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc "option") [d; e])
     | `OptionPrime d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "option'")
                [exp_None _loc;
                exp_apply _loc (exp_glr_fun _loc "apply")
                  [exp_Some_fun _loc; e]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc "option") [d; e])
     | `Fixpoint d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "apply")
                [exp_list_fun _loc "rev";
                exp_apply _loc (exp_glr_fun _loc "fixpoint")
                  [exp_Nil _loc;
                  exp_apply _loc (exp_glr_fun _loc "apply")
                    [exp_Cons_fun _loc; e]]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc "fixpoint") [d; e])
     | `FixpointPrime d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "apply")
                [exp_list_fun _loc "rev";
                exp_apply _loc (exp_glr_fun _loc "fixpoint'")
                  [exp_Nil _loc;
                  exp_apply _loc (exp_glr_fun _loc "apply")
                    [exp_Cons_fun _loc; e]]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc "fixpoint") [d; e])
     | `Fixpoint1 d ->
         (match d with
          | None  ->
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
                     [exp_apply _loc (exp_ident _loc "x") [d]; e])])
     | `Fixpoint1Prime d ->
         (match d with
          | None  ->
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
                     [exp_apply _loc (exp_ident _loc "x") [d]; e])]))
let apply_list_option _loc opt e =
  filter _loc
    (match opt with
     | `Once -> e
     | `Option d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "option") [exp_Nil _loc; e]
          | Some d ->
              exp_apply _loc (exp_glr_fun _loc "option")
                [exp_list _loc [d]; e])
     | `OptionPrime d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "option'") [exp_Nil _loc; e]
          | Some d ->
              exp_apply _loc (exp_glr_fun _loc "option'")
                [exp_list _loc [d]; e])
     | `Fixpoint d ->
         (match d with
          | None  ->
              exp_apply _loc
                (exp_apply _loc (exp_list_fun _loc "map")
                   [exp_list_fun _loc "rev"])
                [exp_apply _loc (exp_glr_fun _loc "list_fixpoint")
                   [exp_Nil _loc;
                   exp_apply _loc (exp_glr_fun _loc "apply")
                     [exp_fun _loc "x"
                        (exp_fun _loc "l"
                           (exp_apply _loc (exp_list_fun _loc "map")
                              [exp_fun _loc "y"
                                 (exp_Cons _loc (exp_ident _loc "y")
                                    (exp_ident _loc "l"));
                              exp_ident _loc "x"]));
                     e]]]
          | Some d ->
              exp_apply _loc (exp_glr_fun _loc "list_fixpoint") [d; e])
     | `FixpointPrime d ->
         (match d with
          | None  ->
              exp_apply _loc
                (exp_apply _loc (exp_list_fun _loc "map")
                   [exp_list_fun _loc "rev"])
                [exp_apply _loc (exp_glr_fun _loc "list_fixpoint'")
                   [exp_Nil _loc;
                   exp_apply _loc (exp_glr_fun _loc "apply")
                     [exp_fun _loc "x"
                        (exp_fun _loc "l"
                           (exp_apply _loc (exp_list_fun _loc "map")
                              [exp_fun _loc "y"
                                 (exp_Cons _loc (exp_ident _loc "y")
                                    (exp_ident _loc "l"));
                              exp_ident _loc "x"]));
                     e]]]
          | Some d ->
              exp_apply _loc (exp_glr_fun _loc "list_fixpoint'") [d; e])
     | `Fixpoint1 d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "list_sequence")
                [e;
                exp_apply _loc (exp_glr_fun _loc "list_fixpoint")
                  [exp_Nil _loc;
                  exp_apply _loc (exp_glr_fun _loc "apply")
                    [exp_fun _loc "x"
                       (exp_fun _loc "l"
                          (exp_apply _loc (exp_list_fun _loc "map")
                             [exp_fun _loc "y"
                                (exp_Cons _loc (exp_ident _loc "y")
                                   (exp_ident _loc "l"));
                             exp_ident _loc "x"]));
                    e]];
                exp_Cons_rev_fun _loc]
          | Some d ->
              exp_apply _loc (exp_glr_fun _loc "list_dependent_sequence")
                [e;
                exp_fun _loc "x"
                  (exp_apply _loc (exp_glr_fun _loc "list_fixpoint")
                     [exp_apply _loc (exp_ident _loc "x") [d]; e])])
     | `Fixpoint1Prime d ->
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "list_sequence")
                [e;
                exp_apply _loc (exp_glr_fun _loc "list_fixpoint'")
                  [exp_Nil _loc;
                  exp_apply _loc (exp_glr_fun _loc "apply")
                    [exp_fun _loc "x"
                       (exp_fun _loc "l"
                          (exp_apply _loc (exp_list_fun _loc "map")
                             [exp_fun _loc "y"
                                (exp_Cons _loc (exp_ident _loc "y")
                                   (exp_ident _loc "l"));
                             exp_ident _loc "x"]));
                    e]];
                exp_Cons_rev_fun _loc]
          | Some d ->
              exp_apply _loc (exp_glr_fun _loc "list_dependent_sequence")
                [e;
                exp_fun _loc "x"
                  (exp_apply _loc (exp_glr_fun _loc "list_fixpoint'")
                     [exp_apply _loc (exp_ident _loc "x") [d]; e])]))
let default_action _loc l =
  let l =
    List.filter
      (function
       | `Normal (("_",_),_,_) -> false
       | `Ignore -> false
       | _ -> true) l in
  let l =
    List.map
      (function
       | `Normal ((id,_),_,_) -> exp_ident _loc id
       | _ -> assert false) l in
  let rec fn =
    function
    | [] -> exp_unit _loc
    | x::[] -> x
    | _::_ as l -> exp_tuple _loc l in
  fn l
module Ext(In:Extension) =
  struct
    include In
    let glr_rules = Glr.declare_grammar "glr_rules"
    let glr_list_rules = Glr.declare_grammar "glr_list_rules"
    let glr_rule = Glr.declare_grammar "glr_rule"
    let glr_list_rule = Glr.declare_grammar "glr_list_rule"
    let glr_parser =
      Glr.alternatives
        [Glr.sequence (locate (Glr.string "parser_locate" ()))
           (Glr.sequence (locate (expression_lvl (next_exp App)))
              (locate (expression_lvl (next_exp App)))
              (fun filter2  ->
                 let (_loc_filter2,filter2) = filter2 in
                 fun merge2  ->
                   let (_loc_merge2,merge2) = merge2 in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc =
                       merge [_loc__unnamed_2; _loc_filter2; _loc_merge2] in
                     do_locate := (Some (filter2, merge2));
                     (Atom, (exp_unit _loc)))) (fun x  -> fun y  -> y x);
        Glr.sequence (locate parser_kw) (locate glr_rules)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun p  ->
               let (_loc_p,p) = p in
               let _loc = merge [_loc__unnamed_0; _loc_p] in (Atom, p));
        Glr.sequence (locate parser_kw)
          (Glr.sequence (locate (Glr.char '*' ())) (locate glr_list_rules)
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun p  ->
                  let (_loc_p,p) = p in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc__unnamed_0; _loc_p] in
                    (Atom, p))) (fun x  -> fun y  -> y x)]
    let extra_expressions = glr_parser :: extra_expressions
    let glr_opt_expr =
      Glr.apply (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (locate (Glr.char '[' ()))
                    (Glr.sequence (locate expression)
                       (locate (Glr.char ']' ()))
                       (fun e  ->
                          let (_loc_e,e) = e in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            fun _unnamed_2  ->
                              let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                              let _loc =
                                merge
                                  [_loc__unnamed_2; _loc_e; _loc__unnamed_1] in
                              e)) (fun x  -> fun y  -> y x)))))
    let glr_option =
      Glr.alternatives
        [Glr.sequence (locate (Glr.char '*' ()))
           (Glr.sequence (locate (Glr.char '*' ())) (locate glr_opt_expr)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun e  ->
                   let (_loc_e,e) = e in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc =
                       merge [_loc__unnamed_2; _loc__unnamed_0; _loc_e] in
                     `FixpointPrime e)) (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.char '*' ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in `Fixpoint e);
        Glr.sequence (locate (Glr.char '+' ()))
          (Glr.sequence (locate (Glr.char '+' ())) (locate glr_opt_expr)
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc__unnamed_0; _loc_e] in
                    `Fixpoint1Prime e)) (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.char '+' ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in `Fixpoint1 e);
        Glr.sequence (locate (Glr.char '?' ()))
          (Glr.sequence (locate (Glr.char '?' ())) (locate glr_opt_expr)
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc =
                      merge [_loc__unnamed_2; _loc__unnamed_0; _loc_e] in
                    `OptionPrime e)) (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.char '?' ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in `Option e);
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in `Once) (locate (Glr.empty ()))]
    let glr_sequence =
      Glr.alternatives
        [Glr.sequence (locate (Glr.char '{' ()))
           (Glr.sequence (locate glr_rules) (locate (Glr.char '}' ()))
              (fun r  ->
                 let (_loc_r,r) = r in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc =
                       merge [_loc__unnamed_2; _loc_r; _loc__unnamed_1] in
                     r)) (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.string "EOF" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge [_loc__unnamed_0; _loc_opt] in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "eof") [e]);
        Glr.sequence (locate (Glr.string "EMPTY" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge [_loc__unnamed_0; _loc_opt] in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "empty") [e]);
        Glr.sequence (locate (Glr.string "FAIL" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in
               exp_apply _loc (exp_glr_fun _loc "fail") [e]);
        Glr.sequence (locate (Glr.string "DEBUG" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in
               exp_apply _loc (exp_glr_fun _loc "debug") [e]);
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in exp_glr_fun _loc "any")
          (locate (Glr.string "ANY" ()));
        Glr.sequence (locate (Glr.string "CHR" ()))
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_e; _loc_opt] in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "char") [e; opt]))
          (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.string "STR" ()))
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_e; _loc_opt] in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "string") [e; opt]))
          (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.string "RE" ()))
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_e; _loc_opt] in
                    let opt =
                      match opt with
                      | None  ->
                          exp_apply _loc (exp_ident _loc "groupe")
                            [exp_int _loc 0]
                      | Some e -> e in
                    match e.pexp_desc with
                    | Pexp_ident { txt = Lident id } ->
                        let id =
                          let l = String.length id in
                          if (l > 3) && ((String.sub id (l - 3) 3) = "_re")
                          then String.sub id 0 (l - 3)
                          else id in
                        exp_lab_apply _loc (exp_glr_fun _loc "regexp")
                          [("name", (exp_string _loc id));
                          ("", e);
                          ("", (exp_fun _loc "groupe" opt))]
                    | _ ->
                        exp_apply _loc (exp_glr_fun _loc "regexp")
                          [e; exp_fun _loc "groupe" opt]))
          (fun x  -> fun y  -> y x);
        Glr.apply (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
          (locate (expression_lvl Atom))]
    let glr_list_sequence =
      Glr.alternatives
        [Glr.sequence (locate (Glr.char '{' ()))
           (Glr.sequence (locate glr_list_rules) (locate (Glr.char '}' ()))
              (fun r  ->
                 let (_loc_r,r) = r in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc =
                       merge [_loc__unnamed_2; _loc_r; _loc__unnamed_1] in
                     r)) (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.string "EOF" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge [_loc__unnamed_0; _loc_opt] in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "list_eof") [e]);
        Glr.sequence (locate (Glr.string "EMPTY" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge [_loc__unnamed_0; _loc_opt] in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "list_empty") [e]);
        Glr.sequence (locate (Glr.string "FAIL" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in
               exp_apply _loc (exp_glr_fun _loc "list_fail") [e]);
        Glr.sequence (locate (Glr.string "DEBUG" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge [_loc__unnamed_0; _loc_e] in
               exp_apply _loc (exp_glr_fun _loc "list_debug") [e]);
        Glr.sequence (locate (Glr.string "CHR" ()))
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_e; _loc_opt] in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "list_char") [e; opt]))
          (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.string "STR" ()))
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_e; _loc_opt] in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "list_string") [e; opt]))
          (fun x  -> fun y  -> y x);
        Glr.sequence (locate (Glr.string "RE" ()))
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  ->
                    let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                    let _loc = merge [_loc__unnamed_2; _loc_e; _loc_opt] in
                    let opt =
                      match opt with
                      | None  ->
                          exp_apply _loc (exp_ident _loc "groupe")
                            [exp_int _loc 0]
                      | Some e -> e in
                    match e.pexp_desc with
                    | Pexp_ident { txt = Lident id } ->
                        let id =
                          let l = String.length id in
                          if (l > 3) && ((String.sub id (l - 3) 3) = "_re")
                          then String.sub id 0 (l - 3)
                          else id in
                        exp_lab_apply _loc (exp_glr_fun _loc "list_regexp")
                          [("name", (exp_string _loc id));
                          ("", e);
                          ("", (exp_fun _loc "groupe" opt))]
                    | _ ->
                        exp_apply _loc (exp_glr_fun _loc "list_regexp")
                          [e; exp_fun _loc "groupe" opt]))
          (fun x  -> fun y  -> y x);
        Glr.apply (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
          (locate (expression_lvl Atom))]
    let glr_ident =
      Glr.alternatives
        [Glr.sequence (locate (pattern_lvl ConstrPat))
           (locate (Glr.char ':' ()))
           (fun p  ->
              let (_loc_p,p) = p in
              fun _unnamed_1  ->
                let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                let _loc = merge [_loc_p; _loc__unnamed_1] in
                match p.ppat_desc with
                | Ppat_alias (p,{ txt = id }) -> (id, (Some p))
                | Ppat_var { txt = id } -> (id, None)
                | _ -> ("_", (Some p)));
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in ("_", None))
          (locate (Glr.empty ()))]
    let dash =
      Glr.black_box
        (fun str  ->
           fun pos  ->
             let (c,str',pos') = Input.read str pos in
             if c = '-'
             then
               let (c',_,_) = Input.read str' pos' in
               (if c' = '>' then raise Glr.Give_up else ((), str', pos'))
             else raise Glr.Give_up) (Charset.singleton '-') false "-"
    let glr_left_member =
      Glr.apply (fun l  -> let (_loc_l,l) = l in let _loc = _loc_l in l)
        (locate
           (Glr.sequence
              (Glr.alternatives
                 [Glr.sequence (locate glr_ident)
                    (Glr.sequence (locate glr_sequence) (locate glr_option)
                       (fun s  ->
                          let (_loc_s,s) = s in
                          fun opt  ->
                            let (_loc_opt,opt) = opt in
                            fun id  ->
                              let (_loc_id,id) = id in
                              let _loc = merge [_loc_id; _loc_s; _loc_opt] in
                              `Normal (id, s, opt)))
                    (fun x  -> fun y  -> y x);
                 Glr.apply
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      let _loc = _loc__unnamed_0 in `Ignore) (locate dash)])
              (Glr.fixpoint []
                 (Glr.apply (fun x  -> fun l  -> x :: l)
                    (Glr.alternatives
                       [Glr.sequence (locate glr_ident)
                          (Glr.sequence (locate glr_sequence)
                             (locate glr_option)
                             (fun s  ->
                                let (_loc_s,s) = s in
                                fun opt  ->
                                  let (_loc_opt,opt) = opt in
                                  fun id  ->
                                    let (_loc_id,id) = id in
                                    let _loc =
                                      merge [_loc_id; _loc_s; _loc_opt] in
                                    `Normal (id, s, opt)))
                          (fun x  -> fun y  -> y x);
                       Glr.apply
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            let _loc = _loc__unnamed_0 in `Ignore)
                         (locate dash)])))
              (fun x  -> fun l  -> x :: (List.rev l))))
    let glr_list_left_member =
      Glr.apply (fun l  -> let (_loc_l,l) = l in let _loc = _loc_l in l)
        (locate
           (Glr.sequence
              (Glr.alternatives
                 [Glr.sequence (locate glr_ident)
                    (Glr.sequence (locate glr_list_sequence)
                       (locate glr_option)
                       (fun s  ->
                          let (_loc_s,s) = s in
                          fun opt  ->
                            let (_loc_opt,opt) = opt in
                            fun id  ->
                              let (_loc_id,id) = id in
                              let _loc = merge [_loc_id; _loc_s; _loc_opt] in
                              `Normal (id, s, opt)))
                    (fun x  -> fun y  -> y x);
                 Glr.apply
                   (fun _unnamed_0  ->
                      let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                      let _loc = _loc__unnamed_0 in `Ignore) (locate dash)])
              (Glr.fixpoint []
                 (Glr.apply (fun x  -> fun l  -> x :: l)
                    (Glr.alternatives
                       [Glr.sequence (locate glr_ident)
                          (Glr.sequence (locate glr_list_sequence)
                             (locate glr_option)
                             (fun s  ->
                                let (_loc_s,s) = s in
                                fun opt  ->
                                  let (_loc_opt,opt) = opt in
                                  fun id  ->
                                    let (_loc_id,id) = id in
                                    let _loc =
                                      merge [_loc_id; _loc_s; _loc_opt] in
                                    `Normal (id, s, opt)))
                          (fun x  -> fun y  -> y x);
                       Glr.apply
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            let _loc = _loc__unnamed_0 in `Ignore)
                         (locate dash)])))
              (fun x  -> fun l  -> x :: (List.rev l))))
    let glr_let = Glr.declare_grammar "glr_let"
    let _ =
      Glr.set_grammar glr_let
        (Glr.alternatives
           [Glr.sequence (locate (Glr.string "let" ()))
              (Glr.sequence (locate rec_flag)
                 (Glr.sequence (locate let_binding)
                    (Glr.sequence (locate (Glr.string "in" ()))
                       (locate glr_let)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun l  ->
                            let (_loc_l,l) = l in
                            fun lbs  ->
                              let (_loc_lbs,lbs) = lbs in
                              fun r  ->
                                let (_loc_r,r) = r in
                                fun _unnamed_4  ->
                                  let (_loc__unnamed_4,_unnamed_4) =
                                    _unnamed_4 in
                                  let _loc =
                                    merge
                                      [_loc__unnamed_4;
                                      _loc_r;
                                      _loc_lbs;
                                      _loc__unnamed_0;
                                      _loc_l] in
                                  fun x  ->
                                    loc_expr _loc (Pexp_let (r, lbs, (l x)))))
                    (fun x  -> fun y  -> y x)) (fun x  -> fun y  -> y x))
              (fun x  -> fun y  -> y x);
           Glr.apply
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                let _loc = _loc__unnamed_0 in fun x  -> x)
             (locate (Glr.empty ()))])
    let glr_cond =
      Glr.alternatives
        [Glr.sequence (locate (Glr.string "when" ())) (locate expression)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun e  ->
                let (_loc_e,e) = e in
                let _loc = merge [_loc__unnamed_0; _loc_e] in Some e);
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in None) (locate (Glr.empty ()))]
    let glr_action =
      Glr.alternatives
        [Glr.sequence (locate (Glr.string "->>" ())) (locate glr_rule)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun ((_,(def,cond,r)) as _unnamed_1)  ->
                let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                let _loc = merge [_loc__unnamed_0; _loc__unnamed_1] in
                DepSeq (def, cond, r));
        Glr.sequence (locate (Glr.string "->" ())) (locate expression)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun action  ->
               let (_loc_action,action) = action in
               let _loc = merge [_loc__unnamed_0; _loc_action] in
               Normal action);
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in Default) (locate (Glr.empty ()))]
    let glr_list_action =
      Glr.alternatives
        [Glr.sequence (locate (Glr.string "->>" ())) (locate glr_list_rule)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun ((_,(def,cond,r)) as _unnamed_1)  ->
                let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                let _loc = merge [_loc__unnamed_0; _loc__unnamed_1] in
                DepSeq (def, cond, r));
        Glr.sequence (locate (Glr.string "->" ()))
          (locate (expression_lvl (next_exp Disj)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun action  ->
               let (_loc_action,action) = action in
               let _loc = merge [_loc__unnamed_0; _loc_action] in
               Normal action);
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in Default) (locate (Glr.empty ()))]
    let _ =
      Glr.set_grammar glr_rule
        (Glr.sequence (locate glr_let)
           (Glr.sequence (locate glr_left_member)
              (Glr.sequence (locate glr_cond) (locate glr_action)
                 (fun condition  ->
                    let (_loc_condition,condition) = condition in
                    fun action  ->
                      let (_loc_action,action) = action in
                      fun l  ->
                        let (_loc_l,l) = l in
                        fun def  ->
                          let (_loc_def,def) = def in
                          let _loc =
                            merge
                              [_loc_def; _loc_l; _loc_condition; _loc_action] in
                          let (iter,action) =
                            match action with
                            | Normal a -> (false, a)
                            | Default  -> (false, (default_action _loc l))
                            | DepSeq (def,cond,a) ->
                                (true,
                                  ((match cond with
                                    | None  -> def a
                                    | Some cond ->
                                        def
                                          (loc_expr _loc
                                             (Pexp_ifthenelse
                                                (cond, a,
                                                  (Some
                                                     (exp_apply _loc
                                                        (exp_glr_fun _loc
                                                           "fail")
                                                        [exp_string _loc ""])))))))) in
                          let rec fn ids l =
                            match l with
                            | [] -> assert false
                            | `Ignore::ls ->
                                exp_apply _loc
                                  (exp_glr_fun _loc "ignore_next_blank")
                                  [fn ids ls]
                            | (`Normal (id,e,opt))::[] ->
                                let e = apply_option _loc opt e in
                                exp_apply _loc (exp_glr_fun _loc "apply")
                                  [apply _loc (id :: ids) action; e]
                            | (`Normal (id,e,opt))::(`Normal (id',e',opt'))::[]
                                ->
                                let e = apply_option _loc opt e in
                                let e' = apply_option _loc opt' e' in
                                exp_apply _loc (exp_glr_fun _loc "sequence")
                                  [e;
                                  e';
                                  apply _loc (id :: id' :: ids) action]
                            | (`Normal (id,e,opt))::ls ->
                                let e = apply_option _loc opt e in
                                exp_apply _loc (exp_glr_fun _loc "sequence")
                                  [e; fn (id :: ids) ls; exp_app _loc] in
                          let res = fn [] l in
                          let res =
                            if iter
                            then
                              exp_apply _loc (exp_glr_fun _loc "iter") [res]
                            else res in
                          (def, condition, res))) (fun x  -> fun y  -> y x))
           (fun x  -> fun y  -> y x))
    let _ =
      Glr.set_grammar glr_list_rule
        (Glr.sequence (locate glr_let)
           (Glr.sequence (locate glr_list_left_member)
              (Glr.sequence (locate glr_cond) (locate glr_list_action)
                 (fun condition  ->
                    let (_loc_condition,condition) = condition in
                    fun action  ->
                      let (_loc_action,action) = action in
                      fun l  ->
                        let (_loc_l,l) = l in
                        fun def  ->
                          let (_loc_def,def) = def in
                          let _loc =
                            merge
                              [_loc_def; _loc_l; _loc_condition; _loc_action] in
                          let (iter,action) =
                            match action with
                            | Normal a -> (false, a)
                            | Default  -> (false, (default_action _loc l))
                            | DepSeq (def,cond,a) ->
                                (true,
                                  ((match cond with
                                    | None  -> def a
                                    | Some cond ->
                                        def
                                          (loc_expr _loc
                                             (Pexp_ifthenelse
                                                (cond, a,
                                                  (Some
                                                     (exp_apply _loc
                                                        (exp_glr_fun _loc
                                                           "fail")
                                                        [exp_string _loc ""])))))))) in
                          let rec fn ids l =
                            match l with
                            | [] -> assert false
                            | `Ignore::ls ->
                                exp_apply _loc
                                  (exp_glr_fun _loc "ignore_next_blank")
                                  [fn ids ls]
                            | (`Normal (id,e,opt))::[] ->
                                let e = apply_list_option _loc opt e in
                                exp_apply _loc (exp_glr_fun _loc "apply")
                                  [apply _loc (id :: ids) action; e]
                            | (`Normal (id,e,opt))::(`Normal (id',e',opt'))::[]
                                ->
                                let e = apply_list_option _loc opt e in
                                let e' = apply_list_option _loc opt' e' in
                                exp_apply _loc
                                  (exp_glr_fun _loc "list_sequence")
                                  [e;
                                  e';
                                  apply _loc (id :: id' :: ids) action]
                            | (`Normal (id,e,opt))::ls ->
                                let e = apply_list_option _loc opt e in
                                exp_apply _loc
                                  (exp_glr_fun _loc "list_sequence")
                                  [e; fn (id :: ids) ls; exp_app _loc] in
                          let res = fn [] l in
                          let res =
                            if iter
                            then
                              exp_apply _loc (exp_glr_fun _loc "iter_list")
                                [res]
                            else res in
                          (def, condition, res))) (fun x  -> fun y  -> y x))
           (fun x  -> fun y  -> y x))
    let glr_rules_aux =
      Glr.sequence
        (locate
           (Glr.option None (Glr.apply (fun x  -> Some x) (Glr.char '|' ()))))
        (Glr.sequence (locate glr_rule)
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  -> fun l  -> x :: l)
                       (Glr.sequence (locate (Glr.char '|' ()))
                          (locate glr_rule)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun r  ->
                               let (_loc_r,r) = r in
                               let _loc = merge [_loc__unnamed_0; _loc_r] in
                               r))))))
           (fun r  ->
              let (_loc_r,r) = r in
              fun rs  ->
                let (_loc_rs,rs) = rs in
                fun _unnamed_2  ->
                  let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                  let _loc = merge [_loc__unnamed_2; _loc_r; _loc_rs] in
                  match rs with
                  | [] -> r
                  | l ->
                      let l =
                        List.fold_right
                          (fun (def,cond,x)  ->
                             fun y  ->
                               match cond with
                               | None  -> def (exp_Cons _loc x y)
                               | Some c ->
                                   def
                                     (loc_expr _loc
                                        (Pexp_let
                                           (Nonrecursive,
                                             [value_binding _loc
                                                (pat_ident _loc "y") y],
                                             (loc_expr _loc
                                                (Pexp_ifthenelse
                                                   (c,
                                                     (exp_Cons _loc x
                                                        (exp_ident _loc "y")),
                                                     (Some
                                                        (exp_ident _loc "y")))))))))
                          (r :: l) (exp_Nil _loc) in
                      (((fun x  -> x)), None,
                        (exp_apply _loc (exp_glr_fun _loc "alternatives") [l]))))
        (fun x  -> fun y  -> y x)
    let glr_list_rules_aux =
      Glr.sequence
        (locate
           (Glr.option None (Glr.apply (fun x  -> Some x) (Glr.char '|' ()))))
        (Glr.sequence (locate glr_list_rule)
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  -> fun l  -> x :: l)
                       (Glr.sequence (locate (Glr.char '|' ()))
                          (locate glr_list_rule)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun r  ->
                               let (_loc_r,r) = r in
                               let _loc = merge [_loc__unnamed_0; _loc_r] in
                               r))))))
           (fun r  ->
              let (_loc_r,r) = r in
              fun rs  ->
                let (_loc_rs,rs) = rs in
                fun _unnamed_2  ->
                  let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                  let _loc = merge [_loc__unnamed_2; _loc_r; _loc_rs] in
                  match rs with
                  | [] -> r
                  | l ->
                      let l =
                        List.fold_right
                          (fun (def,cond,x)  ->
                             fun y  ->
                               match cond with
                               | None  -> def (exp_Cons _loc x y)
                               | Some c ->
                                   def
                                     (loc_expr _loc
                                        (Pexp_let
                                           (Nonrecursive,
                                             [value_binding _loc
                                                (pat_ident _loc "y") y],
                                             (loc_expr _loc
                                                (Pexp_ifthenelse
                                                   (c,
                                                     (exp_Cons _loc x
                                                        (exp_ident _loc "y")),
                                                     (Some
                                                        (exp_ident _loc "y")))))))))
                          (r :: l) (exp_Nil _loc) in
                      (((fun x  -> x)), None,
                        (exp_apply _loc
                           (exp_glr_fun _loc "list_alternatives") [l]))))
        (fun x  -> fun y  -> y x)
    let _ =
      Glr.set_grammar glr_rules
        (Glr.sequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.alternatives
                       [Glr.sequence (locate (Glr.char '|' ()))
                          (locate (Glr.char '|' ()))
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun _unnamed_1  ->
                               let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                               let _loc =
                                 merge [_loc__unnamed_0; _loc__unnamed_1] in
                               ());
                       Glr.apply
                         (fun _unnamed_0  ->
                            let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                            let _loc = _loc__unnamed_0 in ())
                         (locate else_kw)]))))
           (Glr.sequence (locate glr_rules_aux)
              (locate
                 (Glr.apply List.rev
                    (Glr.fixpoint []
                       (Glr.apply (fun x  -> fun l  -> x :: l)
                          (Glr.sequence
                             (locate
                                (Glr.alternatives
                                   [Glr.sequence (locate (Glr.char '|' ()))
                                      (locate (Glr.char '|' ()))
                                      (fun _unnamed_0  ->
                                         let (_loc__unnamed_0,_unnamed_0) =
                                           _unnamed_0 in
                                         fun _unnamed_1  ->
                                           let (_loc__unnamed_1,_unnamed_1) =
                                             _unnamed_1 in
                                           let _loc =
                                             merge
                                               [_loc__unnamed_0;
                                               _loc__unnamed_1] in
                                           ());
                                   Glr.apply
                                     (fun _unnamed_0  ->
                                        let (_loc__unnamed_0,_unnamed_0) =
                                          _unnamed_0 in
                                        let _loc = _loc__unnamed_0 in ())
                                     (locate else_kw)]))
                             (locate glr_rules_aux)
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun r  ->
                                  let (_loc_r,r) = r in
                                  let _loc = merge [_loc__unnamed_0; _loc_r] in
                                  r))))))
              (fun r  ->
                 let (_loc_r,r) = r in
                 fun rs  ->
                   let (_loc_rs,rs) = rs in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc = merge [_loc__unnamed_2; _loc_r; _loc_rs] in
                     match (r, rs) with
                     | ((def,cond,e),[]) ->
                         (match cond with
                          | None  -> def e
                          | Some c ->
                              loc_expr _loc
                                (Pexp_ifthenelse
                                   (c, e,
                                     (Some
                                        (exp_apply _loc
                                           (exp_glr_fun _loc "fail")
                                           [exp_string _loc ""])))))
                     | (r,l) ->
                         let l =
                           List.fold_right
                             (fun (def,cond,x)  ->
                                fun y  ->
                                  match cond with
                                  | None  -> def (exp_Cons _loc x y)
                                  | Some c ->
                                      def
                                        (loc_expr _loc
                                           (Pexp_let
                                              (Nonrecursive,
                                                [value_binding _loc
                                                   (pat_ident _loc "y") y],
                                                (loc_expr _loc
                                                   (Pexp_ifthenelse
                                                      (c,
                                                        (exp_Cons _loc x
                                                           (exp_ident _loc
                                                              "y")),
                                                        (Some
                                                           (exp_ident _loc
                                                              "y"))))))))) (r
                             :: l) (exp_Nil _loc) in
                         exp_apply _loc (exp_glr_fun _loc "alternatives'")
                           [l])) (fun x  -> fun y  -> y x))
    let _ =
      Glr.set_grammar glr_list_rules
        (Glr.sequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x)
                    (Glr.sequence (locate (Glr.char '|' ()))
                       (locate (Glr.char '|' ()))
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun _unnamed_1  ->
                            let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                            let _loc =
                              merge [_loc__unnamed_0; _loc__unnamed_1] in
                            ())))))
           (Glr.sequence (locate glr_list_rules_aux)
              (locate
                 (Glr.apply List.rev
                    (Glr.fixpoint []
                       (Glr.apply (fun x  -> fun l  -> x :: l)
                          (Glr.sequence
                             (locate
                                (Glr.sequence (locate (Glr.char '|' ()))
                                   (locate (Glr.char '|' ()))
                                   (fun _unnamed_0  ->
                                      let (_loc__unnamed_0,_unnamed_0) =
                                        _unnamed_0 in
                                      fun _unnamed_1  ->
                                        let (_loc__unnamed_1,_unnamed_1) =
                                          _unnamed_1 in
                                        let _loc =
                                          merge
                                            [_loc__unnamed_0;
                                            _loc__unnamed_1] in
                                        ()))) (locate glr_list_rules_aux)
                             (fun _unnamed_0  ->
                                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                                fun r  ->
                                  let (_loc_r,r) = r in
                                  let _loc = merge [_loc__unnamed_0; _loc_r] in
                                  r))))))
              (fun r  ->
                 let (_loc_r,r) = r in
                 fun rs  ->
                   let (_loc_rs,rs) = rs in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc = merge [_loc__unnamed_2; _loc_r; _loc_rs] in
                     match (r, rs) with
                     | ((def,cond,e),[]) ->
                         (match cond with
                          | None  -> def e
                          | Some c ->
                              loc_expr _loc
                                (Pexp_ifthenelse
                                   (c, e,
                                     (Some
                                        (exp_apply _loc
                                           (exp_glr_fun _loc "fail")
                                           [exp_string _loc ""])))))
                     | (r,l) ->
                         let l =
                           List.fold_right
                             (fun (def,cond,x)  ->
                                fun y  ->
                                  match cond with
                                  | None  -> def (exp_Cons _loc x y)
                                  | Some c ->
                                      def
                                        (loc_expr _loc
                                           (Pexp_let
                                              (Nonrecursive,
                                                [value_binding _loc
                                                   (pat_ident _loc "y") y],
                                                (loc_expr _loc
                                                   (Pexp_ifthenelse
                                                      (c,
                                                        (exp_Cons _loc x
                                                           (exp_ident _loc
                                                              "y")),
                                                        (Some
                                                           (exp_ident _loc
                                                              "y"))))))))) (r
                             :: l) (exp_Nil _loc) in
                         exp_apply _loc
                           (exp_glr_fun _loc "list_alternatives'") [l]))
           (fun x  -> fun y  -> y x))
  end
let _ = register_extension (module Ext : FExt)
