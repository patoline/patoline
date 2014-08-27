open Asttypes
open Parsetree
open Longident
open Pa_ocaml_prelude
let _ = ()
type ('a,'b) action =
  | Default
  | Normal of 'a
  | DepSeq of 'b
let do_locate = ref None
let exp_int _loc n = loc_expr _loc (Pexp_constant (Const_int n))
let exp_string _loc n = loc_expr _loc (Pexp_constant (const_string n))
let exp_None _loc =
  let cnone = { txt = (Lident "None"); loc = _loc } in
  loc_expr _loc (pexp_construct (cnone, None))
let exp_Some _loc a =
  let csome = { txt = (Lident "Some"); loc = _loc } in
  loc_expr _loc (pexp_construct (csome, (Some a)))
let exp_unit _loc =
  let cunit = { txt = (Lident "()"); loc = _loc } in
  loc_expr _loc (pexp_construct (cunit, None))
let exp_tuple _loc l = loc_expr _loc (Pexp_tuple l)
let exp_Nil _loc =
  let cnil = { txt = (Lident "[]"); loc = _loc } in
  loc_expr _loc (pexp_construct (cnil, None))
let exp_Cons _loc a l =
  loc_expr _loc
    (pexp_construct
       ({ txt = (Lident "::"); loc = _loc }, (Some (exp_tuple _loc [a; l]))))
let exp_list _loc l = List.fold_right (exp_Cons _loc) l (exp_Nil _loc)
let exp_ident _loc id =
  loc_expr _loc (Pexp_ident { txt = (Lident id); loc = _loc })
let pat_ident _loc id = loc_pat _loc (Ppat_var { txt = id; loc = _loc })
let exp_apply _loc f l =
  loc_expr _loc (Pexp_apply (f, (List.map (fun x  -> ("", x)) l)))
let exp_lab_apply _loc f l = loc_expr _loc (Pexp_apply (f, l))
let exp_Some_fun _loc =
  loc_expr _loc
    (pexp_fun
       ("", None, (pat_ident _loc "x"), (exp_Some _loc (exp_ident _loc "x"))))
let exp_fun _loc id e =
  loc_expr _loc (pexp_fun ("", None, (pat_ident _loc id), e))
let exp_glr_fun _loc f =
  loc_expr _loc (Pexp_ident { txt = (Ldot ((Lident "Glr"), f)); loc = _loc })
let exp_list_fun _loc f =
  loc_expr _loc
    (Pexp_ident { txt = (Ldot ((Lident "List"), f)); loc = _loc })
let exp_str_fun _loc f =
  loc_expr _loc (Pexp_ident { txt = (Ldot ((Lident "Str"), f)); loc = _loc })
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
  | (Some p,None ) -> loc_pat _loc (Ppat_alias (p, { txt = id; loc = _loc }))
  | (Some p,Some _) ->
      loc_pat _loc
        (Ppat_alias
           ((loc_pat _loc (Ppat_tuple [loc_pat _loc Ppat_any; p])),
             { txt = id; loc = _loc }))
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
         | (first,_)::ids ->
             let (last,_) = List.hd (List.rev ids) in
             loc_expr _loc
               (Pexp_let
                  (Nonrecursive,
                    [value_binding _loc (pat_ident _loc "_loc")
                       (loc_expr _loc
                          (Pexp_apply
                             (merge,
                               [("", (exp_ident _loc ("_loc_" ^ first)));
                               ("", (exp_ident _loc ("_loc_" ^ last)))])))],
                    e))) in
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
                                           {
                                             txt = ("_loc_" ^ (fst id));
                                             loc = _loc
                                           });
                                     loc_pat _loc
                                       (Ppat_var
                                          { txt = (fst id); loc = _loc })]))
                               (loc_expr _loc
                                  (Pexp_ident
                                     { txt = (Lident (fst id)); loc = _loc }))],
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
  let l = List.filter (function | (("_",_),_,_) -> false | _ -> true) l in
  let l = List.map (fun ((id,_),_,_)  -> exp_ident _loc id) l in
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
        [Glr.sequence
           (Glr.sequence (locate (Glr.string "parser_locate" ()))
              (locate (expression_lvl (next_exp App)))
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun filter2  ->
                   let (_loc_filter2,filter2) = filter2 in
                   fun merge2  ->
                     let (_loc_merge2,merge2) = merge2 in
                     let _loc = merge _loc__unnamed_0 _loc_merge2 in
                     do_locate := (Some (filter2, merge2));
                     (Atom, (exp_unit _loc))))
           (locate (expression_lvl (next_exp App))) (fun x  -> x);
        Glr.sequence (locate parser_kw) (locate glr_rules)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun p  ->
               let (_loc_p,p) = p in
               let _loc = merge _loc__unnamed_0 _loc_p in (Atom, p));
        Glr.sequence
          (Glr.sequence (locate parser_kw) (locate (Glr.char '*' ()))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun p  ->
                    let (_loc_p,p) = p in
                    let _loc = merge _loc__unnamed_0 _loc_p in (Atom, p)))
          (locate glr_list_rules) (fun x  -> x)]
    let extra_expressions = glr_parser :: extra_expressions
    let glr_opt_expr =
      Glr.apply (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence
                    (Glr.sequence (locate (Glr.char '[' ()))
                       (locate expression)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun e  ->
                            let (_loc_e,e) = e in
                            fun _unnamed_2  ->
                              let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                              let _loc =
                                merge _loc__unnamed_0 _loc__unnamed_2 in
                              e)) (locate (Glr.char ']' ())) (fun x  -> x)))))
    let glr_option =
      Glr.alternatives
        [Glr.sequence
           (Glr.sequence (locate (Glr.char '*' ()))
              (locate (Glr.char '*' ()))
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun _unnamed_1  ->
                   let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                   fun e  ->
                     let (_loc_e,e) = e in
                     let _loc = merge _loc__unnamed_0 _loc_e in
                     `FixpointPrime e)) (locate glr_opt_expr) (fun x  -> x);
        Glr.sequence (locate (Glr.char '*' ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge _loc__unnamed_0 _loc_e in `Fixpoint e);
        Glr.sequence
          (Glr.sequence (locate (Glr.char '+' ())) (locate (Glr.char '+' ()))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun e  ->
                    let (_loc_e,e) = e in
                    let _loc = merge _loc__unnamed_0 _loc_e in
                    `Fixpoint1Prime e)) (locate glr_opt_expr) (fun x  -> x);
        Glr.sequence (locate (Glr.char '+' ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge _loc__unnamed_0 _loc_e in `Fixpoint1 e);
        Glr.sequence
          (Glr.sequence (locate (Glr.char '?' ())) (locate (Glr.char '?' ()))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun _unnamed_1  ->
                  let (_loc__unnamed_1,_unnamed_1) = _unnamed_1 in
                  fun e  ->
                    let (_loc_e,e) = e in
                    let _loc = merge _loc__unnamed_0 _loc_e in `OptionPrime e))
          (locate glr_opt_expr) (fun x  -> x);
        Glr.sequence (locate (Glr.char '?' ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge _loc__unnamed_0 _loc_e in `Option e);
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in `Once) (locate (Glr.empty ()))]
    let glr_sequence =
      Glr.alternatives
        [Glr.sequence
           (Glr.sequence (locate (Glr.char '{' ())) (locate glr_rules)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun r  ->
                   let (_loc_r,r) = r in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc = merge _loc__unnamed_0 _loc__unnamed_2 in r))
           (locate (Glr.char '}' ())) (fun x  -> x);
        Glr.sequence (locate (Glr.string "EOF" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge _loc__unnamed_0 _loc_opt in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "eof") [e]);
        Glr.sequence (locate (Glr.string "EMPTY" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge _loc__unnamed_0 _loc_opt in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "empty") [e]);
        Glr.sequence (locate (Glr.string "FAIL" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge _loc__unnamed_0 _loc_e in
               exp_apply _loc (exp_glr_fun _loc "fail") [e]);
        Glr.sequence (locate (Glr.string "DEBUG" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge _loc__unnamed_0 _loc_e in
               exp_apply _loc (exp_glr_fun _loc "debug") [e]);
        Glr.sequence
          (Glr.sequence (locate (Glr.string "CHR" ()))
             (locate (expression_lvl (next_exp App)))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun opt  ->
                    let (_loc_opt,opt) = opt in
                    let _loc = merge _loc__unnamed_0 _loc_opt in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "char") [e; opt]))
          (locate glr_opt_expr) (fun x  -> x);
        Glr.sequence
          (Glr.sequence (locate (Glr.string "STR" ()))
             (locate (expression_lvl (next_exp App)))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun opt  ->
                    let (_loc_opt,opt) = opt in
                    let _loc = merge _loc__unnamed_0 _loc_opt in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "string") [e; opt]))
          (locate glr_opt_expr) (fun x  -> x);
        Glr.sequence
          (Glr.sequence (locate (Glr.string "RE" ()))
             (locate (expression_lvl (next_exp App)))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun opt  ->
                    let (_loc_opt,opt) = opt in
                    let _loc = merge _loc__unnamed_0 _loc_opt in
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
          (locate glr_opt_expr) (fun x  -> x);
        Glr.apply (fun e  -> let (_loc_e,e) = e in let _loc = _loc_e in e)
          (locate (expression_lvl Atom))]
    let glr_list_sequence =
      Glr.alternatives
        [Glr.sequence
           (Glr.sequence (locate (Glr.char '{' ())) (locate glr_list_rules)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun r  ->
                   let (_loc_r,r) = r in
                   fun _unnamed_2  ->
                     let (_loc__unnamed_2,_unnamed_2) = _unnamed_2 in
                     let _loc = merge _loc__unnamed_0 _loc__unnamed_2 in r))
           (locate (Glr.char '}' ())) (fun x  -> x);
        Glr.sequence (locate (Glr.string "EOF" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge _loc__unnamed_0 _loc_opt in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "list_eof") [e]);
        Glr.sequence (locate (Glr.string "EMPTY" ())) (locate glr_opt_expr)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun opt  ->
               let (_loc_opt,opt) = opt in
               let _loc = merge _loc__unnamed_0 _loc_opt in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "list_empty") [e]);
        Glr.sequence (locate (Glr.string "FAIL" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge _loc__unnamed_0 _loc_e in
               exp_apply _loc (exp_glr_fun _loc "list_fail") [e]);
        Glr.sequence (locate (Glr.string "DEBUG" ()))
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun e  ->
               let (_loc_e,e) = e in
               let _loc = merge _loc__unnamed_0 _loc_e in
               exp_apply _loc (exp_glr_fun _loc "list_debug") [e]);
        Glr.sequence
          (Glr.sequence (locate (Glr.string "CHR" ()))
             (locate (expression_lvl (next_exp App)))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun opt  ->
                    let (_loc_opt,opt) = opt in
                    let _loc = merge _loc__unnamed_0 _loc_opt in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "list_char") [e; opt]))
          (locate glr_opt_expr) (fun x  -> x);
        Glr.sequence
          (Glr.sequence (locate (Glr.string "STR" ()))
             (locate (expression_lvl (next_exp App)))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun opt  ->
                    let (_loc_opt,opt) = opt in
                    let _loc = merge _loc__unnamed_0 _loc_opt in
                    let opt =
                      match opt with | None  -> exp_unit _loc | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "list_string") [e; opt]))
          (locate glr_opt_expr) (fun x  -> x);
        Glr.sequence
          (Glr.sequence (locate (Glr.string "RE" ()))
             (locate (expression_lvl (next_exp App)))
             (fun _unnamed_0  ->
                let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun opt  ->
                    let (_loc_opt,opt) = opt in
                    let _loc = merge _loc__unnamed_0 _loc_opt in
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
          (locate glr_opt_expr) (fun x  -> x);
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
                let _loc = merge _loc_p _loc__unnamed_1 in
                match p.ppat_desc with
                | Ppat_alias (p,{ txt = id }) -> (id, (Some p))
                | Ppat_var { txt = id } -> (id, None)
                | _ -> ("_", (Some p)));
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in ("_", None))
          (locate (Glr.empty ()))]
    let glr_left_member =
      Glr.apply (fun l  -> let (_loc_l,l) = l in let _loc = _loc_l in l)
        (locate
           (Glr.sequence
              (Glr.sequence
                 (Glr.sequence (locate glr_ident) (locate glr_sequence)
                    (fun id  ->
                       let (_loc_id,id) = id in
                       fun s  ->
                         let (_loc_s,s) = s in
                         fun opt  ->
                           let (_loc_opt,opt) = opt in
                           let _loc = merge _loc_id _loc_opt in (id, s, opt)))
                 (locate glr_option) (fun x  -> x))
              (Glr.fixpoint []
                 (Glr.apply (fun x  -> fun l  -> x :: l)
                    (Glr.sequence
                       (Glr.sequence (locate glr_ident) (locate glr_sequence)
                          (fun id  ->
                             let (_loc_id,id) = id in
                             fun s  ->
                               let (_loc_s,s) = s in
                               fun opt  ->
                                 let (_loc_opt,opt) = opt in
                                 let _loc = merge _loc_id _loc_opt in
                                 (id, s, opt))) (locate glr_option)
                       (fun x  -> x))))
              (fun x  -> fun l  -> x :: (List.rev l))))
    let glr_list_left_member =
      Glr.apply (fun l  -> let (_loc_l,l) = l in let _loc = _loc_l in l)
        (locate
           (Glr.sequence
              (Glr.sequence
                 (Glr.sequence (locate glr_ident) (locate glr_list_sequence)
                    (fun id  ->
                       let (_loc_id,id) = id in
                       fun s  ->
                         let (_loc_s,s) = s in
                         fun opt  ->
                           let (_loc_opt,opt) = opt in
                           let _loc = merge _loc_id _loc_opt in (id, s, opt)))
                 (locate glr_option) (fun x  -> x))
              (Glr.fixpoint []
                 (Glr.apply (fun x  -> fun l  -> x :: l)
                    (Glr.sequence
                       (Glr.sequence (locate glr_ident)
                          (locate glr_list_sequence)
                          (fun id  ->
                             let (_loc_id,id) = id in
                             fun s  ->
                               let (_loc_s,s) = s in
                               fun opt  ->
                                 let (_loc_opt,opt) = opt in
                                 let _loc = merge _loc_id _loc_opt in
                                 (id, s, opt))) (locate glr_option)
                       (fun x  -> x))))
              (fun x  -> fun l  -> x :: (List.rev l))))
    let glr_let = Glr.declare_grammar "glr_let"
    let _ =
      Glr.set_grammar glr_let
        (Glr.alternatives
           [Glr.sequence
              (Glr.sequence
                 (Glr.sequence
                    (Glr.sequence (locate (Glr.string "let" ()))
                       (locate rec_flag)
                       (fun _unnamed_0  ->
                          let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                          fun r  ->
                            let (_loc_r,r) = r in
                            fun lbs  ->
                              let (_loc_lbs,lbs) = lbs in
                              fun _unnamed_3  ->
                                let (_loc__unnamed_3,_unnamed_3) = _unnamed_3 in
                                fun l  ->
                                  let (_loc_l,l) = l in
                                  let _loc = merge _loc__unnamed_0 _loc_l in
                                  fun x  ->
                                    loc_expr _loc (Pexp_let (r, lbs, (l x)))))
                    (locate let_binding) (fun x  -> x))
                 (locate (Glr.string "in" ())) (fun x  -> x))
              (locate glr_let) (fun x  -> x);
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
                let _loc = merge _loc__unnamed_0 _loc_e in Some e);
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
                let _loc = merge _loc__unnamed_0 _loc__unnamed_1 in
                DepSeq (def, cond, r));
        Glr.sequence (locate (Glr.string "->" ())) (locate expression)
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun action  ->
               let (_loc_action,action) = action in
               let _loc = merge _loc__unnamed_0 _loc_action in Normal action);
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
                let _loc = merge _loc__unnamed_0 _loc__unnamed_1 in
                DepSeq (def, cond, r));
        Glr.sequence (locate (Glr.string "->" ()))
          (locate (expression_lvl (next_exp Disj)))
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             fun action  ->
               let (_loc_action,action) = action in
               let _loc = merge _loc__unnamed_0 _loc_action in Normal action);
        Glr.apply
          (fun _unnamed_0  ->
             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
             let _loc = _loc__unnamed_0 in Default) (locate (Glr.empty ()))]
    let _ =
      Glr.set_grammar glr_rule
        (Glr.sequence
           (Glr.sequence
              (Glr.sequence (locate glr_let) (locate glr_left_member)
                 (fun def  ->
                    let (_loc_def,def) = def in
                    fun l  ->
                      let (_loc_l,l) = l in
                      fun condition  ->
                        let (_loc_condition,condition) = condition in
                        fun action  ->
                          let (_loc_action,action) = action in
                          let _loc = merge _loc_def _loc_action in
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
                            | (id,e,opt)::[] ->
                                let e = apply_option _loc opt e in
                                exp_apply _loc (exp_glr_fun _loc "apply")
                                  [apply _loc (id :: ids) action; e]
                            | (id,e,opt)::(id',e',opt')::[] ->
                                let e = apply_option _loc opt e in
                                let e' = apply_option _loc opt' e' in
                                exp_apply _loc (exp_glr_fun _loc "sequence")
                                  [e';
                                  e;
                                  apply _loc (id' :: id :: ids) action]
                            | (id,e,opt)::ls ->
                                let e = apply_option _loc opt e in
                                exp_apply _loc (exp_glr_fun _loc "sequence")
                                  [fn (id :: ids) ls;
                                  e;
                                  exp_fun _loc "x" (exp_ident _loc "x")] in
                          let res = fn [] (List.rev l) in
                          let res =
                            if iter
                            then
                              exp_apply _loc (exp_glr_fun _loc "iter") [res]
                            else res in
                          (def, condition, res))) (locate glr_cond)
              (fun x  -> x)) (locate glr_action) (fun x  -> x))
    let _ =
      Glr.set_grammar glr_list_rule
        (Glr.sequence
           (Glr.sequence
              (Glr.sequence (locate glr_let) (locate glr_list_left_member)
                 (fun def  ->
                    let (_loc_def,def) = def in
                    fun l  ->
                      let (_loc_l,l) = l in
                      fun condition  ->
                        let (_loc_condition,condition) = condition in
                        fun action  ->
                          let (_loc_action,action) = action in
                          let _loc = merge _loc_def _loc_action in
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
                            | (id,e,opt)::[] ->
                                let e = apply_list_option _loc opt e in
                                exp_apply _loc (exp_glr_fun _loc "apply")
                                  [apply _loc (id :: ids) action; e]
                            | (id,e,opt)::(id',e',opt')::[] ->
                                let e = apply_list_option _loc opt e in
                                let e' = apply_list_option _loc opt' e' in
                                exp_apply _loc
                                  (exp_glr_fun _loc "list_sequence")
                                  [e';
                                  e;
                                  apply _loc (id' :: id :: ids) action]
                            | (id,e,opt)::ls ->
                                let e = apply_list_option _loc opt e in
                                exp_apply _loc
                                  (exp_glr_fun _loc "list_sequence")
                                  [fn (id :: ids) ls;
                                  e;
                                  exp_fun _loc "x" (exp_ident _loc "x")] in
                          let res = fn [] (List.rev l) in
                          let res =
                            if iter
                            then
                              exp_apply _loc (exp_glr_fun _loc "iter_list")
                                [res]
                            else res in
                          (def, condition, res))) (locate glr_cond)
              (fun x  -> x)) (locate glr_list_action) (fun x  -> x))
    let glr_rules_aux =
      Glr.sequence
        (Glr.sequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x) (Glr.char '|' ()))))
           (locate glr_rule)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun r  ->
                let (_loc_r,r) = r in
                fun rs  ->
                  let (_loc_rs,rs) = rs in
                  let _loc = merge _loc__unnamed_0 _loc_rs in
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
                            let _loc = merge _loc__unnamed_0 _loc_r in r))))))
        (fun x  -> x)
    let glr_list_rules_aux =
      Glr.sequence
        (Glr.sequence
           (locate
              (Glr.option None
                 (Glr.apply (fun x  -> Some x) (Glr.char '|' ()))))
           (locate glr_list_rule)
           (fun _unnamed_0  ->
              let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
              fun r  ->
                let (_loc_r,r) = r in
                fun rs  ->
                  let (_loc_rs,rs) = rs in
                  let _loc = merge _loc__unnamed_0 _loc_rs in
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
                            let _loc = merge _loc__unnamed_0 _loc_r in r))))))
        (fun x  -> x)
    let _ =
      Glr.set_grammar glr_rules
        (Glr.sequence
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
                                  let (_loc__unnamed_1,_unnamed_1) =
                                    _unnamed_1 in
                                  let _loc =
                                    merge _loc__unnamed_0 _loc__unnamed_1 in
                                  ());
                          Glr.apply
                            (fun _unnamed_0  ->
                               let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                               let _loc = _loc__unnamed_0 in ())
                            (locate else_kw)])))) (locate glr_rules_aux)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun r  ->
                   let (_loc_r,r) = r in
                   fun rs  ->
                     let (_loc_rs,rs) = rs in
                     let _loc = merge _loc__unnamed_0 _loc_rs in
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
                           [l]))
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
                                          merge _loc__unnamed_0
                                            _loc__unnamed_1 in
                                        ());
                                Glr.apply
                                  (fun _unnamed_0  ->
                                     let (_loc__unnamed_0,_unnamed_0) =
                                       _unnamed_0 in
                                     let _loc = _loc__unnamed_0 in ())
                                  (locate else_kw)])) (locate glr_rules_aux)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun r  ->
                               let (_loc_r,r) = r in
                               let _loc = merge _loc__unnamed_0 _loc_r in r))))))
           (fun x  -> x))
    let _ =
      Glr.set_grammar glr_list_rules
        (Glr.sequence
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
                                 merge _loc__unnamed_0 _loc__unnamed_1 in
                               ()))))) (locate glr_list_rules_aux)
              (fun _unnamed_0  ->
                 let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                 fun r  ->
                   let (_loc_r,r) = r in
                   fun rs  ->
                     let (_loc_rs,rs) = rs in
                     let _loc = merge _loc__unnamed_0 _loc_rs in
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
                                       merge _loc__unnamed_0 _loc__unnamed_1 in
                                     ()))) (locate glr_list_rules_aux)
                          (fun _unnamed_0  ->
                             let (_loc__unnamed_0,_unnamed_0) = _unnamed_0 in
                             fun r  ->
                               let (_loc_r,r) = r in
                               let _loc = merge _loc__unnamed_0 _loc_r in r))))))
           (fun x  -> x))
  end
let _ = register_extension (module Ext)
