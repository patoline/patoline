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
let ppat_alias _loc p id =
  if id = "_" then p else loc_pat _loc (Ppat_alias (p, (id_loc id _loc)))
let mkpatt _loc (id,p) =
  match (p, (!do_locate)) with
  | (None ,_) -> pat_ident _loc id
  | (Some p,None ) -> ppat_alias _loc p id
  | (Some p,Some _) ->
      ppat_alias _loc (loc_pat _loc (Ppat_tuple [loc_pat _loc Ppat_any; p]))
        id
let mkpatt' _loc (id,p) =
  match p with | None  -> pat_ident _loc id | Some p -> ppat_alias _loc p id
let filter _loc visible r =
  match ((!do_locate), visible) with
  | (Some (f,_),true ) -> loc_expr _loc (Pexp_apply (f, [("", r)]))
  | _ -> r
let (push_frame,pop_frame,push_location,pop_location) =
  let loc_tbl = Stack.create () in
  ((fun ()  -> Stack.push (Hashtbl.create 23) loc_tbl),
    (fun ()  ->
       let h = try Stack.pop loc_tbl with | Stack.Empty  -> assert false in
       Hashtbl.iter
         (fun l  ->
            fun _  ->
              try let h' = Stack.top loc_tbl in Hashtbl.replace h' l ()
              with | Stack.Empty  -> ()) h),
    (fun id  ->
       try let h = Stack.top loc_tbl in Hashtbl.replace h id ()
       with | Stack.Empty  -> ()),
    (fun id  ->
       try
         let h = Stack.top loc_tbl in
         if Hashtbl.mem h id then (Hashtbl.remove h id; true) else false
       with | Stack.Empty  -> false))
let rec build_action _loc occur_loc ids e =
  let e =
    match ((!do_locate), occur_loc) with
    | (Some (_,locate2),true ) ->
        exp_fun _loc "__loc__start__buf"
          (exp_fun _loc "__loc__start__pos"
             (exp_fun _loc "__loc__end__buf"
                (exp_fun _loc "__loc__end__pos"
                   (loc_expr _loc
                      (Pexp_let
                         (Nonrecursive,
                           [value_binding _loc (pat_ident _loc "_loc")
                              (exp_apply _loc locate2
                                 [exp_ident _loc "__loc__start__buf";
                                 exp_ident _loc "__loc__start__pos";
                                 exp_ident _loc "__loc__end__buf";
                                 exp_ident _loc "__loc__end__pos"])], e))))))
    | _ -> e in
  List.fold_left
    (fun e  ->
       fun ((id,x),visible)  ->
         match ((!do_locate), visible) with
         | (Some _,true ) ->
             loc_expr _loc
               (pexp_fun
                  ("", None, (mkpatt _loc (id, x)),
                    (loc_expr _loc
                       (Pexp_let
                          (Nonrecursive,
                            [value_binding _loc
                               (loc_pat _loc
                                  (Ppat_tuple
                                     [loc_pat _loc
                                        (Ppat_var
                                           (id_loc ("_loc_" ^ id) _loc));
                                     loc_pat _loc (Ppat_var (id_loc id _loc))]))
                               (loc_expr _loc
                                  (Pexp_ident (id_loc (Lident id) _loc)))],
                            e)))))
         | _ ->
             loc_expr _loc (pexp_fun ("", None, (mkpatt' _loc (id, x)), e)))
    e (List.rev ids)
let apply_option _loc opt visible e =
  filter _loc visible
    (match opt with
     | `Once -> e
     | `Option (strict,d) ->
         let f = if strict then "option'" else "option" in
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc f)
                [exp_None _loc;
                exp_apply _loc (exp_glr_fun _loc "apply")
                  [exp_Some_fun _loc; e]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc f) [d; e])
     | `Fixpoint (strict,d) ->
         let f = if strict then "fixpoint'" else "fixpoint" in
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "apply")
                [exp_list_fun _loc "rev";
                exp_apply _loc (exp_glr_fun _loc f)
                  [exp_Nil _loc;
                  exp_apply _loc (exp_glr_fun _loc "apply")
                    [exp_Cons_fun _loc; e]]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc f) [d; e])
     | `Fixpoint1 (strict,d) ->
         let f = if strict then "fixpoint'" else "fixpoint" in
         (match d with
          | None  ->
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
    let glr_rule = Glr.declare_grammar "glr_rule"
    let location_name_re = "_loc\\([a-zA-Z0-9_']*\\)"
    let glr_parser =
      Glr.alternatives'
        [Glr.fsequence_position (Glr.string "parser_locate" "parser_locate")
           (Glr.sequence (expression_lvl (next_exp App))
              (expression_lvl (next_exp App))
              (fun filter2  ->
                 fun merge2  ->
                   fun _  ->
                     fun __loc__start__buf  ->
                       fun __loc__start__pos  ->
                         fun __loc__end__buf  ->
                           fun __loc__end__pos  ->
                             let _loc =
                               locate2 __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             do_locate := (Some (filter2, merge2));
                             (Atom, (exp_unit _loc))));
        Glr.sequence parser_kw glr_rules (fun _  -> fun p  -> (Atom, p));
        Glr.fsequence_position parser_kw
          (Glr.sequence (Glr.char '*' '*') glr_rules
             (fun _  ->
                fun p  ->
                  fun _  ->
                    fun __loc__start__buf  ->
                      fun __loc__start__pos  ->
                        fun __loc__end__buf  ->
                          fun __loc__end__pos  ->
                            let _loc =
                              locate2 __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            (Atom,
                              (exp_apply _loc (exp_glr_fun _loc "lists") [p]))));
        Glr.apply_position
          (fun (id,id')  ->
             fun __loc__start__buf  ->
               fun __loc__start__pos  ->
                 fun __loc__end__buf  ->
                   fun __loc__end__pos  ->
                     let _loc =
                       locate2 __loc__start__buf __loc__start__pos
                         __loc__end__buf __loc__end__pos in
                     let id' =
                       if id' = ""
                       then ""
                       else
                         if ((String.length id') > 1) && ((id'.[0]) = '_')
                         then String.sub id' 1 ((String.length id') - 1)
                         else raise Glr.Give_up in
                     push_location id'; (Atom, (exp_ident _loc id)))
          (Glr.regexp ~name:"location_name" location_name_re
             (fun groupe  -> ((groupe 0), (groupe 1))))]
    let extra_expressions = glr_parser :: extra_expressions
    let glr_opt_expr =
      Glr.apply (fun e  -> e)
        (Glr.option None
           (Glr.apply (fun x  -> Some x)
              (Glr.fsequence (Glr.char '[' '[')
                 (Glr.sequence expression (Glr.char ']' ']')
                    (fun e  -> fun _  -> fun _  -> e)))))
    let glr_option =
      Glr.alternatives'
        [Glr.fsequence (Glr.char '*' '*')
           (Glr.sequence
              (Glr.option None
                 (Glr.apply (fun x  -> Some x) (Glr.char '*' '*')))
              glr_opt_expr
              (fun strict  ->
                 fun e  -> fun _  -> `Fixpoint ((strict <> None), e)));
        Glr.fsequence (Glr.char '+' '+')
          (Glr.sequence
             (Glr.option None
                (Glr.apply (fun x  -> Some x) (Glr.char '+' '+')))
             glr_opt_expr
             (fun strict  ->
                fun e  -> fun _  -> `Fixpoint1 ((strict <> None), e)));
        Glr.fsequence (Glr.char '?' '?')
          (Glr.sequence
             (Glr.option None
                (Glr.apply (fun x  -> Some x) (Glr.char '?' '?')))
             glr_opt_expr
             (fun strict  ->
                fun e  -> fun _  -> `Option ((strict <> None), e)));
        Glr.apply (fun _  -> `Once) (Glr.empty ())]
    let glr_sequence =
      Glr.alternatives'
        [Glr.fsequence (Glr.char '{' '{')
           (Glr.sequence glr_rules (Glr.char '}' '}')
              (fun r  -> fun _  -> fun _  -> r));
        Glr.sequence_position (Glr.string "EOF" "EOF") glr_opt_expr
          (fun _  ->
             fun opt  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate2 __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       let e =
                         match opt with
                         | None  -> exp_unit _loc
                         | Some e -> e in
                       exp_apply _loc (exp_glr_fun _loc "eof") [e]);
        Glr.sequence_position (Glr.string "EMPTY" "EMPTY") glr_opt_expr
          (fun _  ->
             fun opt  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate2 __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       let e =
                         match opt with
                         | None  -> exp_unit _loc
                         | Some e -> e in
                       exp_apply _loc (exp_glr_fun _loc "empty") [e]);
        Glr.sequence_position (Glr.string "FAIL" "FAIL")
          (expression_lvl (next_exp App))
          (fun _  ->
             fun e  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate2 __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       exp_apply _loc (exp_glr_fun _loc "fail") [e]);
        Glr.sequence_position (Glr.string "DEBUG" "DEBUG")
          (expression_lvl (next_exp App))
          (fun _  ->
             fun e  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate2 __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       exp_apply _loc (exp_glr_fun _loc "debug") [e]);
        Glr.apply_position
          (fun _  ->
             fun __loc__start__buf  ->
               fun __loc__start__pos  ->
                 fun __loc__end__buf  ->
                   fun __loc__end__pos  ->
                     let _loc =
                       locate2 __loc__start__buf __loc__start__pos
                         __loc__end__buf __loc__end__pos in
                     exp_glr_fun _loc "any") (Glr.string "ANY" "ANY");
        Glr.fsequence_position (Glr.string "CHR" "CHR")
          (Glr.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  ->
                fun opt  ->
                  fun _  ->
                    fun __loc__start__buf  ->
                      fun __loc__start__pos  ->
                        fun __loc__end__buf  ->
                          fun __loc__end__pos  ->
                            let _loc =
                              locate2 __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            let opt =
                              match opt with | None  -> e | Some e -> e in
                            exp_apply _loc (exp_glr_fun _loc "char") [e; opt]));
        Glr.fsequence_position (Glr.string "STR" "STR")
          (Glr.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  ->
                fun opt  ->
                  fun _  ->
                    fun __loc__start__buf  ->
                      fun __loc__start__pos  ->
                        fun __loc__end__buf  ->
                          fun __loc__end__pos  ->
                            let _loc =
                              locate2 __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            let opt =
                              match opt with | None  -> e | Some e -> e in
                            exp_apply _loc (exp_glr_fun _loc "string")
                              [e; opt]));
        Glr.fsequence_position (Glr.string "RE" "RE")
          (Glr.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  ->
                fun opt  ->
                  fun _  ->
                    fun __loc__start__buf  ->
                      fun __loc__start__pos  ->
                        fun __loc__end__buf  ->
                          fun __loc__end__pos  ->
                            let _loc =
                              locate2 __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
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
                                  if
                                    (l > 3) &&
                                      ((String.sub id (l - 3) 3) = "_re")
                                  then String.sub id 0 (l - 3)
                                  else id in
                                exp_lab_apply _loc
                                  (exp_glr_fun _loc "regexp")
                                  [("name", (exp_string _loc id));
                                  ("", e);
                                  ("", (exp_fun _loc "groupe" opt))]
                            | _ ->
                                exp_apply _loc (exp_glr_fun _loc "regexp")
                                  [e; exp_fun _loc "groupe" opt]));
        Glr.apply (fun e  -> e) (expression_lvl Atom)]
    let glr_ident =
      Glr.alternatives'
        [Glr.sequence (pattern_lvl ConstrPat) (Glr.char ':' ':')
           (fun p  ->
              fun _  ->
                match p.ppat_desc with
                | Ppat_alias (p,{ txt = id }) -> (id, (Some p))
                | Ppat_var { txt = id } -> (id, None)
                | _ -> ("_", (Some p)));
        Glr.apply (fun _  -> ("_", None)) (Glr.empty ())]
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
      Glr.sequence
        (Glr.fsequence glr_ident
           (Glr.sequence glr_sequence glr_option
              (fun s  -> fun opt  -> fun id  -> `Normal (id, s, opt))))
        (Glr.apply List.rev
           (Glr.fixpoint []
              (Glr.apply (fun x  -> fun l  -> x :: l)
                 (Glr.alternatives'
                    [Glr.fsequence glr_ident
                       (Glr.sequence glr_sequence glr_option
                          (fun s  ->
                             fun opt  -> fun id  -> `Normal (id, s, opt)));
                    Glr.apply (fun _  -> `Ignore) dash]))))
        (fun i  -> fun l  -> i :: l)
    let glr_let = Glr.declare_grammar "glr_let"
    let _ =
      Glr.set_grammar glr_let
        (Glr.alternatives'
           [Glr.fsequence_position (Glr.string "let" "let")
              (Glr.fsequence rec_flag
                 (Glr.fsequence let_binding
                    (Glr.sequence (Glr.string "in" "in") glr_let
                       (fun _  ->
                          fun l  ->
                            fun lbs  ->
                              fun r  ->
                                fun _  ->
                                  fun __loc__start__buf  ->
                                    fun __loc__start__pos  ->
                                      fun __loc__end__buf  ->
                                        fun __loc__end__pos  ->
                                          let _loc =
                                            locate2 __loc__start__buf
                                              __loc__start__pos
                                              __loc__end__buf __loc__end__pos in
                                          fun x  ->
                                            loc_expr _loc
                                              (Pexp_let (r, lbs, (l x)))))));
           Glr.apply (fun _  -> fun x  -> x) (Glr.empty ())])
    let glr_cond =
      Glr.alternatives'
        [Glr.sequence (Glr.string "when" "when") expression
           (fun _  -> fun e  -> Some e);
        Glr.apply (fun _  -> None) (Glr.empty ())]
    let glr_action =
      Glr.alternatives'
        [Glr.sequence (Glr.string "->>" "->>") glr_rule
           (fun _  -> fun (def,cond,r)  -> DepSeq (def, cond, r));
        Glr.sequence (Glr.string "->" "->") expression
          (fun _  -> fun action  -> Normal action);
        Glr.apply (fun _  -> Default) (Glr.empty ())]
    let _ =
      Glr.set_grammar glr_rule
        (Glr.iter
           (Glr.fsequence glr_let
              (Glr.sequence glr_left_member glr_cond
                 (fun l  ->
                    fun condition  ->
                      fun def  ->
                        let _ = push_frame () in
                        Glr.apply_position
                          (fun action  ->
                             fun __loc__start__buf  ->
                               fun __loc__start__pos  ->
                                 fun __loc__end__buf  ->
                                   fun __loc__end__pos  ->
                                     let _loc =
                                       locate2 __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     let (iter,action) =
                                       match action with
                                       | Normal a -> (false, a)
                                       | Default  ->
                                           (false, (default_action _loc l))
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
                                                                (exp_apply
                                                                   _loc
                                                                   (exp_glr_fun
                                                                    _loc
                                                                    "fail")
                                                                   [exp_string
                                                                    _loc ""])))))))) in
                                     let occur_loc = pop_location "" in
                                     let rec fn first ids l =
                                       match l with
                                       | [] -> assert false
                                       | `Ignore::ls -> assert false
                                       | (`Normal (id,e,opt))::`Ignore::ls ->
                                           let e =
                                             exp_apply _loc
                                               (exp_glr_fun _loc
                                                  "ignore_next_blank") 
                                               [e] in
                                           fn first ids
                                             ((`Normal (id, e, opt)) :: ls)
                                       | (`Normal (id,e,opt))::[] ->
                                           let occur_loc_id =
                                             ((fst id) <> "_") &&
                                               (pop_location (fst id)) in
                                           let e =
                                             apply_option _loc opt
                                               occur_loc_id e in
                                           let f =
                                             match ((!do_locate),
                                                     (first && occur_loc))
                                             with
                                             | (Some _,true ) ->
                                                 "apply_position"
                                             | _ -> "apply" in
                                           exp_apply _loc
                                             (exp_glr_fun _loc f)
                                             [build_action _loc occur_loc
                                                ((id, occur_loc_id) :: ids)
                                                action;
                                             e]
                                       | (`Normal (id,e,opt))::(`Normal
                                                                  (id',e',opt'))::[]
                                           ->
                                           let occur_loc_id =
                                             ((fst id) <> "_") &&
                                               (pop_location (fst id)) in
                                           let occur_loc_id' =
                                             ((fst id') <> "_") &&
                                               (pop_location (fst id')) in
                                           let e =
                                             apply_option _loc opt
                                               occur_loc_id e in
                                           let e' =
                                             apply_option _loc opt'
                                               occur_loc_id' e' in
                                           let f =
                                             match ((!do_locate),
                                                     (first && occur_loc))
                                             with
                                             | (Some _,true ) ->
                                                 "sequence_position"
                                             | _ -> "sequence" in
                                           exp_apply _loc
                                             (exp_glr_fun _loc f)
                                             [e;
                                             e';
                                             build_action _loc occur_loc
                                               ((id, occur_loc_id) ::
                                               (id', occur_loc_id') :: ids)
                                               action]
                                       | (`Normal (id,e,opt))::ls ->
                                           let occur_loc_id =
                                             ((fst id) <> "_") &&
                                               (pop_location (fst id)) in
                                           let e =
                                             apply_option _loc opt
                                               occur_loc_id e in
                                           let f =
                                             match ((!do_locate),
                                                     (first && occur_loc))
                                             with
                                             | (Some _,true ) ->
                                                 "fsequence_position"
                                             | _ -> "fsequence" in
                                           exp_apply _loc
                                             (exp_glr_fun _loc f)
                                             [e;
                                             fn false ((id, occur_loc_id) ::
                                               ids) ls] in
                                     let res = fn true [] l in
                                     pop_frame ();
                                     (let res =
                                        if iter
                                        then
                                          exp_apply _loc
                                            (exp_glr_fun _loc "iter") 
                                            [res]
                                        else res in
                                      (def, condition, res))) glr_action))))
    let glr_rules_aux =
      Glr.fsequence_position
        (Glr.option None (Glr.apply (fun x  -> Some x) (Glr.char '|' '|')))
        (Glr.sequence glr_rule
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  -> fun l  -> x :: l)
                    (Glr.sequence (Glr.char '|' '|') glr_rule
                       (fun _  -> fun r  -> r)))))
           (fun r  ->
              fun rs  ->
                fun _  ->
                  fun __loc__start__buf  ->
                    fun __loc__start__pos  ->
                      fun __loc__end__buf  ->
                        fun __loc__end__pos  ->
                          let _loc =
                            locate2 __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
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
                                                        (pat_ident _loc "y")
                                                        y],
                                                     (loc_expr _loc
                                                        (Pexp_ifthenelse
                                                           (c,
                                                             (exp_Cons _loc x
                                                                (exp_ident
                                                                   _loc "y")),
                                                             (Some
                                                                (exp_ident
                                                                   _loc "y")))))))))
                                  (r :: l) (exp_Nil _loc) in
                              (((fun x  -> x)), None,
                                (exp_apply _loc
                                   (exp_glr_fun _loc "alternatives'") 
                                   [l]))))
    let _ =
      Glr.set_grammar glr_rules
        (Glr.fsequence_position
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence (Glr.char '|' '|') (Glr.char '|' '|')
                    (fun _  -> fun _  -> ()))))
           (Glr.sequence glr_rules_aux
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  -> fun l  -> x :: l)
                       (Glr.sequence
                          (Glr.sequence (Glr.char '|' '|') (Glr.char '|' '|')
                             (fun _  -> fun _  -> ())) glr_rules_aux
                          (fun _  -> fun r  -> r)))))
              (fun r  ->
                 fun rs  ->
                   fun _  ->
                     fun __loc__start__buf  ->
                       fun __loc__start__pos  ->
                         fun __loc__end__buf  ->
                           fun __loc__end__pos  ->
                             let _loc =
                               locate2 __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
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
                                                           (pat_ident _loc
                                                              "y") y],
                                                        (loc_expr _loc
                                                           (Pexp_ifthenelse
                                                              (c,
                                                                (exp_Cons
                                                                   _loc x
                                                                   (exp_ident
                                                                    _loc "y")),
                                                                (Some
                                                                   (exp_ident
                                                                    _loc "y")))))))))
                                     (r :: l) (exp_Nil _loc) in
                                 exp_apply _loc
                                   (exp_glr_fun _loc "alternatives") 
                                   [l])))
  end
let _ = register_extension (module Ext : FExt)
