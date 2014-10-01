open Asttypes
open Parsetree
open Longident
open Pa_ocaml_prelude
open Pa_ast
type action =  
  | Default
  | Normal of expression
  | DepSeq of (expression -> expression)* expression option* expression 
let find_locate () =
  try Some (exp_ident Location.none (Sys.getenv "LOCATE"))
  with | Not_found  -> None
let mkpatt _loc (id,p) =
  match (p, (find_locate ())) with
  | (None ,_) -> pat_ident _loc id
  | (Some p,None ) -> ppat_alias _loc p id
  | (Some p,Some _) ->
      ppat_alias _loc (loc_pat _loc (Ppat_tuple [loc_pat _loc Ppat_any; p]))
        id
let mkpatt' _loc (id,p) =
  match p with | None  -> pat_ident _loc id | Some p -> ppat_alias _loc p id
let filter _loc visible r =
  match ((find_locate ()), visible) with
  | (Some f2,true ) ->
      let f =
        exp_fun _loc "x"
          (exp_fun _loc "str"
             (exp_fun _loc "pos"
                (exp_fun _loc "str'"
                   (exp_fun _loc "pos'"
                      (exp_tuple _loc
                         [exp_apply _loc f2
                            [exp_ident _loc "str";
                            exp_ident _loc "pos";
                            exp_ident _loc "str'";
                            exp_ident _loc "pos'"];
                         exp_ident _loc "x"]))))) in
      exp_apply _loc (exp_glr_fun _loc "apply_position") [f; r]
  | _ -> r
let rec build_action _loc occur_loc ids e =
  let e =
    match ((find_locate ()), occur_loc) with
    | (Some locate2,true ) ->
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
    (fun e  ((id,x),visible)  ->
       match ((find_locate ()), visible) with
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
                                      (Ppat_var (id_loc ("_loc_" ^ id) _loc));
                                   loc_pat _loc (Ppat_var (id_loc id _loc))]))
                             (loc_expr _loc
                                (Pexp_ident (id_loc (Lident id) _loc)))], e)))))
       | _ -> loc_expr _loc (pexp_fun ("", None, (mkpatt' _loc (id, x)), e)))
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
    let glr_rules = Decap.declare_grammar "glr_rules"
    let glr_rule = Decap.declare_grammar "glr_rule"
    let location_name_re = "_loc\\([a-zA-Z0-9_']*\\)"
    let glr_parser =
      Decap.alternatives
        [Decap.sequence parser_kw glr_rules (fun _  p  -> (Atom, p));
        Decap.fsequence_position parser_kw
          (Decap.sequence (Decap.char '*' '*') glr_rules
             (fun _  p  _  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                (Atom, (exp_apply _loc (exp_glr_fun _loc "lists") [p]))))]
    let extra_expressions = glr_parser :: extra_expressions
    let glr_opt_expr =
      Decap.apply (fun e  -> e)
        (Decap.option None
           (Decap.apply (fun x  -> Some x)
              (Decap.fsequence (Decap.char '[' '[')
                 (Decap.sequence expression (Decap.char ']' ']')
                    (fun e  _  _  -> e)))))
    let glr_option =
      Decap.alternatives
        [Decap.fsequence (Decap.char '*' '*')
           (Decap.sequence
              (Decap.option None
                 (Decap.apply (fun x  -> Some x) (Decap.char '*' '*')))
              glr_opt_expr
              (fun strict  e  _  -> `Fixpoint ((strict <> None), e)));
        Decap.fsequence (Decap.char '+' '+')
          (Decap.sequence
             (Decap.option None
                (Decap.apply (fun x  -> Some x) (Decap.char '+' '+')))
             glr_opt_expr
             (fun strict  e  _  -> `Fixpoint1 ((strict <> None), e)));
        Decap.fsequence (Decap.char '?' '?')
          (Decap.sequence
             (Decap.option None
                (Decap.apply (fun x  -> Some x) (Decap.char '?' '?')))
             glr_opt_expr
             (fun strict  e  _  -> `Option ((strict <> None), e)));
        Decap.apply (fun _  -> `Once) (Decap.empty ())]
    let glr_sequence =
      Decap.alternatives
        [Decap.fsequence (Decap.char '{' '{')
           (Decap.sequence glr_rules (Decap.char '}' '}') (fun r  _  _  -> r));
        Decap.sequence_position (Decap.string "EOF" "EOF") glr_opt_expr
          (fun _  opt  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             let e = match opt with | None  -> exp_unit _loc | Some e -> e in
             exp_apply _loc (exp_glr_fun _loc "eof") [e]);
        Decap.sequence_position (Decap.string "EMPTY" "EMPTY") glr_opt_expr
          (fun _  opt  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             let e = match opt with | None  -> exp_unit _loc | Some e -> e in
             exp_apply _loc (exp_glr_fun _loc "empty") [e]);
        Decap.sequence_position (Decap.string "FAIL" "FAIL")
          (expression_lvl (next_exp App))
          (fun _  e  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             exp_apply _loc (exp_glr_fun _loc "fail") [e]);
        Decap.sequence_position (Decap.string "DEBUG" "DEBUG")
          (expression_lvl (next_exp App))
          (fun _  e  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             exp_apply _loc (exp_glr_fun _loc "debug") [e]);
        Decap.apply_position
          (fun _  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             exp_glr_fun _loc "any") (Decap.string "ANY" "ANY");
        Decap.fsequence_position (Decap.string "CHR" "CHR")
          (Decap.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  opt  _  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                let opt = match opt with | None  -> e | Some e -> e in
                exp_apply _loc (exp_glr_fun _loc "char") [e; opt]));
        Decap.fsequence_position (Decap.string "STR" "STR")
          (Decap.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  opt  _  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                let opt = match opt with | None  -> e | Some e -> e in
                exp_apply _loc (exp_glr_fun _loc "string") [e; opt]));
        Decap.fsequence_position (Decap.string "RE" "RE")
          (Decap.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  opt  _  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
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
                      [e; exp_fun _loc "groupe" opt]));
        Decap.apply (fun e  -> e) (expression_lvl Atom)]
    let glr_ident =
      Decap.alternatives
        [Decap.sequence (pattern_lvl ConstrPat) (Decap.char ':' ':')
           (fun p  _  ->
              match p.ppat_desc with
              | Ppat_alias (p,{ txt = id }) -> (id, (Some p))
              | Ppat_var { txt = id } -> (id, None)
              | _ -> ("_", (Some p)));
        Decap.apply (fun _  -> ("_", None)) (Decap.empty ())]
    let dash =
      Decap.black_box
        (fun str  pos  ->
           let (c,str',pos') = Input.read str pos in
           if c = '-'
           then
             let (c',_,_) = Input.read str' pos' in
             (if c' = '>'
              then raise (Decap.Give_up "'-' expected")
              else ((), str', pos'))
           else raise (Decap.Give_up "'-' expexted")) (Charset.singleton '-')
        false "-"
    let glr_left_member =
      Decap.sequence
        (Decap.fsequence glr_ident
           (Decap.sequence glr_sequence glr_option
              (fun s  opt  id  -> `Normal (id, s, opt))))
        (Decap.apply List.rev
           (Decap.fixpoint []
              (Decap.apply (fun x  l  -> x :: l)
                 (Decap.alternatives
                    [Decap.fsequence glr_ident
                       (Decap.sequence glr_sequence glr_option
                          (fun s  opt  id  -> `Normal (id, s, opt)));
                    Decap.apply (fun _  -> `Ignore) dash]))))
        (fun i  l  -> i :: l)
    let glr_let = Decap.declare_grammar "glr_let"
    let _ =
      Decap.set_grammar glr_let
        (Decap.alternatives
           [Decap.fsequence_position (Decap.string "let" "let")
              (Decap.fsequence rec_flag
                 (Decap.fsequence let_binding
                    (Decap.sequence (Decap.string "in" "in") glr_let
                       (fun _  l  lbs  r  _  __loc__start__buf 
                          __loc__start__pos  __loc__end__buf  __loc__end__pos
                           ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          fun x  -> loc_expr _loc (Pexp_let (r, lbs, (l x)))))));
           Decap.apply (fun _  x  -> x) (Decap.empty ())])
    let glr_cond =
      Decap.alternatives
        [Decap.sequence (Decap.string "when" "when") expression
           (fun _  e  -> Some e);
        Decap.apply (fun _  -> None) (Decap.empty ())]
    let glr_action =
      Decap.alternatives
        [Decap.sequence (Decap.string "->>" "->>") glr_rule
           (fun _  (def,cond,r)  -> DepSeq (def, cond, r));
        Decap.sequence (Decap.string "->" "->") expression
          (fun _  action  -> Normal action);
        Decap.apply (fun _  -> Default) (Decap.empty ())]
    let _ =
      Decap.set_grammar glr_rule
        (Decap.iter
           (Decap.fsequence glr_let
              (Decap.sequence glr_left_member glr_cond
                 (fun l  condition  def  ->
                    let _ = push_frame () in
                    Decap.apply_position
                      (fun action  __loc__start__buf  __loc__start__pos 
                         __loc__end__buf  __loc__end__pos  ->
                         let _loc =
                           locate __loc__start__buf __loc__start__pos
                             __loc__end__buf __loc__end__pos in
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
                         let occur_loc = pop_location "" in
                         let rec fn first ids l =
                           match l with
                           | [] -> assert false
                           | `Ignore::ls -> assert false
                           | (`Normal (id,e,opt))::`Ignore::ls ->
                               let e =
                                 exp_apply _loc
                                   (exp_glr_fun _loc "ignore_next_blank") 
                                   [e] in
                               fn first ids ((`Normal (id, e, opt)) :: ls)
                           | (`Normal (id,e,opt))::[] ->
                               let occur_loc_id =
                                 ((fst id) <> "_") && (pop_location (fst id)) in
                               let e = apply_option _loc opt occur_loc_id e in
                               let f =
                                 match ((find_locate ()),
                                         (first && occur_loc))
                                 with
                                 | (Some _,true ) -> "apply_position"
                                 | _ -> "apply" in
                               exp_apply _loc (exp_glr_fun _loc f)
                                 [build_action _loc occur_loc
                                    ((id, occur_loc_id) :: ids) action;
                                 e]
                           | (`Normal (id,e,opt))::(`Normal (id',e',opt'))::[]
                               ->
                               let occur_loc_id =
                                 ((fst id) <> "_") && (pop_location (fst id)) in
                               let occur_loc_id' =
                                 ((fst id') <> "_") &&
                                   (pop_location (fst id')) in
                               let e = apply_option _loc opt occur_loc_id e in
                               let e' =
                                 apply_option _loc opt' occur_loc_id' e' in
                               let f =
                                 match ((find_locate ()),
                                         (first && occur_loc))
                                 with
                                 | (Some _,true ) -> "sequence_position"
                                 | _ -> "sequence" in
                               exp_apply _loc (exp_glr_fun _loc f)
                                 [e;
                                 e';
                                 build_action _loc occur_loc
                                   ((id, occur_loc_id) ::
                                   (id', occur_loc_id') :: ids) action]
                           | (`Normal (id,e,opt))::ls ->
                               let occur_loc_id =
                                 ((fst id) <> "_") && (pop_location (fst id)) in
                               let e = apply_option _loc opt occur_loc_id e in
                               let f =
                                 match ((find_locate ()),
                                         (first && occur_loc))
                                 with
                                 | (Some _,true ) -> "fsequence_position"
                                 | _ -> "fsequence" in
                               exp_apply _loc (exp_glr_fun _loc f)
                                 [e; fn false ((id, occur_loc_id) :: ids) ls] in
                         let res = fn true [] l in
                         pop_frame ();
                         (let res =
                            if iter
                            then
                              exp_apply _loc (exp_glr_fun _loc "iter") [res]
                            else res in
                          (def, condition, res))) glr_action))))
    let glr_rules_aux =
      Decap.fsequence_position
        (Decap.option None
           (Decap.apply (fun x  -> Some x)
              (Decap.sequence (Decap.char '|' '|') (Decap.char '|' '|')
                 (fun _  _  -> ()))))
        (Decap.sequence glr_rule
           (Decap.apply List.rev
              (Decap.fixpoint []
                 (Decap.apply (fun x  l  -> x :: l)
                    (Decap.fsequence (Decap.char '|' '|')
                       (Decap.sequence (Decap.char '|' '|') glr_rule
                          (fun _  r  _  -> r))))))
           (fun r  rs  _  __loc__start__buf  __loc__start__pos 
              __loc__end__buf  __loc__end__pos  ->
              let _loc =
                locate __loc__start__buf __loc__start__pos __loc__end__buf
                  __loc__end__pos in
              match rs with
              | [] -> r
              | l ->
                  let l =
                    List.fold_right
                      (fun (def,cond,x)  y  ->
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
                                               (Some (exp_ident _loc "y")))))))))
                      (r :: l) (exp_Nil _loc) in
                  (((fun x  -> x)), None,
                    (exp_apply _loc (exp_glr_fun _loc "alternatives'") [l]))))
    let _ =
      Decap.set_grammar glr_rules
        (Decap.fsequence_position
           (Decap.option None
              (Decap.apply (fun x  -> Some x)
                 (Decap.apply (fun _  -> ()) (Decap.char '|' '|'))))
           (Decap.sequence glr_rules_aux
              (Decap.apply List.rev
                 (Decap.fixpoint []
                    (Decap.apply (fun x  l  -> x :: l)
                       (Decap.sequence
                          (Decap.apply (fun _  -> ()) (Decap.char '|' '|'))
                          glr_rules_aux (fun _  r  -> r)))))
              (fun r  rs  _  __loc__start__buf  __loc__start__pos 
                 __loc__end__buf  __loc__end__pos  ->
                 let _loc =
                   locate __loc__start__buf __loc__start__pos __loc__end__buf
                     __loc__end__pos in
                 match (r, rs) with
                 | ((def,cond,e),[]) ->
                     (match cond with
                      | None  -> def e
                      | Some c ->
                          loc_expr _loc
                            (Pexp_ifthenelse
                               (c, e,
                                 (Some
                                    (exp_apply _loc (exp_glr_fun _loc "fail")
                                       [exp_string _loc ""])))))
                 | (r,l) ->
                     let l =
                       List.fold_right
                         (fun (def,cond,x)  y  ->
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
                                                  (Some (exp_ident _loc "y")))))))))
                         (r :: l) (exp_Nil _loc) in
                     exp_apply _loc (exp_glr_fun _loc "alternatives") [l])))
  end
let _ = register_extension (module Ext : FExt )
