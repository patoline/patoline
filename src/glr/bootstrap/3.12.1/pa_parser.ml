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
let mkpatt' _loc (id,p) =
  match p with
  | None  -> pat_ident _loc id
  | Some p -> loc_pat _loc (Ppat_alias (p, (id_loc id _loc)))
let filter _loc visible r =
  match ((!do_locate), visible) with
  | (Some (f,_),true ) -> loc_expr _loc (Pexp_apply (f, [("", r)]))
  | _ -> r
let rec build_action _loc ids e =
  let ids =
    Array.to_list
      (Array.mapi
         (fun i  (id,x)  ->
            if id = "_"
            then (("_unnamed_" ^ (string_of_int i)), x, false)
            else (id, x, true)) (Array.of_list ids)) in
  let e =
    match !do_locate with
    | None  -> e
    | Some (_,locate2) ->
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
                                 exp_ident _loc "__loc__end__pos"])], e)))))) in
  List.fold_left
    (fun e  (id,x,visible)  ->
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
    let glr_rules = Glr.declare_grammar "glr_rules"
    let glr_rule = Glr.declare_grammar "glr_rule"
    let glr_parser =
      Glr.alternatives'
        [Glr.fsequence_position (Glr.string "parser_locate" "parser_locate")
           (Glr.sequence (locate (expression_lvl (next_exp App)))
              (locate (expression_lvl (next_exp App)))
              (fun filter2  ->
                 let (_loc_filter2,filter2) = filter2 in
                 fun merge2  ->
                   let (_loc_merge2,merge2) = merge2 in
                   fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                     __loc__end__buf  __loc__end__pos  ->
                     let _loc =
                       locate2 __loc__start__buf __loc__start__pos
                         __loc__end__buf __loc__end__pos in
                     do_locate := (Some (filter2, merge2));
                     (Atom, (exp_unit _loc))));
        Glr.sequence_position parser_kw (locate glr_rules)
          (fun _unnamed_0  p  ->
             let (_loc_p,p) = p in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               (Atom, p));
        Glr.fsequence_position parser_kw
          (Glr.sequence (Glr.char '*' '*') (locate glr_rules)
             (fun _unnamed_0  p  ->
                let (_loc_p,p) = p in
                fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                  __loc__end__buf  __loc__end__pos  ->
                  let _loc =
                    locate2 __loc__start__buf __loc__start__pos
                      __loc__end__buf __loc__end__pos in
                  (Atom, (exp_apply _loc (exp_glr_fun _loc "lists") [p]))))]
    let extra_expressions = glr_parser :: extra_expressions
    let glr_opt_expr =
      Glr.apply_position
        (fun e  ->
           let (_loc_e,e) = e in
           fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             e)
        (locate
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.fsequence_position (Glr.char '[' '[')
                    (Glr.sequence (locate expression) (Glr.char ']' ']')
                       (fun e  ->
                          let (_loc_e,e) = e in
                          fun _unnamed_1  _unnamed_2  __loc__start__buf 
                            __loc__start__pos  __loc__end__buf 
                            __loc__end__pos  ->
                            let _loc =
                              locate2 __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            e))))))
    let glr_option =
      Glr.alternatives'
        [Glr.fsequence_position (Glr.char '*' '*')
           (Glr.sequence
              (locate
                 (Glr.option None
                    (Glr.apply (fun x  -> Some x) (Glr.char '*' '*'))))
              (locate glr_opt_expr)
              (fun strict  ->
                 let (_loc_strict,strict) = strict in
                 fun e  ->
                   let (_loc_e,e) = e in
                   fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                     __loc__end__buf  __loc__end__pos  ->
                     let _loc =
                       locate2 __loc__start__buf __loc__start__pos
                         __loc__end__buf __loc__end__pos in
                     `Fixpoint ((strict <> None), e)));
        Glr.fsequence_position (Glr.char '+' '+')
          (Glr.sequence
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x) (Glr.char '+' '+'))))
             (locate glr_opt_expr)
             (fun strict  ->
                let (_loc_strict,strict) = strict in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                    __loc__end__buf  __loc__end__pos  ->
                    let _loc =
                      locate2 __loc__start__buf __loc__start__pos
                        __loc__end__buf __loc__end__pos in
                    `Fixpoint1 ((strict <> None), e)));
        Glr.fsequence_position (Glr.char '?' '?')
          (Glr.sequence
             (locate
                (Glr.option None
                   (Glr.apply (fun x  -> Some x) (Glr.char '?' '?'))))
             (locate glr_opt_expr)
             (fun strict  ->
                let (_loc_strict,strict) = strict in
                fun e  ->
                  let (_loc_e,e) = e in
                  fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                    __loc__end__buf  __loc__end__pos  ->
                    let _loc =
                      locate2 __loc__start__buf __loc__start__pos
                        __loc__end__buf __loc__end__pos in
                    `Option ((strict <> None), e)));
        Glr.apply_position
          (fun _unnamed_0  __loc__start__buf  __loc__start__pos 
             __loc__end__buf  __loc__end__pos  ->
             let _loc =
               locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             `Once) (Glr.empty ())]
    let glr_sequence =
      Glr.alternatives'
        [Glr.fsequence_position (Glr.char '{' '{')
           (Glr.sequence (locate glr_rules) (Glr.char '}' '}')
              (fun r  ->
                 let (_loc_r,r) = r in
                 fun _unnamed_1  _unnamed_2  __loc__start__buf 
                   __loc__start__pos  __loc__end__buf  __loc__end__pos  ->
                   let _loc =
                     locate2 __loc__start__buf __loc__start__pos
                       __loc__end__buf __loc__end__pos in
                   r));
        Glr.sequence_position (Glr.string "EOF" "EOF") (locate glr_opt_expr)
          (fun _unnamed_0  opt  ->
             let (_loc_opt,opt) = opt in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "eof") [e]);
        Glr.sequence_position (Glr.string "EMPTY" "EMPTY")
          (locate glr_opt_expr)
          (fun _unnamed_0  opt  ->
             let (_loc_opt,opt) = opt in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               let e = match opt with | None  -> exp_unit _loc | Some e -> e in
               exp_apply _loc (exp_glr_fun _loc "empty") [e]);
        Glr.sequence_position (Glr.string "FAIL" "FAIL")
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  e  ->
             let (_loc_e,e) = e in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               exp_apply _loc (exp_glr_fun _loc "fail") [e]);
        Glr.sequence_position (Glr.string "DEBUG" "DEBUG")
          (locate (expression_lvl (next_exp App)))
          (fun _unnamed_0  e  ->
             let (_loc_e,e) = e in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               exp_apply _loc (exp_glr_fun _loc "debug") [e]);
        Glr.apply_position
          (fun _unnamed_0  __loc__start__buf  __loc__start__pos 
             __loc__end__buf  __loc__end__pos  ->
             let _loc =
               locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             exp_glr_fun _loc "any") (Glr.string "ANY" "ANY");
        Glr.fsequence_position (Glr.string "CHR" "CHR")
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                    __loc__end__buf  __loc__end__pos  ->
                    let _loc =
                      locate2 __loc__start__buf __loc__start__pos
                        __loc__end__buf __loc__end__pos in
                    let opt = match opt with | None  -> e | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "char") [e; opt]));
        Glr.fsequence_position (Glr.string "STR" "STR")
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                    __loc__end__buf  __loc__end__pos  ->
                    let _loc =
                      locate2 __loc__start__buf __loc__start__pos
                        __loc__end__buf __loc__end__pos in
                    let opt = match opt with | None  -> e | Some e -> e in
                    exp_apply _loc (exp_glr_fun _loc "string") [e; opt]));
        Glr.fsequence_position (Glr.string "RE" "RE")
          (Glr.sequence (locate (expression_lvl (next_exp App)))
             (locate glr_opt_expr)
             (fun e  ->
                let (_loc_e,e) = e in
                fun opt  ->
                  let (_loc_opt,opt) = opt in
                  fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                    __loc__end__buf  __loc__end__pos  ->
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
                    | Pexp_ident (Lident id) ->
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
        Glr.apply_position
          (fun e  ->
             let (_loc_e,e) = e in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               e) (locate (expression_lvl Atom))]
    let glr_ident =
      Glr.alternatives'
        [Glr.sequence_position (locate (pattern_lvl ConstrPat))
           (Glr.char ':' ':')
           (fun p  ->
              let (_loc_p,p) = p in
              fun _unnamed_1  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                match p.ppat_desc with
                | Ppat_alias (p,id) -> (id, (Some p))
                | Ppat_var id -> (id, None)
                | _ -> ("_", (Some p)));
        Glr.apply_position
          (fun _unnamed_0  __loc__start__buf  __loc__start__pos 
             __loc__end__buf  __loc__end__pos  ->
             let _loc =
               locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             ("_", None)) (Glr.empty ())]
    let dash =
      Glr.black_box
        (fun str  pos  ->
           let (c,str',pos') = Input.read str pos in
           if c = '-'
           then
             let (c',_,_) = Input.read str' pos' in
             (if c' = '>' then raise Glr.Give_up else ((), str', pos'))
           else raise Glr.Give_up) (Charset.singleton '-') false "-"
    let glr_left_member =
      Glr.sequence_position
        (locate
           (Glr.fsequence_position (locate glr_ident)
              (Glr.sequence (locate glr_sequence) (locate glr_option)
                 (fun s  ->
                    let (_loc_s,s) = s in
                    fun opt  ->
                      let (_loc_opt,opt) = opt in
                      fun id  ->
                        let (_loc_id,id) = id in
                        fun __loc__start__buf  __loc__start__pos 
                          __loc__end__buf  __loc__end__pos  ->
                          let _loc =
                            locate2 __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          `Normal (id, s, opt)))))
        (locate
           (Glr.apply List.rev
              (Glr.fixpoint []
                 (Glr.apply (fun x  l  -> x :: l)
                    (Glr.alternatives'
                       [Glr.fsequence_position (locate glr_ident)
                          (Glr.sequence (locate glr_sequence)
                             (locate glr_option)
                             (fun s  ->
                                let (_loc_s,s) = s in
                                fun opt  ->
                                  let (_loc_opt,opt) = opt in
                                  fun id  ->
                                    let (_loc_id,id) = id in
                                    fun __loc__start__buf  __loc__start__pos 
                                      __loc__end__buf  __loc__end__pos  ->
                                      let _loc =
                                        locate2 __loc__start__buf
                                          __loc__start__pos __loc__end__buf
                                          __loc__end__pos in
                                      `Normal (id, s, opt)));
                       Glr.apply_position
                         (fun _unnamed_0  __loc__start__buf 
                            __loc__start__pos  __loc__end__buf 
                            __loc__end__pos  ->
                            let _loc =
                              locate2 __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            `Ignore) dash])))))
        (fun i  ->
           let (_loc_i,i) = i in
           fun l  ->
             let (_loc_l,l) = l in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               i :: l)
    let glr_let = Glr.declare_grammar "glr_let"
    let _ =
      Glr.set_grammar glr_let
        (Glr.alternatives'
           [Glr.fsequence_position (Glr.string "let" "let")
              (Glr.fsequence (locate rec_flag)
                 (Glr.fsequence (locate let_binding)
                    (Glr.sequence (Glr.string "in" "in") (locate glr_let)
                       (fun _unnamed_0  l  ->
                          let (_loc_l,l) = l in
                          fun lbs  ->
                            let (_loc_lbs,lbs) = lbs in
                            fun r  ->
                              let (_loc_r,r) = r in
                              fun _unnamed_4  __loc__start__buf 
                                __loc__start__pos  __loc__end__buf 
                                __loc__end__pos  ->
                                let _loc =
                                  locate2 __loc__start__buf __loc__start__pos
                                    __loc__end__buf __loc__end__pos in
                                fun x  ->
                                  loc_expr _loc (Pexp_let (r, lbs, (l x)))))));
           Glr.apply_position
             (fun _unnamed_0  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                fun x  -> x) (Glr.empty ())])
    let glr_cond =
      Glr.alternatives'
        [Glr.sequence_position (Glr.string "when" "when") (locate expression)
           (fun _unnamed_0  e  ->
              let (_loc_e,e) = e in
              fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
                __loc__end__pos  ->
                let _loc =
                  locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                Some e);
        Glr.apply_position
          (fun _unnamed_0  __loc__start__buf  __loc__start__pos 
             __loc__end__buf  __loc__end__pos  ->
             let _loc =
               locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             None) (Glr.empty ())]
    let glr_action =
      Glr.alternatives'
        [Glr.sequence_position (Glr.string "->>" "->>") glr_rule
           (fun _unnamed_0  ((def,cond,r) as _unnamed_1)  __loc__start__buf 
              __loc__start__pos  __loc__end__buf  __loc__end__pos  ->
              let _loc =
                locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                  __loc__end__pos in
              DepSeq (def, cond, r));
        Glr.sequence_position (Glr.string "->" "->") (locate expression)
          (fun _unnamed_0  action  ->
             let (_loc_action,action) = action in
             fun __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               Normal action);
        Glr.apply_position
          (fun _unnamed_0  __loc__start__buf  __loc__start__pos 
             __loc__end__buf  __loc__end__pos  ->
             let _loc =
               locate2 __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             Default) (Glr.empty ())]
    let _ =
      Glr.set_grammar glr_rule
        (Glr.fsequence_position (locate glr_let)
           (Glr.fsequence (locate glr_left_member)
              (Glr.sequence (locate glr_cond) (locate glr_action)
                 (fun condition  ->
                    let (_loc_condition,condition) = condition in
                    fun action  ->
                      let (_loc_action,action) = action in
                      fun l  ->
                        let (_loc_l,l) = l in
                        fun def  ->
                          let (_loc_def,def) = def in
                          fun __loc__start__buf  __loc__start__pos 
                            __loc__end__buf  __loc__end__pos  ->
                            let _loc =
                              locate2 __loc__start__buf __loc__start__pos
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
                                  let e =
                                    apply_option _loc opt ((fst id) <> "_") e in
                                  let f =
                                    match ((!do_locate), first) with
                                    | (Some _,true ) -> "apply_position"
                                    | _ -> "apply" in
                                  exp_apply _loc (exp_glr_fun _loc f)
                                    [build_action _loc (id :: ids) action; e]
                              | (`Normal (id,e,opt))::(`Normal (id',e',opt'))::[]
                                  ->
                                  let e =
                                    apply_option _loc opt ((fst id) <> "_") e in
                                  let e' =
                                    apply_option _loc opt' ((fst id') <> "_")
                                      e' in
                                  let f =
                                    match ((!do_locate), first) with
                                    | (Some _,true ) -> "sequence_position"
                                    | _ -> "sequence" in
                                  exp_apply _loc (exp_glr_fun _loc f)
                                    [e;
                                    e';
                                    build_action _loc (id :: id' :: ids)
                                      action]
                              | (`Normal (id,e,opt))::ls ->
                                  let e =
                                    apply_option _loc opt ((fst id) <> "_") e in
                                  let f =
                                    match ((!do_locate), first) with
                                    | (Some _,true ) -> "fsequence_position"
                                    | _ -> "fsequence" in
                                  exp_apply _loc (exp_glr_fun _loc f)
                                    [e; fn false (id :: ids) ls] in
                            let res = fn true [] l in
                            let res =
                              if iter
                              then
                                exp_apply _loc (exp_glr_fun _loc "iter")
                                  [res]
                              else res in
                            (def, condition, res)))))
    let glr_rules_aux =
      Glr.fsequence_position
        (Glr.option None (Glr.apply (fun x  -> Some x) (Glr.char '|' '|')))
        (Glr.sequence (locate glr_rule)
           (locate
              (Glr.apply List.rev
                 (Glr.fixpoint []
                    (Glr.apply (fun x  l  -> x :: l)
                       (Glr.sequence_position (Glr.char '|' '|')
                          (locate glr_rule)
                          (fun _unnamed_0  r  ->
                             let (_loc_r,r) = r in
                             fun __loc__start__buf  __loc__start__pos 
                               __loc__end__buf  __loc__end__pos  ->
                               let _loc =
                                 locate2 __loc__start__buf __loc__start__pos
                                   __loc__end__buf __loc__end__pos in
                               r))))))
           (fun r  ->
              let (_loc_r,r) = r in
              fun rs  ->
                let (_loc_rs,rs) = rs in
                fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                  __loc__end__buf  __loc__end__pos  ->
                  let _loc =
                    locate2 __loc__start__buf __loc__start__pos
                      __loc__end__buf __loc__end__pos in
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
                        (exp_apply _loc (exp_glr_fun _loc "alternatives'")
                           [l]))))
    let _ =
      Glr.set_grammar glr_rules
        (Glr.fsequence_position
           (Glr.option None
              (Glr.apply (fun x  -> Some x)
                 (Glr.sequence_position (Glr.char '|' '|') (Glr.char '|' '|')
                    (fun _unnamed_0  _unnamed_1  __loc__start__buf 
                       __loc__start__pos  __loc__end__buf  __loc__end__pos 
                       ->
                       let _loc =
                         locate2 __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       ()))))
           (Glr.sequence (locate glr_rules_aux)
              (locate
                 (Glr.apply List.rev
                    (Glr.fixpoint []
                       (Glr.apply (fun x  l  -> x :: l)
                          (Glr.sequence_position
                             (Glr.sequence_position (Glr.char '|' '|')
                                (Glr.char '|' '|')
                                (fun _unnamed_0  _unnamed_1 
                                   __loc__start__buf  __loc__start__pos 
                                   __loc__end__buf  __loc__end__pos  ->
                                   let _loc =
                                     locate2 __loc__start__buf
                                       __loc__start__pos __loc__end__buf
                                       __loc__end__pos in
                                   ())) (locate glr_rules_aux)
                             (fun _unnamed_0  r  ->
                                let (_loc_r,r) = r in
                                fun __loc__start__buf  __loc__start__pos 
                                  __loc__end__buf  __loc__end__pos  ->
                                  let _loc =
                                    locate2 __loc__start__buf
                                      __loc__start__pos __loc__end__buf
                                      __loc__end__pos in
                                  r))))))
              (fun r  ->
                 let (_loc_r,r) = r in
                 fun rs  ->
                   let (_loc_rs,rs) = rs in
                   fun _unnamed_2  __loc__start__buf  __loc__start__pos 
                     __loc__end__buf  __loc__end__pos  ->
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
                                                      (Some
                                                         (exp_ident _loc "y")))))))))
                             (r :: l) (exp_Nil _loc) in
                         exp_apply _loc (exp_glr_fun _loc "alternatives") [l])))
  end
let _ = register_extension (module Ext : FExt )
