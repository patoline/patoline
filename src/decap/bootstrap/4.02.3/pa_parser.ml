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
  try let l = Sys.getenv "LOCATE" in Some (exp_ident Location.none l)
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
    (fun e  ->
       fun ((id,x),visible)  ->
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
     | `Option (greedy,d) ->
         let f = if greedy then "option'" else "option" in
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc f)
                [exp_None _loc;
                exp_apply _loc (exp_glr_fun _loc "apply")
                  [exp_Some_fun _loc; e]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc f) [d; e])
     | `Fixpoint (greedy,d) ->
         let f = if greedy then "fixpoint'" else "fixpoint" in
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "apply")
                [exp_list_fun _loc "rev";
                exp_apply _loc (exp_glr_fun _loc f)
                  [exp_Nil _loc;
                  exp_apply _loc (exp_glr_fun _loc "apply")
                    [exp_Cons_fun _loc; e]]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc f) [d; e])
     | `Fixpoint1 (greedy,d) ->
         let f = if greedy then "fixpoint1'" else "fixpoint1" in
         (match d with
          | None  ->
              exp_apply _loc (exp_glr_fun _loc "apply")
                [exp_list_fun _loc "rev";
                exp_apply _loc (exp_glr_fun _loc f)
                  [exp_Nil _loc;
                  exp_apply _loc (exp_glr_fun _loc "apply")
                    [exp_Cons_fun _loc; e]]]
          | Some d -> exp_apply _loc (exp_glr_fun _loc f) [d; e]))
let default_action _loc l =
  let l =
    List.filter
      (function
       | `Normal (("_",_),false ,_,_,_) -> false
       | `Ignore -> false
       | _ -> true) l in
  let l =
    List.map
      (function
       | `Normal ((id,_),_,_,_,_) -> exp_ident _loc id
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
    let parser_prefix =
      Decap.sequence
        (Decap.option None (Decap.apply (fun x  -> Some x) cached_kw))
        parser_kw (fun e  -> fun _default_0  -> e <> None)
    let glr_parser lvl =
      Decap.alternatives
        [Decap.sequence_position parser_prefix glr_rules
           (fun cached  ->
              fun p  ->
                fun __loc__start__buf  ->
                  fun __loc__start__pos  ->
                    fun __loc__end__buf  ->
                      fun __loc__end__pos  ->
                        let _loc =
                          locate __loc__start__buf __loc__start__pos
                            __loc__end__buf __loc__end__pos in
                        (Atom,
                          (if cached
                           then exp_apply _loc (exp_glr_fun _loc "cache") [p]
                           else p)));
        Decap.fsequence_position parser_prefix
          (Decap.sequence (Decap.char '*' '*') glr_rules
             (fun _  ->
                fun p  ->
                  fun cached  ->
                    fun __loc__start__buf  ->
                      fun __loc__start__pos  ->
                        fun __loc__end__buf  ->
                          fun __loc__end__pos  ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            (Atom,
                              (exp_apply _loc (exp_glr_fun _loc "lists")
                                 [if cached
                                  then
                                    exp_apply _loc (exp_glr_fun _loc "cache")
                                      [p]
                                  else p]))))]
    let glr_binding = Decap.declare_grammar "glr_binding"
    let _ =
      Decap.set_grammar glr_binding
        (Decap.fsequence lowercase_ident
           (Decap.fsequence
              (Decap.option None
                 (Decap.apply (fun x  -> Some x) (pattern_lvl AsPat)))
              (Decap.fsequence
                 (Decap.option None
                    (Decap.apply (fun x  -> Some x)
                       (Decap.sequence (Decap.char ':' ':') typexpr
                          (fun _  -> fun _default_0  -> _default_0))))
                 (Decap.fsequence (Decap.char '=' '=')
                    (Decap.sequence glr_rules
                       (Decap.option []
                          (Decap.sequence and_kw glr_binding
                             (fun _  -> fun _default_0  -> _default_0)))
                       (fun r  ->
                          fun l  ->
                            fun _  ->
                              fun ty  ->
                                fun arg  ->
                                  fun name  -> (name, arg, ty, r) :: l))))))
    let glr_struct_item =
      Decap.fsequence_position let_kw
        (Decap.sequence parser_prefix glr_binding
           (fun cached  ->
              fun l  ->
                fun _default_0  ->
                  fun __loc__start__buf  ->
                    fun __loc__start__pos  ->
                      fun __loc__end__buf  ->
                        fun __loc__end__pos  ->
                          let _loc =
                            locate __loc__start__buf __loc__start__pos
                              __loc__end__buf __loc__end__pos in
                          let rec fn =
                            function
                            | [] -> ([], [])
                            | (name,arg,ty,r)::l ->
                                let (str1,str2) = fn l in
                                let pname =
                                  match (ty, arg) with
                                  | (None ,_) ->
                                      ((Stack.push
                                          (Pa_ocaml_prelude.empty_quote_env2
                                             ()) Pa_ocaml_prelude.quote_stack;
                                        Pa_ocaml_prelude.push_string 44 name);
                                       (let quote_res =
                                          Pa_ocaml_prelude.quote_pattern_2
                                            _loc
                                            "#204 \"pa_parser.ml\"\n                        $lid:name$ " in
                                        ignore
                                          (Stack.pop
                                             Pa_ocaml_prelude.quote_stack);
                                        quote_res))
                                  | (Some ty,None ) ->
                                      (((Stack.push
                                           (Pa_ocaml_prelude.empty_quote_env2
                                              ())
                                           Pa_ocaml_prelude.quote_stack;
                                         Pa_ocaml_prelude.push_type 64 ty);
                                        Pa_ocaml_prelude.push_string 51 name);
                                       (let quote_res =
                                          Pa_ocaml_prelude.quote_pattern_2
                                            _loc
                                            "#205 \"pa_parser.ml\"\n                               $lid:name$ : $ty$ " in
                                        ignore
                                          (Stack.pop
                                             Pa_ocaml_prelude.quote_stack);
                                        quote_res))
                                  | (Some ty,Some _) ->
                                      (((Stack.push
                                           (Pa_ocaml_prelude.empty_quote_env2
                                              ())
                                           Pa_ocaml_prelude.quote_stack;
                                         Pa_ocaml_prelude.push_type 83 ty);
                                        Pa_ocaml_prelude.push_string 53 name);
                                       (let quote_res =
                                          Pa_ocaml_prelude.quote_pattern_2
                                            _loc
                                            "#206 \"pa_parser.ml\"\n                                 $lid:name$ : ('type_of_arg -> $ty$) " in
                                        ignore
                                          (Stack.pop
                                             Pa_ocaml_prelude.quote_stack);
                                        quote_res)) in
                                let r =
                                  if cached
                                  then
                                    exp_apply _loc (exp_glr_fun _loc "cache")
                                      [r]
                                  else r in
                                (match arg with
                                 | None  ->
                                     (((((Stack.push
                                            (Pa_ocaml_prelude.empty_quote_env2
                                               ())
                                            Pa_ocaml_prelude.quote_stack;
                                          Pa_ocaml_prelude.push_string 76
                                            name);
                                         Pa_ocaml_prelude.push_pattern 44
                                           pname);
                                        (let quote_res =
                                           Pa_ocaml_prelude.quote_structure_2
                                             _loc
                                             "#211 \"pa_parser.ml\"\n                    let $pname$ = Decap.declare_grammar $string:name$" in
                                         ignore
                                           (Stack.pop
                                              Pa_ocaml_prelude.quote_stack);
                                         quote_res)) @ str1),
                                       ((((Stack.push
                                             (Pa_ocaml_prelude.empty_quote_env2
                                                ())
                                             Pa_ocaml_prelude.quote_stack;
                                           Pa_ocaml_prelude.push_expression
                                             77 r);
                                          Pa_ocaml_prelude.push_string 66
                                            name);
                                         (let quote_res =
                                            Pa_ocaml_prelude.quote_structure_2
                                              _loc
                                              "#212 \"pa_parser.ml\"\n                    let _ = Decap.set_grammar $lid:name$ $r$ " in
                                          ignore
                                            (Stack.pop
                                               Pa_ocaml_prelude.quote_stack);
                                          quote_res)) @ str2))
                                 | Some arg ->
                                     let set_name = name ^ "__set__grammar" in
                                     ((((((Stack.push
                                             (Pa_ocaml_prelude.empty_quote_env2
                                                ())
                                             Pa_ocaml_prelude.quote_stack;
                                           Pa_ocaml_prelude.push_string 91
                                             name);
                                          Pa_ocaml_prelude.push_string 53
                                            set_name);
                                         Pa_ocaml_prelude.push_pattern 44
                                           pname);
                                        (let quote_res =
                                           Pa_ocaml_prelude.quote_structure_2
                                             _loc
                                             "#215 \"pa_parser.ml\"\n                    let $pname$, $lid:set_name$ = Decap.grammar_family $string:name$" in
                                         ignore
                                           (Stack.pop
                                              Pa_ocaml_prelude.quote_stack);
                                         quote_res)) @ str1),
                                       ((((((Stack.push
                                               (Pa_ocaml_prelude.empty_quote_env2
                                                  ())
                                               Pa_ocaml_prelude.quote_stack;
                                             Pa_ocaml_prelude.push_expression
                                               77 r);
                                            Pa_ocaml_prelude.push_pattern 68
                                              arg);
                                           Pa_ocaml_prelude.push_string 48
                                             set_name);
                                          Pa_ocaml_prelude.push_string 48
                                            set_name);
                                         (let quote_res =
                                            Pa_ocaml_prelude.quote_structure_2
                                              _loc
                                              "#216 \"pa_parser.ml\"\n                    let _ = $lid:set_name$ (fun $arg$ -> $r$) " in
                                          ignore
                                            (Stack.pop
                                               Pa_ocaml_prelude.quote_stack);
                                          quote_res)) @ str2))) in
                          let (str1,str2) = fn l in str1 @ str2))
    let extra_expressions = glr_parser :: extra_expressions
    let extra_structure = glr_struct_item :: extra_structure
    let _ = add_reserved_id "parser"
    let glr_opt_expr =
      Decap.option None
        (Decap.apply (fun x  -> Some x)
           (Decap.fsequence (Decap.char '[' '[')
              (Decap.sequence expression (Decap.char ']' ']')
                 (fun e  -> fun _  -> fun _  -> e))))
    let is_greedy c =
      let greedy =
        try ignore (Sys.getenv "GREEDY"); true with | Not_found  -> false in
      Decap.alternatives
        [Decap.apply (fun _  -> false) (Decap.char '~' '~');
        Decap.apply (fun _  -> true) (Decap.char c c);
        Decap.apply (fun _  -> greedy) (Decap.empty ())]
    let fs = is_greedy '*'
    let fp = is_greedy '+'
    let fq = is_greedy '?'
    let glr_option =
      Decap.alternatives
        [Decap.fsequence (Decap.char '*' '*')
           (Decap.sequence fs glr_opt_expr
              (fun s  -> fun e  -> fun _  -> `Fixpoint (s, e)));
        Decap.fsequence (Decap.char '+' '+')
          (Decap.sequence fp glr_opt_expr
             (fun s  -> fun e  -> fun _  -> `Fixpoint1 (s, e)));
        Decap.fsequence (Decap.char '?' '?')
          (Decap.sequence fq glr_opt_expr
             (fun s  -> fun e  -> fun _  -> `Option (s, e)));
        Decap.apply (fun _  -> `Once) (Decap.empty ())]
    let glr_sequence =
      Decap.alternatives
        [Decap.fsequence (Decap.char '{' '{')
           (Decap.sequence glr_rules (Decap.char '}' '}')
              (fun r  -> fun _  -> fun _  -> (true, r)));
        Decap.sequence_position (Decap.string "EOF" "EOF") glr_opt_expr
          (fun _  ->
             fun opt  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       let e =
                         match opt with
                         | None  -> exp_unit _loc
                         | Some e -> e in
                       ((opt <> None),
                         (exp_apply _loc (exp_glr_fun _loc "eof") [e])));
        Decap.sequence_position (Decap.string "EMPTY" "EMPTY") glr_opt_expr
          (fun _  ->
             fun opt  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       let e =
                         match opt with
                         | None  -> exp_unit _loc
                         | Some e -> e in
                       ((opt <> None),
                         (exp_apply _loc (exp_glr_fun _loc "empty") [e])));
        Decap.sequence_position (Decap.string "FAIL" "FAIL")
          (expression_lvl (next_exp App))
          (fun _  ->
             fun e  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       (false,
                         (exp_apply _loc (exp_glr_fun _loc "fail") [e])));
        Decap.sequence_position (Decap.string "DEBUG" "DEBUG")
          (expression_lvl (next_exp App))
          (fun _  ->
             fun e  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       (false,
                         (exp_apply _loc (exp_glr_fun _loc "debug") [e])));
        Decap.apply_position
          (fun _  ->
             fun __loc__start__buf  ->
               fun __loc__start__pos  ->
                 fun __loc__end__buf  ->
                   fun __loc__end__pos  ->
                     let _loc =
                       locate __loc__start__buf __loc__start__pos
                         __loc__end__buf __loc__end__pos in
                     (true, (exp_glr_fun _loc "any")))
          (Decap.string "ANY" "ANY");
        Decap.fsequence_position (Decap.string "CHR" "CHR")
          (Decap.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  ->
                fun opt  ->
                  fun _  ->
                    fun __loc__start__buf  ->
                      fun __loc__start__pos  ->
                        fun __loc__end__buf  ->
                          fun __loc__end__pos  ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            let o = match opt with | None  -> e | Some e -> e in
                            ((opt <> None),
                              (exp_apply _loc (exp_glr_fun _loc "char")
                                 [e; o]))));
        Decap.sequence_position
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             char_literal) glr_opt_expr
          (fun c  ->
             let (_loc_c,c) = c in
             fun opt  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       let e = loc_expr _loc_c (Pexp_constant (Const_char c)) in
                       let o = match opt with | None  -> e | Some e -> e in
                       ((opt <> None),
                         (exp_apply _loc (exp_glr_fun _loc "char") [e; o])));
        Decap.fsequence_position (Decap.string "STR" "STR")
          (Decap.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  ->
                fun opt  ->
                  fun _  ->
                    fun __loc__start__buf  ->
                      fun __loc__start__pos  ->
                        fun __loc__end__buf  ->
                          fun __loc__end__pos  ->
                            let _loc =
                              locate __loc__start__buf __loc__start__pos
                                __loc__end__buf __loc__end__pos in
                            let o = match opt with | None  -> e | Some e -> e in
                            ((opt <> None),
                              (exp_apply _loc (exp_glr_fun _loc "string")
                                 [e; o]))));
        Decap.sequence_position
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             string_literal) glr_opt_expr
          (fun s  ->
             let (_loc_s,s) = s in
             fun opt  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
                           __loc__end__buf __loc__end__pos in
                       ((opt <> None),
                         (if (String.length s) = 0
                          then Decap.give_up "Empty string litteral in rule.";
                          (let e =
                             loc_expr _loc_s (Pexp_constant (const_string s)) in
                           let opt =
                             match opt with | None  -> e | Some e -> e in
                           exp_apply _loc (exp_glr_fun _loc "string")
                             [e; opt]))));
        Decap.sequence_position
          (Decap.alternatives
             [Decap.sequence (Decap.string "RE" "RE")
                (expression_lvl (next_exp App)) (fun _  -> fun e  -> e);
             Decap.apply
               (fun s  ->
                  let (_loc_s,s) = s in
                  loc_expr _loc_s (Pexp_constant (const_string s)))
               (Decap.apply_position
                  (fun x  ->
                     fun str  ->
                       fun pos  ->
                         fun str'  ->
                           fun pos'  -> ((locate str pos str' pos'), x))
                  regexp_literal)]) glr_opt_expr
          (fun e  ->
             fun opt  ->
               fun __loc__start__buf  ->
                 fun __loc__start__pos  ->
                   fun __loc__end__buf  ->
                     fun __loc__end__pos  ->
                       let _loc =
                         locate __loc__start__buf __loc__start__pos
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
                               (l > 3) && ((String.sub id (l - 3) 3) = "_re")
                             then String.sub id 0 (l - 3)
                             else id in
                           (true,
                             (exp_lab_apply _loc (exp_glr_fun _loc "regexp")
                                [("name", (exp_string _loc id));
                                ("", e);
                                ("", (exp_fun _loc "groupe" opt))]))
                       | _ ->
                           (true,
                             (exp_apply _loc (exp_glr_fun _loc "regexp")
                                [e; exp_fun _loc "groupe" opt])));
        Decap.apply (fun e  -> (true, e)) (expression_lvl Atom)]
    let glr_ident =
      Decap.alternatives
        [Decap.sequence (pattern_lvl ConstrPat) (Decap.char ':' ':')
           (fun p  ->
              fun _  ->
                match p.ppat_desc with
                | Ppat_alias (p,{ txt = id }) ->
                    ((Some true), (id, (Some p)))
                | Ppat_var { txt = id } -> ((Some (id <> "_")), (id, None))
                | Ppat_any  -> ((Some false), ("_", None))
                | _ -> ((Some true), ("_", (Some p))));
        Decap.apply (fun _  -> (None, ("_", None))) (Decap.empty ())]
    let dash =
      Decap.black_box
        (fun str  ->
           fun pos  ->
             let (c,str',pos') = Input.read str pos in
             if c = '-'
             then
               let (c',_,_) = Input.read str' pos' in
               (if c' = '>'
                then Decap.give_up "'-' expected"
                else ((), str', pos'))
             else Decap.give_up "'-' expexted") (Charset.singleton '-') None
        "-"
    let glr_left_member =
      let f x y = match x with | Some x -> x | None  -> y in
      Decap.sequence
        (Decap.fsequence glr_ident
           (Decap.sequence glr_sequence glr_option
              (fun ((cst,s) as _default_0)  ->
                 fun opt  ->
                   fun ((cst',id) as _default_1)  ->
                     `Normal (id, (f cst' ((opt <> `Once) || cst)), s, opt))))
        (Decap.apply List.rev
           (Decap.fixpoint []
              (Decap.apply (fun x  -> fun l  -> x :: l)
                 (Decap.alternatives
                    [Decap.fsequence glr_ident
                       (Decap.sequence glr_sequence glr_option
                          (fun ((cst,s) as _default_0)  ->
                             fun opt  ->
                               fun ((cst',id) as _default_1)  ->
                                 `Normal
                                   (id, (f cst' ((opt <> `Once) || cst)), s,
                                     opt)));
                    Decap.apply (fun _default_0  -> `Ignore) dash]))))
        (fun i  -> fun l  -> i :: l)
    let glr_let = Decap.declare_grammar "glr_let"
    let _ =
      Decap.set_grammar glr_let
        (Decap.alternatives
           [Decap.fsequence_position (Decap.string "let" "let")
              (Decap.fsequence rec_flag
                 (Decap.fsequence let_binding
                    (Decap.sequence (Decap.string "in" "in") glr_let
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
                                            locate __loc__start__buf
                                              __loc__start__pos
                                              __loc__end__buf __loc__end__pos in
                                          fun x  ->
                                            loc_expr _loc
                                              (Pexp_let (r, lbs, (l x)))))));
           Decap.apply (fun _  -> fun x  -> x) (Decap.empty ())])
    let glr_cond =
      Decap.alternatives
        [Decap.sequence (Decap.string "when" "when") expression
           (fun _  -> fun e  -> Some e);
        Decap.apply (fun _  -> None) (Decap.empty ())]
    let build_rule (_loc,occur_loc,def,l,condition,action) =
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
                                 (exp_apply _loc (exp_glr_fun _loc "fail")
                                    [exp_string _loc ""])))))))) in
      let rec fn first ids l =
        match l with
        | [] -> assert false
        | `Ignore::ls -> assert false
        | (`Normal (id,cst,e,opt,oc))::`Ignore::ls ->
            let e = exp_apply _loc (exp_glr_fun _loc "ignore_next_blank") [e] in
            fn first ids ((`Normal (id, cst, e, opt, oc)) :: ls)
        | (`Normal (id,_,e,opt,occur_loc_id))::[] ->
            let e = apply_option _loc opt occur_loc_id e in
            let f =
              match ((find_locate ()), (first && occur_loc)) with
              | (Some _,true ) -> "apply_position"
              | _ -> "apply" in
            (match action.pexp_desc with
             | Pexp_ident { txt = Lident id' } when
                 ((fst id) = id') && (f = "apply") -> e
             | _ ->
                 exp_apply _loc (exp_glr_fun _loc f)
                   [build_action _loc occur_loc ((id, occur_loc_id) :: ids)
                      action;
                   e])
        | (`Normal (id,_,e,opt,occur_loc_id))::(`Normal
                                                  (id',_,e',opt',occur_loc_id'))::[]
            ->
            let e = apply_option _loc opt occur_loc_id e in
            let e' = apply_option _loc opt' occur_loc_id' e' in
            let f =
              match ((find_locate ()), (first && occur_loc)) with
              | (Some _,true ) -> "sequence_position"
              | _ -> "sequence" in
            exp_apply _loc (exp_glr_fun _loc f)
              [e;
              e';
              build_action _loc occur_loc ((id, occur_loc_id) ::
                (id', occur_loc_id') :: ids) action]
        | (`Normal (id,_,e,opt,occur_loc_id))::ls ->
            let e = apply_option _loc opt occur_loc_id e in
            let f =
              match ((find_locate ()), (first && occur_loc)) with
              | (Some _,true ) -> "fsequence_position"
              | _ -> "fsequence" in
            exp_apply _loc (exp_glr_fun _loc f)
              [e; fn false ((id, occur_loc_id) :: ids) ls] in
      let res = fn true [] l in
      let res =
        if iter then exp_apply _loc (exp_glr_fun _loc "iter") [res] else res in
      (def, condition, res)
    let glr_action =
      Decap.alternatives
        [Decap.sequence (Decap.string "->>" "->>") glr_rule
           (fun _  ->
              fun r  -> let (a,b,c) = build_rule r in DepSeq (a, b, c));
        Decap.sequence arrow_re expression
          (fun _default_0  -> fun action  -> Normal action);
        Decap.apply (fun _  -> Default) (Decap.empty ())]
    let _ =
      Decap.set_grammar glr_rule
        (Decap.iter
           (Decap.fsequence glr_let
              (Decap.sequence glr_left_member glr_cond
                 (fun l  ->
                    fun condition  ->
                      fun def  ->
                        let _ = push_frame () in
                        Decap.apply_position
                          (fun action  ->
                             fun __loc__start__buf  ->
                               fun __loc__start__pos  ->
                                 fun __loc__end__buf  ->
                                   fun __loc__end__pos  ->
                                     let _loc =
                                       locate __loc__start__buf
                                         __loc__start__pos __loc__end__buf
                                         __loc__end__pos in
                                     let l =
                                       fst
                                         (List.fold_right
                                            (fun x  ->
                                               fun (res,i)  ->
                                                 match x with
                                                 | `Normal
                                                     (("_",a),true ,c,d) ->
                                                     (((`Normal
                                                          ((("_default_" ^
                                                               (string_of_int
                                                                  i)), a),
                                                            true, c, d,
                                                            false)) :: res),
                                                       (i + 1))
                                                 | `Normal (id,b,c,d) ->
                                                     let occur_loc_id =
                                                       ((fst id) <> "_") &&
                                                         (pop_location
                                                            (fst id)) in
                                                     (((`Normal
                                                          (id, b, c, d,
                                                            occur_loc_id)) ::
                                                       res), i)
                                                 | `Ignore ->
                                                     ((`Ignore :: res), i)) l
                                            ([], 0)) in
                                     let occur_loc = pop_location "" in
                                     pop_frame ();
                                     (_loc, occur_loc, def, l, condition,
                                       action)) glr_action))))
    let apply_def_cond _loc arg =
      let (def,cond,e) = build_rule arg in
      match cond with
      | None  -> def e
      | Some c ->
          def
            (loc_expr _loc
               (Pexp_ifthenelse
                  (c, e,
                    (Some
                       (exp_apply _loc (exp_glr_fun _loc "fail")
                          [exp_string _loc ""])))))
    let eq_expression_opt e1 e2 =
      match (e1, e2) with
      | (Some e1,Some e2) -> Compare.eq_expression e1 e2
      | (None ,None ) -> true
      | _ -> false
    let eq_pattern_opt e1 e2 =
      match (e1, e2) with
      | (Some e1,Some e2) -> Compare.eq_pattern e1 e2
      | (None ,None ) -> true
      | _ -> false
    let eq_option opt opt' =
      match (opt, opt') with
      | (`Once,`Once) -> true
      | (`Option (g,d),`Option (g',d')) ->
          (g = g') && (eq_expression_opt d d')
      | (`Fixpoint (g,d),`Fixpoint (g',d')) ->
          (g = g') && (eq_expression_opt d d')
      | (`Fixpoint1 (g,d),`Fixpoint1 (g',d')) ->
          (g = g') && (eq_expression_opt d d')
      | _ -> false
    let factorisable elt1 elt2 =
      match (elt1, elt2) with
      | ((_loc,occur_loc,def,(`Normal (id,cst,e,opt,occur_loc_id))::_,condition,action),
         (_loc',occur_loc',def',(`Normal (id',cst',e',opt',occur_loc_id'))::_,condition',action'))
          ->
          ((fst id) = (fst id')) &&
            ((cst = cst') &&
               ((Compare.eq_expression e e') &&
                  ((eq_pattern_opt (snd id) (snd id')) &&
                     ((eq_option opt opt') &&
                        (Compare.eq_expression (def (exp_ident _loc "@"))
                           (def' (exp_ident _loc "@")))))))
      | _ -> false
    let build_alternatives _loc comb ls =
      match ls with
      | [] -> exp_apply _loc (exp_glr_fun _loc "fail") [exp_string _loc ""]
      | r::[] -> apply_def_cond _loc r
      | _::_ ->
          ((match ls with
            | elt1::elt2::_ ->
                if factorisable elt1 elt2
                then Printf.eprintf "can left factorise\n%!"
            | _ -> ());
           (let l =
              List.fold_right
                (fun r  ->
                   fun y  ->
                     let (def,cond,e) = build_rule r in
                     match cond with
                     | None  -> def (exp_Cons _loc e y)
                     | Some c ->
                         def
                           (loc_expr _loc
                              (Pexp_let
                                 (Nonrecursive,
                                   [value_binding _loc (pat_ident _loc "y") y],
                                   (loc_expr _loc
                                      (Pexp_ifthenelse
                                         (c,
                                           (exp_Cons _loc e
                                              (exp_ident _loc "y")),
                                           (Some (exp_ident _loc "y")))))))))
                ls (exp_Nil _loc) in
            exp_apply _loc (exp_glr_fun _loc comb) [l]))
    let alt =
      Decap.sequence (Decap.char '|' '|')
        (Decap.option None
           (Decap.apply (fun x  -> Some x) (Decap.char '|' '|')))
        (fun _  -> fun x  -> x = None)
    let _ =
      Decap.set_grammar glr_rules
        (Decap.fsequence_position
           (Decap.option None (Decap.apply (fun x  -> Some x) alt))
           (Decap.sequence glr_rule
              (Decap.apply List.rev
                 (Decap.fixpoint' []
                    (Decap.apply (fun x  -> fun l  -> x :: l)
                       (Decap.sequence alt glr_rule
                          (fun a  -> fun r  -> (a, r))))))
              (fun r  ->
                 fun rs  ->
                   fun a  ->
                     fun __loc__start__buf  ->
                       fun __loc__start__pos  ->
                         fun __loc__end__buf  ->
                           fun __loc__end__pos  ->
                             let _loc =
                               locate __loc__start__buf __loc__start__pos
                                 __loc__end__buf __loc__end__pos in
                             let rec fn a ls =
                               match (a, ls) with
                               | (None ,(a,_)::ls) -> fn (Some a) ls
                               | (Some a',(a,_)::ls) when a <> a' ->
                                   Decap.give_up "Inhomogenous alternatives"
                               | (_,_::ls) -> fn a ls
                               | (_,[]) -> a in
                             let a = fn a rs in
                             let ls = r :: (List.map snd rs) in
                             let f =
                               if a = (Some false)
                               then "alternatives'"
                               else "alternatives" in
                             build_alternatives _loc f ls)))
  end
