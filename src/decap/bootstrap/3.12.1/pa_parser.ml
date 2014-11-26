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
       | `Normal (("_",_),false ,_,_) -> false
       | `Ignore -> false
       | _ -> true) l in
  let l =
    List.map
      (function
       | `Normal ((id,_),_,_,_) -> exp_ident _loc id
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
    let glr_parser lvl =
      Decap.alternatives
        [Decap.sequence parser_kw glr_rules (fun _default_0  p  -> (Atom, p));
        Decap.fsequence_position parser_kw
          (Decap.sequence (Decap.char '*' '*') glr_rules
             (fun _  p  _default_0  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                (Atom, (exp_apply _loc (exp_glr_fun _loc "lists") [p]))))]
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
                          (fun _  _default_0  -> _default_0))))
                 (Decap.fsequence (Decap.char '=' '=')
                    (Decap.sequence glr_rules
                       (Decap.option []
                          (Decap.sequence and_kw glr_binding
                             (fun _  _default_0  -> _default_0)))
                       (fun r  l  _  ty  arg  name  -> (name, arg, ty, r) ::
                          l))))))
    let glr_struct_item =
      Decap.fsequence_position let_kw
        (Decap.sequence parser_kw glr_binding
           (fun _default_0  l  _default_1  __loc__start__buf 
              __loc__start__pos  __loc__end__buf  __loc__end__pos  ->
              let _loc =
                locate __loc__start__buf __loc__start__pos __loc__end__buf
                  __loc__end__pos in
              let rec fn =
                function
                | [] -> ([], [])
                | (name,arg,ty,r)::l ->
                    let (str1,str2) = fn l in
                    let pname =
                      match (ty, arg) with
                      | (None ,_) ->
                          ((Stack.push (Pa_ocaml_prelude.empty_quote_env2 ())
                              Pa_ocaml_prelude.quote_stack;
                            Pa_ocaml_prelude.push_string 44 name);
                           (let quote_res =
                              Pa_ocaml_prelude.quote_pattern_2 _loc
                                "#201 \"pa_parser.ml\"\n                        $lid:name$ " in
                            ignore (Stack.pop Pa_ocaml_prelude.quote_stack);
                            quote_res))
                      | (Some ty,None ) ->
                          (((Stack.push
                               (Pa_ocaml_prelude.empty_quote_env2 ())
                               Pa_ocaml_prelude.quote_stack;
                             Pa_ocaml_prelude.push_type 64 ty);
                            Pa_ocaml_prelude.push_string 51 name);
                           (let quote_res =
                              Pa_ocaml_prelude.quote_pattern_2 _loc
                                "#202 \"pa_parser.ml\"\n                               $lid:name$ : $ty$ " in
                            ignore (Stack.pop Pa_ocaml_prelude.quote_stack);
                            quote_res))
                      | (Some ty,Some _) ->
                          (((Stack.push
                               (Pa_ocaml_prelude.empty_quote_env2 ())
                               Pa_ocaml_prelude.quote_stack;
                             Pa_ocaml_prelude.push_type 83 ty);
                            Pa_ocaml_prelude.push_string 53 name);
                           (let quote_res =
                              Pa_ocaml_prelude.quote_pattern_2 _loc
                                "#203 \"pa_parser.ml\"\n                                 $lid:name$ : ('type_of_arg -> $ty$) " in
                            ignore (Stack.pop Pa_ocaml_prelude.quote_stack);
                            quote_res)) in
                    (match arg with
                     | None  ->
                         (((((Stack.push
                                (Pa_ocaml_prelude.empty_quote_env2 ())
                                Pa_ocaml_prelude.quote_stack;
                              Pa_ocaml_prelude.push_string 76 name);
                             Pa_ocaml_prelude.push_pattern 44 pname);
                            (let quote_res =
                               Pa_ocaml_prelude.quote_structure_2 _loc
                                 "#207 \"pa_parser.ml\"\n                    let $pname$ = Decap.declare_grammar $string:name$" in
                             ignore (Stack.pop Pa_ocaml_prelude.quote_stack);
                             quote_res)) @ str1),
                           ((((Stack.push
                                 (Pa_ocaml_prelude.empty_quote_env2 ())
                                 Pa_ocaml_prelude.quote_stack;
                               Pa_ocaml_prelude.push_expression 77 r);
                              Pa_ocaml_prelude.push_string 66 name);
                             (let quote_res =
                                Pa_ocaml_prelude.quote_structure_2 _loc
                                  "#208 \"pa_parser.ml\"\n                    let _ = Decap.set_grammar $lid:name$ $r$ " in
                              ignore (Stack.pop Pa_ocaml_prelude.quote_stack);
                              quote_res)) @ str2))
                     | Some arg ->
                         let set_name = name ^ "__set__grammar" in
                         ((((((Stack.push
                                 (Pa_ocaml_prelude.empty_quote_env2 ())
                                 Pa_ocaml_prelude.quote_stack;
                               Pa_ocaml_prelude.push_string 91 name);
                              Pa_ocaml_prelude.push_string 53 set_name);
                             Pa_ocaml_prelude.push_pattern 44 pname);
                            (let quote_res =
                               Pa_ocaml_prelude.quote_structure_2 _loc
                                 "#211 \"pa_parser.ml\"\n                    let $pname$, $lid:set_name$ = Decap.grammar_family $string:name$" in
                             ignore (Stack.pop Pa_ocaml_prelude.quote_stack);
                             quote_res)) @ str1),
                           ((((((Stack.push
                                   (Pa_ocaml_prelude.empty_quote_env2 ())
                                   Pa_ocaml_prelude.quote_stack;
                                 Pa_ocaml_prelude.push_expression 77 r);
                                Pa_ocaml_prelude.push_pattern 68 arg);
                               Pa_ocaml_prelude.push_string 48 set_name);
                              Pa_ocaml_prelude.push_string 48 set_name);
                             (let quote_res =
                                Pa_ocaml_prelude.quote_structure_2 _loc
                                  "#212 \"pa_parser.ml\"\n                    let _ = $lid:set_name$ (fun $arg$ -> $r$) " in
                              ignore (Stack.pop Pa_ocaml_prelude.quote_stack);
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
                 (fun e  _  _  -> e))))
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
           (Decap.sequence fs glr_opt_expr (fun s  e  _  -> `Fixpoint (s, e)));
        Decap.fsequence (Decap.char '+' '+')
          (Decap.sequence fp glr_opt_expr (fun s  e  _  -> `Fixpoint1 (s, e)));
        Decap.fsequence (Decap.char '?' '?')
          (Decap.sequence fq glr_opt_expr (fun s  e  _  -> `Option (s, e)));
        Decap.apply (fun _  -> `Once) (Decap.empty ())]
    let glr_sequence =
      Decap.alternatives
        [Decap.fsequence (Decap.char '{' '{')
           (Decap.sequence glr_rules (Decap.char '}' '}')
              (fun r  _  _  -> (true, r)));
        Decap.sequence_position (Decap.string "EOF" "EOF") glr_opt_expr
          (fun _  opt  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             let e = match opt with | None  -> exp_unit _loc | Some e -> e in
             ((opt <> None), (exp_apply _loc (exp_glr_fun _loc "eof") [e])));
        Decap.sequence_position (Decap.string "EMPTY" "EMPTY") glr_opt_expr
          (fun _  opt  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             let e = match opt with | None  -> exp_unit _loc | Some e -> e in
             ((opt <> None), (exp_apply _loc (exp_glr_fun _loc "empty") [e])));
        Decap.sequence_position (Decap.string "FAIL" "FAIL")
          (expression_lvl (next_exp App))
          (fun _  e  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             (false, (exp_apply _loc (exp_glr_fun _loc "fail") [e])));
        Decap.sequence_position (Decap.string "DEBUG" "DEBUG")
          (expression_lvl (next_exp App))
          (fun _  e  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             (false, (exp_apply _loc (exp_glr_fun _loc "debug") [e])));
        Decap.apply_position
          (fun _  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             (true, (exp_glr_fun _loc "any"))) (Decap.string "ANY" "ANY");
        Decap.fsequence_position (Decap.string "CHR" "CHR")
          (Decap.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  opt  _  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                let o = match opt with | None  -> e | Some e -> e in
                ((opt <> None),
                  (exp_apply _loc (exp_glr_fun _loc "char") [e; o]))));
        Decap.sequence_position
          (Decap.apply_position
             (fun x  str  pos  str'  pos'  -> ((locate str pos str' pos'), x))
             char_literal) glr_opt_expr
          (fun c  ->
             let (_loc_c,c) = c in
             fun opt  __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               let e = loc_expr _loc_c (Pexp_constant (Const_char c)) in
               let o = match opt with | None  -> e | Some e -> e in
               ((opt <> None),
                 (exp_apply _loc (exp_glr_fun _loc "char") [e; o])));
        Decap.fsequence_position (Decap.string "STR" "STR")
          (Decap.sequence (expression_lvl (next_exp App)) glr_opt_expr
             (fun e  opt  _  __loc__start__buf  __loc__start__pos 
                __loc__end__buf  __loc__end__pos  ->
                let _loc =
                  locate __loc__start__buf __loc__start__pos __loc__end__buf
                    __loc__end__pos in
                let o = match opt with | None  -> e | Some e -> e in
                ((opt <> None),
                  (exp_apply _loc (exp_glr_fun _loc "string") [e; o]))));
        Decap.sequence_position
          (Decap.apply_position
             (fun x  str  pos  str'  pos'  -> ((locate str pos str' pos'), x))
             string_literal) glr_opt_expr
          (fun s  ->
             let (_loc_s,s) = s in
             fun opt  __loc__start__buf  __loc__start__pos  __loc__end__buf 
               __loc__end__pos  ->
               let _loc =
                 locate __loc__start__buf __loc__start__pos __loc__end__buf
                   __loc__end__pos in
               ((opt <> None),
                 (match String.length s with
                  | 0 ->
                      raise (Decap.Give_up "Empty string litteral in rule.")
                  | 1 ->
                      let e =
                        loc_expr _loc_s (Pexp_constant (Const_char (s.[0]))) in
                      let opt = match opt with | None  -> e | Some e -> e in
                      exp_apply _loc (exp_glr_fun _loc "char") [e; opt]
                  | _ ->
                      let e =
                        loc_expr _loc_s (Pexp_constant (const_string s)) in
                      let opt = match opt with | None  -> e | Some e -> e in
                      exp_apply _loc (exp_glr_fun _loc "string") [e; opt])));
        Decap.sequence_position
          (Decap.alternatives
             [Decap.sequence (Decap.string "RE" "RE")
                (expression_lvl (next_exp App)) (fun _  e  -> e);
             Decap.apply
               (fun s  ->
                  let (_loc_s,s) = s in
                  loc_expr _loc_s (Pexp_constant (const_string s)))
               (Decap.apply_position
                  (fun x  str  pos  str'  pos'  ->
                     ((locate str pos str' pos'), x)) regexp_literal)])
          glr_opt_expr
          (fun e  opt  __loc__start__buf  __loc__start__pos  __loc__end__buf 
             __loc__end__pos  ->
             let _loc =
               locate __loc__start__buf __loc__start__pos __loc__end__buf
                 __loc__end__pos in
             let opt =
               match opt with
               | None  ->
                   exp_apply _loc (exp_ident _loc "groupe") [exp_int _loc 0]
               | Some e -> e in
             match e.pexp_desc with
             | Pexp_ident (Lident id) ->
                 let id =
                   let l = String.length id in
                   if (l > 3) && ((String.sub id (l - 3) 3) = "_re")
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
           (fun p  _  ->
              match p.ppat_desc with
              | Ppat_alias (p,id) -> ((Some true), (id, (Some p)))
              | Ppat_var id -> ((Some (id <> "_")), (id, None))
              | Ppat_any  -> ((Some false), ("_", None))
              | _ -> ((Some true), ("_", (Some p))));
        Decap.apply (fun _  -> (None, ("_", None))) (Decap.empty ())]
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
      let f x y = match x with | Some x -> x | None  -> y in
      Decap.sequence
        (Decap.fsequence glr_ident
           (Decap.sequence glr_sequence glr_option
              (fun ((cst,s) as _default_0)  opt  ((cst',id) as _default_1) 
                 -> `Normal (id, (f cst' ((opt <> `Once) || cst)), s, opt))))
        (Decap.apply List.rev
           (Decap.fixpoint []
              (Decap.apply (fun x  l  -> x :: l)
                 (Decap.alternatives
                    [Decap.fsequence glr_ident
                       (Decap.sequence glr_sequence glr_option
                          (fun ((cst,s) as _default_0)  opt 
                             ((cst',id) as _default_1)  ->
                             `Normal
                               (id, (f cst' ((opt <> `Once) || cst)), s, opt)));
                    Decap.apply (fun _default_0  -> `Ignore) dash]))))
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
           (fun _  ((def,cond,r) as _default_0)  -> DepSeq (def, cond, r));
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
                         let l =
                           fst
                             (List.fold_right
                                (fun x  (res,i)  ->
                                   match x with
                                   | `Normal (("_",a),true ,c,d) ->
                                       (((`Normal
                                            ((("_default_" ^
                                                 (string_of_int i)), a),
                                              true, c, d)) :: res), (
                                         i + 1))
                                   | _ -> ((x :: res), i)) l ([], 0)) in
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
                           | (`Normal (id,cst,e,opt))::`Ignore::ls ->
                               let e =
                                 exp_apply _loc
                                   (exp_glr_fun _loc "ignore_next_blank") 
                                   [e] in
                               fn first ids ((`Normal (id, cst, e, opt)) ::
                                 ls)
                           | (`Normal (id,_,e,opt))::[] ->
                               let occur_loc_id =
                                 ((fst id) <> "_") && (pop_location (fst id)) in
                               let e = apply_option _loc opt occur_loc_id e in
                               let f =
                                 match ((find_locate ()),
                                         (first && occur_loc))
                                 with
                                 | (Some _,true ) -> "apply_position"
                                 | _ -> "apply" in
                               (match action.pexp_desc with
                                | Pexp_ident (Lident id') when
                                    ((fst id) = id') && (f = "apply") -> e
                                | _ ->
                                    exp_apply _loc (exp_glr_fun _loc f)
                                      [build_action _loc occur_loc
                                         ((id, occur_loc_id) :: ids) action;
                                      e])
                           | (`Normal (id,_,e,opt))::(`Normal (id',_,e',opt'))::[]
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
                           | (`Normal (id,_,e,opt))::ls ->
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
              (Decap.fixpoint' []
                 (Decap.apply (fun x  l  -> x :: l)
                    (Decap.fsequence (Decap.char '|' '|')
                       (Decap.sequence (Decap.char '|' '|') glr_rule
                          (fun _  r  _  -> r))))))
           (fun r  rs  _default_0  __loc__start__buf  __loc__start__pos 
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
           (Decap.option false
              (Decap.alternatives
                 [Decap.apply (fun _  -> false) (Decap.char '|' '|');
                 Decap.apply (fun _  -> true) (Decap.char '~' '~')]))
           (Decap.sequence glr_rules_aux
              (Decap.apply List.rev
                 (Decap.fixpoint' []
                    (Decap.apply (fun x  l  -> x :: l)
                       (Decap.sequence (Decap.char '|' '|') glr_rules_aux
                          (fun _  r  -> r)))))
              (fun r  rs  g  __loc__start__buf  __loc__start__pos 
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
                     let f =
                       if g
                       then "alternatives"
                       else
                         (try ignore (Sys.getenv "GREEDY"); "alternatives'"
                          with | Not_found  -> "alternatives") in
                     exp_apply _loc (exp_glr_fun _loc f) [l])))
  end
