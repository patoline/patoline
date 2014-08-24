open Input
open Glr
open Charset
open Asttypes
open Parsetree
open Longident
let memoize1 f =
  let h = Hashtbl.create 1001 in
  fun x  ->
    try Hashtbl.find h x
    with | Not_found  -> let res = f x in (Hashtbl.add h x res; res)
let memoize2 f =
  let h = Hashtbl.create 1001 in
  fun x  ->
    fun y  ->
      try Hashtbl.find h (x, y)
      with | Not_found  -> let res = f x y in (Hashtbl.add h (x, y) res; res)
let fast = ref false
let file = ref None
let ascii = ref false
type entry =
  | FromExt
  | Impl
  | Intf
let entry = ref FromExt
let modern = ref false
let spec =
  [("--ascii", (Arg.Set ascii), "output ascii ast instead of serialized ast");
  ("--impl", (Arg.Unit ((fun ()  -> entry := Impl))),
    "treat file as an implementation");
  ("--intf", (Arg.Unit ((fun ()  -> entry := Intf))),
    "treat file as an interface");
  ("--modern", (Arg.Set modern),
    "enable \"modern\" extensions/restrictions of ocaml's grammar");
  ("--unsafe", (Arg.Set fast), "use unsafe function for arrays")]
let anon_fun s = file := (Some s)
let _ =
  Arg.parse spec anon_fun
    (Printf.sprintf "usage: %s [options] file" (Sys.argv.(0)))
let entry =
  match ((!entry), (!file)) with
  | (FromExt ,Some s) ->
      if Filename.check_suffix s ".mli" then Intf else Impl
  | (FromExt ,None ) -> Intf
  | (i,_) -> i
exception Unclosed_comment of int* int
let print_blank_state ch s =
  let s =
    match s with
    | `Ini -> "Ini"
    | `Str -> "Str"
    | `Cls -> "Cls"
    | `Opn -> "Opn"
    | `Esc -> "Esc"
    | `Chr -> "Chr" in
  Printf.fprintf ch "%s" s
let blank str pos =
  let rec fn lvl state prev ((str,pos) as cur) =
    if is_empty str
    then
      (if lvl > 0
       then raise (Unclosed_comment ((line_num str), pos))
       else cur)
    else
      (let (c,str',pos') = read str pos in
       let next = (str', pos') in
       match (state, c) with
       | (`Esc,_) -> fn lvl `Str cur next
       | (`Str,'"') -> fn lvl `Ini cur next
       | (`Chr,_) -> fn lvl `Ini cur next
       | (`Str,'\\') -> fn lvl `Esc cur next
       | (`Str,_) -> fn lvl `Str cur next
       | (_,'"') when lvl > 0 -> fn lvl `Str cur next
       | (_,'\'') when lvl > 0 -> fn lvl `Chr cur next
       | (`Ini,'(') -> fn lvl `Opn cur next
       | (`Opn,'*') -> fn (lvl + 1) `Ini cur next
       | (`Opn,_) when lvl = 0 -> prev
       | (`Opn,_) -> fn lvl `Ini cur next
       | (`Ini,'*') when lvl = 0 -> cur
       | (`Ini,'*') -> fn lvl `Cls cur next
       | (`Cls,'*') -> fn lvl `Cls cur next
       | (`Cls,')') -> fn (lvl - 1) `Ini cur next
       | (`Cls,_) -> fn lvl `Ini cur next
       | (_,(' '|'\t'|'\r'|'\n')) -> fn lvl `Ini cur next
       | (_,_) when lvl > 0 -> fn lvl `Ini cur next
       | (_,_) -> cur) in
  fn 0 `Ini (str, pos) (str, pos)
let no_blank str pos = (str, pos)
let ghost loc = let open Location in { loc with loc_ghost = true }
let locate g =
  filter_position g
    (let open Lexing in
       fun fname  ->
         fun l  ->
           fun pos  ->
             fun l'  ->
               fun pos'  ->
                 let s =
                   {
                     pos_fname = fname;
                     pos_lnum = l;
                     pos_cnum = pos;
                     pos_bol = 0
                   } in
                 let e =
                   {
                     pos_fname = fname;
                     pos_lnum = l';
                     pos_cnum = pos';
                     pos_bol = 0
                   } in
                 let open Location in
                   { loc_start = s; loc_end = e; loc_ghost = false })
let merge l1 l2 =
  let open Location in
    {
      loc_start = (l1.loc_start);
      loc_end = (l2.loc_end);
      loc_ghost = (l1.loc_ghost && l2.loc_ghost)
    }
module Initial =
  struct
    type expression_lvl =
      | Top
      | Let
      | Seq
      | Coerce
      | If
      | Aff
      | Tupl
      | Disj
      | Conj
      | Eq
      | Append
      | Cons
      | Sum
      | Prod
      | Pow
      | Opp
      | App
      | Dash
      | Dot
      | Prefix
      | Atom
    let next_exp =
      function
      | Top  -> Let
      | Let  -> Seq
      | Seq  -> Coerce
      | Coerce  -> If
      | If  -> Aff
      | Aff  -> Tupl
      | Tupl  -> Disj
      | Disj  -> Conj
      | Conj  -> Eq
      | Eq  -> Append
      | Append  -> Cons
      | Cons  -> Sum
      | Sum  -> Prod
      | Prod  -> Pow
      | Pow  -> Opp
      | Opp  -> App
      | App  -> Dash
      | Dash  -> Dot
      | Dot  -> Prefix
      | Prefix  -> Atom
      | Atom  -> Atom
    let ((expression_lvl : expression_lvl -> expression grammar),set_expression_lvl)
      = grammar_family "expression_lvl"
    let expr = expression_lvl Top
    let expression = expr
    let module_item: structure_item grammar = declare_grammar "module_item"
    let signature_item: signature_item grammar =
      declare_grammar "signature_item"
    type type_prio =
      | TopType
      | As
      | Arr
      | Prod
      | DashType
      | AppType
      | AtomType
    let ((typexpr_lvl : type_prio -> core_type grammar),set_typexpr_lvl) =
      grammar_family "typexpr_lvl"
    let typexpr = typexpr_lvl TopType
    type pattern_prio =
      | TopPat
      | AsPat
      | AltPat
      | TupPat
      | ConsPat
      | ConstrPat
      | AtomPat
    let ((pattern_lvl : pattern_prio -> pattern grammar),set_pattern_lvl) =
      grammar_family "pattern_lvl"
    let pattern = pattern_lvl TopPat
    let let_binding: value_binding list grammar =
      declare_grammar "let_binding"
    let class_body: Parsetree.class_structure grammar =
      declare_grammar "class_body"
    let class_expr: Parsetree.class_expr grammar =
      declare_grammar "class_expr"
    let extra_expressions: (expression_lvl* expression) grammar list = []
    let extra_types: core_type grammar list = []
    let extra_patterns: (pattern_prio* pattern) grammar list = []
    let extra_module_items: structure_item_desc grammar list = []
    let extra_signature_items: signature_item_desc grammar list = []
    let loc_expr _loc e =
      { pexp_desc = e; pexp_loc = _loc; pexp_attributes = [] }
    let loc_pat _loc pat =
      { ppat_desc = pat; ppat_loc = _loc; ppat_attributes = [] }
    let loc_pcl _loc desc =
      { pcl_desc = desc; pcl_loc = _loc; pcl_attributes = [] }
    let loc_typ _loc typ =
      { ptyp_desc = typ; ptyp_loc = _loc; ptyp_attributes = [] }
    let pctf_loc _loc desc =
      { pctf_desc = desc; pctf_loc = _loc; pctf_attributes = [] }
    let pcty_loc _loc desc =
      { pcty_desc = desc; pcty_loc = _loc; pcty_attributes = [] }
    let loc_pcf _loc desc =
      { pcf_desc = desc; pcf_loc = _loc; pcf_attributes = [] }
    let mexpr_loc _loc desc =
      { pmod_desc = desc; pmod_loc = _loc; pmod_attributes = [] }
    let mtyp_loc _loc desc =
      { pmty_desc = desc; pmty_loc = _loc; pmty_attributes = [] }
    let const_string s = Const_string (s, None)
    let constructor_declaration _loc name args res =
      {
        pcd_name = name;
        pcd_args = args;
        pcd_res = res;
        pcd_attributes = [];
        pcd_loc = _loc
      }
    let label_declaration _loc name mut ty =
      {
        pld_name = name;
        pld_mutable = mut;
        pld_type = ty;
        pld_attributes = [];
        pld_loc = _loc
      }
    let params_map _loc params =
      let fn (name,var) =
        match name with
        | None  -> ((loc_typ _loc Ptyp_any), var)
        | Some name -> ((loc_typ name.loc (Ptyp_var (name.txt))), var) in
      List.map fn params
    let type_declaration _loc name params cstrs kind priv manifest =
      let params = params_map _loc params in
      {
        ptype_name = name;
        ptype_params = params;
        ptype_cstrs = cstrs;
        ptype_kind = kind;
        ptype_private = priv;
        ptype_manifest = manifest;
        ptype_attributes = [];
        ptype_loc = _loc
      }
    let class_type_declaration _loc name params virt expr =
      let params = params_map _loc params in
      {
        pci_params = params;
        pci_virt = virt;
        pci_name = name;
        pci_expr = expr;
        pci_attributes = [];
        pci_loc = _loc
      }
    let pstr_eval e = Pstr_eval (e, [])
    let psig_value _loc name ty prim =
      Psig_value
        {
          pval_name = name;
          pval_type = ty;
          pval_prim = prim;
          pval_attributes = [];
          pval_loc = _loc
        }
    let value_binding _loc pat expr =
      { pvb_pat = pat; pvb_expr = expr; pvb_attributes = []; pvb_loc = _loc }
    let module_binding _loc name mt me =
      let me =
        match mt with
        | None  -> me
        | Some mt -> mexpr_loc _loc (Pmod_constraint (me, mt)) in
      { pmb_name = name; pmb_expr = me; pmb_attributes = []; pmb_loc = _loc }
    let module_declaration _loc name mt =
      { pmd_name = name; pmd_type = mt; pmd_attributes = []; pmd_loc = _loc }
    let ppat_construct (a,b) = Ppat_construct (a, b)
    let pexp_construct (a,b) = Pexp_construct (a, b)
    let pexp_constraint (a,b) = Pexp_constraint (a, b)
    let pexp_coerce (a,b,c) = Pexp_coerce (a, b, c)
    let pexp_assertfalse _loc =
      Pexp_assert
        (loc_expr _loc
           (pexp_construct ({ txt = (Lident "false"); loc = _loc }, None)))
    let map_cases cases =
      List.map
        (fun (pat,expr,guard)  ->
           { pc_lhs = pat; pc_rhs = expr; pc_guard = guard }) cases
    let pexp_function cases = Pexp_function cases
    let pexp_fun (label,opt,pat,expr) = Pexp_fun (label, opt, pat, expr)
    type quote_env1 =
      {
      mutable expression_stack: Parsetree.expression list;
      mutable pattern_stack: Parsetree.expression list;
      mutable type_stack: Parsetree.expression list;
      mutable str_item_stack: Parsetree.expression list;
      mutable sig_item_stack: Parsetree.expression list;
      mutable string_stack: Parsetree.expression list;
      mutable int_stack: Parsetree.expression list;
      mutable int32_stack: Parsetree.expression list;
      mutable int64_stack: Parsetree.expression list;
      mutable natint_stack: Parsetree.expression list;
      mutable float_stack: Parsetree.expression list;
      mutable char_stack: Parsetree.expression list;
      mutable bool_stack: Parsetree.expression list;}
    type quote_env2 =
      {
      mutable expression_stack: Parsetree.expression list;
      mutable pattern_stack: Parsetree.pattern list;
      mutable type_stack: Parsetree.core_type list;
      mutable str_item_stack: Parsetree.structure_item_desc list;
      mutable sig_item_stack: Parsetree.signature_item_desc list;
      mutable string_stack: string list;
      mutable int_stack: int list;
      mutable int32_stack: int32 list;
      mutable int64_stack: int64 list;
      mutable natint_stack: nativeint list;
      mutable float_stack: float list;
      mutable char_stack: char list;
      mutable bool_stack: bool list;}
    type quote_env =
      | First of quote_env1
      | Second of quote_env2
    let quote_stack: quote_env Stack.t = Stack.create ()
    let empty_quote_env1 () =
      First
        {
          expression_stack = [];
          pattern_stack = [];
          type_stack = [];
          str_item_stack = [];
          sig_item_stack = [];
          string_stack = [];
          int_stack = [];
          int32_stack = [];
          int64_stack = [];
          natint_stack = [];
          float_stack = [];
          char_stack = [];
          bool_stack = []
        }
    let empty_quote_env2 () =
      Second
        {
          expression_stack = [];
          pattern_stack = [];
          type_stack = [];
          str_item_stack = [];
          sig_item_stack = [];
          string_stack = [];
          int_stack = [];
          int32_stack = [];
          int64_stack = [];
          natint_stack = [];
          float_stack = [];
          char_stack = [];
          bool_stack = []
        }
    let push_pop_expression e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env.expression_stack <- e :: (env.expression_stack); e)
        | Second env ->
            (match env.expression_stack with
             | e::l -> (env.expression_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_expression e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.expression_stack <- e :: (env.expression_stack)
    let push_pop_type e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env.type_stack <- e :: (env.type_stack);
             loc_typ e.pexp_loc Ptyp_any)
        | Second env ->
            (match env.type_stack with
             | e::l -> (env.type_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_type e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.type_stack <- e :: (env.type_stack)
    let push_pop_pattern e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env.pattern_stack <- e :: (env.pattern_stack);
             loc_pat e.pexp_loc Ppat_any)
        | Second env ->
            (match env.pattern_stack with
             | e::l -> (env.pattern_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_pattern e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.pattern_stack <- e :: (env.pattern_stack)
    let push_pop_str_item e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env.str_item_stack <- e :: (env.str_item_stack); pstr_eval e)
        | Second env ->
            (match env.str_item_stack with
             | e::l -> (env.str_item_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_str_item e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.str_item_stack <- e :: (env.str_item_stack)
    let push_pop_sig_item e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env.sig_item_stack <- e :: (env.sig_item_stack);
             (let _loc = e.pexp_loc in
              psig_value _loc { txt = ""; loc = _loc }
                (loc_typ _loc Ptyp_any) []))
        | Second env ->
            (match env.sig_item_stack with
             | e::l -> (env.sig_item_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_sig_item e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.sig_item_stack <- e :: (env.sig_item_stack)
    let push_pop_string e =
      try
        match Stack.top quote_stack with
        | First env -> (env.string_stack <- e :: (env.string_stack); "")
        | Second env ->
            (match env.string_stack with
             | e::l -> (env.string_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_string e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.string_stack <- e :: (env.string_stack)
    let push_pop_int e =
      try
        match Stack.top quote_stack with
        | First env -> (env.int_stack <- e :: (env.int_stack); 0)
        | Second env ->
            (match env.int_stack with
             | e::l -> (env.int_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_int e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.int_stack <- e :: (env.int_stack)
    let push_pop_int32 e =
      try
        match Stack.top quote_stack with
        | First env -> (env.int32_stack <- e :: (env.int32_stack); 0l)
        | Second env ->
            (match env.int32_stack with
             | e::l -> (env.int32_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_int32 e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.int32_stack <- e :: (env.int32_stack)
    let push_pop_int64 e =
      try
        match Stack.top quote_stack with
        | First env -> (env.int64_stack <- e :: (env.int64_stack); 0L)
        | Second env ->
            (match env.int64_stack with
             | e::l -> (env.int64_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_int64 e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.int64_stack <- e :: (env.int64_stack)
    let push_pop_natint e =
      try
        match Stack.top quote_stack with
        | First env -> (env.natint_stack <- e :: (env.natint_stack); 0n)
        | Second env ->
            (match env.natint_stack with
             | e::l -> (env.natint_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_natint e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.natint_stack <- e :: (env.natint_stack)
    let push_pop_float e =
      try
        match Stack.top quote_stack with
        | First env -> (env.float_stack <- e :: (env.float_stack); 0.0)
        | Second env ->
            (match env.float_stack with
             | e::l -> (env.float_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_float e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.float_stack <- e :: (env.float_stack)
    let push_pop_char e =
      try
        match Stack.top quote_stack with
        | First env -> (env.char_stack <- e :: (env.char_stack); ' ')
        | Second env ->
            (match env.char_stack with
             | e::l -> (env.char_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_char e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.char_stack <- e :: (env.char_stack)
    let push_pop_bool e =
      try
        match Stack.top quote_stack with
        | First env -> (env.bool_stack <- e :: (env.bool_stack); false)
        | Second env ->
            (match env.bool_stack with
             | e::l -> (env.bool_stack <- l; e)
             | _ -> assert false)
      with | Stack.Empty  -> raise Give_up
    let push_bool e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env.bool_stack <- e :: (env.bool_stack)
    let quote_expression _loc e name =
      let cols =
        let n =
          let open Location in let open Lexing in (_loc.loc_start).pos_cnum in
        String.make (n + 1) ' ' in
      let e =
        ((let open Location in
            let open Lexing in
              Printf.sprintf "#%d %S\n%s" ((_loc.loc_start).pos_lnum - 1)
                (_loc.loc_start).pos_fname) cols)
          ^ e in
      Stack.push (empty_quote_env1 ()) quote_stack;
      (let _ =
         match name with
         | "expression" ->
             ignore
               (parse_string
                  (Glr.sequence expression (Glr.eof ())
                     (fun e  -> fun _unnamed_1  -> e)) blank "quote..." e)
         | "type" -> ignore (parse_string typexpr blank "quote..." e)
         | "pattern" -> ignore (parse_string pattern blank "quote..." e)
         | "str_item" -> ignore (parse_string module_item blank "quote..." e)
         | "sig_item" ->
             ignore (parse_string signature_item blank "quote..." e)
         | _ -> assert false in
       let env =
         match Stack.pop quote_stack with
         | First e -> e
         | Second _ -> assert false in
       let push_expr =
         loc_expr _loc
           (Pexp_apply
              ((loc_expr _loc
                  (Pexp_ident
                     { txt = (Ldot ((Lident "Stack"), "push")); loc = _loc })),
                [("",
                   (loc_expr _loc
                      (Pexp_apply
                         ((loc_expr _loc
                             (Pexp_ident
                                {
                                  txt =
                                    (Ldot
                                       ((Lident "Pa_ocaml_prelude"),
                                         "empty_quote_env2"));
                                  loc = _loc
                                })),
                           [("",
                              (loc_expr _loc
                                 (pexp_construct
                                    ({ txt = (Lident "()"); loc = _loc },
                                      None))))]))));
                ("",
                  (loc_expr _loc
                     (Pexp_ident
                        {
                          txt =
                            (Ldot
                               ((Lident "Pa_ocaml_prelude"), "quote_stack"));
                          loc = _loc
                        })))])) in
       let fill push_expr name l =
         let p =
           List.fold_left
             (fun acc  ->
                fun e  ->
                  let push_e =
                    loc_expr _loc
                      (Pexp_apply
                         ((loc_expr _loc
                             (Pexp_ident
                                {
                                  txt =
                                    (Ldot ((Lident "Pa_ocaml_prelude"), name));
                                  loc = _loc
                                })), [("", e)])) in
                  match acc with
                  | None  -> Some push_e
                  | Some acc ->
                      Some (loc_expr _loc (Pexp_sequence (acc, push_e))))
             None l in
         match p with
         | None  -> push_expr
         | Some e -> loc_expr _loc (Pexp_sequence (push_expr, e)) in
       let push_expr = fill push_expr "push_expression" env.expression_stack in
       let push_expr = fill push_expr "push_pattern" env.pattern_stack in
       let push_expr = fill push_expr "push_type" env.type_stack in
       let push_expr = fill push_expr "push_str_item" env.str_item_stack in
       let push_expr = fill push_expr "push_sig_item" env.sig_item_stack in
       let push_expr = fill push_expr "push_string" env.string_stack in
       let push_expr = fill push_expr "push_int" env.int_stack in
       let push_expr = fill push_expr "push_int32" env.int32_stack in
       let push_expr = fill push_expr "push_int64" env.int64_stack in
       let push_expr = fill push_expr "push_natint" env.natint_stack in
       let push_expr = fill push_expr "push_float" env.float_stack in
       let push_expr = fill push_expr "push_char" env.char_stack in
       let push_expr = fill push_expr "push_bool" env.bool_stack in
       let pop_expr =
         loc_expr _loc
           (Pexp_apply
              ((loc_expr _loc
                  (Pexp_ident { txt = (Lident "ignore"); loc = _loc })),
                [("",
                   (loc_expr _loc
                      (Pexp_apply
                         ((loc_expr _loc
                             (Pexp_ident
                                {
                                  txt = (Ldot ((Lident "Stack"), "pop"));
                                  loc = _loc
                                })),
                           [("",
                              (loc_expr _loc
                                 (Pexp_ident
                                    {
                                      txt =
                                        (Ldot
                                           ((Lident "Pa_ocaml_prelude"),
                                             "quote_stack"));
                                      loc = _loc
                                    })))]))))])) in
       let parse_expr =
         loc_expr _loc
           (Pexp_apply
              ((loc_expr _loc
                  (Pexp_ident
                     {
                       txt =
                         (Ldot
                            ((Lident "Pa_ocaml_prelude"),
                              ("quote_" ^ (name ^ "_2"))));
                       loc = _loc
                     })),
                [("", (loc_expr _loc (Pexp_constant (const_string e))))])) in
       loc_expr _loc
         (Pexp_sequence
            (push_expr,
              (loc_expr _loc
                 (Pexp_let
                    (Nonrecursive,
                      [value_binding _loc
                         (loc_pat _loc
                            (Ppat_var { txt = "quote_res"; loc = _loc }))
                         parse_expr],
                      (loc_expr _loc
                         (Pexp_sequence
                            (pop_expr,
                              (loc_expr _loc
                                 (Pexp_ident
                                    { txt = (Lident "quote_res"); loc = _loc
                                    })))))))))))
    let quote_expression_2 e = parse_string expression blank "quote..." e
    let quote_type_2 e = parse_string typexpr blank "quote..." e
    let quote_pattern_2 e = parse_string pattern blank "quote..." e
    let quote_str_item_2 e = parse_string module_item blank "quote..." e
    let quote_sig_item_2 e = parse_string signature_item blank "quote..." e
    let par_re s = "\\(" ^ (s ^ "\\)")
    let union_re l =
      let l = List.map (fun s  -> par_re s) l in String.concat "\\|" l
    let lident_re = "\\([a-z][a-zA-Z0-9_']*\\)\\|\\([_][a-zA-Z0-9_']+\\)"
    let cident_re = "[A-Z][a-zA-Z0-9_']*"
    let ident_re = "[A-Za-z_][a-zA-Z0-9_']*"
    let reserved_ident =
      ["and";
      "as";
      "assert";
      "asr";
      "begin";
      "class";
      "constraint";
      "do";
      "done";
      "downto";
      "else";
      "end";
      "exception";
      "external";
      "false";
      "for";
      "fun";
      "function";
      "functor";
      "if";
      "in";
      "include";
      "inherit";
      "initializer";
      "land";
      "lazy";
      "let";
      "lor";
      "lsl";
      "lsr";
      "lxor";
      "match";
      "method";
      "mod";
      "module";
      "mutable";
      "new";
      "object";
      "of";
      "open";
      "or";
      "private";
      "rec";
      "sig";
      "struct";
      "then";
      "to";
      "true";
      "try";
      "type";
      "val";
      "virtual";
      "when";
      "while";
      "with"]
    let is_reserved_id w = List.mem w reserved_ident
    let ident =
      Glr.alternatives
        [Glr.apply (fun id  -> if is_reserved_id id then raise Give_up; id)
           (Glr.regexp ~name:"ident" ident_re (fun groupe  -> groupe 0));
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "ident" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  -> fun _unnamed_4  -> push_pop_string e))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
    let capitalized_ident =
      Glr.alternatives
        [Glr.apply (fun id  -> id)
           (Glr.regexp ~name:"cident" cident_re (fun groupe  -> groupe 0));
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "uid" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  -> fun _unnamed_4  -> push_pop_string e))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
    let lowercase_ident =
      Glr.alternatives
        [Glr.apply (fun id  -> if is_reserved_id id then raise Give_up; id)
           (Glr.regexp ~name:"lident" lident_re (fun groupe  -> groupe 0));
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "lid" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  -> fun _unnamed_4  -> push_pop_string e))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
    let key_word s =
      let len_s = String.length s in
      assert (len_s > 0);
      black_box
        (fun str  ->
           fun pos  ->
             let str' = ref str in
             let pos' = ref pos in
             for i = 0 to len_s - 1 do
               (let (c,_str',_pos') = read (!str') (!pos') in
                if c <> (s.[i]) then raise Give_up;
                str' := _str';
                pos' := _pos')
             done;
             (let str' = !str'
              and pos' = !pos' in
              let (c,_,_) = read str' pos' in
              match c with
              | 'a'
                |'b'
                 |'c'
                  |'d'
                   |'e'
                    |'f'
                     |'g'
                      |'h'
                       |'i'
                        |'j'
                         |'k'
                          |'l'
                           |'m'
                            |'n'
                             |'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'
                |'A'
                 |'B'
                  |'C'
                   |'D'
                    |'E'
                     |'F'
                      |'G'
                       |'H'
                        |'I'
                         |'J'
                          |'K'
                           |'L'
                            |'M'
                             |'N'
                              |'O'
                               |'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'
                |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'_'|'\'' ->
                  raise Give_up
              | _ -> ((), str', pos'))) (Charset.singleton (s.[0])) false s
    let mutable_kw = key_word "mutable"
    let mutable_flag =
      Glr.alternatives
        [Glr.apply (fun _unnamed_0  -> Mutable) mutable_kw;
        Glr.apply (fun _unnamed_0  -> Immutable) (Glr.empty ())]
    let private_kw = key_word "private"
    let private_flag =
      Glr.alternatives
        [Glr.apply (fun _unnamed_0  -> Private) private_kw;
        Glr.apply (fun _unnamed_0  -> Public) (Glr.empty ())]
    let virtual_kw = key_word "virtual"
    let virtual_flag =
      Glr.alternatives
        [Glr.apply (fun _unnamed_0  -> Virtual) virtual_kw;
        Glr.apply (fun _unnamed_0  -> Concrete) (Glr.empty ())]
    let rec_kw = key_word "rec"
    let rec_flag =
      Glr.alternatives
        [Glr.apply (fun _unnamed_0  -> Recursive) rec_kw;
        Glr.apply (fun _unnamed_0  -> Nonrecursive) (Glr.empty ())]
    let to_kw = key_word "to"
    let downto_kw = key_word "downto"
    let downto_flag =
      Glr.alternatives
        [Glr.apply (fun _unnamed_0  -> Upto) to_kw;
        Glr.apply (fun _unnamed_0  -> Downto) downto_kw]
    let method_kw = key_word "method"
    let object_kw = key_word "object"
    let class_kw = key_word "class"
    let inherit_kw = key_word "inherit"
    let as_kw = key_word "as"
    let of_kw = key_word "of"
    let module_kw = key_word "module"
    let open_kw = key_word "open"
    let include_kw = key_word "include"
    let type_kw = key_word "type"
    let val_kw = key_word "val"
    let external_kw = key_word "external"
    let constraint_kw = key_word "constraint"
    let begin_kw = key_word "begin"
    let end_kw = key_word "end"
    let and_kw = key_word "and"
    let true_kw = key_word "true"
    let false_kw = key_word "false"
    let exception_kw = key_word "exception"
    let when_kw = key_word "when"
    let fun_kw = key_word "fun"
    let function_kw = key_word "function"
    let let_kw = key_word "let"
    let in_kw = key_word "in"
    let initializer_kw = key_word "initializer"
    let with_kw = key_word "with"
    let while_kw = key_word "while"
    let for_kw = key_word "for"
    let do_kw = key_word "do"
    let done_kw = key_word "done"
    let new_kw = key_word "new"
    let assert_kw = key_word "assert"
    let if_kw = key_word "if"
    let then_kw = key_word "then"
    let else_kw = key_word "else"
    let try_kw = key_word "try"
    let match_kw = key_word "match"
    let struct_kw = key_word "struct"
    let functor_kw = key_word "functor"
    let sig_kw = key_word "sig"
    let lazy_kw = key_word "lazy"
    let glr_kw = key_word "glr"
    let int_dec_re = "[0-9][0-9_]*"
    let int_hex_re = "[0][xX][0-9a-fA-F][0-9a-fA-F_]*"
    let int_oct_re = "[0][oO][0-7][0-7_]*"
    let int_bin_re = "[0][bB][01][01_]*"
    let int_pos_re =
      union_re [int_hex_re; int_oct_re; int_bin_re; int_dec_re]
    let int_re = int_pos_re
    let int32_re = (par_re int_pos_re) ^ "l"
    let int64_re = (par_re int_pos_re) ^ "L"
    let natint_re = (par_re int_pos_re) ^ "n"
    let integer_literal =
      Glr.alternatives
        [Glr.apply (fun i  -> int_of_string i)
           (Glr.regexp ~name:"int_pos" int_pos_re (fun groupe  -> groupe 0));
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "int" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  -> fun _unnamed_4  -> push_pop_int e))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
    let int32_lit =
      Glr.alternatives
        [Glr.apply (fun i  -> Int32.of_string i)
           (Glr.regexp ~name:"int32" int32_re (fun groupe  -> groupe 1));
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "int32" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  -> fun _unnamed_4  -> push_pop_int32 e))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
    let int64_lit =
      Glr.alternatives
        [Glr.apply (fun i  -> Int64.of_string i)
           (Glr.regexp ~name:"int64" int64_re (fun groupe  -> groupe 1));
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "int64" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  -> fun _unnamed_4  -> push_pop_int64 e))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
    let nat_int_lit =
      Glr.alternatives
        [Glr.apply (fun i  -> Nativeint.of_string i)
           (Glr.regexp ~name:"natint" natint_re (fun groupe  -> groupe 1));
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "natint" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  -> fun _unnamed_4  -> push_pop_natint e))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
    let bool_lit =
      Glr.alternatives
        [Glr.apply (fun _unnamed_0  -> "false") false_kw;
        Glr.apply (fun _unnamed_0  -> "true") true_kw;
        Glr.sequence
          (Glr.sequence
             (Glr.sequence
                (Glr.sequence (Glr.char '$' ()) (Glr.string "bool" ())
                   (fun _unnamed_0  ->
                      fun _unnamed_1  ->
                        fun _unnamed_2  ->
                          fun e  ->
                            fun _unnamed_4  ->
                              if push_pop_bool e then "true" else "false"))
                (Glr.char ':' ()) (fun x  -> x)) expression (fun x  -> x))
          (Glr.char '$' ()) (fun x  -> x)]
  end
module type Extension  = module type of Initial
module type FExt  = functor (E : Extension) -> Extension
let extensions_mod = ref ([] : (module FExt) list)
let register_extension e = extensions_mod := (e :: (!extensions_mod))
include Initial
