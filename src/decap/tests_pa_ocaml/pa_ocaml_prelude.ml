open Input
open Decap
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
let memoize2' f =
  let h = Hashtbl.create 1001 in
  fun a  ->
    fun x  ->
      fun y  ->
        try Hashtbl.find h (x, y)
        with
        | Not_found  -> let res = f a x y in (Hashtbl.add h (x, y) res; res)
let fast = ref false
let file: string option ref = ref None
let ascii = ref false
let in_ocamldep = ref false
type entry =
  | FromExt
  | Impl
  | Intf
  | Toplvl
let entry = ref FromExt
let modern = ref false
let spec =
  ref
    [("--ascii", (Arg.Set ascii),
       "output ascii ast instead of serialized ast");
    ("--impl", (Arg.Unit ((fun ()  -> entry := Impl))),
      "treat file as an implementation");
    ("--intf", (Arg.Unit ((fun ()  -> entry := Intf))),
      "treat file as an interface");
    ("--modern", (Arg.Set modern),
      "enable \"modern\" extensions/restrictions of ocaml's grammar");
    ("--unsafe", (Arg.Set fast), "use unsafe function for arrays");
    ("--ocamldep", (Arg.Set in_ocamldep),
      "set a flag to inform parser that we are computing dependencies")]
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
       | (`StrO l,'a'..'z') -> fn lvl (`StrO (c :: l)) cur next
       | (`StrO l,'|') -> fn lvl (`StrI (List.rev l)) cur next
       | (`StrO _,_) -> fn lvl `Ini cur next
       | (`StrI l,'|') -> fn lvl (`StrC (l, l)) cur next
       | (`StrC (l',a::l),a') when a = a' -> fn lvl (`StrC (l', l)) cur next
       | (`StrC (_,[]),'}') -> fn lvl `Ini cur next
       | (`StrC (l',_),_) -> fn lvl (`StrI l') cur next
       | (_,'"') when lvl > 0 -> fn lvl `Str cur next
       | (_,'\'') when lvl > 0 -> fn lvl `Chr cur next
       | (_,'{') when lvl > 0 -> fn lvl (`StrO []) cur next
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
let start_pos loc = loc.Location.loc_start
let end_pos loc = loc.Location.loc_end
let locate str pos str' pos' =
  let open Lexing in
    let s = Input.lexing_position str pos in
    let e = Input.lexing_position str' pos' in
    let open Location in { loc_start = s; loc_end = e; loc_ghost = false }
let locate2 = locate
let rec merge =
  function
  | [] -> assert false
  | loc::[] -> loc
  | l1::_ as ls ->
      let ls = List.rev ls in
      let rec fn =
        function
        | [] -> assert false
        | loc::[] -> loc
        | l2::ls when let open Location in l2.loc_start = l2.loc_end -> fn ls
        | l2::ls ->
            let open Location in
              {
                loc_start = (l1.loc_start);
                loc_end = (l2.loc_end);
                loc_ghost = (l1.loc_ghost && l2.loc_ghost)
              } in
      fn ls
let merge2 l1 l2 =
  let open Location in
    {
      loc_start = (l1.loc_start);
      loc_end = (l2.loc_end);
      loc_ghost = (l1.loc_ghost && l2.loc_ghost)
    }
let (push_frame,pop_frame,push_location,pop_location) =
  let loc_tbl: (string,unit) Hashtbl.t Stack.t = Stack.create () in
  ((fun ()  -> Stack.push (Hashtbl.create 23) loc_tbl),
    (fun ()  ->
       try
         let h = Stack.pop loc_tbl in
         Hashtbl.iter
           (fun l  ->
              fun _  ->
                try let h' = Stack.top loc_tbl in Hashtbl.replace h' l ()
                with | Stack.Empty  -> ()) h
       with | Stack.Empty  -> ()),
    (fun id  ->
       try let h = Stack.top loc_tbl in Hashtbl.replace h id ()
       with | Stack.Empty  -> ()),
    (fun id  ->
       try
         let h = Stack.top loc_tbl in
         if Hashtbl.mem h id then (Hashtbl.remove h id; true) else false
       with | Stack.Empty  -> false))
module Initial =
  struct
    type expression_prio =
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
    let ((expression_lvl : expression_prio -> expression grammar),set_expression_lvl)
      = grammar_family "expression_lvl"
    let expr = expression_lvl Top
    let expression = expr
    let structure_item: structure_item list grammar =
      declare_grammar "structure_item"
    let signature_item: signature_item list grammar =
      declare_grammar "signature_item"
    let structure =
      Decap.apply (fun l  -> List.flatten l)
        (Decap.apply List.rev
           (Decap.fixpoint' []
              (Decap.apply (fun x  -> fun l  -> x :: l)
                 (Decap.apply (fun s  -> s) structure_item))))
    let signature =
      Decap.apply (fun l  -> List.flatten l)
        (Decap.apply List.rev
           (Decap.fixpoint' []
              (Decap.apply (fun x  -> fun l  -> x :: l)
                 (Decap.apply (fun s  -> s) signature_item))))
    type type_prio =
      | TopType
      | As
      | Arr
      | ProdType
      | DashType
      | AppType
      | AtomType
    let type_prios =
      [TopType; As; Arr; ProdType; DashType; AppType; AtomType]
    let type_prio_to_string =
      function
      | TopType  -> "TopType"
      | As  -> "As"
      | Arr  -> "Arr"
      | ProdType  -> "ProdType"
      | DashType  -> "DashType"
      | AppType  -> "AppType"
      | AtomType  -> "AtomType"
    let next_type_prio =
      function
      | TopType  -> As
      | As  -> Arr
      | Arr  -> ProdType
      | ProdType  -> DashType
      | DashType  -> AppType
      | AppType  -> AtomType
      | AtomType  -> AtomType
    let ((typexpr_lvl : type_prio -> core_type grammar),set_typexpr_lvl) =
      grammar_family ~param_to_string:type_prio_to_string "typexpr_lvl"
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
    type value_binding = (Parsetree.pattern* Parsetree.expression)
    let let_binding: value_binding list grammar =
      declare_grammar "let_binding"
    let class_body: class_structure grammar = declare_grammar "class_body"
    let class_expr: class_expr grammar = declare_grammar "class_expr"
    let extra_expressions: (expression_prio* expression) grammar list = []
    let extra_types: core_type grammar list = []
    let extra_patterns: (pattern_prio* pattern) grammar list = []
    let extra_structure: structure_item list grammar list = []
    let extra_signature: signature_item list grammar list = []
    let loc_str _loc desc = { pstr_desc = desc; pstr_loc = _loc }
    let loc_sig _loc desc = { psig_desc = desc; psig_loc = _loc }
    let loc_expr ?(attributes= [])  _loc e =
      { pexp_desc = e; pexp_loc = _loc }
    let loc_pat ?(attributes= [])  _loc pat =
      { ppat_desc = pat; ppat_loc = _loc }
    let loc_pcl ?(attributes= [])  _loc desc =
      { pcl_desc = desc; pcl_loc = _loc }
    let loc_typ ?(attributes= [])  _loc typ =
      { ptyp_desc = typ; ptyp_loc = _loc }
    let pctf_loc ?(attributes= [])  _loc desc =
      { pctf_desc = desc; pctf_loc = _loc }
    let loc_pcf ?(attributes= [])  _loc desc =
      { pcf_desc = desc; pcf_loc = _loc }
    let pcty_loc ?(attributes= [])  _loc desc =
      { pcty_desc = desc; pcty_loc = _loc }
    let mexpr_loc ?(attributes= [])  _loc desc =
      { pmod_desc = desc; pmod_loc = _loc }
    let mtyp_loc ?(attributes= [])  _loc desc =
      { pmty_desc = desc; pmty_loc = _loc }
    let id_loc txt loc = { txt; loc }
    let const_string s = Const_string s
    type constructor_declaration =
      (string Asttypes.loc* Parsetree.core_type list* Parsetree.core_type
        option* Location.t)
    let constructor_declaration _loc name args res = (name, args, res, _loc)
    type label_declaration =
      (string Asttypes.loc* Asttypes.mutable_flag* Parsetree.core_type*
        Location.t)
    type case = (pattern* expression)
    let label_declaration _loc name mut ty = (name, mut, ty, _loc)
    let type_declaration _loc name params cstrs kind priv manifest =
      let (params,variance) = List.split params in
      {
        ptype_params = params;
        ptype_cstrs = cstrs;
        ptype_kind = kind;
        ptype_private = priv;
        ptype_variance = variance;
        ptype_manifest = manifest;
        ptype_loc = _loc
      }
    let class_type_declaration _loc' _loc name params virt expr =
      let (params,variance) = List.split params in
      let params =
        List.map (function | None  -> id_loc "" _loc' | Some x -> x) params in
      {
        pci_params = (params, _loc');
        pci_variance = variance;
        pci_virt = virt;
        pci_name = name;
        pci_expr = expr;
        pci_loc = _loc
      }
    let pstr_eval e = Pstr_eval e
    let psig_value ?(attributes= [])  _loc name ty prim =
      Psig_value
        (name, { pval_type = ty; pval_prim = prim; pval_loc = _loc })
    let value_binding ?(attributes= [])  _loc pat expr = (pat, expr)
    let module_binding _loc name mt me = (name, mt, me)
    let module_declaration _loc name mt = (name, mt)
    let ppat_construct (a,b) = Ppat_construct (a, b, false)
    let pexp_construct (a,b) = Pexp_construct (a, b, false)
    let pexp_constraint (a,b) = Pexp_constraint (a, (Some b), None)
    let pexp_coerce (a,b,c) = Pexp_constraint (a, b, (Some c))
    let pexp_assertfalse _loc = Pexp_assertfalse
    let map_cases cases =
      List.map
        (fun (pat,expr,guard)  ->
           match guard with
           | None  -> (pat, expr)
           | Some e ->
               (pat,
                 (loc_expr (merge2 e.pexp_loc expr.pexp_loc)
                    (Pexp_when (e, expr))))) cases
    let pexp_function cases = Pexp_function ("", None, cases)
    let pexp_fun (label,opt,pat,expr) =
      Pexp_function (label, opt, [(pat, expr)])
    let constr_decl_list: constructor_declaration list grammar =
      declare_grammar "constr_decl_list"
    let field_decl_list: label_declaration list grammar =
      declare_grammar "field_decl_list"
    let ((match_cases : expression_prio -> case list grammar),set_match_cases)
      = grammar_family "match_cases"
    let module_expr: module_expr grammar = declare_grammar "module_expr"
    let module_type: module_type grammar = declare_grammar "module_type"
    type quote_env1 = (int* string* Parsetree.expression) list ref
    type quote_env2_data =
      | Expression of Parsetree.expression
      | Expression_list of Parsetree.expression list
      | Pattern of Parsetree.pattern
      | Pattern_list of Parsetree.pattern list
      | Type of Parsetree.core_type
      | Type_list of Parsetree.core_type list
      | Structure of Parsetree.structure_item list
      | Signature of Parsetree.signature_item list
      | Constr_decl of constructor_declaration list
      | Field_decl of label_declaration list
      | Let_binding of value_binding list
      | Module_expr of module_expr
      | Module_type of module_type
      | Cases of case list
      | String of string
      | Int of int
      | Int32 of int32
      | Int64 of int64
      | Natint of nativeint
      | Float of float
      | Char of char
      | Bool of bool
    type quote_env2 = (int* quote_env2_data) list ref
    type quote_env =
      | First of quote_env1
      | Second of quote_env2
    let quote_stack: quote_env Stack.t = Stack.create ()
    let empty_quote_env1 () = First (ref [])
    let empty_quote_env2 () = Second (ref [])
    let push_pop_expression pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_expression", e) :: (!env)); e)
        | Second env ->
            (match List.assoc pos (!env) with
             | Expression e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
      | Not_found  -> assert false
    let push_expression pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Expression e)) :: (!env))
    let push_pop_module_expr pos e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env := ((pos, "push_module_expr", e) :: (!env));
             mexpr_loc Location.none
               (Pmod_ident (id_loc (Lident "") Location.none)))
        | Second env ->
            (match List.assoc pos (!env) with
             | Module_expr e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
      | Not_found  -> assert false
    let push_module_expr pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Module_expr e)) :: (!env))
    let push_pop_module_type pos e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env := ((pos, "push_module_type", e) :: (!env));
             mtyp_loc Location.none
               (Pmty_ident (id_loc (Lident "") Location.none)))
        | Second env ->
            (match List.assoc pos (!env) with
             | Module_type e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
      | Not_found  -> assert false
    let push_module_type pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Module_type e)) :: (!env))
    let push_pop_expression_list pos e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env := ((pos, "push_expression_list", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Expression_list e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_expression_list pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Expression_list e)) :: (!env))
    let push_pop_constr_decl pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_constr_decl", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Constr_decl e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_constr_decl pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Constr_decl e)) :: (!env))
    let push_pop_field_decl pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_field_decl", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Field_decl e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_field_decl pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Field_decl e)) :: (!env))
    let push_pop_cases pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_cases", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Cases e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_cases pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Cases e)) :: (!env))
    let push_pop_let_binding pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_let_binding", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Let_binding e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_let_binding pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Let_binding e)) :: (!env))
    let push_pop_type pos e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env := ((pos, "push_type", e) :: (!env));
             loc_typ e.pexp_loc Ptyp_any)
        | Second env ->
            (match List.assoc pos (!env) with
             | Type e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_type pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Type e)) :: (!env))
    let push_pop_type_list pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_type_list", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Type_list e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_type_list pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Type_list e)) :: (!env))
    let push_pop_pattern pos e =
      try
        match Stack.top quote_stack with
        | First env ->
            (env := ((pos, "push_pattern", e) :: (!env));
             loc_pat e.pexp_loc Ppat_any)
        | Second env ->
            (match List.assoc pos (!env) with
             | Pattern e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_pattern pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Pattern e)) :: (!env))
    let push_pop_pattern_list pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_pattern_list", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Pattern_list e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_pattern_list pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Pattern_list e)) :: (!env))
    let push_pop_structure pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_structure", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Structure e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_structure pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Structure e)) :: (!env))
    let push_pop_signature pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_signature", e) :: (!env)); [])
        | Second env ->
            (match List.assoc pos (!env) with
             | Signature e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_signature pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Signature e)) :: (!env))
    let push_pop_string pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_string", e) :: (!env)); "")
        | Second env ->
            (match List.assoc pos (!env) with
             | String e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_string pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (String e)) :: (!env))
    let push_pop_int pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_int", e) :: (!env)); 0)
        | Second env ->
            (match List.assoc pos (!env) with
             | Int e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_int pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Int e)) :: (!env))
    let push_pop_int32 pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_int32", e) :: (!env)); 0l)
        | Second env ->
            (match List.assoc pos (!env) with
             | Int32 e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_int32 pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Int32 e)) :: (!env))
    let push_pop_int64 pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_int64", e) :: (!env)); 0L)
        | Second env ->
            (match List.assoc pos (!env) with
             | Int64 e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_int64 pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Int64 e)) :: (!env))
    let push_pop_natint pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_natint", e) :: (!env)); 0n)
        | Second env ->
            (match List.assoc pos (!env) with
             | Natint e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_natint pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Natint e)) :: (!env))
    let push_pop_float pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_float", e) :: (!env)); 0.0)
        | Second env ->
            (match List.assoc pos (!env) with
             | Float e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_float pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Float e)) :: (!env))
    let push_pop_char pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_char", e) :: (!env)); ' ')
        | Second env ->
            (match List.assoc pos (!env) with
             | Char e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_char pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Char e)) :: (!env))
    let push_pop_bool pos e =
      try
        match Stack.top quote_stack with
        | First env -> (env := ((pos, "push_bool", e) :: (!env)); false)
        | Second env ->
            (match List.assoc pos (!env) with
             | Bool e -> e
             | _ -> assert false)
      with | Stack.Empty  -> raise (Give_up "Illegal anti-quotation")
    let push_bool pos e =
      match Stack.top quote_stack with
      | First env -> assert false
      | Second env -> env := ((pos, (Bool e)) :: (!env))
    let localise _loc e =
      let len = String.length e in
      if (len = 0) || ((e.[0]) = '#')
      then e
      else
        (let cols =
           let n =
             let open Location in
               let open Lexing in
                 (_loc.loc_start).pos_cnum - (_loc.loc_start).pos_bol in
           String.make (n + 1) ' ' in
         ((let open Location in
             let open Lexing in
               Printf.sprintf "#%d %S\n%s" ((_loc.loc_start).pos_lnum - 1)
                 (_loc.loc_start).pos_fname) cols)
           ^ e)
    let loc_none =
      let loc =
        let open Lexing in
          { pos_fname = "none"; pos_lnum = 1; pos_bol = 0; pos_cnum = (-1) } in
      let open Location in
        { loc_start = loc; loc_end = loc; loc_ghost = true }
    let parse_string' g e' =
      try parse_string g blank e'
      with | e -> (Printf.eprintf "Error in quotation: %s\n%!" e'; raise e)
    let quote_expression _loc loc e name =
      Stack.push (empty_quote_env1 ()) quote_stack;
      (let e = localise _loc e in
       let _ =
         match name with
         | "expression" -> ignore (parse_string' expression e)
         | "type" -> ignore (parse_string' typexpr e)
         | "pattern" -> ignore (parse_string' pattern e)
         | "str_item" -> ignore (parse_string' structure_item e)
         | "sig_item" -> ignore (parse_string' signature_item e)
         | "structure" -> ignore (parse_string' structure e)
         | "signature" -> ignore (parse_string' signature e)
         | "constructors" -> ignore (parse_string' constr_decl_list e)
         | "fields" -> ignore (parse_string' field_decl_list e)
         | "cases" -> ignore (parse_string' (match_cases Top) e)
         | "let_binding" -> ignore (parse_string' let_binding e)
         | "module_expr" -> ignore (parse_string' module_expr e)
         | "module_type" -> ignore (parse_string' module_type e)
         | _ -> (Printf.eprintf "unexpected %s\n%!" name; assert false) in
       let env =
         match Stack.pop quote_stack with
         | First e -> e
         | Second _ -> assert false in
       let _loc = loc_none in
       let push_expr =
         loc_expr _loc
           (Pexp_apply
              ((loc_expr _loc
                  (Pexp_ident (id_loc (Ldot ((Lident "Stack"), "push")) _loc))),
                [("",
                   (loc_expr _loc
                      (Pexp_apply
                         ((loc_expr _loc
                             (Pexp_ident
                                (id_loc
                                   (Ldot
                                      ((Lident "Pa_ocaml_prelude"),
                                        "empty_quote_env2")) _loc))),
                           [("",
                              (loc_expr _loc
                                 (pexp_construct
                                    ((id_loc (Lident "()") _loc), None))))]))));
                ("",
                  (loc_expr _loc
                     (Pexp_ident
                        (id_loc
                           (Ldot ((Lident "Pa_ocaml_prelude"), "quote_stack"))
                           _loc))))])) in
       let push_expr =
         List.fold_left
           (fun acc  ->
              fun (pos,name,e)  ->
                let push_e =
                  loc_expr _loc
                    (Pexp_apply
                       ((loc_expr _loc
                           (Pexp_ident
                              (id_loc
                                 (Ldot ((Lident "Pa_ocaml_prelude"), name))
                                 _loc))),
                         [("",
                            (loc_expr _loc (Pexp_constant (Const_int pos))));
                         ("", e)])) in
                loc_expr _loc (Pexp_sequence (acc, push_e))) push_expr (
           !env) in
       let pop_expr =
         loc_expr _loc
           (Pexp_apply
              ((loc_expr _loc (Pexp_ident (id_loc (Lident "ignore") _loc))),
                [("",
                   (loc_expr _loc
                      (Pexp_apply
                         ((loc_expr _loc
                             (Pexp_ident
                                (id_loc (Ldot ((Lident "Stack"), "pop")) _loc))),
                           [("",
                              (loc_expr _loc
                                 (Pexp_ident
                                    (id_loc
                                       (Ldot
                                          ((Lident "Pa_ocaml_prelude"),
                                            "quote_stack")) _loc))))]))))])) in
       let args =
         match loc with
         | None  ->
             [("",
                (loc_expr _loc (Pexp_ident (id_loc (Lident "_loc") _loc))));
             ("", (loc_expr _loc (Pexp_constant (const_string e))))]
         | Some loc ->
             [("", loc);
             ("", (loc_expr _loc (Pexp_constant (const_string e))))] in
       let parse_expr =
         loc_expr _loc
           (Pexp_apply
              ((loc_expr _loc
                  (Pexp_ident
                     (id_loc
                        (Ldot
                           ((Lident "Pa_ocaml_prelude"),
                             ("quote_" ^ (name ^ "_2")))) _loc))), args)) in
       loc_expr _loc
         (Pexp_sequence
            (push_expr,
              (loc_expr _loc
                 (Pexp_let
                    (Nonrecursive,
                      [value_binding _loc
                         (loc_pat _loc (Ppat_var (id_loc "quote_res" _loc)))
                         parse_expr],
                      (loc_expr _loc
                         (Pexp_sequence
                            (pop_expr,
                              (loc_expr _loc
                                 (Pexp_ident
                                    (id_loc (Lident "quote_res") _loc))))))))))))
    let quote_expression_2 loc e =
      loc_expr loc (parse_string' expression e).pexp_desc
    let quote_type_2 loc e = loc_typ loc (parse_string' typexpr e).ptyp_desc
    let quote_pattern_2 loc e =
      loc_pat loc (parse_string' pattern e).ppat_desc
    let quote_str_item_2 loc e = parse_string' structure_item e
    let quote_sig_item_2 loc e = parse_string' signature_item e
    let quote_structure_2 loc e = parse_string' structure e
    let quote_signature_2 loc e = parse_string' signature e
    let quote_constructors_2 loc e = parse_string' constr_decl_list e
    let quote_fields_2 loc e = parse_string' field_decl_list e
    let quote_let_binding_2 loc e = parse_string' let_binding e
    let quote_module_expr_2 loc e =
      mexpr_loc loc (parse_string' module_expr e).pmod_desc
    let quote_module_type_2 loc e =
      mtyp_loc loc (parse_string' module_type e).pmty_desc
    let quote_cases_2 loc e = parse_string' (match_cases Top) e
    let par_re s = "\\(" ^ (s ^ "\\)")
    let union_re l =
      let l = List.map (fun s  -> par_re s) l in String.concat "\\|" l
    let lident_re = "\\([a-z][a-zA-Z0-9_']*\\)\\|\\([_][a-zA-Z0-9_']+\\)"
    let cident_re = "[A-Z][a-zA-Z0-9_']*"
    let ident_re = "[A-Za-z_][a-zA-Z0-9_']*"
    let make_reserved l =
      let reserved =
        ref (List.sort (fun s  -> fun s'  -> - (compare s s')) l) in
      let re_from_list l =
        Str.regexp
          (String.concat "\\|"
             (List.map (fun s  -> "\\(" ^ ((Str.quote s) ^ "\\)")) l)) in
      let re = ref (re_from_list (!reserved)) in
      ((fun s  ->
          (Str.string_match (!re) s 0) &&
            ((Str.match_end ()) = (String.length s))),
        (fun s  ->
           reserved :=
             (List.sort (fun s  -> fun s'  -> - (compare s s')) (s ::
                (!reserved)));
           re := (re_from_list (!reserved))))
    let (is_reserved_id,add_reserved_id) =
      make_reserved
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
        "function";
        "functor";
        "fun";
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
    let ident =
      Decap.alternatives
        [Decap.apply
           (fun id  ->
              if is_reserved_id id
              then raise (Give_up (id ^ " is a keyword..."));
              id)
           (Decap.regexp ~name:"ident" ident_re (fun groupe  -> groupe 0));
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "ident" "ident")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              push_pop_string
                                (start_pos _loc_dol).Lexing.pos_cnum e))))]
    let capitalized_ident =
      Decap.alternatives
        [Decap.apply (fun id  -> id)
           (Decap.regexp ~name:"cident" cident_re (fun groupe  -> groupe 0));
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "uid" "uid")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              push_pop_string
                                (start_pos _loc_dol).Lexing.pos_cnum e))))]
    let lowercase_ident =
      Decap.alternatives
        [Decap.apply
           (fun id  ->
              let len = String.length id in
              (try
                 if (len >= 4) && ((String.sub id 0 4) = "_loc")
                 then
                   let id' =
                     if len = 4
                     then ""
                     else
                       if (len > 4) && ((id.[4]) = '_')
                       then String.sub id 5 ((String.length id) - 5)
                       else raise Exit in
                   push_location id'
               with | Exit  -> ());
              if is_reserved_id id
              then raise (Give_up (id ^ " is a keyword..."));
              id)
           (Decap.regexp ~name:"lident" lident_re (fun groupe  -> groupe 0));
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "lid" "lid")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              push_pop_string
                                (start_pos _loc_dol).Lexing.pos_cnum e))))]
    let (is_reserved_symb,add_reserved_symb) =
      make_reserved
        ["#";
        "'";
        "(";
        ")";
        ",";
        "->";
        ".";
        "..";
        ":";
        ":>";
        ";";
        ";;";
        "<-";
        ">]";
        ">}";
        "?";
        "[";
        "[<";
        "[>";
        "[|";
        "]";
        "_";
        "`";
        "{";
        "{<";
        "|";
        "|]";
        "}";
        "~"]
    let infix_symb_re =
      union_re
        ["[=<>@^|&+*/$%-][!$%&*+./:<=>?@^|~-]*";
        "!=";
        "::";
        ":=";
        "mod" ^ "\\b";
        "land" ^ "\\b";
        "lor" ^ "\\b";
        "or" ^ "\\b";
        "lxor" ^ "\\b";
        "lsl" ^ "\\b";
        "lsr" ^ "\\b";
        "asr" ^ "\\b"]
    let prefix_symb_re =
      "\\([!][!$%&*+./:<=>?@^|~-]*\\)\\|\\([~?][!$%&*+./:<=>?@^|~-]+\\)\\|\\([-+][.]?\\)"
    let infix_symbol =
      Decap.apply
        (fun sym  ->
           if is_reserved_symb sym
           then
             raise (Give_up ("The infix sybol " ^ (sym ^ "is reserved...")));
           sym)
        (Decap.regexp ~name:"infix_symb" infix_symb_re
           (fun groupe  -> groupe 0))
    let prefix_symbol =
      Decap.apply
        (fun sym  ->
           if (is_reserved_symb sym) || (sym = "!=")
           then
             raise
               (Give_up ("The prefix symbol " ^ (sym ^ "is reserved...")));
           sym)
        (Decap.regexp ~name:"prefix_symb" prefix_symb_re
           (fun groupe  -> groupe 0))
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
                if c <> (s.[i])
                then
                  raise (Give_up ("The keyword " ^ (s ^ " was expected...")));
                str' := _str';
                pos' := _pos')
             done;
             (let str' = !str'
              and pos' = !pos' in
              let (c,_,_) = read str' pos' in
              match c with
              | 'a'..'z'|'A'..'Z'|'0'..'9'|'_'|'\'' ->
                  raise (Give_up ("The keyword " ^ (s ^ " was expected...")))
              | _ -> ((), str', pos'))) (Charset.singleton (s.[0])) false s
    let mutable_kw = key_word "mutable"
    let mutable_flag =
      Decap.alternatives
        [Decap.apply (fun _  -> Mutable) mutable_kw;
        Decap.apply (fun _  -> Immutable) (Decap.empty ())]
    let private_kw = key_word "private"
    let private_flag =
      Decap.alternatives
        [Decap.apply (fun _  -> Private) private_kw;
        Decap.apply (fun _  -> Public) (Decap.empty ())]
    let virtual_kw = key_word "virtual"
    let virtual_flag =
      Decap.alternatives
        [Decap.apply (fun _  -> Virtual) virtual_kw;
        Decap.apply (fun _  -> Concrete) (Decap.empty ())]
    let rec_kw = key_word "rec"
    let rec_flag =
      Decap.alternatives
        [Decap.apply (fun _  -> Recursive) rec_kw;
        Decap.apply (fun _  -> Nonrecursive) (Decap.empty ())]
    let to_kw = key_word "to"
    let downto_kw = key_word "downto"
    let downto_flag =
      Decap.alternatives
        [Decap.apply (fun _  -> Upto) to_kw;
        Decap.apply (fun _  -> Downto) downto_kw]
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
    let parser_kw = key_word "parser"
    let char_literal: char grammar = declare_grammar "char_literal"
    let string_literal: string grammar = declare_grammar "string_literal"
    let regexp_literal: string grammar = declare_grammar "regexp_literal"
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
      Decap.alternatives
        [Decap.apply (fun i  -> int_of_string i)
           (Decap.regexp ~name:"int_pos" int_pos_re (fun groupe  -> groupe 0));
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "int" "int")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              push_pop_int
                                (start_pos _loc_dol).Lexing.pos_cnum e))))]
    let int32_lit =
      Decap.alternatives
        [Decap.apply (fun i  -> Int32.of_string i)
           (Decap.regexp ~name:"int32" int32_re (fun groupe  -> groupe 1));
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "int32" "int32")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              push_pop_int32
                                (start_pos _loc_dol).Lexing.pos_cnum e))))]
    let int64_lit =
      Decap.alternatives
        [Decap.apply (fun i  -> Int64.of_string i)
           (Decap.regexp ~name:"int64" int64_re (fun groupe  -> groupe 1));
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "int64" "int64")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              push_pop_int64
                                (start_pos _loc_dol).Lexing.pos_cnum e))))]
    let nat_int_lit =
      Decap.alternatives
        [Decap.apply (fun i  -> Nativeint.of_string i)
           (Decap.regexp ~name:"natint" natint_re (fun groupe  -> groupe 1));
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "natint" "natint")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              push_pop_natint
                                (start_pos _loc_dol).Lexing.pos_cnum e))))]
    let bool_lit =
      Decap.alternatives
        [Decap.apply (fun _  -> "false") false_kw;
        Decap.apply (fun _  -> "true") true_kw;
        Decap.fsequence
          (Decap.apply_position
             (fun x  ->
                fun str  ->
                  fun pos  ->
                    fun str'  -> fun pos'  -> ((locate str pos str' pos'), x))
             (Decap.ignore_next_blank (Decap.char '$' '$')))
          (Decap.fsequence (Decap.string "bool" "bool")
             (Decap.fsequence (Decap.char ':' ':')
                (Decap.sequence
                   (Decap.ignore_next_blank (expression_lvl App))
                   (Decap.char '$' '$')
                   (fun e  ->
                      fun _  ->
                        fun _  ->
                          fun _  ->
                            fun dol  ->
                              let (_loc_dol,dol) = dol in
                              if
                                push_pop_bool
                                  (start_pos _loc_dol).Lexing.pos_cnum e
                              then "true"
                              else "false"))))]
    let entry_points:
      (string*
        [ `Impl of Parsetree.structure_item list Decap.grammar
        | `Intf of Parsetree.signature_item list Decap.grammar | `Top]) list
        ref
      = ref [(".mli", (`Intf signature)); (".ml", (`Impl structure))]
  end
module type Extension  = module type of Initial
module type FExt  = functor (E : Extension) -> Extension
let extensions_mod = ref ([] : (module FExt) list)
let register_extension e = extensions_mod := (e :: (!extensions_mod))
include Initial
