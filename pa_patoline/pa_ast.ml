open Asttypes
open Parsetree
open Longident
let longident_of_string s =
  match Longident.unflatten (String.split_on_char '.' s) with
  | Some li -> li
  | None -> invalid_arg ("Pa_ast.longident_of_string: " ^ s)

(** P2-5: Parsetree compat.  OCaml 5.4 wraps [string] in [Longident.Lident]
    and [Longident.Ldot] as [string loc] and parents of [Ldot] as
    [t loc].  Wrap a bare [Longident.t] into a fully location-annotated
    one. *)
let loc_id loc (s : string) : string loc = { txt = s; loc }
let rec longident_loc (loc : Location.t) (li : Longident.t loc) : Longident.t loc =
  match li.txt with
  | Longident.Lident s ->
      { txt = Longident.Lident s; loc }
  | Longident.Ldot (parent, s) ->
      { txt = Longident.Ldot (longident_loc loc parent, loc_id loc s.txt); loc }
  | Longident.Lapply (a, b) ->
      { txt = Longident.Lapply (longident_loc loc a, longident_loc loc b); loc }

(* Bridge for pa_ocaml.ml's [String.concat "." (id :: l)] style
   dotted-string builders used to synthesize module paths from a
   list of segments.  OCaml 5.4's Longident.t carries per-component
   locs, so we materialize a fresh dummy location for each segment. *)
let dotted_to_longident (s : string) : Longident.t =
  let rec loop i =
    match String.index_from_opt s i '.' with
    | None ->
        Longident.Lident (String.sub s i (String.length s - i))
    | Some j ->
        let head = String.sub s i (j - i) in
        let tail = loop (j + 1) in
        Longident.Ldot (Location.mknoloc tail, Location.mknoloc head)
  in
  loop 0

(* The OCaml-4 era id_loc returned a polymorphic record
   [{txt; loc}].  OCaml 5.4 preserves this row polymorphism: callers
   determine whether the record is [string loc] or [Longident.t loc].
   This restores the original behaviour that the Earley grammars in
   pa_ocaml.ml rely on. *)
type 'a label = 'a Asttypes.loc
let id_loc txt loc = { Asttypes.txt; loc }
(* Convenience for the few call sites that need an explicit Longident
   conversion. *)
let id_loc_longident li loc = { Asttypes.txt = li; loc }
let longident_loc_to_string (l : Longident.t loc) : string loc =
  match l.txt with
  | Longident.Lident s -> { txt = s; loc = l.loc }
  | _ -> { txt = "_"; loc = l.loc }
let longident_loc_of_string (l : string loc) : Longident.t loc =
  { txt = Longident.Lident l.txt; loc = l.loc }
let id_loc_string (s : string) (loc : Location.t) : Longident.t loc =
  { txt = Longident.Lident s; loc }
let id_loc_string_opt (o : string option) (loc : Location.t) : Longident.t loc =
  match o with | Some s -> id_loc_string s loc | None -> id_loc_string "_" loc
let id_loc_str_opt (o : string option) (loc : Location.t) : string Asttypes.loc =
  match o with
  | Some s -> { txt = s; loc }
  | None -> { txt = "_"; loc }
let loc_id loc (txt : string) : string loc = { txt; loc }
let loc_id_loc loc (s : string) : string loc = { txt = s; loc }

let loc_str _loc desc = { pstr_desc = desc; pstr_loc = _loc }
let loc_sig _loc desc = { psig_desc = desc; psig_loc = _loc }
let loc_expr ?(attributes= [])  _loc e =
  {
    pexp_desc = e;
    pexp_loc = _loc;
    pexp_attributes = attributes;
    pexp_loc_stack = []
  }
let loc_pat ?(attributes= [])  _loc pat =
  {
    ppat_desc = pat;
    ppat_loc = _loc;
    ppat_attributes = attributes;
    ppat_loc_stack = []
  }
let loc_pcl ?(attributes= [])  _loc desc =
  { pcl_desc = desc; pcl_loc = _loc; pcl_attributes = attributes }
let loc_typ ?(attributes= [])  _loc typ =
  {
    ptyp_desc = typ;
    ptyp_loc = _loc;
    ptyp_attributes = attributes;
    ptyp_loc_stack = []
  }
let pctf_loc ?(attributes= [])  _loc desc =
  { pctf_desc = desc; pctf_loc = _loc; pctf_attributes = attributes }
let pcty_loc ?(attributes= [])  _loc desc =
  { pcty_desc = desc; pcty_loc = _loc; pcty_attributes = attributes }
let loc_pcf ?(attributes= [])  _loc desc =
  { pcf_desc = desc; pcf_loc = _loc; pcf_attributes = attributes }
let mexpr_loc ?(attributes= [])  _loc desc =
  { pmod_desc = desc; pmod_loc = _loc; pmod_attributes = attributes }
let mtyp_loc ?(attributes= [])  _loc desc =
  { pmty_desc = desc; pmty_loc = _loc; pmty_attributes = attributes }
let pexp_construct (a, b) = Pexp_construct (a, b)

(** P2-5: Parsetree compatibility shim.  OCaml 5.2 removed the
    [Pexp_fun] constructor and merged it into the [Pexp_function]
    constructor (now taking a list of [function_param] plus an optional
    type constraint plus a [function_body]).  Wrap a single-argument
    [fun P -> E] so the rest of the vendored parser/ast code keeps
    using the four-tuple signature it was written against. *)
let pexp_fun (label, opt, pat, expr) =
  Pexp_function
    ([{ pparam_loc = Location.none;
        pparam_desc = Pparam_val (label, opt, pat) }],
     None,
     Pfunction_body expr)
let ghost loc = let open Location in { loc with loc_ghost = true }
let no_ghost loc = let open Location in { loc with loc_ghost = false }
let de_ghost e = loc_expr (no_ghost e.pexp_loc) e.pexp_desc
let loc_id loc txt = { txt; loc }
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
let const_string ~loc  s = Ast_helper.Const.mk (Pconst_string (s, loc, None))
let exp_string loc s = loc_expr loc (Pexp_constant (const_string ~loc s))
let const_float s = Pconst_float (s, None)
let const_char s = Pconst_char s
let const_int s = Pconst_integer ((string_of_int s), None)
let const_int32 s = Pconst_integer ((Int32.to_string s), (Some 'l'))
let const_int64 s = Pconst_integer ((Int64.to_string s), (Some 'L'))
let const_nativeint s = Pconst_integer ((Nativeint.to_string s), (Some 'n'))
let exp_int _loc i = loc_expr _loc (Pexp_constant (Ast_helper.Const.integer (string_of_int i)))
let exp_char _loc c = loc_expr _loc (Pexp_constant (Ast_helper.Const.char c))
let exp_float _loc f = loc_expr _loc (Pexp_constant (Ast_helper.Const.float f))
let exp_int32 _loc i =
  loc_expr _loc
    (Pexp_constant (Ast_helper.Const.int32 i))
let exp_int64 _loc i =
  loc_expr _loc
    (Pexp_constant (Ast_helper.Const.int64 i))
let exp_nativeint _loc i =
  loc_expr _loc
    (Pexp_constant (Ast_helper.Const.nativeint i))
let exp_const _loc c es =
  let c = id_loc c _loc in
  loc_expr _loc (pexp_construct (longident_loc_of_string c, es))
let exp_const_longident _loc (c : Longident.t loc) es =
  loc_expr _loc (pexp_construct (c, es))
let exp_record _loc fs =
  let f (l, e) = ((longident_loc_of_string (id_loc l _loc)), e) in
  let fs = List.map f fs in loc_expr _loc (Pexp_record (fs, None))
let exp_None _loc =
  let cnone = id_loc_longident (Longident.Lident "None") _loc in
  loc_expr _loc (pexp_construct (cnone, None))
let exp_Some _loc a =
  let csome = id_loc_longident (Lident "Some") _loc in
  loc_expr _loc (pexp_construct (csome, (Some a)))
let exp_option _loc =
  function | None -> exp_None _loc | Some e -> exp_Some _loc e
let exp_unit _loc =
  let cunit = id_loc_longident (Lident "()") _loc in
  loc_expr _loc (pexp_construct (cunit, None))
let exp_tuple _loc l =
  match l with
  | [] -> exp_unit _loc
  | e::[] -> e
  | _ -> loc_expr _loc (Pexp_tuple (List.map (fun e -> (None, e)) l))
let exp_array _loc l = loc_expr _loc (Pexp_array l)
let exp_Nil _loc =
  let cnil = id_loc_longident (Lident "[]") _loc in
  loc_expr _loc (pexp_construct (cnil, None))
let exp_true _loc =
  let ctrue = id_loc_longident (Lident "true") _loc in
  loc_expr _loc (pexp_construct (ctrue, None))
let exp_false _loc =
  let cfalse = id_loc_longident (Lident "false") _loc in
  loc_expr _loc (pexp_construct (cfalse, None))
let exp_bool _loc b = if b then exp_true _loc else exp_false _loc
let exp_Cons _loc a l =
  loc_expr _loc
    (pexp_construct
       ((id_loc_longident (Lident "::") _loc), (Some (exp_tuple _loc [a; l]))))
let exp_list _loc l = List.fold_right (exp_Cons _loc) l (exp_Nil _loc)
let exp_ident _loc id = loc_expr _loc (Pexp_ident (id_loc_longident (Lident id) _loc))
let exp_lident _loc id = loc_expr _loc (Pexp_ident (longident_loc_of_string (id_loc id _loc)))
let exp_ident_longident _loc (li : Longident.t loc) = loc_expr _loc (Pexp_ident li)
let pat_ident _loc id = loc_pat _loc (Ppat_var (loc_id _loc id))
let pat_array _loc l = loc_pat _loc (Ppat_array l)
let typ_unit _loc =
  loc_typ _loc (Ptyp_constr ((id_loc_longident (Lident "unit") _loc), []))
let typ_tuple _loc l =
  match l with
  | [] -> typ_unit _loc
  | t::[] -> t
  | _ -> loc_typ _loc (Ptyp_tuple (List.map (fun t -> (None, t)) l))
let nolabel = Nolabel
let labelled s = Labelled s
let optional s = Optional s
let exp_apply _loc f l =
  loc_expr _loc (Pexp_apply (f, (List.map (fun x -> (nolabel, x)) l)))
let exp_apply1 _loc f x = loc_expr _loc (Pexp_apply (f, [(nolabel, x)]))
let exp_apply2 _loc f x y =
  loc_expr _loc (Pexp_apply (f, [(nolabel, x); (nolabel, y)]))
let exp_Some_fun _loc =
  loc_expr _loc
    (pexp_fun
       (nolabel, None, (pat_ident _loc "x"),
         (exp_Some _loc (exp_ident _loc "x"))))
let exp_fun _loc id e =
  loc_expr _loc (pexp_fun (nolabel, None, (pat_ident _loc id), e))
let exp_lab_apply _loc f l = loc_expr _loc (Pexp_apply (f, l))
let exp_app _loc =
  exp_fun _loc "x"
    (exp_fun _loc "y"
       (exp_apply _loc (exp_ident _loc "y") [exp_ident _loc "x"]))
let exp_glr_fun _loc f =
  loc_expr _loc
    (Pexp_ident
       (id_loc_longident
          (Ldot
             (Location.mknoloc
                (Ldot
                   (Location.mknoloc (Longident.Lident "Earley_core"),
                    Location.mknoloc "Earley")),
              Location.mknoloc f))
          _loc))
let exp_glrstr_fun _loc f =
  loc_expr _loc
    (Pexp_ident
       (id_loc_longident
          (Ldot
             (Location.mknoloc (Longident.Lident "Earley_str"),
              Location.mknoloc f))
          _loc))
let exp_list_fun _loc f =
  loc_expr _loc
    (Pexp_ident
       (id_loc_longident
          (Ldot
             (Location.mknoloc (Longident.Lident "List"),
              Location.mknoloc f))
          _loc))
let exp_str_fun _loc f =
  loc_expr _loc
    (Pexp_ident
       (id_loc_longident
          (Ldot
             (Location.mknoloc (Longident.Lident "Str"),
              Location.mknoloc f))
          _loc))
let exp_prelude_fun _loc f =
  loc_expr _loc
    (Pexp_ident
       (id_loc_longident
          (Ldot
             (Location.mknoloc (Longident.Lident "Pa_ocaml_prelude"),
              Location.mknoloc f))
          _loc))
let exp_location_fun _loc f =
  loc_expr _loc
    (Pexp_ident
       (id_loc_longident
          (Ldot
             (Location.mknoloc (Longident.Lident "Location"),
              Location.mknoloc f))
          _loc))
let exp_Cons_fun _loc =
  exp_fun _loc "x"
    (exp_fun _loc "l"
       (exp_Cons _loc (exp_ident _loc "x") (exp_ident _loc "l")))
let exp_Cons_rev_fun _loc =
  exp_fun _loc "x"
    (exp_fun _loc "l"
       (exp_Cons _loc (exp_ident _loc "x")
          (exp_apply _loc (exp_list_fun _loc "rev") [exp_ident _loc "l"])))
let exp_apply_fun _loc =
  exp_fun _loc "a"
    (exp_fun _loc "f"
       (exp_apply _loc (exp_ident _loc "f") [exp_ident _loc "a"]))
let ppat_alias _loc p id =
  if id = "_" then p else loc_pat _loc (Ppat_alias (p, (loc_id _loc id)))
let constructor_declaration ?(attributes= [])  _loc name args res =
  {
    pcd_name = name;
    pcd_vars = [];
    pcd_args = args;
    pcd_res = res;
    pcd_attributes = attributes;
    pcd_loc = _loc
  }
let label_declaration ?(attributes= [])  _loc name mut ty =
  {
    pld_name = name;
    pld_mutable = mut;
    pld_type = ty;
    pld_attributes = attributes;
    pld_loc = _loc
  }
type tpar =
  | Joker of Location.t 
  | Name of string loc 
let params_map params =
  let fn (name, var) =
    let v = (var, NoInjectivity) in
    match name with
    | Joker _loc -> ((loc_typ _loc Ptyp_any), v)
    | Name name -> ((loc_typ name.loc (Ptyp_var (name.txt))), v) in
  List.map fn params
let type_declaration ?(attributes= [])  _loc name params cstrs kind priv
  manifest =
  let params = params_map params in
  {
    ptype_name = name;
    ptype_params = params;
    ptype_cstrs = cstrs;
    ptype_kind = kind;
    ptype_private = priv;
    ptype_manifest = manifest;
    ptype_attributes = attributes;
    ptype_loc = _loc
  }
let class_type_declaration ?(attributes= [])  _loc name params virt expr =
  let params = params_map params in
  {
    pci_params = params;
    pci_virt = virt;
    pci_name = name;
    pci_expr = expr;
    pci_attributes = attributes;
    pci_loc = _loc
  }
let pstr_eval e = Pstr_eval (e, [])
let psig_value ?(attributes= [])  _loc name ty prim =
  Psig_value
    {
      pval_name = name;
      pval_type = ty;
      pval_prim = prim;
      pval_attributes = attributes;
      pval_loc = _loc
    }
let value_binding ?(attributes= [])  _loc pat expr =
  {
    pvb_pat = pat;
    pvb_expr = expr;
    pvb_constraint = None;
    pvb_attributes = attributes;
    pvb_loc = _loc
  }
let module_binding _loc name mt me =
  let me =
    match mt with
    | None -> me
    | Some mt -> mexpr_loc _loc (Pmod_constraint (me, mt)) in
  { pmb_name = name; pmb_expr = me; pmb_attributes = []; pmb_loc = _loc }
let module_declaration ?(attributes= [])  _loc name mt =
  {
    pmd_name = name;
    pmd_type = mt;
    pmd_attributes = attributes;
    pmd_loc = _loc
  }
let ppat_construct (a, b) =
  match b with
  | None -> Ppat_construct (a, None)
  | Some p -> Ppat_construct (a, (Some ([], p)))
let pexp_constraint (a, b) = Pexp_constraint (a, b)
let pexp_coerce (a, b, c) = Pexp_coerce (a, b, c)
let pexp_assertfalse _loc =
  loc_expr _loc
    (Pexp_assert
       (loc_expr _loc
          (pexp_construct ({ txt = (Lident "false"); loc = _loc }, None))))
let make_case pat expr guard =
  { pc_lhs = pat; pc_rhs = expr; pc_guard = guard }
(** P2-5: Parsetree compat.  In OCaml 5.2+ [Pexp_function] takes a
    list of cases wrapped in [Pfunction_cases], not just a case list.
    Build the appropriate [function_body] here so callers can keep
    passing a plain case list. *)

let pexp_function cases =
  Pexp_function ([], None, Pfunction_cases (cases, Location.none, []))
let pat_unit _loc =
  let unt = id_loc_longident (Lident "()") _loc in
  loc_pat _loc (ppat_construct (unt, None))
let pat_tuple _loc l =
  match l with
  | [] -> pat_unit _loc
  | p::[] -> p
  | _ -> loc_pat _loc (Ppat_tuple (List.map (fun p -> (None, p)) l, Closed))
let pat_list _loc _loc_c l =
  let nil = id_loc_longident (Lident "[]") (ghost _loc_c) in
  let hd = match l with | [] -> assert false | x::_ -> x in
  let cons x xs =
    let cloc = ghost (merge2 x.ppat_loc _loc) in
    let c = id_loc_longident (Lident "::") cloc in
    let cons = ppat_construct (c, (Some (loc_pat cloc (Ppat_tuple ([(None, x); (None, xs)], Closed))))) in
    let loc = if x == hd then _loc else cloc in loc_pat loc cons in
  List.fold_right cons l
    (loc_pat (ghost _loc_c) (ppat_construct (nil, None)))
let ppat_list = pat_list
