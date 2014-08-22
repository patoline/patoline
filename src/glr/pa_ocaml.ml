open Input
open Glr
open Charset
open Asttypes
open Parsetree
open Longident

include Pa_ocaml_prelude

let _ = glr_locate locate merge

module Make = functor (Initial:Extension) -> struct

include Initial

let mk_unary_opp name _loc_name arg _loc_arg =
  let res = 
    match name, arg.pexp_desc with
    | "-", Pexp_constant(Const_int n) ->
       Pexp_constant(Const_int(-n))
    | "-", Pexp_constant(Const_int32 n) ->
       Pexp_constant(Const_int32(Int32.neg n))
    | "-", Pexp_constant(Const_int64 n) ->
       Pexp_constant(Const_int64(Int64.neg n))
    | "-", Pexp_constant(Const_nativeint n) ->
       Pexp_constant(Const_nativeint(Nativeint.neg n))
    | ("-" | "-."), Pexp_constant(Const_float f) ->
       Pexp_constant(Const_float("-" ^ f))
    | "+", Pexp_constant(Const_int _)
    | "+", Pexp_constant(Const_int32 _)
    | "+", Pexp_constant(Const_int64 _)
    | "+", Pexp_constant(Const_nativeint _)
    | ("+" | "+."), Pexp_constant(Const_float _) -> arg.pexp_desc
    | ("-" | "-." | "+" | "+."), _ ->
       let p = loc_expr _loc_name (Pexp_ident { txt = Lident ("~" ^ name); loc = _loc_name}) in
       Pexp_apply(p, ["", arg])
    | _ ->
       let p = loc_expr _loc_name (Pexp_ident { txt = Lident (name); loc = _loc_name}) in
       Pexp_apply(p, ["", arg])
  in
  loc_expr (merge _loc_name _loc_arg) res

let check_variable vl loc v =
  if List.mem v vl then
    raise Syntaxerr.(Error(Variable_in_scope(loc,v)))

let varify_constructors var_names t =
  let rec loop t =
    let desc =
      match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
          check_variable var_names t.ptyp_loc x;
          Ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
          Ptyp_arrow(label, loop core_type, loop core_type')
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Ptyp_constr(longident, lst) ->
          Ptyp_constr(longident, List.map loop lst)
#ifversion >= 4.02
      | Ptyp_object (lst, cl) ->
          Ptyp_object (List.map loop_core_field lst, cl)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_extension(_) as ty -> ty
#else
      | Ptyp_object lst ->
          Ptyp_object (List.map loop_core_field lst)
      | Ptyp_class (longident, lst, lbl_list) ->
          Ptyp_class (longident, List.map loop lst, lbl_list)
#endif
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (check_variable var_names t.ptyp_loc) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
    in
    {t with ptyp_desc = desc}
#ifversion >= 4.02
  and loop_core_field (str, attr, ty) = (str, attr, loop ty)
#else
  and loop_core_field t =
    let desc =
      match t.pfield_desc with
      | Pfield(n,typ) ->
          Pfield(n,loop typ)
      | Pfield_var ->
          Pfield_var
    in
    { t with pfield_desc=desc}
#endif
  and loop_row_field  =
    function
#ifversion >= 4.02
      | Rtag(label,attr,flag,lst) ->
          Rtag(label,attr,flag,List.map loop lst)
#else
      | Rtag(label,flag,lst) ->
          Rtag(label,flag,List.map loop lst)
#endif
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let wrap_type_annotation _loc newtypes core_type body =
  let exp = loc_expr _loc (pexp_constraint(body,core_type)) in
  let exp =
    List.fold_right (fun newtype exp -> loc_expr _loc (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  (exp, loc_typ _loc (Ptyp_poly(newtypes,varify_constructors newtypes core_type)))

(* Floating-point literals *)
let float_lit_dec    = "[0-9][0-9_]*[.][0-9_]*\\([eE][+-][0-9][0-9_]*\\)?"
let float_lit_no_dec = "[0-9][0-9_]*[eE][+-][0-9][0-9_]*"
let float_re = union_re [float_lit_dec; float_lit_no_dec]

let float_literal =
  glr
    f:RE(float_re)   -> f
  | CHR('$') STR("float") CHR(':') e:expression CHR('$') -> string_of_float (push_pop_float e)  end

(* Character literals *)
let char_regular = "[^\\']"
let string_regular = "[^\\\"]"
let char_escaped = "[\\\\][\\\\\\\"\\\'ntbrs ]"
let char_dec     = "[\\\\][0-9][0-9][0-9]"
let char_hex     = "[\\\\][x][0-9a-fA-F][0-9a-fA-F]"

exception Illegal_escape of string

let one_char is_char =
  glr
    c:RE(char_regular) when is_char -> c.[0]
  | c:CHR('\n') -> '\n' 
  | c:RE(string_regular) when not is_char -> c.[0]
  | c:RE(char_escaped) -> (match c.[1] with
                            | 'n' -> '\n'
                            | 't' -> '\t'
                            | 'b' -> '\b'
                            | 'r' -> '\r'
                            | 's' -> ' '
                            | c   -> c)
  | c:RE(char_dec)     -> (let str = String.sub c 1 3 in
                           let i = Scanf.sscanf str "%i" (fun i -> i) in
                           if i > 255 then
                             raise (Illegal_escape str)
                           else char_of_int i)
  | c:RE(char_hex)     -> (let str = String.sub c 2 2 in
                           let str' = String.concat "" ["0x"; str] in
                           let i = Scanf.sscanf str' "%i" (fun i -> i) in
                           char_of_int i)
  end

let char_literal =
  glr
    r:(change_layout (
      glr CHR('\'') c:(one_char true) CHR('\'') -> c end
    ) no_blank) -> r
  | CHR('$') STR("char") CHR(':') e:expression CHR('$') -> push_pop_char e  
  end

(* String literals *)
let interspace = "[ \t]*"

let string_literal =
  let char_list_to_string lc =
    let len = List.length lc in
#ifversion >= 4.02
    let str = Bytes.create len in
#else
    let str = String.create len in
#endif
    let ptr = ref lc in
    for i = 0 to len - 1 do
      match !ptr with
	[] -> assert false
      | x::l -> str.[i] <- x; ptr := l
    done;
#ifversion >= 4.02
    Bytes.unsafe_to_string str
#else
    str
#endif
  in
  glr 
    r:(change_layout (
    glr
      CHR('"') lc:(one_char false)*
        lcs:(glr CHR('\\') CHR('\n') RE(interspace) lc:(one_char false)* -> lc end)*
        CHR('"') -> char_list_to_string (List.flatten (lc::lcs))
    end) no_blank) -> r
  | CHR('$') STR("string") CHR(':') e:expression CHR('$') -> push_pop_string e
  end

(* Naming labels *)
let label_name = lowercase_ident

let label =
  glr
  | STR("~") ln:label_name -> ln
  end

let opt_label =
  glr
  | STR("?") ln:label_name -> ln
  end

let maybe_opt_label =
  glr
  | o:STR("?")? ln:label_name ->
      (if o = None then ln else ("?" ^ ln))
  end

(* Prefix and infix symbols *)
let reserved_symbols =
  [ "#" ; "'" ; "(" ; ")" ; "," ; "->" ; "." ; ".." ; ":" ; ":>" ; ";" ; ";;" ; "<-"
  ; ">]" ; ">}" ; "?" ; "[" ; "[<" ; "[>" ; "[|" ; "]" ; "_" ; "`" ; "{" ; "{<" ; "|" ; "|]" ; "}" ; "~" ]

let is_reserved_symb s =
  List.mem s reserved_symbols

let infix_symb_re  = union_re [
 "[=<>@^|&+*/$%-][!$%&*+./:<=>?@^|~-]*";
 "!="; "::"; ":=";
 "mod" ^ "\\b";
 "land" ^ "\\b";
 "lor" ^ "\\b";
 "or" ^ "\\b";
 "lxor" ^ "\\b";
 "lsl" ^ "\\b";
 "lsr" ^ "\\b";
 "asr" ^ "\\b"]
let prefix_symb_re = "\\([!-][!$%&*+./:<=>?@^|~-]*\\)\\|\\([~?][!$%&*+./:<=>?@^|~-]+\\)\\|\\(+[.]?\\)"

let infix_symbol =
  glr
    sym:RE(infix_symb_re) -> (if is_reserved_symb sym then raise Give_up; sym)
  end

let prefix_symbol =
  glr
    sym:RE(prefix_symb_re) -> (if is_reserved_symb sym || sym = "!=" then raise Give_up; sym)
  end

(* Line number directives *)
(*
let linenum_directive =
  glr
    STR("#") n:RE("[0-9]+") s:string_literal ->
      let n = Scanf.sscanf n "%i" (fun i -> i) in
      assert false (* TODO *)
  end
*)

(****************************************************************************
 * Names                                                                    *
 ****************************************************************************)
(* Naming objects *)
let infix_op =
  glr
    sym:infix_symbol -> sym
  end

let operator_name =
  glr
    op:prefix_symbol -> op
  | op:infix_op      -> op
  end

let value_name =
  glr
    id:lowercase_ident -> id
  | STR("(") op:operator_name STR(")") -> op
  end

let constr_name     = capitalized_ident  
let tag_name        = 
  glr STR("`") c:ident -> c end

let typeconstr_name = lowercase_ident  
let field_name      = lowercase_ident  
let module_name     = capitalized_ident  
let modtype_name    = ident  
let class_name      = lowercase_ident  
let inst_var_name   = lowercase_ident  
let method_name     = lowercase_ident

let module_path_gen, set_module_path_gen  = grammar_family "module_path_gen"
let module_path_suit, set_module_path_suit  = grammar_family "module_path_suit"

let module_path_suit_aux = memoize1 (fun allow_app ->
  glr
    STR("(") m':(module_path_gen true) STR(")") when allow_app ->
      (fun a -> Lapply(a, m'))
  | STR(".") m:module_name ->
      (fun acc -> Ldot(acc, m))
  end)

let _ = set_module_path_suit (fun allow_app ->
    glr
      f:(module_path_suit_aux allow_app) g:(module_path_suit allow_app) -> (fun acc -> g (f acc))
    else EMPTY -> (fun acc -> acc)
    end) [true; false]

let _ = set_module_path_gen (fun allow_app ->
  glr
  | m:module_name s:(module_path_suit allow_app) -> s (Lident m)
  end) [true; false]

let module_path = module_path_gen false
let extended_module_path = module_path_gen true

let value_path =
  glr
  | mp:{m:module_path STR(".")}? vn:value_name ->
      (match mp with
       | None   -> Lident vn
       | Some p -> Ldot(p, vn))
  end

let constr =
  glr
  | mp:{m:module_path STR(".")}? cn:constr_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))
  end

let typeconstr =
  glr
  | mp:{m:extended_module_path STR(".")}? tcn:typeconstr_name ->
      (match mp with
       | None   -> Lident tcn
       | Some p -> Ldot(p, tcn))
  end

let field =
  glr
  | mp:{m:module_path STR(".")}? fn:field_name ->
      (match mp with
       | None   -> Lident fn
       | Some p -> Ldot(p, fn))
  end

let class_path =
  glr
  | mp:{m:module_path STR(".")}? cn:class_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))
  end

let modtype_path =
  glr
  | mp:{m:extended_module_path STR(".")}? mtn:modtype_name ->
      (match mp with
       | None   -> Lident mtn
       | Some p -> Ldot(p, mtn))
  end

let classtype_path =
  glr
  | mp:{m:extended_module_path STR(".")}? cn:class_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))
  end

let opt_variance =
  glr
  | v:RE("[+-]")? ->
      (match v with
       | None     -> (false, false)
       | Some "+" -> (true , false)
       | Some "-" -> false, true
       | _        -> assert false)
  end

let override_flag =
  glr
    o:STR("!")? -> (if o <> None then Override else Fresh)
  end


(****************************************************************************
 * Type expressions                                                         *
 ****************************************************************************)

let type_prios = [TopType; As; Arr; Prod; DashType; AppType; AtomType]

let next_type_prio = function
  | TopType -> As
  | As -> Arr
  | Arr -> Prod
  | Prod -> DashType
  | DashType -> AppType
  | AppType -> AtomType
  | AtomType -> AtomType

let poly_typexpr =
  glr
  | ids:{STR("'") id:ident}+ STR(".") te:typexpr ->
      loc_typ _loc (Ptyp_poly (ids, te))
  | te:typexpr -> loc_typ _loc (Ptyp_poly ([], te))
  end

let poly_syntax_typexpr =
  glr
  | type_kw ids:typeconstr_name+ STR(".") te:typexpr ->
      (ids, te)
  end
   
let pfield_loc _loc d = { pfield_desc = d; pfield_loc = _loc }
let method_type =
  glr
  | mn:method_name STR(":") pte:poly_typexpr ->
      pfield_loc _loc (Pfield (mn, pte))
  end

let tag_spec =
  glr
  | tn:tag_name te:{of_kw amp:CHR('&')? te:typexpr}? ->
      let amp,t = match te with
              | None   -> true, []
              | Some (amp,l) -> amp<>None, [l]
      in
      Rtag (tn, amp, t)
  | te:typexpr ->
      Rinherit te
  end

let tag_spec_first =
  glr
  | tn:tag_name te:{of_kw amp:CHR('&')? te:typexpr}? ->
      let amp,t = match te with
              | None   -> true,[]
              | Some (amp,l) -> amp<>None, [l]
      in
      [Rtag (tn, amp, t)]
  | te:typexpr? STR("|") ts:tag_spec ->
      match te with
      | None    -> [ts]
      | Some te -> [Rinherit te; ts]
  end

let tag_spec_full =
  glr
  | tn:tag_name (amp,tes):{of_kw amp:STR("&")? te:typexpr
    tes:{STR("&") te:typexpr}* -> (amp<>None,(te::tes))}?[true,[]] ->
		    
      Rtag (tn, amp, tes)
  | te:typexpr ->
      Rinherit te
  end

let polymorphic_variant_type : core_type grammar =
  glr
  | STR("[") tsf:tag_spec_first tss:{STR("|") ts:tag_spec}* STR("]") ->
      loc_typ _loc (Ptyp_variant (tsf @ tss, true, None))
  | STR("[>") ts:tag_spec? tss:{STR("|") ts:tag_spec}* STR("]") ->
      let tss = match ts with
                | None    -> tss
                | Some ts -> ts :: tss
      in
      loc_typ _loc (Ptyp_variant (tss, false, None))
  | STR("[<") STR("|")? tfs:tag_spec_full tfss:{STR("|") tsf:tag_spec_full}*
    tns:{STR(">") tns:tag_name+}?[[]] STR("]") ->
      loc_typ _loc (Ptyp_variant (tfs :: tfss, true, Some tns))
  end

let package_constraint =
  glr
  | type_kw tc:typeconstr CHR('=') te:typexpr ->
      let tc = { txt = tc; loc = _loc_tc } in
      (tc, te)
  end

let package_type =
  glr
  | mtp:modtype_path cs:{with_kw pc:package_constraint
    pcs:{and_kw pc:package_constraint}* -> (pc::pcs)}?[[]] ->
      let mtp = { txt = mtp; loc = _loc_mtp } in
      Ptyp_package (mtp, cs)
  end

let opt_present =
  glr
  | STR("[>") l:tag_name+ STR("]") -> l
  | EMPTY -> []
end

let mkoption loc d =
  let loc = ghost loc in 
  loc_typ loc (Ptyp_constr({ txt = Ldot (Lident "*predef*", "option"); loc = loc},[d]))

let typexpr_base : core_type grammar =
  glr
  | e:(alternatives extra_types) -> e
  | STR("'") id:ident ->
      loc_typ _loc (Ptyp_var id)
  | STR("_") ->
      loc_typ _loc Ptyp_any
  | STR("(") module_kw pt:package_type STR(")") ->
      loc_typ _loc pt
  | STR("(") te:typexpr STR(")") ->
      loc_typ _loc te.ptyp_desc
  | ln:opt_label STR(":") te:(typexpr_lvl (next_type_prio Arr)) STR("->") te':typexpr ->
      loc_typ _loc (Ptyp_arrow ("?" ^ ln, mkoption _loc_te te, te'))
  | ln:label_name STR(":") te:(typexpr_lvl (next_type_prio Arr)) STR("->") te':typexpr ->
      loc_typ _loc (Ptyp_arrow (ln, te, te'))
  | tc:typeconstr ->
      loc_typ _loc (Ptyp_constr ({ txt = tc; loc = _loc_tc }, []))
  | STR("(") te:typexpr tes:{STR(",") te:typexpr}* STR(")") tc:typeconstr ->
      let constr = { txt = tc ; loc = _loc_tc } in
      loc_typ _loc (Ptyp_constr (constr, te::tes))
  | pvt:polymorphic_variant_type -> pvt
  | STR("<") rv:STR("..")? STR(">") ->
#ifversion >= 4.02
      let ml = if rv = None then Closed else Open in
      loc_typ _loc (Ptyp_object([], ml))
#else
      let ml = if rv = None then [] else [pfield_loc _loc_rv Pfield_var] in
      loc_typ _loc (Ptyp_object ml)
#endif
  | STR("<") mt:method_type mts:{STR(";") mt:method_type}*
    rv:{STR(";") rv:STR("..")?}? STR(">") ->
#ifversion >= 4.02
      let ml = if rv = None || rv = Some None then Closed else Open in
      loc_typ _loc (Ptyp_object ((mt :: mts), ml))
#else
      let ml = if rv = None || rv = Some None then [] else [pfield_loc _loc_rv Pfield_var] in
      loc_typ _loc (Ptyp_object (mt :: mts @ ml))
#endif
  | STR("#") cp:class_path o:opt_present ->
      let cp = { txt = cp; loc = _loc_cp } in
      loc_typ _loc (Ptyp_class (cp, [], o))
  | STR("(") te:typexpr tes:{STR(",") te:typexpr}* STR(")")
    STR("#") cp:class_path  o:opt_present ->
      let cp = { txt = cp; loc = _loc_cp } in
      loc_typ _loc (Ptyp_class (cp, te::tes, o))
  | CHR('$') e:(expression_lvl If) CHR('$') -> push_pop_type e
  end

let typexpr_suit_aux : type_prio -> type_prio -> (type_prio * (core_type -> core_type)) grammar = memoize1 (fun lvl' lvl ->
  let ln f _loc e = loc_typ (merge f.ptyp_loc _loc) e in
  glr
  | STR("->") te':(typexpr_lvl Arr) when lvl' > Arr && lvl <= Arr ->
      (Arr, fun te -> ln te _loc (Ptyp_arrow ("", te, te')))
  | tes:{STR("*") te:(typexpr_lvl (next_type_prio Prod))}+  when lvl' > Prod && lvl <= Prod->
      (Prod, fun te -> ln te _loc (Ptyp_tuple (te::tes)))
  | tc:typeconstr when lvl' >= AppType && lvl <= AppType ->
      (AppType, fun te -> ln te _loc (Ptyp_constr ({ txt = tc; loc = _loc_tc }, [te])))
  | as_kw STR("'") id:ident when lvl' >= As && lvl <= As ->
      (As, fun te -> ln te _loc (Ptyp_alias (te, id)))
  | STR("#") cp:class_path o:opt_present when lvl' >= DashType && lvl <= DashType ->
      let cp = { txt = cp; loc = _loc_cp } in
      let tex = fun te ->
        ln te _loc (Ptyp_class (cp, [te], o))
      in (DashType, tex)
  end)

let typexpr_suit =
  let f type_suit =
    memoize2
      (fun lvl' lvl ->
         glr
         | (p1,f1):(typexpr_suit_aux lvl' lvl) ->> (p2,f2):(type_suit p1 lvl) -> (p2, fun f -> f2 (f1 f))
         | EMPTY -> (lvl', fun f -> f) 
         end)
  in
  let rec res x y = f res x y in
  res

let _ = set_typexpr_lvl (fun lvl ->
  glr
  | t:typexpr_base ft:(typexpr_suit AtomType lvl) -> snd ft t
  end) type_prios

(****************************************************************************
 * Type and exception definitions                                           *
 ****************************************************************************)

(* Type definition *)
let type_param =
  glr
  | var:opt_variance CHR('\'') id:ident ->
      (Some { txt = id; loc = _loc }, var)
  | var:opt_variance CHR('_') ->
      (None, var)
  end

let type_params =
  glr
  | tp:type_param -> [tp]
  | STR("(") tp:type_param tps:{STR(",") tp:type_param -> tp}* STR(")") ->
      tp::tps
  end

let type_equation =
  glr
  | CHR('=') p:private_flag te:typexpr -> (p,te)
  end

let type_constraint =
  glr
  | constraint_kw STR("'") id:ident CHR('=') te:typexpr ->
      (loc_typ _loc_id (Ptyp_var id), te, _loc)
  end

let constr_decl =
  let constr_name =
    glr
    | cn:constr_name    -> cn
    | STR("(") STR(")") -> "()"
    end
  in
  glr
  | cn:constr_name (tes,te):{ te:{of_kw te:typexpr}? ->
			      let tes =
				match te with
				| None   -> []
				| Some { ptyp_desc = Ptyp_tuple tes; ptyp_loc = _ } -> tes
				| Some t -> [t]
			      in (tes, None)
			    | CHR(':') ats:{te:(typexpr_lvl (next_type_prio Prod)) tes:{CHR('*') te:(typexpr_lvl (next_type_prio Prod))}*
							     STR("->") -> (te::tes)}?[[]] te:typexpr -> (ats, Some te)}
	    -> (let c = { txt = cn; loc = _loc_cn } in
	        (c, tes, te, _loc_cn))
  end

let field_decl =
  glr
  | m:mutable_flag fn:field_name STR(":") pte:poly_typexpr ->
      ({ txt = fn; loc = _loc_fn }, m, pte, _loc_m)
  end

let type_representation =
  glr
  | STR("|")? cd:constr_decl cds:{STR("|") cd:constr_decl -> cd}* ->
      Ptype_variant (cd::cds)
  | STR("{") fd:field_decl fds:{STR(";") fd:field_decl -> fd}* STR(";")?
    STR("}") ->
      Ptype_record (fd::fds)
  end

let type_information =
  glr
  | te:type_equation? ptr:{CHR('=') pri:private_flag tr:type_representation}?
    cstrs:type_constraint* ->
      let pri, tkind =
        match ptr with
        | None   -> (Public, Ptype_abstract)
        | Some c -> c
      in
      (pri, te, tkind, cstrs)
  end

let typedef_gen : ' a Glr.grammar -> ('a loc * type_declaration) Glr.grammar = (fun constr ->
  glr
  | tps:type_params?[[]] tcn:constr ti:type_information ->
      let (pri, te, tkind, cstrs) = ti in
      let pri, te = match te with
	  None -> pri, None
	| Some(Private, te) -> 
	   if pri = Private then raise Give_up; (* ty = private ty' = private A | B is not legal *) 
	   Private, Some te
	| Some(_, te) -> pri, Some te
      in
      let tdec =
        { ptype_params   = List.map fst tps
        ; ptype_cstrs    = cstrs
        ; ptype_kind     = tkind
        ; ptype_private  = pri
        ; ptype_manifest = te
        ; ptype_variance = List.map snd tps
        ; ptype_loc      = _loc_tps
        }
      in ({ txt = tcn; loc = _loc_tcn }, tdec)
end)

let typedef = typedef_gen typeconstr_name
let typedef_in_constraint = typedef_gen typeconstr


let type_definition =
  glr
  | type_kw td:typedef tds:{and_kw td:typedef -> td}* -> (td::tds)
  end

let exception_declaration =
  glr
  | exception_kw cn:constr_name te:{of_kw te:typexpr}? ->
      (let tes =
        match te with
        | None   -> []
        | Some { ptyp_desc = Ptyp_tuple tes; ptyp_loc = _ } -> tes
        | Some t -> [t]
      in ({ txt = cn; loc = _loc_cn }, tes))
  end

(* Exception definition *)
let exception_definition =
  glr
  | exception_kw cn:constr_name CHR('=') c:constr ->
      (let name = { txt = cn; loc = _loc_cn } in
      let ex = { txt = c; loc = _loc_c } in
      Pstr_exn_rebind (name, ex))
  | (name,ed):exception_declaration ->
      Pstr_exception (name, ed)
  end

(****************************************************************************
 * Classes                                                                  *
 ****************************************************************************)
(* Class types *)
let class_field_spec = declare_grammar "class_field_spec"
let class_body_type = declare_grammar "class_body_type"

let pctf_loc _loc desc = { pctf_desc = desc; pctf_loc = _loc }
let pcty_loc _loc desc = { pcty_desc = desc; pcty_loc = _loc }

let virt_mut = 
  glr
  | v:virtual_flag m:mutable_flag -> (v, m)
  | mutable_kw virtual_kw -> (Virtual, Mutable)
  end

let virt_priv = 
  glr
  | v:virtual_flag p:private_flag -> (v, p)
  | private_kw virtual_kw -> (Virtual, Private)
  end

let _ = set_grammar class_field_spec (
  glr
  | inherit_kw cbt:class_body_type ->
      pctf_loc _loc (Pctf_inher cbt)
  | val_kw (vir,mut):virt_mut ivn:inst_var_name STR(":") te:typexpr ->
      pctf_loc _loc (Pctf_val (ivn, mut, vir, te))
  | method_kw (v,pri):virt_priv mn:method_name STR(":") te:poly_typexpr ->
      (if v = Concrete then
        pctf_loc _loc (Pctf_meth (mn, pri, te))
      else
        pctf_loc _loc (Pctf_virt (mn, pri, te)))
  | constraint_kw te:typexpr CHR('=') te':typexpr ->
      pctf_loc _loc (Pctf_cstr (te, te'))
  end)

let _ = set_grammar class_body_type (
  glr
  | object_kw te:{STR("(") te:typexpr STR(")")}? cfs:class_field_spec*
    end_kw ->
      let self = match te with
                 | None   -> loc_typ _loc Ptyp_any
                 | Some t -> t
      in
      let sign =
        { pcsig_self = self
        ; pcsig_fields = cfs
        ; pcsig_loc = _loc }
      in
      pcty_loc _loc (Pcty_signature sign)
  | tes:{STR("[") te:typexpr tes:{STR(",") te:typexpr}*
    STR("]") -> (te::tes)}?[[]] ctp:classtype_path ->
      let ctp = { txt = ctp; loc = _loc_ctp } in
      pcty_loc _loc (Pcty_constr (ctp, tes))
  end)

let class_type =
  glr
  | tes:{l:maybe_opt_label? STR(":") te:typexpr -> (l, te)}* cbt:class_body_type ->
      let app acc (lab, te) =
        match lab with
        | None   -> pcty_loc _loc (Pcty_fun ("", te, acc))
        | Some l -> pcty_loc _loc (Pcty_fun (l, (if l.[0] = '?' then mkoption _loc_tes te else te), acc))
      in
      List.fold_left app cbt (List.rev tes)
  end

let type_parameters =
  glr
  | i1:type_param l:{ STR(",") i2:type_param }* -> i1::l
  end

(* Class specification *)
let class_spec =
  glr
  | v:virtual_flag tp:{STR("[") tp:type_parameters STR("]")}?[[]]
    cn:class_name STR(":") ct:class_type ->
      let params, variance = List.split tp in
      let params = List.map (function None   -> { txt = ""; loc = _loc}
                                    | Some x -> x) params
      in
      { pci_virt = v
      ; pci_params = params, _loc_tp
      ; pci_name = { txt = cn; loc = _loc_cn }
      ; pci_expr = ct
      ; pci_variance = variance
      ; pci_loc = _loc }
  end

let class_specification =
  glr
  | cs:class_spec css:{and_kw cd:class_spec}* -> (cs::css)
  end

(* Class type definition *)
let classtype_def =
  glr
  | v:virtual_flag tp:{STR("[") tp:type_parameters STR("]")}?[[]] cn:class_name
    CHR('=') cbt:class_body_type ->
      let params, variance = List.split tp in
      let params = List.map (function None   -> { txt = ""; loc = _loc}
                                    | Some x -> x) params
      in
      { pci_virt = v
      ; pci_params = params, _loc_tp
      ; pci_name = { txt = cn; loc = _loc_cn }
      ; pci_expr = cbt
      ; pci_variance = variance
      ; pci_loc = _loc }
  end

let classtype_definition =
  glr
  | type_kw cd:classtype_def cds:{and_kw cd:classtype_def}* ->
      (cd::cds)
  end


(****************************************************************************
 * Constants and Patterns                                                   *
 ****************************************************************************)
(* Constants *)
let constant =
  glr
    f:float_literal   -> Const_float f
  | c:char_literal    -> Const_char c
  | s:string_literal  -> Const_string s
  | i:int32_lit       -> Const_int32 i
  | i:int64_lit       -> Const_int64 i
  | i:nat_int_lit     -> Const_nativeint i
  | i:integer_literal -> Const_int i
  end

(* we do like parser.mly from ocaml: neg_constant for pattern only *)
let neg_constant =
  glr
    {CHR('-') | STR("-.")} f:float_literal -> Const_float ("-"^f)
  | CHR('-') i:int32_lit       -> Const_int32 (Int32.neg i)
  | CHR('-') i:int64_lit       -> Const_int64 (Int64.neg i)
  | CHR('-') i:nat_int_lit     -> Const_nativeint (Nativeint.neg i)
  | CHR('-') i:integer_literal -> Const_int (-i)
  end

(* Patterns *)

let pattern_prios = [ TopPat ; AsPat ; AltPat ; TupPat ; ConsPat ; ConstrPat
                    ; AtomPat ]

let next_pat_prio = function
    TopPat -> AsPat
  | AsPat -> AltPat
  | AltPat -> TupPat
  | TupPat -> ConsPat
  | ConsPat -> ConstrPat
  | ConstrPat -> AtomPat
  | AtomPat -> AtomPat

let pattern_base = memoize1 (fun lvl ->
  glr
  | e:(alternatives extra_patterns) -> e
  | vn:value_name ->
      (AtomPat, loc_pat _loc (Ppat_var { txt = vn; loc = _loc_vn }))
  | STR("_") ->
      (AtomPat, loc_pat _loc Ppat_any)
  | c1:char_literal STR("..") c2:char_literal ->
      let ic1, ic2 = Char.code c1, Char.code c2 in
      if ic1 > ic2 then assert false; (* FIXME error message invalid range *)
      let const i = Ppat_constant (Const_char (Char.chr i)) in
      let rec range acc a b =
        if a > b then assert false
        else if a = b then a::acc
        else range (a::acc) (a+1) b
      in
      let opts = List.map (fun i -> loc_pat _loc (const i)) (range [] ic1 ic2) in
      (AtomPat, List.fold_left (fun acc o -> loc_pat _loc (Ppat_or(o, acc))) (List.hd opts) (List.tl opts))
  | c:{c:constant | c:neg_constant} ->
      (AtomPat, loc_pat _loc (Ppat_constant c))
  | STR("(") p:pattern STR(")") -> (AtomPat, p)
  | lazy_kw p:(pattern_lvl ConstrPat) when lvl <= ConstrPat ->
      let ast = Ppat_lazy(p) in
      (ConstrPat, loc_pat _loc ast)
  | c:constr p:(pattern_lvl ConstrPat) when lvl <= ConstrPat ->
      let ast = Ppat_construct({ txt = c; loc = _loc_c }, Some p, false) in
      (ConstrPat, loc_pat _loc ast)
  | c:constr ->
      let ast = Ppat_construct({ txt = c; loc = _loc_c }, None, false) in
      (AtomPat, loc_pat _loc ast)
  | b:bool_lit ->
      let fls = { txt = Lident b; loc = _loc } in
      (AtomPat, loc_pat _loc (Ppat_construct (fls, None, false)))
  | c:tag_name p:(pattern_lvl ConstrPat) when lvl <= ConstrPat ->
      (ConstrPat, loc_pat _loc (Ppat_variant (c, Some p)))
  | c:tag_name ->
      (AtomPat, loc_pat _loc (Ppat_variant (c, None)))
  | s:STR("#") t:typeconstr ->
      (AtomPat, loc_pat _loc (Ppat_type { txt = t; loc = _loc_t }))
  | s:STR("{") f:field p:{CHR('=') p:pattern}? fps:{STR(";") f:field
    p:{CHR('=') p:pattern}? -> ({ txt = f; loc = _loc_f }, p)}*
    clsd:{STR(";") STR("_") -> ()}? STR(";")? STR("}") ->
      let all = ({ txt = f; loc = _loc_f },p)::fps in
      let f (lab, pat) =
        match pat with
        | Some p -> (lab, p)
        | None   -> let slab = match lab.txt with
                               | Lident s -> { txt = s; loc = lab.loc }
                               | _        -> assert false (* FIXME *)
                    in (lab, loc_pat lab.loc (Ppat_var slab))
      in
      let all = List.map f all in
      let cl = match clsd with
               | None   -> Closed
               | Some _ -> Open
      in
      (AtomPat, loc_pat _loc (Ppat_record (all, cl)))
  | STR("[") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("]") ->
      let nil = { txt = Lident "[]"; loc = _loc } in
      let cons x xs =
        let c = { txt = Lident "::"; loc = _loc } in
        let cons = Ppat_construct (c, Some (loc_pat _loc (Ppat_tuple [x;xs])), false) in
        loc_pat _loc cons
      in
      (AtomPat, List.fold_right cons (p::ps) (loc_pat _loc (Ppat_construct (nil, None, false))))
  | STR("[") STR("]") ->
      let nil = { txt = Lident "[]"; loc = _loc } in
      (AtomPat, loc_pat _loc (Ppat_construct (nil, None, false)))
  | STR("[|") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("|]") ->
      (AtomPat, loc_pat _loc (Ppat_array (p::ps)))
  | STR("[|") STR("|]") ->
      (AtomPat, loc_pat _loc (Ppat_array []))
  | STR("(") STR(")") ->
      let unt = { txt = Lident "()"; loc = _loc } in
      (AtomPat, loc_pat _loc (Ppat_construct (unt, None, false)))
  | begin_kw end_kw ->
      let unt = { txt = Lident "()"; loc = _loc } in
      (AtomPat, loc_pat _loc (Ppat_construct (unt, None, false)))
  | STR("(") module_kw mn:module_name pt:{STR(":") pt:package_type}? STR(")") ->
      let unpack = Ppat_unpack { txt = mn; loc = _loc_mn } in
      let pat = match pt with
                | None    -> unpack
                | Some pt -> let pt = loc_typ _loc_pt pt in
                             Ppat_constraint (loc_pat _loc_mn unpack, pt)
      in
      (AtomPat, loc_pat _loc pat)
  | CHR('$') e:expression CHR('$') -> (AtomPat, push_pop_pattern e)
 end)

let pattern_suit_aux : pattern_prio -> pattern_prio -> (pattern_prio * (pattern -> pattern)) grammar = memoize1 (fun lvl' lvl ->
  let ln f _loc e = loc_pat (merge f.ppat_loc _loc) e in
  glr
  | as_kw vn:value_name when lvl' >= AsPat && lvl <= AsPat ->
      (AsPat, fun p ->
        ln p _loc (Ppat_alias(p, { txt = vn; loc= _loc_vn })))
  | STR("|") p':(pattern_lvl (next_pat_prio AltPat)) when lvl' >= AltPat && lvl <= AltPat ->
      (AltPat, fun p ->
        ln p _loc (Ppat_or(p, p')))
  | ps:{STR(",") p:(pattern_lvl (next_pat_prio TupPat)) -> p}+ when lvl' > TupPat && lvl <= TupPat ->
      (TupPat, fun p ->
        ln p _loc (Ppat_tuple(p::ps)))
  | c:STR("::") p':(pattern_lvl ConsPat) when lvl' > ConsPat && lvl <= ConsPat ->
      (ConsPat, fun p ->
        let cons = { txt = Lident "::"; loc = _loc_c } in
        let args = loc_pat _loc (Ppat_tuple [p; p']) in
        ln p _loc (Ppat_construct(cons, Some args, false)))
  (* next is just for polymorphic type annotation in let ? *)
  | te:STR(":") ids:{STR("'") id:ident}+ STR(".") te:typexpr when lvl' >= AsPat && lvl <= AsPat ->
      (AsPat, fun p -> 
        ln p _loc (Ppat_constraint(p, loc_typ _loc (Ptyp_poly (ids, te)))))
  | te:STR(":") ty:typexpr when lvl' >= AsPat && lvl <= AsPat ->
      (AsPat, fun p -> 
        ln p _loc (Ppat_constraint(p, ty)))
  end)

let pattern_suit =
  let f pat_suit =
    memoize2
      (fun lvl' lvl ->
         glr
         | (p1,f1):(pattern_suit_aux lvl' lvl) ->> (p2,f2):(pat_suit p1 lvl) -> (p2, fun f -> f2 (f1 f))
         | EMPTY -> (lvl', fun f -> f) 
         end)
  in
  let rec res x y = f res x y in
  res

let _ = set_pattern_lvl (fun lvl ->
  glr
  | (lvl',t):(pattern_base lvl) ->> ft:(pattern_suit lvl' lvl) -> snd ft t
  end) pattern_prios

(****************************************************************************
 * Expressions                                                              *
 ****************************************************************************)

let expression_lvls = [ Top; Let; Seq; Coerce; If; Aff; Tupl; Disj; Conj; Eq; Append; Cons; Sum; Prod; Pow; Opp; App; Dash; Dot; Prefix; Atom]

let let_prio lvl = if !modern then lvl else Let
let let_re = if !modern then "\\(let\\)\\|\\(val\\)\\b" else "let\\b"


type assoc = NoAssoc | Left | Right

let assoc = function
  Prefix | Dot | Dash | Opp -> NoAssoc
| Prod | Sum | Eq -> Left
| _ -> Right

let infix_prio s =
  let s1 = if String.length s > 1 then s.[1] else ' ' in
  match s.[0], s1 with
  | _ when List.mem s ["lsl"; "lsr"; "asr"] -> Pow
  | _ when List.mem s ["mod"; "land"; "lor"; "lxor"] -> Prod
  | _ when List.mem s ["&"; "&&"] -> Conj
  | _ when List.mem s ["or"; "||"] -> Disj
  | _ when List.mem s [":="; "<-"] -> Aff
  | ('*', '*') -> Pow
  | ('*' | '/' | '%'), _ -> Prod
  | ('+' | '-'), _ -> Sum
  | ':', _ -> Cons
  | ('@' | '^'), _ -> Append
  | ('=' | '<' | '>' | '|' | '&' | '$' | '!'), _ -> Eq
  | _ -> Printf.printf "%s\n%!" s; assert false

let prefix_prio s =
  if s = "-" || s = "-." || s = "+" || s = "+." then Opp else Prefix

let array_function loc str name =
  let name = if !fast then "unsafe_" ^ name else name in
  loc_expr loc (Pexp_ident { txt = Ldot(Lident str, name); loc = loc })

let bigarray_function loc str name =
  let name = if !fast then "unsafe_" ^ name else name in
  let lid = Ldot(Ldot(Lident "Bigarray", str), name) in
  loc_expr loc (Pexp_ident { txt = lid; loc = loc })

let untuplify exp =
  match exp.pexp_desc with
  | Pexp_tuple es -> es
  | _             -> [exp]

let bigarray_get loc arr arg =
  let get = if !fast then "unsafe_get" else "get" in
  match untuplify arg with
  | [c1] ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Array1" get,
                       ["", arr; "", c1]))
  | [c1;c2] ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Array2" get,
                       ["", arr; "", c1; "", c2]))
  | [c1;c2;c3] ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Array3" get,
                       ["", arr; "", c1; "", c2; "", c3]))
  | coords ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Genarray" "get",
                       ["", arr; "", loc_expr loc (Pexp_array coords)]))

let bigarray_set loc arr arg newval =
  let set = if !fast then "unsafe_set" else "set" in
  match untuplify arg with
  | [c1] ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Array1" set,
                       ["", arr; "", c1; "", newval]))
  | [c1;c2] ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Array2" set,
                       ["", arr; "", c1; "", c2; "", newval]))
  | [c1;c2;c3] ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Array3" set,
                       ["", arr; "", c1; "", c2; "", c3; "", newval]))
  | coords ->
      loc_expr loc (Pexp_apply(bigarray_function loc "Genarray" "set",
                       ["", arr; "", loc_expr loc (Pexp_array coords); "", newval]))

let constructor =
  glr
  | m:{ m:module_path STR"." }? id:{id:capitalized_ident -> id | b:bool_lit -> b } ->
      match m with
      | None   -> Lident id
      | Some m -> Ldot(m, id)
  end 

let argument =
  glr
  | id:label STR(":") e:(expression_lvl (next_exp App)) -> (id, e)
  | id:opt_label STR(":") e:(expression_lvl (next_exp App)) -> ("?"^id, e)
  | id:label -> (id, loc_expr _loc (Pexp_ident { txt = Lident id; loc = _loc }))
  | id:opt_label -> ("?"^id, loc_expr _loc (Pexp_ident { txt = Lident id; loc = _loc }))
    (* NOTE the "id" in the first position of the couple was not prefixed with a "?". I guess this was a bug. *)
  | e:(expression_lvl (next_exp App)) -> ("", e)
  end

let parameter allow_new_type =
  glr
  | pat:(pattern_lvl AtomPat) -> `Arg ("", None, pat)
  | STR("~") STR("(") id:lowercase_ident t:{ STR":" t:typexpr }? STR")" -> (
      let pat =  loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }) in
      let pat = match t with
      | None   -> pat
      | Some t -> loc_pat _loc (Ppat_constraint (pat, t))
      in
      `Arg (id, None, pat))
  | id:label STR":" pat:pattern -> `Arg (id, None, pat)
  | id:label -> `Arg (id, None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  | STR("?") STR"(" id:lowercase_ident t:{ STR":" t:typexpr -> t }? e:{STR"=" e:expression -> e}? STR")" -> (
      let pat = loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }) in
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge _loc_id _loc_t) (Ppat_constraint(pat,t))
      in `Arg ("?"^id, e, pat))
  | id:opt_label STR":" STR"(" pat:pattern t:{STR(":") t:typexpr}? e:{CHR('=') e:expression}? STR")" -> (
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge _loc_pat _loc_t) (Ppat_constraint(pat,t))
      in `Arg ("?"^id, e, pat))
  | id:opt_label STR":" pat:pattern -> `Arg ("?"^id, None, pat)
  | id:opt_label -> `Arg ("?"^id, None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  | CHR('(') type_kw name:typeconstr_name CHR(')') when allow_new_type -> `Type(name)
     
  end

let apply_params _loc params e =
  let f acc = function
    | `Arg (lbl,opt,pat) ->
      loc_expr _loc (Pexp_function (lbl, opt, [pat, acc]))
    | `Type name -> loc_expr _loc (Pexp_newtype(name,acc))
  in
  List.fold_left f e (List.rev params)

let apply_params_cls _loc params e =
  let f acc = function
    | `Arg (lbl,opt,pat) ->
      loc_pcl _loc (Pcl_fun(lbl, opt, pat, acc))
    | `Type name -> assert false
  in
  List.fold_left f e (List.rev params)

let right_member =
  glr

  | l:{lb:(parameter true)}* ty:{CHR(':') t:typexpr}? CHR('=') e:expression -> 
      let e = match ty with
	None -> e
      | Some ty -> loc_expr _loc (pexp_constraint(e, ty))
      in
      apply_params _loc l e
  end

let _ = set_grammar let_binding (
  glr
  | pat:(pattern_lvl (next_pat_prio AsPat)) e:right_member l:{and_kw l:let_binding}?[[]] ->
      ((pat, e)::l)
  | vn:lowercase_ident CHR(':') ty:poly_typexpr e:right_member l:{and_kw l:let_binding}?[[]] ->
      let pat = loc_pat _loc (Ppat_constraint(
        loc_pat _loc (Ppat_var { txt = vn; loc = _loc_vn }),
        ty))
      in
      (pat, e)::l
  | vn:lowercase_ident CHR(':') (ids,ty):poly_syntax_typexpr e:right_member l:{and_kw l:let_binding}?[[]] ->
    let (e, ty) = wrap_type_annotation _loc ids ty e in 									     
    let pat = loc_pat _loc (Ppat_constraint(
	loc_pat _loc (Ppat_var { txt = vn; loc = _loc_vn }),
        ty))
    in
    (pat, e)::l
  end)

let match_cases = memoize1 (fun lvl ->
  glr
  | l:{STR"|"? pat:pattern w:{when_kw e:expression }? STR"->" e:(expression_lvl lvl) 
      l:{STR"|" pat:pattern  w:{when_kw e:expression }? STR"->" e:(expression_lvl lvl) -> 
         let e = match w with None -> e | Some e' -> loc_expr _loc (Pexp_when(e',e)) in
           (pat,e)}* -> 
             let e = match w with None -> e | Some e' -> loc_expr _loc (Pexp_when(e',e)) in
               ((pat,e)::l)}?[[]] -> l
  end)

let type_coercion =
  glr
  | STR(":") t:typexpr t':{STR(":>") t':typexpr}? -> (Some t, t')
  | STR(":>") t':typexpr -> (None, Some t')
  end

let expression_list =
  glr
  | e:(expression_lvl (next_exp Seq)) l:{ STR(";") e:(expression_lvl (next_exp Seq)) }* STR(";")? -> (e::l)
  | EMPTY -> []
  end

let record_item = 
  glr
  | f:field CHR('=') e:(expression_lvl (next_exp Seq)) -> ({ txt = f; loc = _loc_f},e) 
  | f:lowercase_ident -> (let id = { txt = Lident f; loc = _loc_f} in id, loc_expr _loc_f (Pexp_ident(id)))
  end

let record_list =
  glr
  | it:record_item l:{ STR(";") it:record_item }* STR(";")? -> (it::l)
  | EMPTY -> []
  end

(****************************************************************************
 * classes and objects                                                      *
 ****************************************************************************)

let obj_item = 
  glr 
  | v:inst_var_name CHR('=') e:(expression_lvl (next_exp Seq)) -> ({ txt = v ; loc = _loc_v }, e)
  end

(* Class expression *)

let class_expr_base =
  glr
  | cp:class_path -> 
      let cp = { txt = cp; loc = _loc_cp } in
      loc_pcl _loc (Pcl_constr (cp, []))
  | CHR('[') te:typexpr tes:{STR(",") te:typexpr}* CHR(']') cp:class_path ->
      let cp = { txt = cp; loc = _loc_cp } in
      loc_pcl _loc (Pcl_constr (cp, te :: tes))
  | STR("(") ce:class_expr STR(")") ->
      loc_pcl _loc ce.pcl_desc
  | STR("(") ce:class_expr STR(":") ct:class_type STR(")") ->
      loc_pcl _loc (Pcl_constraint (ce, ct))
  | fun_kw ps:(parameter false)+ STR("->") ce:class_expr ->
      apply_params_cls _loc ps ce
  | let_kw r:rec_flag lbs:let_binding in_kw ce:class_expr ->
      loc_pcl _loc (Pcl_let (r, lbs, ce))
  | object_kw cb:class_body end_kw ->
      loc_pcl _loc (Pcl_structure cb)
  end

let _ = set_grammar class_expr (
  glr
  | ce:class_expr_base args:{arg:argument+}? ->
      (match args with
       | None   -> ce
       | Some l -> loc_pcl _loc (Pcl_apply (ce, l)))
  end)

let class_field =
  let loc_pcf _loc desc = { pcf_desc = desc; pcf_loc = _loc } in
  glr
  | inherit_kw o:override_flag ce:class_expr id:{as_kw id:lowercase_ident}? ->
      loc_pcf _loc (Pcf_inher (o, ce, id))
  | val_kw o:override_flag m:mutable_flag ivn:inst_var_name te:{CHR(':') t:typexpr}?
    CHR('=') e:expr ->
      let ivn = { txt = ivn; loc = _loc_ivn } in
      let ex =
        match te with
        | None   -> e
        | Some t -> { pexp_desc = pexp_constraint (e, t)
                    ; pexp_loc  = _loc_te }
      in
      loc_pcf _loc (Pcf_val (ivn, m, o, ex))
  | val_kw m:mutable_flag virtual_kw ivn:inst_var_name
    STR(":") te:typexpr ->
      let ivn = { txt = ivn; loc = _loc_ivn } in
      loc_pcf _loc (Pcf_valvirt (ivn, m, te))
  | val_kw virtual_kw mutable_kw ivn:inst_var_name STR(":") te:typexpr ->
      let ivn = { txt = ivn; loc = _loc_ivn } in
      loc_pcf _loc (Pcf_valvirt (ivn, Mutable, te))
  | method_kw o:override_flag p:private_flag mn:method_name
    STR(":") te:poly_typexpr CHR('=') e:expr ->
      let mn = { txt = mn; loc = _loc_mn } in
      let e = loc_expr _loc (Pexp_poly (e, Some te)) in
      loc_pcf _loc (Pcf_meth (mn, p, o, e))
  | method_kw o:override_flag p:private_flag mn:method_name
    STR(":") (ids,te):poly_syntax_typexpr CHR('=') e:expr ->
      let mn = { txt = mn; loc = _loc_mn } in
      let e, poly =  wrap_type_annotation _loc ids te e in
      let e = loc_expr _loc (Pexp_poly (e, Some poly)) in
      loc_pcf _loc (Pcf_meth (mn, p, o, e))
  | method_kw o:override_flag p:private_flag mn:method_name ps:(parameter true)*
    te:{STR(":") te:typexpr}? CHR('=') e:expr ->
      let mn = { txt = mn; loc = _loc_mn } in
      let e = 
	match te with
	  None -> e
	| Some te ->
	   loc_expr _loc (pexp_constraint (e, te))
      in
      let e : expression = apply_params _loc ps e in
      let e = loc_expr _loc (Pexp_poly (e, None)) in
      loc_pcf _loc (Pcf_meth (mn, p, o, e))
  | method_kw p:private_flag virtual_kw mn:method_name STR(":")
    pte:poly_typexpr ->
      let mn = { txt = mn ; loc = _loc_mn } in
      loc_pcf _loc (Pcf_virt (mn, p, pte))
  | method_kw virtual_kw private_kw mn:method_name
    STR(":") pte:poly_typexpr ->
      let mn = { txt = mn ; loc = _loc_mn } in
      loc_pcf _loc (Pcf_virt (mn, Private, pte))
  | constraint_kw te:typexpr CHR('=') te':typexpr ->
      loc_pcf _loc (Pcf_constr (te, te'))
  | initializer_kw e:expr ->
      loc_pcf _loc (Pcf_init e)
  end

let _ = set_grammar class_body (
  glr
  | p:pattern? f:class_field* -> 
      let p = match p with None -> loc_pat _loc_p Ppat_any | Some p -> p in
      { pcstr_pat = p; pcstr_fields = f }
  end)

(* Class definition *)
(* FIXME do not know what to do with ps *)
let class_binding =
  glr
  | v:virtual_flag tp:{STR("[") tp:type_parameters STR("]")}?[[]]
    cn:class_name ps:(parameter false)* ct:{STR(":") ct:class_type}? CHR('=')
    ce:class_expr ->
      let params, variance = List.split tp in
      let params = List.map (function None   -> { txt = ""; loc = _loc}
                                    | Some x -> x) params
      in
      let ce = apply_params_cls _loc ps ce in
      let ce = match ct with
               | None    -> ce
               | Some ct -> loc_pcl _loc (Pcl_constraint(ce, ct))
      in
      { pci_virt = v
      ; pci_params = params, _loc_tp
      ; pci_name = { txt = cn; loc = _loc_cn }
      ; pci_expr = ce
      ; pci_variance = variance
      ; pci_loc = _loc }
  end

let class_definition =
  glr
  | cb:class_binding cbs:{and_kw cb:class_binding}* -> (cb::cbs)
  end

let module_expr = declare_grammar "module_expr"
let mexpr_loc _loc desc = { pmod_desc = desc; pmod_loc = _loc }
let module_type = declare_grammar "module_type"
let mtyp_loc _loc desc = { pmty_desc = desc; pmty_loc = _loc }


(* Expressions *)
let expression_base = memoize1 (fun lvl ->
  glr
  | e:(alternatives extra_expressions) -> e
  | v:inst_var_name STR("<-") e:(expression_lvl (next_exp Aff)) when lvl <= Aff->
      (Aff, loc_expr _loc (Pexp_setinstvar({ txt = v ; loc = _loc_v }, e)))
  | id:value_path -> (Atom, loc_expr _loc (Pexp_ident { txt = id; loc = _loc_id }))
  | c:constant -> (Atom, loc_expr _loc (Pexp_constant c))
  | let_kw open_kw o:override_flag mp:module_path in_kw
    e:(expression_lvl (let_prio lvl)) when (lvl < App) ->
      let mp = { txt = mp; loc = _loc_mp } in
      (Let, loc_expr _loc (Pexp_open (o, mp, e))) (* NOTE on the git repository, the override flag arguments seems to have disapeared *)
  | mp:module_path STR(".") STR("(") e:expression STR(")") ->
      let mp = { txt = mp; loc = _loc_mp } in
      (Atom, loc_expr _loc (Pexp_open (Fresh, mp, e))) (* NOTE idem (override flag) *)
  | let_kw r:{r:rec_flag l:let_binding in_kw e:(expression_lvl (let_prio lvl)) when (lvl < App)
                  -> (Let, loc_expr _loc (Pexp_let (r, l, e)))
             | module_kw mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> ({ txt = mn; loc = _loc_mn}, mt)}*
                 mt:{STR":" mt:module_type }? STR"=" me:module_expr in_kw e:(expression_lvl (let_prio lvl)) when (lvl < App) ->
               let me = match mt with None -> me | Some mt -> mexpr_loc _loc (Pmod_constraint(me, mt)) in
               let me = List.fold_left (fun acc (mn,mt) ->
                 mexpr_loc _loc (Pmod_functor(mn, mt, acc))) me (List.rev l) in
               (Let, loc_expr _loc (Pexp_letmodule({ txt = mn ; loc = _loc_mn }, me, e)))
             } -> r
  | function_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_function("", None, l)))
  | fun_kw l:{lbl:(parameter true)}* STR"->" e:(expression_lvl (let_prio lvl)) when (lvl < App) -> 
     (Let, apply_params _loc l e)
  | match_kw e:expression with_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_match(e, l)))
  | try_kw e:expression with_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_try(e, l)))
  | if_kw c:expression then_kw e:(expression_lvl If) e':{else_kw e:(expression_lvl If)}? when (lvl <= If) ->
     (If, loc_expr _loc (Pexp_ifthenelse(c,e,e')))
  | STR("(") e:expression? STR(")") -> (Atom, match e with Some e -> e | None ->
      let cunit = { txt = Lident "()"; loc = _loc } in
      loc_expr _loc (Pexp_construct(cunit, None, false)))
  | begin_kw e:expression? end_kw -> (Atom, match e with Some e -> e | None ->
      let cunit = { txt = Lident "()"; loc = _loc } in
      loc_expr _loc (Pexp_construct(cunit, None, false)))
  | c:constructor e:{ e:(expression_lvl (next_exp App)) when lvl <= App }? -> (App, loc_expr _loc (Pexp_construct({ txt = c; loc = _loc_c},e,false)))
  | assert_kw e:{ false_kw -> Pexp_assertfalse | e:(expression_lvl App) -> Pexp_assert(e)} when (lvl <= App) 
      -> (App,  loc_expr _loc e)
  | lazy_kw e:(expression_lvl App) when (lvl <= App) -> (App,  loc_expr _loc (Pexp_lazy(e)))
  | l:tag_name e:(expression_lvl (next_exp App)) when (lvl <= App) -> (App, loc_expr _loc (Pexp_variant(l,Some e)))
  | l:tag_name -> (Atom, loc_expr _loc (Pexp_variant(l,None)))
  | STR("[|") l:expression_list STR("|]") -> (Atom, loc_expr _loc (Pexp_array l))
  | STR("[") l:expression_list STR("]") ->
     (Atom, (List.fold_right (fun x acc ->
       loc_expr _loc (Pexp_construct({ txt = Lident "::"; loc = _loc}, Some (loc_expr _loc (Pexp_tuple [x;acc])), false)))
                    l (loc_expr _loc (Pexp_construct({ txt = Lident "[]"; loc = _loc}, None, false)))))
  | STR("{") e:{e:(expression_lvl (next_exp Seq)) with_kw}? l:record_list STR("}") ->
     (Atom, loc_expr _loc (Pexp_record(l,e)))
  | while_kw e:expression do_kw e':expression done_kw ->
      (Atom, loc_expr _loc (Pexp_while(e, e')))
  | for_kw id:lowercase_ident CHR('=') e:expression d:downto_flag
    e':expression do_kw e'':expression done_kw ->
      (Atom, loc_expr _loc (Pexp_for({ txt = id ; loc = _loc_id}, e, e', d, e'')))
  | new_kw p:class_path -> (Atom, loc_expr _loc (Pexp_new({ txt = p; loc = _loc_p})))
  | object_kw o:class_body end_kw -> (Atom, loc_expr _loc (Pexp_object o))
  | STR("{<") l:{ o:obj_item l:{STR";" o:obj_item}* STR(";")? -> o::l }?[[]] STR(">}") -> (Atom, loc_expr _loc (Pexp_override l))
  | STR("(") module_kw me:module_expr pt:{STR(":") pt:package_type}? STR(")") ->
      let desc = match pt with
                 | None    -> Pexp_pack me
                 | Some pt -> let me = loc_expr _loc_me (Pexp_pack me) in
                              let pt = loc_typ _loc_pt pt in
                              pexp_constraint (me, pt)
      in
      (Atom, loc_expr _loc desc)
  | CHR('<') name:{ STR("expr") -> "expression" | STR("type") -> "type" | STR("pat") -> "pattern"
                  | STR("str_item") -> "str_item" | STR("sig_item") -> "sig_item" } 
       CHR(':') s:string_literal CHR('>') -> (Atom, quote_expression _loc_s s name)
  | CHR('$') e:expression CHR('$') -> (Atom, push_pop_expression e)
  | p:prefix_symbol ->> let lvl' = prefix_prio p in e:(expression_lvl lvl') when lvl <= lvl' -> 
     (lvl', mk_unary_opp p _loc_p e _loc_e)
  end)

let apply_lbl _loc (lbl, e) =
  let e = match e with
      None -> loc_expr _loc (Pexp_ident { txt = Lident lbl; loc = _loc })
    | Some e -> e
  in (lbl, e)

let rec mk_seq = function
    [] -> assert false
  | [e] -> e
  | x::l -> 
     let res = mk_seq l in
     loc_expr (merge x.pexp_loc res.pexp_loc) (Pexp_sequence(x,res))

let semi_col = black_box 
  (fun str pos ->
   let c,str',pos' = read str pos in
   if c = ';' then
     let c',_,_ = read str' pos' in
     if c' = ';' then raise Give_up
     else (), str', pos'
   else
     raise Give_up)
  (Charset.singleton ';') false (";")

let expression_suit_aux = memoize2 (fun lvl' lvl ->
  let ln f _loc e = loc_expr (merge f.pexp_loc _loc) e in
  glr
  | l:{STR(",") e:(expression_lvl (next_exp Tupl))}+ when (lvl' > Tupl && lvl <= Tupl) -> 
      (Tupl, fun f -> ln f _loc (Pexp_tuple(f::l)))
  | t:type_coercion when (lvl' > Coerce && lvl <= Coerce) ->
      (Seq, fun e' -> ln e' _loc (
			   match t with Some t1, None -> pexp_constraint(e', t1)
				      | t1, Some t2 -> pexp_coerce(e', t1, t2)
				      | None, None -> assert false))
  | l:{semi_col e:(expression_lvl (next_exp Seq))}+ when (lvl' > Seq && lvl <= Seq) -> 
      (Seq, fun f -> mk_seq (f::l))
  | semi_col when (lvl' >= Seq && lvl <= Seq) -> (Seq, fun e -> e)
  | STR(".") r:{ STR("(") f:expression STR(")") STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' > Aff && lvl <= Aff) -> 
      (Aff, fun e' -> ln e' _loc (Pexp_apply(array_function _loc "Array" "set",[("",e');("",f);("",e)]))) 
  |            STR("(") f:expression STR(")") when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> ln e' _loc (Pexp_apply(array_function _loc "Array" "get",[("",e');("",f)])))
  |            STR("[") f:expression STR("]") STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' -> ln e' _loc (Pexp_apply(array_function _loc "String" "set",[("",e');("",f);("",e)]))) 
  |            STR("[") f:expression STR("]")  when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> ln e' _loc (Pexp_apply(array_function _loc "String" "get",[("",e');("",f)])))
  |            STR("{") f:expression STR("}") STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' -> bigarray_set _loc e' f e)
  |            STR("{") f:expression STR("}") when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> bigarray_get _loc e' f)
  |            f:field STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' ->
              let f = { txt = f; loc = _loc_f } in loc_expr _loc (Pexp_setfield(e',f,e)))
  |            f:field when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' ->
              let f = { txt = f; loc = _loc_f } in loc_expr _loc (Pexp_field(e',f))) } -> r
  | STR("#") f:method_name when (lvl' >= Dash && lvl <= Dash) -> 
      (Dash, fun e' -> ln e' _loc (Pexp_send(e',f)))
  | l:{a:argument}+ when (lvl' > App && lvl <= App) -> 
      (App, fun f -> ln f _loc (Pexp_apply(f,l)))
  | op:infix_op ->> let p = infix_prio op in let a = assoc p in 
                    e:(expression_lvl (if a = Right then p else next_exp p))
                      when lvl <= p && (lvl' > p || (a = Left && lvl' = p)) ->
      (p, fun e' -> ln e' e.pexp_loc (
          if op = "::" then
            Pexp_construct({ txt = Lident "::"; loc = _loc_op}, Some (loc_expr _loc_op (Pexp_tuple [e';e])), false)
          else 
            Pexp_apply(loc_expr _loc_op (Pexp_ident { txt = Lident op; loc = _loc_op }),
                     [("", e') ; ("", e)])))
  end)

let expression_suit =
  let f expression_suit =
    memoize2
      (fun lvl' lvl ->
         glr
          (p1,f1):(expression_suit_aux lvl' lvl) ->> (p2,f2):(expression_suit p1 lvl)
	       -> (p2, fun f -> f2 (f1 f))
      | EMPTY -> (lvl', fun f -> f) end)
  in
  let rec res x y = f res x y in
  res

let _ = set_expression_lvl (fun lvl ->
    glr
      (lvl',e):(expression_base lvl) ->> (_, f):(expression_suit lvl' lvl) -> f e
    end) expression_lvls

(****************************************************************************
 * Module expressions (module implementations)                              *
 ****************************************************************************)

let module_expr_base = 
  glr
  | mp:module_path ->
      let mid = { txt = mp; loc = _loc } in
      mexpr_loc _loc (Pmod_ident mid)
  | struct_kw ms:module_item* end_kw -> 
      mexpr_loc _loc (Pmod_structure(ms))
  | functor_kw STR("(") mn:module_name STR(":") mt:module_type STR(")")
    STR("->") me:module_expr -> mexpr_loc _loc (Pmod_functor({ txt = mn; loc = _loc_mn}, mt, me))
  | STR("(") me:module_expr mt:{STR(":") mt:module_type}? STR(")") ->
      (match mt with
       | None    -> me
       | Some mt -> mexpr_loc _loc (Pmod_constraint (me, mt)))
  | STR("(") val_kw e:expr pt:{STR(":") pt:package_type}? STR(")") ->
      let e = match pt with
              | None    -> e
              | Some pt -> let pt = loc_typ _loc_pt pt in
                           loc_expr _loc (pexp_constraint (e, pt))
      in
      mexpr_loc _loc (Pmod_unpack e)
  end

let _ = set_grammar module_expr (
  glr
    m:module_expr_base l:{STR("(") m:module_expr STR(")") -> (_loc, m)}* ->
      List.fold_left (fun acc (_loc_n, n) -> mexpr_loc (merge _loc_m _loc_n) (Pmod_apply(acc, n))) m l
  end)

let module_type_base = 
  glr
  | mp:modtype_path ->
      let mid = { txt = mp; loc = _loc } in
      mtyp_loc _loc (Pmty_ident mid)
  | sig_kw ms:signature_item* end_kw -> 
     mtyp_loc _loc (Pmty_signature(ms))
  | functor_kw STR("(") mn:module_name STR(":") mt:module_type STR(")")
     STR("->") me:module_type -> mtyp_loc _loc (Pmty_functor({ txt = mn; loc = _loc_mn}, mt, me))
  | STR("(") mt:module_type STR(")") -> mt
  | module_kw type_kw of_kw me:module_expr -> mtyp_loc _loc (Pmty_typeof me)
  end

let mod_constraint = 
  glr
  | type_kw tdef:typedef_in_constraint ->
     (fst tdef, Pwith_type(snd tdef))
  | module_kw m1:module_path CHR('=') m2:extended_module_path ->
     ({ txt = m1; loc = _loc_m1 }, Pwith_module { txt = m2; loc = _loc_m2 })
  | type_kw tps:type_params?[[]] tcn:typeconstr_name STR(":=") te:typexpr ->
      let td = { ptype_params = List.map fst tps
               ; ptype_cstrs = []
               ; ptype_kind = Ptype_abstract
               ; ptype_private = Public
               ; ptype_manifest = Some te
               ; ptype_variance = List.map snd tps
               ; ptype_loc = _loc
               }
      in
      ({ txt = Lident tcn; loc = _loc_tcn }, Pwith_typesubst td)
  | module_kw mn:module_name STR(":=") emp:extended_module_path ->
     ({ txt = Lident mn; loc = _loc_mn }, Pwith_modsubst { txt = emp; loc = _loc_emp })
  end

let _ = set_grammar module_type (
  glr
    m:module_type_base l:{with_kw m:mod_constraint l:{and_kw m:mod_constraint}* -> m::l } ? ->
      (match l with
         None -> m
       | Some l -> mtyp_loc _loc (Pmty_with(m, l)))
  end)
      
let module_item_base =
  glr
  | e:(alternatives extra_module_items) -> e
  | RE(let_re) r:rec_flag l:let_binding ->
      (match l with
       | [({ppat_desc = Ppat_any; ppat_loc = _}, e)] -> Pstr_eval e
       | _                                           -> Pstr_value (r, l))
  | external_kw n:value_name STR":" ty:typexpr STR"=" ls:string_literal* ->
      let l = List.length ls in
      if l < 1 || l > 3 then raise Give_up;
      Pstr_primitive({ txt = n; loc = _loc_n }, { pval_type = ty; pval_prim = ls; pval_loc = _loc})
  | td:type_definition -> Pstr_type td
  | ex:exception_definition -> ex
  | module_kw r:{rec_kw mn:module_name STR(":") mt:module_type CHR('=')
    me:module_expr ms:{and_kw mn:module_name STR(":") mt:module_type CHR('=')
    me:module_expr -> ({ txt = mn; loc = _loc_mn}, mt, me)}* ->
      let m = ({ txt = mn; loc = _loc_mn }, mt, me) in
      Pstr_recmodule (m::ms)
  |            mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> ({ txt = mn; loc = _loc_mn}, mt)}*
       mt:{STR":" mt:module_type }? STR"=" me:module_expr ->
     let me = match mt with None -> me | Some mt -> mexpr_loc _loc (Pmod_constraint(me, mt)) in
     let me = List.fold_left (fun acc (mn,mt) ->
       mexpr_loc _loc (Pmod_functor(mn, mt, acc))) me (List.rev l) in
     Pstr_module({ txt = mn ; loc = _loc_mn }, me)
  |            type_kw mn:modtype_name STR"=" mt:module_type ->
      Pstr_modtype({ txt = mn ; loc = _loc_mn }, mt) } -> r
  | open_kw o:override_flag m:module_path -> Pstr_open(o, { txt = m; loc = _loc_m} )
  | include_kw me:module_expr -> Pstr_include me
  | class_kw r:{ ctd:classtype_definition -> Pstr_class_type ctd
               | cds:class_definition -> Pstr_class cds } -> r
  | CHR('$') e:expression CHR('$') -> push_pop_str_item e
  | e:expression -> Pstr_eval e
  end

let _ = set_grammar module_item (
  glr
    s:module_item_base STR(";;")? -> { pstr_desc = s; pstr_loc = _loc; }
  end)

let structure =
  glr
    l : module_item* EOF -> l
  end

let signature_item_base =
 glr
  | e:(alternatives extra_signature_items) -> e
  | val_kw n:value_name STR(":") ty:typexpr ->
     Psig_value({ txt = n; loc = _loc_n }, { pval_type = ty; pval_prim = []; pval_loc = _loc})
  | external_kw n:value_name STR":" ty:typexpr STR"=" ls:string_literal* ->
      let l = List.length ls in
      if l < 1 || l > 3 then raise Give_up;
      Psig_value({ txt = n; loc = _loc_n }, { pval_type = ty; pval_prim = ls; pval_loc = _loc})
  | td:type_definition -> Psig_type td
  | (name,ed):exception_declaration -> Psig_exception (name, ed)
  | module_kw rec_kw mn:module_name STR(":") mt:module_type
    ms:{and_kw mn:module_name STR(":") mt:module_type -> ({ txt = mn;
    loc = _loc_mn}, mt)}* ->
      let m = ({ txt = mn; loc = _loc_mn }, mt) in
      Psig_recmodule (m::ms)
  | module_kw r:{mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> ({ txt = mn; loc = _loc_mn}, mt)}*
                                    STR":" me:module_type ->
     let me = List.fold_left (fun acc (mn,mt) ->
                                  mtyp_loc _loc (Pmty_functor(mn, mt, acc))) me (List.rev l) in
     Psig_module({ txt = mn ; loc = _loc_mn }, me)
  |           type_kw mn:modtype_name mt:{ STR"=" mt:module_type }? ->
     let mt = match mt with
              | None    -> Pmodtype_abstract
              | Some mt -> Pmodtype_manifest mt
     in Psig_modtype({ txt = mn ; loc = _loc_mn }, mt) } -> r
  | open_kw o:override_flag m:module_path -> Psig_open(o, { txt = m; loc = _loc_m} )
  | include_kw me:module_type -> Psig_include me
  | class_kw r:{ ctd:classtype_definition -> Psig_class_type ctd
               | cs:class_specification -> Psig_class cs } -> r
  | CHR('$') e:expression CHR('$') -> push_pop_sig_item e
end

let _ = set_grammar signature_item (
  glr
    s:signature_item_base STR(";;")? -> { psig_desc = s; psig_loc = _loc; }
  end)

let signature =
  glr
    l : signature_item* EOF -> l
  end

let ast =
  (* read the whole file with a buffer ...
     to be able to read stdin *)
  let name, ch = match !file with
      None -> "stdin", stdin
    | Some name -> 
(*       let buffer = Input.buffer_from_file name in
       List.iter (fun line ->
		  Printf.eprintf "%s\n" line.Input.contents) buffer;*)
       name, open_in name
  in
  try
    if entry = Impl then 
      `Struct (parse_channel structure blank name ch)
    else
      `Sig (parse_channel signature blank name ch)
  with
    Parse_error (fname,l,n,msgs) ->
    let msgs = String.concat " | " msgs in
    Printf.eprintf "File %S, line %d, characters %d:\n\
                    Error: Syntax error, %s expected\n"
                   fname l n msgs;
    exit 1

let _ = 
  if !ascii then begin
    begin
      match ast with 
      | `Struct ast -> Pprintast.structure Format.std_formatter ast;
      | `Sig ast -> Pprintast.signature Format.std_formatter ast;
    end;
    Format.print_newline ()
  end else begin
    let magic = match ast with 
      | `Struct _ -> Config.ast_impl_magic_number
      | `Sig _ -> Config.ast_intf_magic_number
    in
    output_string stdout magic;
    output_value stdout (match !file with None -> "" | Some name -> name);
    begin
      match ast with 
      | `Struct ast -> output_value stdout ast
      | `Sig ast -> output_value stdout ast
    end;
    close_out stdout
  end

end

module Final = (val
		  List.fold_left (fun (module Acc:Extension) (module Ext:FExt) -> 
                      (module (Ext(Acc)))) (module Initial) (List.rev !extensions_mod))
  
module Main = Make(Final)


