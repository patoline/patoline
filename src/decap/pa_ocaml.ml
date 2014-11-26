(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

open Input
open Decap
open Charset
#ifversion >= 4.02
open Ast_helper
#endif
open Asttypes
open Parsetree
open Longident

include Pa_ocaml_prelude

#define LOCATE locate

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
       let p = loc_expr _loc_name (Pexp_ident (id_loc (Lident ("~" ^ name)) _loc_name)) in
       Pexp_apply(p, ["", arg])
    | _ ->
       let p = loc_expr _loc_name (Pexp_ident (id_loc (Lident (name)) _loc_name)) in
       Pexp_apply(p, ["", arg])
  in
  loc_expr (merge2 _loc_name _loc_arg) res

#ifversion >= 4.00
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
#ifversion >= 4.00
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
#else
      | Ptyp_constr( Lident s, []) when List.mem s var_names ->
#endif
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
#endif

(* Floating-point literals *)
let float_lit_dec    = "[0-9][0-9_]*[.][0-9_]*\\([eE][+-][0-9][0-9_]*\\)?"
let float_lit_no_dec = "[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*"
let float_re = union_re [float_lit_no_dec; float_lit_dec]

let float_literal =
  parser
    f:RE(float_re)   -> f
  | dol:CHR('$') - STR("float") CHR(':') e:(expression_lvl App) - CHR('$') -> string_of_float (push_pop_float (start_pos _loc_dol).Lexing.pos_cnum e)

(* Character literals *)
let char_regular = "[^\\']"
let string_regular = "[^\\\"]"
let re_regular = "[^']"
let char_escaped = "[\\\\][\\\\\\\"\\\'ntbrs ]"
let re_escaped = "[\\\\][ntbrs]"
let char_dec     = "[\\\\][0-9][0-9][0-9]"
let char_hex     = "[\\\\][x][0-9a-fA-F][0-9a-fA-F]"

exception Illegal_escape of string

type string_litteral_type = Char | String | Re

let single_quote = black_box 
  (fun str pos ->
   let c,str',pos' = read str pos in
   if c = '\'' then
     let c',_,_ = read str' pos' in
     if c' = '\'' then raise (Give_up "" (* FIXME *))
     else (), str', pos'
   else
     raise (Give_up "" (* FIXME *)))
  (Charset.singleton '\'') false ("'")

let one_char slt =
  parser
  | '\n' -> '\n' 
  | single_quote when slt = Re -> '\''
  | c:RE(if slt = Re then re_escaped else char_escaped) ->
      (match c.[1] with
       | 'n' -> '\n'
       | 't' -> '\t'
       | 'b' -> '\b'
       | 'r' -> '\r'
       | 's' -> ' '
       | c   -> c)
  | c:RE(match slt with Char -> char_regular | String -> string_regular | Re -> re_regular)
      -> c.[0]
  | c:RE(char_dec)     -> (let str = String.sub c 1 3 in
                           let i = Scanf.sscanf str "%i" (fun i -> i) in
                           if i > 255 then
                             raise (Illegal_escape str)
                           else char_of_int i)
  | c:RE(char_hex)     -> (let str = String.sub c 2 2 in
                           let str' = String.concat "" ["0x"; str] in
                           let i = Scanf.sscanf str' "%i" (fun i -> i) in
                           char_of_int i)

let _ = set_grammar char_literal (
  parser
    r:(change_layout (
      parser CHR('\'') c:(one_char Char) CHR('\'') -> c
    ) no_blank) -> r
  | dol:CHR('$') - STR("char") CHR(':') e:(expression_lvl App) - CHR('$') ->
      push_pop_char (start_pos _loc_dol).Lexing.pos_cnum e)

(* String literals *)
let interspace = "[ \t]*"

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
      | x::l -> 
#ifversion >= 4.02
	 Bytes.unsafe_set str i x;
#else
	 String.unsafe_set str i x;
#endif
	 ptr := l
    done;
#ifversion >= 4.02
    Bytes.unsafe_to_string str
#else
    str
#endif

let _ = set_grammar string_literal (
  parser
  | r:(change_layout (
    parser
      CHR('"') lc:(one_char String)*
        lcs:(parser CHR('\\') CHR('\n') RE(interspace) lc:(one_char String)* -> lc)*
        CHR('"') -> char_list_to_string (List.flatten (lc::lcs))
    ) no_blank) -> r

  | r:(change_layout (
	parser CHR('{') id:RE("[a-z]*") CHR('|') ->>
	  let string_literal_suit = declare_grammar "string_literal_suit" in
	  let _ = set_grammar string_literal_suit (
	    parser
	    | CHR('|') STR(id) CHR('}') -> []
	    | c:ANY r:string_literal_suit -> c::r)
	  in r:string_literal_suit -> char_list_to_string r) no_blank) -> r

  | dol:CHR('$') - STR("string") CHR(':') e:(expression_lvl App) - CHR('$') -> push_pop_string (start_pos _loc_dol).Lexing.pos_cnum e)


let _ = set_grammar regexp_literal (
  parser
  | r:(change_layout (
    parser
      "''" lc:(one_char Re)*
        lcs:(parser '\\' '\n' RE(interspace) lc:(one_char Re)* -> lc)*
        "''" -> char_list_to_string (List.flatten (lc::lcs))
    ) no_blank) -> r)


type tree = Node of tree * tree | Leaf of string

let (string_of_tree:tree->string) t =
  let b = Buffer.create 101 in
  let rec fn = function
      Leaf s -> Buffer.add_string b s
    | Node(a,b) -> fn a; fn b
  in
  fn t;
  Buffer.contents b

let quotation = 
  let quotation_aux:tree grammar = declare_grammar "quotation" in				      
  let _ = set_grammar quotation_aux
      (parser
       | | "<:" q:quotation_aux q':quotation_aux -> Node(Node((Leaf "<:"), q), Node(Leaf ">>", q'))
       | | s: string_literal q:quotation_aux -> Node(Leaf (Printf.sprintf "%S" s), q)
       | | ">>" -> Leaf ""
       | | s:RE("[^<>\"\n]+") q:quotation_aux -> Node(Leaf s, q)
       | | c:{c:'<' || c:'>' || c:'\n' || c:'"'} q:quotation_aux -> Node(Leaf (String.make 1 c), q))
  in
  apply string_of_tree (change_layout quotation_aux no_blank)

(* Naming labels *)
let label_name = lowercase_ident

let label =
  parser
  | STR("~") ln:label_name -> ln

let opt_label =
  parser
  | STR("?") ln:label_name -> ln

let maybe_opt_label =
  parser
  | o:STR("?")? ln:label_name ->
      (if o = None then ln else ("?" ^ ln))

(****************************************************************************
 * Names                                                                    *
 ****************************************************************************)
(* Naming objects *)
let infix_op =
  parser
    sym:infix_symbol -> sym

let operator_name =
  parser
  | op:infix_op      -> op
  | op:prefix_symbol -> op

let value_name =
  parser
    id:lowercase_ident -> id
  | STR("(") op:operator_name STR(")") -> op

let constr_name     = capitalized_ident  
let tag_name        = 
  parser STR("`") c:ident -> c

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
  parser
    STR("(") m':(module_path_gen true) STR(")") when allow_app ->
      (fun a -> Lapply(a, m'))
  | STR(".") m:module_name ->
      (fun acc -> Ldot(acc, m))
  )

let _ = set_module_path_suit (fun allow_app ->
    parser
    ~ f:(module_path_suit_aux allow_app) g:(module_path_suit allow_app) -> (fun acc -> g (f acc))
    | EMPTY -> (fun acc -> acc)
    )

let _ = set_module_path_gen (fun allow_app ->
  parser
  | m:module_name s:(module_path_suit allow_app) -> s (Lident m)
  )

let module_path = module_path_gen false
let extended_module_path = module_path_gen true

let value_path =
  parser
  | mp:{m:module_path STR(".")}? vn:value_name ->
      (match mp with
       | None   -> Lident vn
       | Some p -> Ldot(p, vn))

let constr =
  parser
  | mp:{m:module_path STR(".")}? cn:constr_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))

let typeconstr =
  parser
  | mp:{m:extended_module_path STR(".")}? tcn:typeconstr_name ->
      (match mp with
       | None   -> Lident tcn
       | Some p -> Ldot(p, tcn))

let field =
  parser
  | mp:{m:module_path STR(".")}? fn:field_name ->
      (match mp with
       | None   -> Lident fn
       | Some p -> Ldot(p, fn))

let class_path =
  parser
  | mp:{m:module_path STR(".")}? cn:class_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))

let modtype_path =
  parser
  | mp:{m:extended_module_path STR(".")}? mtn:modtype_name ->
      (match mp with
       | None   -> Lident mtn
       | Some p -> Ldot(p, mtn))

let classtype_path =
  parser
  | mp:{m:extended_module_path STR(".")}? cn:class_name ->
      (match mp with
       | None   -> Lident cn
       | Some p -> Ldot(p, cn))

let opt_variance =
  parser
  | v:RE("[+-]")? ->
#ifversion >= 4.02
      (match v with
       | None     -> Invariant
       | Some "+" -> Covariant
       | Some "-" -> Contravariant
       | _        -> assert false)
#else
      (match v with
       | None     -> (false, false)
       | Some "+" -> (true , false)
       | Some "-" -> false, true
       | _        -> assert false)
#endif

let override_flag =
  parser
    o:STR("!")? -> (if o <> None then Override else Fresh)

(****************************************************************************
 * Attributes (parsed but ignored for ocaml < 4.02                     *
 ****************************************************************************)

let attr_id =
  parser
  | id:RE(ident_re) l:{ CHR('.') id:RE(ident_re)}* ->
      id_loc (String.concat "." (id::l)) _loc

#ifversion < 4.02
type attr =
    PStr of structure_item list
  | PTyp of core_type
  | PPat of pattern * expression option
#endif

let payload =
  parser
  | s:structure -> PStr(s)
  | CHR(':') t:typexpr -> PTyp(t)
  | CHR('?') p:pattern e:{STR("when") e:expression}? -> PPat(p,e)

let attribute = 
  parser
  | STR("[@") id:attr_id p:payload

let attributes =
  parser
  | {a:attribute}*

let ext_attributes =
  parser
  | a:{CHR('%') a:attribute}? l:attributes -> a, l

let post_item_attributes =
  parser
  | l:{STR("[@@") id:attr_id p:payload CHR(']')}*

let ext_attributes =
  parser
  | l:{STR("[@@@") id:attr_id p:payload CHR(']')}*

let extension =
  parser
  | STR("[%") id:attr_id p:payload CHR(']')

let item_extension =
  parser
  | STR("[%%") id:attr_id p:payload CHR(']')

(****************************************************************************
 * Type expressions                                                         *
 ****************************************************************************)

let poly_typexpr =
  parser
  | ids:{STR("'") id:ident}+ STR(".") te:typexpr ->
      loc_typ _loc (Ptyp_poly (ids, te))
  | te:typexpr ->
#ifversion >= 4.02
       te
#else
       loc_typ _loc (Ptyp_poly ([], te))
#endif

let poly_syntax_typexpr =
  parser
  | type_kw ids:typeconstr_name+ STR(".") te:typexpr ->
      (ids, te)
   
let method_type =
  parser
  | mn:method_name STR(":") pte:poly_typexpr ->
#ifversion >= 4.02
      mn, [], pte
#else
    { pfield_desc = Pfield (mn, pte); pfield_loc = _loc }
#endif

let tag_spec =
  parser
  | tn:tag_name te:{_:of_kw CHR('&')? typexpr}? ->
      let amp,t = match te with
              | None   -> true, []
              | Some (amp,l) -> amp<>None, [l]
      in
#ifversion >= 4.02
      Rtag (tn, [], amp, t)
#else
      Rtag (tn, amp, t)
#endif
  | te:typexpr ->
      Rinherit te

let tag_spec_first =
  parser
  | tn:tag_name te:{_:of_kw CHR('&')? typexpr}? ->
      let amp,t = match te with
              | None   -> true,[]
              | Some (amp,l) -> amp<>None, [l]
      in
#ifversion >= 4.02
      [Rtag (tn, [], amp, t)]
#else
      [Rtag (tn, amp, t)]
#endif
  | te:typexpr? STR("|") ts:tag_spec ->
      match te with
      | None    -> [ts]
      | Some te -> [Rinherit te; ts]

let tag_spec_full =
  parser
  | tn:tag_name (amp,tes):{of_kw amp:STR("&")? te:typexpr
    tes:{STR("&") te:typexpr}* -> (amp<>None,(te::tes))}?[true,[]] ->
#ifversion >= 4.02		    
      Rtag (tn, [], amp, tes)
#else
      Rtag (tn, amp, tes)
#endif
  | te:typexpr ->
      Rinherit te

let polymorphic_variant_type : core_type grammar =
  parser
  | STR("[") tsf:tag_spec_first tss:{STR("|") ts:tag_spec}* STR("]") ->
#ifversion >= 4.02
      let flag = Closed in
#else
      let flag = true in
#endif
      loc_typ _loc (Ptyp_variant (tsf @ tss, flag, None))
  | STR("[>") ts:tag_spec? tss:{STR("|") ts:tag_spec}* STR("]") ->
      let tss = match ts with
                | None    -> tss
                | Some ts -> ts :: tss
      in
#ifversion >= 4.02
      let flag = Open in
#else
      let flag = false in
#endif
      loc_typ _loc (Ptyp_variant (tss, flag, None))
  | STR("[<") STR("|")? tfs:tag_spec_full tfss:{STR("|") tsf:tag_spec_full}*
    tns:{STR(">") tns:tag_name+}?[[]] STR("]") ->
#ifversion >= 4.02
      let flag = Closed in
#else
      let flag = true in
#endif
      loc_typ _loc (Ptyp_variant (tfs :: tfss, flag, Some tns))

let package_constraint =
  parser
#ifversion >= 4.00
  | type_kw tc:typeconstr CHR('=') te:typexpr ->
#else
  | type_kw tc:capitalized_ident CHR('=') te:typexpr ->
#endif
      let tc = id_loc tc _loc_tc in
      (tc, te)

let package_type =
  parser
  | mtp:modtype_path cs:{with_kw pc:package_constraint
    pcs:{_:and_kw package_constraint}* -> (pc::pcs)}?[[]] ->
      let mtp = id_loc mtp _loc_mtp in
      Ptyp_package (mtp, cs)

let opt_present =
  parser
  | STR("[>") l:tag_name+ STR("]") -> l
  | EMPTY -> []

let mkoption loc d =
  let loc = ghost loc in 
  loc_typ loc (Ptyp_constr(id_loc (Ldot (Lident "*predef*", "option")) loc,[d]))

let extra_types_grammar = alternatives extra_types
				       
let typexpr_base : core_type grammar =
  parser
  | e:extra_types_grammar -> e
  | STR("'") id:ident ->
      loc_typ _loc (Ptyp_var id)
  | STR("_") ->
      loc_typ _loc Ptyp_any
  | STR("(") module_kw pt:package_type STR(")") ->
      loc_typ _loc pt
  | STR("(") te:typexpr STR(")") ->
      te
  | ln:opt_label STR(":") te:(typexpr_lvl (next_type_prio Arr)) STR("->") te':typexpr ->
      loc_typ _loc (Ptyp_arrow ("?" ^ ln, mkoption _loc_te te, te'))
  | ln:label_name STR(":") te:(typexpr_lvl (next_type_prio Arr)) STR("->") te':typexpr ->
      loc_typ _loc (Ptyp_arrow (ln, te, te'))
  | tc:typeconstr ->
      loc_typ _loc (Ptyp_constr (id_loc tc _loc_tc, []))
  | STR("(") te:typexpr tes:{STR(",") te:typexpr}* STR(")") tc:typeconstr ->
      let constr = id_loc tc _loc_tc in
      loc_typ _loc (Ptyp_constr (constr, te::tes))
  | pvt:polymorphic_variant_type -> pvt
  | STR("<") rv:STR("..")? STR(">") ->
#ifversion >= 4.02
      let ml = if rv = None then Closed else Open in
      loc_typ _loc (Ptyp_object([], ml))
#else
      let ml = if rv = None then [] else [{ pfield_desc = Pfield_var; pfield_loc = _loc_rv}] in
      loc_typ _loc (Ptyp_object ml)
#endif
  | STR("<") mt:method_type mts:{STR(";") mt:method_type}*
    rv:{STR(";") rv:STR("..")?}? STR(">") ->
#ifversion >= 4.02
      let ml = if rv = None || rv = Some None then Closed else Open in
      loc_typ _loc (Ptyp_object ((mt :: mts), ml))
#else
      let ml = if rv = None || rv = Some None then [] else [{ pfield_desc = Pfield_var; pfield_loc = _loc_rv}] in
      loc_typ _loc (Ptyp_object (mt :: mts @ ml))
#endif
#ifversion >= 4.02
  | STR("#") cp:class_path ->
      let cp = id_loc cp _loc_cp in
      loc_typ _loc (Ptyp_class (cp, []))
  | STR("(") te:typexpr tes:{STR(",") te:typexpr}* STR(")")
    STR("#") cp:class_path ->
      let cp = id_loc cp _loc_cp in
      loc_typ _loc (Ptyp_class (cp, te::tes))
#else
  | STR("#") cp:class_path o:opt_present ->
      let cp = id_loc cp _loc_cp in
      loc_typ _loc (Ptyp_class (cp, [], o))
  | STR("(") te:typexpr tes:{STR(",") te:typexpr}* STR(")")
    STR("#") cp:class_path  o:opt_present ->
      let cp = id_loc cp _loc_cp in
      loc_typ _loc (Ptyp_class (cp, te::tes, o))
#endif
 | dol:CHR('$') - t:{t:{STR("tuple") -> "tuple"} CHR(':') }? e:(expression_lvl App) - CHR('$') ->
	 (match t with
	   None ->
	   push_pop_type (start_pos _loc_dol).Lexing.pos_cnum e
	 | Some str ->
	    let l = push_pop_type_list (start_pos _loc_dol).Lexing.pos_cnum e in
	    match str with
	    | "tuple" -> loc_typ _loc (Ptyp_tuple l)
	    | _ -> raise (Give_up "" (* FIXME *)))

let extra_type_suits_grammar lvl' lvl = alternatives (List.map (fun g -> g lvl' lvl) extra_type_suits)
let typexpr_suit_aux : type_prio -> type_prio -> (type_prio * (core_type -> Location.t -> core_type)) grammar = memoize1 (fun lvl' lvl ->
  let ln f _loc e _loc_f = loc_typ (merge2 _loc_f _loc) e in
  parser
  | e:(extra_type_suits_grammar lvl' lvl) -> e
  | STR("->") te':(typexpr_lvl Arr) when lvl' > Arr && lvl <= Arr ->
      (Arr, fun te -> ln te _loc (Ptyp_arrow ("", te, te')))
  | tes:{STR("*") te:(typexpr_lvl (next_type_prio ProdType))}+  when lvl' > ProdType && lvl <= ProdType->
      (ProdType, fun te -> ln te _loc (Ptyp_tuple (te::tes)))
  | tc:typeconstr when lvl' >= AppType && lvl <= AppType ->
      (AppType, fun te -> ln te _loc (Ptyp_constr (id_loc tc _loc_tc, [te])))
  | as_kw STR("'") id:ident when lvl' >= As && lvl <= As ->
      (As, fun te -> ln te _loc (Ptyp_alias (te, id)))
#ifversion >= 4.02
  | STR("#") cp:class_path when lvl' >= DashType && lvl <= DashType ->
      let cp = id_loc cp _loc_cp in
      let tex = fun te ->
        ln te _loc (Ptyp_class (cp, [te]))
      in (DashType, tex)
#else
  | STR("#") cp:class_path o:opt_present when lvl' >= DashType && lvl <= DashType ->
      let cp = id_loc cp _loc_cp in
      let tex = fun te ->
        ln te _loc (Ptyp_class (cp, [te], o))
      in (DashType, tex)
#endif
  )

let typexpr_suit =
  let f =
    memoize2'
      (fun type_suit lvl' lvl ->
         parser
         | (p1,f1):(typexpr_suit_aux lvl' lvl) ->> (p2,f2):(type_suit p1 lvl) -> (p2, fun f _loc_f -> f2 (f1 f _loc_f) _loc_f)
         | EMPTY -> (lvl', fun f _loc_f -> f) 
         )
  in
  let rec res x y = f res x y in
  res

let _ = set_typexpr_lvl (fun lvl ->
  parser
  | t:typexpr_base ft:(typexpr_suit AtomType lvl) -> snd ft t _loc_t
  )

(****************************************************************************
 * Type and exception definitions                                           *
 ****************************************************************************)

(* Type definition *)
let type_param =
  parser
  | var:opt_variance CHR('\'') id:ident ->
      (Some (id_loc id _loc_id), var)
  | var:opt_variance CHR('_') ->
      (None, var)

let type_params =
  parser
  | tp:type_param -> [tp]
  | STR("(") tp:type_param tps:{STR(",") tp:type_param -> tp}* STR(")") ->
      tp::tps

let type_equation =
  parser
  | CHR('=') p:private_flag te:typexpr -> (p,te)

let type_constraint =
  parser
  | constraint_kw STR("'") id:ident CHR('=') te:typexpr ->
      (loc_typ _loc_id (Ptyp_var id), te, _loc)

let constr_decl =
  let constr_name =
    parser
    | cn:constr_name    -> cn
    | STR("(") STR(")") -> "()"
  in
  parser
  | cn:constr_name (tes,te):{ te:{_:of_kw typexpr}? ->
			      let tes =
				match te with
				| None   -> []
				| Some { ptyp_desc = Ptyp_tuple tes; ptyp_loc = _ } -> tes
				| Some t -> [t]
			      in (tes, None)
#ifversion >= 4.00
			    | CHR(':') ats:{te:(typexpr_lvl (next_type_prio ProdType)) tes:{CHR('*') te:(typexpr_lvl (next_type_prio ProdType))}*
							     STR("->") -> (te::tes)}?[[]] te:typexpr -> (ats, Some te)
#endif
                            }
	    -> (let c = id_loc cn _loc_cn in
	        constructor_declaration _loc c tes te)

let field_decl =
  parser
  | m:mutable_flag fn:field_name STR(":") pte:poly_typexpr ->
      label_declaration _loc (id_loc fn _loc_fn) m pte

let _ = set_grammar constr_decl_list (
  parser
  | STR("|")? cd:constr_decl cds:{STR("|") cd:constr_decl -> cd}* ls:constr_decl_list -> (cd::cds) @ ls
  | STR("|")? dol:CHR('$') - e:(expression_lvl App) - CHR('$') ls:constr_decl_list -> push_pop_constr_decl (start_pos _loc_dol).Lexing.pos_cnum e @ ls
  | EMPTY -> []
  )

let _ = set_grammar field_decl_list (
  parser
  | fd:field_decl fds:{STR(";") fd:field_decl -> fd}* STR(";")? ls:field_decl_list -> (fd::fds) @ ls
  | dol:CHR('$') - e:(expression_lvl App) - CHR('$') STR(";")? ls:field_decl_list -> push_pop_field_decl (start_pos _loc_dol).Lexing.pos_cnum e @ ls
  | EMPTY -> []
  )

let type_representation =
  parser
  | STR("{") fds:field_decl_list STR("}") -> Ptype_record fds
  | cds:constr_decl_list -> if cds = [] then raise (Give_up "Illegal empty constructors declaration"); Ptype_variant (cds)

let type_information =
  parser
  | te:type_equation? ptr:{CHR('=') pri:private_flag tr:type_representation}?
    cstrs:type_constraint* ->
      let pri, tkind =
        match ptr with
        | None   -> (Public, Ptype_abstract)
        | Some c -> c
      in
      (pri, te, tkind, cstrs)

let typedef_gen = (fun ?prev_loc constr filter ->
  parser
  | tps:type_params?[[]] tcn:constr ti:type_information ->
      let _loc = match
 	  prev_loc with None -> _loc
	| Some l -> merge2 l _loc
      in
      let (pri, te, tkind, cstrs) = ti in
      let pri, te = match te with
	  None -> pri, None
	| Some(Private, te) -> 
	   if pri = Private then raise (Give_up "" (* FIXME *)); (* ty = private ty' = private A | B is not legal *) 
	   Private, Some te
	| Some(_, te) -> pri, Some te
      in
#ifversion < 4.00
      let tps = List.map (function
			     (Some s, t) -> s,t | None, _ -> raise (Give_up "" (* FIXME *))) tps in
#endif
      id_loc tcn _loc_tcn, 
         type_declaration _loc (id_loc (filter tcn) _loc_tcn)
	   tps cstrs tkind pri te
   )

let typedef = typedef_gen typeconstr_name (fun x -> x)
let typedef_in_constraint prev_loc = typedef_gen ~prev_loc typeconstr Longident.last


let type_definition =
  parser
  | type_kw td:typedef tds:{and_kw td:typedef -> td}* -> (td::tds)
  

let exception_declaration =
  parser
  | exception_kw cn:constr_name te:{_:of_kw typexpr}? ->
      (let tes =
        match te with
        | None   -> []
        | Some { ptyp_desc = Ptyp_tuple tes; ptyp_loc = _ } -> tes
        | Some t -> [t]
      in (id_loc cn _loc_cn, tes, merge2 _loc_cn _loc_te))

(* Exception definition *)
let exception_definition =
  parser
  | exception_kw cn:constr_name CHR('=') c:constr ->
      (let name = id_loc cn _loc_cn in
      let ex = id_loc c _loc_c in
#ifversion >= 4.02
       (Str.exception_ ~loc:_loc (Te.rebind ~loc:(merge2 _loc_cn _loc_c) name ex))).pstr_desc
#else
      Pstr_exn_rebind (name, ex))
#endif
  | (name,ed,_loc'):exception_declaration ->
#ifversion >= 4.02
      (Str.exception_ ~loc:_loc (Te.decl ~loc:_loc' ~args:ed name)).pstr_desc
#else
      Pstr_exception (name, ed)
#endif

(****************************************************************************
 * Classes                                                                  *
 ****************************************************************************)
(* Class types *)
let class_field_spec = declare_grammar "class_field_spec"
let class_body_type = declare_grammar "class_body_type"

let virt_mut = 
  parser
  | v:virtual_flag m:mutable_flag -> (v, m)
  | mutable_kw virtual_kw -> (Virtual, Mutable)

let virt_priv = 
  parser
  | v:virtual_flag p:private_flag -> (v, p)
  | private_kw virtual_kw -> (Virtual, Private)

let _ = set_grammar class_field_spec (
  parser
  | inherit_kw cbt:class_body_type ->
#ifversion >= 4.02
      pctf_loc _loc (Pctf_inherit cbt)
#else
      pctf_loc _loc (Pctf_inher cbt)
#endif
  | val_kw (vir,mut):virt_mut ivn:inst_var_name STR(":") te:typexpr ->
#ifversion >= 4.00
      pctf_loc _loc (Pctf_val (ivn, mut, vir, te))
#else
      Pctf_val (ivn, mut, vir, te, _loc)
#endif
  | method_kw (v,pri):virt_priv mn:method_name STR(":") te:poly_typexpr ->
#ifversion >= 4.02
        pctf_loc _loc (Pctf_method (mn, pri, v, te))
#else
      (if v = Concrete then
#ifversion >= 4.00
        pctf_loc _loc (Pctf_meth (mn, pri, te))
#else
        Pctf_meth (mn, pri, te, _loc)
#endif
      else
#ifversion >= 4.00
        pctf_loc _loc (Pctf_virt (mn, pri, te))
#else
        Pctf_virt (mn, pri, te, _loc)
#endif
		    )
#endif
  | constraint_kw te:typexpr CHR('=') te':typexpr ->
#ifversion >= 4.02
      pctf_loc _loc (Pctf_constraint (te, te'))
#else
#ifversion >= 4.00
      pctf_loc _loc (Pctf_cstr (te, te'))
#else
      Pctf_cstr (te, te',_loc)
#endif
#endif
  )

let _ = set_grammar class_body_type (
  parser
  | object_kw te:{STR("(") te:typexpr STR(")")}? cfs:class_field_spec*
    end_kw ->
      let self = match te with
                 | None   -> loc_typ _loc_te Ptyp_any
                 | Some t -> t
      in
      let sign =
#ifversion < 4.00
	(self, cfs)
#else
        { pcsig_self = self
        ; pcsig_fields = cfs
#ifversion <= 4.01
        ; pcsig_loc = merge2 _loc_te _loc_cfs
#endif
        }
#endif
      in
      pcty_loc _loc (Pcty_signature sign)
  | tes:{STR("[") te:typexpr tes:{STR(",") te:typexpr}*
    STR("]") -> (te::tes)}?[[]] ctp:classtype_path ->
      let ctp = id_loc ctp _loc_ctp in
      pcty_loc _loc (Pcty_constr (ctp, tes))
  )

let class_type =
  parser
  | tes:{l:maybe_opt_label? STR(":") te:typexpr -> (l, te)}* cbt:class_body_type ->
      let app acc (lab, te) =
#ifversion >= 4.02
        match lab with
        | None   -> pcty_loc _loc (Pcty_arrow ("", te, acc))
        | Some l -> pcty_loc _loc (Pcty_arrow (l, (if l.[0] = '?' then mkoption _loc_tes te else te), acc))
#else
        match lab with
        | None   -> pcty_loc _loc (Pcty_fun ("", te, acc))
        | Some l -> pcty_loc _loc (Pcty_fun (l, (if l.[0] = '?' then mkoption _loc_tes te else te), acc))
#endif
      in
      List.fold_left app cbt (List.rev tes)

let type_parameters =
  parser
  | i1:type_param l:{ STR(",") i2:type_param }* -> i1::l

(* Class specification *)
let class_spec =
  parser
  | v:virtual_flag params:{STR("[") params:type_parameters STR("]")}?[[]]
    cn:class_name STR(":") ct:class_type ->
      class_type_declaration _loc_params _loc (id_loc cn _loc_cn) params v ct

let class_specification =
  parser
  | cs:class_spec css:{_:and_kw class_spec}* -> (cs::css)

(* Class type definition *)
let classtype_def =
  parser
  | v:virtual_flag params:{STR("[") tp:type_parameters STR("]")}?[[]] cn:class_name
    CHR('=') cbt:class_body_type ->
      class_type_declaration _loc_params _loc (id_loc cn _loc_cn) params v cbt

let classtype_definition =
  parser
  | type_kw cd:classtype_def cds:{_:and_kw classtype_def}* ->
      (cd::cds)

(****************************************************************************
 * Constants and Patterns                                                   *
 ****************************************************************************)
(* Constants *)
let constant =
  parser
    f:float_literal   -> Const_float f
  | c:char_literal    -> Const_char c
  | s:string_literal  -> const_string s
  | s:regexp_literal  -> const_string s
  | i:int32_lit       -> Const_int32 i
  | i:int64_lit       -> Const_int64 i
  | i:nat_int_lit     -> Const_nativeint i
  | i:integer_literal -> Const_int i

(* we do like parser.mly from ocaml: neg_constant for pattern only *)
let neg_constant =
  parser
    {CHR('-') | STR("-.")} f:float_literal -> Const_float ("-"^f)
  | CHR('-') i:int32_lit       -> Const_int32 (Int32.neg i)
  | CHR('-') i:int64_lit       -> Const_int64 (Int64.neg i)
  | CHR('-') i:nat_int_lit     -> Const_nativeint (Nativeint.neg i)
  | CHR('-') i:integer_literal -> Const_int (-i)

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

let ppat_list _loc l =
  let nil = id_loc (Lident "[]") _loc in
  let cons x xs =
    let c = id_loc (Lident "::") _loc in
    let cons = ppat_construct (c, Some (loc_pat _loc (Ppat_tuple [x;xs]))) in
    loc_pat _loc cons
  in
  List.fold_right cons l (loc_pat _loc (ppat_construct (nil, None)))

let extra_patterns_grammar lvl = alternatives (List.map (fun g -> g lvl) extra_patterns)
let pattern_base = memoize1 (fun lvl ->
  parser
  | e:(extra_patterns_grammar lvl) -> e
  | vn:value_name ->
      (AtomPat, loc_pat _loc (Ppat_var (id_loc vn _loc_vn)))
  | STR("_") ->
      (AtomPat, loc_pat _loc Ppat_any)
  | c1:char_literal STR("..") c2:char_literal ->
      let ic1, ic2 = Char.code c1, Char.code c2 in
      if ic1 > ic2 then assert false; (* FIXME error message invalid range *)
#ifversion >= 4.02
      (AtomPat, loc_pat _loc (Ppat_interval (Const_char (Char.chr ic1), Const_char (Char.chr ic2))))
#else 
      let const i = Ppat_constant (Const_char (Char.chr i)) in
      let rec range acc a b =
        if a > b then assert false
        else if a = b then a::acc
        else range (a::acc) (a+1) b
      in
      let opts = List.map (fun i -> loc_pat _loc (const i)) (range [] ic1 ic2) in
      (AtomPat, List.fold_left (fun acc o -> loc_pat _loc (Ppat_or(o, acc))) (List.hd opts) (List.tl opts))
#endifx
  | c:{c:constant | c:neg_constant} ->
      (AtomPat, loc_pat _loc (Ppat_constant c))
  | STR("(") p:pattern STR(")") -> (AtomPat, p)
  | lazy_kw p:(pattern_lvl ConstrPat) when lvl <= ConstrPat ->
      let ast = Ppat_lazy(p) in
      (ConstrPat, loc_pat _loc ast)
  | c:constr p:(pattern_lvl ConstrPat) when lvl <= ConstrPat ->
      let ast = ppat_construct(id_loc c _loc_c, Some p) in
      (ConstrPat, loc_pat _loc ast)
  | c:constr ->
      let ast = ppat_construct(id_loc c _loc_c, None) in
      (AtomPat, loc_pat _loc ast)
  | b:bool_lit ->
      let fls = id_loc (Lident b) _loc in
      (AtomPat, loc_pat _loc (ppat_construct (fls, None)))
  | c:tag_name p:(pattern_lvl ConstrPat) when lvl <= ConstrPat ->
      (ConstrPat, loc_pat _loc (Ppat_variant (c, Some p)))
  | c:tag_name ->
      (AtomPat, loc_pat _loc (Ppat_variant (c, None)))
  | s:STR("#") t:typeconstr ->
      (AtomPat, loc_pat _loc (Ppat_type(id_loc t _loc_t)))
  | s:STR("{") f:field p:{CHR('=') p:pattern}? fps:{STR(";") f:field
    p:{CHR('=') p:pattern}? -> (id_loc f _loc_f, p)}*
    clsd:{STR(";") STR("_") -> ()}? STR(";")? STR("}") ->
      let all = (id_loc f _loc_f, p)::fps in
      let f (lab, pat) =
        match pat with
        | Some p -> (lab, p)
        | None   -> 
#ifversion < 4.00
	   let slab = match lab with
                               | Lident s -> s
                               | _        -> raise (Give_up "" (* FIXME *))
                    in (lab, loc_pat _loc (Ppat_var slab))
#else
	   let slab = match lab.txt with
                               | Lident s -> id_loc s lab.loc
                               | _        -> raise (Give_up "" (* FIXME *))
                    in (lab, loc_pat lab.loc (Ppat_var slab))
#endif
      in
      let all = List.map f all in
      let cl = match clsd with
               | None   -> Closed
               | Some _ -> Open
      in
      (AtomPat, loc_pat _loc (Ppat_record (all, cl)))
  | STR("[") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("]") ->
      (AtomPat, ppat_list _loc (p::ps))
  | STR("[") STR("]") ->
      let nil = id_loc (Lident "[]") _loc in
      (AtomPat, loc_pat _loc (ppat_construct (nil, None)))
  | STR("[|") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("|]") ->
      (AtomPat, loc_pat _loc (Ppat_array (p::ps)))
  | STR("[|") STR("|]") ->
      (AtomPat, loc_pat _loc (Ppat_array []))
  | STR("(") STR(")") ->
      let unt = id_loc (Lident "()") _loc in
      (AtomPat, loc_pat _loc (ppat_construct (unt, None)))
  | begin_kw end_kw ->
      let unt = id_loc (Lident "()") _loc in
      (AtomPat, loc_pat _loc (ppat_construct (unt, None)))
#ifversion >= 4.00
  | STR("(") module_kw mn:module_name pt:{STR(":") pt:package_type}? STR(")") ->
      let unpack = Ppat_unpack { txt = mn; loc = _loc_mn } in
      let pat = match pt with
                | None    -> unpack
                | Some pt -> let pt = loc_typ _loc_pt pt in
                             Ppat_constraint (loc_pat _loc_mn unpack, pt)
      in
      (AtomPat, loc_pat _loc pat)
#endif
  | CHR('$') - c:capitalized_ident ->
     (try let str = Sys.getenv c in
	  AtomPat, parse_string ~filename:("ENV:"^c) pattern blank str
      with Not_found -> raise (Give_up "" (* FIXME *)))

  | dol:CHR('$') - t:{t:{ STR("tuple") -> "tuple" | 
		    STR("list") -> "list" |
		    STR("array") -> "array" } CHR(':') }? 
       e:(expression_lvl App) - CHR('$') ->
	 (match t with
	   None ->
	   (AtomPat, push_pop_pattern (start_pos _loc_dol).Lexing.pos_cnum e)
	 | Some str ->
	    let l = push_pop_pattern_list (start_pos _loc_dol).Lexing.pos_cnum e in
	    match str with
	    | "tuple" -> (AtomPat, loc_pat _loc (Ppat_tuple l))
	    | "array" -> (AtomPat, loc_pat _loc (Ppat_array l))
	    | "list" -> (AtomPat, ppat_list _loc l)
	    | _ -> raise (Give_up "" (* FIXME *)))
  )

let extra_pattern_suits_grammar lvl' lvl = alternatives (List.map (fun g -> g lvl' lvl) extra_pattern_suits)
let pattern_suit_aux : pattern_prio -> pattern_prio -> (pattern_prio * (pattern -> pattern)) grammar = memoize1 (fun lvl' lvl ->
  let ln f _loc e = loc_pat (merge2 f.ppat_loc _loc) e in
  parser
  | e:(extra_pattern_suits_grammar lvl' lvl) -> e
  | as_kw vn:value_name when lvl' >= AsPat && lvl <= AsPat ->
      (lvl', fun p ->
        ln p _loc (Ppat_alias(p, id_loc vn _loc_vn)))
  | STR("|") p':(pattern_lvl (next_pat_prio AltPat)) when lvl' >= AltPat && lvl <= AltPat ->
      (AltPat, fun p ->
        ln p _loc (Ppat_or(p, p')))
  | ps:{STR(",") p:(pattern_lvl (next_pat_prio TupPat)) -> p}+ when lvl' > TupPat && lvl <= TupPat ->
      (TupPat, fun p ->
        ln p _loc (Ppat_tuple(p::ps)))
  | c:STR("::") p':(pattern_lvl ConsPat) when lvl' > ConsPat && lvl <= ConsPat ->
      (ConsPat, fun p ->
        let cons = id_loc (Lident "::") _loc_c in
        let args = loc_pat _loc (Ppat_tuple [p; p']) in
        ln p _loc (ppat_construct(cons, Some args)))
  (* next is just for polymorphic type annotation in let ? *)
  | STR(":") ids:{STR("'") id:ident}+ STR(".") te:typexpr when lvl' >= AsPat && lvl <= AsPat ->
      (AsPat, fun p -> 
        ln p _loc (Ppat_constraint(p, loc_typ _loc (Ptyp_poly (ids, te)))))
  | STR(":") ty:typexpr when lvl' >= TopPat && lvl <= TopPat ->
      (lvl', fun p -> 
        ln p _loc (Ppat_constraint(p, ty)))
  )

let pattern_suit =
  let f =
    memoize2'
      (fun pat_suit lvl' lvl ->
         parser
         | (p1,f1):(pattern_suit_aux lvl' lvl) ->> (p2,f2):(pat_suit p1 lvl) -> (p2, fun f -> f2 (f1 f))
         | EMPTY -> (lvl', fun f -> f))
  in
  let rec res x y = f res x y in
  res

let _ = set_pattern_lvl (fun lvl ->
  parser
  | (lvl',t):(pattern_base lvl) ->> ft:(pattern_suit lvl' lvl) -> snd ft t
  )

(****************************************************************************
 * Expressions                                                              *
 ****************************************************************************)

let expression_lvls = [ Top; Let; Seq; Coerce; If; Aff; Tupl; Disj; Conj; Eq; Append; Cons; Sum; Prod; Pow; Opp; App; Dash; Dot; Prefix; Atom]

let let_prio lvl = if !modern then lvl else Let
let let_re = "\\(let\\)\\|\\(val\\)\\b"

type assoc = NoAssoc | Left | Right

let assoc = function
  Prefix | Dot | Dash | Opp -> NoAssoc
| Prod | Sum | Eq -> Left
| _ -> Right

let infix_prio s =
  match s.[0] with
  | '*' -> if String.length s > 1 && s.[1] = '*' then Pow else Prod
  | '/' | '%' -> Prod
  | '+' | '-' -> Sum
  | ':' -> if String.length s > 1 && s.[1] = '=' then Aff else Cons
  | '<' -> if String.length s > 1 && s.[1] = '-' then Aff else Eq
  | '@' | '^' -> Append
  | '&' -> if String.length s = 1 || 
		(String.length s = 2 && s.[1] = '&') then Conj else Eq
  | '|' -> if String.length s = 2 && s.[1] = '|' then Disj else Eq
  | '=' | '>' | '$' | '!' -> Eq
  | 'o' -> Disj 
  | 'm' -> Prod
  | 'a' -> Pow
  | 'l' -> (match s.[1] with 's' -> Pow | _ -> Prod)
  | _ -> Printf.printf "%s\n%!" s; assert false

let prefix_prio s =
  if s = "-" || s = "-." || s = "+" || s = "+." then Opp else Prefix

let array_function loc str name =
  let name = if !fast then "unsafe_" ^ name else name in
  loc_expr loc (Pexp_ident (id_loc (Ldot(Lident str, name)) loc ))

let bigarray_function loc str name =
  let name = if !fast then "unsafe_" ^ name else name in
  let lid = Ldot(Ldot(Lident "Bigarray", str), name) in
  loc_expr loc (Pexp_ident (id_loc lid loc))

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
  parser
  | m:{ m:module_path STR"." }? id:{id:capitalized_ident -> id | b:bool_lit -> b } ->
      match m with
      | None   -> Lident id
      | Some m -> Ldot(m, id)

let argument =
  parser
  | id:label STR(":") e:(expression_lvl (next_exp App)) -> (id, e)
  | id:opt_label STR(":") e:(expression_lvl (next_exp App)) -> ("?"^id, e)
  | id:label -> (id, loc_expr _loc (Pexp_ident(id_loc (Lident id) _loc)))
  | id:opt_label -> ("?"^id, loc_expr _loc (Pexp_ident(id_loc (Lident id) _loc)))
    (* NOTE the "id" in the first position of the couple was not prefixed with a "?". I guess this was a bug. *)
  | e:(expression_lvl (next_exp App)) -> ("", e)

let _ = set_parameter (fun allow_new_type ->
  parser
  | pat:(pattern_lvl AtomPat) -> `Arg ("", None, pat)
  | STR("~") STR("(") id:lowercase_ident t:{ STR":" t:typexpr }? STR")" -> (
      let pat =  loc_pat _loc_id (Ppat_var(id_loc id _loc_id)) in
      let pat = match t with
      | None   -> pat
      | Some t -> loc_pat _loc (Ppat_constraint (pat, t))
      in
      `Arg (id, None, pat))
  | id:label STR":" pat:pattern -> `Arg (id, None, pat)
  | CHR('~') id:ident -> `Arg (id, None, loc_pat _loc_id (Ppat_var(id_loc id _loc_id)))
  | STR("?") STR"(" id:lowercase_ident t:{ STR":" t:typexpr -> t }? e:{STR"=" e:expression -> e}? STR")" -> (
      let pat = loc_pat _loc_id (Ppat_var(id_loc id _loc_id)) in
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge2 _loc_id _loc_t) (Ppat_constraint(pat,t))
      in `Arg ("?"^id, e, pat))
  | id:opt_label STR":" STR"(" pat:pattern t:{STR(":") t:typexpr}? e:{CHR('=') e:expression}? STR")" -> (
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge2 _loc_pat _loc_t) (Ppat_constraint(pat,t))
      in `Arg ("?"^id, e, pat))
  | id:opt_label STR":" pat:pattern -> `Arg ("?"^id, None, pat)
  | id:opt_label -> `Arg ("?"^id, None, loc_pat _loc_id (Ppat_var(id_loc id _loc_id)))
  | CHR('(') type_kw name:typeconstr_name CHR(')') when allow_new_type -> `Type(name))

let apply_params params e =
  let f acc = function
    | `Arg (lbl,opt,pat), _loc' ->
      loc_expr (merge2 _loc' e.pexp_loc) (pexp_fun (lbl, opt, pat, acc))
    | `Type name, _loc' -> loc_expr (merge2 _loc' e.pexp_loc) (Pexp_newtype(name,acc))
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
  parser
  | l:{lb:(parameter true) -> lb, _loc_lb}* ty:{CHR(':') t:typexpr}? CHR('=') e:expression -> 
      let e = match ty with
	None -> e
      | Some ty -> loc_expr _loc (pexp_constraint(e, ty))
      in
      apply_params l e

let _ = set_grammar let_binding (
  parser
  | pat:(pattern_lvl AsPat) e:right_member a:post_item_attributes l:{_:and_kw let_binding}?[[]] ->
      (value_binding ~attributes:a (merge2 _loc_pat _loc_e) pat e::l)
  | vn:lowercase_ident CHR(':') ty:poly_typexpr e:right_member a:post_item_attributes l:{_:and_kw let_binding}?[[]] ->
      let pat = loc_pat _loc (Ppat_constraint(
        loc_pat _loc (Ppat_var(id_loc vn _loc_vn)),
        ty))
      in
      value_binding ~attributes:a (merge2 _loc_vn _loc_e) pat e::l
#ifversion >= 4.00
  | vn:lowercase_ident CHR(':') (ids,ty):poly_syntax_typexpr e:right_member a:post_item_attributes l:{_:and_kw let_binding}?[[]] ->
    let (e, ty) = wrap_type_annotation _loc ids ty e in 									     
    let pat = loc_pat _loc (Ppat_constraint(
	loc_pat _loc (Ppat_var(id_loc vn _loc_vn)),
        ty))
    in
    value_binding ~attributes:a (merge2 _loc_vn _loc_e) pat e::l
#endif
  | dol:CHR('$') - STR("bindings") CHR(':') e:(expression_lvl App) - CHR('$') l:{_:and_kw let_binding}?[[]] ->
     push_pop_let_binding (start_pos _loc_dol).Lexing.pos_cnum e @ l
  )

let _ = set_match_cases (fun lvl ->
  parser
  | CHR('|')? pat:pattern w:{_:when_kw expression }? STR"->" e:(expression_lvl lvl) 
      l:{CHR'|' pat:pattern  w:{_:when_kw expression }? STR"->" e:(expression_lvl lvl) -> (pat,e,w)}**
        ls:(match_cases lvl) -> map_cases ((pat,e,w)::l) @ ls
  | CHR('|')? dol:CHR('$') - STR("cases") CHR(':') e:(expression_lvl App) - CHR('$') ls:(match_cases lvl) -> push_pop_cases (start_pos _loc_dol).Lexing.pos_cnum e @ ls
  | EMPTY -> []
  )

let type_coercion =
  parser
  | STR(":") t:typexpr t':{STR(":>") t':typexpr}? -> (Some t, t')
  | STR(":>") t':typexpr -> (None, Some t')

let expression_list =
  parser
  | e:(expression_lvl (next_exp Seq)) l:{ STR(";") e:(expression_lvl (next_exp Seq)) -> (e,_loc_e)}* STR(";")? -> ((e,_loc_e)::l)
  | EMPTY -> []

let record_item = 
  parser
  | f:field CHR('=') e:(expression_lvl (next_exp Seq)) -> (id_loc f _loc_f,e) 
  | f:lowercase_ident -> (let id = id_loc (Lident f) _loc_f in id, loc_expr _loc_f (Pexp_ident(id)))

let record_list =
  parser
  | it:record_item l:{ STR(";") it:record_item }* STR(";")? -> (it::l)
  | EMPTY -> []

(****************************************************************************
 * classes and objects                                                      *
 ****************************************************************************)

let obj_item = 
  parser
  | v:inst_var_name CHR('=') e:(expression_lvl (next_exp Seq)) -> (id_loc v _loc_v, e)

(* Class expression *)

let class_expr_base =
  parser
  | cp:class_path -> 
      let cp = id_loc cp _loc_cp in
      loc_pcl _loc (Pcl_constr (cp, []))
  | CHR('[') te:typexpr tes:{STR(",") te:typexpr}* CHR(']') cp:class_path ->
      let cp = id_loc cp _loc_cp in
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

let _ = set_grammar class_expr (
  parser
  | ce:class_expr_base args:{arg:argument+}? ->
      (match args with
       | None   -> ce
       | Some l -> loc_pcl _loc (Pcl_apply (ce, l)))
  )

let class_field =
  parser
  | inherit_kw o:override_flag ce:class_expr id:{_:as_kw lowercase_ident}? ->
#ifversion >= 4.02
      loc_pcf _loc (Pcf_inherit (o, ce, id))
#else
      loc_pcf _loc (Pcf_inher (o, ce, id))
#endif
  | val_kw o:override_flag m:mutable_flag ivn:inst_var_name te:{CHR(':') t:typexpr}?
    CHR('=') e:expr ->
      let ivn = id_loc ivn _loc_ivn in
      let ex =
        match te with
        | None   -> e
        | Some t -> loc_expr _loc_te (pexp_constraint (e, t))
      in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_val (ivn, m, Cfk_concrete(o,ex)))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_val (ivn, m, o, ex))
#else
      Pcf_val (ivn, m, o, ex, _loc)
#endif
#endif
  | val_kw m:mutable_flag virtual_kw ivn:inst_var_name
    STR(":") te:typexpr ->
      let ivn = id_loc ivn _loc_ivn in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_val (ivn, m, Cfk_virtual te))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_valvirt (ivn, m, te))
#else
      Pcf_valvirt (ivn, m, te, _loc)
#endif
#endif
  | val_kw virtual_kw mutable_kw ivn:inst_var_name STR(":") te:typexpr ->
      let ivn = id_loc ivn _loc_ivn in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_val (ivn, Mutable, Cfk_virtual te))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_valvirt (ivn, Mutable, te))
#else
      Pcf_valvirt (ivn, Mutable, te, _loc)
#endif
#endif
  | method_kw o:override_flag p:private_flag mn:method_name
    STR(":") te:poly_typexpr CHR('=') e:expr ->
      let mn = id_loc mn _loc_mn in
      let e = loc_expr _loc (Pexp_poly (e, Some te)) in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_method (mn, p, Cfk_concrete(o,e)))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_meth (mn, p, o, e))
#else
      Pcf_meth (mn, p, o, e, _loc)
#endif
#endif
#ifversion >= 4.00 
  | method_kw o:override_flag p:private_flag mn:method_name
    STR(":") (ids,te):poly_syntax_typexpr CHR('=') e:expr ->
      let mn = id_loc mn _loc_mn in
      let e, poly =  wrap_type_annotation _loc ids te e in
      let e = loc_expr _loc (Pexp_poly (e, Some poly)) in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_method (mn, p, Cfk_concrete(o,e)))
#else
      loc_pcf _loc (Pcf_meth (mn, p, o, e))
#endif
#endif
  | method_kw o:override_flag p:private_flag mn:method_name ps:{p:(parameter true) -> p,_loc_p}*
    te:{STR(":") te:typexpr}? CHR('=') e:expr ->
      let mn = id_loc mn _loc_mn in
      let e = 
	match te with
	  None -> e
	| Some te ->
	   loc_expr _loc (pexp_constraint (e, te))
      in
      let e : expression = apply_params ps e in
      let e = loc_expr _loc (Pexp_poly (e, None)) in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_method (mn, p, Cfk_concrete(o,e)))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_meth (mn, p, o, e))
#else
      Pcf_meth (mn, p, o, e, _loc)
#endif
#endif
  | method_kw p:private_flag virtual_kw mn:method_name STR(":")
    pte:poly_typexpr ->
      let mn = id_loc mn _loc_mn in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_method (mn, p, Cfk_virtual(pte)))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_virt (mn, p, pte))
#else
      Pcf_virt (mn, p, pte, _loc)
#endif
#endif
  | method_kw virtual_kw private_kw mn:method_name
    STR(":") pte:poly_typexpr ->
      let mn = id_loc mn _loc_mn in
#ifversion >= 4.02
      loc_pcf _loc (Pcf_method (mn, Private, Cfk_virtual(pte)))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_virt (mn, Private, pte))
#else
      Pcf_virt (mn, Private, pte,_loc)
#endif
#endif
  | constraint_kw te:typexpr CHR('=') te':typexpr ->
#ifversion >= 4.02
      loc_pcf _loc (Pcf_constraint (te, te'))
#else
#ifversion >= 4.00
      loc_pcf _loc (Pcf_constr (te, te'))
#else
      Pcf_cstr (te, te',_loc)
#endif
#endif
  | initializer_kw e:expr ->
#ifversion >= 4.02
      loc_pcf _loc (Pcf_initializer e)
#else
      loc_pcf _loc (Pcf_init e)
#endif

let _ = set_grammar class_body (
  parser
  | p:pattern? f:class_field* -> 
      let p = match p with None -> loc_pat _loc_p Ppat_any | Some p -> p in
#ifversion >= 4.02
      { pcstr_self = p; pcstr_fields = f }
#else
#ifversion >= 4.00
      { pcstr_pat = p; pcstr_fields = f }
#else
      (p, f)
#endif
#endif
  )

(* Class definition *)
let class_binding =
  parser
  | v:virtual_flag params:{STR("[") params:type_parameters STR("]")}?[[]]
    cn:class_name ps:(parameter false)* ct:{STR(":") ct:class_type}? CHR('=')
    ce:class_expr ->
      let ce = apply_params_cls _loc ps ce in
      let ce = match ct with
               | None    -> ce
               | Some ct -> loc_pcl _loc (Pcl_constraint(ce, ct))
      in
      class_type_declaration _loc_params _loc (id_loc cn _loc_cn) params v ce

let class_definition =
  parser
  | cb:class_binding cbs:{_:and_kw class_binding}* -> (cb::cbs)

let pexp_list _loc ?loc_cl l =
  if l = [] then
    loc_expr _loc (pexp_construct(id_loc (Lident "[]") _loc, None))
  else
    let loc_cl = match loc_cl with None -> _loc | Some pos -> pos in
    List.fold_right (fun (x,pos) acc ->
		     let _loc = merge2 pos loc_cl in
		     loc_expr _loc (pexp_construct(id_loc (Lident "::") _loc, Some (loc_expr _loc (Pexp_tuple [x;acc])))))
		    l (loc_expr loc_cl (pexp_construct(id_loc (Lident "[]") loc_cl, None)))

(* Expressions *)
let extra_expressions_grammar lvl = alternatives (List.map (fun g -> g lvl) extra_expressions)
let expression_base = memoize1 (fun lvl ->
  parser
  | e:(extra_expressions_grammar lvl) -> e
  | v:inst_var_name STR("<-") e:(expression_lvl (next_exp Aff)) when lvl <= Aff->
      (Aff, loc_expr _loc (Pexp_setinstvar(id_loc v _loc_v, e)))
  | id:value_path -> (Atom, loc_expr _loc (Pexp_ident(id_loc id _loc_id)))
  | c:constant -> (Atom, loc_expr _loc (Pexp_constant c))
  | mp:module_path STR(".") STR("(") e:expression STR(")") ->
      let mp = id_loc mp _loc_mp in
#ifversion >= 4.01
      (Atom, loc_expr _loc (Pexp_open (Fresh, mp, e)))
#else
      (Atom, loc_expr _loc (Pexp_open (mp, e)))
#endif
  | let_kw r:{r:rec_flag l:let_binding in_kw e:(expression_lvl (let_prio lvl)) when (lvl < App)
                  -> fun _loc -> (Let, loc_expr _loc (Pexp_let (r, l, e)))
#ifversion >= 4.02
             | module_kw mn:module_name l:{ STR"(" mn:module_name mt:{STR":" mt:module_type}? STR ")" ->
		     (id_loc mn _loc_mn, mt, _loc)}*
#else
             | module_kw mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> (id_loc mn _loc_mn, mt, _loc)}*
#endif
                 mt:{STR":" mt:module_type }? STR"=" me:module_expr in_kw e:(expression_lvl (let_prio lvl)) when (lvl < App) ->
               let me = match mt with None -> me | Some mt -> mexpr_loc (merge2 _loc_mt _loc_me) (Pmod_constraint(me, mt)) in
               let me = List.fold_left (fun acc (mn,mt,_loc) ->
                 mexpr_loc (merge2 _loc _loc_me) (Pmod_functor(mn, mt, acc))) me (List.rev l) in
               fun _loc -> (Let, loc_expr _loc (Pexp_letmodule(id_loc mn _loc_mn, me, e)))
             | open_kw o:override_flag mp:module_path in_kw
		 e:(expression_lvl (let_prio lvl)) when (lvl < App) ->
	       let mp = id_loc mp _loc_mp in
#ifversion >= 4.01
		fun _loc -> (Let, loc_expr _loc (Pexp_open (o, mp, e)))
#else
                fun _loc -> (Let, loc_expr _loc (Pexp_open (mp, e)))
#endif
             } -> r _loc
  | function_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (pexp_function l))
  | fun_kw l:{lbl:(parameter true) -> lbl,_loc_lbl}* STR"->" e:(expression_lvl (let_prio lvl)) when (lvl < App) -> 
     (Let, loc_expr _loc (apply_params l e).pexp_desc)
  | match_kw e:expression with_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_match(e, l)))
  | try_kw e:expression with_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_try(e, l)))
  | if_kw c:expression then_kw e:(expression_lvl If) e':{_:else_kw (expression_lvl If)}? when (lvl < App) ->
     (If, loc_expr _loc (Pexp_ifthenelse(c,e,e')))
  | STR("(") e:expression? STR(")") -> (Atom, match e with Some e -> loc_expr _loc e.pexp_desc | None ->
      let cunit = id_loc (Lident "()") _loc in
      loc_expr _loc (pexp_construct(cunit, None)))
  | begin_kw e:expression? end_kw -> (Atom, match e with Some e -> e | None ->
      let cunit = id_loc (Lident "()") _loc in
      loc_expr _loc (pexp_construct(cunit, None)))
  | c:constructor e:{ e:(expression_lvl (next_exp App)) when lvl <= App }? -> (App, loc_expr _loc (pexp_construct(id_loc c _loc_c, e)))
  | assert_kw e:{ false_kw -> pexp_assertfalse _loc | e:(expression_lvl App) -> Pexp_assert(e)} when (lvl <= App) 
      -> (App,  loc_expr _loc e)
  | lazy_kw e:(expression_lvl App) when (lvl <= App) -> (App,  loc_expr _loc (Pexp_lazy(e)))
  | l:tag_name e:(expression_lvl (next_exp App)) when (lvl <= App) -> (App, loc_expr _loc (Pexp_variant(l,Some e)))
  | l:tag_name -> (Atom, loc_expr _loc (Pexp_variant(l,None)))
  | STR("[|") l:expression_list STR("|]") -> (Atom, loc_expr _loc (Pexp_array (List.map fst l)))
  | STR("[") l:expression_list cl:STR("]") ->
	(Atom, loc_expr _loc (pexp_list _loc ~loc_cl:_loc_cl l).pexp_desc)
  | STR("{") e:{(expression_lvl (next_exp Seq)) _:with_kw}? l:record_list STR("}") ->
      (Atom, loc_expr _loc (Pexp_record(l,e)))
  | while_kw e:expression do_kw e':expression done_kw ->
      (Atom, loc_expr _loc (Pexp_while(e, e')))
#ifversion >= 4.02
  | for_kw id:pattern CHR('=') e:expression d:downto_flag
    e':expression do_kw e'':expression done_kw ->
      (Atom, loc_expr _loc (Pexp_for(id, e, e', d, e'')))
#else
  | for_kw id:lowercase_ident CHR('=') e:expression d:downto_flag
    e':expression do_kw e'':expression done_kw ->
      (Atom, loc_expr _loc (Pexp_for(id_loc id _loc_id, e, e', d, e'')))
#endif
  | new_kw p:class_path -> (Atom, loc_expr _loc (Pexp_new(id_loc p _loc_p)))
  | object_kw o:class_body end_kw -> (Atom, loc_expr _loc (Pexp_object o))
  | STR("{<") l:{ o:obj_item l:{STR";" o:obj_item}* STR(";")? -> o::l }?[[]] STR(">}") -> (Atom, loc_expr _loc (Pexp_override l))
  | STR("(") module_kw me:module_expr pt:{STR(":") pt:package_type}? STR(")") ->
      let desc = match pt with
#ifversion >= 4.0
                 | None    -> Pexp_pack me
                 | Some pt -> let me = loc_expr _loc_me (Pexp_pack me) in
                              let pt = loc_typ _loc_pt pt in
                              pexp_constraint (me, pt)
#else
                 | Some(Ptyp_package(n,l)) -> Pexp_pack(me,(n,l))
                 | _    -> raise (Give_up "" (* FIXME *))
#endif
      in
      (Atom, loc_expr _loc desc)
  | STR("<:") name:{ STR("expr") -> "expression"     | STR("type") -> "type" | STR("pat") -> "pattern"
 		   | STR("structure") -> "structure" | STR("signature") -> "signature"
		   | STR("constructors") -> "constructors" 
		   | STR("fields") -> "fields"
		   | STR("bindings") -> "let_binding"
		   | STR("cases") -> "cases"
		   | STR("module") ty:STR("type")? -> if ty = None then "module_expr" else "module_type"
		   }
       loc:{CHR('@') e:(expression_lvl App) }? CHR('<') q:quotation ->
       if loc = None then push_location "";
       (Atom, quote_expression _loc_q loc q name)
  | CHR('$') - c:capitalized_ident -> 
     Atom, (match c with
      | "FILE" -> 
	 loc_expr _loc (Pexp_constant (const_string ((start_pos _loc).Lexing.pos_fname)))
      | "LINE" ->
	 loc_expr _loc (Pexp_constant (Const_int ((start_pos _loc).Lexing.pos_lnum)))
      | _ ->
	 try let str = Sys.getenv c in
	     parse_string ~filename:("ENV:"^c) expression blank str
	 with Not_found -> raise (Give_up "" (* FIXME *)))
  | dol:CHR('$') - t:{t:{ STR("tuple") -> "tuple" | 
		    STR("list") -> "list" |
		    STR("array") -> "array" } CHR(':') }? 
       e:(expression_lvl App) - CHR('$') ->
	 (match t with
	   None ->
	   (Atom, push_pop_expression (start_pos _loc_dol).Lexing.pos_cnum e)
	 | Some str ->
	    let l = push_pop_expression_list (start_pos _loc_dol).Lexing.pos_cnum e in
	    match str with
	    | "tuple" -> (Atom, loc_expr _loc (Pexp_tuple l))
	    | "array" -> (Atom, loc_expr _loc (Pexp_array l))
	    | "list" ->   
	       let l = List.map (fun x -> x,_loc) l in
	       (Atom, loc_expr _loc (pexp_list _loc l).pexp_desc)
	    | _ -> raise (Give_up "" (* FIXME *)))
  | p:prefix_symbol ->> let lvl' = prefix_prio p in e:(expression_lvl lvl') when lvl <= lvl' -> 
     (lvl', mk_unary_opp p _loc_p e _loc_e)
  )

let apply_lbl _loc (lbl, e) =
  let e = match e with
      None -> loc_expr _loc (Pexp_ident(id_loc (Lident lbl) _loc ))
    | Some e -> e
  in (lbl, e)

let rec mk_seq = function
    [] -> assert false
  | [e] -> e
  | x::l -> 
     let res = mk_seq l in
     loc_expr (merge2 x.pexp_loc res.pexp_loc) (Pexp_sequence(x,res))

let semi_col = black_box 
  (fun str pos ->
   let c,str',pos' = read str pos in
   if c = ';' then
     let c',_,_ = read str' pos' in
     if c' = ';' then raise (Give_up "" (* FIXME *))
     else (), str', pos'
   else
     raise (Give_up "" (* FIXME *)))
  (Charset.singleton ';') false (";")

let double_semi_col = black_box 
  (fun str pos ->
   let c,str',pos' = read str pos in
   if c = ';' then
     let c',_,_ = read str' pos' in
     if c' <> ';' then raise (Give_up "" (* FIXME *))
     else (), str', pos'
   else
     raise (Give_up "" (* FIXME *)))
  (Charset.singleton ';') false (";;")

let extra_expression_suits_grammar lvl' lvl = alternatives (List.map (fun g -> g lvl' lvl) extra_expression_suits)
let expression_suit_aux = memoize2 (fun lvl' lvl ->
  let ln f _loc e = loc_expr (merge2 f.pexp_loc _loc) e in
  parser
  | e:(extra_expression_suits_grammar lvl' lvl) -> e
  | l:{STR(",") e:(expression_lvl (next_exp Tupl))}+ when (lvl' > Tupl && lvl <= Tupl) -> 
      (Tupl, fun f -> ln f _loc (Pexp_tuple(f::l)))
  | t:type_coercion when (lvl' > Coerce && lvl <= Coerce) ->
      (Seq, fun e' -> ln e' _loc (
			   match t with Some t1, None -> pexp_constraint(e', t1)
				      | t1, Some t2 -> pexp_coerce(e', t1, t2)
				      | None, None -> assert false))
  | l:{_:semi_col (expression_lvl (next_exp Seq))}+ when (lvl' > Seq && lvl <= Seq) -> 
      (Seq, fun f -> mk_seq (f::l))
  | semi_col when (lvl' >= Seq && lvl <= Seq) -> (Seq, fun e -> e)
  | STR(".") r:{ STR("(") f:expression STR(")") STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' > Aff && lvl <= Aff) -> 
      (Aff, fun e' -> ln e' _loc (Pexp_apply(array_function (merge2 e'.pexp_loc _loc) "Array" "set",[("",e');("",f);("",e)]))) 
  |            STR("(") f:expression STR(")") when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> ln e' _loc (Pexp_apply(array_function (merge2 e'.pexp_loc _loc) "Array" "get",[("",e');("",f)])))
  |            STR("[") f:expression STR("]") STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' -> ln e' _loc (Pexp_apply(array_function (merge2 e'.pexp_loc _loc) "String" "set",[("",e');("",f);("",e)]))) 
  |            STR("[") f:expression STR("]")  when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> ln e' _loc (Pexp_apply(array_function (merge2 e'.pexp_loc _loc) "String" "get",[("",e');("",f)])))
  |            STR("{") f:expression STR("}") STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' -> bigarray_set (merge2 e'.pexp_loc _loc) e' f e)
  |            STR("{") f:expression STR("}") when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> bigarray_get (merge2 e'.pexp_loc _loc) e' f)
  |            f:field STR("<-") e:(expression_lvl (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' ->
              let f = id_loc f _loc_f in loc_expr _loc (Pexp_setfield(e',f,e)))
  |            f:field when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' ->
              let f = id_loc f _loc_f in loc_expr _loc (Pexp_field(e',f))) } -> r
  | STR("#") f:method_name when (lvl' >= Dash && lvl <= Dash) -> 
      (Dash, fun e' -> ln e' _loc (Pexp_send(e',f)))
  | l:{a:argument}+ when (lvl' > App && lvl <= App) -> 
      (App, fun f -> ln f _loc (Pexp_apply(f,l)))
  | op:infix_op ->> let p = infix_prio op in let a = assoc p in 
                    e:(expression_lvl (if a = Right then p else next_exp p))
                      when lvl <= p && (lvl' > p || (a = Left && lvl' = p)) ->
      (p, fun e' -> ln e' e.pexp_loc (
          if op = "::" then
            pexp_construct(id_loc (Lident "::") _loc_op, Some (ln e' _loc (Pexp_tuple [e';e])))
          else 
            Pexp_apply(loc_expr _loc_op (Pexp_ident(id_loc (Lident op) _loc_op)),
                     [("", e') ; ("", e)])))
  )

let expression_suit =
  let f =
    memoize2'
      (fun expression_suit lvl' lvl ->
         parser
          (p1,f1):(expression_suit_aux lvl' lvl) ->> (p2,f2):(expression_suit p1 lvl)
	       -> (p2, fun f -> f2 (f1 f))
      | EMPTY -> (lvl', fun f -> f))
  in
  let rec res x y = f res x y in
  res

let _ = set_expression_lvl (fun lvl ->
    parser
      (lvl',e):(expression_base lvl) ->> (_, f):(expression_suit lvl' lvl) -> f e
    )

(****************************************************************************
 * Module expressions (module implementations)                              *
 ****************************************************************************)

let module_expr_base = 
  parser
  | mp:module_path ->
      let mid = id_loc mp _loc in
      mexpr_loc _loc (Pmod_ident mid)
  | struct_kw ms:structure end_kw -> 
      mexpr_loc _loc (Pmod_structure(ms))
#ifversion >= 4.02
  | functor_kw STR("(") mn:module_name mt:{STR(":") mt:module_type}? STR(")")
#else
  | functor_kw STR("(") mn:module_name STR(":") mt:module_type STR(")")
#endif
    STR("->") me:module_expr -> mexpr_loc _loc (Pmod_functor(id_loc mn _loc_mn, mt, me))
  | STR("(") me:module_expr mt:{STR(":") mt:module_type}? STR(")") ->
      (match mt with
       | None    -> me
       | Some mt -> mexpr_loc _loc (Pmod_constraint (me, mt)))
  | STR("(") val_kw e:expr pt:{STR(":") pt:package_type}? STR(")") ->
      let e = match pt with
#ifversion >= 4.00
              | None    -> Pmod_unpack e
              | Some pt -> let pt = loc_typ _loc_pt pt in
                           Pmod_unpack (loc_expr _loc (pexp_constraint (e, pt)))
#else
                 | Some(Ptyp_package(n,l)) -> Pmod_unpack(e,(n,l))
                 | _    -> raise (Give_up "" (* FIXME *))
#endif
      in
      mexpr_loc _loc e
  | dol:CHR('$') - e:(expression_lvl App) - CHR('$') -> push_pop_module_expr (start_pos _loc_dol).Lexing.pos_cnum e
		     
let _ = set_grammar module_expr (
  parser
    m:module_expr_base l:{STR("(") m:module_expr STR(")") -> (_loc, m)}* ->
      List.fold_left (fun acc (_loc_n, n) -> mexpr_loc (merge2 _loc_m _loc_n) (Pmod_apply(acc, n))) m l
  )

let module_type_base = 
  parser
  | mp:modtype_path ->
      let mid = id_loc mp _loc in
      mtyp_loc _loc (Pmty_ident mid)
  | sig_kw ms:signature end_kw -> 
     mtyp_loc _loc (Pmty_signature(ms))
#ifversion >= 4.02
  | functor_kw STR("(") mn:module_name mt:{STR(":") mt:module_type}? STR(")")
#else
  | functor_kw STR("(") mn:module_name STR(":") mt:module_type STR(")")
#endif
     STR("->") me:module_type -> mtyp_loc _loc (Pmty_functor(id_loc mn _loc_mn, mt, me))
  | STR("(") mt:module_type STR(")") -> mt
  | module_kw type_kw of_kw me:module_expr -> mtyp_loc _loc (Pmty_typeof me)
  | dol:CHR('$') - e:(expression_lvl App) - CHR('$') -> push_pop_module_type (start_pos _loc_dol).Lexing.pos_cnum e

let mod_constraint = 
  parser
  | t:type_kw ->> (tn,ty):(typedef_in_constraint _loc_t) ->
#ifversion >= 4.02		    
     Pwith_type(tn,ty)
#else
     (tn, Pwith_type(ty))
#endif
  | module_kw m1:module_path CHR('=') m2:extended_module_path ->
     let name = id_loc m1 _loc_m1 in
#ifversion >= 4.02		    
     Pwith_module(name, id_loc m2 _loc_m2 )
#else
     (name, Pwith_module(id_loc m2 _loc_m2 ))
#endif
  | type_kw tps:type_params?[[]] tcn:typeconstr_name STR(":=") te:typexpr ->
#ifversion < 4.00
      let tps = List.map (function
			     (Some s, t) -> s,t | None, _ -> raise (Give_up "" (* FIXME *))) tps in
#endif
      let td = type_declaration _loc (id_loc tcn _loc_tcn)
	   tps [] Ptype_abstract Public (Some te) in
#ifversion >= 4.02		    
      Pwith_typesubst td
#else
      (id_loc (Lident tcn) _loc_tcn, Pwith_typesubst td)
#endif
  | module_kw mn:module_name STR(":=") emp:extended_module_path ->
#ifversion >= 4.02		    
     Pwith_modsubst(id_loc mn _loc_mn, id_loc emp _loc_emp)
#else
     (id_loc (Lident mn) _loc_mn, Pwith_modsubst(id_loc emp _loc_emp))
#endif

let _ = set_grammar module_type (
  parser
    m:module_type_base l:{_:with_kw m:mod_constraint l:{_:and_kw mod_constraint}* -> m::l } ? ->
      (match l with
         None -> m
       | Some l -> mtyp_loc _loc (Pmty_with(m, l)))
  )
      
let structure_item_base =
  parser
  | RE(let_re) r:rec_flag l:let_binding ->
      (match l with
#ifversion >= 4.02
       | [{pvb_pat = {ppat_desc = Ppat_any}; pvb_expr = e}] -> pstr_eval e
#else
       | [({ppat_desc = Ppat_any; ppat_loc = _}, e)] -> pstr_eval e
#endif
       | _                                           -> Pstr_value (r, l))
  | external_kw n:value_name STR":" ty:typexpr STR"=" ls:string_literal* ->
      let l = List.length ls in
      if l < 1 || l > 3 then raise (Give_up "" (* FIXME *));
#ifversion >= 4.02
      Pstr_primitive({ pval_name = id_loc n _loc_n; pval_type = ty; pval_prim = ls; pval_loc = _loc; pval_attributes = [] })
#else
      Pstr_primitive(id_loc n _loc_n, { pval_type = ty; pval_prim = ls; 
#ifversion >= 4.00
					pval_loc = _loc
#endif
				      })
#endif
#ifversion >= 4.02
  | td:type_definition -> Pstr_type (List.map snd td)
#else
  | td:type_definition -> Pstr_type td
#endif
  | ex:exception_definition -> ex
#ifversion >= 4.02
  | module_kw r:{rec_kw mn:module_name mt:{STR(":") mt:module_type}? CHR('=')
    me:module_expr ms:{and_kw mn:module_name mt:{STR(":") mt:module_type}? CHR('=')
#else
  | module_kw r:{rec_kw mn:module_name STR(":") mt:module_type CHR('=')
    me:module_expr ms:{and_kw mn:module_name STR(":") mt:module_type CHR('=')
#endif
    me:module_expr -> (module_binding _loc (id_loc mn _loc_mn) mt me)}* ->
      let m = (module_binding _loc (id_loc mn _loc_mn) mt me) in
      Pstr_recmodule (m::ms)
#ifversion >= 4.02
  |            mn:module_name l:{ STR"(" mn:module_name mt:{STR":" mt:module_type }? STR")" -> (id_loc mn _loc_mn, mt, _loc)}*
#else
  |            mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> (id_loc mn _loc_mn, mt, _loc)}*
#endif
     mt:{STR":" mt:module_type }? STR"=" me:module_expr ->
    let me = match mt with None -> me | Some mt -> mexpr_loc (merge2 _loc_mt _loc_me) (Pmod_constraint(me,mt)) in
     let me = List.fold_left (fun acc (mn,mt,_loc) ->
       mexpr_loc (merge2 _loc _loc_me) (Pmod_functor(mn, mt, acc))) me (List.rev l) in
#ifversion >= 4.02
     Pstr_module(module_binding _loc (id_loc mn _loc_mn) None me)
#else
     let (name, _, me) = module_binding _loc (id_loc mn _loc_mn) None me in	   
     Pstr_module(name,me)
#endif
#ifversion >= 4.02
  |            type_kw mn:modtype_name mt:{STR"=" mt:module_type}? ->
      Pstr_modtype{pmtd_name = id_loc mn _loc_mn; pmtd_type = mt; pmtd_attributes = []; pmtd_loc = _loc }
#else
  |            type_kw mn:modtype_name STR"=" mt:module_type ->
      Pstr_modtype(id_loc mn _loc_mn, mt) 
#endif
               } -> r
  | open_kw o:override_flag m:module_path ->
#ifversion >= 4.02
    Pstr_open{ popen_lid = id_loc m _loc_m; popen_override = o; popen_loc = _loc; popen_attributes = []}
#else
#ifversion >= 4.01
    Pstr_open(o, id_loc m _loc_m)
#else
    Pstr_open(id_loc m _loc_m)
#endif
#endif
  | include_kw me:module_expr ->
#ifversion >= 4.02
    Pstr_include {pincl_mod = me; pincl_loc = _loc; pincl_attributes = [] }
#else
    Pstr_include me
#endif
  | class_kw r:{ ctd:classtype_definition -> Pstr_class_type ctd
               | cds:class_definition -> Pstr_class cds } -> r
  | e:expression -> pstr_eval e

let _ = set_grammar structure_item (
  parser
  | e:(alternatives extra_structure) -> e
  | dol:CHR('$') - e:(expression_lvl App) - CHR('$') STR(";;")? -> push_pop_structure (start_pos _loc_dol).Lexing.pos_cnum e
  | s:structure_item_base STR(";;")? -> [loc_str _loc_s s]
  )

let signature_item_base =
 parser
  | val_kw n:value_name STR(":") ty:typexpr a:post_item_attributes ->
     psig_value ~attributes:a _loc (id_loc n _loc_n) ty []
  | external_kw n:value_name STR":" ty:typexpr STR"=" ls:string_literal* a:post_item_attributes ->
      let l = List.length ls in
      if l < 1 || l > 3 then raise (Give_up "" (* FIXME *));
      psig_value ~attributes:a _loc (id_loc n _loc_n) ty ls
  | td:type_definition -> 
#ifversion >= 4.02
       Psig_type (List.map snd td)
#else
       Psig_type td
#endif
  | (name,ed,_loc'):exception_declaration -> 
#ifversion >= 4.02
       Psig_exception (Te.decl ~loc:_loc' ~args:ed name)
#else
       Psig_exception (name, ed)
#endif
  | module_kw rec_kw mn:module_name STR(":") mt:module_type
    ms:{and_kw mn:module_name STR(":") mt:module_type -> (module_declaration _loc (id_loc mn _loc_mn) mt)}* ->
      let m = (module_declaration _loc (id_loc mn _loc_mn) mt) in
      Psig_recmodule (m::ms)
#ifversion >= 4.02		
  | module_kw r:{mn:module_name l:{ STR"(" mn:module_name mt:{STR":" mt:module_type}? STR ")" -> (id_loc mn _loc_mn, mt, _loc)}*
#else
  | module_kw r:{mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> (id_loc mn _loc_mn, mt, _loc)}*
#endif
                                    STR":" mt:module_type ->
     let mt = List.fold_left (fun acc (mn,mt,_loc) ->
                                  mtyp_loc (merge2 _loc _loc_mt) (Pmty_functor(mn, mt, acc))) mt (List.rev l) in
#ifversion >= 4.02
     Psig_module(module_declaration _loc (id_loc mn _loc_mn) mt)
#else
     let a, b = module_declaration _loc (id_loc mn _loc_mn) mt in
     Psig_module(a,b)
#endif
  |           type_kw mn:modtype_name mt:{ STR"=" mt:module_type }? ->
#ifversion >= 4.02
     Psig_modtype{pmtd_name = id_loc mn _loc_mn; pmtd_type = mt; pmtd_attributes = []; pmtd_loc = _loc}
#else
     let mt = match mt with
              | None    -> Pmodtype_abstract
              | Some mt -> Pmodtype_manifest mt
     in 
     Psig_modtype(id_loc mn _loc_mn, mt)
#endif
		} -> r
  | open_kw o:override_flag m:module_path ->
#ifversion >= 4.02
    Psig_open{ popen_lid = id_loc m _loc_m; popen_override = o; popen_loc = _loc; popen_attributes = []}
#else
#ifversion >= 4.01
    Psig_open(o, id_loc m _loc_m)
#else
    Psig_open(id_loc m _loc_m)
#endif
#endif
  | include_kw me:module_type ->
#ifversion >= 4.02
    Psig_include {pincl_mod = me; pincl_loc = _loc; pincl_attributes = [] }
#else
    Psig_include me
#endif
  | class_kw r:{ ctd:classtype_definition -> Psig_class_type ctd
               | cs:class_specification -> Psig_class cs } -> r

let _ = set_grammar signature_item (
  parser
  | e:(alternatives extra_signature) -> e
  | dol:CHR('$') - e:(expression_lvl App) - CHR('$') -> push_pop_signature (start_pos _loc_dol).Lexing.pos_cnum e
  | s:signature_item_base STR(";;")? -> [loc_sig _loc s]
  )

exception Top_Exit

let top_phrase = 
  parser
  | CHR(';')? l:{s:structure_item_base -> loc_str _loc s}+ (double_semi_col) -> Ptop_def(l)
  | CHR(';')? EOF -> raise Top_Exit

end



