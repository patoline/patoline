(* pa_ast.ml - AST construction helpers for pa_patoline.
   Replaces the quotation syntax (<:expr<...>>, <:struct<...>>, <:record<...>>)
   that was available in earley 2.0.0 but removed in earley 3.x. *)

open Ast_helper
open Parsetree
open Asttypes
open Longident

let mknoloc txt = Location.{txt; loc = none}
let mkloc txt loc = Location.{txt; loc}

(* Expression helpers *)
let loc_expr _loc desc =
  {pexp_desc = desc; pexp_loc = _loc; pexp_loc_stack = []; pexp_attributes = []}

let exp_ident _loc lid =
  Exp.ident ~loc:_loc (mkloc lid _loc)

let exp_lid _loc s = exp_ident _loc (Lident s)
let exp_uid _loc s = exp_ident _loc (Lident s)

let exp_dot _loc m field = exp_ident _loc (Ldot (Lident m, field))
let exp_dot2 _loc m1 m2 field = exp_ident _loc (Ldot (Ldot (Lident m1, m2), field))

let exp_string _loc s = Exp.constant ~loc:_loc (Const.string s)
let exp_int _loc i = Exp.constant ~loc:_loc (Const.int i)
let exp_float _loc f = Exp.constant ~loc:_loc (Const.float f)

let exp_bool _loc b =
  Exp.construct ~loc:_loc (mkloc (Lident (string_of_bool b)) _loc) None

let exp_unit _loc =
  Exp.construct ~loc:_loc (mkloc (Lident "()") _loc) None

let exp_none _loc =
  Exp.construct ~loc:_loc (mkloc (Lident "None") _loc) None

let exp_some _loc e =
  Exp.construct ~loc:_loc (mkloc (Lident "Some") _loc) (Some e)

let exp_nil _loc =
  Exp.construct ~loc:_loc (mkloc (Lident "[]") _loc) None

let exp_cons _loc hd tl =
  Exp.construct ~loc:_loc (mkloc (Lident "::") _loc)
    (Some (Exp.tuple ~loc:_loc [hd; tl]))

let exp_list _loc es =
  List.fold_right (exp_cons _loc) es (exp_nil _loc)

let exp_array _loc es = Exp.array ~loc:_loc es

let exp_apply _loc f args =
  Exp.apply ~loc:_loc f (List.map (fun a -> (Nolabel, a)) args)

let exp_apply1 _loc f a = exp_apply _loc f [a]
let exp_apply2 _loc f a b = exp_apply _loc f [a; b]
let exp_apply3 _loc f a b c = exp_apply _loc f [a; b; c]

let exp_fun _loc pat body =
  Exp.fun_ ~loc:_loc Nolabel None pat body

let exp_fun_s _loc s body =
  exp_fun _loc (Pat.var ~loc:_loc (mkloc s _loc)) body

let exp_let _loc ?(r=Nonrecursive) name e body =
  Exp.let_ ~loc:_loc r
    [Vb.mk ~loc:_loc (Pat.var ~loc:_loc (mkloc name _loc)) e]
    body

let exp_let_mod _loc name me body =
  Exp.letmodule ~loc:_loc (mkloc (Some name) _loc) me body

let exp_sequence _loc e1 e2 = Exp.sequence ~loc:_loc e1 e2

let exp_tuple _loc es = Exp.tuple ~loc:_loc es

let exp_record _loc fields base =
  Exp.record ~loc:_loc fields base

let exp_field _loc modpath field_name value =
  (mkloc (Ldot (Lident modpath, field_name)) _loc, value)

let exp_open _loc m e =
  Exp.open_ ~loc:_loc
    (Opn.mk ~loc:_loc (Mod.ident ~loc:_loc (mkloc (Lident m) _loc)))
    e

let exp_construct _loc lid arg =
  Exp.construct ~loc:_loc (mkloc lid _loc) arg

let exp_match _loc e cases = Exp.match_ ~loc:_loc e cases

let exp_setfield _loc e field v =
  Exp.setfield ~loc:_loc e (mkloc field _loc) v

let exp_field_access _loc e field =
  Exp.field ~loc:_loc e (mkloc field _loc)

let exp_array_get _loc arr idx =
  exp_apply2 _loc (exp_ident _loc (Ldot (Lident "Array", "get"))) arr idx

let exp_infix _loc op l r =
  exp_apply2 _loc (exp_lid _loc op) l r

let exp_prefix _loc op e =
  exp_apply1 _loc (exp_lid _loc op) e

let exp_deref _loc e = exp_prefix _loc "!" e

let exp_assign _loc e v =
  Exp.setfield ~loc:_loc e (mkloc (Lident "contents") _loc) v

let exp_ref_assign _loc lhs rhs =
  exp_apply2 _loc (exp_lid _loc ":=") lhs rhs

(* Structure helpers *)
let str_eval _loc e = [Str.eval ~loc:_loc e]

let str_let _loc ?(r=Nonrecursive) name e =
  [Str.value ~loc:_loc r [Vb.mk ~loc:_loc (Pat.var ~loc:_loc (mkloc name _loc)) e]]

let str_let_wild _loc e =
  [Str.value ~loc:_loc Nonrecursive [Vb.mk ~loc:_loc (Pat.any ~loc:_loc ()) e]]

let str_module _loc name me =
  [Str.module_ ~loc:_loc (Mb.mk ~loc:_loc (mkloc (Some name) _loc) me)]

let str_open _loc m =
  [Str.open_ ~loc:_loc (Opn.mk ~loc:_loc (Mod.ident ~loc:_loc (mkloc (Lident m) _loc)))]

let str_open_lid _loc lid =
  [Str.open_ ~loc:_loc (Opn.mk ~loc:_loc (Mod.ident ~loc:_loc (mkloc lid _loc)))]

(* Module expression helpers *)
let mod_ident _loc lid = Mod.ident ~loc:_loc (mkloc lid _loc)
let mod_apply _loc me1 me2 = Mod.apply ~loc:_loc me1 me2
let mod_structure _loc str = Mod.structure ~loc:_loc str

(* Pattern helpers *)
let pat_var _loc s = Pat.var ~loc:_loc (mkloc s _loc)
let pat_any _loc = Pat.any ~loc:_loc ()
let pat_unit _loc = Pat.construct ~loc:_loc (mkloc (Lident "()") _loc) None

(* Maths-specific helpers *)
let maths_lid _loc field = exp_ident _loc (Ldot (Lident "Maths", field))
let maths_construct _loc cname arg =
  exp_construct _loc (Ldot (Lident "Maths", cname)) arg

let maths_node _loc sym =
  exp_apply1 _loc (maths_lid _loc "node") sym

let maths_glyphs _loc s =
  exp_apply1 _loc (maths_lid _loc "glyphs") (exp_string _loc s)

let maths_ordinary_node _loc sym =
  exp_list _loc [maths_construct _loc "Ordinary" (Some (maths_node _loc sym))]

let maths_bin _loc prio drawing left right =
  exp_list _loc [exp_apply _loc (maths_lid _loc "bin")
    [exp_int _loc prio; drawing; left; right]]

let maths_normal _loc nsl md nsr =
  maths_construct _loc "Normal" (Some (exp_tuple _loc [exp_bool _loc nsl; md; exp_bool _loc nsr]))

let maths_invisible _loc =
  maths_construct _loc "Invisible" None

let maths_fraction _loc l r =
  exp_list _loc [exp_apply2 _loc (maths_lid _loc "fraction") l r]

let maths_record_field _loc field_name value =
  (mkloc (Ldot (Lident "Maths", field_name)) _loc, value)

(* Location merge helper *)
let merge2 l1 l2 =
  Location.{loc_start = l1.loc_start; loc_end = l2.loc_end; loc_ghost = false}
