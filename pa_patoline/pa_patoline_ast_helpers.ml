(* Helpers to build Parsetree fragments without camlp4 quotations (earley 3.0). *)

open Pa_ast
open Ast_helper
open Asttypes
open Longident
open Parsetree
open Location
(* Compat shim for OCaml 5.2+: [Ast_helper.fun_] was removed.
   Route every `fun pat -> body` site through [Pa_parser.fun_]. *)
let fun_ = Pa_parser.fun_

let idl _loc txt = id_loc_longident (Lident txt) _loc
(* Build a [Longident.t with_loc] for a 2-part dotted path.  Plain
   [Ldot (Lident m, f)] no longer type-checks in OCaml 5.2+ because
   [Ldot] now takes per-part locations. *)
let dot2 _loc m f =
  let mk t = { loc = _loc; txt = t } in
  { loc = _loc;
    txt = Longident.Ldot (mk (Longident.Lident m), mk f) }

(* Build a plain [Longident.t] (not with_loc) with proper per-part locs.
   Use this when callers expect [Longident.t] but construct [Ldot] which
   now requires its parts to be [loc]'d. *)
let longident_dot2 _loc m f =
  let mk_loc t = { loc = _loc; txt = t } in
  Longident.Ldot (mk_loc (Longident.Lident m), { loc = _loc; txt = f })

(* Back-compat: returns a [Longident.t with_loc] of the same shape as
   [dot2].  Kept under the original name so existing call sites can move
   over gradually. *)
let iddot _loc m f = dot2 _loc m f

(* Wrap an arbitrary [Longident.t] into a [Longident.t with_loc] suitable for
   OCaml 5.2+ AST payloads.  Each inner part of [Ldot]/[Lapply] receives the
   supplied [_loc]. *)
let lid_wrap _loc (li : Longident.t) : Longident.t Location.loc =
  let loc = _loc in
  let mk_loc t = { loc; txt = t } in
  let rec loop = function
    | Longident.Lident _ as l -> l
    | Longident.Ldot ({ txt = l; _ }, { txt = s; _ }) ->
      Longident.Ldot (mk_loc (loop l), mk_loc s)
    | Longident.Lapply ({ txt = a; _ }, { txt = b; _ }) ->
      Longident.Lapply (mk_loc (loop a), mk_loc (loop b))
  in
  { loc; txt = loop li }

(* Same as [exp_lident] but takes an already-wrapped [Longident.t with_loc]. *)
let exp_lident_l _loc li =
  loc_expr _loc (Pexp_ident li)

(* Build a 2-part [Longident.t with_loc] from string components.  Used for
   dotted paths whose parts are variables rather than literals. *)
let lid_loc_dot _loc m f =
  let mk_loc t = { loc = _loc; txt = t } in
  { Location.loc = _loc;
    txt = Longident.Ldot (mk_loc (Longident.Lident m), { loc = _loc; txt = f }) }

(* Build a [Longident.t with_loc] for a 3-part dotted path (X.Y.Z). *)
let dot3 _loc m f g =
  let mk t = { loc = _loc; txt = t } in
  let inner = (dot2 _loc m f).txt in
  { Location.loc = _loc;
    txt = Longident.Ldot (mk inner, mk g) }


let str_let_any _loc e =
  Str.value ~loc:_loc Nonrecursive [
    Vb.mk (Pat.any ~loc:_loc ()) e
  ]

let str_let_pat _loc pat e =
  Str.value ~loc:_loc Nonrecursive [
    Vb.mk pat e
  ]

let exp_bang _loc e =
  Exp.apply ~loc:_loc (Exp.ident ~loc:_loc (idl _loc "!")) [ (Nolabel, e) ]

let exp_infix _loc op e1 e2 =
  Exp.apply ~loc:_loc (Exp.ident ~loc:_loc (idl _loc op))
    [ (Nolabel, e1); (Nolabel, e2) ]

let str_open_lid _loc (lid : Longident.t) =
  Str.open_ ~loc:_loc (Opn.mk ~loc:_loc (Mod.ident ~loc:_loc (lid_wrap _loc lid)))

let exp_mod_open _loc m e =
  Exp.open_ ~loc:_loc
    (Opn.mk ~loc:_loc (Mod.ident ~loc:_loc (idl _loc m)))
    e

let real_name_fold _loc mid mp =
  List.fold_left (fun acc m -> exp_mod_open _loc m acc) (exp_ident _loc mid) mp

let maths_glyphs_str _loc s =
  exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "Maths" "glyphs")) (exp_string _loc s)

let ordinary_list_node _loc node_e =
  exp_list _loc [
    exp_const_longident _loc (dot2 _loc "Maths" "Ordinary") (Some node_e)
  ]

(** [Maths.Ordinary (Maths.node (fun x y -> List.flatten (Maths.multi_glyphs dv x y)))] as list expr *)
let ordinary_multi_glyphs_node _loc dv =
  let fun_xy =
    fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "x" })
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "y" })
         (exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "List" "flatten"))
            (exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "multi_glyphs"))
               [ dv; exp_ident _loc "x"; exp_ident _loc "y" ])))
  in
  ordinary_list_node _loc
    (exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "Maths" "node")) fun_xy)

let ordinary_hd_glyphs_node _loc dv =
  let fun_xy =
    fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "x" })
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "y" })
         (exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "List" "hd"))
            (exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "multi_glyphs"))
               [ dv; exp_ident _loc "x"; exp_ident _loc "y" ])))
  in
  ordinary_list_node _loc
    (exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "Maths" "node")) fun_xy)

let maths_normal _loc nsl md nsr =
  exp_const_longident _loc (dot2 _loc "Maths" "Normal")
    (Some (exp_tuple _loc [ exp_bool _loc nsl; md; exp_bool _loc nsr ]))

(** Spacing cell between quantifier arguments / invisible infix (cf. [print_math_symbol Invisible]). *)
let maths_invisible _loc =
  let glyph_node =
    exp_apply1 _loc
      (exp_ident_longident _loc (dot2 _loc "Maths" "node"))
      (maths_glyphs_str _loc "invisible")
  in
  maths_normal _loc true glyph_node true

let maths_bin _loc prio nd left right =
  exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "bin")) [
    exp_int _loc prio;
    nd;
    left;
    right;
  ]

let maths_ordinary_cell _loc md =
  exp_list _loc [
    exp_const_longident _loc (dot2 _loc "Maths" "Ordinary") (Some md)
  ]

let quantifier_nested_bin _loc md inter d_expr m_expr =
  let inner = maths_bin _loc 1 inter d_expr m_expr in
  exp_list _loc [
    maths_bin _loc 3 (maths_normal _loc true md true) (exp_list _loc [])
      (exp_list _loc [ inner ]);
  ]

let maths_op_limits _loc deco m_expr =
  exp_list _loc [
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "op_limits")) [
      exp_list _loc [];
      deco;
      m_expr;
    ]
  ]

let maths_op_nolimits _loc deco m_expr =
  exp_list _loc [
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "op_nolimits")) [
      exp_list _loc [];
      deco;
      m_expr;
    ]
  ]

let maths_fraction _loc l r =
  exp_list _loc [
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "fraction")) [ l; r ]
  ]

let maths_binary_record _loc sp inter l r =
  exp_list _loc [
    Exp.construct ~loc:_loc (iddot _loc "Maths" "Binary")
      (Some
         (Exp.record ~loc:_loc
            [
              (idl _loc "bin_priority", exp_int _loc sp);
              (idl _loc "bin_drawing", inter);
              (idl _loc "bin_left", l);
              (idl _loc "bin_right", r);
            ]
            None))
  ]

let maths_decoration _loc l r m =
  exp_list _loc [
    exp_const_longident _loc (dot2 _loc "Maths" "Decoration")
      (Some
         (exp_tuple _loc [
            exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "open_close")) [ l; r ];
            m;
          ]))
  ]

let inline_math_kdraw_bb _loc m =
  let kbody =
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "kdraw")) [
      exp_list _loc [ exp_ident _loc "env0" ];
      m;
    ]
  in
  exp_list _loc [
    exp_apply1 _loc (exp_ident _loc "bB")
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "env0" }) kbody);
  ]

let display_math_kdraw_bb _loc m =
  let inner = exp_apply1 _loc (exp_ident _loc "displayStyle") m in
  let kbody =
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "kdraw")) [
      exp_list _loc [ exp_ident _loc "env0" ];
      inner;
    ]
  in
  exp_list _loc [
    exp_apply1 _loc (exp_ident _loc "bB")
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "env0" }) kbody);
  ]

let paragraph_newpar_struct _loc p ~indented =
  let d_struct = Exp.ident ~loc:_loc (iddot _loc "D" "structure") in
  let env =
    fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "x" })
      (Exp.record ~loc:_loc
         [ (idl _loc "par_indent", exp_Nil _loc) ]
         (Some (Exp.ident ~loc:_loc (idl _loc "x"))))
  in
  let args =
    if indented then
      [
        (Nolabel, exp_bang _loc d_struct);
        (Nolabel, Exp.ident ~loc:_loc (iddot _loc "Complete" "normal"));
        (Nolabel, Exp.ident ~loc:_loc (iddot _loc "Patoline_Format" "parameters"));
        (Nolabel, p);
      ]
    else
      [
        (Nolabel, exp_bang _loc d_struct);
        (Labelled "environment", env);
        (Nolabel, Exp.ident ~loc:_loc (iddot _loc "Complete" "normal"));
        (Nolabel, Exp.ident ~loc:_loc (iddot _loc "Patoline_Format" "parameters"));
        (Nolabel, p);
      ]
  in
  let rhs = Exp.apply ~loc:_loc (Exp.ident ~loc:_loc (idl _loc "newPar")) args in
  [ str_let_any _loc (exp_infix _loc ":=" d_struct rhs) ]

let array_get_lid _loc nom idx =
  exp_apply2 _loc (exp_ident_longident _loc (dot2 _loc "Array" "get"))
    (exp_ident _loc nom) (exp_int _loc idx)

let struct_filter_default_expr _loc (s : structure) =
  let me = Mod.structure ~loc:_loc s in
  let drawing_arg =
    Exp.apply ~loc:_loc
      (Exp.ident ~loc:_loc (iddot _loc "Res" "drawing"))
      [ (Nolabel, Exp.construct ~loc:_loc (idl _loc "()") None) ]
  in
  let list_inner =
    exp_list _loc
      [ Exp.construct ~loc:_loc (idl _loc "Drawing") (Some drawing_arg) ]
  in
  let body =
    Exp.letmodule ~loc:_loc { loc = _loc; txt = Some "Res" } me list_inner
  in
  let f = fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "env" }) body in
  exp_list _loc [ exp_apply1 _loc (exp_ident _loc "bB") f ]

let struct_filter_diagram_expr _loc (s : structure) =
  let functor_arg =
    Mod.structure ~loc:_loc [
      Str.value ~loc:_loc Nonrecursive [
        Vb.mk
          (Pat.var ~loc:_loc { loc = _loc; txt = "env" })
          (Exp.ident ~loc:_loc (idl _loc "env"))
      ]
    ]
  in
  let diagram_me =
    Mod.apply ~loc:_loc
      (Mod.ident ~loc:_loc (idl _loc "MakeDiagram"))
      functor_arg
  in
  let mb_diagram =
    Mb.mk ~loc:_loc { loc = _loc; txt = Some "Diagram" } diagram_me
  in
  let res_mod =
    Mod.structure ~loc:_loc (
      Str.module_ ~loc:_loc mb_diagram
      :: Str.open_ ~loc:_loc (Opn.mk ~loc:_loc (Mod.ident ~loc:_loc (idl _loc "Diagram")))
      :: s
    )
  in
  let make_drawing =
    Exp.apply ~loc:_loc
      (Exp.ident ~loc:_loc (dot3 _loc "Res" "Diagram" "make"))
      [ (Nolabel, Exp.construct ~loc:_loc (idl _loc "()") None) ]
  in
  let list_inner =
    exp_list _loc
      [ Exp.construct ~loc:_loc (idl _loc "Drawing") (Some make_drawing) ]
  in
  let body =
    Exp.letmodule ~loc:_loc { loc = _loc; txt = Some "Res" } res_mod list_inner
  in
  let f = fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "env" }) body in
  exp_list _loc [ exp_apply1 _loc (exp_ident _loc "bB") f ]

(** Record fields for [Maths] superscripts (with [r] accumulator in caller). *)
let maths_field_super_right_same_script _loc =
  (iddot _loc "Maths" "super_right_same_script", exp_bool _loc true)

let maths_field_superscript_right _loc e =
  (iddot _loc "Maths" "superscript_right", e)

let maths_field_subscript_right _loc e =
  (iddot _loc "Maths" "subscript_right", e)

let maths_field_super_left_same_script _loc =
  (iddot _loc "Maths" "super_left_same_script", exp_bool _loc true)

let maths_field_superscript_left _loc e =
  (iddot _loc "Maths" "superscript_left", e)

let maths_field_subscript_left _loc e =
  (iddot _loc "Maths" "subscript_left", e)

let print_math_deco_sym_finish _loc (fields : (Longident.t Location.loc * expression) list) node_inner =
  let rec_e = loc_expr _loc (Pexp_record (fields, Some node_inner)) in
  Exp.construct ~loc:_loc (iddot _loc "Maths" "Ordinary") (Some rec_e)

let print_math_deco_finish _loc (fields : (Longident.t Location.loc * expression) list) elt =
  let draw_inner =
    Exp.apply ~loc:_loc
      (Exp.apply ~loc:_loc
         (exp_ident_longident _loc (dot2 _loc "Maths" "draw"))
         [ (Nolabel, exp_list _loc [ exp_ident _loc "env" ]) ])
      [ (Nolabel, elt) ]
  in
  let node_fun =
    fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "env" })
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "st" })
         draw_inner)
  in
  let node_inner = exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "Maths" "node")) node_fun in
  let rec_e = loc_expr _loc (Pexp_record (fields, Some node_inner)) in
  exp_list _loc [ Exp.construct ~loc:_loc (iddot _loc "Maths" "Ordinary") (Some rec_e) ]

(** Verbose symbol paragraph: [newPar ...] with display maths row. *)
let symbol_paragraph_structure _loc ~syms ~names : structure =
  let d_struct = Exp.ident ~loc:_loc (iddot _loc "D" "structure") in
  let env_fun =
    fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "x" })
      (Exp.record ~loc:_loc
         [ (idl _loc "par_indent", exp_Nil _loc) ]
         (Some (Exp.ident ~loc:_loc (idl _loc "x"))))
  in
  let left_arrow =
    exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "Maths" "glyphs"))
      (exp_string _loc "\226\135\144")
  in
  let normal_inner =
    Exp.construct ~loc:_loc (iddot _loc "Maths" "Normal")
      (Some
         (exp_tuple _loc [
            exp_bool _loc false;
            exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "Maths" "node")) left_arrow;
            exp_bool _loc false;
          ]))
  in
  let bin_expr =
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "bin")) [
      exp_int _loc 0;
      normal_inner;
      syms;
      names;
    ]
  in
  let env0_record =
    Exp.record ~loc:_loc
      [ (idl _loc "mathStyle",
         Exp.construct ~loc:_loc
           (dot2 _loc "Mathematical" "Display")
           None) ]
      (Some (Exp.ident ~loc:_loc (idl _loc "env0")))
  in
  let kdraw_env_list = exp_list _loc [ env0_record ] in
  let inner_list = exp_list _loc [ bin_expr ] in
  let kdraw_app =
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "kdraw")) [
      kdraw_env_list;
      inner_list;
    ]
  in
  let bb =
    exp_apply1 _loc (exp_ident _loc "bB")
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "env0" })
         kdraw_app)
  in
  let par_list = exp_list _loc [ bb ] in
  let newpar_app =
    Exp.apply ~loc:_loc (Exp.ident ~loc:_loc (idl _loc "newPar")) [
      (Nolabel, exp_bang _loc d_struct);
      (Labelled "environment", env_fun);
      (Nolabel, Exp.ident ~loc:_loc (iddot _loc "Complete" "normal"));
      (Nolabel, Exp.ident ~loc:_loc (iddot _loc "Patoline_Format" "parameters"));
      (Nolabel, par_list);
    ]
  in
  let assign = exp_infix _loc ":=" d_struct newpar_app in
  [ str_let_any _loc assign ]

(* --- write_main.ml generator (Pprintast in pa_patoline) --- *)

let write_main_structure _loc ~dcache ~driver ~form ~m ~full : structure =
  let opens =
    List.map (str_open_lid _loc) [
      Lident "Patoraw";
      Lident "Typography";
      longident_dot2 _loc "Typography" "Box";
      longident_dot2 _loc "Typography" "Document";
      Lident "RawContent";
      Lident "Color";
    ]
  in
  let read_cache =
    str_let_any _loc
      (exp_apply1 _loc
         (exp_ident_longident _loc (dot2 _loc "Distance" "read_cache"))
         (exp_string _loc dcache))
  in
  let node_tags =
    exp_list _loc [
      exp_tuple _loc [ exp_string _loc "intoc"; exp_string _loc "" ]
    ]
  in
  let record_node =
    Exp.record ~loc:_loc
      [ (idl _loc "node_tags", node_tags) ]
      (Some (Exp.ident ~loc:_loc (idl _loc "empty")))
  in
  let node_tree =
    Exp.construct ~loc:_loc (idl _loc "Node") (Some record_node)
  in
  let tuple_state =
    exp_tuple _loc [ node_tree; exp_Nil _loc ]
  in
  let ref_expr =
    exp_apply1 _loc (Exp.ident ~loc:_loc (idl _loc "ref")) tuple_state
  in
  let d_inner =
    Str.value ~loc:_loc Nonrecursive [
      Vb.mk
        (Pat.var ~loc:_loc { loc = _loc; txt = "structure" })
        ref_expr
    ]
  in
  let d_mod =
    Mod.constraint_ ~loc:_loc
      (Mod.structure ~loc:_loc [ d_inner ])
      (Mty.ident ~loc:_loc (idl _loc "DocumentStructure"))
  in
  let d_binding =
    Str.module_ ~loc:_loc (Mb.mk ~loc:_loc { loc = _loc; txt = Some "D" } d_mod)
  in
  let driver_mod =
    Str.module_ ~loc:_loc
      (Mb.mk ~loc:_loc { loc = _loc; txt = Some "Driver" }
         (Mod.ident ~loc:_loc (id_loc_longident (Lident driver) _loc)))
  in
  (* Synthesize an [Arg.align]-wrapped list of options that override
     [output_path] when the user passes [-o] / [--output]. The ref
     [output_path] is initialized to [~full] in [output_ref_init]
     below. We splice this list into the generated [Arg.parse_argv]
     call so the override is handled at the top level (driver-agnostic). *)
  let assign_output_path s =
    exp_apply2 _loc (exp_ident _loc ":=")
      (Exp.ident ~loc:_loc (idl _loc "output_path"))
      (Exp.ident ~loc:_loc (idl _loc s))
  in
  let arg_string_setter =
    Exp.construct ~loc:_loc (iddot _loc "Arg" "String") (Some
      (fun_ ~loc:_loc Nolabel None
         (Pat.var ~loc:_loc { loc = _loc; txt = "s" })
         (assign_output_path "s")))
  in
  let entry name doc =
    Exp.tuple ~loc:_loc
    (List.map (fun e -> (None, e))
       [ exp_string _loc name; arg_string_setter; exp_string _loc doc ])
  in
  let output_spec =
    exp_apply _loc
      (exp_ident_longident _loc (dot2 _loc "Arg" "align"))
      [
        exp_list _loc [
          entry "-o" "<file> Output path";
          entry "--output" "<file> Output path";
        ]
      ]
  in
  let driver_opts_app =
    exp_infix _loc "@"
      (exp_infix _loc "@"
         (exp_infix _loc "@"
            output_spec
            (Exp.ident ~loc:_loc (iddot _loc "Driver" "driver_options")))
         (Exp.ident ~loc:_loc (iddot _loc "DefaultFormat" "spec")))
      (exp_list _loc [])
  in
  let output_ref_init =
    str_let_pat _loc
      (Pat.var ~loc:_loc { loc = _loc; txt = "output_path" })
      (exp_apply1 _loc
         (exp_ident_longident _loc (dot2 _loc "Stdlib" "ref"))
         (exp_string _loc full))
  in
  let parse_argv_e =
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Arg" "parse_argv")) [
      exp_apply1 _loc
        (exp_ident_longident _loc (dot2 _loc "Driver" "filter_options"))
        (Exp.ident ~loc:_loc (iddot _loc "Sys" "argv"));
      driver_opts_app;
      Exp.ident ~loc:_loc (idl _loc "ignore");
      exp_string _loc "Usage :";
    ]
  in
  let parse_argv = str_let_any _loc parse_argv_e in
  let pf0_me =
    Mod.apply ~loc:_loc
      (Mod.ident ~loc:_loc
         ((lid_loc_dot _loc form "Format")))
      (Mod.ident ~loc:_loc (idl _loc "D"))
  in
  let pf0 = Str.module_ ~loc:_loc (Mb.mk ~loc:_loc { loc = _loc; txt = Some "Patoline_Format0" } pf0_me) in
  let open_pf0 = str_open_lid _loc (Lident "Patoline_Format0") in
  let pf_alias =
    Str.module_ ~loc:_loc
      (Mb.mk ~loc:_loc { loc = _loc; txt = Some "Patoline_Format" }
         (Mod.ident ~loc:_loc (idl _loc "Patoline_Format0")))
  in
  let out_me =
    Mod.apply ~loc:_loc
      (Mod.ident ~loc:_loc
         (dot2 _loc "Patoline_Format0" "Output"))
      (Mod.ident ~loc:_loc (idl _loc "Driver"))
  in
  let out_mod =
    Str.module_ ~loc:_loc
      (Mb.mk ~loc:_loc { loc = _loc; txt = Some "Patoline_Output" } out_me)
  in
  let tmp_me =
    Mod.apply ~loc:_loc
      (Mod.apply ~loc:_loc
         (Mod.ident ~loc:_loc
            ((lid_loc_dot _loc m "Document")))
         (Mod.ident ~loc:_loc (idl _loc "Patoline_Output")))
      (Mod.ident ~loc:_loc (idl _loc "D"))
  in
  let tmp_mod =
    Str.module_ ~loc:_loc (Mb.mk ~loc:_loc { loc = _loc; txt = Some "TMP" } tmp_me)
  in
  let open_tmp = str_open_lid _loc (Lident "TMP") in
  let fold_fun =
    fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "acc" })
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "f" })
         (exp_apply1 _loc (exp_ident _loc "f") (exp_ident _loc "acc")))
  in
  let output_call =
    Exp.apply ~loc:_loc
      (exp_ident_longident _loc (dot2 _loc "Patoline_Output" "output"))
      [
        (Nolabel,
         Exp.ident ~loc:_loc (iddot _loc "Patoline_Output" "outputParams"));
        (Nolabel,
         exp_apply1 _loc (exp_ident _loc "fst")
           (exp_apply1 _loc (exp_ident _loc "top")
              (exp_bang _loc (Exp.ident ~loc:_loc (iddot _loc "D" "structure")))));
        (Nolabel,
         exp_apply _loc (exp_ident_longident _loc (dot2 _loc "List" "fold_left")) [
           fold_fun;
           Exp.ident ~loc:_loc (iddot _loc "Patoline_Format" "defaultEnv");
           exp_bang _loc (Exp.ident ~loc:_loc (idl _loc "init_env_hook"));
         ]);
        (Nolabel,
         exp_apply1 _loc (exp_ident _loc "!")
           (Exp.ident ~loc:_loc (idl _loc "output_path")));
      ]
  in
  let output_stmt = str_let_any _loc output_call in
  let write_cache =
    str_let_any _loc
      (exp_apply1 _loc
         (exp_ident_longident _loc (dot2 _loc "Distance" "write_cache"))
         (exp_string _loc dcache))
  in
  opens @ [
    read_cache;
    d_binding;
    driver_mod;
    output_ref_init;
    parse_argv;
    pf0;
    open_pf0;
    pf_alias;
    out_mod;
    tmp_mod;
    open_tmp;
    output_stmt;
    write_cache;
  ]

let wrap_document_structure _loc ~basename ~patoline_format ~cache_arr ~mcache_arr ~ast :
    structure =
  let opens =
    List.map (str_open_lid _loc) [
      Lident "Patoraw";
      Lident "Typography";
      longident_dot2 _loc "Typography" "Box";
      longident_dot2 _loc "Typography" "Document";
      longident_dot2 _loc "Typography" "Maths";
      Lident "RawContent";
      Lident "Color";
      Lident "Driver";
      Lident "DefaultMacros";
    ]
  in
  let cache_name = "cache_" ^ basename in
  let mcache_name = "mcache_" ^ basename in
  let let_cache =
    Str.value ~loc:_loc Nonrecursive [
      Vb.mk
        (Pat.var ~loc:_loc { loc = _loc; txt = cache_name })
        cache_arr
    ]
  in
  let let_mcache =
    Str.value ~loc:_loc Nonrecursive [
      Vb.mk
        (Pat.var ~loc:_loc { loc = _loc; txt = mcache_name })
        mcache_arr
    ]
  in
  let pf_me =
    Mod.apply ~loc:_loc
      (Mod.ident ~loc:_loc
         ((lid_loc_dot _loc patoline_format "Format")))
      (Mod.ident ~loc:_loc (idl _loc "D"))
  in
  let pf_mod =
    Str.module_ ~loc:_loc
      (Mb.mk ~loc:_loc { loc = _loc; txt = Some "Patoline_Format" } pf_me)
  in
  let open_pf = str_open_lid _loc (Lident patoline_format) in
  let open_pf2 = str_open_lid _loc (Lident "Patoline_Format") in
  let temp1 =
    Str.value ~loc:_loc Nonrecursive [
      Vb.mk
        (Pat.var ~loc:_loc { loc = _loc; txt = "temp1" })
        (exp_apply _loc (exp_ident_longident _loc (dot2 _loc "List" "map")) [
           exp_ident _loc "fst";
           exp_apply1 _loc (exp_ident _loc "snd")
             (exp_bang _loc (Exp.ident ~loc:_loc (iddot _loc "D" "structure")));
         ])
    ]
  in
  let follow_stmt =
    str_let_any _loc
      (exp_infix _loc ":="
         (Exp.ident ~loc:_loc (iddot _loc "D" "structure"))
         (exp_apply _loc (exp_ident _loc "follow") [
            exp_apply1 _loc (exp_ident _loc "top")
              (exp_bang _loc (Exp.ident ~loc:_loc (iddot _loc "D" "structure")));
            exp_apply1 _loc (exp_ident_longident _loc (dot2 _loc "List" "rev"))
              (exp_ident _loc "temp1");
          ]))
  in
  let fp_out =
    Named
      ( { loc = _loc; txt = Some "Patoline_Output" },
        Mty.ident ~loc:_loc
          (dot2 _loc "DefaultFormat" "Output") )
  in
  let fp_d =
    Named
      ( { loc = _loc; txt = Some "D" },
        Mty.ident ~loc:_loc (idl _loc "DocumentStructure") )
  in
  let inner =
    Mod.structure ~loc:_loc
      (let_cache :: let_mcache :: pf_mod :: open_pf :: open_pf2 :: temp1 :: ast @ [ follow_stmt ])
  in
  let inner = Mod.functor_ ~loc:_loc fp_d inner in
  let inner = Mod.functor_ ~loc:_loc fp_out inner in
  let doc_mod =
    Str.module_ ~loc:_loc
      (Mb.mk ~loc:_loc { loc = _loc; txt = Some "Document" } inner)
  in
  opens @ [ doc_mod ]

let str_module_ _loc name me =
  Str.module_ ~loc:_loc
    (Mb.mk ~loc:_loc { loc = _loc; txt = Some name } me)

let exp_unit _loc =
  Exp.construct ~loc:_loc (idl _loc "()") None

let exp_ident_ldot _loc m field =
  Exp.ident ~loc:_loc (lid_loc_dot _loc m field)

let include_document_structure _loc ~temp_id ~id : structure =
  let doc_me =
    Mod.apply ~loc:_loc
      (Mod.apply ~loc:_loc
         (Mod.ident ~loc:_loc
            ((lid_loc_dot _loc id "Document")))
         (Mod.ident ~loc:_loc (idl _loc "Patoline_Output")))
      (Mod.ident ~loc:_loc (idl _loc "D"))
  in
  [ str_module_ _loc temp_id doc_me; str_open_lid _loc (Lident temp_id) ]

let macrouid_with_args_structure _loc ~m1 ~m2 ~id ~(ts : expression list) :
    structure =
  let inner_struct =
    List.concat_map
      (fun t ->
        [
          Str.value ~loc:_loc Nonrecursive [
            Vb.mk
              (Pat.var ~loc:_loc { loc = _loc; txt = "arg1" })
              t;
          ];
        ])
      ts
  in
  let m2_mod = Mod.structure ~loc:_loc inner_struct in
  let m1_me =
    Mod.apply ~loc:_loc
      (Mod.ident ~loc:_loc (idl _loc id))
      (Mod.ident ~loc:_loc (idl _loc m2))
  in
  [
    str_module_ _loc m2 m2_mod;
    str_module_ _loc m1 m1_me;
    str_let_any _loc
      (exp_apply1 _loc (exp_ident_ldot _loc m1 "do_begin_env") (exp_unit _loc));
    str_let_any _loc
      (exp_apply1 _loc (exp_ident_ldot _loc m1 "do_end_env") (exp_unit _loc));
  ]

let macrouid_plain_structure _loc ~m1 ~id : structure =
  [
    str_module_ _loc m1 (Mod.ident ~loc:_loc (idl _loc id));
    str_let_any _loc
      (exp_apply1 _loc (exp_ident_ldot _loc m1 "do_begin_env") (exp_unit _loc));
    str_let_any _loc
      (exp_apply1 _loc (exp_ident_ldot _loc m1 "do_end_env") (exp_unit _loc));
  ]

let begin_env_paragraph_structure _loc ~m1 ~m2 ~idb
    ~(args : expression list) ~(ps : bool -> structure) ~indent_first : structure =
  let arg_items =
    if args = [] then []
    else
      let arg_struct_items =
        List.concat_map
          (fun (i, e) ->
            let id = Printf.sprintf "arg%i" (i + 1) in
            [
              Str.value ~loc:_loc Nonrecursive [
                Vb.mk (Pat.var ~loc:_loc { loc = _loc; txt = id }) e;
              ];
            ])
          (List.mapi (fun i e -> (i, e)) args)
      in
      let arg_mod_name = "Arg_" ^ m2 in
      [
        str_module_ _loc arg_mod_name
          (Mod.structure ~loc:_loc arg_struct_items);
      ]
  in
  let env_name = "Env_" ^ idb in
  let def_me =
    if args = [] then Mod.ident ~loc:_loc (idl _loc env_name)
    else
      Mod.apply ~loc:_loc
        (Mod.ident ~loc:_loc (idl _loc env_name))
        (Mod.ident ~loc:_loc (idl _loc ("Arg_" ^ m2)))
  in
  let def_item = str_module_ _loc m2 def_me in
  let inner_items =
    arg_items
    @ [
        def_item;
        str_open_lid _loc (Lident m2);
        str_let_any _loc
          (exp_apply1 _loc (exp_ident_ldot _loc m2 "do_begin_env") (exp_unit _loc));
      ]
    @ ps indent_first
    @ [
        str_let_any _loc
          (exp_apply1 _loc (exp_ident_ldot _loc m2 "do_end_env") (exp_unit _loc));
      ]
  in
  [ str_module_ _loc m1 (Mod.structure ~loc:_loc inner_items) ]

let display_math_paragraph_structure _loc (m : expression) : structure =
  let d_struct = Exp.ident ~loc:_loc (iddot _loc "D" "structure") in
  let env_fun =
    fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "x" })
      (Exp.record ~loc:_loc
         [ (idl _loc "par_indent", exp_Nil _loc) ]
         (Some (Exp.ident ~loc:_loc (idl _loc "x"))))
  in
  let env0_record =
    Exp.record ~loc:_loc
      [
        ( idl _loc "mathStyle",
          Exp.construct ~loc:_loc
            (dot2 _loc "Mathematical" "Display")
            None );
      ]
      (Some (Exp.ident ~loc:_loc (idl _loc "env0")))
  in
  let kdraw_env_list = exp_list _loc [ env0_record ] in
  let kdraw_app =
    exp_apply _loc (exp_ident_longident _loc (dot2 _loc "Maths" "kdraw")) [
      kdraw_env_list;
      m;
    ]
  in
  let bb =
    exp_apply1 _loc (exp_ident _loc "bB")
      (fun_ ~loc:_loc Nolabel None (Pat.var ~loc:_loc { loc = _loc; txt = "env0" })
         kdraw_app)
  in
  let par_list = exp_list _loc [ bb ] in
  let newpar_app =
    Exp.apply ~loc:_loc (Exp.ident ~loc:_loc (idl _loc "newPar")) [
      (Nolabel, exp_bang _loc d_struct);
      (Labelled "environment", env_fun);
      (Nolabel, Exp.ident ~loc:_loc (iddot _loc "Complete" "normal"));
      (Nolabel, Exp.ident ~loc:_loc (idl _loc "displayedFormula"));
      (Nolabel, par_list);
    ]
  in
  [ str_let_any _loc (exp_infix _loc ":=" d_struct newpar_app) ]

let section_text_structure_in_toc _loc ~in_toc ~num ~title ~(body : structure) :
    structure =
  let d_struct = Exp.ident ~loc:_loc (iddot _loc "D" "structure") in
  let assign =
    exp_infix _loc ":=" d_struct
      (Exp.apply ~loc:_loc (Exp.ident ~loc:_loc (idl _loc "newStruct")) [
         (Labelled "in_toc", exp_bool _loc in_toc);
         (Labelled "numbered", exp_bool _loc num);
         (Nolabel, exp_bang _loc d_struct);
         (Nolabel, title);
       ])
  in
  str_let_any _loc assign :: body
  @ [
      str_let_any _loc
        (exp_apply1 _loc (exp_ident _loc "go_up")
           (Exp.ident ~loc:_loc (iddot _loc "D" "structure")));
    ]

let section_text_structure_numbered _loc ~num ~title ~(body : structure) :
    structure =
  let d_struct = Exp.ident ~loc:_loc (iddot _loc "D" "structure") in
  let assign =
    exp_infix _loc ":=" d_struct
      (Exp.apply ~loc:_loc (Exp.ident ~loc:_loc (idl _loc "newStruct")) [
         (Labelled "numbered", exp_bool _loc num);
         (Nolabel, exp_bang _loc d_struct);
         (Nolabel, title);
       ])
  in
  str_let_any _loc assign :: body
  @ [
      str_let_any _loc
        (exp_apply1 _loc (exp_ident _loc "go_up")
           (Exp.ident ~loc:_loc (iddot _loc "D" "structure")));
    ]
