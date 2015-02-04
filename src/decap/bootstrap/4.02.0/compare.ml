open Asttypes
open Parsetree
open Longident

(* Generic functions *)
let eq_option eq o1 o2 =
  match o1, o2 with
  | None   , None    -> true
  | Some e1, Some e2 -> eq e1 e2
  | _ -> false

let eq_list eq l1 l2 =
  try List.for_all2 eq l1 l2 with Invalid_argument _ -> false

let rec eq_longident i1 i2 =
  match i1, i2 with
  | Lident s1     , Lident s2      -> s1 = s2
  | Ldot (e1,s1)  , Ldot (e2, s2)  -> eq_longident e1 e2 && s1 = s2
  | Lapply (e1,f1), Lapply (e2,f2) -> eq_longident e1 e2 && eq_longident f1 f2
  | _ -> false

(* asttypes.mli *)
let eq_constant c1 c2 =
  match c1, c2 with
  | Const_int(x), Const_int(y) -> (=) x y
  | Const_char(x), Const_char(y) -> (=) x y
  | Const_string(x1,x2), Const_string(y1,y2) -> true && ((=) x1 y1) && ((eq_option (=)) x2 y2)  | Const_float(x), Const_float(y) -> (=) x y
  | Const_int32(x), Const_int32(y) -> (=) x y
  | Const_int64(x), Const_int64(y) -> (=) x y
  | Const_nativeint(x), Const_nativeint(y) -> (=) x y
  | _, _ -> false
let eq_rec_flag c1 c2 =
  match c1, c2 with
  | Nonrecursive, Nonrecursive -> true
  | Recursive, Recursive -> true
  | _, _ -> false
let eq_direction_flag c1 c2 =
  match c1, c2 with
  | Upto, Upto -> true
  | Downto, Downto -> true
  | _, _ -> false
let eq_private_flag c1 c2 =
  match c1, c2 with
  | Private, Private -> true
  | Public, Public -> true
  | _, _ -> false
let eq_mutable_flag c1 c2 =
  match c1, c2 with
  | Immutable, Immutable -> true
  | Mutable, Mutable -> true
  | _, _ -> false
let eq_virtual_flag c1 c2 =
  match c1, c2 with
  | Virtual, Virtual -> true
  | Concrete, Concrete -> true
  | _, _ -> false
let eq_override_flag c1 c2 =
  match c1, c2 with
  | Override, Override -> true
  | Fresh, Fresh -> true
  | _, _ -> false
let eq_closed_flag c1 c2 =
  match c1, c2 with
  | Closed, Closed -> true
  | Open, Open -> true
  | _, _ -> false
let eq_label c1 c2 = (=) c1 c2
let eq_loc : 'a. ('a -> 'a -> bool) -> 'a loc -> 'a loc -> bool = fun eq_a r1 r2 -> true && eq_a r1.txt r2.txt && (fun _ _ -> true) r1.loc r2.loc
let eq_variance c1 c2 =
  match c1, c2 with
  | Covariant, Covariant -> true
  | Contravariant, Contravariant -> true
  | Invariant, Invariant -> true
  | _, _ -> false

(* parsetree.mli *)
let rec eq_attribute c1 c2 = (fun (x1,x2) (y1,y2) -> true && ((eq_loc (=)) x1 y1) && (eq_payload x2 y2)) c1 c2
and eq_extension c1 c2 = (fun (x1,x2) (y1,y2) -> true && ((eq_loc (=)) x1 y1) && (eq_payload x2 y2)) c1 c2
and eq_attributes c1 c2 = (eq_list eq_attribute) c1 c2
and eq_payload c1 c2 =
  match c1, c2 with
  | PStr(x), PStr(y) -> eq_structure x y
  | PTyp(x), PTyp(y) -> eq_core_type x y
  | PPat(x1,x2), PPat(y1,y2) -> true && (eq_pattern x1 y1) && ((eq_option eq_expression) x2 y2)  | _, _ -> false
and eq_core_type = fun r1 r2 -> true && eq_core_type_desc r1.ptyp_desc r2.ptyp_desc && (fun _ _ -> true) r1.ptyp_loc r2.ptyp_loc && eq_attributes r1.ptyp_attributes r2.ptyp_attributes
and eq_core_type_desc c1 c2 =
  match c1, c2 with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var(x), Ptyp_var(y) -> (=) x y
  | Ptyp_arrow(x1,x2,x3), Ptyp_arrow(y1,y2,y3) -> true && (eq_label x1 y1) && (eq_core_type x2 y2) && (eq_core_type x3 y3)  | Ptyp_tuple(x), Ptyp_tuple(y) -> (eq_list eq_core_type) x y
  | Ptyp_constr(x1,x2), Ptyp_constr(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_list eq_core_type) x2 y2)  | Ptyp_object(x1,x2), Ptyp_object(y1,y2) -> true && ((eq_list (fun (x1,x2,x3) (y1,y2,y3) -> true && ((=) x1 y1) && (eq_attributes x2 y2) && (eq_core_type x3 y3))) x1 y1) && (eq_closed_flag x2 y2)  | Ptyp_class(x1,x2), Ptyp_class(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_list eq_core_type) x2 y2)  | Ptyp_alias(x1,x2), Ptyp_alias(y1,y2) -> true && (eq_core_type x1 y1) && ((=) x2 y2)  | Ptyp_variant(x1,x2,x3), Ptyp_variant(y1,y2,y3) -> true && ((eq_list eq_row_field) x1 y1) && (eq_closed_flag x2 y2) && ((eq_option (eq_list eq_label)) x3 y3)  | Ptyp_poly(x1,x2), Ptyp_poly(y1,y2) -> true && ((eq_list (=)) x1 y1) && (eq_core_type x2 y2)  | Ptyp_package(x), Ptyp_package(y) -> eq_package_type x y
  | Ptyp_extension(x), Ptyp_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_package_type c1 c2 = (fun (x1,x2) (y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_list (fun (x1,x2) (y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && (eq_core_type x2 y2))) x2 y2)) c1 c2
and eq_row_field c1 c2 =
  match c1, c2 with
  | Rtag(x1,x2,x3,x4), Rtag(y1,y2,y3,y4) -> true && (eq_label x1 y1) && (eq_attributes x2 y2) && ((=) x3 y3) && ((eq_list eq_core_type) x4 y4)  | Rinherit(x), Rinherit(y) -> eq_core_type x y
  | _, _ -> false
and eq_pattern = fun r1 r2 -> true && eq_pattern_desc r1.ppat_desc r2.ppat_desc && (fun _ _ -> true) r1.ppat_loc r2.ppat_loc && eq_attributes r1.ppat_attributes r2.ppat_attributes
and eq_pattern_desc c1 c2 =
  match c1, c2 with
  | Ppat_any, Ppat_any -> true
  | Ppat_var(x), Ppat_var(y) -> (eq_loc (=)) x y
  | Ppat_alias(x1,x2), Ppat_alias(y1,y2) -> true && (eq_pattern x1 y1) && ((eq_loc (=)) x2 y2)  | Ppat_constant(x), Ppat_constant(y) -> eq_constant x y
  | Ppat_interval(x1,x2), Ppat_interval(y1,y2) -> true && (eq_constant x1 y1) && (eq_constant x2 y2)  | Ppat_tuple(x), Ppat_tuple(y) -> (eq_list eq_pattern) x y
  | Ppat_construct(x1,x2), Ppat_construct(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_option eq_pattern) x2 y2)  | Ppat_variant(x1,x2), Ppat_variant(y1,y2) -> true && (eq_label x1 y1) && ((eq_option eq_pattern) x2 y2)  | Ppat_record(x1,x2), Ppat_record(y1,y2) -> true && ((eq_list (fun (x1,x2) (y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && (eq_pattern x2 y2))) x1 y1) && (eq_closed_flag x2 y2)  | Ppat_array(x), Ppat_array(y) -> (eq_list eq_pattern) x y
  | Ppat_or(x1,x2), Ppat_or(y1,y2) -> true && (eq_pattern x1 y1) && (eq_pattern x2 y2)  | Ppat_constraint(x1,x2), Ppat_constraint(y1,y2) -> true && (eq_pattern x1 y1) && (eq_core_type x2 y2)  | Ppat_type(x), Ppat_type(y) -> (eq_loc eq_longident) x y
  | Ppat_lazy(x), Ppat_lazy(y) -> eq_pattern x y
  | Ppat_unpack(x), Ppat_unpack(y) -> (eq_loc (=)) x y
  | Ppat_exception(x), Ppat_exception(y) -> eq_pattern x y
  | Ppat_extension(x), Ppat_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_expression = fun r1 r2 -> true && eq_expression_desc r1.pexp_desc r2.pexp_desc && (fun _ _ -> true) r1.pexp_loc r2.pexp_loc && eq_attributes r1.pexp_attributes r2.pexp_attributes
and eq_expression_desc c1 c2 =
  match c1, c2 with
  | Pexp_ident(x), Pexp_ident(y) -> (eq_loc eq_longident) x y
  | Pexp_constant(x), Pexp_constant(y) -> eq_constant x y
  | Pexp_let(x1,x2,x3), Pexp_let(y1,y2,y3) -> true && (eq_rec_flag x1 y1) && ((eq_list eq_value_binding) x2 y2) && (eq_expression x3 y3)  | Pexp_function(x), Pexp_function(y) -> (eq_list eq_case) x y
  | Pexp_fun(x1,x2,x3,x4), Pexp_fun(y1,y2,y3,y4) -> true && (eq_label x1 y1) && ((eq_option eq_expression) x2 y2) && (eq_pattern x3 y3) && (eq_expression x4 y4)  | Pexp_apply(x1,x2), Pexp_apply(y1,y2) -> true && (eq_expression x1 y1) && ((eq_list (fun (x1,x2) (y1,y2) -> true && (eq_label x1 y1) && (eq_expression x2 y2))) x2 y2)  | Pexp_match(x1,x2), Pexp_match(y1,y2) -> true && (eq_expression x1 y1) && ((eq_list eq_case) x2 y2)  | Pexp_try(x1,x2), Pexp_try(y1,y2) -> true && (eq_expression x1 y1) && ((eq_list eq_case) x2 y2)  | Pexp_tuple(x), Pexp_tuple(y) -> (eq_list eq_expression) x y
  | Pexp_construct(x1,x2), Pexp_construct(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_option eq_expression) x2 y2)  | Pexp_variant(x1,x2), Pexp_variant(y1,y2) -> true && (eq_label x1 y1) && ((eq_option eq_expression) x2 y2)  | Pexp_record(x1,x2), Pexp_record(y1,y2) -> true && ((eq_list (fun (x1,x2) (y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && (eq_expression x2 y2))) x1 y1) && ((eq_option eq_expression) x2 y2)  | Pexp_field(x1,x2), Pexp_field(y1,y2) -> true && (eq_expression x1 y1) && ((eq_loc eq_longident) x2 y2)  | Pexp_setfield(x1,x2,x3), Pexp_setfield(y1,y2,y3) -> true && (eq_expression x1 y1) && ((eq_loc eq_longident) x2 y2) && (eq_expression x3 y3)  | Pexp_array(x), Pexp_array(y) -> (eq_list eq_expression) x y
  | Pexp_ifthenelse(x1,x2,x3), Pexp_ifthenelse(y1,y2,y3) -> true && (eq_expression x1 y1) && (eq_expression x2 y2) && ((eq_option eq_expression) x3 y3)  | Pexp_sequence(x1,x2), Pexp_sequence(y1,y2) -> true && (eq_expression x1 y1) && (eq_expression x2 y2)  | Pexp_while(x1,x2), Pexp_while(y1,y2) -> true && (eq_expression x1 y1) && (eq_expression x2 y2)  | Pexp_for(x1,x2,x3,x4,x5), Pexp_for(y1,y2,y3,y4,y5) -> true && (eq_pattern x1 y1) && (eq_expression x2 y2) && (eq_expression x3 y3) && (eq_direction_flag x4 y4) && (eq_expression x5 y5)  | Pexp_constraint(x1,x2), Pexp_constraint(y1,y2) -> true && (eq_expression x1 y1) && (eq_core_type x2 y2)  | Pexp_coerce(x1,x2,x3), Pexp_coerce(y1,y2,y3) -> true && (eq_expression x1 y1) && ((eq_option eq_core_type) x2 y2) && (eq_core_type x3 y3)  | Pexp_send(x1,x2), Pexp_send(y1,y2) -> true && (eq_expression x1 y1) && ((=) x2 y2)  | Pexp_new(x), Pexp_new(y) -> (eq_loc eq_longident) x y
  | Pexp_setinstvar(x1,x2), Pexp_setinstvar(y1,y2) -> true && ((eq_loc (=)) x1 y1) && (eq_expression x2 y2)  | Pexp_override(x), Pexp_override(y) -> (eq_list (fun (x1,x2) (y1,y2) -> true && ((eq_loc (=)) x1 y1) && (eq_expression x2 y2))) x y
  | Pexp_letmodule(x1,x2,x3), Pexp_letmodule(y1,y2,y3) -> true && ((eq_loc (=)) x1 y1) && (eq_module_expr x2 y2) && (eq_expression x3 y3)  | Pexp_assert(x), Pexp_assert(y) -> eq_expression x y
  | Pexp_lazy(x), Pexp_lazy(y) -> eq_expression x y
  | Pexp_poly(x1,x2), Pexp_poly(y1,y2) -> true && (eq_expression x1 y1) && ((eq_option eq_core_type) x2 y2)  | Pexp_object(x), Pexp_object(y) -> eq_class_structure x y
  | Pexp_newtype(x1,x2), Pexp_newtype(y1,y2) -> true && ((=) x1 y1) && (eq_expression x2 y2)  | Pexp_pack(x), Pexp_pack(y) -> eq_module_expr x y
  | Pexp_open(x1,x2,x3), Pexp_open(y1,y2,y3) -> true && (eq_override_flag x1 y1) && ((eq_loc eq_longident) x2 y2) && (eq_expression x3 y3)  | Pexp_extension(x), Pexp_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_case = fun r1 r2 -> true && eq_pattern r1.pc_lhs r2.pc_lhs && (eq_option eq_expression) r1.pc_guard r2.pc_guard && eq_expression r1.pc_rhs r2.pc_rhs
and eq_value_description = fun r1 r2 -> true && (eq_loc (=)) r1.pval_name r2.pval_name && eq_core_type r1.pval_type r2.pval_type && (eq_list (=)) r1.pval_prim r2.pval_prim && eq_attributes r1.pval_attributes r2.pval_attributes && (fun _ _ -> true) r1.pval_loc r2.pval_loc
and eq_type_declaration = fun r1 r2 -> true && (eq_loc (=)) r1.ptype_name r2.ptype_name && (eq_list (fun (x1,x2) (y1,y2) -> true && (eq_core_type x1 y1) && (eq_variance x2 y2))) r1.ptype_params r2.ptype_params && (eq_list (fun (x1,x2,x3) (y1,y2,y3) -> true && (eq_core_type x1 y1) && (eq_core_type x2 y2) && ((fun _ _ -> true) x3 y3))) r1.ptype_cstrs r2.ptype_cstrs && eq_type_kind r1.ptype_kind r2.ptype_kind && eq_private_flag r1.ptype_private r2.ptype_private && (eq_option eq_core_type) r1.ptype_manifest r2.ptype_manifest && eq_attributes r1.ptype_attributes r2.ptype_attributes && (fun _ _ -> true) r1.ptype_loc r2.ptype_loc
and eq_type_kind c1 c2 =
  match c1, c2 with
  | Ptype_abstract, Ptype_abstract -> true
  | Ptype_variant(x), Ptype_variant(y) -> (eq_list eq_constructor_declaration) x y
  | Ptype_record(x), Ptype_record(y) -> (eq_list eq_label_declaration) x y
  | Ptype_open, Ptype_open -> true
  | _, _ -> false
and eq_label_declaration = fun r1 r2 -> true && (eq_loc (=)) r1.pld_name r2.pld_name && eq_mutable_flag r1.pld_mutable r2.pld_mutable && eq_core_type r1.pld_type r2.pld_type && (fun _ _ -> true) r1.pld_loc r2.pld_loc && eq_attributes r1.pld_attributes r2.pld_attributes
and eq_constructor_declaration = fun r1 r2 -> true && (eq_loc (=)) r1.pcd_name r2.pcd_name && (eq_list eq_core_type) r1.pcd_args r2.pcd_args && (eq_option eq_core_type) r1.pcd_res r2.pcd_res && (fun _ _ -> true) r1.pcd_loc r2.pcd_loc && eq_attributes r1.pcd_attributes r2.pcd_attributes
and eq_type_extension = fun r1 r2 -> true && (eq_loc eq_longident) r1.ptyext_path r2.ptyext_path && (eq_list (fun (x1,x2) (y1,y2) -> true && (eq_core_type x1 y1) && (eq_variance x2 y2))) r1.ptyext_params r2.ptyext_params && (eq_list eq_extension_constructor) r1.ptyext_constructors r2.ptyext_constructors && eq_private_flag r1.ptyext_private r2.ptyext_private && eq_attributes r1.ptyext_attributes r2.ptyext_attributes
and eq_extension_constructor = fun r1 r2 -> true && (eq_loc (=)) r1.pext_name r2.pext_name && eq_extension_constructor_kind r1.pext_kind r2.pext_kind && (fun _ _ -> true) r1.pext_loc r2.pext_loc && eq_attributes r1.pext_attributes r2.pext_attributes
and eq_extension_constructor_kind c1 c2 =
  match c1, c2 with
  | Pext_decl(x1,x2), Pext_decl(y1,y2) -> true && ((eq_list eq_core_type) x1 y1) && ((eq_option eq_core_type) x2 y2)  | Pext_rebind(x), Pext_rebind(y) -> (eq_loc eq_longident) x y
  | _, _ -> false
and eq_class_type = fun r1 r2 -> true && eq_class_type_desc r1.pcty_desc r2.pcty_desc && (fun _ _ -> true) r1.pcty_loc r2.pcty_loc && eq_attributes r1.pcty_attributes r2.pcty_attributes
and eq_class_type_desc c1 c2 =
  match c1, c2 with
  | Pcty_constr(x1,x2), Pcty_constr(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_list eq_core_type) x2 y2)  | Pcty_signature(x), Pcty_signature(y) -> eq_class_signature x y
  | Pcty_arrow(x1,x2,x3), Pcty_arrow(y1,y2,y3) -> true && (eq_label x1 y1) && (eq_core_type x2 y2) && (eq_class_type x3 y3)  | Pcty_extension(x), Pcty_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_class_signature = fun r1 r2 -> true && eq_core_type r1.pcsig_self r2.pcsig_self && (eq_list eq_class_type_field) r1.pcsig_fields r2.pcsig_fields
and eq_class_type_field = fun r1 r2 -> true && eq_class_type_field_desc r1.pctf_desc r2.pctf_desc && (fun _ _ -> true) r1.pctf_loc r2.pctf_loc && eq_attributes r1.pctf_attributes r2.pctf_attributes
and eq_class_type_field_desc c1 c2 =
  match c1, c2 with
  | Pctf_inherit(x), Pctf_inherit(y) -> eq_class_type x y
  | Pctf_val(x1,x2,x3,x4), Pctf_val(y1,y2,y3,y4) -> true && ((=) x1 y1) && (eq_mutable_flag x2 y2) && (eq_virtual_flag x3 y3) && (eq_core_type x4 y4)  | Pctf_method(x1,x2,x3,x4), Pctf_method(y1,y2,y3,y4) -> true && ((=) x1 y1) && (eq_private_flag x2 y2) && (eq_virtual_flag x3 y3) && (eq_core_type x4 y4)  | Pctf_constraint(x1,x2), Pctf_constraint(y1,y2) -> true && (eq_core_type x1 y1) && (eq_core_type x2 y2)  | Pctf_attribute(x), Pctf_attribute(y) -> eq_attribute x y
  | Pctf_extension(x), Pctf_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_class_infos : 'a. ('a -> 'a -> bool) -> 'a class_infos -> 'a class_infos -> bool = fun eq_a r1 r2 -> true && eq_virtual_flag r1.pci_virt r2.pci_virt && (eq_list (fun (x1,x2) (y1,y2) -> true && (eq_core_type x1 y1) && (eq_variance x2 y2))) r1.pci_params r2.pci_params && (eq_loc (=)) r1.pci_name r2.pci_name && eq_a r1.pci_expr r2.pci_expr && (fun _ _ -> true) r1.pci_loc r2.pci_loc && eq_attributes r1.pci_attributes r2.pci_attributes
and eq_class_description c1 c2 = (eq_class_infos eq_class_type) c1 c2
and eq_class_type_declaration c1 c2 = (eq_class_infos eq_class_type) c1 c2
and eq_class_expr = fun r1 r2 -> true && eq_class_expr_desc r1.pcl_desc r2.pcl_desc && (fun _ _ -> true) r1.pcl_loc r2.pcl_loc && eq_attributes r1.pcl_attributes r2.pcl_attributes
and eq_class_expr_desc c1 c2 =
  match c1, c2 with
  | Pcl_constr(x1,x2), Pcl_constr(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_list eq_core_type) x2 y2)  | Pcl_structure(x), Pcl_structure(y) -> eq_class_structure x y
  | Pcl_fun(x1,x2,x3,x4), Pcl_fun(y1,y2,y3,y4) -> true && (eq_label x1 y1) && ((eq_option eq_expression) x2 y2) && (eq_pattern x3 y3) && (eq_class_expr x4 y4)  | Pcl_apply(x1,x2), Pcl_apply(y1,y2) -> true && (eq_class_expr x1 y1) && ((eq_list (fun (x1,x2) (y1,y2) -> true && (eq_label x1 y1) && (eq_expression x2 y2))) x2 y2)  | Pcl_let(x1,x2,x3), Pcl_let(y1,y2,y3) -> true && (eq_rec_flag x1 y1) && ((eq_list eq_value_binding) x2 y2) && (eq_class_expr x3 y3)  | Pcl_constraint(x1,x2), Pcl_constraint(y1,y2) -> true && (eq_class_expr x1 y1) && (eq_class_type x2 y2)  | Pcl_extension(x), Pcl_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_class_structure = fun r1 r2 -> true && eq_pattern r1.pcstr_self r2.pcstr_self && (eq_list eq_class_field) r1.pcstr_fields r2.pcstr_fields
and eq_class_field = fun r1 r2 -> true && eq_class_field_desc r1.pcf_desc r2.pcf_desc && (fun _ _ -> true) r1.pcf_loc r2.pcf_loc && eq_attributes r1.pcf_attributes r2.pcf_attributes
and eq_class_field_desc c1 c2 =
  match c1, c2 with
  | Pcf_inherit(x1,x2,x3), Pcf_inherit(y1,y2,y3) -> true && (eq_override_flag x1 y1) && (eq_class_expr x2 y2) && ((eq_option (=)) x3 y3)  | Pcf_val(x1,x2,x3), Pcf_val(y1,y2,y3) -> true && ((eq_loc (=)) x1 y1) && (eq_mutable_flag x2 y2) && (eq_class_field_kind x3 y3)  | Pcf_method(x1,x2,x3), Pcf_method(y1,y2,y3) -> true && ((eq_loc (=)) x1 y1) && (eq_private_flag x2 y2) && (eq_class_field_kind x3 y3)  | Pcf_constraint(x1,x2), Pcf_constraint(y1,y2) -> true && (eq_core_type x1 y1) && (eq_core_type x2 y2)  | Pcf_initializer(x), Pcf_initializer(y) -> eq_expression x y
  | Pcf_attribute(x), Pcf_attribute(y) -> eq_attribute x y
  | Pcf_extension(x), Pcf_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_class_field_kind c1 c2 =
  match c1, c2 with
  | Cfk_virtual(x), Cfk_virtual(y) -> eq_core_type x y
  | Cfk_concrete(x1,x2), Cfk_concrete(y1,y2) -> true && (eq_override_flag x1 y1) && (eq_expression x2 y2)  | _, _ -> false
and eq_class_declaration c1 c2 = (eq_class_infos eq_class_expr) c1 c2
and eq_module_type = fun r1 r2 -> true && eq_module_type_desc r1.pmty_desc r2.pmty_desc && (fun _ _ -> true) r1.pmty_loc r2.pmty_loc && eq_attributes r1.pmty_attributes r2.pmty_attributes
and eq_module_type_desc c1 c2 =
  match c1, c2 with
  | Pmty_ident(x), Pmty_ident(y) -> (eq_loc eq_longident) x y
  | Pmty_signature(x), Pmty_signature(y) -> eq_signature x y
  | Pmty_functor(x1,x2,x3), Pmty_functor(y1,y2,y3) -> true && ((eq_loc (=)) x1 y1) && ((eq_option eq_module_type) x2 y2) && (eq_module_type x3 y3)  | Pmty_with(x1,x2), Pmty_with(y1,y2) -> true && (eq_module_type x1 y1) && ((eq_list eq_with_constraint) x2 y2)  | Pmty_typeof(x), Pmty_typeof(y) -> eq_module_expr x y
  | Pmty_extension(x), Pmty_extension(y) -> eq_extension x y
  | Pmty_alias(x), Pmty_alias(y) -> (eq_loc eq_longident) x y
  | _, _ -> false
and eq_signature c1 c2 = (eq_list eq_signature_item) c1 c2
and eq_signature_item = fun r1 r2 -> true && eq_signature_item_desc r1.psig_desc r2.psig_desc && (fun _ _ -> true) r1.psig_loc r2.psig_loc
and eq_signature_item_desc c1 c2 =
  match c1, c2 with
  | Psig_value(x), Psig_value(y) -> eq_value_description x y
  | Psig_type(x), Psig_type(y) -> (eq_list eq_type_declaration) x y
  | Psig_typext(x), Psig_typext(y) -> eq_type_extension x y
  | Psig_exception(x), Psig_exception(y) -> eq_extension_constructor x y
  | Psig_module(x), Psig_module(y) -> eq_module_declaration x y
  | Psig_recmodule(x), Psig_recmodule(y) -> (eq_list eq_module_declaration) x y
  | Psig_modtype(x), Psig_modtype(y) -> eq_module_type_declaration x y
  | Psig_open(x), Psig_open(y) -> eq_open_description x y
  | Psig_include(x), Psig_include(y) -> eq_include_description x y
  | Psig_class(x), Psig_class(y) -> (eq_list eq_class_description) x y
  | Psig_class_type(x), Psig_class_type(y) -> (eq_list eq_class_type_declaration) x y
  | Psig_attribute(x), Psig_attribute(y) -> eq_attribute x y
  | Psig_extension(x1,x2), Psig_extension(y1,y2) -> true && (eq_extension x1 y1) && (eq_attributes x2 y2)  | _, _ -> false
and eq_module_declaration = fun r1 r2 -> true && (eq_loc (=)) r1.pmd_name r2.pmd_name && eq_module_type r1.pmd_type r2.pmd_type && eq_attributes r1.pmd_attributes r2.pmd_attributes && (fun _ _ -> true) r1.pmd_loc r2.pmd_loc
and eq_module_type_declaration = fun r1 r2 -> true && (eq_loc (=)) r1.pmtd_name r2.pmtd_name && (eq_option eq_module_type) r1.pmtd_type r2.pmtd_type && eq_attributes r1.pmtd_attributes r2.pmtd_attributes && (fun _ _ -> true) r1.pmtd_loc r2.pmtd_loc
and eq_open_description = fun r1 r2 -> true && (eq_loc eq_longident) r1.popen_lid r2.popen_lid && eq_override_flag r1.popen_override r2.popen_override && (fun _ _ -> true) r1.popen_loc r2.popen_loc && eq_attributes r1.popen_attributes r2.popen_attributes
and eq_include_infos : 'a. ('a -> 'a -> bool) -> 'a include_infos -> 'a include_infos -> bool = fun eq_a r1 r2 -> true && eq_a r1.pincl_mod r2.pincl_mod && (fun _ _ -> true) r1.pincl_loc r2.pincl_loc && eq_attributes r1.pincl_attributes r2.pincl_attributes
and eq_include_description c1 c2 = (eq_include_infos eq_module_type) c1 c2
and eq_include_declaration c1 c2 = (eq_include_infos eq_module_expr) c1 c2
and eq_with_constraint c1 c2 =
  match c1, c2 with
  | Pwith_type(x1,x2), Pwith_type(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && (eq_type_declaration x2 y2)  | Pwith_module(x1,x2), Pwith_module(y1,y2) -> true && ((eq_loc eq_longident) x1 y1) && ((eq_loc eq_longident) x2 y2)  | Pwith_typesubst(x), Pwith_typesubst(y) -> eq_type_declaration x y
  | Pwith_modsubst(x1,x2), Pwith_modsubst(y1,y2) -> true && ((eq_loc (=)) x1 y1) && ((eq_loc eq_longident) x2 y2)  | _, _ -> false
and eq_module_expr = fun r1 r2 -> true && eq_module_expr_desc r1.pmod_desc r2.pmod_desc && (fun _ _ -> true) r1.pmod_loc r2.pmod_loc && eq_attributes r1.pmod_attributes r2.pmod_attributes
and eq_module_expr_desc c1 c2 =
  match c1, c2 with
  | Pmod_ident(x), Pmod_ident(y) -> (eq_loc eq_longident) x y
  | Pmod_structure(x), Pmod_structure(y) -> eq_structure x y
  | Pmod_functor(x1,x2,x3), Pmod_functor(y1,y2,y3) -> true && ((eq_loc (=)) x1 y1) && ((eq_option eq_module_type) x2 y2) && (eq_module_expr x3 y3)  | Pmod_apply(x1,x2), Pmod_apply(y1,y2) -> true && (eq_module_expr x1 y1) && (eq_module_expr x2 y2)  | Pmod_constraint(x1,x2), Pmod_constraint(y1,y2) -> true && (eq_module_expr x1 y1) && (eq_module_type x2 y2)  | Pmod_unpack(x), Pmod_unpack(y) -> eq_expression x y
  | Pmod_extension(x), Pmod_extension(y) -> eq_extension x y
  | _, _ -> false
and eq_structure c1 c2 = (eq_list eq_structure_item) c1 c2
and eq_structure_item = fun r1 r2 -> true && eq_structure_item_desc r1.pstr_desc r2.pstr_desc && (fun _ _ -> true) r1.pstr_loc r2.pstr_loc
and eq_structure_item_desc c1 c2 =
  match c1, c2 with
  | Pstr_eval(x1,x2), Pstr_eval(y1,y2) -> true && (eq_expression x1 y1) && (eq_attributes x2 y2)  | Pstr_value(x1,x2), Pstr_value(y1,y2) -> true && (eq_rec_flag x1 y1) && ((eq_list eq_value_binding) x2 y2)  | Pstr_primitive(x), Pstr_primitive(y) -> eq_value_description x y
  | Pstr_type(x), Pstr_type(y) -> (eq_list eq_type_declaration) x y
  | Pstr_typext(x), Pstr_typext(y) -> eq_type_extension x y
  | Pstr_exception(x), Pstr_exception(y) -> eq_extension_constructor x y
  | Pstr_module(x), Pstr_module(y) -> eq_module_binding x y
  | Pstr_recmodule(x), Pstr_recmodule(y) -> (eq_list eq_module_binding) x y
  | Pstr_modtype(x), Pstr_modtype(y) -> eq_module_type_declaration x y
  | Pstr_open(x), Pstr_open(y) -> eq_open_description x y
  | Pstr_class(x), Pstr_class(y) -> (eq_list eq_class_declaration) x y
  | Pstr_class_type(x), Pstr_class_type(y) -> (eq_list eq_class_type_declaration) x y
  | Pstr_include(x), Pstr_include(y) -> eq_include_declaration x y
  | Pstr_attribute(x), Pstr_attribute(y) -> eq_attribute x y
  | Pstr_extension(x1,x2), Pstr_extension(y1,y2) -> true && (eq_extension x1 y1) && (eq_attributes x2 y2)  | _, _ -> false
and eq_value_binding = fun r1 r2 -> true && eq_pattern r1.pvb_pat r2.pvb_pat && eq_expression r1.pvb_expr r2.pvb_expr && eq_attributes r1.pvb_attributes r2.pvb_attributes && (fun _ _ -> true) r1.pvb_loc r2.pvb_loc
and eq_module_binding = fun r1 r2 -> true && (eq_loc (=)) r1.pmb_name r2.pmb_name && eq_module_expr r1.pmb_expr r2.pmb_expr && eq_attributes r1.pmb_attributes r2.pmb_attributes && (fun _ _ -> true) r1.pmb_loc r2.pmb_loc
let rec eq_toplevel_phrase c1 c2 =
  match c1, c2 with
  | Ptop_def(x), Ptop_def(y) -> eq_structure x y
  | Ptop_dir(x1,x2), Ptop_dir(y1,y2) -> true && ((=) x1 y1) && (eq_directive_argument x2 y2)  | _, _ -> false
and eq_directive_argument c1 c2 =
  match c1, c2 with
  | Pdir_none, Pdir_none -> true
  | Pdir_string(x), Pdir_string(y) -> (=) x y
  | Pdir_int(x), Pdir_int(y) -> (=) x y
  | Pdir_ident(x), Pdir_ident(y) -> eq_longident x y
  | Pdir_bool(x), Pdir_bool(y) -> (=) x y
  | _, _ -> false
