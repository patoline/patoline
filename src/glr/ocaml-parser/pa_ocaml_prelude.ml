open Input
open Glr
open Charset
open Asttypes
open Parsetree
open Longident

let memoize1 f = 
  let h = Hashtbl.create 1001 in
  (fun x ->
     try Hashtbl.find h x with 
       Not_found ->
       let res = f x in
       Hashtbl.add h x res;
       res)

let memoize2 f = 
  let h = Hashtbl.create 1001 in
  (fun x y ->
     try Hashtbl.find h (x, y) with 
       Not_found ->
       let res = f x y in
       Hashtbl.add h (x, y) res;
       res)


let fast = ref false
let file = ref None
let ascii = ref false
type entry = FromExt | Impl | Intf
let entry = ref FromExt
let modern = ref false
  (* if true, 
     - let priority is inherited ! 3 * let x = 2 in x + 1 is parser as (3 * let x = 2 in x) + 1
       if a then let x = 3 in b ; c as (if a then let x = 3 in b) ; c
     - val is accepted in structure
  *)

let spec = [
  "--ascii", Arg.Set ascii , "output ascii ast instead of serialized ast" ;
  "--impl", Arg.Unit (fun () -> entry := Impl), "treat file as an implementation" ;
  "--intf", Arg.Unit (fun () -> entry := Intf), "treat file as an interface" ;
  "--modern", Arg.Set modern, "enable \"modern\" extensions/restrictions of ocaml's grammar" ;
  "--unsafe", Arg.Set fast, "use unsafe function for arrays" ;
]

let anon_fun s = file := Some s

let _ = Arg.parse spec anon_fun (Printf.sprintf "usage: %s [options] file" Sys.argv.(0)) 

let entry =
  match !entry, !file with
    FromExt, Some s -> if Filename.check_suffix s ".mli" then Intf else Impl
  | FromExt, None -> Intf
  | i, _ -> i

(****************************************************************************
 * Things that have to do with comments and things to be ignored            *
 ****************************************************************************)

exception Unclosed_comment of int * int

(*
 * Characters to be ignored are:
 *   - ' ', '\t', '\r', '\n',
 *   - everything between "(*" and "*)" (ocaml-like comments).
 * Remarks on what is allowed inside an ocaml-like comment:
 *   - nested comments,
 *   - single-line string literals including those containing the substrings
 *     "(*" and or "*)",
 *   - single '"' character.
 *)
let blank str pos =
  let rec fn lvl state prev (str, pos as cur) =
    if is_empty str then (if lvl > 0 then raise (Unclosed_comment (line_num str, pos)) else cur)
    else 
      let c,str',pos' = read str pos in 
      let next =str', pos' in
      match state, c with
      | `Ini , '('                  -> fn lvl `Opn cur next
      | `Opn , '*'                  -> fn (lvl + 1) `Ini cur next
      | `Opn , _   when lvl = 0     -> prev
      | `Opn , _                    -> fn lvl `Ini cur next
      | `Ini , '*' when lvl = 0     -> cur
      | `Ini , '*'                  -> fn lvl `Cls cur next
      | `Cls , '*'                  -> fn lvl `Cls cur next
      | `Cls , ')'                  -> fn (lvl - 1) `Ini cur next
      | `Cls , _                    -> fn lvl `Ini cur next

      | `Str , '"'                  -> fn lvl `Ini cur next
      | _    , '"' when lvl > 0     -> (try fn lvl `Str cur next with
                                         Unclosed_comment _ ->
                                           fn lvl `Ini cur next)
      | `Str , '\\'                 -> fn lvl `Esc cur next
      | `Esc , _                    -> fn lvl `Str cur next
      | `Str , _                    -> fn lvl `Str cur next

      | _    , (' '|'\t'|'\r'|'\n') -> fn lvl `Ini cur next
      | _    , _ when lvl > 0       -> fn lvl `Ini cur next
      | _    , _                    -> cur
  in fn 0 `Ini (str, pos) (str, pos)

let no_blank str pos = str, pos

let ghost loc =
  Location.({loc with loc_ghost = true})

let locate g =
  filter_position g Lexing.(fun fname l pos l' pos' ->
    let s = { pos_fname = fname; pos_lnum = l; pos_cnum = pos; pos_bol = 0 } in
    let e = { pos_fname = fname; pos_lnum = l'; pos_cnum = pos'; pos_bol = 0 } in
    Location.({loc_start = s; loc_end = e; loc_ghost = false}))

let merge l1 l2 =
  Location.(
    {loc_start = l1.loc_start; loc_end = l2.loc_end; loc_ghost = l1.loc_ghost && l2.loc_ghost})

(* declare expression soon for antiquotation *)
module Initial = struct
  type expression_lvl = Top | Let | Seq | Coerce | If | Aff | Tupl | Disj | Conj | Eq | Append | Cons | Sum | Prod | Pow | Opp | App | Dash | Dot | Prefix | Atom
  let (expression_lvl : expression_lvl -> expression grammar), set_expression_lvl = grammar_family ()
  let expr = expression_lvl Top
  let expression= expr
  let module_item : structure_item grammar = declare_grammar ()
  let signature_item : signature_item grammar = declare_grammar ()
  type type_prio = TopType | As | Arr | Prod | DashType | AppType | AtomType
  let (typexpr_lvl : type_prio -> core_type grammar), set_typexpr_lvl = grammar_family ()
  let typexpr = typexpr_lvl TopType
  type pattern_prio = TopPat | AsPat | AltPat | TupPat | ConsPat | CoercePat | ConstrPat
                      | AtomPat
  let (pattern_lvl : pattern_prio -> pattern grammar), set_pattern_lvl = grammar_family ()
  let pattern = pattern_lvl TopPat

  let extra_expressions = ([] : (expression_lvl * expression) grammar list)
  let extra_types = ([] : core_type grammar list)
  let extra_patterns = ([] : (pattern_prio * pattern) grammar list)
  let extra_module_items = ([] : structure_item_desc grammar list)
  let extra_signature_items = ([] : signature_item_desc grammar list)

  let loc_expr _loc e = { pexp_desc = e; pexp_loc = _loc; }
  let loc_pat _loc pat = { ppat_desc = pat; ppat_loc = _loc; }


(****************************************************************************
 * Quotation and anti-quotation code                                        *
 ****************************************************************************)

type quote_env1 = {
  mutable expression_stack : Parsetree.expression list;
  mutable pattern_stack : Parsetree.expression list;
  mutable type_stack : Parsetree.expression list;
  mutable str_item_stack : Parsetree.expression list;
  mutable sig_item_stack : Parsetree.expression list;
  mutable string_stack : Parsetree.expression list;
  mutable int_stack : Parsetree.expression list;
  mutable int32_stack : Parsetree.expression list;
  mutable int64_stack : Parsetree.expression list;
  mutable natint_stack : Parsetree.expression list;
  mutable float_stack : Parsetree.expression list;
  mutable char_stack : Parsetree.expression list;
  mutable bool_stack : Parsetree.expression list;
}
type quote_env2 = {
  mutable expression_stack : Parsetree.expression list;
  mutable pattern_stack : Parsetree.pattern list;
  mutable type_stack : Parsetree.core_type list;
  mutable str_item_stack : Parsetree.structure_item_desc list;
  mutable sig_item_stack : Parsetree.signature_item_desc list;
  mutable string_stack : string list;
  mutable int_stack : int list;
  mutable int32_stack : int32 list;
  mutable int64_stack : int64 list;
  mutable natint_stack : nativeint list;
  mutable float_stack : float list;
  mutable char_stack : char list;
  mutable bool_stack : bool list;
}
type quote_env =
    First of quote_env1 | Second of quote_env2

let quote_stack : quote_env Stack.t =
  Stack.create ()


let empty_quote_env1 () = First {
  expression_stack = [];
  pattern_stack  = [];
  type_stack =  [];
  str_item_stack =  [];
  sig_item_stack =  [];
  string_stack = [];
  int_stack = [];
  int32_stack = [];
  int64_stack = [];
  natint_stack = [];
  float_stack = [];
  char_stack = [];
  bool_stack = [];
}

let empty_quote_env2 () = Second {
  expression_stack = [];
  pattern_stack  = [];
  type_stack =  [];
  str_item_stack =  [];
  sig_item_stack =  [];
  string_stack = [];
  int_stack = [];
  int32_stack = [];
  int64_stack = [];
  natint_stack = [];
  float_stack = [];
  char_stack = [];
  bool_stack = [];
}

let push_pop_expression e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.expression_stack <- e::env.expression_stack; e
    | Second env ->
       match env.expression_stack with
	 e::l -> env.expression_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_expression e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.expression_stack <- e::env.expression_stack

let push_pop_type e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.type_stack <- e::env.type_stack; { ptyp_desc = Ptyp_any; ptyp_loc = e.pexp_loc; }
    | Second env ->
       match env.type_stack with
	 e::l -> env.type_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_type e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.type_stack <- e::env.type_stack

let push_pop_pattern e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.pattern_stack <- e::env.pattern_stack; { ppat_desc = Ppat_any; ppat_loc = e.pexp_loc; }
    | Second env ->
       match env.pattern_stack with
	 e::l -> env.pattern_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_pattern e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.pattern_stack <- e::env.pattern_stack

let push_pop_str_item e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.str_item_stack <- e::env.str_item_stack; Pstr_eval e; 
    | Second env ->
       match env.str_item_stack with
	 e::l -> env.str_item_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_str_item e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.str_item_stack <- e::env.str_item_stack

let push_pop_sig_item e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.sig_item_stack <- e::env.sig_item_stack;
	let _loc = e.pexp_loc in 
	Psig_value({ txt = ""; loc = _loc }, { pval_type = { ptyp_desc = Ptyp_any; ptyp_loc = e.pexp_loc; };
							     pval_prim = []; pval_loc = _loc})
    | Second env ->
       match env.sig_item_stack with
	 e::l -> env.sig_item_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_sig_item e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.sig_item_stack <- e::env.sig_item_stack

let push_pop_string e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.string_stack <- e::env.string_stack; ""
    | Second env ->
       match env.string_stack with
	 e::l -> env.string_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_string e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.string_stack <- e::env.string_stack

let push_pop_int e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.int_stack <- e::env.int_stack; 0
    | Second env ->
       match env.int_stack with
	 e::l -> env.int_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_int e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.int_stack <- e::env.int_stack

let push_pop_int32 e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.int32_stack <- e::env.int32_stack; 0l
    | Second env ->
       match env.int32_stack with
	 e::l -> env.int32_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_int32 e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.int32_stack <- e::env.int32_stack

let push_pop_int64 e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.int64_stack <- e::env.int64_stack; 0L
    | Second env ->
       match env.int64_stack with
	 e::l -> env.int64_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_int64 e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.int64_stack <- e::env.int64_stack

let push_pop_natint e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.natint_stack <- e::env.natint_stack; 0n
    | Second env ->
       match env.natint_stack with
	 e::l -> env.natint_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_natint e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.natint_stack <- e::env.natint_stack

let push_pop_float e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.float_stack <- e::env.float_stack; 0.0
    | Second env ->
       match env.float_stack with
	 e::l -> env.float_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_float e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.float_stack <- e::env.float_stack

let push_pop_char e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.char_stack <- e::env.char_stack; ' '
    | Second env ->
       match env.char_stack with
	 e::l -> env.char_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_char e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.char_stack <- e::env.char_stack

let push_pop_bool e =
  try
    match Stack.top quote_stack with
    | First env ->
	env.bool_stack <- e::env.bool_stack; false
    | Second env ->
       match env.bool_stack with
	 e::l -> env.bool_stack <- l; e
       | _ -> assert false
  with
    Stack.Empty -> raise Give_up

let push_bool e =
    match Stack.top quote_stack with
    | First env -> assert false
    | Second env ->
	env.bool_stack <- e::env.bool_stack

let quote_expression _loc e name =
  let cols =
    let n = Location.(Lexing.(_loc.loc_start.pos_cnum)) in
    String.make (n+1) ' '
  in
  let e = Location.(Lexing.(Printf.sprintf "#%d %S\n%s" (_loc.loc_start.pos_lnum - 1) _loc.loc_start.pos_fname)) cols ^ e in  
  Stack.push (empty_quote_env1 ()) quote_stack ;
  let _ = match name with
    | "expression" -> ignore (parse_string (glr e:expression EOF end) blank "quote..." e)
    | "type"  -> ignore (parse_string typexpr blank "quote..." e)
    | "pattern"  -> ignore (parse_string pattern blank "quote..." e)
    | "str_item"  -> ignore (parse_string module_item blank "quote..." e)
    | "sig_item"  -> ignore (parse_string signature_item blank "quote..." e)
    | _ -> assert false
    in
  let env = match Stack.pop quote_stack with
      First e -> e | Second _ -> assert false
  in
  let push_expr =
    loc_expr _loc (Pexp_apply(
		       loc_expr _loc (Pexp_ident{ txt = Ldot(Lident "Stack","push"); loc = _loc }),
		   ["", loc_expr _loc (Pexp_apply(
							      (loc_expr _loc (Pexp_ident{ txt = Ldot(Lident "Pa_ocaml_prelude","empty_quote_env2"); loc = _loc })), 
							      ["", loc_expr _loc (Pexp_construct({ txt = Lident "()"; loc = _loc }, None, false))]));
		   
		    "", loc_expr _loc (Pexp_ident{ txt = Ldot(Lident "Pa_ocaml_prelude","quote_stack"); loc = _loc })]))
  in
  let fill push_expr name l = 
    let p = 
      List.fold_left
	(fun acc e ->
	 let push_e =
	   loc_expr _loc (Pexp_apply(
			      loc_expr _loc (Pexp_ident{ txt = Ldot(Lident "Pa_ocaml_prelude",name); loc = _loc }),
			      ["", e]))
	 in
	 match acc with
	   None -> Some push_e
	 | Some acc -> 
	    Some (loc_expr _loc (Pexp_sequence(acc, push_e))))
	None l
    in
    match p with
      None -> push_expr
    | Some e -> loc_expr _loc (Pexp_sequence(push_expr, e))
  in
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
  let pop_expr =    loc_expr _loc (Pexp_apply(loc_expr _loc (Pexp_ident{ txt = Lident "ignore"; loc = _loc }),
				       ["",loc_expr _loc (Pexp_apply(
		       loc_expr _loc (Pexp_ident{ txt = Ldot(Lident "Stack","pop"); loc = _loc }),
		   ["", loc_expr _loc (Pexp_ident{ txt = Ldot(Lident "Pa_ocaml_prelude","quote_stack"); loc = _loc })]))]))
  in
  let parse_expr = 
    loc_expr _loc (Pexp_apply(
		       loc_expr _loc (Pexp_ident{ txt = Ldot(Lident "Pa_ocaml_prelude","quote_"^name^"_2"); loc = _loc }),
		       ["", loc_expr _loc (Pexp_constant( Const_string e ))]))
  in
  loc_expr _loc (Pexp_sequence(push_expr,loc_expr _loc (Pexp_let(Nonrecursive, [loc_pat _loc (Ppat_var { txt = "quote_res"; loc = _loc }),  parse_expr],loc_expr _loc (Pexp_sequence(pop_expr,loc_expr _loc (Pexp_ident{ txt = Lident "quote_res"; loc = _loc })))))))

let quote_expression_2 e =
  parse_string expression blank "quote..." e

let quote_type_2 e =
  parse_string typexpr blank "quote..." e

let quote_pattern_2 e =
  parse_string pattern blank "quote..." e

let quote_str_item_2 e =
  parse_string module_item blank "quote..." e

let quote_sig_item_2 e =
  parse_string signature_item blank "quote..." e


end

module type Extension = module type of Initial

module type FExt = functor (E:Extension) -> Extension

let extensions_mod  = ref ([] : (module FExt) list)

let register_extension e = extensions_mod := e::!extensions_mod

include Initial
