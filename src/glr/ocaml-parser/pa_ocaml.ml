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
let extension = ref false
  (* if true, 
     if a then let ... or [ fun ... ] are rejected because [ let x = 3 in x ; x] is nasty *)
  (* val is accepted in structure *)

let spec = [
  "--ascii", Arg.Set ascii , "output ascii ast instead of serialized ast" ;
  "--impl", Arg.Unit (fun () -> entry := Impl), "treat file as an implementation" ;
  "--intf", Arg.Unit (fun () -> entry := Intf), "treat file as an interface" ;
  "--ext", Arg.Set extension, "enable glr extensions/restrictions of ocaml's grammar" ;
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

exception Unclosed_comment of int

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
  let len = String.length str in
  let rec fn lvl state pos =
    if pos >= len then (if lvl > 0 then raise (Unclosed_comment len) else len)
    else match state, str.[pos] with
      | `Ini , '('                  -> fn lvl `Opn (pos + 1)
      | `Opn , '*'                  -> fn (lvl + 1) `Ini (pos + 1)
      | `Opn , _   when lvl = 0     -> pos - 1
      | `Opn , _                    -> fn lvl `Ini (pos + 1)
      | `Ini , '*' when lvl = 0     -> pos
      | `Ini , '*'                  -> fn lvl `Cls (pos + 1)
      | `Cls , '*'                  -> fn lvl `Cls (pos + 1)
      | `Cls , ')'                  -> fn (lvl - 1) `Ini (pos + 1)
      | `Cls , _                    -> fn lvl `Ini (pos + 1)

      | `Str , '"'                  -> fn lvl `Ini (pos + 1)
      | _    , '"' when lvl > 0     -> (try fn lvl `Str (pos + 1) with
                                         Unclosed_comment _ ->
                                           fn lvl `Ini (pos + 1))
      | `Str , '\\'                 -> fn lvl `Esc (pos + 1)
      | `Esc , _                    -> fn lvl `Str (pos + 1)
      | `Str , _                    -> fn lvl `Str (pos + 1)

      | _    , (' '|'\t'|'\r'|'\n') -> fn lvl `Ini (pos + 1)
      | _    , _ when lvl > 0       -> fn lvl `Ini (pos + 1)
      | _    , _                    -> pos
  in fn 0 `Ini pos

let no_blank _ pos = pos

(****************************************************************************
 * Functions for computing line numbers and positions.                      *
 ****************************************************************************)

(* computes the line number (with a cache) for a given position in a string *)
let bol = Hashtbl.create 1001
let find_pos str n =
  let rec fn i =
    (*Printf.fprintf stderr "str: %s, i: %d\n%!" str i;*)
    if i < String.length str && str.[i] = '\n' then
      try Hashtbl.find bol i, i
      with Not_found ->
        if i = 0 then (2,i) else
          let lnum, _ = fn (i-1) in
          let lnum = lnum + 1 in
          let i = if i + 1 < String.length str && str.[i+1] = '\r' then i + 1 else i in
          Hashtbl.add bol i lnum;
          lnum, i
    else if i <= 0 then (1, i)
    else fn (i-1)
  in
  let (lnum, bol) = fn n in
  Lexing.({ pos_fname = (match !file with None -> "stdin" | Some s -> s);
            pos_lnum  = lnum;
            pos_bol   = bol;
            pos_cnum  = n })

let locate g =
  filter_position g (fun str pos pos' ->
    let s = find_pos str pos in
    let e = find_pos str pos' in
    Location.({loc_start = s; loc_end = e; loc_ghost = false}))

let merge l1 l2 =
  Location.({loc_start = l1.loc_start; loc_end = l2.loc_end; loc_ghost = l1.loc_ghost && l2.loc_ghost})

let _ = glr_locate locate merge

(****************************************************************************
 * Basic syntactic elements (identifiers and literals)                      *
 ****************************************************************************)
let par_re s = "\\(" ^ s ^ "\\)"
let union_re l = 
  let l = List.map (fun s -> par_re s ) l in
  String.concat "\\|" l


(* Identifiers *)
(* NOTE "_" is not a valid identifier, we handle it separately *)
let lident_re = "\\([a-z][a-zA-Z0-9_']*\\)\\|\\([_][a-zA-Z0-9_']+\\)\\b"
let cident_re = "[A-Z][a-zA-Z0-9_']*\\b"
let ident_re = "[A-Za-z_][a-zA-Z0-9_']*\\b"

let reserved_ident =
  [ "and" ; "as" ; "assert" ; "asr" ; "begin" ; "class" ; "constraint" ; "do"
  ; "done" ; "downto" ; "else" ; "end" ; "exception" ; "external" ; "false"
  ; "for" ; "fun" ; "function" ; "functor" ; "if" ; "in" ; "include"
  ; "inherit" ; "initializer" ; "land" ; "lazy" ; "let" ; "lor" ; "lsl"
  ; "lsr" ; "lxor" ; "match" ; "method" ; "mod" ; "module" ; "mutable" ; "new"
  ; "object" ; "of" ; "open" ; "or" ; "private" ; "rec" ; "sig" ; "struct"
  ; "then" ; "to" ; "true" ; "try" ; "type" ; "val" ; "virtual" ; "when"
  ; "while" ; "with" ]

let is_reserved_id w =
  List.mem w reserved_ident

let ident =
  glr
    id:RE(ident_re) -> (if is_reserved_id id then raise Give_up; id)
  end

let capitalized_ident =
  glr
    id:RE(cident_re) -> id
  end

let lowercase_ident =
  glr
    id:RE(lident_re) -> if is_reserved_id id then raise Give_up; id
  end

(* Integer literals *)
let int_dec_re = "[-]?[0-9][0-9_]*"
let int_hex_re = "[-]?[0][xX][0-9a-fA-F][0-9a-fA-F_]*"
let int_oct_re = "[-]?[0][oO][0-7][0-7_]*"
let int_bin_re = "[-]?[0][bB][01][01_]*"
let int_gen_re = (union_re [int_hex_re; int_oct_re;int_bin_re;int_dec_re]) (* decimal à la fin sinon ça ne marche pas !!! *)
let int_re = int_gen_re
let int32_re = par_re int_re ^ "l"
let int64_re = par_re int_re ^ "L"
let natint_re = par_re int_re ^ "n"
let integer_literal =
  glr
    i:RE(int_re) -> int_of_string i
  end

let int32_lit =
  glr
    i:RE(int32_re)[groupe 1] -> Int32.of_string i
  end

let int64_lit =
  glr
    i:RE(int64_re)[groupe 1] -> Int64.of_string i
  end

let nat_int_lit =
  glr
    i:RE(natint_re)[groupe 1] -> Nativeint.of_string i
  end

(* Floating-point literals *)
let float_lit_dec    = "[-]?[0-9][0-9_]*[.][0-9_]*\\([eE][+-][0-9][0-9_]*\\)?"
let float_lit_no_dec = "[-]?[0-9][0-9_]*[eE][+-][0-9][0-9_]*"
let float_re = union_re [float_lit_dec; float_lit_no_dec]

let float_literal =
  glr
    f:RE(float_re)   -> f
  end

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
  change_layout (
    glr STR("\'") c:(one_char true) STR("\'") -> c end
  ) no_blank

(* String literals *)
let interspace = "[\\][\n][ \t]*"

let string_literal =
  let char_list_to_string lc =
    let len = List.length lc in
    let str = String.create len in
    for i = 0 to len - 1 do
      str.[i] <- List.nth lc i
    done;
    str
  in
  change_layout (
    glr
      STR("\"") lc:(one_char false)*
        lcs:(glr RE(interspace) lc:(one_char false)* -> lc end)*
        STR("\"") -> char_list_to_string (List.flatten (lc::lcs))
    end
  ) no_blank

(* Naming labels *)
let label_name = lowercase_ident

let label =
  glr
  | STR("~") ln:label_name -> ln
  end

let opt_label =
  glr
  | STR("?") ln:label_name -> ("?" ^ ln)
  end

let maybe_opt_label =
  glr
  | o:STR("?")? ln:label_name ->
      if o = None then ln else ("?" ^ ln)
  end

(* Prefix and infix symbols *)
let reserved_symbols =
  [ "#" ; "'" ; "(" ; ")" ; "," ; "->" ; "." ; ".." ; ":" ; ":>" ; ";" ; ";;" ; "<-"
  ; ">]" ; ">}" ; "?" ; "[" ; "[<" ; "[>" ; "[|" ; "]" ; "_" ; "`" ; "{" ; "{<" ; "|" ; "|]" ; "}" ; "~" ]

let is_reserved_symb s =
  List.mem s reserved_symbols

let infix_symb_re  = union_re [
 "[=<>@^|&+*/$%:-][!$%&*+./:<=>?@^|~-]*";
 "mod" ^ "\\b";
 "land" ^ "\\b";
 "lor" ^ "\\b";
 "or" ^ "\\b";
 "lxor" ^ "\\b";
 "lsl" ^ "\\b";
 "lsr" ^ "\\b";
 "asr" ^ "\\b"]
let prefix_symb_re = "\\([!-][!$%&*+./:<=>?@^|~-]*\\)\\|\\([~?][!$%&*+./:<=>?@^|~-]+\\)"

let infix_symbol =
  glr
    sym:RE(infix_symb_re) -> if is_reserved_symb sym then raise Give_up; sym
  end

let prefix_symbol =
  glr
    sym:RE(prefix_symb_re) -> if is_reserved_symb sym then raise Give_up; sym
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
let tag_name        = capitalized_ident  
let typeconstr_name = lowercase_ident  
let field_name      = lowercase_ident  
let module_name     = capitalized_ident  
let modtype_name    = ident  
let class_name      = lowercase_ident  
let inst_var_name   = lowercase_ident  
let method_name     = lowercase_ident

let module_path_gen, set_module_path_gen  = grammar_family ()
let module_path_suit, set_module_path_suit  = grammar_family ()

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

(****************************************************************************
 * Several shortcuts for flags and keywords                                 *
 ****************************************************************************)
let key_word s = 
   let len_s = String.length s in
   assert(len_s > 0);
   black_box 
     (fun str pos ->
      let len = String.length str in
      if len < pos + len_s then raise Give_up;
      for i = 0 to len_s - 1 do
	if str.[pos + i] <> s.[i] then raise Give_up
      done;
      let pos = pos + len_s in
      if pos < len then
	match str.[pos] with
	  'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '\'' -> raise Give_up
	  | _ -> (), pos
      else (), pos)
     (Charset.singleton s.[0]) false s

let mutable_kw = key_word "mutable"
let mutable_flag =
  glr
  | mutable_kw -> Mutable
  | EMPTY      -> Immutable
  end

let private_kw = key_word "private"
let private_flag =
  glr
  | private_kw -> Private
  | EMPTY      -> Public
  end

let virtual_kw = key_word "virtual"
let virtual_flag =
  glr
  | virtual_kw -> Virtual
  | EMPTY      -> Concrete
  end

let rec_kw = key_word "rec"
let rec_flag =
  glr
  | rec_kw -> Recursive
  | EMPTY  -> Nonrecursive
  end

let to_kw = key_word "to"
let downto_kw = key_word "downto"
let downto_flag =
  glr
  | to_kw     -> Upto
  | downto_kw -> Downto
  end

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

let opt_variance =
  glr
  | v:RE("[+-]")? ->
      (match v with
       | None     -> (false, false)
       | Some "+" -> (true , false)
       | Some "-" -> false, true
       | _        -> assert false)
  end

(****************************************************************************
 * Type expressions                                                         *
 * FIXME we never use the constructor Ptyp_package, what is it used for?    *
 ****************************************************************************)

type type_prio = TopType | As | Arr | Prod | Dash | AppType | AtomType

let type_prios = [TopType; As; Arr; Prod; Dash; AppType; AtomType]

let next_type_prio = function
  | TopType -> As
  | As -> Arr
  | Arr -> Prod
  | Prod -> Dash
  | Dash -> AppType
  | AppType -> AtomType
  | AtomType -> AtomType

let typexpr_lvl, set_typexpr_lvl = grammar_family ()
let typexpr = typexpr_lvl TopType
let loc_typ _loc typ = { ptyp_desc = typ; ptyp_loc = _loc; }

let poly_typexpr =
  glr
  | te:typexpr -> te
  | ids:{STR("'") id:ident}+ STR(".") te:typexpr ->
      loc_typ _loc (Ptyp_poly (ids, te))
  end
   
let pfield_loc _loc d = { pfield_desc = d; pfield_loc = _loc }
let method_type =
  glr
  | mn:method_name STR(":") pte:poly_typexpr ->
      pfield_loc _loc (Pfield (mn, pte))
  end

let tag_spec =
  glr
  | STR("`") tn:tag_name te:{of_kw te:typexpr}? ->
      let t = match te with
              | None   -> []
              | Some l -> [l]
      in
      Rtag (tn, false, t)
  | te:typexpr ->
      Rinherit te
  end

let tag_spec_first =
  glr
  | tn:tag_name te:{of_kw te:typexpr}? ->
      let t = match te with
              | None   -> []
              | Some l -> [l]
      in
      [Rtag (tn, false, t)]
  | te:typexpr? STR("|") ts:tag_spec ->
      match te with
      | None    -> [ts]
      | Some te -> [Rinherit te; ts]
  end

let tag_spec_full =
  glr
  | STR("`") tn:tag_name tes:{of_kw STR("&")? te:typexpr
    tes:{STR("&") te:typexpr}* -> (te::tes)}? ->
      let tes = match tes with
                | None   -> []
                | Some l -> l
      in
      Rtag (tn, false, tes)
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
    tns:{STR(">") tns:{STR("`") tns:tag_name}+}? STR("]") ->
      loc_typ _loc (Ptyp_variant (tfs :: tfss, true, tns))
  end

let typexpr_base : core_type grammar =
  glr
  | STR("'") id:ident ->
      loc_typ _loc (Ptyp_var id)
  | STR("_") ->
      loc_typ _loc Ptyp_any
  | STR("(") te:typexpr STR(")") ->
      loc_typ _loc te.ptyp_desc
  | ln:opt_label STR(":") te:(typexpr_lvl (next_type_prio Arr)) STR("->") te':typexpr ->
      loc_typ _loc (Ptyp_arrow (ln, te, te'))
  | ln:label_name STR(":") te:(typexpr_lvl (next_type_prio Arr)) STR("->") te':typexpr ->
      loc_typ _loc (Ptyp_arrow (ln, te, te'))
  | tc:typeconstr ->
      loc_typ _loc (Ptyp_constr ({ txt = tc; loc = _loc_tc }, []))
  | STR("(") te:typexpr tes:{STR(",") te:typexpr}* STR(")") tc:typeconstr ->
      let constr = { txt = tc ; loc = _loc_tc } in
      loc_typ _loc (Ptyp_constr (constr, te::tes))
  | pvt:polymorphic_variant_type -> pvt
  | STR("<") rv:STR("..")? STR(">") ->
      let ml = if rv = None then [] else [pfield_loc _loc_rv Pfield_var] in
      loc_typ _loc (Ptyp_object ml)
  | STR("<") mt:method_type mts:{STR(";") mt:method_type}*
    rv:{STR(";") rv:STR("..")?}? STR(">") ->
      let ml = if rv = None then [] else [pfield_loc _loc_rv Pfield_var] in
      loc_typ _loc (Ptyp_object (mt :: mts @ ml))
  | STR("#") cp:class_path ->
      let cp = { txt = cp; loc = _loc_cp } in
      loc_typ _loc (Ptyp_class (cp, [], []))
  | STR("(") te:typexpr tes:{STR(",") te:typexpr}* STR(")")
    STR("#") cp:class_path ->
      let cp = { txt = cp; loc = _loc_cp } in
      loc_typ _loc (Ptyp_class (cp, te::tes, []))
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
  | STR("#") cp:class_path when lvl' >= Dash && lvl <= Dash ->
      let cp = { txt = cp; loc = _loc_cp } in
      let tex = fun te ->
        ln te _loc (Ptyp_class (cp, [te], [])) (* FIXME what is the last list for?! *)
      in (Dash, tex)
  end)

let typexpr_suit =
  let f type_suit =
    memoize2
      (fun lvl' lvl ->
         glr
         | (p1,f1):(typexpr_suit_aux lvl' lvl) ->> (p2,f2):(type_suit p1 lvl) -> p2, fun f -> f2 (f1 f)
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
  | var:opt_variance STR("'") id:ident ->
      (Some { txt = id; loc = _loc }, var)
  | var:opt_variance STR("_") ->
      (Some { txt = "_"; loc = _loc }, var)
  end

let type_params =
  glr
  | tp:type_param -> [tp]
  | STR("(") tp:type_param tps:{STR(",") tp:type_param -> tp}* STR(")") ->
      tp::tps
  end

let type_equation =
  glr
  | STR("=") te:typexpr -> te
  end

let type_constraint =
  glr
  | constraint_kw STR("'") id:ident STR("=") te:typexpr ->
      loc_typ _loc_id (Ptyp_var id), te, _loc
  end

let constr_decl =
  let constr_name =
    glr
    | cn:constr_name    -> cn
    | STR("(") STR(")") -> "()"
    end
  in
  glr
    | cn:constr_name tes:{of_kw te:typexpr
      tes:{STR("*") te:typexpr -> te}* -> te::tes}?[[]] ->
        let c = { txt = cn; loc = _loc_cn } in
        (c, tes, None, _loc_cn) (* TODO GADT Stuff *)
  end

let field_decl =
  glr
  | m:mutable_flag fn:field_name STR(":") pte:poly_typexpr ->
      { txt = fn; loc = _loc_fn }, m, pte, _loc_m
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
  | te:type_equation? tr:{STR("=") tr:type_representation -> tr}?
    cstrs:type_constraint* ->
      let tkind =
        match tr with
        | None    -> Ptype_abstract
        | Some tr -> tr
      in
      (te, tkind, cstrs)
  end

let typedef : (string loc * type_declaration) Glr.grammar =
  glr
  | tps:type_params?[[]] tcn:typeconstr_name ti:type_information ->
      let (te, tkind, cstrs) = ti in
      let tdec =
        { ptype_params   = List.map fst tps
        ; ptype_cstrs    = cstrs
        ; ptype_kind     = tkind
        ; ptype_private  = Public (* FIXME ?? *)
        ; ptype_manifest = te
        ; ptype_variance = List.map snd tps
        ; ptype_loc      = _loc_tps
        }
      in ({ txt = tcn; loc = _loc_tcn }, tdec)
  end

let typedef_in_constraint : (Longident.t loc * type_declaration) Glr.grammar =
  glr
  | tps:type_params?[[]] tcn:typeconstr ti:type_information ->
      let (te, tkind, cstrs) = ti in
      let tdec =
        { ptype_params   = List.map fst tps
        ; ptype_cstrs    = cstrs
        ; ptype_kind     = tkind
        ; ptype_private  = Public (* FIXME ?? *)
        ; ptype_manifest = te
        ; ptype_variance = List.map snd tps
        ; ptype_loc      = _loc_tps
        }
      in ({ txt = tcn; loc = _loc_tcn }, tdec)
  end

let type_definition =
  glr
  | type_kw td:typedef tds:{and_kw td:typedef -> td}* -> (td::tds)
  end

let exception_declaration =
  glr
  | exception_kw cn:constr_name typ:{of_kw te:typexpr
    tes:{STR("*") te:typexpr -> te}* -> (te::tes) }?[[]] ->
      ({ txt = cn; loc = _loc_cn }, typ)
  end

(* Exception definition *)
let exception_definition =
  glr
  | exception_kw cn:constr_name STR("=") c:constr ->
      let name = { txt = cn; loc = _loc_cn } in
      let ex = { txt = c; loc = _loc_c } in
      Pstr_exn_rebind (name, ex)
  | (name,ed):exception_declaration ->
      Pstr_exception (name, ed)
  end

(****************************************************************************
 * Classes                                                                  *
 ****************************************************************************)
(* Class types *)
let class_field_spec = declare_grammar ()
let class_body_type = declare_grammar ()

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
      if v = Concrete then
        pctf_loc _loc (Pctf_meth (mn, pri, te))
      else
        pctf_loc _loc (Pctf_virt (mn, pri, te))
  | constraint_kw te:typexpr STR("=") te':typexpr ->
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
        | Some l -> pcty_loc _loc (Pcty_fun (l, te, acc))
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
    STR("=") cbt:class_body_type ->
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

(* Patterns *)
type pattern_prio = TopPat | AsPat | AltPat | TupPat | ConsPat | CoercePat | ConstrPat
                  | AtomPat

let pattern_prios = [ TopPat ; AsPat ; AltPat ; TupPat ; ConsPat ; CoercePat ; ConstrPat
                    ; AtomPat ]

let next_pat_prio = function
    TopPat -> AsPat
  | AsPat -> AltPat
  | AltPat -> TupPat
  | TupPat -> ConsPat
  | ConsPat -> CoercePat
  | CoercePat -> ConstrPat
  | ConstrPat -> AtomPat
  | AtomPat -> AtomPat

let pattern_lvl, set_pattern_lvl = grammar_family ()
let pattern = pattern_lvl TopPat

let loc_pat _loc pat = { ppat_desc = pat; ppat_loc = _loc; }

let pattern_base = memoize1 (fun lvl ->
  glr
  | vn:value_name ->
      (AtomPat, loc_pat _loc_vn (Ppat_var { txt = vn; loc = _loc_vn }))
  | STR("_") ->
      (AtomPat, loc_pat _loc Ppat_any)
  | c1:char_literal STR("..") c2:char_literal ->
      let ic1, ic2 = Char.code c1, Char.code c2 in
      if ic1 > ic2 then assert false; (* FIXME error message invalid range *)
      let const i = Ppat_constant (Const_char (Char.chr i)) in
      let rec range a b =
        if a > b then assert false
        else if a = b then [a]
        else a :: range (a+1) b
      in
      let opts = List.map (fun i -> loc_pat _loc (const i)) (range ic1 ic2) in
      (AtomPat, List.fold_left (fun acc o -> loc_pat _loc (Ppat_or(acc, o))) (List.hd opts) (List.tl opts))
  | c:constant ->
      (AtomPat, loc_pat _loc_c (Ppat_constant c))
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
  | false_kw ->
      let fls = { txt = Lident "false"; loc = _loc } in
      (AtomPat, loc_pat _loc (Ppat_construct (fls, None, false)))
  | true_kw  -> 
      let tru = { txt = Lident "true"; loc = _loc } in
      (AtomPat, loc_pat _loc (Ppat_construct (tru, None, false)))
  | s:STR("`") c:tag_name p:(pattern_lvl ConstrPat) when lvl <= ConstrPat ->
      (ConstrPat, loc_pat _loc_s (Ppat_variant (c, Some p)))
  | s:STR("`") c:tag_name ->
      (AtomPat, loc_pat _loc_s (Ppat_variant (c, None)))
  | s:STR("#") t:typeconstr ->
      (AtomPat, loc_pat _loc_s (Ppat_type { txt = t; loc = _loc_t }))
  | s:STR("{") f:field STR("=") p:pattern
    fps:{STR(";") f:field STR("=") p:pattern -> ({ txt = f; loc = _loc_f }, p)}*
    clsd:{STR(";") STR("_") -> ()}? STR(";")? STR("}") ->
      let all = ({ txt = f; loc = _loc_f },p)::fps in
      let cl = match clsd with
               | None   -> Closed
               | Some _ -> Open
      in
      (AtomPat, loc_pat _loc_s (Ppat_record (all, cl)))
  | STR("[") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("]") ->
      let nil = { txt = Lident "[]"; loc = _loc } in
      let cons x xs =
        let c = { txt = Lident "::"; loc = _loc } in
        let cons = Ppat_construct (c, Some (loc_pat _loc (Ppat_tuple [x;xs])), false) in
        loc_pat _loc cons
      in
      (AtomPat, List.fold_right cons (p::ps) (loc_pat _loc_p (Ppat_construct (nil, None, false))))
  | s:STR("[") STR("]") ->
      let nil = { txt = Lident "[]"; loc = _loc_s } in
      (AtomPat, loc_pat _loc_s (Ppat_construct (nil, None, false)))
  | s:STR("[|") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("|]") ->
      (AtomPat, loc_pat _loc_s (Ppat_array (p::ps)))
  | s:STR("[|") STR("|]") ->
      (AtomPat, loc_pat _loc_s (Ppat_array []))
  | s:STR("(") STR(")") ->
      let unt = { txt = Lident "()"; loc = _loc_s } in
      (AtomPat, loc_pat _loc_s (Ppat_construct (unt, None, false)))
  | begin_kw end_kw ->
      let unt = { txt = Lident "()"; loc = _loc } in
      (AtomPat, loc_pat _loc (Ppat_construct (unt, None, false)))
  end)

let pattern_suit_aux : pattern_prio -> pattern_prio -> (pattern_prio * (pattern -> pattern)) grammar = memoize1 (fun lvl' lvl ->
  let ln f _loc e = loc_pat (merge f.ppat_loc _loc) e in
  glr
  | as_kw vn:value_name when lvl' > AsPat && lvl <= AsPat ->
      (AsPat, fun p ->
        ln p _loc (Ppat_alias(p, { txt = vn; loc= _loc_vn })))
  | STR("|") p':pattern when lvl' > AltPat && lvl <= AltPat ->
      (AltPat, fun p ->
        ln p _loc (Ppat_or(p, p')))
  | ps:{STR(",") p:pattern -> p}+ when lvl' > TupPat && lvl <= TupPat ->
      (TupPat, fun p ->
        ln p _loc (Ppat_tuple(p::ps)))
  | c:STR("::") p':pattern when lvl' > ConsPat && lvl <= ConsPat ->
      (ConsPat, fun p ->
        let cons = { txt = Lident "::"; loc = _loc_c } in
        let args = loc_pat _loc (Ppat_tuple [p; p']) in
        ln p _loc (Ppat_construct(cons, Some args, false)))
  | te:STR(":") ty:typexpr when lvl' >= CoercePat && lvl <= CoercePat ->
      (CoercePat, fun p -> 
        ln p _loc (Ppat_constraint(p, ty)))

  end)

let pattern_suit =
  let f pat_suit =
    memoize2
      (fun lvl' lvl ->
         glr
         | (p1,f1):(pattern_suit_aux lvl' lvl) ->> (p2,f2):(pat_suit p1 lvl) -> p2, fun f -> f2 (f1 f)
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

let reserved_kwd = [ "->"; ":" ; "|" ]

type expression_lvl = Top | Let | Seq | If | Aff | Tupl | Disj | Conj | Eq | Append | Cons | Sum | Prod | Pow | Opp | App | Coerce | Dash | Dot | Prefix | Atom

let expression_lvls = [ Top; Let; Seq; If; Aff; Tupl; Disj; Conj; Eq; Append; Cons; Sum; Prod; Pow; Opp; App; Coerce; Dash; Dot; Prefix; Atom]

let let_prio lvl = if !extension then lvl else Let
let let_re = if !extension then "\\(let\\)\\|\\(val\\)\\b" else "let\\b"

let next_exp = function
    Top -> Let
  | Let -> Seq
  | Seq -> If
  | If -> Aff
  | Aff -> Tupl
  | Tupl -> Disj
  | Disj -> Conj
  | Conj -> Eq
  | Eq -> Append
  | Append -> Cons
  | Cons -> Sum
  | Sum -> Prod
  | Prod -> Pow
  | Pow -> Opp
  | Opp -> App
  | App -> Coerce
  | Coerce -> Dash
  | Dash -> Dot
  | Dot -> Prefix
  | Prefix -> Atom
  | Atom -> Atom

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
  if s = "-" || s = "-." then Opp else Prefix

let (expression_lvl, set_expression_lvl) = grammar_family ()
let expr = expression_lvl Top
let expression= expression_lvl Top
let loc_expr _loc e = { pexp_desc = e; pexp_loc = _loc; }

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
  | m:{ m:module_path STR"." }? id:{id:capitalized_ident -> id | false_kw -> "false" | true_kw -> "true" } ->
      match m with
      | None   -> Lident id
      | Some m -> Ldot(m, id)
  end 

let argument =
  glr
  | id:label STR(":") e:(expression_lvl (next_exp App)) -> (id, e)
  | id:opt_label STR(":") e:(expression_lvl (next_exp App)) -> (id, e)
  | id:label -> (id, loc_expr _loc (Pexp_ident { txt = Lident id; loc = _loc }))
  | id:opt_label -> (id, loc_expr _loc (Pexp_ident { txt = Lident id; loc = _loc }))
    (* NOTE the "id" in the first position of the couple was not prefixed with a "?". I guess this was a bug. *)
  | e:(expression_lvl (next_exp App)) -> ("", e)
  end

let parameter =
  glr
  | pat:pattern -> ("", None, pat)
  | STR("~") STR("(") id:lowercase_ident t:{ STR":" t:typexpr }? STR")" -> (
      let pat =  loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }) in
      let pat = match t with
      | None   -> pat
      | Some t -> loc_pat _loc (Ppat_constraint (pat, t))
      in
      (id, None, pat))
  | id:label STR":" pat:pattern -> (id, None, pat)
  | id:label -> (id, None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  | STR("?") STR"(" id:lowercase_ident t:{ STR":" t:typexpr -> t }? e:{STR"=" e:expression -> e}? STR")" -> (
      let pat = loc_pat _loc_id (Ppat_var { txt = "?"^id; loc = _loc_id }) in
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge _loc_id _loc_t) (Ppat_constraint(pat,t))
      in (("?"^id), e, pat))
  | id:opt_label STR":" STR"(" pat:pattern t:{STR(":") t:typexpr}? e:{STR("=") e:expression}? STR")" -> (
      let pat = match t with
                | None -> pat
                | Some t -> loc_pat (merge _loc_pat _loc_t) (Ppat_constraint(pat,t))
      in (id, e, pat))
  | id:opt_label STR":" pat:pattern -> (id, None, pat)
  | id:opt_label -> (id, None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  end

let right_member =
  glr
  | l:{lb:parameter}* STR("=") e:expression -> 
      let f (lbl,opt,pat) acc =
        loc_expr _loc (Pexp_function (lbl, opt, [pat, acc]))
      in
      List.fold_right f l e
  end

let value_binding =
  glr
  | pat:pattern e:right_member l:{and_kw pat:pattern e:right_member}* -> ((pat, e)::l)
  end

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
  | f:field STR("=") e:(expression_lvl (next_exp Seq)) -> ({ txt = f; loc = _loc_f},e) 
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
  | v:inst_var_name STR("=") e:expression -> { txt = v ; loc = _loc_v }, e 
  end

let class_body = declare_grammar ()

let let_binding = declare_grammar ()

(* Class expression *)
let class_expr = declare_grammar ()
let loc_pcl _loc desc = { pcl_desc = desc; pcl_loc = _loc }

let class_expr_base =
  glr
  | cp:class_path -> 
      let cp = { txt = cp; loc = _loc_cp } in
      loc_pcl _loc (Pcl_constr (cp, []))
  | STR("[") te:typexpr tes:{STR(",") te:typexpr}* cp:class_path ->
      let cp = { txt = cp; loc = _loc_cp } in
      loc_pcl _loc (Pcl_constr (cp, te :: tes))
  | STR("(") ce:class_expr STR(")") ->
      loc_pcl _loc ce.pcl_desc
  | STR("(") ce:class_expr STR(":") ct:class_type STR(")") ->
      loc_pcl _loc (Pcl_constraint (ce, ct))
  | fun_kw ps:parameter+ STR("->") ce:class_expr ->
      let f (l, eo, pat) acc =
        loc_pcl _loc (Pcl_fun (l, eo, pat, acc))
      in
      List.fold_right f ps ce
  | let_kw r:rec_flag lb:let_binding
    lbs:{and_kw lb:let_binding}* in_kw ce:class_expr ->
      loc_pcl _loc (Pcl_let (r, lb :: lbs, ce))
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

(* FIXME override *)
let class_field =
  let loc_pcf _loc desc = { pcf_desc = desc; pcf_loc = _loc } in
  glr
  | inherit_kw ce:class_expr id:{as_kw id:lowercase_ident}? ->
      loc_pcf _loc (Pcf_inher (Fresh, ce, id))
  | val_kw m:mutable_flag ivn:inst_var_name te:typexpr? STR("=")
    e:expr ->
      let ivn = { txt = ivn; loc = _loc_ivn } in
      let ex =
        match te with
        | None   -> e
        | Some t -> { pexp_desc = Pexp_poly (e, Some t)
                    ; pexp_loc  = _loc_te }
      in
      loc_pcf _loc (Pcf_val (ivn, m, Fresh, ex))
  | val_kw m:mutable_flag virtual_kw ivn:inst_var_name
    STR(":") te:typexpr ->
      let ivn = { txt = ivn; loc = _loc_ivn } in
      loc_pcf _loc (Pcf_valvirt (ivn, m, te))
  | val_kw virtual_kw mutable_kw ivn:inst_var_name STR(":") te:typexpr ->
      let ivn = { txt = ivn; loc = _loc_ivn } in
      loc_pcf _loc (Pcf_valvirt (ivn, Mutable, te))
  | method_kw p:private_flag mn:method_name ps:parameter* te:{STR(":")
    te:typexpr}? STR("=") e:expr ->
      let mn = { txt = mn; loc = _loc_mn } in
      let f (_,_,pat) acc =
        { pexp_desc = Pexp_function("", None, [(pat, acc)])
        ; pexp_loc  = _loc_ps }
      in
      let e : expression = List.fold_right f ps e in
      let te = { pexp_desc = Pexp_poly (e, te)
               ; pexp_loc  = _loc_te }
      in
      loc_pcf _loc (Pcf_meth (mn, p, Fresh, te))
  | method_kw p:private_flag mn:method_name STR(":")
    pte:poly_typexpr STR("=") e:expr ->
      let mn = { txt = mn ; loc = _loc_mn } in
      let et = { pexp_desc = Pexp_poly (e, Some pte)
               ; pexp_loc  = _loc_pte }
      in
      loc_pcf _loc (Pcf_meth (mn, p, Fresh, et))
  | method_kw p:private_flag virtual_kw mn:method_name STR(":")
    pte:poly_typexpr ->
      let mn = { txt = mn ; loc = _loc_mn } in
      loc_pcf _loc (Pcf_virt (mn, p, pte))
  | method_kw virtual_kw private_kw mn:method_name
    STR(":") pte:poly_typexpr ->
      let mn = { txt = mn ; loc = _loc_mn } in
      loc_pcf _loc (Pcf_virt (mn, Private, pte))
  | constraint_kw te:typexpr STR("=") te':typexpr ->
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
    cn:class_name ps:parameter* ct:{STR(":") ct:class_type}? STR("=")
    ce:class_expr ->
      let params, variance = List.split tp in
      let params = List.map (function None   -> { txt = ""; loc = _loc}
                                    | Some x -> x) params
      in
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

let module_expr = declare_grammar ()
let mexpr_loc _loc desc = { pmod_desc = desc; pmod_loc = _loc }
let module_type = declare_grammar ()
let mtyp_loc _loc desc = { pmty_desc = desc; pmty_loc = _loc }

(* Expressions *)
let expression_base = memoize1 (fun lvl ->
  glr
  | v:inst_var_name STR("<-") e:(expression_lvl (next_exp Aff)) when lvl <= Aff->
      (Aff, loc_expr _loc (Pexp_setinstvar({ txt = v ; loc = _loc_v }, e)))
  | id:value_path -> (Atom, loc_expr _loc (Pexp_ident { txt = id; loc = _loc_id }))
  | c:constant -> (Atom, loc_expr _loc (Pexp_constant c))
  | let_kw r:{r:rec_flag l:value_binding in_kw e:(expression_lvl (let_prio lvl)) when (lvl < App)
                  -> (Let, loc_expr _loc (Pexp_let (r, l, e)))
             | module_kw mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> ({ txt = mn; loc = _loc_mn}, mt)}*
                 mt:{STR":" mt:module_type }? STR"=" me:module_expr in_kw e:(expression_lvl (let_prio lvl)) when (lvl < App) ->
               let me = match mt with None -> me | Some mt -> mexpr_loc _loc (Pmod_constraint(me, mt)) in
               let me = List.fold_left (fun acc (mn,mt) ->
                 mexpr_loc _loc (Pmod_functor(mn, mt, acc))) me (List.rev l) in
               (Let, loc_expr _loc (Pexp_letmodule({ txt = mn ; loc = _loc_mn }, me, e)))
             } -> r
  | function_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_function("", None, l)))
  | fun_kw l:{lbl:parameter}* STR"->" e:(expression_lvl (let_prio lvl)) when (lvl < App) -> 
     (Let, (List.fold_right (fun (lbl,opt,pat) acc -> loc_expr _loc (Pexp_function(lbl, opt, [pat, acc]))) l e))
  | match_kw e:expression with_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_match(e, l)))
  | try_kw e:expression with_kw l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, loc_expr _loc (Pexp_try(e, l)))
  | if_kw c:expression then_kw e:(expression_lvl If) e':{else_kw e:(expression_lvl If)}? when (lvl <= If) ->
     (If, loc_expr _loc (Pexp_ifthenelse(c,e,e')))
  | STR("(") e:expression? STR(")") -> (Atom, match e with Some e -> e | None -> loc_expr _loc (Pexp_tuple([])))
  | begin_kw e:expression? end_kw -> (Atom, match e with Some e -> e | None -> loc_expr _loc (Pexp_tuple([])))
  | c:constructor e:{ e:(expression_lvl App) when lvl <= App }? -> (App, loc_expr _loc (Pexp_construct({ txt = c; loc = _loc_c},e,false)))
  | assert_kw e:{ false_kw -> Pexp_assertfalse | e:(expression_lvl App) -> Pexp_assert(e)} when (lvl <= App) 
      -> (App,  loc_expr _loc e)
  | lazy_kw e:(expression_lvl App) when (lvl <= App) -> (App,  loc_expr _loc (Pexp_lazy(e)))
  | STR("`") l:RE(ident_re) e:{e:(expression_lvl App)}? when (lvl <= App) -> (App, loc_expr _loc (Pexp_variant(l,e)))
  | STR("[|") l:expression_list STR("|]") -> (Atom, loc_expr _loc (Pexp_array l))
  | STR("[") l:expression_list STR("]") ->
     (Atom, (List.fold_right (fun x acc ->
       loc_expr _loc (Pexp_construct({ txt = Lident "::"; loc = _loc}, Some (loc_expr _loc (Pexp_tuple [x;acc])), false)))
                    l (loc_expr _loc (Pexp_construct({ txt = Lident "[]"; loc = _loc}, None, false)))))
  | STR("{") e:{e:expression with_kw}? l:record_list STR("}") ->
     (Atom, loc_expr _loc (Pexp_record(l,e)))
  | p:prefix_symbol ->> let lvl' = prefix_prio p in e:(expression_lvl lvl') when lvl <= lvl' -> 
     let p = match p with "-" -> "~-" | "-." -> "~-." | _ -> p in
     (lvl', loc_expr _loc (Pexp_apply(loc_expr _loc_p (Pexp_ident { txt = Lident p; loc = _loc_p}), ["", e])))
  | while_kw e:expression do_kw e':expression done_kw ->
      (Atom, loc_expr _loc (Pexp_while(e, e')))
  | for_kw id:lowercase_ident STR("=") e:expression d:downto_flag
    e':expression do_kw e'':expression done_kw ->
      (Atom, loc_expr _loc (Pexp_for({ txt = id ; loc = _loc_id}, e, e', d, e'')))
  | new_kw p:class_path -> (Atom, loc_expr _loc (Pexp_new({ txt = p; loc = _loc_p})))
  | object_kw o:class_body end_kw -> (Atom, loc_expr _loc (Pexp_object o))
  | STR("{<") l:{ o:obj_item l:{STR";" o:obj_item}* STR(";")? -> o::l }?[[]] STR(">}") -> (Atom, loc_expr _loc (Pexp_override l))
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
     loc_expr (merge x.pexp_loc res.pexp_loc) (Pexp_sequence(x,mk_seq l))

let semi_col = black_box 
  (fun str pos ->
   let len = String.length str in
   if len > pos && str.[pos] = ';' && (len = pos + 1 || str.[pos+1] <> ';') then ((), pos+1)
   else raise Give_up)
  (Charset.singleton ';') false (";")

let expression_suit_aux = memoize2 (fun lvl' lvl ->
  let ln f _loc e = loc_expr (merge f.pexp_loc _loc) e in
  glr
    l:{a:argument}+ when (lvl' > App && lvl <= App) -> 
      (App, fun f -> ln f _loc (Pexp_apply(f,l)))
  | l:{STR(",") e:(expression_lvl (next_exp Tupl))}+ when (lvl' > Tupl && lvl <= Tupl) -> 
      (Tupl, fun f -> ln f _loc (Pexp_tuple(f::l)))
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
  | t:type_coercion when (lvl' >= Coerce && lvl <= Coerce) ->
      (Coerce, fun e' -> ln e' _loc (Pexp_constraint(e', fst t, snd t)))
  | op:infix_op ->> let p = infix_prio op in let a = assoc p in 
                    e:(expression_lvl (if a = Right then p else next_exp p))
                      when lvl <= p && (lvl' > p || (a = Left && lvl' = p)) ->
      (p, fun e' -> ln e' _loc_e (
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
         | (p1,f1):(expression_suit_aux lvl' lvl) ->> (p2,f2):(expression_suit p1 lvl)
	       -> p2, fun f -> f2 (f1 f)
         | EMPTY -> (lvl', fun f -> f) end)
  in
  let rec res x y = f res x y in
  res

let _ = set_expression_lvl (fun lvl ->
    glr
      (lvl',e):(expression_base lvl) ->> (_, f):(expression_suit lvl' lvl) -> f e
    end) expression_lvls

let override_flag =
  glr
    o:STR("!")? -> (if o <> None then Override else Fresh)
  end

(****************************************************************************
 * Module expressions (module implementations)                              *
 ****************************************************************************)
let module_item = declare_grammar ()
let signature_item = declare_grammar ()

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
  end

let mod_constraint = 
  glr
  | type_kw tdef:typedef_in_constraint ->
     fst tdef, Pwith_type(snd tdef)
  | module_kw m1:module_path STR("=") m2:extended_module_path ->
     ({ txt = m1; loc = _loc_m1 }, Pwith_module { txt = m2; loc = _loc_m2 })
(* TODO: Pwith_typesubst and Pwithmodsubst are missing *)
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
  | RE(let_re) r:rec_flag l:value_binding -> Pstr_value (r, l)
  | external_kw n:value_name STR":" ty:typexpr STR"=" ls:string_literal* ->
      let l = List.length ls in
      if l < 1 || l > 3 then raise Give_up;
      Pstr_primitive({ txt = n; loc = _loc_n }, { pval_type = ty; pval_prim = ls; pval_loc = _loc})
  | td:type_definition -> Pstr_type td
  | ex:exception_definition -> ex
  | module_kw r:{mn:module_name l:{ STR"(" mn:module_name STR":" mt:module_type STR ")" -> ({ txt = mn; loc = _loc_mn}, mt)}*
       mt:{STR":" mt:module_type }? STR"=" me:module_expr ->
     let me = match mt with None -> me | Some mt -> mexpr_loc _loc (Pmod_constraint(me, mt)) in
     let me = List.fold_left (fun acc (mn,mt) ->
       mexpr_loc _loc (Pmod_functor(mn, mt, acc))) me (List.rev l) in
     Pstr_module({ txt = mn ; loc = _loc_mn }, me)
  |             type_kw mn:modtype_name STR"=" mt:module_type ->
     Pstr_modtype({ txt = mn ; loc = _loc_mn }, mt) } -> r
  | open_kw o:override_flag m:module_path -> Pstr_open(o, { txt = m; loc = _loc_m} )
  | include_kw me:module_expr -> Pstr_include me
  | class_kw r:{ ctd:classtype_definition -> Pstr_class_type ctd
               | cds:class_definition -> Pstr_class cds } -> r
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
  | val_kw n:value_name STR(":") ty:typexpr ->
     Psig_value({ txt = n; loc = _loc_n }, { pval_type = ty; pval_prim = []; pval_loc = _loc})
  | external_kw n:value_name STR":" ty:typexpr STR"=" ls:string_literal* ->
      let l = List.length ls in
      if l < 1 || l > 3 then raise Give_up;
      Psig_value({ txt = n; loc = _loc_n }, { pval_type = ty; pval_prim = ls; pval_loc = _loc})
  | td:type_definition -> Psig_type td
  | (name,ed):exception_declaration -> Psig_exception (name, ed)
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
  let b = Buffer.create 0x10000 in
  let ch = match !file with
      None -> stdin
    | Some name -> open_in name
  in
  try
    while true do
      let l = input_line ch in
      Buffer.add_string b l; Buffer.add_char b '\n'
    done;
    assert false
  with
    End_of_file ->
  let s = Buffer.contents b in
  try
    if entry = Impl then 
      `Struct (parse_string structure blank s)
    else
      `Sig (parse_string signature blank s)
  with
    Parse_error (n,l) ->
    let pos = find_pos s n in
    let msgs = String.concat " | " l in
    Lexing.(Printf.eprintf "File %S, line %d, characters %d:\n\
                            Error: Syntax error, %s expected\n"
                            pos.pos_fname pos.pos_lnum
                            (pos.pos_cnum - pos.pos_bol) msgs);
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
    let magic = Config.ast_impl_magic_number in
    output_string stdout magic;
    output_value stdout (match !file with None -> "" | Some name -> name);
    begin
      match ast with 
      | `Struct ast -> output_value stdout ast
      | `Sig ast -> output_value stdout ast
    end;
    close_out stdout
  end
