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

(* Should contain the name of the file being parsed. *)
let fname = ref ""

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
          Hashtbl.add bol i lnum;
          lnum, i
    else if i <= 0 then (1, i)
    else fn (i-1)
  in
  let (lnum, bol) = fn n in
  Lexing.({ pos_fname = !fname;
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

(* Identifiers *)
(* NOTE "_" is not a valid identifier, we handle it separately *)
let lident_re = "\\b\\([a-z][a-zA-Z0-9_']*\\)\\|\\([_][a-zA-Z0-9_']+\\)\\b"
let cident_re = "\\b[A-Z][a-zA-Z0-9_']*\\b"
let ident_re = "\\b[A-Za-z_][a-zA-Z0-9_']*\\b"

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

let integer_literal =
  glr
    i:RE(int_dec_re ^ "\\b") -> int_of_string i
  | i:RE(int_hex_re ^ "\\b") -> int_of_string i
  | i:RE(int_oct_re ^ "\\b") -> int_of_string i
  | i:RE(int_bin_re ^ "\\b") -> int_of_string i
  end

let int32_lit =
  glr
    i:RE("\\(" ^ int_dec_re ^ "\\)l\\b")[groupe 1] -> Int32.of_string i
  | i:RE("\\(" ^ int_hex_re ^ "\\)l\\b")[groupe 1] -> Int32.of_string i
  | i:RE("\\(" ^ int_oct_re ^ "\\)l\\b")[groupe 1] -> Int32.of_string i
  | i:RE("\\(" ^ int_bin_re ^ "\\)l\\b")[groupe 1] -> Int32.of_string i
  end

let int64_lit =
  glr
    i:RE("\\(" ^ int_dec_re ^ "\\)L\\b")[groupe 1] -> Int64.of_string i
  | i:RE("\\(" ^ int_hex_re ^ "\\)L\\b")[groupe 1] -> Int64.of_string i
  | i:RE("\\(" ^ int_oct_re ^ "\\)L\\b")[groupe 1] -> Int64.of_string i
  | i:RE("\\(" ^ int_bin_re ^ "\\)L\\b")[groupe 1] -> Int64.of_string i
  end

let nat_int_lit =
  glr
    i:RE("\\(" ^ int_dec_re ^ "\\)n\\b")[groupe 1] -> Nativeint.of_string i
  | i:RE("\\(" ^ int_hex_re ^ "\\)n\\b")[groupe 1] -> Nativeint.of_string i
  | i:RE("\\(" ^ int_oct_re ^ "\\)n\\b")[groupe 1] -> Nativeint.of_string i
  | i:RE("\\(" ^ int_bin_re ^ "\\)n\\b")[groupe 1] -> Nativeint.of_string i
  end

(* Floating-point literals *)
let float_lit_dec    = "[-]?[0-9][0-9_]*[.][0-9_]*\\([eE][+-][0-9][0-9_]*\\)?"
let float_lit_no_dec = "[-]?[0-9][0-9_]*[eE][+-][0-9][0-9_]*"

let float_literal =
  glr
    f:RE(float_lit_dec)   -> f
  | f:RE(float_lit_no_dec) -> f
  end

(* Character literals *)
let char_regular = "[ !#-&(-Z^-~]\\|\\[\\|\\]"
let char_escaped = "[\\\\][\\\\\\\"\\\'ntbrs ]"
let char_dec     = "[\\\\][0-9][0-9][0-9]"
let char_hex     = "[\\\\][x][0-9a-fA-F][0-9a-fA-F]"

let one_char =
  glr
    c:RE(char_regular) -> c.[0]
  | c:RE(char_escaped) -> (match c.[1] with
                            | 'n' -> '\n'
                            | 't' -> '\t'
                            | 'b' -> '\b'
                            | 'r' -> '\r'
                            | 's' -> ' '
                            | c   -> c)
  | c:RE(char_dec)     -> (let str = String.sub c 1 3 in
                            let i = Scanf.sscanf str "%i" (fun i -> i) in
                            if i > 255 then assert false; (* TODO error message *)
                            char_of_int i)
  | c:RE(char_hex)     -> (let str = String.sub c 2 2 in
                            let str' = String.concat "" ["0x"; str] in
                            let i = Scanf.sscanf str' "%i" (fun i -> i) in
                            char_of_int i)
  end

let char_literal =
  change_layout (
    glr STR("\'") c:one_char STR("\'") -> c end
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
      STR("\"") lc:one_char*
        lcs:(glr RE(interspace) lc:one_char** -> lc end)*
        STR("\"") -> char_list_to_string (List.flatten (lc::lcs))
    end
  ) no_blank

(* Naming labels *)
let label_name = lowercase_ident

let label =
  glr
    STR("~") l:label_name -> l
  end

let optlabel =
  glr
    STR("?") l:label_name -> l
  end

(* Prefix and infix symbols *)
let reserved_symbols =
  [ "#" ; "'" ; "(" ; ")" ; "," ; "->" ; "." ; ".." ; ":" ; ":>" ; ";" ; ";;" ; "<-"
  ; ">]" ; ">}" ; "?" ; "[" ; "[<" ; "[>" ; "[|" ; "]" ; "_" ; "`" ; "{" ; "{<" ; "|" ; "|]" ; "}" ; "~" ]

let is_reserved_symb s =
  List.mem s reserved_symbols

let infix_symb_re  = "[=<>@^|&+*/$%:-][!$%&*+./:<=>?@^|~-]*"
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
  | sym:STR("mod")   -> "mod"
  | sym:STR("land")  -> "land"
  | sym:STR("lor")   -> "lor"
  | sym:STR("lxor")  -> "lxor"
  | sym:STR("lsl")   -> "lsl"
  | sym:STR("lsr")   -> "lsr"
  | sym:STR("asr")   -> "asr"
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

(* Refering to named objects *)
let module_path =
  glr
    mn:module_name mns:{STR(".") m:module_name -> m}* ->
      List.fold_left (fun acc m -> Ldot(acc, m)) (Lident mn) mns
  end
 
let extended_module_path = declare_grammar ()

let extended_module_name =
  glr
    mn:module_name
    emps:{STR("(") emp:extended_module_path STR(")") -> emp}* ->
      List.fold_left (fun acc m -> Lapply(acc, m)) (Lident mn) emps
  end

let _ = set_grammar extended_module_path (
  glr
    emn:extended_module_name emns:{STR(".") emn:extended_module_name -> emn}* ->
      let rec ldot_cons emn emn' =
        match emn' with
        | Lident mn       -> Ldot(emn, mn)
        | Ldot(mp, mn)    -> Ldot(ldot_cons emn mp, mn)
        | Lapply(mp, mp') -> Lapply(ldot_cons emn mp, mp')
      in
      List.fold_left (fun acc m -> ldot_cons acc m) emn emns
  end)

let value_path =
  glr
    mp:{m:module_path STR(".") -> m}? vn:value_name ->
      match mp with
      | None   -> Lident vn
      | Some p -> Ldot(p, vn)
  end

let constr =
  glr
    mp:{m:module_path STR(".") -> m}? cn:constr_name ->
      match mp with
      | None   -> Lident cn
      | Some p -> Ldot(p, cn)
  end

let typeconstr =
  glr
    mp:{m:module_path STR(".") -> m}? tcn:typeconstr_name ->
      match mp with
      | None   -> Lident tcn
      | Some p -> Ldot(p, tcn)
  end

let field =
  glr
    mp:{m:module_path STR(".") -> m}? fn:field_name ->
      match mp with
      | None   -> Lident fn
      | Some p -> Ldot(p, fn)
  end

let class_path =
  glr
    mp:{m:module_path STR(".") -> m}? cn:class_name ->
      match mp with
      | None   -> Lident cn
      | Some p -> Ldot(p, cn)
  end

let modtype_path =
  glr
    mp:(glr m:extended_module_path STR(".") -> m end)? mtn:modtype_name ->
      match mp with
      | None   -> Lident mtn
      | Some p -> Ldot(p, mtn)
  end

let classtype_path =
  glr
    mp:(glr m:extended_module_path STR(".") -> m end)? cn:class_name ->
      match mp with
      | None   -> Lident cn
      | Some p -> Ldot(p, cn)
  end

(****************************************************************************
 * Type expressions                                                         *
 ****************************************************************************)

type type_prio = TopType | As | Arr | Prod | Dash | AppType | AtomType

let type_prios = [TopType; As; Arr; Prod; Dash; AppType; AtomType]

let typeexpr = declare_grammar ()
let typeexpr_lvl, set_typeexpr_lvl = grammar_family ()
let loc_typ _loc typ = { ptyp_desc = typ; ptyp_loc = _loc; }

let poly_typexpr =
  glr
  | te:typeexpr -> te
  | ids:{STR("'") id:ident}+ STR(".") te:typeexpr ->
      assert false (* TODO *)
  end
   
let method_type =
  glr
  | mn:method_name STR(":") pte:poly_typexpr ->
      () (* TODO *)
  end

let tag_spec =
  glr
  | STR("`") tn:tag_name te:{RE("of\\b") te:typeexpr}? ->
      () (* TODO *)
  | te:typeexpr ->
      () (* TODO *)
  end

let tag_spec_first =
  glr
  | tn:tag_name te:{RE("of\\b") te:typeexpr} ->
      () (* TODO *)
  | te:typeexpr? STR("|") ts:tag_spec ->
      () (* TODO *)
  end

let tag_spec_full =
  glr
  | STR("`") tn:tag_name tes:{RE("of\\b") STR("&")? te:typeexpr
    tes:{STR("&") te:typeexpr}* -> (te::tes)}? ->
      () (* TODO *)
  | te:typeexpr ->
      () (* TODO *)
  end

let polymorphic_variant_type : core_type grammar =
  glr
  | STR("[") tsf:tag_spec_first tss:{STR("|")
    ts:tag_spec -> ts}* STR("]") ->
      assert false (* TODO *) 
  | STR("[>") ts:tag_spec?
    tss:{STR("|") ts:tag_spec}* STR("]") ->
      assert false (* TODO *) 
  | STR("[<") STR("|")? tfs:tag_spec_full tsfs:{STR("|") tsf:tag_spec_full}
    tns:{STR(">") tns:{STR("`") tns:tag_name}+}? STR("]") ->
      assert false (* TODO *)  
  end

let typeexpr_base : core_type grammar =
  glr
  | STR("`") id:ident ->
      loc_typ _loc (Ptyp_var id)
  | STR("_") ->
      loc_typ _loc Ptyp_any
  | STR("(") te:typeexpr STR(")") ->
      loc_typ _loc te.ptyp_desc
  | ln:optlabel te:typeexpr STR("->") te':typeexpr ->
      let opt = { txt = Lident "option"; loc = _loc_te } in
      let teopt = loc_typ te.ptyp_loc (Ptyp_constr (opt, [te])) in
      loc_typ _loc (Ptyp_arrow (ln, teopt, te'))
  | ln:label STR(":") te:typeexpr STR("->") te':typeexpr ->
      loc_typ _loc (Ptyp_arrow (ln, te, te'))
  | tc:typeconstr ->
      loc_typ _loc (Ptyp_constr ({ txt = tc; loc = _loc_tc }, []))
  | STR("(") te:typeexpr tes:{STR(",") te:typeexpr}* STR(")") tc:typeconstr ->
      let constr = { txt = tc ; loc = _loc_tc } in
      loc_typ _loc (Ptyp_constr (constr, te::tes))
  | pvt:polymorphic_variant_type -> pvt
  | STR("<") STR("..")? STR(">") ->
      assert false (* TODO *)
  | STR("<") mt:method_type mts:{STR(";") mt:method_type -> mt}*
    {_sc:STR(";") STR("..")?}? ->
      assert false (* TODO *)
  | STR("#") cp:class_path ->
      assert false (* TODO *)
  | STR("(") te:typeexpr tes:{STR(",") te:typeexpr}* STR(")")
    STR("#") cp:class_path ->
      assert false (* TODO *)
  end

let next_type_prio = function
  | TopType -> As
  | As -> Arr
  | Arr -> Prod
  | Prod -> Dash
  | Dash -> AppType
  | AppType -> AtomType
  | AtomType -> AtomType

let typeexpr_suit_aux : type_prio -> type_prio -> (type_prio * (core_type -> core_type)) grammar = memoize1 (fun lvl' lvl ->
  glr
  | STR("->") te':(typeexpr_lvl Arr) when lvl' > Arr && lvl <= Arr ->
      (Arr, fun te -> loc_typ (merge te.ptyp_loc _loc) (Ptyp_arrow ("", te, te')))
  | tes:{STR("*") te:(typeexpr_lvl (next_type_prio Prod))}+  when lvl' > Prod && lvl <= Prod->
      (Prod, fun te -> loc_typ (merge te.ptyp_loc _loc) (Ptyp_tuple (te::tes)))
  | tc:typeconstr when lvl' >= AppType && lvl <= AppType ->
      (AppType, fun te -> loc_typ (merge te.ptyp_loc _loc)
        (Ptyp_constr ({ txt = tc; loc = _loc_tc }, [te])))
  | RE("as\\b") STR("`") id:ident when lvl' >= As && lvl <= As ->
      (As, fun te -> loc_typ (merge te.ptyp_loc _loc) (Ptyp_alias (te, id)))
  | STR("#") cp:class_path when lvl' >= Dash && lvl <= Dash ->
      (Dash, fun te -> loc_typ (merge te.ptyp_loc _loc) (assert false (* TODO *)))
  end)

let typeexpr_suit =
  let f type_suit =
    memoize2
      (fun lvl' lvl ->
         glr
         | (p1,f1):(typeexpr_suit_aux lvl' lvl) ->> (p2,f2):(type_suit p1 lvl) -> p2, fun f -> f2 (f1 f)
         | EMPTY -> (lvl', fun f -> f) 
         end)
  in
  let rec res x y = f res x y in
  res

let _ = set_typeexpr_lvl (fun lvl ->
  glr
  | t:typeexpr_base ft:(typeexpr_suit AtomType lvl) -> snd ft t
  end) type_prios

let _ = set_grammar typeexpr (typeexpr_lvl TopType)

(****************************************************************************
 * Type and exception definitions                                           *
 ****************************************************************************)

(* Type definition *)
type variance = Covariant | Contravariant

let type_param =
  glr
    var:RE("[+-]")? STR("`") id:ident ->
      let variance =
        match var with
        | None     -> (false, false)
        | Some "+" -> (true , false)
        | Some "-" -> false, true
        | _        -> assert false
      in (Some { txt = id; loc = _loc_id }, variance) (* FIXME None in which case? *)
  end

let type_params =
  glr
    tp:type_param -> [tp]
  | STR("(") tp:type_param
    tps:{STR(",") tp:type_param -> tp}* STR(")") -> tp::tps
  end

let type_equation =
  glr
    STR("=") te:typeexpr -> te
  end

let type_constraint =
  glr
    s:RE("\\bconstraint\\b") STR("`") id:ident STR("=") te:typeexpr ->
      loc_typ _loc_id (Ptyp_var id), te, _loc_s
  end

let constr_decl =
  let constr_name =
    glr
      cn:constr_name    -> cn
    | STR("(") STR(")") -> "()"
    end
  in
  glr
    cn:constr_name tes:{RE("\\bof\\b") te:typeexpr
    tes:{STR("*") te:typeexpr -> te}* -> te::tes}? ->
      let c = { txt = cn; loc = _loc_cn } in
      let tel = match tes with
                | None   -> []
                | Some l -> l
      in
      (c, tel, None, _loc_cn) (* TODO GADT Stuff *)
  end

let field_decl =
  let mutable_flag =
    glr
      m:RE("\\bmutable\\b")? -> (match m with
                                 | None -> Immutable
                                 | _    -> Mutable)
    end
  in
  glr
    m:mutable_flag fn:field_name STR(":") pte:poly_typexpr ->
      { txt = fn; loc = _loc_fn }, m, pte, _loc_m
  end

let type_representation =
  glr
    STR("|")? cd:constr_decl
    cds:{STR("|") cd:constr_decl -> cd}* ->
      Ptype_variant (cd::cds)
  | STR("{") fd:field_decl
    fds:{STR(";") fd:field_decl -> fd}*
    STR(";")? STR("}") ->
      Ptype_record (fd::fds)
  end

let type_information =
  glr
    te:type_equation?
    tr:{STR("=") tr:type_representation -> tr}?
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
    tps:type_params? tcn:typeconstr_name ti:type_information ->
      let (te, tkind, cstrs) = ti in
      let tps = match tps with
                | None   -> []
                | Some l -> l
      in
      let tdec =
        { ptype_params   = List.map fst tps
        ; ptype_cstrs    = cstrs
        ; ptype_kind     = tkind
        ; ptype_private  = Public (* FIXME ?? *)
        ; ptype_manifest = te
        ; ptype_variance = List.map snd tps
        ; ptype_loc      = _loc_tps
        }
      in ({ txt = tcn; loc = _loc_tcn }, tdec) (*  tps, ti) *)
  end

let type_definition =
  glr
    RE("\\btype\\b") td:typedef tds:{RE("\\band\\b") td:typedef -> td}* ->
      (td::tds)
  end

(* Exception definition *)
type expn = NewExpn of core_type list option
          | SynExpn of Longident.t loc

let exception_definition =
  glr
    RE("\\bexception\\b") cn:constr_name STR("=") c:constr ->
      { txt = cn; loc = _loc_cn }, SynExpn { txt = c; loc = _loc_c }
 else RE("\\bexception\\b") cn:constr_name
    typ:{RE("\\bof\\b") te:typeexpr
    tes:{STR("*") te:typeexpr -> te}* -> (te::tes) }? ->
      { txt = cn; loc = _loc_cn }, NewExpn typ
  end

(****************************************************************************
 * Constants and Patterns                                                   *
 ****************************************************************************)

let constant =
  glr
    f:float_literal         -> Const_float f
  | c:char_literal          -> Const_char c
  | s:string_literal        -> Const_string s
  | i:int32_lit -> Const_int32 i
  | i:int64_lit -> Const_int64 i
  | i:nat_int_lit -> Const_nativeint i
  | i:integer_literal -> Const_int i
  end

let pattern = declare_grammar ()
let loc_pat _loc pat = { ppat_desc = pat; ppat_loc = _loc; }

let _ = set_grammar pattern (
  glr
    vn:value_name ->
      loc_pat _loc_vn (Ppat_var { txt = vn; loc = _loc_vn })
  | STR("_") ->
      loc_pat _loc Ppat_any
  | c:constant ->
      loc_pat _loc_c (Ppat_constant c)
(*  | p:pattern RE("as\\b") vn:value_name ->
      { ppat_desc = Ppat_alias (p, { txt = vn; loc= _loc_vn })
      ; ppat_loc = _loc_p }*)
  | par:STR("(") p:pattern te:{STR(":") t:typeexpr -> t}? STR(")") ->
      let pat =
        match te with
        | None    -> p.ppat_desc
        | Some ty -> Ppat_constraint(p, ty)
      in loc_pat _loc_par pat
(*  | p:pattern STR("|") p':pattern ->
      { ppat_desc = Ppat_or(p, p')
      ; ppat_loc = _loc_p }*)
  | c:constr p:pattern? ->
      loc_pat _loc_c (Ppat_construct({ txt = c; loc = _loc_c }, p, false))
  | c:RE("false\\b") ->
      let fls = { txt = Lident "false"; loc = _loc_c } in
      loc_pat _loc_c (Ppat_construct (fls, None, false))
  | c:RE("true\\b")  -> 
      let tru = { txt = Lident "true"; loc = _loc_c } in
      loc_pat _loc_c (Ppat_construct (tru, None, false))
  | s:STR("`") c:tag_name p:pattern? ->
      loc_pat _loc_s (Ppat_variant (c, p))
  | s:STR("#") t:typeconstr ->
      loc_pat _loc_s (Ppat_type { txt = t; loc = _loc_t })
(*  | p:pattern ps:{STR(",") p:pattern -> p}+ ->
      { ppat_desc = Ppat_tuple(p::ps)
      ; ppat_loc = _loc_p }*)
  | s:STR("{") f:field STR("=") p:pattern
    fps:{STR(";") f:field STR("=") p:pattern -> ({ txt = f; loc = _loc_f }, p)}*
    clsd:{STR(";") STR("_") -> ()}? STR(";")? STR("}") ->
      let all = ({ txt = f; loc = _loc_f },p)::fps in
      let cl = match clsd with
               | None   -> Closed
               | Some _ -> Open
      in
      loc_pat _loc_s (Ppat_record (all, cl))
  | STR("[") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("]") ->
      (* FIXME don't know how to handle loc in this case since it is syntactic sugar (isn't it?) *)
      let nil = { txt = Lident "[]"; loc = _loc_p } in
      let cons x xs =
        let c = { txt = Lident "::"; loc = x.ppat_loc } in
        let cons = Ppat_construct (c, Some (loc_pat x.ppat_loc (Ppat_tuple [x;xs])), false) in
        loc_pat x.ppat_loc cons
      in
      List.fold_right cons (p::ps) (loc_pat _loc_p (Ppat_construct (nil, None, false)))
  | s:STR("[") STR("]") ->
      let nil = { txt = Lident "[]"; loc = _loc_s } in
      loc_pat _loc_s (Ppat_construct (nil, None, false))
(*  | p:pattern STR("::") p':pattern ->
      assert false (* TODO *)*)
  | s:STR("[|") p:pattern ps:{STR(";") p:pattern -> p}* STR(";")? STR("|]") ->
      loc_pat _loc_s (Ppat_array (p::ps))
  | s:STR("[|") STR("|]") ->
      loc_pat _loc_s (Ppat_array []) (* FIXME not sure if this should be a constructor instead *)
  | s:STR("(") STR(")") ->
      let unt = { txt = Lident "()"; loc = _loc_s } in
      loc_pat _loc_s (Ppat_construct (unt, None, false))
  | s:RE("begin\\b") STR("end\\b") ->
      let unt = { txt = Lident "()"; loc = _loc_s } in
      loc_pat _loc_s (Ppat_construct (unt, None, false))
  end)

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
  | _ when List.mem s ["lsl"; "lsr"; "ast"] -> Pow
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

let (expression, set_expression) = grammar_family ()
let loc_expr _loc e = { pexp_desc = e; pexp_loc = _loc; }

let array_function loc str name =
  loc_expr loc (Pexp_ident ({ txt = Longident.Ldot(Longident.Lident str, (if !fast then "unsafe_" ^ name else name)); loc  }))

let bigarray_function loc str name =
  loc_expr loc (Pexp_ident ({ txt = Longident.Ldot(Longident.Ldot(Longident.Lident "Bigarray", str),
						   (if !fast then "unsafe_" ^ name else name)); loc  }))
let untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get loc arr arg =
  let get = if !fast then "unsafe_get" else "get" in
  match untuplify arg with
    [c1] ->
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
    [c1] ->
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

let module_path = declare_grammar ()

let module_path_aux =
  glr
    id:ident l:{ STR"(" m:module_path STR")" }* ->
      List.fold_left (fun acc m -> Longident.Lapply(acc, m)) (Longident.Lident id) l
  end

let _ = set_grammar module_path (
  glr
    m:module_path_aux l:{ STR(".") m:ident }* ->
     List.fold_left (fun acc m -> Longident.Ldot(acc, m)) m l
  end)

let constructor =
  glr
    m:{ m:module_path STR"." }? id:{id:capitalized_ident -> id | RE"false\\b" -> "false" | RE"true\\b" -> "true" } ->
      match m with None -> Longident.Lident id
		 | Some m -> Longident.Ldot(m, id)
  end 

let argument =
  glr
    STR("~") id:lowercase_ident STR(":") e:(expression (next_exp App)) -> (id, e)
  | STR("?") id:lowercase_ident STR(":") e:(expression (next_exp App)) -> (("?"^id), e)
  | STR("~") id:lowercase_ident -> (id, loc_expr _loc (Pexp_ident { txt = Longident.Lident id; loc = _loc }))
  | STR("?") id:lowercase_ident -> (id, loc_expr _loc (Pexp_ident { txt = Longident.Lident ("?"^id); loc = _loc }))
  | e:(expression (next_exp App)) -> ("", e)
  end

let parameter =
  glr
    pat:pattern -> ("", None, pat)
  | STR("~") STR("(") id:lowercase_ident t:{ STR":" t:typeexpr }? STR")" -> (
      let pat =  loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }) in
      let pat = match t with
      | None -> pat
      | Some t -> loc_pat _loc (Ppat_constraint (pat, t))
      in
      (id, None, pat))
  | STR("~") id:lowercase_ident STR":" pat:pattern -> (id, None, pat)
  | STR("~") id:lowercase_ident -> (id, None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  | STR("?") STR"(" id:lowercase_ident t:{ STR":" t:typeexpr -> t }? e:{STR"=" e:(expression Top) -> e}? STR")" -> (
      let pat = loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }) in
      let pat = match t with
	| None -> pat
	| Some t -> loc_pat (merge _loc_id _loc_t) (Ppat_constraint(pat,t))
      in (("?"^id), e, pat))
  | STR("?") id:lowercase_ident STR":" STR"(" pat:pattern t:{ STR":" t:typeexpr -> t }? e:{ STR"=" e:(expression Top) -> e}? STR")" -> (
      let pat = match t with
	| None -> pat
	| Some t -> loc_pat (merge _loc_pat _loc_t) (Ppat_constraint(pat,t))
      in (("?"^id), e, pat))
  | STR("?") id:lowercase_ident STR":" pat:pattern -> (("?"^id), None, pat)
  | STR("?") id:lowercase_ident -> (("?"^id), None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  end

let right_member =
  glr
    l:{lb:parameter}* STR("=") e:(expression Top) -> 
      List.fold_right (fun (lbl,opt,pat) e ->
		       loc_expr _loc (Pexp_function (lbl, opt, [pat, e]))) l e
  end

let value_binding =
  glr
    pat:pattern e:right_member l:{RE("and\\b") pat:pattern e:right_member -> (pat, e)}* -> ((pat, e)::l)
  end

let match_cases = memoize1 (fun lvl ->
  glr
     l:{STR"|"? pat:pattern STR"->" e:(expression lvl) 
         l:{STR"|" pat:pattern STR"->" e:(expression lvl) -> (pat,e)}*
         -> ((pat,e)::l)}?[[]] -> l
  end)

let type_coercion =
  glr
    STR(":") t:typeexpr t':{STR(":>") t':typeexpr}? -> (Some t, t')
  | STR(":>") t':typeexpr -> (None, Some t')
  end

let expression_list =
  glr
    e:(expression (next_exp Seq)) l:{ STR(";") e:(expression (next_exp Seq)) }* STR(";")? -> (e::l)
  | EMPTY -> []
  end

let record_item = 
  glr
    f:field STR("=") e:(expression (next_exp Seq)) -> ({ txt = f; loc = _loc_f},e) 
  | f:lowercase_ident -> (let id = { txt = Longident.Lident f; loc = _loc_f} in id, loc_expr _loc_f (Pexp_ident(id)))
  end

let record_list =
  glr
    it:record_item l:{ STR(";") it:record_item }* STR(";")? -> (it::l)
  | EMPTY -> []
  end

let expression_desc = memoize1 (fun lvl ->
  glr
    id:value_path -> (Atom, Pexp_ident { txt = id; loc = _loc_id })
  | c:constant -> (Atom, Pexp_constant c)
  | RE("let\\b") r:RE("rec\\b")? l:value_binding RE("in\\b") e:(expression (let_prio lvl)) when (lvl < App)
    -> (Let, Pexp_let ((if r = None then Nonrecursive else Recursive), l, e))
  | RE("function\\b") l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, Pexp_function("", None, l))
  | RE("fun\\b") l:{lbl:parameter}* STR"->" e:(expression (let_prio lvl)) when (lvl < App) -> 
     (Let, (List.fold_right (fun (lbl,opt,pat) acc -> loc_expr _loc (Pexp_function(lbl, opt, [pat, acc]))) l e).pexp_desc)
  | RE("match\\b") e:(expression Top) RE("with\\b") l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, Pexp_match(e, l))
  | RE("try\\b") e:(expression Top) RE("with\\b") l:(match_cases (let_prio lvl)) when (lvl < App) -> (Let, Pexp_try(e, l))
  | RE("if\\b") c:(expression Top) RE("then\\b") e:(expression If) e':{RE("else\\b") e:(expression If)}? when (lvl <= If) ->
     (If, Pexp_ifthenelse(c,e,e'))
  | STR("(") e:(expression Top) STR(")") -> (Atom, e.pexp_desc)
  | STR("(") STR(")") -> (Atom, Pexp_tuple([]))
  | RE("begin\\b") e:(expression Top) RE("end\\b") -> (Atom, e.pexp_desc)
    (* FIXME: à quoi sert ce booleen, il esr toujours faux dans le parser d'OCaml *)
  | c:constructor e:{e:(expression App)}? when (lvl <= App) -> (App, Pexp_construct({ txt = c; loc = _loc_c},e,false)) 
  | STR("`") l:RE(ident_re) e:{e:(expression App)}? when (lvl <= App) -> (App, Pexp_variant(l,e)) 
  | STR("[|") l:expression_list STR("|]") -> (Atom, Pexp_array l)
  | STR("[") l:expression_list STR("]") ->
     (Atom, (List.fold_right (fun x acc ->
       loc_expr _loc (Pexp_construct({ txt = Longident.Lident "::"; loc = _loc}, Some (loc_expr _loc (Pexp_tuple [x;acc])), false)))
		    l (loc_expr _loc (Pexp_construct({ txt = Longident.Lident "[]"; loc = _loc}, None, false)))).pexp_desc)
  | STR("{") e:{e:(expression Top) RE("with\\b")}? l:record_list STR("}") ->
     (Atom, Pexp_record(l,e))
  | p:prefix_symbol ->> let lvl' = prefix_prio p in e:(expression lvl') when lvl <= lvl' -> 
     let p = match p with "-" -> "~-" | "-." -> "~-." | _ -> p in
     (lvl', Pexp_apply(loc_expr _loc_p (Pexp_ident { txt = Longident.Lident p; loc = _loc_p}), ["", e]))
  | RE("while\\b")  e:(expression Top) RE("do\\b") e':(expression Top) RE("done\\b") ->
      (Atom, Pexp_while(e, e'))
  | RE("for\\b") id:lowercase_ident STR("=")  
      e:(expression Top) d:RE("\\(down\\)?to\\b") e':(expression Top) RE("do\\b") e'':(expression Top) RE("done\\b") ->
        (let dir = if d = "to" then Upto else Downto in
         (Atom, Pexp_for({ txt = id ; loc = _loc_id}, e, e', dir, e'')))
  end)

let apply_lbl _loc (lbl, e) =
  let e = match e with
      None -> loc_expr _loc (Pexp_ident { txt = Longident.Lident lbl; loc = _loc })
    | Some e -> e
  in (lbl, e)

let rec mk_seq _loc = function
    [] -> assert false
  | [e] -> e
  | x::l -> loc_expr _loc (Pexp_sequence(x,mk_seq _loc l))

let expression_suit_aux = memoize2 (fun lvl' lvl ->
  glr
    l:{a:argument}+ when (lvl' > App && lvl <= App) -> 
      (App, fun f -> loc_expr _loc (Pexp_apply(f,l)))
  | l:{STR(",") e:(expression (next_exp Tupl)) -> e}+ when (lvl' > Tupl && lvl <= Tupl) -> 
      (Tupl, fun f -> loc_expr _loc (Pexp_tuple(f::l)))
  | l:{STR(";") e:(expression (next_exp Seq)) -> e}+ STR(";")? when (lvl' > Seq && lvl <= Seq) -> 
      (Seq, fun f -> mk_seq _loc (f::l))
  | STR(";") when (lvl' > Seq && lvl <= Seq) -> (Seq, fun f -> f) 
  | STR(".") STR("(") f:(expression Top) STR(")") STR("<-") e:(expression (next_exp Aff)) when (lvl' > Aff && lvl <= Aff) -> 
      (Aff, fun e' -> loc_expr _loc (Pexp_apply(array_function _loc "Array" "set",[("",e');("",f);("",e)]))) 
  | STR(".") STR("(") f:(expression Top) STR(")") when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> loc_expr _loc (Pexp_apply(array_function _loc "Array" "get",[("",e');("",f)])))
  | STR(".") STR("[") f:(expression Top) STR("]") STR("<-") e:(expression (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' -> loc_expr _loc (Pexp_apply(array_function _loc "String" "set",[("",e');("",f);("",e)]))) 
  | STR(".") STR("[") f:(expression Top) STR("]")  when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> loc_expr _loc (Pexp_apply(array_function _loc "String" "get",[("",e');("",f)])))
  | STR(".") STR("{") f:(expression Top) STR("}") STR("<-") e:(expression (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' -> bigarray_set _loc e' f e)
  | STR(".") STR("{") f:(expression Top) STR("}") when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' -> bigarray_get _loc e' f)
  | STR(".") f:field STR("<-") e:(expression (next_exp Aff)) when (lvl' >= Aff && lvl <= Aff) -> 
      (Aff, fun e' ->
	    let f = { txt = f; loc = _loc_f } in loc_expr _loc (Pexp_setfield(e',f,e)))
  | STR(".") f:field when (lvl' >= Dot && lvl <= Dot) -> 
      (Dot, fun e' ->
	    let f = { txt = f; loc = _loc_f } in loc_expr _loc (Pexp_field(e',f)))
  | t:type_coercion when (lvl' >= Coerce && lvl <= Coerce) ->
      (Coerce, fun e' -> loc_expr _loc (Pexp_constraint(e', fst t, snd t)))
  | op:infix_op ->> let p = infix_prio op in let a = assoc p in 
                    e:(expression (if a = Right then p else next_exp p))
                      when lvl <= p && (lvl' > p || (a = Left && lvl' = p)) ->
      (p, fun e' -> loc_expr (merge _loc_e _loc) (
	if op = "::" then
	  Pexp_construct({ txt = Longident.Lident "::"; loc = _loc_op}, Some (loc_expr _loc_op (Pexp_tuple [e';e])), false)
	else 
	  Pexp_apply(loc_expr _loc_op (Pexp_ident { txt = Longident.Lident op; loc = _loc_op }),
                     [("", e') ; ("", e)])))
  end)

let expression_suit =
  let f expression_suit =
    memoize2
      (fun lvl' lvl ->
         glr
         | (p1,f1):(expression_suit_aux lvl' lvl) ->> (p2,f2):(expression_suit p1 lvl) -> p2, fun f -> f2 (f1 f)
         | EMPTY -> (lvl', fun f -> f) end)
  in
  let rec res x y = f res x y in
  res

let _ = set_expression (fun lvl ->
    glr
      (lvl',e' as e):(expression_desc lvl) ->> f:(expression_suit lvl' lvl) -> snd f (loc_expr (merge _loc_e _loc) e')
    end) expression_lvls

let override_flag =
  glr
    o:STR("!")? -> (if o <> None then Override else Fresh)
  end

let structure_item_desc =
  glr
  | RE("open\\b") o:override_flag m:module_path -> Pstr_open(o, { txt = m; loc = _loc_m} )
  | RE(let_re) r:RE("rec\\b")? l:value_binding -> Pstr_value ((if r = None then Nonrecursive else Recursive), l)
  | td:type_definition -> Pstr_type td
  end

let structure_item =
  glr
    s:structure_item_desc -> { pstr_desc = s; pstr_loc = _loc; }
  end

let structure =
  glr
    l : structure_item* EOF -> l
  end

let signature_item_desc = fail () (* not yet written *)

let signature_item =
  glr
    s:signature_item_desc -> { psig_desc = s; psig_loc = _loc; }
  end

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
  if entry = Impl then 
    `Struct (parse_string structure blank s)
  else
    `Sig (parse_string signature blank s)

let _ = 
  if !ascii then
    begin
      begin
	match ast with 
	| `Struct ast -> Pprintast.structure Format.std_formatter ast;
	| `Sig ast -> Pprintast.signature Format.std_formatter ast;
      end;
      Format.print_newline ()
    end
  else
    begin
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

