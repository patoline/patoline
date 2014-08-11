open Glr
open Charset
open Asttypes
open Parsetree
open Longident
open Blank
open Memoize

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
let tag_name        = 
  glr STR("`") c:ident -> c end

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
       | Some "-" -> (false, true)
       | _        -> assert false)
  end

let override_flag =
  glr
    o:STR("!")? -> (if o <> None then Override else Fresh)
  end
