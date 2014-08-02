open Glr
open Charset
open Asttypes
open Parsetree

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
let lident_re = "\\b\\([a-z][a-zA-Z0-9_']*\\)\\|\\([_][a-zA-Z0-9_']+\\)\\b"
let cident_re = "\\b[A-Z][a-zA-Z0-9_']*\\b"
let ident_re = "\\b[A-Za-z_][a-zA-Z0-9_']*\\b"

let reserved = ["in"; "if"; "then"; "else"; "let"; "begin"; "end"; "and"; "rec"; "match"; "with"; "try"; "fun"; "function"; "lsl";
	        "lsr";"asr";"mod";"land";"lor";"lxor";"or";"true";"false"]
let kreserved = [ "->"; ":" ]
let lident = glr id:RE(lident_re) -> if List.mem id reserved then raise Give_up; id end
let cident = glr id:RE(cident_re) -> id end
let ident = glr id:RE(ident_re) -> if List.mem id reserved then raise Give_up; id end

let infix_re = "\\([=<>@|&+*/$%:^-][!$%&*+./:<=>?@|~^-]*\\)\\|\\(lsl\\)\\|\\(lsr\\)\\|\\(asr\\)\\|\\(mod\\)\\|\\(land\\)\\|\\(lor\\)\\|\\(lxor\\)\\|\\(or\\)"
let prefix_re = "\\([!][!$%&*+./:<=>?@^|~-]*\\)\\|\\([~?][!$%&*+./:<=>?@^|~-]+\\)"

type expression_lvl = Top | Let | Seq | If | Aff | Tupl | Disj | Conj | Eq | Append | Cons | Sum | Prod | Pow | Opp | App | Dash | Dot | Prefix | Atom

let expression_lvls = [ Top; Let; Seq; If; Aff; Tupl; Disj; Conj; Eq; Append; Cons; Sum; Prod; Pow; Opp; App; Dash; Dot; Prefix; Atom]

let expression_lvl_to_string () = function
    Top -> "Top"
  | Let -> "Let"
  | Seq -> "Seq"
  | If -> "If" 
  | Aff -> "Aff"
  | Tupl -> "Tuple"
  | Disj -> "Disj"
  | Conj -> "Conj"
  | Eq -> "Eq"
  | Append -> "Append"
  | Cons -> "Cons"
  | Sum -> "Sum"
  | Prod -> "Prod"
  | Pow -> "Pow"
  | Opp -> "Opp"
  | App -> "App"
  | Dash -> "Dash"
  | Dot -> "Dot"
  | Prefix -> "Prefix"
  | Atom -> "Atom"

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
  | App -> Dash
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

(* Integer literals *)
let int_dec_re = "[-]?[0-9][0-9_]*"
let int_hex_re = "[-]?[0][xX][0-9a-fA-F][0-9a-fA-F_]*"
let int_oct_re = "[-]?[0][oO][0-7][0-7_]*"
let int_bin_re = "[-]?[0][bB][01][01_]*"

let strip_underscores s =
  let len = String.length s in
  let s' = String.create len in
  let p = ref 0 in
  for i = 0 to len - 1 do
    if s.[i] <> '_' then begin
      s'.[!p] <- s.[i];
      incr p
    end
  done;
  String.sub s' 0 !p

let int_lit =
  glr
    i:RE(int_dec_re ^ "\\b") -> int_of_string i
  | i:RE(int_hex_re ^ "\\b") -> int_of_string i
  | i:RE(int_oct_re ^ "\\b") -> int_of_string i
  | i:RE(int_bin_re ^ "\\b") -> int_of_string i
  end

let remove_last s =
  String.sub s 0 (String.length s - 1)

let int32_lit =
  glr
    i:RE(int_dec_re ^ "l\\b") -> Int32.of_string (remove_last i)
  | i:RE(int_hex_re ^ "l\\b") -> Int32.of_string (remove_last i)
  | i:RE(int_oct_re ^ "l\\b") -> Int32.of_string (remove_last i)
  | i:RE(int_bin_re ^ "l\\b") -> Int32.of_string (remove_last i)
  end

let int64_lit =
  glr
    i:RE(int_dec_re ^ "L\\b") -> Int64.of_string (remove_last i)
  | i:RE(int_hex_re ^ "L\\b") -> Int64.of_string (remove_last i)
  | i:RE(int_oct_re ^ "L\\b") -> Int64.of_string (remove_last i)
  | i:RE(int_bin_re ^ "L\\b") -> Int64.of_string (remove_last i)
  end

let nat_int_lit =
  glr
    i:RE(int_dec_re ^ "n\\b") -> Nativeint.of_string (remove_last i)
  | i:RE(int_hex_re ^ "n\\b") -> Nativeint.of_string (remove_last i)
  | i:RE(int_oct_re ^ "n\\b") -> Nativeint.of_string (remove_last i)
  | i:RE(int_bin_re ^ "n\\b") -> Nativeint.of_string (remove_last i)
  end

(* Floating-point literals *)
let float_lit_dec    = "[-]?[0-9][0-9_]*[.][0-9_]*\\([eE][+-][0-9][0-9_]*\\)?"
let float_lit_no_dec = "[-]?[0-9][0-9_]*[eE][+-][0-9][0-9_]*"

let float_lit =
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

let char_lit =
  change_layout (
    glr STR("\'") c:one_char STR("\'") -> c end
  ) no_blank

(* String literals *)
let interspace = "[\\][\n][ \t]*"

let string_lit =
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

let constant =
  glr
    i:int_lit -> Const_int i
  | c:char_lit -> Const_char c
  | s:string_lit -> Const_string s
  | f:float_lit -> Const_float f
  | i:int32_lit -> Const_int32 i
  | i:int64_lit -> Const_int64 i
  | i:nat_int_lit -> Const_nativeint i
  end

let (expression, set_expression) = grammar_family ~param_to_string:expression_lvl_to_string ()
let pattern = declare_grammar ()
let loc_expr _loc e = { pexp_desc = e; pexp_loc = _loc; }
let loc_pat _loc pat = { ppat_desc = pat; ppat_loc = _loc; }
let loc_typ _loc typ = { ptyp_desc = typ; ptyp_loc = _loc; }

let typexp = declare_grammar ()

let typexp_desc = fail () 

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
    m:{ m:module_path STR"." }? id:{id:cident -> id | RE"\\bfalse\\b" -> "false" | RE"\\btrue\\b" -> "true" } ->
      match m with None -> Longident.Lident id
		 | Some m -> Longident.Ldot(m, id)
  end 

let _ = set_grammar typexp (
  glr 
    t:typexp_desc -> loc_typ _loc t
  end)

let pattern_desc =
  glr
    id:lident -> Ppat_var { txt = id; loc = _loc_id }
  | STR("_")  -> Ppat_any			  
  | c:constant -> Ppat_constant c
  end

let _ = set_grammar pattern (
  glr
    pat:pattern_desc -> loc_pat _loc pat
  end)

let argument =
  glr
    STR("~") id:lident -> (id, loc_expr _loc (Pexp_ident { txt = Longident.Lident id; loc = _loc }))
  | STR("?") id:lident -> (id, loc_expr _loc (Pexp_ident { txt = Longident.Lident ("?"^id); loc = _loc }))
  | STR("~") id:lident STR(":") e:(expression (next_exp App)) -> (id, e)
  | STR("?") id:lident STR(":") e:(expression (next_exp App)) -> (("?"^id), e)
  | e:(expression (next_exp App)) -> ("", e)
  end

let parameter =
  glr
    pat:pattern -> ("", None, pat)
  | STR("~") id:lident -> (id, None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  | STR("~") STR("(") id:lident t:{ STR":" t:typexp }? STR")" -> (
      let pat =  loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }) in
      let pat = match t with
      | None -> pat
      | Some t -> loc_pat _loc (Ppat_constraint (pat, t))
      in
      (id, None, pat))
  | STR("~") id:lident STR":" pat:pattern -> (id, None, pat)
  | STR("?") id:lident -> (("?"^id), None, loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }))
  | STR("?") id:lident STR":" pat:pattern -> (("?"^id), None, pat)
  | STR("?") STR"(" id:lident t:{ STR":" t:typexp -> t }? e:{STR"=" e:(expression Top) -> e}? STR")" -> (
      let pat = loc_pat _loc_id (Ppat_var { txt = id; loc = _loc_id }) in
      let pat = match t with
	| None -> pat
	| Some t -> loc_pat (merge _loc_id _loc_t) (Ppat_constraint(pat,t))
      in (("?"^id), e, pat))
  | STR("?") id:lident STR":" STR"(" pat:pattern t:{ STR":" t:typexp -> t }? e:{ STR"=" e:(expression Top) -> e}? STR")" -> (
      let pat = match t with
	| None -> pat
	| Some t -> loc_pat (merge _loc_pat _loc_t) (Ppat_constraint(pat,t))
      in (("?"^id), e, pat))
  end

let right_member =
  glr
    l:{lb:parameter}* STR("=") e:(expression Top) -> 
      List.fold_right (fun (lbl,opt,pat) e ->
		       loc_expr _loc (Pexp_function (lbl, opt, [pat, e]))) l e
  end

let value_binding =
  glr
    pat:pattern e:right_member l:{RE("\\band\\b") pat:pattern e:right_member -> (pat, e)}* -> ((pat, e)::l)
  end

let match_cases =
  glr
     l:{STR"|"? pat:pattern STR"->" e:(expression Let) 
         l:{STR"|" pat:pattern STR"->" e:(expression Let) -> (pat,e)}* 
         -> ((pat,e)::l)}?[[]] -> l
  end

let expression_desc lvl =
  glr
    id:lident -> (Atom, Pexp_ident { txt = Longident.Lident id; loc = _loc_id })
  | c:constant -> (Atom, Pexp_constant c)
  | RE("\\blet\\b") r:RE("\\brec\\b")? l:value_binding RE("\\bin\\b") e:(expression Let) when (lvl <= Let)
    -> (Let, Pexp_let ((if r = None then Nonrecursive else Recursive), l, e))
  | RE("\\bfunction\\b") l:match_cases when (lvl <= Let) -> (Let, Pexp_function("", None, l))
  | RE("\\bfun\\b") l:{lbl:parameter}* STR"->" e:(expression Let) -> 
     (Let, (List.fold_right (fun (lbl,opt,pat) acc -> loc_expr _loc (Pexp_function(lbl, opt, [pat, acc]))) l e).pexp_desc)
  | RE("\\bmatch\\b") e:(expression Top) RE("\\bwith\\b") l:match_cases when (lvl <= Let) -> (Let, Pexp_match(e, l))
  | RE("\\btry\\b") e:(expression Top) RE("\\bwith\\b") l:match_cases when (lvl <= Let) -> (Let, Pexp_try(e, l))
  | RE("\\bif\\b") c:(expression Top) RE("\\bthen\\b") e:(expression If) e':{RE("\\belse\\b") e:(expression If)}? when (lvl <= If) ->
     (If, Pexp_ifthenelse(c,e,e'))
  | STR("(") e:(expression Top) STR(")") -> (Atom, e.pexp_desc)
  | RE("\\bbegin\\b") e:(expression Top) RE("\\bend\\b") -> (Atom, e.pexp_desc)
    (* FIXME: à quoi sert ce booleen, il esr toujours faux dans le parser d'OCaml *)
  | c:constructor e:{e:(expression App)}? -> (App, Pexp_construct({ txt = c; loc = _loc_c},e,false)) 
  end

let apply_lbl _loc (lbl, e) =
  let e = match e with
      None -> loc_expr _loc (Pexp_ident { txt = Longident.Lident lbl; loc = _loc })
    | Some e -> e
  in (lbl, e)

let expression_suit_aux lvl' lvl f =
  glr
    l:{a:argument}+ when (lvl' > App && lvl <= App) -> 
      (App, loc_expr _loc (Pexp_apply(f,l)))
  | l:{STR(",") e:(expression (next_exp Tupl)) -> e}+ when (lvl' > Tupl && lvl <= Tupl) -> 
      (Tupl, loc_expr _loc (Pexp_tuple(f::l)))
  | a:(dependent_sequence (glr k:RE(infix_re) -> (if List.mem k kreserved then raise Give_up; _loc_k, k) end)
		       (fun (_loc_k, k) ->
			let p = infix_prio k in
			let a = assoc p in
			if lvl > p || lvl' < p || (a <> Left && lvl' = p) then raise Give_up;
			let p' = if a = Right then p else next_exp p in
			apply (fun e -> p, Pexp_apply(loc_expr _loc_k (Pexp_ident { txt = Longident.Lident k; loc = _loc_k }),
                                                   [("", f) ; ("", e)])) (expression p')))
       -> (fst a, loc_expr _loc (snd a))
  end

let rec expression_suit lvl' lvl f =
  option f (
   dependent_sequence (expression_suit_aux lvl' lvl f) (fun (lvl', e) -> expression_suit lvl' lvl e))

let _ = set_expression (fun lvl ->
    dependent_sequence (locate (expression_desc lvl)) (fun (_loc, (lvl', e)) -> expression_suit lvl' lvl (loc_expr _loc e)))
  expression_lvls 

let structure_item_desc =
  glr
    RE("\\blet\\b") r:RE("\\brec\\b")? l:value_binding -> Pstr_value ((if r = None then Nonrecursive else Recursive), l)
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

let file = ref None
let ascii = ref false
type entry = FromExt | Impl | Intf
let entry = ref FromExt

let spec = [
  "--ascii", Arg.Set ascii , "output ascii ast instead of serialized ast" ;
  "--impl", Arg.Unit (fun () -> entry := Impl), "treat file as an implementation" ;
  "--intf", Arg.Unit (fun () -> entry := Intf), "treat file as an interface" ;
]

let anon_fun s = file := Some s

let _ = Arg.parse spec anon_fun (Printf.sprintf "usage: %s [options] file" Sys.argv.(0)) 

let entry =
  match !entry, !file with
    FromExt, Some s -> if Filename.check_suffix s ".mli" then Intf else Impl
  | FromExt, None -> Intf
  | i, _ -> i

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

