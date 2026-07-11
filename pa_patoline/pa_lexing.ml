open Earley_core
exception Unclosed_comment of bool * Input.buffer * int 
let unclosed_comment : type a. (Input.buffer * int) -> a =
  fun (buf, pos) -> raise (Unclosed_comment (false, buf, pos))
let unclosed_comment_string : type a. (Input.buffer * int) -> a =
  fun (buf, pos) -> raise (Unclosed_comment (true, buf, pos))
let ocamldoc_comments = ref []
let ocamldoc_stack = ref []
let push_comments () =
  ocamldoc_stack := ((!ocamldoc_comments) :: (!ocamldoc_stack));
  ocamldoc_comments := []
let pop_comments () =
  match !ocamldoc_stack with
  | [] -> assert false
  | cs::ls ->
      (ocamldoc_comments := ((!ocamldoc_comments) @ cs); ocamldoc_stack := ls)
let ocaml_blank buf pos =
  let ocamldoc = ref false in
  let ocamldoc_buf = Buffer.create 1024 in
  let new_line = ref false in
  let previous_newline = ref (Input.line_num buf) in
  let rec fn state stack prev curr =
    let (buf, pos) = curr in
    let (c, buf', pos') = Input.read buf pos in
    if !ocamldoc then Buffer.add_char ocamldoc_buf c;
    (let next = (buf', pos') in
     let count_newline () =
       if !new_line then previous_newline := (Input.line_num buf');
       new_line := true in
     match (state, stack, c) with
     | (`Ini, [], ' ')|(`Ini, [], '\t')|(`Ini, [], '\r')|(`Ini, [], '\n') ->
         (count_newline (); fn `Ini stack curr next)
     | (`Ini, _, '(') -> fn (`Opn curr) stack curr next
     | (`Ini, [], _) -> curr
     | (`Opn p, _, '*') ->
         if stack = []
         then
           let (c, buf', pos') = Input.read buf' pos' in
           let (c', _, _) = Input.read buf' pos' in
           (if (c = '*') && (c' <> '*')
            then (ocamldoc := true; fn `Cls (p :: stack) curr (buf', pos'))
            else fn `Ini (p :: stack) curr next)
         else fn `Ini (p :: stack) curr next
     | (`Opn _, _::_, '"') -> fn (`Str curr) stack curr next
     | (`Opn _, _::_, '{') -> fn (`SOp ([], curr)) stack curr next
     | (`Opn _, _::_, '(') -> fn (`Opn curr) stack curr next
     | (`Opn _, [], _) -> prev
     | (`Opn _, _, _) -> fn `Ini stack curr next
     | (`Ini, _::_, '"') -> fn (`Str curr) stack curr next
     | (`Str _, _::_, '"') -> fn `Ini stack curr next
     | (`Str p, _::_, '\\') -> fn (`Esc p) stack curr next
     | (`Esc p, _::_, _) -> fn (`Str p) stack curr next
     | (`Str p, _::_, '\255') -> unclosed_comment_string p
     | (`Str _, _::_, _) -> fn state stack curr next
     | (`Str _, [], _) -> assert false
     | (`Esc _, [], _) -> assert false
     | (`Ini, _::_, '{') -> fn (`SOp ([], curr)) stack curr next
     | (`SOp (l, p), _::_, 'a'..'z')|(`SOp (l, p), _::_, '_') ->
         fn (`SOp ((c :: l), p)) stack curr next
     | (`SOp (_, _), p::_, '\255') -> unclosed_comment p
     | (`SOp (l, p), _::_, '|') ->
         fn (`SIn ((List.rev l), p)) stack curr next
     | (`SOp (_, _), _::_, _) -> fn `Ini stack curr next
     | (`SIn (l, p), _::_, '|') -> fn (`SCl (l, (l, p))) stack curr next
     | (`SIn (_, p), _::_, '\255') -> unclosed_comment_string p
     | (`SIn (_, _), _::_, _) -> fn state stack curr next
     | (`SCl ([], b), _::_, '}') -> fn `Ini stack curr next
     | (`SCl ([], b), _::_, '\255') -> unclosed_comment_string (snd b)
     | (`SCl ([], b), _::_, _) -> fn (`SIn b) stack curr next
     | (`SCl (l, b), _::_, c) ->
         if c = (List.hd l)
         then let l = List.tl l in fn (`SCl (l, b)) stack curr next
         else fn (`SIn b) stack curr next
     | (`SOp (_, _), [], _) -> assert false
     | (`SIn (_, _), [], _) -> assert false
     | (`SCl (_, _), [], _) -> assert false
     | (`Ini, _::_, '*') -> fn `Cls stack curr next
     | (`Cls, _::_, '*') -> fn `Cls stack curr next
     | (`Cls, _::_, '"') -> fn (`Str curr) stack curr next
     | (`Cls, _::_, '{') -> fn (`SOp ([], curr)) stack curr next
     | (`Cls, p::s, ')') ->
         (if (!ocamldoc) && (s = [])
          then
            (let comment =
               try
                 Buffer.sub ocamldoc_buf 0 ((Buffer.length ocamldoc_buf) - 2)
               with | Invalid_argument _ -> "" in
             Buffer.clear ocamldoc_buf;
             ocamldoc_comments := ((p, next, comment, (!previous_newline)) ::
               (!ocamldoc_comments));
             ocamldoc := false);
          new_line := false;
          fn `Ini s curr next)
     | (`Cls, _::_, _) -> fn `Ini stack curr next
     | (`Cls, [], _) -> assert false
     | (`Ini, p::_, '\255') -> unclosed_comment p
     | (`Ini, _::_, _) -> fn `Ini stack curr next) in
  fn `Ini [] (buf, pos) (buf, pos)
let ident_char c =
  match c with | 'a'..'z'|'A'..'Z'|'0'..'9'|'_'|'\'' -> true | _ -> false
let key_word s =
  if (String.length s) <= 0
  then invalid_arg "Pa_lexing.key_word (empty keyword)";
  Earley.keyword s ident_char ()
let mutable_kw = key_word "mutable"
let private_kw = key_word "private"
let virtual_kw = key_word "virtual"
let rec_kw = key_word "rec"
let to_kw = key_word "to"
let downto_kw = key_word "downto"
let joker_kw = key_word "_"
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
let parser_kw = key_word "parser"
let cached_kw = key_word "cached"
let no_keyword s =
  let len = String.length s in
  let rec fn i buf pos =
    let (c, buf, pos) = Input.read buf pos in
    if i >= len
    then ((), (ident_char c))
    else if c <> (s.[i]) then ((), true) else fn (i + 1) buf pos in
  Earley.test ~name:("no_" ^ s) Charset.full (fn 0)
let no_else = no_keyword "else"
let no_false = no_keyword "false"
let no_parser = no_keyword "parser"
let no_with = no_keyword "with"
let no_as = no_keyword "as"
let no_dot =
  Earley.test ~name:"no_dot" Charset.full
    (fun buf ->
       fun pos ->
         let (c, buf, pos) = Input.read buf pos in
         if c <> '.' then ((), true) else ((), false))
let no_semi =
  Earley.test ~name:"no_semi" Charset.full
    (fun buf ->
       fun pos ->
         let (c, buf, pos) = Input.read buf pos in
         if c <> ';'
         then ((), true)
         else
           (let (c, buf, pos) = Input.read buf pos in
            if c = ';' then ((), true) else ((), false)))
let no_colon =
  Earley.test ~name:"no_colon" Charset.full
    (fun buf ->
       fun pos ->
         let (c, buf, pos) = Input.read buf pos in
         if c <> ':'
         then ((), true)
         else
           (let (c, buf, pos) = Input.read buf pos in
            if c = ':' then ((), true) else ((), false)))
let make_reserved l =
  let cmp s1 s2 = String.compare s2 s1 in
  let re_from_list l =
    let l = List.map (fun s -> "\\(" ^ ((Str.quote s) ^ "\\)")) l in
    Str.regexp (String.concat "\\|" l) in
  let reserved = ref (List.sort cmp l) in
  let re = ref (re_from_list (!reserved)) in
  let is_reserved s =
    (Str.string_match (!re) s 0) && ((Str.match_end ()) = (String.length s)) in
  let add_reserved s =
    reserved := (List.sort cmp (s :: (!reserved)));
    re := (re_from_list (!reserved)) in
  (is_reserved, add_reserved)
let reserved_ids =
  ["and";
  "as";
  "assert";
  "asr";
  "begin";
  "class";
  "constraint";
  "do";
  "done";
  "downto";
  "else";
  "end";
  "exception";
  "external";
  "false";
  "for";
  "function";
  "functor";
  "fun";
  "if";
  "in";
  "include";
  "inherit";
  "initializer";
  "land";
  "lazy";
  "let";
  "lor";
  "lsl";
  "lsr";
  "lxor";
  "match";
  "method";
  "mod";
  "module";
  "mutable";
  "new";
  "object";
  "of";
  "open";
  "or";
  "private";
  "rec";
  "sig";
  "struct";
  "then";
  "to";
  "true";
  "try";
  "type";
  "val";
  "virtual";
  "when";
  "while";
  "with"]
let reserved_symbs =
  ["#";
  "'";
  "(";
  ")";
  ",";
  "->";
  "->>";
  ".";
  "..";
  ":";
  ":>";
  ";";
  ";;";
  "<-";
  ">]";
  ">}";
  "?";
  "[";
  "[<";
  "[>";
  "[|";
  "]";
  "_";
  "`";
  "{";
  "{<";
  "|";
  "|]";
  "}";
  "~";
  "$"]
let (is_reserved_id, add_reserved_id) = make_reserved reserved_ids
let (is_reserved_symb, add_reserved_symb) = make_reserved reserved_symbs
let not_special =
  let special = "!$%&*+./:<=>?@^|~-" in
  let cs = ref Charset.empty in
  String.iter (fun c -> cs := (Charset.add (!cs) c)) special;
  Earley.blank_not_in_charset ~name:"not_special" (!cs)
let ident = Earley_core.Earley.declare_grammar "ident"
let _ =
  Earley_core.Earley.set_grammar ident
    (Earley_core.Earley.fsequence
       (Earley_str.regexp ~name:"[A-Za-z_][a-zA-Z0-9_']*"
          "[A-Za-z_][a-zA-Z0-9_']*" (fun group -> group 0))
       (Earley_core.Earley.empty
          (fun id -> if is_reserved_id id then Earley.give_up (); id)))
let lident = Earley_core.Earley.declare_grammar "lident"
let _ =
  Earley_core.Earley.set_grammar lident
    (Earley_core.Earley.fsequence
       (Earley_str.regexp
          ~name:"\\\\([a-z][a-zA-Z0-9_']*\\\\)\\\\|\\\\([_][a-zA-Z0-9_']+\\\\)"
          "\\([a-z][a-zA-Z0-9_']*\\)\\|\\([_][a-zA-Z0-9_']+\\)"
          (fun group -> group 0))
       (Earley_core.Earley.empty
          (fun id -> if is_reserved_id id then Earley.give_up (); id)))
let uident = Earley_core.Earley.declare_grammar "uident"
let _ =
  Earley_core.Earley.set_grammar uident
    (Earley_str.regexp ~name:"[A-Z][a-zA-Z0-9_']*" "[A-Z][a-zA-Z0-9_']*"
       (fun group -> group 0))
let union_re l = String.concat "\\|" (List.map (Printf.sprintf "\\(%s\\)") l)
let cs_to_string cs =
  String.concat "" (List.map (fun c -> String.make 1 c) cs)
let single_char c =
  let s = String.make 1 c in
  let f str pos =
    let (c', str', pos') = Input.read str pos in
    if c' = c
    then
      let (c'', _, _) = Input.read str' pos' in
      (if c'' = c then Earley.give_up () else ((), str', pos'))
    else Earley.give_up () in
  Earley.black_box f (Charset.singleton c) false s
let double_char c =
  let s = String.make 2 c in
  let f str pos =
    let (c', str', pos') = Input.read str pos in
    if c' = c
    then
      let (c'', str', pos') = Input.read str' pos' in
      (if c'' <> c then Earley.give_up () else ((), str', pos'))
    else Earley.give_up () in
  Earley.black_box f (Charset.singleton c) false s
let semi_col = single_char ';'
let double_semi_col = double_char ';'
let single_quote = single_char '\''
let double_quote = double_char '\''
let bool_lit = Earley_core.Earley.declare_grammar "bool_lit"
let _ =
  Earley_core.Earley.set_grammar bool_lit
    (Earley_core.Earley.alternatives
       (List.cons
          (Earley_core.Earley.fsequence true_kw
             (Earley_core.Earley.empty (fun _default_0 -> "true")))
          (List.cons
             (Earley_core.Earley.fsequence false_kw
                (Earley_core.Earley.empty (fun _default_0 -> "false"))) [])) : 
    string Earley.grammar)
let num_suffix =
  let suffix_cs = let open Charset in union (range 'g' 'z') (range 'G' 'Z') in
  let no_suffix_cs =
    Earley.blank_test Charset.full
      (fun buf ->
         fun pos ->
           fun _ ->
             fun _ ->
               let (c, _, _) = Input.read buf pos in
               ((),
                 ((c <> '.') &&
                    ((c <> 'e') &&
                       ((c <> 'E') && (not (Charset.mem suffix_cs c))))))) in
  Earley_core.Earley.alternatives
    (List.cons
       (Earley_core.Earley.fsequence no_suffix_cs
          (Earley_core.Earley.empty (fun _default_0 -> None)))
       (List.cons
          (Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.no_blank_test ())
             (Earley_core.Earley.fsequence (Earley.in_charset suffix_cs)
                (Earley_core.Earley.empty (fun s -> Some s)))) []))
let int_litteral : (string * char option) Earley.grammar =
  let int_re =
    union_re
      ["[0][xX][0-9a-fA-F][0-9a-fA-F_]*";
      "[0][oO][0-7][0-7_]*";
      "[0][bB][01][01_]*";
      "[0-9][0-9_]*"] in
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"int" int_re (fun group -> group 0))
    (Earley_core.Earley.fsequence num_suffix
       (Earley_core.Earley.empty (fun _default_0 -> fun i -> (i, _default_0))))
let float_litteral : (string * char option) Earley.grammar =
  let float_re =
    union_re
      ["[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*";
      "[0-9][0-9_]*[.][0-9_]*\\([eE][+-][0-9][0-9_]*\\)?"] in
  Earley_core.Earley.fsequence
    (Earley_str.regexp ~name:"float" float_re (fun group -> group 0))
    (Earley_core.Earley.fsequence num_suffix
       (Earley_core.Earley.empty (fun _default_0 -> fun f -> (f, _default_0))))
let escaped_char : char Earley.grammar =
  let char_dec = "[0-9][0-9][0-9]" in
  let char_hex = "[x][0-9a-fA-F][0-9a-fA-F]" in
  let char_esc = "[\\\\\\\"\\'ntbrs ]" in
  Earley_core.Earley.alternatives
    (List.cons
       (Earley_core.Earley.fsequence
          (Earley_str.regexp ~name:"char_esc" char_esc (fun group -> group 0))
          (Earley_core.Earley.empty
             (fun e ->
                match e.[0] with
                | 'n' -> '\n'
                | 't' -> '\t'
                | 'b' -> '\b'
                | 'r' -> '\r'
                | 's' -> ' '
                | c -> c)))
       (List.cons
          (Earley_core.Earley.fsequence
             (Earley_str.regexp ~name:"char_dec" char_dec
                (fun group -> group 0))
             (Earley_core.Earley.empty
                (fun e -> char_of_int (int_of_string e))))
          (List.cons
             (Earley_core.Earley.fsequence
                (Earley_str.regexp ~name:"char_hex" char_hex
                   (fun group -> group 0))
                (Earley_core.Earley.empty
                   (fun e -> char_of_int (int_of_string ("0" ^ e))))) [])))
let char_litteral : char Earley.grammar =
  let char_reg = "[^\\\\\\']" in
  let single_char =
    Earley_core.Earley.alternatives
      (List.cons
         (Earley_core.Earley.fsequence_ignore
            (Earley_core.Earley.char '\\' '\\')
            (Earley_core.Earley.fsequence escaped_char
               (Earley_core.Earley.empty (fun e -> e))))
         (List.cons
            (Earley_core.Earley.fsequence
               (Earley_str.regexp ~name:"char_reg" char_reg
                  (fun group -> group 0))
               (Earley_core.Earley.empty (fun c -> c.[0]))) [])) in
  Earley.no_blank_layout
    (Earley_core.Earley.fsequence_ignore single_quote
       (Earley_core.Earley.fsequence single_char
          (Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.no_blank_test ())
             (Earley_core.Earley.fsequence_ignore
                (Earley_core.Earley.char '\'' '\'')
                (Earley_core.Earley.empty (fun c -> c))))))
let quoted_string : (string * string option) Earley.grammar =
  let f buf pos =
    let rec fn st str buf pos =
      let (c, buf', pos') = Input.read buf pos in
      match (st, c) with
      | (`Ini, '{') -> fn (`Opn []) str buf' pos'
      | (`Opn l, 'a'..'z')|(`Opn l, '_') -> fn (`Opn (c :: l)) str buf' pos'
      | (`Opn l, '|') -> fn (`Cnt (List.rev l)) str buf' pos'
      | (`Cnt l, '|') -> fn (`Cls (l, [], l)) str buf' pos'
      | (`Cnt l, '\255') -> Earley.give_up ()
      | (`Cnt _, _) -> fn st (c :: str) buf' pos'
      | (`Cls ([], _, l), '}') -> (str, l, buf', pos')
      | (`Cls ([], _, _), '\255') -> Earley.give_up ()
      | (`Cls ([], b, l), _) -> fn (`Cnt l) (b @ str) buf' pos'
      | (`Cls (_::_, _, _), '\255') -> Earley.give_up ()
      | (`Cls (x::y, b, l), _) ->
          if x = c
          then fn (`Cls (y, (x :: b), l)) str buf' pos'
          else fn (`Cnt l) (List.append b str) buf' pos'
      | (_, _) -> Earley.give_up () in
    let (cs, id, buf, pos) = fn `Ini [] buf pos in
    let r = ((cs_to_string cs), (Some (cs_to_string id))) in (r, buf, pos) in
  Earley.black_box f (Charset.singleton '{') false "quoted_string"
let normal_string : string Earley.grammar =
  let single_char =
    Earley_core.Earley.alternatives
      (List.cons
         (Earley_core.Earley.fsequence_ignore
            (Earley_core.Earley.char '\\' '\\')
            (Earley_core.Earley.fsequence_ignore
               (Earley_core.Earley.no_blank_test ())
               (Earley_core.Earley.fsequence escaped_char
                  (Earley_core.Earley.empty (fun e -> e)))))
         (List.cons
            (Earley_core.Earley.fsequence Earley_core.Earley.any
               (Earley_core.Earley.empty
                  (fun c ->
                     if (c = '"') || (c = '\\') then Earley.give_up (); c)))
            [])) in
  Earley_core.Earley.fsequence_ignore (Earley_core.Earley.char '"' '"')
    (Earley_core.Earley.fsequence
       (Earley_core.Earley.apply (fun f -> f [])
          (Earley_core.Earley.fixpoint' (fun l -> l) single_char
             (fun x -> fun f -> fun l -> f (List.cons x l))))
       (Earley_core.Earley.fsequence
          (Earley_core.Earley.apply (fun f -> f [])
             (Earley_core.Earley.fixpoint' (fun l -> l)
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string "\\\n" "\\\n")
                   (Earley_core.Earley.fsequence_ignore
                      (Earley_core.Earley.greedy
                         (Earley_str.regexp "[ \t]*" (fun group -> group 0)))
                      (Earley_core.Earley.fsequence
                         (Earley_core.Earley.apply (fun f -> f [])
                            (Earley_core.Earley.fixpoint' (fun l -> l)
                               single_char
                               (fun x -> fun f -> fun l -> f (List.cons x l))))
                         (Earley_core.Earley.empty
                            (fun _default_0 -> _default_0)))))
                (fun x -> fun f -> fun l -> f (List.cons x l))))
          (Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.char '"' '"')
             (Earley_core.Earley.empty
                (fun css -> fun cs -> cs_to_string (List.flatten (cs :: css)))))))
let string_litteral : (string * string option) Earley.grammar =
  let string_litteral =
    Earley_core.Earley.alternatives
      (List.cons quoted_string
         (List.cons
            (Earley_core.Earley.fsequence normal_string
               (Earley_core.Earley.empty (fun s -> (s, None)))) [])) in
  Earley.no_blank_layout string_litteral
let regexp =
  let regexp_char =
    Earley_core.Earley.alternatives
      (List.cons
         (Earley_core.Earley.fsequence_ignore
            (Earley_core.Earley.char '\\' '\\')
            (Earley_core.Earley.fsequence
               (Earley_str.regexp "[ntbrs\\\\()|]" (fun group -> group 0))
               (Earley_core.Earley.empty
                  (fun e ->
                     match e.[0] with
                     | 'n' -> "\n"
                     | 't' -> "\t"
                     | 'b' -> "\b"
                     | 'r' -> "\r"
                     | 's' -> " "
                     | '\\' -> "\\"
                     | '(' -> "\\("
                     | ')' -> "\\)"
                     | '|' -> "\\|"
                     | _ -> assert false))))
         (List.cons (Earley_str.regexp "[^'\\\\]" (fun group -> group 0))
            (List.cons
               (Earley_core.Earley.fsequence_ignore single_quote
                  (Earley_core.Earley.empty "'")) []))) in
  Earley_core.Earley.fsequence
    (Earley_core.Earley.apply (fun f -> f [])
       (Earley_core.Earley.fixpoint' (fun l -> l) regexp_char
          (fun x -> fun f -> fun l -> f (List.cons x l))))
    (Earley_core.Earley.empty (fun cs -> String.concat "" cs))
let regexp_litteral : string Earley.grammar =
  Earley_core.Earley.fsequence_ignore double_quote
    (Earley_core.Earley.fsequence_ignore
       (Earley_core.Earley.no_blank_test ())
       (Earley_core.Earley.fsequence
          (Earley.no_blank_layout
             (Earley_core.Earley.fsequence regexp
                (Earley_core.Earley.fsequence_ignore
                   (Earley_core.Earley.string "''" "''")
                   (Earley_core.Earley.empty (fun _default_0 -> _default_0)))))
          (Earley_core.Earley.empty (fun _default_0 -> _default_0))))
let new_regexp_litteral : string Earley.grammar =
  Earley.no_blank_layout
    (Earley_core.Earley.fsequence_ignore
       (Earley_core.Earley.string "{#" "{#")
       (Earley_core.Earley.fsequence regexp
          (Earley_core.Earley.fsequence_ignore
             (Earley_core.Earley.string "#}" "#}")
             (Earley_core.Earley.empty (fun _default_0 -> _default_0)))))
