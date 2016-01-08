(*************************************************************************
 * Character literals and special character handling.                    *
 *************************************************************************)

let decnum = Charset.range '0' '9'
let low_af = Charset.range 'a' 'f'
let upp_af = Charset.range 'A' 'F'
let hexnum = Charset.union decnum (Charset.union low_af upp_af)

let reserved =
  let l = ['$'; '^'; '\\'; '.'; '*'; '+'; '?'; '['; ']'] in
  List.fold_left Charset.add Charset.empty_charset l

let charset_reserved =
  let l = ['\\'; '-'; ']'] in
  List.fold_left Charset.add Charset.empty_charset l

let dec = Decap.in_charset decnum
let hex = Decap.in_charset hexnum

let string_of_chars cs =
  String.concat "" (List.map (String.make 1) cs)

let char_of_int i =
  try char_of_int i with _ -> Decap.give_up "Invalid charcter number..."

let char_of_hex n1 n2 =
  let s = "0x" ^ (string_of_chars [n1; n2]) in
  char_of_int (Scanf.sscanf s "%i" (fun i -> i))

let char_of_dec n1 n2 n3 =
  let s = string_of_chars [n1; n2; n3] in
  char_of_int (Scanf.sscanf s "%i" (fun i -> i))

let parser escaped =
  | 'x' n1:hex n2:hex    -> char_of_hex n1 n2
  | n1:dec n2:dec n3:dec -> char_of_dec n1 n2 n3
  | 'n'                  -> '\n'
  | 'r'                  -> '\r'
  | 't'                  -> '\t'
  | 's'                  -> ' '
  | c:ANY                -> if Charset.mem reserved c then c
                            else Decap.give_up "Not reserved..."

let parser char_lit =
  | '\\' c:escaped -> c
  | c:ANY          -> if Charset.mem reserved c then
                        Decap.give_up "Reserved..."
                      else c

let parser charset_escaped =
  | 'x' n1:hex n2:hex    -> char_of_hex n1 n2
  | n1:dec n2:dec n3:dec -> char_of_dec n1 n2 n3
  | 'n'                  -> '\n'
  | 'r'                  -> '\r'
  | 't'                  -> '\t'
  | 's'                  -> ' '
  | '\\'                 -> '\\'
 
let parser charset_char =
  | '\\' c:charset_escaped -> c
  | c:ANY                  -> if Charset.mem charset_reserved c then
                                Decap.give_up "Reserved..."
                              else c


(*************************************************************************
 * Abstract syntax tree, printer and parser for regular expressions.     *
 *************************************************************************)

type regexp =
  | Chr of char            (* Single character. *)
  | Set of Charset.charset (* Any character in a charset. *)
  | Seq of regexp list     (* Sequence of two regular expressions. *)
  | Alt of regexp list     (* Alternative between several regexps. *)
  | Opt of regexp          (* Optional regexp. *)
  | Str of regexp          (* Zero or more times the regexp. *)
  | Pls of regexp          (* One or more times the regexp. *)
  | Grp of regexp          (* Group containing a regular expression. *)
  | Ref of int             (* Reference to a group. *)
  | BOL                    (* Beginning of line. *)
  | EOL                    (* End of line. *)
  | Bnd                    (* Word boundary. *)


let dot_set = Charset.del (Charset.del Charset.full_charset '\255') '\n'


let rec print_regexp ch r =
  let prnt x = Printf.fprintf ch x in
  match r with
  | Chr(c)  when Charset.mem reserved c -> prnt "\\%c" c
  | Chr(c)  -> prnt "%s" (Char.escaped c)
  | Set(cs) when cs = dot_set -> prnt "."
  | Set(cs) -> Printf.fprintf ch "[%a]" Charset.print_charset cs
  | Seq(rs) -> List.iter (print_regexp ch) rs
  | Alt(rs) -> begin
                 match rs with
                 | []    -> ()
                 | [r]   -> print_regexp ch r
                 | r::rs -> print_regexp ch r;
                            List.iter (prnt "\\|%a" print_regexp) rs
               end
  | Opt(r)  -> Printf.fprintf ch "%a?" print_regexp r
  | Str(r)  -> Printf.fprintf ch "%a*" print_regexp r
  | Pls(r)  -> Printf.fprintf ch "%a+" print_regexp r
  | Grp(r)  -> prnt "\\(%a\\)" print_regexp r
  | Ref(i)  -> prnt "\\%i" i
  | BOL     -> prnt "^"
  | EOL     -> prnt "$"
  | Bnd     -> prnt "\\b"


let parser range_element =
  | cmin:charset_char '-' cmax:charset_char -> Charset.range cmin cmax
  | c:charset_char                          -> Charset.singleton c

let parser range =
  | fst:{c:']' -> ']' | c:'-' -> '-'}? cs:range_element* lst:{'-' -> '-'}? ->
      let fst =
        match fst with
        | None   -> Charset.empty_charset
        | Some c -> Charset.singleton c
      in
      let lst =
        match lst with
        | None   -> Charset.empty_charset
        | Some c -> Charset.singleton c
      in
      List.fold_left Charset.union fst (lst :: cs)

let grp_ref =
  let f c = int_of_string (String.make 1 c) in
  Decap.apply f (Decap.in_charset (Charset.range '1' '9'))

let parser re =
  | c:char_lit              -> Chr(c)
  | '.'                     -> Set(dot_set)
  | r:re '*'                -> Str(r)
  | r:re '+'                -> Pls(r)
  | r:re '?'                -> Opt(r)
  | '[' c:'^'? cs:range ']' -> let cs =
                                 if c = None then cs
                                 else Charset.complement cs
                               in Set(cs)
  | r:re rs:{"\\|" r:re}+   -> Alt(r::rs)
  | r:re rs:re+             -> Seq(r::rs)
  | "\\(" r:re "\\)"        -> Grp(r)
  | '\\' i:grp_ref          -> Ref(i-1)
  | '^'                     -> BOL
  | '$'                     -> EOL
  | "\\b"                   -> Bnd

(*************************************************************************
 * Regular expression combinatorsCompilation to combinators.                                           *
 *************************************************************************)

let raw_regexp : regexp -> string Decap.grammar = fun r ->
  let rec compile = function
    | Chr(c)  -> Decap.char c (String.make 1 c)
    | Set(cs) -> Decap.apply (String.make 1) (Decap.in_charset cs)
    | Seq(rs) ->
        begin
          match rs with
          | []    -> Decap.empty ""
          | r::rs -> Decap.sequence (compile r) (compile (Seq(rs))) (^)
        end
    | Alt(rs) -> Decap.alternatives' (List.map compile rs)
    | Opt(r)  -> Decap.option' "" (compile r)
    | Str(r)  -> Decap.fixpoint' "" (Decap.apply (fun a b -> b^a) (compile r))
    | Pls(r)  -> compile (Seq([r; Str(r)]))
    | Grp(r)  -> compile r (* FIXME *)
    | Ref(i)  -> assert false (* TODO *)
    | BOL     -> assert false (* TODO *)
    | EOL     -> assert false (* TODO *)
    | Bnd     -> assert false (* TODO *)
  in
  Decap.change_layout (compile r) Decap.no_blank

let regexp : string -> string Decap.grammar = fun s ->
  raw_regexp (Decap.parse_string re Decap.no_blank s)
 
(*************************************************************************
 * Tests.                                                                *
 *************************************************************************)

let _ =
  let re = Sys.argv.(2) in
  let usage () =
    Printf.eprintf "Usage: %s [str|decap] [regexp]\n" Sys.argv.(0);
    exit 1
  in
  let parser_re =
    match Sys.argv.(1) with
    | "decap" -> regexp re
    | "str"   -> Decap.regexp re (fun f -> f 0)
    | _       -> usage ()
  in
  let parse_r = Decap.parse_string parser_re Decap.no_blank in
  let parse = Decap.handle_exception parse_r in
  while true do
    try
      let input = read_line () in
      Printf.fprintf stdout "Parsed: %S\n" (parse input)
    with End_of_file -> exit 0
  done
