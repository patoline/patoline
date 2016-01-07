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
  | BOL                    (* Beginning of line. *)
  | EOL                    (* End of line. *)
  | Grp of regexp          (* Group containing a regular expression. *)
  | Ref of int             (* Reference to a group. *)
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
  | BOL     -> prnt "^"
  | EOL     -> prnt "$"
  | Grp(r)  -> prnt "\\(%a\\)" print_regexp r
  | Ref(i)  -> prnt "\\%i" i
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
  | '^'                     -> BOL
  | '$'                     -> EOL
  | r:re rs:{"\\|" r:re}+   -> Alt(r::rs)
  | r:re rs:re+             -> Seq(r::rs)
  | "\\(" r:re "\\)"        -> Grp(r)
  | '\\' i:grp_ref          -> Ref(i-1)
  | "\\b"                   -> Bnd

(*************************************************************************
 * Compilation to combinators.                                           *
 *************************************************************************)

module Buffers =
  struct
    type t = unit

    let create : int -> t =
      assert false

    let madd_char : char -> int list -> t -> t =
      assert false

    let madd_string : string -> int list -> t -> t =
      assert false

    let icontents : t -> int -> string =
      assert false

    let contents : t -> string array =
      assert false

    let add_char c b = assert false
    let add_string s b = assert false
  end

exception Invalid_regexp of string
let invalid_regexp msg = raise (Invalid_regexp msg)

let compile : regexp -> string array Decap.grammar =
  let next_grp = ref 0 in
  let avail_grp = ref [] in
  let rec compile wgrps bufs = function
    | Chr(c)  -> let act () = Buffers.madd_char c wgrps bufs in
                 Decap.apply act (Decap.char c ())
    | Set(cs) -> let act c = Buffers.madd_char c wgrps bufs in
                 Decap.apply act (Decap.in_charset cs)
    | Seq(rs) -> assert false
    | Alt(rs) -> assert false
    | Opt(r)  -> assert false
    | Str(r)  -> assert false
    | Pls(r)  -> assert false
    | BOL     -> assert false
    | EOL     -> assert false
    | Grp(r)  -> let gid = !next_grp in
                 incr next_grp;
                 let res = compile (gid::wgrps) bufs r in
                 avail_grp := gid :: !avail_grp;
                 res
    | Ref(i)  -> if not (List.mem i !avail_grp) then
                   invalid_regexp "Undefined group...";
                 let s = Buffers.icontents bufs i in
                 let act () = Buffers.madd_string s wgrps bufs in
                 Decap.apply act (Decap.string s ())
    | Bnd     -> assert false
  in
  let compile = compile [] (Buffers.create 9) in
  let compile r = Decap.apply Buffers.contents (compile r) in
  compile


    (*

  | Chr(c)   -> Decap.char c ()
  | Set(cs)  -> Decap.apply (fun _ -> ()) (Decap.in_charset cs)
  | Seq(a,b) -> Decap.sequence (compile a) (compile b) (fun _ _ -> ())
  | Alt(rs)  -> Decap.alternatives' (List.map compile rs)
  | Opt(r)   -> Decap.option () (compile r)
  | Str(r)   -> let g = Decap.apply (fun _ -> (fun _ -> ())) (compile r) in
                Decap.fixpoint () g
  | Pls(r)   -> compile (Seq(r,Str(r)))
*)




let parse_re = Decap.parse_string re Decap.no_blank

let _ =
  let r = parse_re Sys.argv.(1) in
  Printf.fprintf stdout "Regexp parsed: \"%a\"\n" print_regexp r

  (*
  let g = compile r in
  try Decap.parse_string g no_blank Sys.argv.(2); Printf.printf "OK\n"
  with _ -> Printf.printf "Error...\n"
  *)
 
