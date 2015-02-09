(* AST of types *)
type btype =
  | Bool | Int | Char | String | Int32 | Int64 | Nativeint
  | Option of btype
  | List of btype
  | Location_t | Location_loc
  | Longident_t
  | Class_infos of btype
  | Include_infos of btype
  | Var of string
  | Loc of btype
  | Name of string
  | Prod of btype list

type typetype =
  | Syn of string * btype
  | Sum of string * (string * btype list) list
  | Rec of string option * string * (string * btype) list

type item =
  | Type of typetype list
  | Open of string

type ast = item list

(* Parser *)
let parser lid = | RE("[a-z][_a-z]*")
let parser uid = | RE("[A-Z][_a-zA-Z0-9]*")
let parser arg = | '\'' - RE("[a-z]+")

let parser base_type p_auth =
  | t:(base_type false) "option"                  -> Option t
  | t:(base_type false) "list"                    -> List t
  | t:(base_type false) "loc"                     -> Loc t
  | t:(base_type false) "class_infos"             -> Class_infos t
  | t:(base_type false) "include_infos"           -> Include_infos t
  | t:(base_type false) ts:{'*' (base_type false)}++ when p_auth -> Prod (t :: ts)
  | "bool"                                        -> Bool
  | "int"                                         -> Int
  | "char"                                        -> Char
  | "string"                                      -> String
  | "int32"                                       -> Int32
  | "int64"                                       -> Int64
  | "nativeint"                                   -> Nativeint
  | "Location.t"                                  -> Location_t
  | "Location.loc"                                -> Location_loc
  | a:arg                                         -> Var a
  | n:lid                                         -> Name n
  | "Longident.t"                                 -> Longident_t
  | '(' (base_type true) ')'


let base_type = base_type true

let parser cdecl =
  | c:uid t:{"of" base_type}? ->
      begin
        let ts =
          match t with
          | None           -> []
          | Some (Prod ts) -> ts
          | Some t         -> [t]
        in (c, ts)
      end

let parser csdecl =
  | '|'? c:cdecl cs:{'|' cdecl}* -> c::cs

let parser field =
  | lid ':' base_type

let parser fields =
  | f:field fs:{';' field}* ';'? -> f::fs

let parser any_decl =
  | n:lid '=' b:base_type                                      -> Syn (n,b)
  | n:lid '=' cl:csdecl                                        -> Sum (n,cl)
  | a:arg? n:lid '=' _:{arg? base_type '='}? '{' fs:fields '}' -> Rec (a,n,fs)

let parser any_rec_decl =
  | "type" t:any_decl ts:{"and" any_decl}* -> Type (t::ts)
  | "open" n:uid                           -> Open n

let parser any_decls = | any_rec_decl*

let blank = Pa_ocaml_prelude.blank
let parse_file = Decap.parse_file any_decls blank
let parse_channel = Decap.parse_channel any_decls blank

let parse () =
  match Array.length Sys.argv with
  | 1 -> parse_channel stdin
  | 2 -> parse_file Sys.argv.(1)
  | _ -> failwith "Wrong number of arguments..."

(* Printer *)
open Printf

let rec zip xs ys =
  match xs, ys with
  | []   , []    -> []
  | x::xs, y::ys -> (x,y) :: zip xs ys
  | _    , _     -> assert false

let rec print_btype ch = function
  | Bool            -> output_string ch "quote_bool"
  | Int             -> output_string ch "quote_int"
  | Char            -> output_string ch "quote_char"
  | String          -> output_string ch "quote_string"
  | Int32           -> output_string ch "quote_int32"
  | Int64           -> output_string ch "quote_int64"
  | Nativeint       -> output_string ch "quote_nativeint"
  | Option t        -> fprintf ch "(quote_option %a)" print_btype t
  | List t          -> fprintf ch "(quote_list %a)" print_btype t
  | Location_t      -> output_string ch "quote_location_t"
  | Location_loc    -> assert false (* never used *)
  | Longident_t     -> output_string ch "quote_longident"
  | Class_infos t   -> fprintf ch "(quote_class_infos %a)" print_btype t
  | Include_infos t -> fprintf ch "(quote_include_infos %a)" print_btype t
  | Var a           -> fprintf ch "quote_%s" a
  | Loc t           -> fprintf ch "(quote_loc %a)" print_btype t
  | Name n          -> fprintf ch "quote_%s" n
  | Prod lt         ->
      let len = List.length lt in
      let rec build_list pfx n =
        if n = 0 then [] else (pfx^(string_of_int n)) :: build_list pfx (n-1)
      in
      let qs = List.rev (build_list "q" len) in
      let lqs = String.concat " " qs in
      let xs = List.rev (build_list "x" len) in
      let cxs = "(" ^ (String.concat "," xs) ^ ")" in
      let xqs = zip xs qs in
      fprintf ch "(fun %s _loc %s -> quote_tuple _loc [" lqs cxs;
      let f (x, q) = fprintf ch "%s _loc %s;" q x in
      List.iter f xqs; fprintf ch "])";
      List.iter (print_btype ch) lt

let print_type ch = function
  | Syn (n,t)    -> fprintf ch "quote_%s = %a" n print_btype t
  | Sum (n,cl)   ->
      fprintf ch "quote_%s _loc = function\n" n;
      let f (c, ts) =
        match ts with
        | []  -> fprintf ch "  | %s -> quote_const _loc %s []\n" c c
        | [t] -> fprintf ch "  | %s(x) -> quote_const _loc %s [%a x]\n" c c
                   print_btype t
        | _   ->
            let len = List.length ts in
            let rec build_list pfx n =
              if n = 0 then []
              else (pfx^(string_of_int n)) :: build_list pfx (n-1)
            in
            let xs = List.rev (build_list "x" len) in
            let cxs = "(" ^ (String.concat "," xs) ^ ")" in
            fprintf ch "  | %s%s -> quote_const _loc %s [" c cxs c;
            let txs = zip ts xs in
            let f (t,x) = Printf.fprintf ch " %a _loc %s;" print_btype t x in
            List.iter f txs;
            fprintf ch "]\n"
      in
      List.iter f cl
  | Rec (a,n,fl) ->
      (match a with
       | None   -> fprintf ch "quote_%s _loc r = " n
       | Some a -> fprintf ch "quote_%s _loc eq_%s r =\n" n a);
      fprintf ch "  quote_record _loc [\n";
      let f (l, t) =
        fprintf ch "    (%s, %a _loc r.%s) ;\n" l print_btype t l
      in
      List.iter f fl;
      fprintf ch "  ]\n"

let print_types ch = function
  | []      -> assert false
  | [x]     -> Printf.fprintf ch "let %a\n" print_type x
  | x :: xs -> Printf.fprintf ch "let rec %a\n" print_type x;
               let f t = Printf.fprintf ch "and %a\n" print_type t in
               List.iter f xs

let rec print ch = function
  | []           -> ()
  | Open _ :: xs -> print ch xs
  | Type l :: xs -> print_types ch l; print ch xs

(* Main program *)
let _ =
  Printf.eprintf "Parsing ... %!";
  let ast = parse () in
  Printf.eprintf "[OK - %i]\n%!" (List.length ast);
  print stdout ast;
