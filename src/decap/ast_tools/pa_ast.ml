let parser lid = | RE("[a-z][_a-z]*")
let parser uid = | RE("[A-Z][_a-zA-Z0-9]*")
let parser arg = | RE("['][a-z]*")

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
  | Sum of string * (string * btype option) list
  | Rec of string option * string * (string * btype) list

type ast =
  | Type of typetype list
  | Open of string

let parser base_type =
  | "bool"         -> Bool
  | "int"          -> Int
  | "char"         -> Char
  | "string"       -> String
  | "int32"        -> Int32
  | "int64"        -> Int64
  | "nativeint"    -> Nativeint
  | "Location.t"   -> Location_t
  | "Location.loc" -> Location_loc
  | a:arg          -> Var a
  | n:lid          -> Name n
  | "Longident.t"  -> Longident_t
  | '(' base_type ')'
  | t:base_type "option"        -> Option t
  | t:base_type "list"          -> List t
  | t:base_type "loc"           -> Loc t
  | t:base_type "class_infos"   -> Class_infos t
  | t:base_type "include_infos" -> Include_infos t
  | t1:base_type '*' t2:base_type ts:{'*' base_type}* -> Prod (t1 :: t2 :: ts)

(*
  | t:base_type r:rest ->
      begin
        match r with
        | `Option  -> Option t
        | `List    -> List t
        | `Loc     -> Loc t
        | `Prod ts -> Prod (t :: ts)
      end
and rest =
  | "option" -> `Option
  | "list"   -> `List
  | "loc"    -> `Loc
  | DEBUG"prod" ts:{'*' base_type}+ -> `Prod ts
*)

let parser cdecl =
  | uid {"of" base_type}?

let parser csdecl =
  | '|'? c:cdecl cs:{'|' cdecl}* -> c::cs

let parser field =
  | lid ':' base_type

let parser fields =
  | f:field fs:{';' field}* ';'? -> f::fs

let parser any_decl =
  | n:lid '=' cl:csdecl                                        -> Sum (n,cl)
  | a:arg? n:lid '=' _:{arg? base_type '='}? '{' fs:fields '}' -> Rec (a,n,fs)
  | n:lid '=' b:base_type                                      -> Syn (n,b)

let parser any_rec_decl =
  | "type" t:any_decl ts:{"and" any_decl}** -> Type (t::ts)
  | "open" n:uid                            -> Open n

let parser any_decls = | any_rec_decl*

let blank = Decap.blank_regexp "[ \n\r\t]*"
let parse = Decap.parse_file any_decls blank

(*
let rec print_quote_base = function
  | Bool         -> "<:expr< quote_bool >>"
  | Int          -> "<:expr< quote_int >>"
  | Char         -> "<:expr< quote_char >>"
  | String       -> "<:expr< quote_string >>"
  | Int32        -> "<:expr< quote_int32 >>"
  | Int64        -> "<:expr< quote_int64 >>"
  | Nativeint    -> "<:expr< quote_nativeint >>"
  | Option t     -> Printf.sprintf "<:expr< quote_option (%s) >>"
                    (print_quote_base t)
  | List t       -> Printf.sprintf "<:expr< quote_list (%s) >>"
                    (print_quote_base t)
  | Location_t   -> "<:expr< quote_Location_t >>"
  | Location_loc -> "<:expr< quote_Location_loc >>"
  | _            -> assert false

let print_quote = function
  | Syn (n,b)         ->
      begin
        Printf.printf "let quote_%s =\n%!" n;
        Printf.printf "  %s\n\n%!" (print_quote_base b)
      end
  | Sum (n,cl)        ->
      begin
        Printf.printf "let quote_%s = function\n%!" n;
        let print_case (c, p) =
          match p with
          | [] -> Printf.printf "  | %s -> <:expr< %s >>\n%!" c c
          | _  -> Printf.printf "  | %s ... -> ...\n%!" c
        in
        List.iter print_case cl;
        Printf.printf "\n%!";
        (* TODO *)
      end
  | Rec (None,n,fl)   ->
      begin
        Printf.printf "let quote_%s =\n%!" n;
        Printf.printf "  ...\n\n%!";
        (* TODO *)
      end
  | Rec (Some a,n,fl) ->
      begin
        Printf.printf "let quote_%s =\n%!" n;
        Printf.printf "  ...\n\n%!";
        (* TODO *)
      end
*)

let _ =
  let grammar = any_decls in
  let blank = Decap.blank_regexp "[ \n\r\t]*" in
  let ast =
    match Array.length Sys.argv with
    | 1 -> Decap.parse_channel grammar blank stdin
    | 2 -> Decap.parse_file grammar blank Sys.argv.(1)
    | 3 -> let _ = Decap.parse_string base_type blank Sys.argv.(2) in []
    | _ -> failwith "Wrong number of arguments..."
  in
  Printf.eprintf "OK! (%i)\n%!" (List.length ast)
  (*
  Printf.printf "let _loc = Location.none\n\n%!";
  List.iter print_quote ast;
  *)
