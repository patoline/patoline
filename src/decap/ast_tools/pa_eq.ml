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
let parser lid = | ''[a-z][_a-z]*''
let parser uid = | ''[A-Z][_a-zA-Z0-9]*''
let parser arg = | '\'' - RE("[a-z]+")

let parser base_type p_auth =
  | "bool"                                        -> Bool
  | "int32"                                       -> Int32
  | "int64"                                       -> Int64
  | "int"                                         -> Int
  | "char"                                        -> Char
  | "string"                                      -> String
  | "nativeint"                                   -> Nativeint
  | "Longident.t"                                 -> Longident_t
  | "Location.t"                                  -> Location_t
  | "Location.loc"                                -> Location_loc
  | t:(base_type false) "option"                  -> Option t
  | t:(base_type false) "list"                    -> List t
  | t:(base_type false) "loc"                     -> Loc t
  | t:(base_type false) "class_infos"             -> Class_infos t
  | t:(base_type false) "include_infos"           -> Include_infos t
  | t:(base_type false) ts:{'*' (base_type false)}++ when p_auth -> Prod (t :: ts)
  | a:arg                                         -> Var a
  | n:lid                                         -> Name n
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
let rec print_btype ch = function
  | Bool | Int | Char | String | Int32 | Int64
  | Nativeint       -> Printf.fprintf ch "(=)"
  | Option t        -> Printf.fprintf ch "(eq_option %a)" print_btype t
  | List t          -> Printf.fprintf ch "(eq_list %a)" print_btype t
  | Location_t      -> Printf.fprintf ch "(fun _ _ -> true)"
  | Location_loc    -> assert false (* never used *)
  | Longident_t     -> Printf.fprintf ch "eq_longident"
  | Class_infos t   -> Printf.fprintf ch "(eq_class_infos %a)" print_btype t
  | Include_infos t -> Printf.fprintf ch "(eq_include_infos %a)" print_btype t
  | Var a           -> Printf.fprintf ch "eq_%s" a
  | Loc t           -> Printf.fprintf ch "(eq_loc %a)" print_btype t
  | Name n          -> Printf.fprintf ch "eq_%s" n
  | Prod lt         ->
      let len = List.length lt in
      let rec build_list pfx n =
        if n = 0 then [] else (pfx^(string_of_int n)) :: build_list pfx (n-1)
      in
      let xs = List.rev (build_list "x" len) in
      let ys = List.rev (build_list "y" len) in
      let cxs = "(" ^ (String.concat "," xs) ^ ")" in
      let cys = "(" ^ (String.concat "," ys) ^ ")" in
      let rec zip3 xs ys ts =
        match xs, ys, ts with
        | []   , []   , []    -> []
        | x::xs, y::ys, t::ts -> (x,y,t) :: zip3 xs ys ts
        | _ -> assert false
      in
      let data = zip3 xs ys lt in
      Printf.fprintf ch "(fun %s %s -> true" cxs cys;
      let f (x, y, t) =
        Printf.fprintf ch " && (%a %s %s)" print_btype t x y
      in
      List.iter f data;
      Printf.fprintf ch ")"

let print_type ch = function
  | Syn (n,t)    -> Printf.fprintf ch "eq_%s c1 c2 = %a c1 c2" n print_btype t
  | Sum (n,cl)   ->
      Printf.fprintf ch "eq_%s c1 c2 =\n  match c1, c2 with\n" n;
      let f (c, ts) =
        match ts with
        | []     -> Printf.fprintf ch "  | %s, %s -> true\n" c c
        | [t]    -> Printf.fprintf ch "  | %s(x), %s(y) -> %a x y\n" c c
                      print_btype t
        | ts     ->
            let len = List.length ts in
            let rec build_list pfx n =
              if n = 0 then []
              else (pfx^(string_of_int n)) :: build_list pfx (n-1)
            in
            let xs = List.rev (build_list "x" len) in
            let ys = List.rev (build_list "y" len) in
            let cxs = "(" ^ (String.concat "," xs) ^ ")" in
            let cys = "(" ^ (String.concat "," ys) ^ ")" in
            let rec zip3 xs ys ts =
              match xs, ys, ts with
              | []   , []   , []    -> []
              | x::xs, y::ys, t::ts -> (x,y,t) :: zip3 xs ys ts
              | _ -> assert false
            in
            let data = zip3 xs ys ts in
            Printf.fprintf ch "  | %s%s, %s%s -> true" c cxs c cys;
            let f (x, y, t) =
              Printf.fprintf ch " && (%a %s %s)" print_btype t x y
            in
            List.iter f data;
      in
      List.iter f cl;
      Printf.fprintf ch "  | _, _ -> false"
  | Rec (a,n,fl) ->
      (match a with
        | None   -> Printf.fprintf ch "eq_%s = fun r1 r2 -> true" n
        | Some a -> Printf.fprintf ch "eq_%s : 'a. ('a -> 'a -> bool) -> 'a %s -> 'a %s -> bool = fun eq_%s r1 r2 -> true"
	                    n n n a);
      let f (l,t) =
        Printf.fprintf ch " && %a r1.%s r2.%s" print_btype t l l
      in
      List.iter f fl

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
