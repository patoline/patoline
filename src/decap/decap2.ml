open Charset
open Input
open Str

let debug_lvl = ref 0

exception Give_up of string

let give_up s = raise (Give_up s)

type blank = buffer -> int -> buffer * int

let no_blank str pos = str, pos

let blank_regexp r =
  let r = Str.regexp r in
  let accept_newline = string_match r "\n" 0 && match_end () = 1 in
  let rec fn str pos =
    let str,pos = normalize str pos in
    if string_match r (line str) pos then
      let pos' = match_end () in
      if accept_newline && pos' = String.length (line str) && not (is_empty str pos') then fn str pos'
      else str, pos'
    else str, pos
  in
  fn

(* type de la BNF d'un langage *)
type 'a symbole =
  | Term of (buffer -> int -> 'a * buffer * int)
  | NonTerm of  'a grammar

and _ regle =
  | Empty : 'a -> 'a regle
  | Next : 'b symbole * ('b -> 'a) regle -> 'a regle

and 'a grammar = 'a regle list

type (_) actions =
  | Nothing : ('a -> 'a) actions
  | Something : 'a Lazy.t * ('b -> 'c) actions -> (('a -> 'b) -> 'c) actions

type (_) shifts =
  | Unit   : unit shifts
  | Direct : ('a -> 'a) shifts
  | Shift  : ('a -> 'b) * ('b -> 'c) actions * ('c -> 'd) shifts -> ('a -> 'd) shifts

let rec apply_actions : type a b.(a -> b) actions -> a -> b =
  fun l a ->
    match l with
    | Nothing -> a
    | Something(lazy b,l') -> apply_actions l' (a b)

let rec apply_shifts : type a b.(a -> b) shifts -> a -> b =
  fun l a ->
    match l with
    | Direct -> a
    | Shift(f,acts,s) -> apply_shifts s (apply_actions acts (f a))

(* type de la table de Earley *)
type position = int
type ('s,'a,'b,'c,'d,'r) cell = {
  debut : position;
  fin   : position;
  stack : ('d, 'r) element list ref;
  acts  : 'a actions;
  shifts: 's shifts;
  rest  : 'b regle;
  full  : 'c regle }
and (_,_) element = C : ('a -> 's, 'b -> 'c, 's -> 'b, 'c, 'c, 'r) cell -> ('a,'r) element
and _ final   = D : (unit, 'b -> 'c, 'b, 'c, 'c, 'r) cell -> 'r final
(* si t : table et t.(j) = (i, R, R' R) cela veut dire qu'entre i et j on a parsé
   la règle R' et qu'il reste R à parser. On a donc toujours
   i <= j et R suffix de R' R (c'est pour ça que j'ai écris R' R)
*)

type ('a,'b) eq  = Eq : ('a, 'a) eq | Neq : ('a, 'b) eq

let (===) : type a b.a -> b -> (a,b) eq = fun r1 r2 ->
  if Obj.repr r1 == Obj.repr r2 then Obj.magic Eq else Neq

let eq : 'a 'b.'a -> 'b -> bool = fun x y -> (x === y) <> Neq

let memq : type a b.a -> b list -> (a, b) eq = fun x l ->
  let rec fn : type b.b list -> (a, b) eq = function
    | [] -> Neq
    | y::l' -> match (x === y) with Eq -> Eq | _ -> fn l'
  in fn l

type _ dep_pair = P : 'a regle * ('a, 'b) element list ref * (('a, 'b) element -> unit) ref -> 'b dep_pair

let rec memo_assq : type a b. a regle -> b dep_pair list ref -> ((a, b) element -> unit) -> unit =
  fun r l0 f ->
    let rec fn = function
      | [] -> l0 := P(r,ref [], ref f)::!l0
      | P(r',ptr,g)::l ->
	 match r === r' with
	 | Eq -> g := let g = !g in (fun el -> f el; g el)
	 | _ -> fn l
    in fn !l0

let rec add_assq : type a b. a regle -> (a, b) element -> b dep_pair list ref -> (a, b) element list ref =
  fun r el l0 ->
    let rec fn = function
      | [] -> let res = ref [el] in l0 := P(r,res, ref (fun el -> ()))::!l0; res
      | P(r',ptr,g)::l ->
	 match r === r' with
	 | Eq -> ptr := el :: !ptr; !g el; ptr
	 | _ -> fn l
    in fn !l0

let debut = function D { debut } -> debut

(* ajoute un élément dans la table et retourne true si il est nouveau *)
let add : string -> 'a final -> (int, 'a final list) Hashtbl.t -> bool =
  fun info element old ->
    let debut = debut element in
    let oldl = try Hashtbl.find old debut with Not_found -> [] in
    let eq (D {debut; fin; rest; full}) (D {debut=d'; fin=f'; rest=r'; full=fu'}) =
      assert(fin = f');
      debut = d' && eq rest r' && eq full fu' in
    let absent = not (List.exists (eq element) oldl) in
    if absent then begin
      Hashtbl.replace old debut (element :: oldl);
    end;
    absent

let taille : 'a final list -> int = fun els ->
  let cast_elements : type a b.(a,b) element list -> (Obj.t, Obj.t) element list = Obj.magic in
  let adone = ref [] in
  let res = ref (List.length els) in
  let rec fn : (Obj.t, Obj.t) element list -> unit =
    fun els ->
      if List.exists (eq els) !adone then () else begin
	res := !res + List.length els;
	adone := els :: !adone;
	List.iter (function C {stack} -> fn (cast_elements !stack)) els
      end
  in
  List.iter (function D {stack} -> fn (cast_elements !stack)) els;
  !res

type string_tree =
    TEmpty | Message of string | Expected of string | Node of string_tree * string_tree

exception Error of string_tree

let expected s = raise (Error (Expected s))
let unexpected s = raise (Error (Message s))

let (@@) t1 t2 = Node(t1,t2)
let (~~) t1 = Expected t1
let (~!) t1 = Message t1

let collect_tree t =
  let adone = ref [] in
  let rec fn acc acc' t =
    if List.memq t !adone then acc, acc' else begin
      adone := t :: !adone;
      match t with
	TEmpty -> acc, acc'
      | Message t -> if List.mem t acc then acc, acc' else (t::acc), acc'
      | Expected t -> if List.mem t acc' then acc, acc' else acc, (t::acc')
      | Node(t1,t2) ->
	 let acc, acc' = fn acc acc' t1 in
	 fn acc acc' t2
    end
  in
  let acc, acc' = fn [] [] t in
  List.sort compare acc, List.sort compare acc'

type err_info = {
  mutable max_err_pos:int;
  mutable max_err_buf:buffer;
  mutable max_err_col:int;
  mutable err_msgs: string_tree;
}

let record_error err_info msg str col =
  let pos = Input.line_beginning str + col in
  let pos' = err_info.max_err_pos in
  let c = compare pos pos' in
  if c = 0 then err_info.err_msgs <- msg @@ err_info.err_msgs
  else if c > 0 then
    begin
      err_info.max_err_pos <- pos;
      err_info.max_err_buf <- str;
      err_info.max_err_col <- col;
      err_info.err_msgs <- msg;
    end

(* phase de lecture d'un caractère, qui ne dépend pas de la bnf *)
let lecture : type a.err_info -> buffer -> int -> blank -> (int, a final list) Hashtbl.t ->
		  a final buf_table -> a final buf_table =
  fun err_info buf pos blank elements tbl ->
    let buf, pos = blank buf pos in
    let tbl = ref tbl in
    Hashtbl.iter (fun _ l -> List.iter (function
    | D {debut; fin; stack;acts; rest=Next(Term f,rest); full} ->
       (try
	 let a, buf, pos = f buf pos in
	 let state =
	   (D {debut; fin=line_beginning buf + pos; stack; shifts = Unit;
	      acts = Something(Lazy.from_val a,acts); rest; full})
	 in
	 tbl := insert buf pos state !tbl
       with Error msg -> record_error err_info msg buf pos)
    | _ -> ()) l) elements;
    !tbl

(* selectionnne les éléments commençant par un terminal
   ayant la règle donnée *)
type 'b action = { a : 'a.'a regle -> ('a, 'b) element list ref -> unit }

let pop_final : type a b. b dep_pair list ref -> b final -> b action -> unit =
  fun dlr element act ->
    match element with
    | D {rest=Next(NonTerm(regles),rest); acts; full; debut; fin; stack} ->
       (match rest, debut <> fin with
       | Empty f, true ->
	   List.iter (function C {rest; shifts; acts=acts'; full; debut; stack} ->
	     let c = C {rest; shifts=Shift(f,acts,shifts); acts=acts'; full; debut; fin; stack} in
 	 List.iter (fun r -> act.a r (add_assq r c dlr)) regles) !stack
       | _ ->
	  let c = C {rest; acts; shifts=Direct; full; debut; fin; stack} in
	  List.iter (fun r -> act.a r (add_assq r c dlr)) regles)
    | _ -> assert false


(* fait toutes les prédictions et productions pour un element donné et
   comme une prédiction ou une production peut en entraîner d'autres,
   c'est une fonction récursive *)
let rec one_prediction_production
 : type a. a dep_pair list ref -> (int, a final list) Hashtbl.t -> a final -> unit
 = fun dlr elements element ->
  match element with
  (* prediction (pos, i, ... o NonTerm name::rest_regle) dans la table *)
  | D {fin; acts; stack; rest=Next(NonTerm (regles),_)} ->
     let acts =
       { a = (fun regle stack ->
	 let nouveau = D {shifts = Unit; debut =fin; fin; acts = Nothing; stack; rest  = regle; full = regle} in
	 let b = add "P" nouveau elements in
	 if b then one_prediction_production dlr elements nouveau) }
     in pop_final dlr element acts

  (* production	(pos, i, ... o ) dans la table *)
  | D {debut=i; fin=j; acts; stack=els;rest=Empty a; full=regle} ->
     let x = apply_actions acts a in
     let complete element =
       match element with
       | C {debut=k; fin=i'; stack=els'; shifts; acts; rest; full} ->
	  assert(i=i');
	 let x = Lazy.from_fun (fun () -> apply_shifts shifts x) in
	 let nouveau = D {shifts = Unit; debut=k; acts = Something(x,acts); fin=j; stack=els'; rest; full} in
	 let b = add "C" nouveau elements in
	 if b then one_prediction_production dlr elements nouveau
     in
     if i = j then memo_assq regle dlr complete;
     List.iter complete !els
  | _ -> ()

(* fait toutes les prédictions productions pour les entrées de la
   table à la position indiquée *)
let rec prediction_production : type a.(int, a final list) Hashtbl.t -> unit = fun elements ->
  let dlr = ref [] in
  Hashtbl.iter ((fun _ l -> List.iter (fun el -> one_prediction_production dlr elements el) l))
    elements

exception Parse_error of string * int * int * string list * string list

let parse_buffer : type a.a grammar -> blank -> buffer -> a =
  fun main blank buf ->
    (* construction de la table initiale *)
    let err_info =
      { max_err_pos = -1 ; max_err_buf = buf
      ; max_err_col = -1; err_msgs = TEmpty}
    in
    let pos = ref 0 and buf = ref buf in
    let elements : (int, a final list) Hashtbl.t = Hashtbl.create 31 in
    let r0 : a regle = Next(NonTerm main,Empty(fun x -> x)) in
    let s0 : (a, a) element list ref = ref [] in
    let _ = add "I" (D {shifts = Unit; debut=0; fin=0; acts = Nothing; stack=s0; rest=r0; full=r0}) elements in
    let forward = ref empty_buf in
    let parse_error () =
      let buf = err_info.max_err_buf in
      let pos = err_info.max_err_col in
      let msgs = err_info.err_msgs in
      let msg, expected = collect_tree msgs in
      raise (Parse_error (fname buf, line_num buf, pos, msg, expected))
    in
    (* boucle principale *)
    while not (is_empty !buf !pos) do
      prediction_production elements;
      forward := lecture err_info !buf !pos blank elements !forward;
      let (buf', pos', l, forward') = pop_firsts !forward in
      pos := pos';
      buf := buf';
      forward := forward';
      Hashtbl.clear elements;
      List.iter (fun s -> ignore (add "L" s elements)) l;
      if Hashtbl.length elements = 0 then parse_error ()
    done;
    prediction_production elements;
  (* on regarde si on a parsé complètement la catégorie initiale *)
    let rec fn : type a.a final list -> a = function
      | [] -> parse_error ()
      | D {debut=0; stack=s1; rest=Empty f; acts = a; full=r1} :: els ->
	 (match r1 === r0, s1 === s0 with
	 | Eq, Eq -> apply_actions a f
	 | _ -> fn els)
      | _ :: els -> fn els
    in fn (try Hashtbl.find elements 0 with Not_found -> [])

let parse_string ?(filename="") grammar blank str =
  let str = buffer_from_string ~filename str in
  parse_buffer grammar blank str

let parse_channel ?(filename="") grammar blank ic  =
  let str = buffer_from_channel ~filename ic in
  parse_buffer grammar blank str

let parse_file grammar blank filename  =
  let str = buffer_from_file filename in
  parse_buffer grammar blank str

let eof : 'a -> 'a grammar
  = fun a ->
    let fn buf pos =
      if is_empty buf pos then (a,buf,pos) else expected "EOF"
    in
    [Next(Term(fn),Empty(fun x -> x))]

let char : char -> 'a -> 'a grammar
  = fun c a ->
    let msg = Printf.sprintf "%C" c in
    let fn buf pos =
      let c', str', pos' = read buf pos in
      if c = c' then (a,buf,pos) else expected msg
    in
    [Next(Term(fn),Empty(fun x -> x))]

let in_charset : charset -> char grammar
  = fun cs ->
    let msg = Printf.sprintf "%s" (String.concat "|" (list_of_charset cs)) in
    let fn buf pos =
      let c, str', pos' = read buf pos in
      if mem cs c then (c,buf,pos) else expected msg
    in
    [Next(Term(fn),Empty(fun x -> x))]

let any : char grammar
  = let fn buf pos =
      let c', str', pos' = read buf pos in
      (c',buf,pos)
    in
    [Next(Term(fn),Empty(fun x -> x))]

let string : string -> 'a -> 'a grammar
  = fun s a ->
    let fn buf pos =
      let buf = ref buf in
      let pos = ref pos in
      let len_s = String.length s in
      for i = 0 to len_s - 1 do
        let c, _buf, _pos = read !buf !pos in
        if c <> s.[i] then expected s;
        buf := _buf; pos := _pos
      done;
      (a,!buf,!pos)
    in
    [Next(Term(fn),Empty(fun x -> x))]

let regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a grammar
  = fun r0 ?(name=String.escaped r0) a ->
    let r = Str.regexp r0 in
    let fn buf pos =
      let l = line buf in
      if pos > String.length l then expected name;
      if string_match r l pos then (
        let f n = matched_group n l in
        let pos' = match_end () in
	if pos' = pos then expected name;
	let res = try a f with Give_up msg -> unexpected msg in
	(res,buf,pos))
      else expected name
    in
    [Next(Term(fn),Empty(fun x -> x))]

(* charset is now useless ... will be suppressed soon *)
let black_box : (buffer -> int -> 'a * buffer * int) -> charset -> 'a option -> string -> 'a grammar
  = fun fn _ empty name ->
    match empty with
    | None -> [Next(Term(fn),Empty(fun x -> x))]
    | Some a -> [Next(Term(fn),Empty(fun x -> x)); Empty a]

let fail : string -> 'a grammar
  = fun msg ->
    let fn buf pos =
      unexpected msg
    in
    [Next(Term(fn),Empty(fun x -> x))]

let empty : 'a -> 'a grammar = fun a -> [Empty a]

let sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 l2 f ->
    [Next(NonTerm l1, Next(NonTerm l2, Empty(fun b a -> f a b)))]

let sequence3 : 'a grammar -> 'b grammar -> 'c grammar -> ('a -> 'b -> 'c -> 'd) -> 'd grammar
  = fun l1 l2 l3 f ->
    [Next(NonTerm l1, Next(NonTerm l2, Next(NonTerm l3, Empty(fun c b a -> f a b c))))]

(* TODO *)
let sequence_position : 'a grammar -> 'b grammar -> ('a -> 'b -> buffer -> int -> buffer -> int -> 'c) -> 'c grammar
   = fun l1 l2 f -> failwith "sequence_position uniplemented"

let fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence l1 l2 (fun x f -> f x)

let fsequence_position : 'a grammar -> ('a -> buffer -> int -> buffer -> int -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence_position l1 l2 (fun x f -> f x)

let dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun l1 f2 -> failwith "dependent_sequence uniplemented"

let iter : 'a grammar grammar -> 'a grammar
  = fun g -> dependent_sequence g (fun x -> x)

let conditional_sequence : 'a grammar -> ('a -> bool) -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 cond l2 f ->
    let l1 = [Next(NonTerm l1, Empty(fun a -> if cond a then a else raise (Error TEmpty)))] in
    [Next(NonTerm l1, Next(NonTerm l2, Empty(fun b a -> f a b)))]

let change_layout : ?new_blank_before:bool -> ?old_blank_after:bool -> 'a grammar -> blank -> 'a grammar
  = fun ?(new_blank_before=true) ?(old_blank_after=true) l1 blank1 ->
    failwith "change_layout unimplemented"

let ignore_next_blank : 'a grammar -> 'a grammar
  = fun l1 ->
    failwith "ignore_next_blank unimplemented"

let option : 'a -> 'a grammar -> 'a grammar
  = fun a l -> Empty a::l

let option' = option

let fixpoint :  'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    let rec res = [Empty a;
		   Next(NonTerm res, Next(NonTerm f1, Empty(fun f a -> f a)))] in
    res

let fixpoint' = fixpoint

let fixpoint1 :  'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    let rec res = [Next(NonTerm f1, Empty(fun f -> f a));
		   Next(NonTerm res, Next(NonTerm f1, Empty(fun f a -> f a)))] in
    res

let fixpoint1' = fixpoint1

let delim g = g

let alternatives : 'a grammar list -> 'a grammar = List.flatten

let alternatives' = alternatives

let blank_grammar grammar blank str pos = failwith "blank_grammar unimplemented"

let accept_empty grammar = failwith "accept_empty unimplemented"

let declare_grammar name = failwith "declare_grammar unimplemented"

let set_grammar p1 p2 = failwith "set_grammar unimplemented"

let print_exception = function
  | Parse_error(fname,l,n,msg, expected) ->
     let expected =
       if expected = [] then "" else
	 Printf.sprintf "'%s' expected" (String.concat "|" expected)
     in
     let msg = if msg = [] then "" else (String.concat "," msg)
     in
     let sep = if msg <> "" && expected <> "" then ", " else "" in
     Printf.eprintf "File %S, line %d, characters %d-%d:\nParse error:%s%s%s\n%!" fname l n n msg sep expected
  | _ -> assert false

let handle_exception f a =
  try f a with Parse_error _ as e -> print_exception e; failwith "No parse."

let handle_exception= failwith "handle_exception unimplemented"
let grammar_family = failwith "grammar_family unimplemented"
let merge = failwith "merge unimplemented"
let lists = failwith "lists unimplemented"
let position = failwith "position unimplemented"
let cache = failwith "cache unimplemented"
let apply = failwith "apply unimplemented"
let internal_parse_buffer = failwith "internal_parse_buffer unimplemented"
let active_debug = failwith "active_debug unimplemented"
let debug = failwith "debug unimplemented"
let partial_parse_string = failwith "partial_parse_string unimplemented"
let partial_parse_buffer = failwith "partial_parse_buffer unimplemented"
let apply_position = failwith "apply_position unimplemented"
