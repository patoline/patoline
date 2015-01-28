(*
  ======================================================================
  Copyright Christophe Raffalli & Rodolphe Lepigre
  LAMA, UMR 5127 - Université Savoie Mont Blanc

  christophe.raffalli@univ-savoie.fr
  rodolphe.lepigre@univ-savoie.fr

  This software contains implements a parser combinator library together
  with a syntax extension mechanism for the OCaml language.  It  can  be
  used to write parsers using a BNF-like format through a syntax extens-
  ion called pa_parser.

  This software is governed by the CeCILL-B license under French law and
  abiding by the rules of distribution of free software.  You  can  use,
  modify and/or redistribute it under the terms of the CeCILL-B  license
  as circulated by CEA, CNRS and INRIA at the following URL:

            http://www.cecill.info

  The exercising of this freedom is conditional upon a strong obligation
  of giving credits for everybody that distributes a software incorpora-
  ting a software ruled by the current license so as  all  contributions
  to be properly identified and acknowledged.

  As a counterpart to the access to the source code and rights to  copy,
  modify and redistribute granted by the  license,  users  are  provided
  only with a limited warranty and the software's author, the holder  of
  the economic rights, and the successive licensors  have  only  limited
  liability.

  In this respect, the user's attention is drawn to the risks associated
  with loading, using, modifying and/or developing  or  reproducing  the
  software by the user in light of its specific status of free software,
  that may mean that it is complicated  to  manipulate,  and  that  also
  therefore means that it is reserved  for  developers  and  experienced
  professionals having in-depth computer knowledge. Users are  therefore
  encouraged to load and test  the  software's  suitability  as  regards
  their requirements in conditions enabling the security of  their  sys-
  tems and/or data to be ensured and, more generally, to use and operate
  it in the same conditions as regards security.

  The fact that you are presently reading this means that you  have  had
  knowledge of the CeCILL-B license and that you accept its terms.
  ======================================================================
*)

open Str
open Charset
open Input

exception Parse_error of string * int * int * string list * string list
exception Give_up of string
exception Error

type string_tree =
    Empty | Message of string | Expected of string | Node of string_tree * string_tree

let (@@) t1 t2 = Node(t1,t2)
let (~~) t1 = Expected t1
let (~!) t1 = Message t1

let collect_tree t =
  let rec fn acc acc' = function
      Empty -> acc, acc'
    | Message t -> if List.mem t acc then acc, acc' else (t::acc), acc'
    | Expected t -> if List.mem t acc' then acc, acc' else acc, (t::acc')
    | Node(t1,t2) -> 
       let acc, acc' = fn acc acc' t1 in
       fn acc acc' t2
  in
  let acc, acc' = fn [] [] t in
  List.sort compare acc, List.sort compare acc'

module Pos = struct
  type t = buffer * int
  let compare = fun (b,p) (b',p') ->
    line_beginning b + p - line_beginning b' - p'
end

module PosMap = Map.Make(Pos)

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

type err_info = {
  mutable max_err_pos:int;
  mutable max_err_buf:buffer;
  mutable max_err_col:int;
  mutable err_msgs: string_tree;
}

type stack = EmptyStack | Pushed of int * Obj.t * stack

let rec print_stack ch = function
  | Pushed(n,_,l) -> Printf.fprintf ch "W %d %a" n print_stack l
  | EmptyStack -> ()

let pop_stack n = function
  | Pushed(n',v,l) when n = n' -> Obj.obj v, l
  | _ -> raise Not_found
	       
type grouped = {
  blank: blank;
  err_info: err_info;
  stack : stack;
  leftrecs : int list;
}

let rec test_clear grouped =
  match grouped.stack with
  | Pushed(_,_,_) -> raise Error
  | EmptyStack -> ()

let push_rec_info n str pos charset grouped =
  { grouped with leftrecs = n::grouped.leftrecs;  }
    
let rec check_rec_info n str pos grouped =
  if not (List.mem n grouped.leftrecs) then raise Not_found

let print_info ch grouped =
  Printf.fprintf ch "%a %a"
		 (fun ch -> List.iter (Printf.fprintf ch "%d ")) grouped.leftrecs
		 print_stack grouped.stack
  
let set_stack grouped stack =
  if grouped.stack == stack then grouped else { grouped with stack; }

let set_stack_and_reset grouped stack =
  if grouped.stack == stack && grouped.leftrecs = [] then grouped
  else { grouped with leftrecs = []; stack }
						
type next = {
  accepted_char: charset;
  first_syms : string_tree;
}

let all_next = 
  { accepted_char = full_charset; 
    first_syms = Empty }

type ('a, 'b) continuation = buffer -> int -> buffer -> int -> buffer -> int -> stack -> 'a -> 'b

type empty_type = R of empty_type

type 'a accept_empty =
    Non_empty
  | Unknown (* during building of left recursion *)
  | May_empty of (buffer -> int -> 'a)

type left_rec =
    Non_rec
  | Left_rec

(* if a grammar has a prefix which is a left recursive grammar with ident n,
      (n, b, s) is in the list and
      - b = true if there may be nothing after the prefix
      - s is the cahracter set accepted after the prefix *)
type charset_after_left = (int * (bool * charset * string_tree)) list

		  
module rec G: sig
  type 'a grammar = {
    ident : int;
    mutable firsts : charset;
    mutable first_sym : string_tree;
    mutable accept_empty : 'a accept_empty;
    mutable left_rec : left_rec;
    mutable charset_after_left : charset_after_left;
    mutable parse : 'b. grouped -> buffer -> int -> next -> ('a, 'b) continuation -> 'b;
    mutable set_info : unit -> unit;
    mutable deps : TG.t option;
  }

  include Hashtbl.HashedType with type t = empty_type grammar
							
end = struct
  type 'a grammar = {
    ident : int;
    mutable firsts : charset;
    mutable first_sym : string_tree;
    mutable accept_empty : 'a accept_empty;
    mutable left_rec : left_rec;
    mutable charset_after_left : charset_after_left;
    mutable parse : 'b. grouped -> buffer -> int -> next -> ('a, 'b) continuation -> 'b;
    mutable set_info : unit -> unit;
    mutable deps : TG.t option;
  }

  type t = empty_type grammar

  let hash g = Hashtbl.hash g.ident

  let equal g1 g2 = g1.ident = g2.ident
end

and TG:Weak.S with type data = G.t = Weak.Make(G)

include G

let new_ident =
  let c = ref 0 in
  (fun () -> let i = !c in c := i + 1; i)

let cast : 'a grammar -> empty_type grammar = Obj.magic
		    
let record_error grouped msg str col =
  let pos = Input.line_beginning str + col in
  let pos' = grouped.err_info.max_err_pos in
  let c = compare pos pos' in
  if c = 0 then grouped.err_info.err_msgs <- msg @@ grouped.err_info.err_msgs
  else if c > 0 then
    begin
      grouped.err_info.max_err_pos <- pos;
      grouped.err_info.max_err_buf <- str;
      grouped.err_info.max_err_col <- col;
      grouped.err_info.err_msgs <- msg;
    end

let parse_error grouped msg str pos =
  record_error grouped msg str pos;
  raise Error


let firsts g = g.firsts
let first_sym g = g.first_sym
let next_sym g ={
                 accepted_char = firsts g;
                 first_syms = first_sym g;
               }

let next_sym' grouped g =
  match grouped.stack with
    EmptyStack -> next_sym g
  | Pushed(n,_,_) ->
     try
       let (_,charset,syms) = List.assoc n g.charset_after_left in
       {
         accepted_char = charset;
	 first_syms = syms;
       }
     with
       Not_found ->
       {
         accepted_char = empty_charset;
	 first_syms = Empty;
       }

let apply_blank grouped str p =
  grouped.blank str p

let test grouped next str p =
  let c = get str p in
  let res = mem next.accepted_char c in
  if not res then
    begin
      let msg = next.first_syms in
      record_error grouped msg str p
    end;
  res

let accept_empty l1 = l1.accept_empty <> Non_empty

let read_empty e =
  match e.accept_empty with
  | May_empty x -> x
  | _ -> assert false		  
					   
let union_firsts l1 l2 =
  if accept_empty l1 then
    union (firsts l1) (firsts l2)
  else
    (firsts l1)

let union_first_sym l1 l2 =
  if accept_empty l1 then
    first_sym l1 @@ first_sym l2
  else
    first_sym l1

let union'' gram next =
  { accepted_char = union next.accepted_char (firsts gram);
    first_syms = next.first_syms @@ first_sym gram;
  }

let union' gram next =
  if accept_empty gram then union'' gram next
  else next_sym gram

let merge_list l1 l2 merge =
  let rec fn l1 l2 = match l1, l2 with
    | ([], l) | (l, []) -> l
    | ((n1,d1 as c1)::l1'), ((n2,d2 as c2)::l2') ->
       match compare n1 n2 with
       | -1 -> c1::fn l1' l2
       | 1 -> c2::fn l1 l2'
       | 0 -> (n1, merge d1 d2)::fn l1' l2'
       | _ -> assert false
  in fn l1 l2

let sum_charset_after_left l1 l2 =
  merge_list l1 l2.charset_after_left
	     (fun (b1,c1,s1) (b2,c2,s2) -> b1 || b2, Charset.union c1 c2, Node(s1,s2))

let compose_charset_after_left l1 l2 =
  let l0 = List.map (fun (n,(b,c,s) as x) ->
		     if b then (n,(accept_empty l2,Charset.union c l2.firsts, Node(s,l2.first_sym))) else x) l1.charset_after_left
  in
  if accept_empty l1 then sum_charset_after_left l0 l2 else l0

let not_ready name _ = failwith ("not_ready: "^name)
				
let declare_grammar name = let id = new_ident () in {
  ident = id;
  firsts = empty_charset;
  first_sym = Empty;
  accept_empty = Unknown;
  left_rec = Non_rec;
  charset_after_left = [id,(true,empty_charset,Empty)];
  deps = Some (TG.create 7);
  set_info = (fun () -> ());
  parse = (not_ready name);
}

let chg_empty s1 s2 = match s1, s2 with
    Non_empty, Non_empty -> false
  | Unknown, Unknown -> false
  | May_empty _, May_empty _ -> false
  | _ -> true
	   
let rec update (g : empty_type grammar) =
  let old_firsts = g.firsts in
  let old_accept_empty = g.accept_empty in
  let old_charset_after_left = g.charset_after_left in
  g.set_info ();
  if (old_firsts <> g.firsts
      || chg_empty old_accept_empty g.accept_empty
      || old_charset_after_left <> g.charset_after_left)
  then match g.deps with None -> () | Some t -> TG.iter update t

let add_deps p1 p2 = match p2.deps with None -> () | Some t -> TG.add t (cast p1)

let tbl = Ahash.create 1001

let debug_leftrec = ref false

let set_grammar p1 p2 =
  let rec fn p =
    match p.deps with
      None -> false
    | Some deps ->
       (try Ahash.find tbl p.ident
	with Not_found ->
	  Ahash.add tbl p.ident false;
	  let res = TG.mem deps (cast p2) ||
		      TG.fold (fun d acc -> fn d || acc) deps false
	  in
	  if res then p.left_rec <- Left_rec;
	  Ahash.add tbl p.ident res;
	  res
       )
  in
  if fn (cast p1) then
    p1.left_rec <- Left_rec
  else
    p1.left_rec <- Non_rec;
  Ahash.clear tbl;
  let parse =
    fun grouped str pos next g ->
    if p1.left_rec = Left_rec then
      try
	(*	if !debug_leftrec then Printf.eprintf "trying to pop %d in %a...\n%!" p1.ident print_stack grouped.stack;*)
	let v, stack = pop_stack p1.ident grouped.stack in
	(*	if !debug_leftrec then Printf.eprintf "pop ok\n%!";*)
	g str pos str pos str pos stack v
      with Not_found -> try
		       (*	 if !debug_leftrec then Printf.eprintf "check_rec_info %d %a ...%!" p1.ident print_stack grouped.stack;*)
	check_rec_info p1.ident str pos grouped;
	raise Error
      with Not_found ->
        let grouped = push_rec_info p1.ident str pos p1.charset_after_left grouped in
	(*	if !debug_leftrec then Printf.eprintf " failed %a\n%!" print_stack grouped.stack;*)
	let (_,after_accepted_char,syms) =
	  try
	    List.assoc p1.ident p1.charset_after_left
	  with Not_found -> assert false
	in
	let next' =
	  { accepted_char = union next.accepted_char after_accepted_char;
	    first_syms = Node(syms, next.first_syms);
	  }
	in
	(*	if !debug_leftrec then Printf.eprintf "next' = %a\n%!" print_charset next'.accepted_char;*)

	let rec fn str' pos' str'' pos'' stack v =
 	  (*if !debug_leftrec then Printf.eprintf "PUSH W %d in %a\n%!" p1.ident print_stack stack;*)
	  let grouped' = {
	    grouped with
	    leftrecs = [];
	    stack = Pushed(p1.ident, Obj.repr v, stack) } in
	  if test grouped next str'' pos'' then
	    try
              (*if !debug_leftrec then Printf.eprintf "calling in try %d %a\n%!" p1.ident print_stack grouped'.stack;*)
	      p2.parse grouped' str'' pos'' next'
		       (fun _ _ str0' pos0' str0'' pos0'' stack v ->
			(*if !debug_leftrec then Printf.eprintf "call continuation in try %d %a\n%!" p1.ident print_stack stack;*)
			fn str0' pos0' str0'' pos0'' stack v)
	    with Error ->
              (*if !debug_leftrec then Printf.eprintf "capturing Error %d %a\n%!" p1.ident print_stack stack;*)
	      g str pos str' pos' str'' pos'' stack v
	  else (
            (*if !debug_leftrec then Printf.eprintf "calling without try %d %a\n%!" p1.ident print_stack grouped'.stack;*)
	    p2.parse grouped' str'' pos'' next'
		     (fun _ _ str0' pos0' str0'' pos0'' stack v ->
		      (*if !debug_leftrec then Printf.eprintf "call continuation without try %d %a\n%!" p1.ident print_stack stack;*)
		      fn str0' pos0' str0'' pos0'' stack v))
	in
	(*if !debug_leftrec then Printf.eprintf "initial calling %d\n%!" p1.ident;*)
	p2.parse grouped str pos next'
		 (fun _ _ str' pos' str'' pos'' stack v ->
		  (*if !debug_leftrec then Printf.eprintf "initial call continuation %d %a\n%!" p1.ident print_stack stack;*)
		  fn str' pos' str'' pos'' stack v)
		 
    else p2.parse grouped str pos next g
  in
  p1.parse <- parse;
  p1.set_info <- (fun () -> 
  		  p1.firsts <- p2.firsts;
		  p1.first_sym <- p2.first_sym;
		  p1.charset_after_left <- sum_charset_after_left p1.charset_after_left p2;
		  p1.accept_empty <- p2.accept_empty);
  add_deps p1 p2;
  update (cast p1)
(*  Printf.eprintf "%d: accept empty = %s\n%!" p1.ident (match p1.accept_empty with
					       Unknown -> "Unknown" | Non_empty -> "Non_empty" | _ -> "May_empty")*)

let grammar_family ?(param_to_string=fun _ -> "<param>") name =
  let tbl = Ahash.create 101 in
  let definition = ref None in
  let seeds = ref [] in
  let record p = seeds := p::!seeds in
  let do_fix fn =
    while !seeds <> [] do
      let new_seeds = !seeds in
      seeds := [];
      List.iter (fun key ->
		 let g = Ahash.find tbl key in
		 set_grammar g (fn key)) new_seeds;
    done;
  in
  let gn = fun param ->
    try Ahash.find tbl param
    with Not_found ->
      record param;
      let g = declare_grammar (name ^ ":" ^ (param_to_string param)) in
      Ahash.add tbl param g;
      (match !definition with
       | Some f ->
          do_fix f;
       | None -> ());
      g
  in gn,
  (fun fn ->
   do_fix fn;
   definition := Some fn)

let imit_deps l = match l.deps with None -> None | Some _ -> Some (TG.create 7)

let case_empty e f =
  match e with
  | May_empty x -> May_empty(f x)
  | Non_empty -> Non_empty
  | Unknown -> Unknown

let case_empty2_and e1 e2 f =
  match e1.accept_empty, e2.accept_empty with
  | (Non_empty, _) | (_, Non_empty) -> Non_empty
  | May_empty x, May_empty y -> May_empty(f x y)
  | _ -> Unknown

let case_empty2_or e1 e2 f =
  match e1, e2.accept_empty with
  | (May_empty x, _) | (_, May_empty x) -> May_empty x
  | (Unknown, _) | (_, Unknown) -> Unknown
  | _ -> Non_empty
			      
let apply : 'a 'b.('a -> 'b) -> 'a grammar -> 'b grammar
  = fun f l ->
  let res = {
      ident = new_ident ();
      firsts = firsts l;
      first_sym = first_sym l;
      accept_empty = case_empty l.accept_empty (fun x str pos -> f (x str pos));
      left_rec = Non_rec;
      charset_after_left = l.charset_after_left;
      deps = imit_deps l;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
        l.parse grouped str pos next 
	   (fun l c l' c' l'' c'' stack x -> 
	    let r = try f x with Give_up msg -> parse_error grouped (~!msg) l' c' in
	    g l c l' c' l'' c'' stack r);
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l;
    res.first_sym <- first_sym l;
    res.charset_after_left <- l.charset_after_left;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos -> f (x str pos)));
  add_deps res l;
  res

let delim : 'a grammar -> 'a grammar
  = fun l ->
  let res =
    { ident = new_ident ();
      firsts = firsts l;
      first_sym = first_sym l;
      left_rec = Non_rec;
      charset_after_left = l.charset_after_left;
      accept_empty = l.accept_empty;
      deps = imit_deps l;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
        let cont l c l' c' l'' c'' stack x () = g l c l' c' l'' c'' stack x in
        l.parse grouped str pos next cont ()
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l;
    res.first_sym <- first_sym l;
    res.accept_empty <- l.accept_empty);
  add_deps res l;
  res

let merge : 'a 'b.('a -> 'b) -> ('b -> 'b -> 'b) -> 'a grammar -> 'b grammar
  = fun unit merge l ->
  let res =
    { ident = new_ident ();
      firsts = firsts l;
      first_sym = first_sym l;
      left_rec = Non_rec;
      charset_after_left = l.charset_after_left;
      accept_empty = case_empty l.accept_empty (fun x str pos -> unit (x str pos));
      deps = imit_deps l;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
        let m = ref PosMap.empty in
        let cont l c l' c' l'' c'' stack x =
	  let x = try unit x with Give_up msg -> parse_error grouped (~!msg) l' c' in
          if stack = EmptyStack then (try
              let (_,_,_,_,old) = PosMap.find (l'', c'') !m in
	      let r = try merge x old with Give_up msg -> parse_error grouped (~!msg) l' c' in
              m := PosMap.add (l'', c'') (l, c, l', c', r) !m
            with Not_found ->
              m := PosMap.add (l'', c'') (l, c, l', c', x) !m);
          raise Error
        in
        try
          ignore (l.parse grouped str pos next cont);
          assert false
        with Error ->
          let res = ref None in
          PosMap.iter (fun (str'',pos'') (str, pos, str', pos', x) ->
                       try
                         res := Some (g str pos str' pos' str'' pos'' EmptyStack x);
                         raise Exit
                       with
                         Error | Exit -> ()) !m;
          match !res with
            None -> raise Error
          | Some x -> x
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l;
    res.first_sym <- first_sym l;
    res.charset_after_left <- l.charset_after_left;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos -> unit (x str pos)));
  add_deps res l;
  res

let lists : 'a grammar -> 'a list grammar =
  fun gr -> merge (fun x -> [x]) (@) gr

let position : 'a grammar -> (string * int * int * int * int * 'a) grammar
  = fun l ->
   let res =
     { ident = new_ident ();
       firsts = firsts l;
       first_sym = first_sym l;
       left_rec = Non_rec;
       charset_after_left = l.charset_after_left;
       accept_empty = case_empty l.accept_empty (fun x str pos -> let l = line_num str in
								  fname str, l, pos, l, pos,x str pos);
      deps = imit_deps l;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          l.parse grouped str pos next (fun l c l' c' l'' c'' stack x ->
                                          g l c l' c' l'' c'' stack (
                                              (fname l, line_num l, c, line_num l', c', x)))
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l;
    res.first_sym <- first_sym l;
    res.charset_after_left <- l.charset_after_left;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos ->
						   let l = line_num str in
						   fname str, l, pos, l, pos,x str pos));
  add_deps res l;
  res

let apply_position : ('a -> buffer -> int -> buffer -> int -> 'b) -> 'a grammar -> 'b grammar
  = fun f l ->
   let res =
     { ident = new_ident ();
       firsts = firsts l;
      first_sym = first_sym l;
      left_rec = Non_rec;
      charset_after_left = l.charset_after_left;
      accept_empty = case_empty l.accept_empty (fun x str pos ->
						f (x str pos) str pos str pos);
      deps = imit_deps l;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          l.parse grouped str pos next
                  (fun l c l' c' l'' c'' stack x -> 
		   let r = try f x l c l' c' with Give_up msg -> parse_error grouped (~!msg) l' c' in
		   g l c l' c' l'' c'' stack r)
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l;
    res.first_sym <- first_sym l;
    res.charset_after_left <- l.charset_after_left;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos -> f (x str pos) str 0 str 0));
  add_deps res l;
  res

let eof : 'a -> 'a grammar
  = fun a ->
    let set = singleton '\255' in
    { ident = new_ident ();
      firsts = set;
      first_sym = ~~ "EOF";
      accept_empty = Non_empty;
      left_rec = Non_rec;
      charset_after_left = [];
      set_info = (fun () -> ());
      deps = None;
      parse =
        fun grouped str pos next g ->
	  test_clear grouped;
          if is_empty str pos then g str pos str pos str pos grouped.stack a else parse_error grouped (~~ "EOF") str pos
    }

let empty : 'a -> 'a grammar = fun a ->
  { ident = new_ident ();
    firsts = empty_charset;
    first_sym = Empty;
    accept_empty = May_empty (fun _ _ -> a);
    charset_after_left = [];
    left_rec = Non_rec;
    set_info = (fun () -> ());
    deps = None;
    parse = fun grouped str pos next g -> raise Error (* FIXME: parse_error ? *)
  }

let active_debug = ref true
		       
let debug : string -> unit grammar = fun msg ->
  { ident = new_ident ();
    firsts = empty_charset;
    first_sym = Empty;
    accept_empty = May_empty (fun _ _ -> ());
    left_rec = Non_rec;
    charset_after_left = [];
    deps = None;
    set_info = (fun () -> ());
    parse = fun grouped str pos next g ->
	    if !active_debug then
	      begin
		let l = line str in
		let current = try String.sub l pos (min (String.length l - pos) 10) with _ -> "???" in
		Printf.eprintf "%s(%d,%d): %S %a %a\n%!" msg (line_num str) pos current print_charset next.accepted_char print_info grouped;
	      end;
	    raise Error} (* FIXME: parse_error ? *)

let fail : string -> 'a grammar = fun msg ->
  { ident = new_ident ();
    firsts = empty_charset;
    first_sym = Empty;
    accept_empty = Non_empty;
    left_rec = Non_rec;
    charset_after_left = [];
    deps = None;
    set_info = (fun () -> ());
    parse = fun grouped str pos next g ->
             parse_error grouped (~~ msg) str pos }

let  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> 'a option -> string -> 'a grammar =
  (fun fn set empty name ->
   { ident = new_ident ();
     firsts = set;
     first_sym = ~~ name;
     accept_empty = (match empty with None -> Non_empty | Some a -> May_empty (fun _ _ -> a));
     left_rec = Non_rec;
     charset_after_left = [];
     deps = None;
     set_info = (fun () -> ());
     parse = fun grouped str pos next g ->
             let a, str', pos' = try fn str pos with Give_up msg -> parse_error grouped (~! msg) str pos in
	     if str' == str && pos' == pos then raise Error;
             let str'', pos'' = apply_blank grouped str' pos' in
	     if str == str' && pos == pos' then raise Error;
	     test_clear grouped;
             g str pos str' pos' str'' pos'' grouped.stack a })

let char : char -> 'a -> 'a grammar
  = fun s a ->
    let set = singleton s in
    let s' = String.make 1 s in
    { ident = new_ident ();
      firsts = set;
      first_sym = ~~ s';
      accept_empty = Non_empty;
      left_rec = Non_rec;
      charset_after_left = [];
      deps = None;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
	  test_clear grouped;
          let c, str', pos' = read str pos in
          if c <> s then parse_error grouped (~~ s') str pos;
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' grouped.stack a
    }

let any : char grammar
  = let set = del full_charset '\255' in
    { ident = new_ident ();
      firsts = set;
      first_sym = ~~ "ANY";
      accept_empty = Non_empty;
      left_rec = Non_rec;
      charset_after_left = [];
      deps = None;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
	  test_clear grouped;
          let c, str', pos' = read str pos in
          if c = '\255' then parse_error grouped (~~ "ANY") str pos;
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' grouped.stack c
    }

let string : string -> 'a -> 'a grammar
  = fun s a ->
   let len_s = String.length s in
    let set = if len_s > 0 then singleton s.[0] else empty_charset in
    { ident = new_ident ();
      firsts = set;
      first_sym = if len_s > 0 then (~~ s) else Empty;
      accept_empty = if (len_s = 0) then May_empty (fun str pos -> a) else Non_empty;
      left_rec = Non_rec;
      charset_after_left = [];
      deps = None;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
	  if len_s = 0 then raise Error;
	  test_clear grouped;
          let str' = ref str in
          let pos' = ref pos in
          for i = 0 to len_s - 1 do
            let c, _str', _pos' = read !str' !pos' in
            if c <> s.[i] then parse_error grouped (~~s) str pos;
            str' := _str'; pos' := _pos'
          done;
          let str' = !str' and pos' = !pos' in
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' grouped.stack a
    }

let regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a grammar
  = fun r0 ?(name=String.escaped r0) a ->
    let r = Str.regexp r0 in
    let set = Charset.copy empty_charset in
    let found = ref false in
    for i = 0 to 254 do
      let s = String.make 1 (Char.chr i) in
      if Str.string_partial_match r s 0 && Str.match_end () > 0 then
        (found := true; addq set (Char.chr i))
    done;
    if not !found then failwith "regexp: illegal empty regexp";
    { ident = new_ident ();
      firsts = set;
      first_sym = ~~ name;
      accept_empty = if (Str.string_match r "" 0) then May_empty (fun _ _ -> let f n = "" in a f) else Non_empty ;
      charset_after_left = [];
      left_rec = Non_rec;
      deps = None;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          let l = line str in
          if pos > String.length l then
            parse_error grouped (~~ name) str pos;
          if string_match r l pos then (
            let f n = matched_group n l in
            let pos' = match_end () in
	    if pos' = pos then raise Error;
	    test_clear grouped;
	    let res = try a f with Give_up msg -> parse_error grouped (~!msg) str pos' in
            let str'', pos'' = apply_blank grouped str pos' in
            g str pos str pos' str'' pos'' grouped.stack res
          ) else (
            parse_error grouped (~~ name) str pos)
    }

let imit_deps_seq l1 l2 =
  if accept_empty l1 then
    match l1.deps, l2.deps with None, None -> None | _ -> Some (TG.create 7)
  else
    imit_deps l1

let tryif cond f g =
  if cond then
    try f () with Error -> g ()
  else
    f ()

let sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 l2 f ->
   let res =
     { ident = new_ident ();
       firsts = union_firsts l1 l2;
      first_sym = union_first_sym l1 l2;
      accept_empty = case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos));
      charset_after_left = compose_charset_after_left l1 l2;
      left_rec = Non_rec;
      set_info = (fun () -> ());
      deps = imit_deps_seq l1 l2;
      parse =
        fun grouped str pos next g ->
	let next' = union' l2 next in
        tryif (accept_empty l1 && test grouped next' str pos)
	  (fun () -> l1.parse grouped str pos next'
                     (fun str pos str' pos' str'' pos'' stack a ->
		      let grouped = set_stack_and_reset grouped stack in
                      tryif (accept_empty l2 && test grouped next str'' pos'')
			    (fun () -> l2.parse grouped str'' pos'' next
						(fun _ _ str' pos' str'' pos'' stack x ->
						 let res = try f a x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
						 g str pos str' pos' str'' pos'' stack res))
			    (fun () -> let x = read_empty l2 in
				       let res = try f a (x str' pos') with Give_up msg -> parse_error grouped (~!msg) str' pos' in
				       g str pos str' pos' str'' pos'' stack res)))
	  (fun () ->  let a = read_empty l1 in
		      l2.parse grouped str pos next
			       (fun str pos str' pos' str'' pos'' stack x ->
				let res = try f (a str pos) x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
				g str pos str' pos' str'' pos'' stack res))
     } in
    res.set_info <- (fun () ->
		   res.firsts <- union_firsts l1 l2;
		   res.first_sym <- union_first_sym l1 l2;
		   res.accept_empty <- case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos));
		   res.charset_after_left <- compose_charset_after_left l1 l2);
    add_deps res l1;
    (* FIXME: if accept_empty l1 becomes false, this dependency could be removed *)
    if accept_empty l1 then add_deps res l2;
  res

let sequence_position : 'a grammar -> 'b grammar -> ('a -> 'b -> buffer -> int -> buffer -> int -> 'c) -> 'c grammar
  = fun l1 l2 f ->
   let res =
     { ident = new_ident ();
       firsts = union_firsts l1 l2;
      first_sym = union_first_sym l1 l2;
      accept_empty = case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos) str pos str pos);
      charset_after_left = compose_charset_after_left l1 l2;
      left_rec = Non_rec;
      set_info = (fun () -> ());
      deps = imit_deps_seq l1 l2;
      parse =
        fun grouped str pos next g ->
	  let next' = union' l2 next in
          tryif (accept_empty l1 && test grouped next' str pos)
          (fun () -> l1.parse grouped str pos (union' l2 next)
                   (fun str pos str' pos' str'' pos'' stack a ->
		    let grouped = set_stack_and_reset grouped stack in
                    tryif (accept_empty l2 && test grouped next str'' pos'')
		     (fun () -> l2.parse grouped str'' pos'' next
                             (fun _ _ str' pos' str'' pos'' stack b ->
			      let res = try f a b str pos str' pos' with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                              g str pos str' pos' str'' pos'' stack res))
		     (fun () -> let b = read_empty l2 in
			      let res = try f a (b str' pos') str pos str' pos' with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                              g str pos str' pos' str'' pos'' stack res)))
	  (fun () -> let a = read_empty l1 in
		    l2.parse grouped str pos next
                             (fun str pos str' pos' str'' pos'' stack b ->
			      let res = try f (a str pos) b str pos str' pos' with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                              g str pos str' pos' str'' pos'' stack res))
	       
    } in
    res.set_info <- (fun () ->
		   res.firsts <- union_firsts l1 l2;
		   res.first_sym <- union_first_sym l1 l2;
		   res.accept_empty <- case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos) str pos str pos);
		   res.charset_after_left <- compose_charset_after_left l1 l2);
    add_deps res l1;
    (* FIXME: if accept_empty l1 becomes false, this dependency could be removed *)
    if accept_empty l1 then add_deps res l2;
  res

let fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence l1 l2 (fun x f -> f x)

let fsequence_position : 'a grammar -> ('a -> buffer -> int -> buffer -> int -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence_position l1 l2 (fun x f -> f x)

let sequence3 : 'a grammar -> 'b grammar -> 'c grammar -> ('a -> 'b -> 'c -> 'd) -> 'd grammar
  = fun l1 l2 l3 g ->
    sequence (sequence l1 l2 (fun x y z -> g x y z)) l3 (fun f -> f)

let dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun l1 f2 ->
  if l1.accept_empty <> Non_empty && l1.accept_empty <> Unknown then
    failwith "dependant sequence with empty prefix are not supported";
  let res =
    { ident = new_ident ();
      firsts = firsts l1;
      first_sym = first_sym l1;
      accept_empty = Non_empty;
      charset_after_left = l1.charset_after_left; (* FIXME: to check *)
      left_rec = Non_rec;
      set_info = (fun () -> ());
      deps = imit_deps l1;
      parse =
        fun grouped str pos next g ->
	l1.parse grouped str pos all_next
                 (fun str pos str' pos' str'' pos'' stack a ->
		  let grouped = set_stack_and_reset grouped stack in
		  let g2 = try f2 a with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                  tryif (accept_empty g2 && test grouped next str'' pos'')
			(fun () -> g2.parse grouped str'' pos'' next
					    (fun _ _ str' pos' str'' pos'' stack b ->
					     g str pos str' pos' str'' pos'' stack b))
			(fun () -> let b = read_empty g2 in
				   g str pos str' pos' str'' pos'' stack (b str' pos')))
    }
  in
  res.set_info <- (fun () ->
		   if l1.accept_empty <> Non_empty && l1.accept_empty <> Unknown then
		     failwith "dependant sequence with empty prefix are not supported";
		   res.firsts <- firsts l1;
		   res.charset_after_left <- l1.charset_after_left; (* FIXME: to check *)
		   res.first_sym <- first_sym l1);
  add_deps res l1;
  res

let iter : 'a grammar grammar -> 'a grammar
  = fun g -> dependent_sequence g (fun x -> x)

let change_layout : ?new_blank_before:bool -> ?old_blank_after:bool -> 'a grammar -> blank -> 'a grammar
  = fun ?(new_blank_before=true) ?(old_blank_after=true) l1 blank1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
   let res =
     { ident = new_ident ();
       firsts = firsts l1;
      first_sym = first_sym l1;
      accept_empty = l1.accept_empty;
      charset_after_left = l1.charset_after_left;
      left_rec = Non_rec;
      deps = imit_deps l1;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
        let grouped' = { grouped with blank = blank1 } in
            let str, pos = if new_blank_before then apply_blank grouped' str pos else str, pos in
          l1.parse grouped' str pos all_next
                   (if old_blank_after then
                     (fun str pos str' pos' str'' pos'' x ->
                      let str'', pos'' = apply_blank grouped str'' pos'' in
                      g str pos str' pos' str'' pos'' x)
                   else g)
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l1;
    res.first_sym <- first_sym l1;
    res.charset_after_left <- l1.charset_after_left;
     res.accept_empty <- l1.accept_empty);
  add_deps res l1;
  res

let ignore_next_blank : 'a grammar -> 'a grammar
  = fun l1 ->
   let res =
     { ident = new_ident ();
       firsts = firsts l1;
      first_sym = first_sym l1;
      accept_empty = l1.accept_empty;
      charset_after_left = l1.charset_after_left;
      left_rec = Non_rec;
      deps = imit_deps l1;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          l1.parse grouped str pos all_next (fun s p s' p' s'' p'' -> g s p s' p' s' p')
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l1;
    res.first_sym <- first_sym l1;
    res.charset_after_left <- l1.charset_after_left;
    res.accept_empty <- l1.accept_empty);
  add_deps res l1;
  res

let option : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
  let res =
    { ident = new_ident ();
      firsts = firsts l;
      first_sym = first_sym l;
      accept_empty = May_empty (fun _ _ -> a);
      charset_after_left = l.charset_after_left; 
      left_rec = Non_rec;
      deps = imit_deps l;
      set_info = (fun () -> ());
      parse = l.parse
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l;
     res.charset_after_left <- l.charset_after_left;
     res.first_sym <- first_sym l);
  add_deps res l;
  res

let option' : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
  let res =
    { ident = new_ident ();
      firsts = firsts l;
      first_sym = first_sym l;
      accept_empty = May_empty (fun _ _ -> a);
      charset_after_left = l.charset_after_left; 
      left_rec = Non_rec;
      deps = imit_deps l;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
            l.parse grouped str pos next
                    (fun s p s' p' s'' p'' stack x () -> g s p s' p' s'' p'' stack x) ()
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts l;
    res.charset_after_left <- l.charset_after_left; 
    res.first_sym <- first_sym l);
  add_deps res l;
  res

let fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
  let res =
    { ident = new_ident ();
      firsts = firsts f1;
      first_sym = first_sym f1;
      accept_empty = May_empty (fun _ _ -> a);
      charset_after_left = f1.charset_after_left; 
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          let next' = union'' f1 next in
          let rec fn str' pos' str'' pos'' grouped x =
            if test grouped next str'' pos'' then
              try
                f1.parse grouped str'' pos'' next'
                         (fun _ _  str' pos' str'' pos'' stack f ->
			  let grouped = set_stack grouped stack in
			  let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                          fn str' pos' str'' pos'' grouped res)
              with
              | Error -> g str pos str' pos' str'' pos'' grouped.stack x
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' stack f ->
			let grouped = set_stack grouped stack in
			let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                        fn str' pos' str'' pos'' grouped res)
          in f1.parse grouped str pos next'
                      (fun _ _  str' pos' str'' pos'' stack f ->
		          let grouped = set_stack grouped stack in
			  let res = try f a with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                          fn str' pos' str'' pos'' grouped res)
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts f1;
    res.charset_after_left <- f1.charset_after_left; 
    res.first_sym <- first_sym f1);
  add_deps res f1;
  res

let fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
   let res =
     { ident = new_ident ();
       firsts = firsts f1;
      first_sym = first_sym f1;
      accept_empty = May_empty (fun _ _ -> a); 
      charset_after_left = f1.charset_after_left; 
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          let next' = union'' f1 next in
          let rec fn str' pos' str'' pos'' grouped x () =
            if test grouped next str'' pos'' then
              (try
                  f1.parse grouped str'' pos'' next'
                           (fun _ _ str' pos' str'' pos'' stack f ->
			    let grouped = set_stack grouped stack in
			    let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                            fn str' pos' str'' pos'' grouped res)
              with
              | Error -> fun () -> g str pos str' pos' str'' pos'' grouped.stack x) ()
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' stack f ->
			let grouped = set_stack grouped stack in
			let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                        fn str' pos' str'' pos'' grouped res) ()
          in
          f1.parse grouped str pos next'
                   (fun _ _ str' pos' str'' pos'' stack f ->
		    let grouped = set_stack grouped stack in
		    let res = try f a with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                    fn str' pos' str'' pos'' grouped res) ()
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts f1;
    res.charset_after_left <- f1.charset_after_left; 
    res.first_sym <- first_sym f1);
  add_deps res f1;
  res

let fixpoint1 : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
  let res =
    { ident = new_ident ();
      firsts = firsts f1;
      first_sym = first_sym f1;
      accept_empty = case_empty f1.accept_empty (fun f str pos -> f str pos a);
      charset_after_left = f1.charset_after_left; 
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          let next' = union'' f1 next in
          let rec fn str' pos' str'' pos'' grouped x =
            if test grouped next str'' pos'' then
              try
                f1.parse grouped str'' pos'' next'
                         (fun _ _ str' pos' str'' pos'' stack f ->
			  let grouped = set_stack grouped stack in
			  let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                          fn str' pos' str'' pos'' grouped res)
              with
              | Error -> g str pos str' pos' str'' pos'' grouped.stack x
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' stack f ->
			   let grouped = set_stack grouped stack in
			   let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                           fn str' pos' str'' pos'' grouped res)
          in f1.parse grouped str pos next'
                      (fun _ _  str' pos' str'' pos'' stack f ->
		          let grouped = set_stack grouped stack in
			  let res = try f a with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                          fn str' pos' str'' pos'' grouped res)
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts f1;
    res.accept_empty <- case_empty f1.accept_empty (fun f str pos -> f str pos a);
    res.charset_after_left <- f1.charset_after_left; 
    res.first_sym <- first_sym f1);
  add_deps res f1;
  res


let fixpoint1' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
  let res =
    { ident = new_ident ();
      firsts = firsts f1;
      first_sym = first_sym f1;
      accept_empty = case_empty f1.accept_empty (fun f str pos -> f str pos a);
      charset_after_left = f1.charset_after_left; 
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      parse =
        fun grouped str pos next g ->
          let next' = union'' f1 next in
          let rec fn str' pos' str'' pos'' grouped x () =
            if test grouped next str'' pos'' then
              (try
                  f1.parse grouped str'' pos'' next'
                           (fun _ _ str' pos' str'' pos'' stack f ->
			    let grouped = set_stack grouped stack in
			    let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                            fn str' pos' str'' pos'' grouped res)
              with
              | Error -> fun () -> g str pos str' pos' str'' pos'' grouped.stack x) ()
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' stack f ->
			let grouped = set_stack grouped stack in
			let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                        fn str' pos' str'' pos'' grouped res) ()
          in
          f1.parse grouped str pos next'
                   (fun _ _ str' pos' str'' pos'' stack f ->
		    let grouped = set_stack grouped stack in
		    let res = try f a with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                    fn str' pos' str'' pos'' grouped res) ()
    }
  in
  res.set_info <- (fun () ->
    res.firsts <- firsts f1;
    res.accept_empty <- case_empty f1.accept_empty (fun f str pos -> f str pos a);
    res.charset_after_left <- f1.charset_after_left; 
    res.first_sym <- first_sym f1);
  add_deps res f1;
  res

let alternatives : 'a grammar list -> 'a grammar
  = fun ls ->
  let res =
    { ident = new_ident ();
      firsts = List.fold_left (fun s p -> union s (firsts p)) empty_charset ls;
    first_sym = List.fold_left (fun s p -> s @@ (first_sym p)) Empty ls;
    accept_empty = List.fold_left (fun s p -> match s, p.accept_empty with
						(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
						| _ -> Non_empty) Non_empty ls;
    charset_after_left = List.fold_left (fun s p -> sum_charset_after_left s p) [] ls;
    deps = if List.exists (fun l -> l.deps <> None) ls then Some (TG.create 7) else None;
    set_info = (fun () -> ());
    left_rec = Non_rec;
    parse =
      fun grouped str pos next g ->
          let empty_ok = test grouped next str pos in
          let ls = List.filter (fun g ->
                                (empty_ok && accept_empty g) ||
                                  test grouped (next_sym' grouped g) str pos) ls in
	  if ls = [] then raise Error else
          let rec fn = function
            | [l] ->
	       l.parse grouped str pos next g
            | l::ls ->
              (try
                l.parse grouped str pos next g
              with
                Error -> fn ls)
	    | _ -> assert false
          in
          fn ls
    }
  in
  res.set_info <- (fun () ->
    res.firsts <-
      List.fold_left (fun s p -> union s (firsts p)) empty_charset ls;
    res.first_sym <- 
      List.fold_left (fun s p -> s @@ (first_sym p)) Empty ls;
    res.accept_empty <-
      List.fold_left (fun s p ->
		      match s, p.accept_empty with
			(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
			| _ -> Non_empty) Non_empty ls;
    res.charset_after_left <-
      List.fold_left (fun s p -> sum_charset_after_left s p) [] ls);
  List.iter (fun l -> add_deps res l) ls;
  res

let alternatives' : 'a grammar list -> 'a grammar
  = fun ls ->
  let res =
    { ident = new_ident ();
      firsts = List.fold_left (fun s p -> union s (firsts p)) empty_charset ls;
    first_sym = List.fold_left (fun s p -> s @@ (first_sym p)) Empty ls;
    accept_empty = List.fold_left (fun s p -> match s, p.accept_empty with
						(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
						| _ -> Non_empty) Non_empty ls;
    charset_after_left = List.fold_left (fun s p -> sum_charset_after_left s p) [] ls;
    left_rec = Non_rec;
    deps = if List.exists (fun l -> l.deps <> None) ls then Some (TG.create 7) else None;
    set_info = (fun () -> ());
    parse =
        fun grouped str pos next g ->
          let empty_ok = test grouped next str pos in
          let ls = List.filter (fun g ->
                                (empty_ok && accept_empty g) ||
                                  test grouped (next_sym' grouped g) str pos) ls in
	  if ls = [] then raise Error else
          let rec fn = function
            | [l] ->
                l.parse grouped str pos next
                        (fun s p s' p' s'' p'' stack x () ->  g s p s' p' s'' p'' stack x)
            | l::ls ->
              (try
                l.parse grouped str pos next
                        (fun s p s' p' s'' p'' stack x () ->  g s p s' p' s'' p'' stack x)
              with
                Error -> fn ls)
	    | _ -> assert false
          in
          fn ls ()
    }
  in
  res.set_info <- (fun () ->
    res.firsts <-
      List.fold_left (fun s p -> union s (firsts p)) empty_charset ls;
    res.first_sym <- 
      List.fold_left (fun s p -> s @@ (first_sym p)) Empty ls;
    res.accept_empty <- List.fold_left (fun s p -> match s, p.accept_empty with
						(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
						| _ -> Non_empty) Non_empty ls;
    res.charset_after_left <- List.fold_left (fun s p -> sum_charset_after_left s p) [] ls);
  List.iter (fun l -> add_deps res l) ls;
  res

let parse_buffer grammar blank str =
  let grammar = sequence grammar (eof ()) (fun x _ -> x) in
  let grouped = { blank;
                  err_info = {max_err_pos = -1;
                              max_err_buf = str;
                              max_err_col = -1;
                              err_msgs = Empty };
		  stack = EmptyStack;
		  leftrecs = [];
                }
  in
  let str, pos = apply_blank grouped str 0 in
  try
      grammar.parse grouped str pos all_next (fun _ _ _ _ _ _ _ x -> x)
    with Error ->
      let str = grouped.err_info.max_err_buf in
      let pos = grouped.err_info.max_err_col in
      let msgs = grouped.err_info.err_msgs in
      let msg, expected = collect_tree msgs in
        raise (Parse_error (fname str, line_num str, pos, msg, expected))

let partial_parse_buffer grammar blank str pos =
  let grouped = { blank;
                  err_info = {max_err_pos = -1;
                              max_err_buf = str;
                              max_err_col = -1;
                              err_msgs = Empty };
		  stack = EmptyStack;
		  leftrecs = [];
                }
  in
  let cont l c l' c' l'' c'' _ x = (l'',c'',x) in
  let str, pos = apply_blank grouped str pos in
  try
    grammar.parse grouped str pos all_next cont;
  with Error ->
    let str = grouped.err_info.max_err_buf in
    let pos = grouped.err_info.max_err_col in
    let msgs = grouped.err_info.err_msgs in
    let msg, expected = collect_tree msgs in
    raise (Parse_error (fname str, line_num str, pos, msg, expected))

let partial_parse_string ?(filename="") grammar blank str =
  let str = buffer_from_string ~filename str in
  partial_parse_buffer grammar blank str

let parse_string ?(filename="") grammar blank str =
  let str = buffer_from_string ~filename str in
  parse_buffer grammar blank str

let parse_channel ?(filename="") grammar blank ic  =
  let str = buffer_from_channel ~filename ic in
  parse_buffer grammar blank str

let parse_file grammar blank filename  =
  let str = buffer_from_file filename in
  parse_buffer grammar blank str

let print_exception = function
  | Parse_error(fname,l,n,msg, expected) ->
     let expected = 
       if expected = [] then "" else 
	 Printf.sprintf "'%s' expected" (String.concat "|" expected)
     in
     let msg = if msg = [] then "" else (String.concat "," msg)
     in
     let sep = if msg <> "" && expected <> "" then ", " else "" in
     Printf.eprintf "%s: parse error after %d:%d, %s%s%s\n%!" fname l n msg sep expected 
  | _ -> assert false

let handle_exception f a =
  try
    f a
  with
    Parse_error _ as e -> print_exception e; exit 1

let blank_grammar grammar blank str pos =
  let (str, pos, _) = partial_parse_buffer grammar blank str pos in
  (str, pos)
