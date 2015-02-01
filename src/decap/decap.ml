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

type stack' = EmptyStack | Pushed of int * Obj.t

type stack = int list * stack'

let print_stack ch (l, s) =
  Printf.fprintf ch "<<%a>> " (fun ch l ->
			      List.iter (Printf.fprintf ch "%d") l) l;
  match s with
  | Pushed(n,_) -> Printf.fprintf ch "W %d" n
  | EmptyStack -> ()

let pop_stack n = function
  | l, Pushed(n',v) when n = n' -> Obj.obj v, (l, EmptyStack)
  | _ -> raise Not_found

type grouped = {
  blank: blank;
  err_info: err_info;
  stack : stack;
}

let rec test_clear grouped =
  match grouped.stack with
  | _, Pushed(_,_) -> raise Error
  | l', EmptyStack -> [], EmptyStack

let print_info ch grouped =
  Printf.fprintf ch "%a" print_stack grouped.stack

let set_stack grouped stack =
  if grouped.stack == stack then grouped else { grouped with stack; }

(* if a grammar has a prefix which is a left recursive grammar with ident n,
      (n, b, s) is in the list and
      - b = true if there may be nothing after the prefix
      - s is the character set accepted after the prefix *)
type next_after_prefix = (int * (bool * charset * string_tree)) list

let rec eq_next_after_prefix c1 c2 = match c1, c2 with
  | [], [] -> true
  | (n1,(b1,s1,_))::c1,(n2,(b2,s2,_))::c2 ->
     b1 = b2 && s1 = s2 && n1 = n2 && eq_next_after_prefix c1 c2
  | _ -> false

type next = {
  accepted_char: charset;
  first_syms : string_tree;
  next_after_prefix : next_after_prefix;
}

let all_next =
  { accepted_char = full_charset;
    first_syms = Empty;
    next_after_prefix = []; (* this is incorrect ... FIXME *)
  }

let print_char_set_after_left ch l =
  List.iter (fun (id,(b,first,_)) ->
	     Printf.fprintf ch "(%d, %b, %a) " id b print_charset first) l

let print_next ch n =
  Printf.fprintf ch "%a [%a]" print_charset n.accepted_char print_char_set_after_left n.next_after_prefix

type ('a, 'b) continuation = buffer -> int -> buffer -> int -> buffer -> int -> stack -> 'a -> 'b

type empty_type = R of empty_type

type 'a accept_empty =
    Non_empty
  | Unknown (* during building of left recursion *)
  | May_empty of (buffer -> int -> 'a)

let print_accept_empty ch = function
    Non_empty -> Printf.fprintf ch "Non_empty"
  | Unknown -> Printf.fprintf ch "Unknown"
  | May_empty _ -> Printf.fprintf ch "May_empty"

module rec G: sig
  type 'a grammar = {
    ident : int;
    mutable def : 'a grammar option;
    mutable accept_empty : 'a accept_empty;
    mutable left_rec : left_rec;
    mutable next : next;
    mutable parse : 'b. grouped -> buffer -> int -> next -> ('a, 'b) continuation -> 'b;
    mutable set_info : unit -> unit;
    mutable deps : TG.t option;
  }
   and left_rec =
     | Non_rec
     | Left_rec of empty_type grammar list

  include Hashtbl.HashedType with type t = empty_type grammar

end = struct
  type 'a grammar = {
    ident : int;
    mutable def : 'a grammar option;
    mutable accept_empty : 'a accept_empty;
    mutable left_rec : left_rec;
    mutable next : next;
    mutable parse : 'b. grouped -> buffer -> int -> next -> ('a, 'b) continuation -> 'b;
    mutable set_info : unit -> unit;
    mutable deps : TG.t option;
  }
   and left_rec =
     | Non_rec
     | Left_rec of empty_type grammar list

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
let uncast : empty_type grammar -> 'a grammar = Obj.magic

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

let apply_blank grouped str p =
  grouped.blank str p

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

let tryif cond f g =
  if cond then
    try f () with Error -> g ()
  else
    f ()

let test grouped next str p =
  let c = get str p in
  assert (Printf.eprintf "test: %c %a %a\n%!" c print_info grouped print_next next; true);
  match grouped.stack with
  | _, EmptyStack ->
     let res = mem next.accepted_char c in
     if not res then record_error grouped next.first_syms str p;
     res
  | _, Pushed(n,_) ->
     try
       let (_,s,msg) = List.assoc n next.next_after_prefix in
       let res = mem s c in
       if not res then record_error grouped msg str p;
       res
     with
       Not_found -> false

let test_next l1 grouped str p =
  let c = get str p in
  match grouped.stack with
  | _, EmptyStack ->
     let res = mem l1.next.accepted_char c in
     if not res then record_error grouped l1.next.first_syms str p;
     res
  | _, Pushed(n,_) ->
     try
       let (_,s,msg) = List.assoc n l1.next.next_after_prefix in
       let res = mem s c in
       if not res then record_error grouped msg str p;
       res
     with
       Not_found -> false

let read_empty e =
  match e.accept_empty with
  | May_empty x -> x
  | _ -> assert false

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

let empty_next = {
  accepted_char = empty_charset;
  first_syms = Empty;
  next_after_prefix = [];
}

let sum_next_after_prefix l1 l2 =
  merge_list l1 l2 (fun (b1,c1,s1) (b2,c2,s2) -> b1 || b2, Charset.union c1 c2, s1 @@ s2)

let sum_next n1 n2 =
  {
    accepted_char = Charset.union n1.accepted_char n2.accepted_char;
    first_syms = n1.first_syms @@ n2.first_syms;
    next_after_prefix = sum_next_after_prefix n1.next_after_prefix n2.next_after_prefix
  }

let sum_nexts ls =
  List.fold_left (fun s l -> sum_next s l.next) empty_next ls

let accept_empty l1 = l1.accept_empty <> Non_empty

let compose_next l1 l2 =
  let n1 = l1.next in
  let n2 = l2.next in
  let b = accept_empty l1 in
  let res = {
    accepted_char = if b then Charset.union n1.accepted_char n2.accepted_char
		    else n1.accepted_char;
      first_syms =  if b then n1.first_syms @@ n2.first_syms
		    else n1.first_syms;
    next_after_prefix =
      let l0 =
	List.map (fun (n,(b,c,s) as x) ->
		  if b then (n,(accept_empty l2,
				Charset.union c n2.accepted_char,
				s @@ n2.first_syms))
		  else x)
		 n1.next_after_prefix
      in
      if b then sum_next_after_prefix l0 n2.next_after_prefix else l0
  }
  in
  assert (Printf.eprintf "compose %d %d %a %a => %a\n%!" l1.ident l2.ident print_next n1 print_next n2 print_next res; true);
  res

let compose_next' l1 n2 =
  let b = accept_empty l1 in
  let n1 = l1.next in
  {
    accepted_char = if b then Charset.union n1.accepted_char n2.accepted_char
		    else n1.accepted_char;
    first_syms =  if b then n1.first_syms @@ n2.first_syms
		  else n1.first_syms;
    next_after_prefix =
      let l0 =
	List.map (fun (n,(b,c,s) as x) ->
		  if b then (n,(true (* not used in fact here, could be used at the end of file only *),
				Charset.union c n2.accepted_char,
				s @@ n2.first_syms))
		  else x)
		 n1.next_after_prefix
      in
      if b then sum_next_after_prefix l0 n2.next_after_prefix else l0
  }

let not_ready name _ = failwith ("not_ready: "^name)

let declare_grammar name = let id = new_ident () in {
  ident = id;
  accept_empty = Unknown;
  next = {
    accepted_char = empty_charset;
    first_syms = Empty;
    next_after_prefix = [id,(true,empty_charset,Empty)];
  };
  left_rec = Non_rec;
  deps = Some (TG.create 7);
  set_info = (fun () -> ());
  parse = (not_ready name);
  def = None;
}

let chg_empty s1 s2 = match s1, s2 with
    Non_empty, Non_empty -> false
  | Unknown, Unknown -> false
  | May_empty _, May_empty _ -> false
  | _ -> true

let rec update (g : empty_type grammar) =
  assert (Printf.eprintf "update(1) %d: accept empty = %a, next = %a\n%!"
					g.ident print_accept_empty g.accept_empty print_next g.next; true);
  let old_accepted_char = g.next.accepted_char in
  let old_accept_empty = g.accept_empty in
  let old_next_after_prefix = g.next.next_after_prefix in
  g.set_info ();
  assert (Printf.eprintf "update(2) %d: accept empty = %a, next = %a\n%!"
			 g.ident print_accept_empty g.accept_empty print_next g.next; true);
  if (old_accepted_char <> g.next.accepted_char
      || chg_empty old_accept_empty g.accept_empty
      || not (eq_next_after_prefix old_next_after_prefix g.next.next_after_prefix))
  then match g.deps with None -> () | Some t -> TG.iter update t

let add_deps p1 p2 = match p2.deps with None -> () | Some t -> TG.add t (cast p1)

let tbl = Ahash.create 1001

let print_left_rec ch = function
  | Non_rec -> Printf.fprintf ch "NonRec"
  | Left_rec l -> Printf.fprintf ch "LeftRec(%a)" (fun ch l -> List.iter (fun x -> Printf.fprintf ch "%d " x.ident) l) l

let set_grammar p1 p2 =
  p1.def <- Some p2;
  let companion = ref [cast p1] in
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
	  if res && p.def <> None && not (List.mem p !companion) then
	    companion := p :: !companion;
	  Ahash.add tbl p.ident res;
	  res
       )
  in
  if fn (cast p1) then (
    List.iter (fun p -> p.left_rec <- Left_rec !companion) !companion)
  else
    p1.left_rec <- Non_rec;
  assert (Printf.eprintf "set_grammar p1.next = %a, p2.next = %a, left_rec = %a\n" print_next p1.next print_next p2.next print_left_rec p1.left_rec; true);
  Ahash.clear tbl;
  let parse =
    fun grouped str pos next g -> match p1.left_rec with Left_rec companion ->
      (try
	assert (Printf.eprintf "trying to pop %d in %a...\n%!" p1.ident print_stack grouped.stack; true);
	let v, stack = pop_stack p1.ident grouped.stack in
	assert (Printf.eprintf "pop ok\n%!"; true);
	g str pos str pos str pos stack v
      with Not_found ->
	if List.mem p1.ident (fst grouped.stack) then raise Error;
	let grouped = {
	    grouped with
	    stack = ((List.map (fun p -> p.ident) companion @ fst grouped.stack), snd grouped.stack)
	} in
	assert (Printf.eprintf " failed %a\n%!" print_stack grouped.stack; true);
	let next' = List.fold_left (fun n l -> sum_next l.next n) next companion in
	assert (Printf.eprintf "next = %a, next' = %a\n%!" print_next next print_next next'; true);

	let rec fn pi str' pos' str'' pos'' stack v =
	  if snd stack <> EmptyStack then raise Error;
 	  assert (Printf.eprintf "PUSH W %d in %a\n%!" pi.ident print_stack stack; true);
	  let grouped' = {
	    grouped with
	    stack = (fst stack, Pushed(pi.ident, Obj.repr v))
	  } in
	  let grouped = set_stack grouped stack in
	  let rec fn' = function
	    | [] -> raise Error
	    | [p] ->
	       (match p.def with None -> assert false
			       | Some p' -> gn (uncast p) (uncast p'))
	    | p::l ->
	       try
		 (match p.def with None -> assert false
				 | Some p' -> gn (uncast p) (uncast p'))
	       with Error -> fn' l

	  and gn pi pidef =
	    assert (Printf.eprintf "testing %d = %d && next = %a\n%!" p1.ident pi.ident print_next next; true);
	    if p1 == pi && test grouped next str'' pos'' then
	      try
		assert (Printf.eprintf "calling in try %d %a\n%!" pi.ident print_stack grouped'.stack; true);
		pidef.parse grouped' str'' pos'' next'
			    (fun _ _ str0' pos0' str0'' pos0'' stack v ->
			     assert (Printf.eprintf "call continuation in try %d %a\n%!" pi.ident print_stack stack; true);
			     fn pi str0' pos0' str0'' pos0'' stack v)
	      with Error ->
		assert (Printf.eprintf "capturing Error %d %a\n%!" pi.ident print_stack stack; true);
		g str pos str' pos' str'' pos'' stack v
		else (
		  assert (Printf.eprintf "calling without try %d %a\n%!" pi.ident print_stack grouped'.stack; true);
		  pidef.parse grouped' str'' pos'' next'
			      (fun _ _ str0' pos0' str0'' pos0'' stack v ->
			       assert (Printf.eprintf "call continuation without try %d %a\n%!" p1.ident print_stack stack; true);
			       fn pi str0' pos0' str0'' pos0'' stack v))
	  in
	  let companion = List.filter
			    (fun g -> g.ident == p1.ident || test_next g grouped' str'' pos'')
			    companion
	  in
	  fn' companion
	in
	assert (Printf.eprintf "initial calling %d\n%!" p1.ident; true);
	let fn'' p =
	  (match p.def with
	     None -> assert false
	   | Some p' ->
	      tryif (accept_empty p')
		    (fun () ->
		     (uncast p').parse grouped str pos next'
				(fun _ _ str' pos' str'' pos'' stack v ->
				 assert (Printf.eprintf "initial call continuation %d %a\n%!" p.ident print_stack stack; true);
				 fn (uncast p) str' pos' str'' pos'' stack v))
		    (fun () ->
		     let v = read_empty (uncast p') str pos in
		     fn (uncast p) str pos str pos grouped.stack v))
	in
	let rec fn' = function
	  | [] -> raise Error
	  | [p] -> fn'' p
	  | p::l -> try fn'' p with Error -> fn' l
	in
	let empty_ok = test grouped next str pos in
	let companion = List.filter
			  (fun g -> (empty_ok && accept_empty g)
				    || test_next g grouped str pos)
			  companion
	in
	fn' companion)

     | Non_rec -> p2.parse grouped str pos next g
  in
  p1.parse <- parse;
  p1.set_info <- (fun () ->
  		  p1.next <- sum_next p1.next p2.next;
		  p1.accept_empty <- p2.accept_empty);
  add_deps p1 p2;
  update (cast p1)

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

let apply : 'a 'b.('a -> 'b) -> 'a grammar -> 'b grammar
  = fun f l ->
  let res = {
      ident = new_ident ();
      next = l.next;
      accept_empty = case_empty l.accept_empty (fun x str pos -> f (x str pos));
      left_rec = Non_rec;
      deps = imit_deps l;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
        l.parse grouped str pos next
	   (fun l c l' c' l'' c'' stack x ->
	    let r = try f x with Give_up msg -> parse_error grouped (~!msg) l' c' in
	    g l c l' c' l'' c'' stack r);
    }
  in
  res.set_info <- (fun () ->
    res.next <- l.next;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos -> f (x str pos)));
  add_deps res l;
  res

let delim : 'a grammar -> 'a grammar
  = fun l ->
  let res =
    { ident = new_ident ();
      next = l.next;
      left_rec = Non_rec;
      accept_empty = l.accept_empty;
      deps = imit_deps l;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
        let cont l c l' c' l'' c'' stack x () = g l c l' c' l'' c'' stack x in
        l.parse grouped str pos next cont ()
    }
  in
  res.set_info <- (fun () ->
    res.next  <- l.next;
    res.accept_empty <- l.accept_empty);
  add_deps res l;
  res

let merge : 'a 'b.('a -> 'b) -> ('b -> 'b -> 'b) -> 'a grammar -> 'b grammar
  = fun unit merge l ->
  let res =
    { ident = new_ident ();
      next = l.next;
      left_rec = Non_rec;
      accept_empty = case_empty l.accept_empty (fun x str pos -> unit (x str pos));
      deps = imit_deps l;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
        let m = ref PosMap.empty in
        let cont l c l' c' l'' c'' stack x =
	  let x = try unit x with Give_up msg -> parse_error grouped (~!msg) l' c' in
          if snd stack = EmptyStack then (try
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
                         res := Some (g str pos str' pos' str'' pos'' (fst grouped.stack, EmptyStack) x);
                         raise Exit
                       with
                         Error | Exit -> ()) !m;
          match !res with
            None -> raise Error
          | Some x -> x
    }
  in
  res.set_info <- (fun () ->
    res.next <- l.next;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos -> unit (x str pos)));
  add_deps res l;
  res

let lists : 'a grammar -> 'a list grammar =
  fun gr -> merge (fun x -> [x]) (@) gr

let position : 'a grammar -> (string * int * int * int * int * 'a) grammar
  = fun l ->
   let res =
     { ident = new_ident ();
       next = l.next;
       left_rec = Non_rec;
       accept_empty = case_empty l.accept_empty (fun x str pos -> let l = line_num str in
								  fname str, l, pos, l, pos,x str pos);
      deps = imit_deps l;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          l.parse grouped str pos next (fun l c l' c' l'' c'' stack x ->
                                          g l c l' c' l'' c'' stack (
                                              (fname l, line_num l, c, line_num l', c', x)))
    }
  in
  res.set_info <- (fun () ->
    res.next <- l.next;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos ->
						   let l = line_num str in
						   fname str, l, pos, l, pos,x str pos));
  add_deps res l;
  res

let apply_position : ('a -> buffer -> int -> buffer -> int -> 'b) -> 'a grammar -> 'b grammar
  = fun f l ->
   let res =
     { ident = new_ident ();
       next = l.next;
       left_rec = Non_rec;
       accept_empty = case_empty l.accept_empty (fun x str pos ->
						f (x str pos) str pos str pos);
      deps = imit_deps l;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          l.parse grouped str pos next
                  (fun l c l' c' l'' c'' stack x ->
		   let r = try f x l c l' c' with Give_up msg -> parse_error grouped (~!msg) l' c' in
		   g l c l' c' l'' c'' stack r)
    }
  in
  res.set_info <- (fun () ->
    res.next <- l.next;
    res.accept_empty <- case_empty l.accept_empty (fun x str pos -> f (x str pos) str 0 str 0));
  add_deps res l;
  res

let eof : 'a -> 'a grammar
  = fun a ->
    let set = singleton '\255' in
    { ident = new_ident ();
      next = { accepted_char = set;
	       first_syms = ~~ "EOF";
	       next_after_prefix = [];
	     };
      accept_empty = Non_empty;
      left_rec = Non_rec;
      set_info = (fun () -> ());
      deps = None;
      def = None;
      parse =
        fun grouped str pos next g ->
	let stack = test_clear grouped in
        if is_empty str pos then g str pos str pos str pos stack a else parse_error grouped (~~ "EOF") str pos
    }

let empty : 'a -> 'a grammar = fun a ->
  { ident = new_ident ();
    next = empty_next;
    accept_empty = May_empty (fun _ _ -> a);
    left_rec = Non_rec;
    set_info = (fun () -> ());
    deps = None;
    def = None;
    parse = fun grouped str pos next g -> raise Error (* FIXME: parse_error ? *)
  }

let active_debug = ref true

let debug : string -> unit grammar = fun msg ->
  { ident = new_ident ();
    next = empty_next;
    accept_empty = May_empty (fun _ _ -> ());
    left_rec = Non_rec;
    deps = None;
    set_info = (fun () -> ());
    def = None;
    parse = fun grouped str pos next g ->
	    if !active_debug then
	      begin
		let l = line str in
		let current = try String.sub l pos (min (String.length l - pos) 10) with _ -> "???" in
		Printf.eprintf "%s(%d,%d): %S %a %a\n%!" msg (line_num str) pos current print_next next print_info grouped;
	      end;
	    raise Error} (* FIXME: parse_error ? *)

let fail : string -> 'a grammar = fun msg ->
  { ident = new_ident ();
    next = empty_next;
    accept_empty = Non_empty;
    left_rec = Non_rec;
    deps = None;
    set_info = (fun () -> ());
    def = None;
    parse = fun grouped str pos next g ->
             parse_error grouped (~~ msg) str pos }

let  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> 'a option -> string -> 'a grammar =
  (fun fn set empty name ->
   { ident = new_ident ();
     next = { accepted_char = set;
	      first_syms = ~~ name;
	      next_after_prefix = [];
	    };
     accept_empty = (match empty with None -> Non_empty | Some a -> May_empty (fun _ _ -> a));
     left_rec = Non_rec;
     deps = None;
     set_info = (fun () -> ());
     def = None;
     parse = fun grouped str pos next g ->
             let a, str', pos' = try fn str pos with Give_up msg -> parse_error grouped (~! msg) str pos in
	     if str' == str && pos' == pos then raise Error;
             let str'', pos'' = apply_blank grouped str' pos' in
	     if str == str' && pos == pos' then raise Error;
	     let stack = test_clear grouped in
             g str pos str' pos' str'' pos'' stack a })

let char : char -> 'a -> 'a grammar
  = fun s a ->
    let set = singleton s in
    let s' = String.make 1 s in
    { ident = new_ident ();
      next = { accepted_char = set;
	       first_syms = ~~ s';
	       next_after_prefix = [];
	     };
      accept_empty = Non_empty;
      left_rec = Non_rec;
      deps = None;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
	let stack = test_clear grouped in
          let c, str', pos' = read str pos in
          if c <> s then parse_error grouped (~~ s') str pos;
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' stack a
    }

let any : char grammar
  = let set = del full_charset '\255' in
    { ident = new_ident ();
      next = { accepted_char = set;
	       first_syms = ~~ "ANY";
	       next_after_prefix = [];
	     };
      accept_empty = Non_empty;
      left_rec = Non_rec;
      deps = None;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
	let stack = test_clear grouped in
        let c, str', pos' = read str pos in
        if c = '\255' then parse_error grouped (~~ "ANY") str pos;
        let str'', pos'' = apply_blank grouped str' pos' in
        g str pos str' pos' str'' pos'' stack c
    }

let string : string -> 'a -> 'a grammar
  = fun s a ->
   let len_s = String.length s in
    let set = if len_s > 0 then singleton s.[0] else empty_charset in
    { ident = new_ident ();
      next = { accepted_char = set;
	       first_syms = if len_s > 0 then (~~ s) else Empty;
	       next_after_prefix = [];
	     };
      accept_empty = if (len_s = 0) then May_empty (fun str pos -> a) else Non_empty;
      left_rec = Non_rec;
      deps = None;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
	  if len_s = 0 then raise Error;
	  let stack = test_clear grouped in
          let str' = ref str in
          let pos' = ref pos in
          for i = 0 to len_s - 1 do
            let c, _str', _pos' = read !str' !pos' in
            if c <> s.[i] then parse_error grouped (~~s) str pos;
            str' := _str'; pos' := _pos'
          done;
          let str' = !str' and pos' = !pos' in
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' stack a
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
      next = { accepted_char = set;
	       first_syms = ~~ name;
	       next_after_prefix = [];
	     };
      accept_empty = if (Str.string_match r "" 0) then May_empty (fun _ _ -> let f n = "" in a f) else Non_empty ;
      left_rec = Non_rec;
      deps = None;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          let l = line str in
          if pos > String.length l then
            parse_error grouped (~~ name) str pos;
          if string_match r l pos then (
            let f n = matched_group n l in
            let pos' = match_end () in
	    if pos' = pos then raise Error;
	    let stack = test_clear grouped in
	    let res = try a f with Give_up msg -> parse_error grouped (~!msg) str pos' in
            let str'', pos'' = apply_blank grouped str pos' in
            g str pos str pos' str'' pos'' stack res
          ) else (
            parse_error grouped (~~ name) str pos)
    }

let imit_deps_seq l1 l2 =
  if l1.accept_empty = Non_empty then
    imit_deps l1
  else
    match l1.deps, l2.deps with None, None -> None | _ -> Some (TG.create 7)

let sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 l2 f ->
   let res =
     { ident = new_ident ();
       next = compose_next l1 l2;
       accept_empty = case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos));
      left_rec = Non_rec;
      set_info = (fun () -> ());
      deps = imit_deps_seq l1 l2;
      def = None;
      parse =
        fun grouped str pos next g ->
	let next' = compose_next' l2 next in
        tryif (accept_empty l1 && test grouped next' str pos)
	  (fun () -> l1.parse grouped str pos next'
                     (fun str pos str' pos' str'' pos'' stack a ->
		      let grouped = set_stack grouped stack in
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
		   res.next <- compose_next l1 l2;
		   res.accept_empty <- case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos)));
    add_deps res l1;
    (* FIXME: if accept_empty l1 becomes false, this dependency could be removed *)
    if accept_empty l1 then add_deps res l2;
  res

let sequence_position : 'a grammar -> 'b grammar -> ('a -> 'b -> buffer -> int -> buffer -> int -> 'c) -> 'c grammar
  = fun l1 l2 f ->
   let res =
     { ident = new_ident ();
       next = compose_next l1 l2;
      accept_empty = case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos) str pos str pos);
      left_rec = Non_rec;
      set_info = (fun () -> ());
      deps = imit_deps_seq l1 l2;
      def = None;
      parse =
        fun grouped str pos next g ->
	  let next' = compose_next' l2 next in
          tryif (accept_empty l1 && test grouped next' str pos)
          (fun () -> l1.parse grouped str pos next'
                   (fun str pos str' pos' str'' pos'' stack a ->
		    let grouped = set_stack grouped stack in
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
		   res.next <- compose_next l1 l2;
		   res.accept_empty <- case_empty2_and l1 l2 (fun x y str pos -> f (x str pos) (y str pos) str pos str pos));
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
      next = l1.next;  (* FIXME: to check *)
      accept_empty = Non_empty;
      left_rec = Non_rec;
      set_info = (fun () -> ());
      deps = imit_deps l1;
      def = None;
      parse =
        fun grouped str pos next g ->
	l1.parse grouped str pos all_next
                 (fun str pos str' pos' str'' pos'' stack a ->
		  let grouped = set_stack grouped stack in
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
		   res.next <- l1.next);
  add_deps res l1;
  res

let iter : 'a grammar grammar -> 'a grammar
  = fun g -> dependent_sequence g (fun x -> x)

let change_layout : ?new_blank_before:bool -> ?old_blank_after:bool -> 'a grammar -> blank -> 'a grammar
  = fun ?(new_blank_before=true) ?(old_blank_after=true) l1 blank1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
   let res =
     { ident = new_ident ();
       next = l1.next;
       accept_empty = l1.accept_empty;
      left_rec = Non_rec;
      deps = imit_deps l1;
      set_info = (fun () -> ());
      def = None;
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
    res.next <- l1.next;
    res.accept_empty <- l1.accept_empty);
  add_deps res l1;
  res

let ignore_next_blank : 'a grammar -> 'a grammar
  = fun l1 ->
   let res =
     { ident = new_ident ();
       next = l1.next;
      accept_empty = l1.accept_empty;
      left_rec = Non_rec;
      deps = imit_deps l1;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          l1.parse grouped str pos all_next (fun s p s' p' s'' p'' -> g s p s' p' s' p')
    }
  in
  res.set_info <- (fun () ->
    res.next <- l1.next;
    res.accept_empty <- l1.accept_empty);
  add_deps res l1;
  res

let option : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
  let res =
    { ident = new_ident ();
      next = l.next;
      accept_empty = May_empty (fun _ _ -> a);
      left_rec = Non_rec;
      deps = imit_deps l;
      set_info = (fun () -> ());
      def = None;
      parse = l.parse
    }
  in
  res.set_info <- (fun () ->
    res.next <- l.next);
  add_deps res l;
  res

let option' : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
  let res =
    { ident = new_ident ();
      next = l.next;
      accept_empty = May_empty (fun _ _ -> a);
      left_rec = Non_rec;
      deps = imit_deps l;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
            l.parse grouped str pos next
                    (fun s p s' p' s'' p'' stack x () -> g s p s' p' s'' p'' stack x) ()
    }
  in
  res.set_info <- (fun () ->
    res.next <- l.next);
  add_deps res l;
  res

let fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
  let res =
    { ident = new_ident ();
      next = f1.next;
      accept_empty = May_empty (fun _ _ -> a);
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          let next' = sum_next f1.next next in
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
  res.set_info <- (fun () -> res.next <- f1.next);
  add_deps res f1;
  res

let fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
   let res =
     { ident = new_ident ();
       next = f1.next;
      accept_empty = May_empty (fun _ _ -> a);
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          let next' = sum_next f1.next next in
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
  res.set_info <- (fun () -> res.next <- f1.next);
  add_deps res f1;
  res

let fixpoint1 : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
  let res =
    { ident = new_ident ();
      next = f1.next;
      accept_empty = case_empty f1.accept_empty (fun f str pos -> f str pos a);
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          let next' = sum_next f1.next next in
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
    res.next <- f1.next;
    res.accept_empty <- case_empty f1.accept_empty (fun f str pos -> f str pos a));
  add_deps res f1;
  res


let fixpoint1' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
  let res =
    { ident = new_ident ();
      next = f1.next;
      accept_empty = case_empty f1.accept_empty (fun f str pos -> f str pos a);
      left_rec = Non_rec;
      deps = imit_deps f1;
      set_info = (fun () -> ());
      def = None;
      parse =
        fun grouped str pos next g ->
          let next' = sum_next f1.next next in
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
    res.next <- f1.next;
    res.accept_empty <- case_empty f1.accept_empty (fun f str pos -> f str pos a));
  add_deps res f1;
  res

let alternatives : 'a grammar list -> 'a grammar
  = fun ls ->
  let res =
    { ident = new_ident ();
      next = sum_nexts ls;
    accept_empty = List.fold_left (fun s p -> match s, p.accept_empty with
						(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
						| _ -> Non_empty) Non_empty ls;
    deps = if List.exists (fun l -> l.deps <> None) ls then Some (TG.create 7) else None;
    set_info = (fun () -> ());
    left_rec = Non_rec;
    def = None;
    parse =
      fun grouped str pos next g ->
          let empty_ok = test grouped next str pos in
          let ls = List.filter (fun g ->
                                (empty_ok && accept_empty g) || test_next g grouped str pos) ls in
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
    res.next <- sum_nexts ls;
    res.accept_empty <-
      List.fold_left (fun s p ->
		      match s, p.accept_empty with
			(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
			| _ -> Non_empty) Non_empty ls);
  List.iter (fun l -> add_deps res l) ls;
  res

let alternatives' : 'a grammar list -> 'a grammar
  = fun ls ->
  let res =
    { ident = new_ident ();
      next = sum_nexts ls;
      accept_empty = List.fold_left (fun s p -> match s, p.accept_empty with
						(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
						| _ -> Non_empty) Non_empty ls;
    left_rec = Non_rec;
    deps = if List.exists (fun l -> l.deps <> None) ls then Some (TG.create 7) else None;
    set_info = (fun () -> ());
    def = None;
    parse =
        fun grouped str pos next g ->
          let empty_ok = test grouped next str pos in
          let ls = List.filter (fun g ->
                                (empty_ok && accept_empty g) || test_next g grouped str pos) ls in
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
    res.next <- sum_nexts ls;
    res.accept_empty <- List.fold_left (fun s p -> match s, p.accept_empty with
						(May_empty _ as x, _) | (_, (May_empty _ as x)) -> x
						| _ -> Non_empty) Non_empty ls);
  List.iter (fun l -> add_deps res l) ls;
  res

let parse_buffer grammar blank str =
  let grammar = sequence grammar (eof ()) (fun x _ -> x) in
  let grouped = { blank;
                  err_info = {max_err_pos = -1;
                              max_err_buf = str;
                              max_err_col = -1;
                              err_msgs = Empty };
		  stack = [], EmptyStack;
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
		  stack = [], EmptyStack;
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
