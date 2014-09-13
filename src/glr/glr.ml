open Str
open Charset
open Input

exception Ambiguity of string * int * int * string * int * int
exception Parse_error of string * int * int * string list
exception Give_up

type string_tree =
    Empty | Leaf of string | Node of string_tree * string_tree

let (@@) t1 t2 = Node(t1,t2)
let (~~) t1 = Leaf t1

let collect_tree t =
  let rec fn acc = function
      Empty -> acc
    | Leaf t -> if List.mem t acc then acc else t::acc
    | Node(t1,t2) -> fn (fn acc t1) t2
  in
  List.sort compare (fn [] t)

let max_hash = Hashtbl.create 101

module Pos = struct
  type t = buffer * int
  let compare = fun (b,p) (b',p') -> 
    line_beginning b + p - line_beginning b' - p'
end

module PosMap = Umap.Make(Pos)

type blank = buffer -> int -> buffer * int

let blank_regexp r = 
  let accept_newline = string_match r "\n" 0 && match_end () = 1 in
  let rec fn str pos =
    if string_match r (line str) pos then
      let pos' = match_end () in
      if accept_newline then (
	let c, str'', pos'' = read str pos' in if c = '\n' then fn str'' pos''
					     else str, pos')
      else str, pos'
    else str, pos
  in
  fn

type error_position_key = int

let next_key =
  let c = ref 0 in
  fun () ->
  let k = !c in c := k + 1; k

type grouped = {
  blank: blank;
  err_key:error_position_key;
  ignore_next_blank:buffer * int; (* ..., -1 not to ignore *)
}

type next = {
  accepted_char: charset;
  first_syms : string_tree;
}

type 'a grammar = {
  mutable firsts : charset Lazy.t;
  mutable first_sym : string_tree Lazy.t;
  mutable accept_empty : bool Lazy.t;
  mutable parse : 'b. grouped -> buffer -> int -> next option -> (buffer -> int -> buffer -> int -> 'a -> 'b) -> 'b;
}

let parse_error grouped msg line pos =
  let key = grouped.err_key in
  let (line', pos', msgs) = try Hashtbl.find max_hash key with Not_found -> (line, -1, Empty) in
  let c = Pos.compare (line, pos) (line', pos') in
  if c = 0 then Hashtbl.replace max_hash key (line, pos, msg @@ msgs) 
  else if c > 0 then Hashtbl.replace max_hash key (line, pos, msg);
  raise Give_up
 
let accept_empty g = Lazy.force g.accept_empty
let firsts g = Lazy.force g.firsts
let first_sym g = Lazy.force g.first_sym
let next_sym g = Some {
		 accepted_char = firsts g;
		 first_syms = first_sym g;
	       }

let apply_blank grouped str p =
  let str', p' = grouped.ignore_next_blank in
  if p = p' && str == str' then str, p
  else grouped.blank str p

let test grouped next str p =
  match next with
    None -> true
  | Some next -> 
     let str, p = apply_blank grouped str p in
     let c, _, _ = read str p in
     let res = get next.accepted_char c in
     if not res then
       begin
	 let key = grouped.err_key in
	 let (str', p', msgs) = try Hashtbl.find max_hash key with Not_found -> (str, -1, Empty) in
	 let c = Pos.compare (str, p) (str', p') in
	 let msg = next.first_syms in
	 if c = 0 then Hashtbl.replace max_hash key (str, p, msg @@ msgs)
	 else if c > 0 then Hashtbl.replace max_hash key (str, p, msg)
       end;
     res

let not_ready name _ = failwith ("not_ready: "^name)

let declare_grammar name = {
  firsts = lazy (not_ready name ());
  first_sym = lazy (not_ready name ());
  accept_empty = lazy (not_ready name ());
  parse = (not_ready name);
}

let set_grammar p1 p2 =
(*  if p1.parse != not_ready then failwith "this grammar can not be set";*)
  p1.firsts <- p2.firsts;
  p1.first_sym <- p2.first_sym;
  p1.accept_empty <- p2.accept_empty;
  p1.parse <- p2.parse

let declare_grammar name = {
  firsts = lazy (not_ready name ());
  first_sym = lazy (not_ready name ());
  accept_empty = lazy (not_ready name ());
  parse = (not_ready name);
}

let grammar_family ?(param_to_string=fun _ -> "<param>") name =
  let tbl = Hashtbl.create 101 in
  let definition = ref None in 
  let seeds = ref [] in
  let record p = seeds := p::!seeds in
  let do_fix fn =
    while !seeds <> [] do
      let new_seeds = !seeds in
      seeds := [];
      List.iter (fun k -> ignore (fn k)) new_seeds;
    done;
    Hashtbl.iter (fun key g -> set_grammar g (fn key)) tbl;
  in
  let gn = fun param ->
    try Hashtbl.find tbl param
    with Not_found ->
      record param;
      let g = match !definition with
	  Some f -> 
	  let g = declare_grammar (name ^ ":" ^ (param_to_string param)) in
	  Hashtbl.add tbl param g;
	  let _ = f param in
	  do_fix f;
	  g
	| None ->
	   declare_grammar (name ^ ":" ^ (param_to_string param))
      in
      Hashtbl.replace tbl param g;
      g
  in gn,
  (fun fn ->
   do_fix fn;
   definition := Some fn)

let apply : ('a -> 'b) -> 'a grammar -> 'b grammar
  = fun f l -> 
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun grouped str pos next g ->
	  l.parse grouped str pos next (fun l c l' c' x -> g l c l' c' (f x));
    }

let merge : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a grammar -> 'b grammar
  = fun unit merge l ->
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun grouped str pos next g ->
	let m = ref PosMap.empty in
	let cont l c l' c' x =
	  let x = unit x in
	  (try 
	      let (_,_,old) = PosMap.find (l', c') !m in
	      m := PosMap.add (l', c') (l, c, merge x old) !m				 
	    with Not_found ->
	      m := PosMap.add (l', c') (l, c, x) !m);
	  raise Give_up
	in
	try
	  ignore (l.parse grouped str pos next cont);
	  assert false
	with Give_up ->
	  let res = ref None in
	  PosMap.iter (fun (str',pos') (str, pos, x) ->
		       try 
			 res := Some (g str pos str' pos' x); 
			 raise Exit
		       with
			 Give_up -> ()) !m;
	  match !res with
	    None -> raise Give_up
	  | Some x -> x
    }

let lists : 'a grammar -> 'a list grammar =
  fun gr -> merge (fun x -> [x]) (@) gr

let position : 'a grammar -> (string * int * int * int * int * 'a) grammar
  = fun l -> 
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun grouped str pos next g ->
	  l.parse grouped str pos next (fun l c l' c' x -> 
					  g l c l' c' (
					      (fname l, line_num l, c, line_num l', c', x)))
    }

let apply_position : ('a -> buffer -> int -> buffer -> int -> 'b) -> 'a grammar -> 'b grammar
  = fun f l -> 
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun grouped str pos next g ->
	  l.parse grouped str pos next
		  (fun l c l' c' x -> g l c l' c' (
		   f x l c l' c'))
    }

let filter_position : 'a grammar -> (string -> int -> int -> int -> int -> int -> int -> 'b) -> ('b * 'a) grammar
  = fun l filter -> 
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun grouped str pos next g ->
	  l.parse grouped str pos next
		  (fun l c l' c' x -> g l c l' c' (
					  filter (fname l) (line_num l) (line_beginning l) c (line_num l') (line_beginning l') c', x))
    }

let eof : 'a -> 'a grammar
  = fun a ->
    let set = singleton '\255' in
    { firsts = lazy set;
      first_sym = lazy (~~ "EOF");
      accept_empty = lazy false;
      parse =
	fun grouped str pos next g ->
	let str, pos = apply_blank grouped str pos in
	  if is_empty str then g str pos str pos a else parse_error grouped (~~ "EOF") str pos
    }

let list_eof : 'a -> 'a list grammar = 
  fun x -> eof [x]

let empty : 'a -> 'a grammar = fun a ->
  { firsts = lazy empty_charset;
    first_sym = lazy Empty;
    accept_empty = lazy true;
    parse = fun grouped str pos next g -> g str pos str pos a }

let list_empty x = empty [x]
 
let debug_aux : 'a -> string -> 'a grammar = fun a msg ->
  { firsts = lazy empty_charset;
    first_sym = lazy Empty;
    accept_empty = lazy true;
    parse = fun grouped str pos next g ->
	    let l = line str in
	    let current = String.sub l pos (min (String.length l - pos) 10) in
	    Printf.eprintf "%s(%d,%d): %S %a\n" msg (line_num str) pos current print_charset (match next with
												None -> None | Some next -> Some next.accepted_char);
	    g str pos str pos a }

let debug = debug_aux ()
let list_debug = debug_aux [()]

let fail : string -> 'a grammar = fun msg ->
  { firsts = lazy empty_charset;
    first_sym = lazy Empty;
    accept_empty =  lazy false;
    parse = fun grouped str pos next g -> 
	     parse_error grouped (~~ msg) str pos }

let list_fail = fail

let  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool -> string -> 'a grammar =
  (fun fn set empty msg ->
   { firsts = lazy set;
    first_sym = lazy (~~ msg);
     accept_empty = lazy empty;
     parse = fun grouped str pos next g ->
	     let str, pos = apply_blank grouped str pos in 
	     let a, str', pos' = try fn str pos with Give_up -> parse_error grouped (~~ msg) str pos in
	     g str pos str' pos' a })

let char : char -> 'a -> 'a grammar 
  = fun s a -> 
    let set = singleton s in
    let s' = String.make 1 s in
    { firsts = lazy set;
      first_sym = lazy (~~ s');
      accept_empty = lazy false;
      parse =
	fun grouped str pos next g ->
	  let str, pos = apply_blank grouped str pos in
          let c, str', pos' = read str pos in
	  if c <> s then parse_error grouped (~~ s') str pos;
	  g str pos str' pos' a
    }

let any : char grammar 
  = let set = del full_charset '\255' in
    { firsts = lazy set;
      first_sym = lazy (~~ "ANY");
      accept_empty = lazy false;
      parse =
	fun grouped str pos next g ->
	  let str, pos = apply_blank grouped str pos in
          let c, str', pos' = read str pos in
	  if c = '\255' then parse_error grouped (~~ "ANY") str pos;
	  g str pos str' pos' c
    }

let list_char c x = char c [x]

let string : string -> 'a -> 'a grammar 
  = fun s a -> 
   let len_s = String.length s in
    let set = if len_s > 0 then singleton s.[0] else empty_charset in
    { firsts = lazy set;
      first_sym = lazy (if len_s > 0 then (~~ s) else Empty);
      accept_empty = lazy (len_s = 0);
      parse =
	fun grouped str pos next g ->
	  let str, pos = apply_blank grouped str pos in
          let str' = ref str in
	  let pos' = ref pos in
	  for i = 0 to len_s - 1 do
	    let c, _str', _pos' = read !str' !pos' in
	    if c <> s.[i] then parse_error grouped (~~s) str pos;
	    str' := _str'; pos' := _pos'
	  done;
	  let str' = !str' and pos' = !pos' in 
	  g str pos str' pos' a
    }

let list_string : string -> 'a -> 'a list grammar
  = fun s a -> string s [a]

let regexp : string -> ?name:string -> ((int -> string) -> 'a) -> 'a grammar
  = fun r0 ?(name=String.escaped r0) a ->
    let r = Str.regexp r0 in
    let set = Array.copy empty_charset in
    let found = ref false in
    for i = 0 to 254 do
      let s = String.make 1 (Char.chr i) in
      if Str.string_partial_match r s 0 && Str.match_end () > 0 then
	(found := true; addq set (Char.chr i))
    done;
    if not !found then failwith "regexp: illegal empty regexp";
    { firsts = lazy set;
      first_sym = lazy (~~ name);
      accept_empty = lazy (Str.string_match r "" 0);
      parse = 
	fun grouped str pos next g ->
	let str, pos = apply_blank grouped str pos in
        let l = line str in
	if pos > String.length l then
	  parse_error grouped (~~ name) str pos;
	if string_match r l pos then
	  let f n = matched_group n l in
	  let pos' = match_end () in
	  g str pos str pos' (a f)
	  else (
	    parse_error grouped (~~ name) str pos)
    }

let mk_empty in_analysis fn =
  lazy (
      if !in_analysis then failwith "illegal left recursion";
      in_analysis := true;
      let r = fn () in
      in_analysis := false;
      r)

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
  match next with
  | None -> None 
  | Some s -> Some { accepted_char = union s.accepted_char (firsts gram);
		first_syms = s.first_syms @@ first_sym gram; }

let union' gram next =
  if accept_empty gram then union'' gram next
  else next_sym gram

let sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 l2 f ->
  let flag = ref false in
    { firsts = lazy (union_firsts l1 l2);
      first_sym = lazy (union_first_sym l1 l2);
      accept_empty = mk_empty flag (fun () -> accept_empty l1 && accept_empty l2);
      parse =
	fun grouped str pos next g ->
	  l1.parse grouped str pos (union' l2 next)
		   (fun str pos str' pos' a ->
		    let first_empty = str == str' && pos = pos' in
		    l2.parse grouped str' pos' next
			     (fun str0 pos0 str' pos' x ->
			      let str, pos = if first_empty then str0, pos0 else str, pos in
			      g str pos str' pos' (f a x)))
    }

let sequence_position : 'a grammar -> 'b grammar -> ('a -> 'b -> buffer -> int -> buffer -> int -> 'c) -> 'c grammar
  = fun l1 l2 f ->
  let flag = ref false in
    { firsts = lazy (union_firsts l1 l2);
      first_sym = lazy (union_first_sym l1 l2);
      accept_empty = mk_empty flag (fun () -> accept_empty l1 && accept_empty l2);
      parse =
	fun grouped str pos next g ->
	  l1.parse grouped str pos (union' l2 next)
		   (fun str pos str' pos' a ->
		    let first_empty = str == str' && pos = pos' in
		    l2.parse grouped str' pos' next
			     (fun str0 pos0 str' pos' b -> 
			      let str, pos = if first_empty then str0, pos0 else str, pos in
			      g str pos str' pos' (f a b str pos str' pos')))
    }

let fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence l1 l2 (fun x f -> f x)

let fsequence_position : 'a grammar -> (buffer -> int -> buffer -> int -> 'a -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence_position l1 l2 (fun x f s p s' p' -> f s p s' p' x)

let sequence3 : 'a grammar -> 'b grammar -> 'c grammar -> ('a -> 'b -> 'c -> 'd) -> 'd grammar
  = fun l1 l2 l3 g ->
    sequence (sequence l1 l2 (fun x y z -> g x y z)) l3 (fun f -> f)

let dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun l1 f2 ->
  let flag = ref false in
    { firsts = lazy (firsts l1);
      first_sym = lazy (first_sym l1);
      accept_empty = mk_empty flag (fun () -> 
        let res = accept_empty l1 in 
	if res then failwith "initial rule must not parse empty sequence in dependent sequence";
	false);
      parse =
	fun grouped str pos next g ->
	  l1.parse grouped str pos None
		   (fun str pos str' pos' a ->
		    let first_empty = str == str' && pos = pos' in
		    (f2 a).parse grouped str' pos' next
			  (fun str0 pos0 str' pos' b ->
			   let str, pos = if first_empty then str0, pos0 else str, pos in
			   g str pos str' pos' b))
    }

let iter : 'a grammar grammar -> 'a grammar 
  = fun g -> dependent_sequence g (fun x -> x)

let change_layout : ?old_blank_before:bool -> ?new_blank_after:bool -> 'a grammar -> blank -> 'a grammar
  = fun ?(old_blank_before=true) ?(new_blank_after=false) l1 blank1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
    { firsts = lazy (firsts l1);
      first_sym = lazy (first_sym l1);
      accept_empty = lazy (accept_empty l1);
      parse =
	fun grouped str pos next g ->
  	  let str, pos = if old_blank_before then apply_blank grouped str pos else str, pos in
	  l1.parse { grouped with blank = blank1 } str pos None
		   (if new_blank_after then
		     (fun str pos str' pos' x ->
		      let str', pos' = blank1 str' pos' in
		      g str pos str' pos' x)
		   else g)
    }

let ignore_next_blank : 'a grammar -> 'a grammar
  = fun l1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
    { firsts = lazy (firsts l1);
      first_sym = lazy (first_sym l1);
      accept_empty = lazy (accept_empty l1);
      parse =
	fun grouped str pos next g  ->
	let grouped = { grouped with ignore_next_blank = str, pos } in
	  l1.parse grouped str pos None g
    }

let option : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy true;
      parse =
	fun grouped str pos next g ->
	if test grouped next str pos then
	  try
	    l.parse grouped str pos next g
	  with
	  | Give_up ->
	    g str pos str pos a
	else
	  l.parse grouped str pos next g
    }

let option' : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy true;
      parse =
	fun grouped str pos next g ->
	if test grouped next str pos then
	  (try
	    l.parse grouped str pos next
		    (fun s p s' p' x () -> g s p s' p' x)
	  with
	  | Give_up ->
	    fun () -> g str pos str pos a) ()
	else
	  l.parse grouped str pos next g
    }

let fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = lazy (firsts f1);
      first_sym = lazy (first_sym f1);
      accept_empty = lazy true;
      parse =
	fun grouped str pos next g ->
	  let next' = union'' f1 next in
	  let rec fn ipos str' pos' x =
	    let str, pos =
	      match ipos with None -> str, pos
			    | Some(str,pos) -> str,pos
	    in
	    if test grouped next str' pos' then
	      try 
		f1.parse grouped str' pos' next'
			 (fun str pos str' pos' f ->
			  let ipos =
			    match ipos with None -> Some(str,pos)
					  | Some _ -> ipos
			  in
			  fn ipos str' pos' (f x))
	      with
	      | Give_up -> g str pos str' pos' x
	    else
	      f1.parse grouped str' pos' next'
		       (fun str pos str' pos' f ->
			let ipos =
			  match ipos with None -> Some(str,pos)
					| Some _ -> ipos
			in
			fn ipos str' pos' (f x))
	  in fn None str pos a
    }

let fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = lazy (firsts f1);
      first_sym = lazy (first_sym f1);
      accept_empty = lazy true;
      parse =
	fun grouped str pos next g ->
	  let next' = union'' f1 next in
	  let rec fn ipos str' pos' x =
	    let str, pos =
	      match ipos with None -> str, pos
			    | Some(str,pos) -> str,pos
	    in
	    if test grouped next str' pos' then
	      try 
		f1.parse grouped str' pos' next'
			 (fun str pos str' pos' f ->
			  let ipos =
			    match ipos with None -> Some(str,pos)
					  | Some _ -> ipos
			  in
			  fn ipos str' pos' (f x))
	      with
	      | Give_up -> fun () -> g str pos str' pos' x
	    else
	      f1.parse grouped str' pos' next'
		       (fun str pos str' pos' f ->
			let ipos =
			  match ipos with None -> Some(str,pos)
					| Some _ -> ipos
			in
			fn ipos str' pos' (f x))
	  in 
	  fn None str pos a ()
    }

let alternatives : 'a grammar list -> 'a grammar 
  = fun ls ->
  let flag = ref false in
  { firsts = lazy (List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    first_sym = lazy (List.fold_left (fun s p -> s @@ (first_sym p)) Empty ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun grouped str pos next g ->
          let empty_ok = test grouped next str pos in
	  let ls = List.filter (fun g ->
				(empty_ok && accept_empty g) ||
				  test grouped (next_sym g) str pos) ls in
	  let rec fn = function
	    | l::ls ->
	      (try
		l.parse grouped str pos next (fun s p s' p' x ->
					      g s p s' p' x)
	      with
		Give_up -> fn ls)
	    | [] ->
	       raise Give_up
	  in
	  fn ls
    }

let alternatives' : 'a grammar list -> 'a grammar 
  = fun ls ->
  let flag = ref false in
  { firsts = lazy (List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    first_sym = lazy (List.fold_left (fun s p -> s @@ (first_sym p)) Empty ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun grouped str pos next g ->
          let empty_ok = test grouped next str pos in
	  let ls = List.filter (fun g ->
				(empty_ok && accept_empty g) ||
				  test grouped (next_sym g) str pos) ls in
	  let rec fn = function
	    | l::ls ->
	      (try
		l.parse grouped str pos next
			(fun s p s' p' x () ->  g s p s' p' x)
	      with
		Give_up -> fn ls)
	    | [] ->
	       raise Give_up
	  in
	  fn ls ()
    }

let parse_buffer grammar blank str =
  let grammar = sequence grammar (eof ()) (fun x _ -> x) in
  let key = next_key () in
  let grouped = { blank; err_key = key; ignore_next_blank = (str, -1) } in
  let a = try
      grammar.parse grouped str 0 None (fun _ _ str pos x -> x) 
    with Give_up -> 
      let str, pos, msgs = try Hashtbl.find max_hash key with Not_found -> str, 0, Empty in
      raise (Parse_error (fname str, line_num str, pos, collect_tree msgs))
  in
  Hashtbl.remove max_hash key;
  a

let partial_parse_buffer grammar blank str pos = 
  let key = next_key () in
  let grouped = { blank; err_key = key; ignore_next_blank = str, -1 } in
  let m = ref PosMap.empty in
  let cont l c l' c' x =
    (try 
	let (_,_,old) = PosMap.find (l', c') !m in
	m := PosMap.add (l', c') (l, c, x :: old) !m				 
      with Not_found ->
	m := PosMap.add (l', c') (l, c, [x]) !m);
    raise Give_up
  in
  (try
    ignore (grammar.parse grouped str pos None cont);
    assert false
  with Give_up -> 
    ());
  Hashtbl.remove max_hash key;
  PosMap.fold (fun (str',pos') (str,pos,la) acc -> List.rev_append (List.map (fun a -> (str',pos',a)) la) acc) !m []

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
  | Parse_error(fname,l,n,msg) -> 
     Printf.eprintf "%s: parse error after %d:%d, '%s' expected\n%!" fname l n (String.concat "|" msg)
  | Ambiguity(fname,l,n,_,l',n') ->
     Printf.eprintf "%s: ambiguous expression from %d:%d to %d:%d\n%!" fname l n l' n'
  | _ -> assert false

let handle_exception f a =
  try
    f a
  with
    Parse_error _ | Ambiguity _ as e -> print_exception e; exit 1
 
