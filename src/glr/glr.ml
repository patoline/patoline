open Str
open Charset
open Input

(*DEFINE DEBUG*)

exception Ambiguity of string * int * int * string * int * int
exception Parse_error of string * int * int * string list
exception Give_up

let max_hash = Hashtbl.create 101

module Pos = struct
  type t = buffer * int
  let compare = fun (b,p) (b',p') -> 
    let c = line_num b - line_num b' in
    if c = 0 then
      let c = p - p' in
      if c = 0 then
	compare (fname b) (fname b')
      else c
    else c
end

module PosMap = Umap.Make(Pos)

let single l c a = PosMap.add (l, c) a PosMap.empty

let rec merge_map1 = fun l c la lb ->
(*
    List.iter (fun (pos,a) -> Printf.printf "%d:%d" pos (Obj.magic a)) la;
    Printf.printf " ++ ";
    List.iter (fun (pos,a) -> Printf.printf "%d:%d" pos (Obj.magic a)) lb;
    Printf.printf "\n";
*)
  PosMap.union (fun (l',c') a b ->
    if a == b then a else raise (Ambiguity(fname l, line_num l,c,fname l', line_num l',c')))
    la lb

let rec merge_map2 = fun fn la lb ->
  PosMap.union (fun n a b -> fn a b) la lb

type blank = buffer -> int -> buffer * int

let blank_regexp r = 
  let accept_newline = string_match r "\n" 0 in
  let rec fn str pos =
    if string_match r (line str) pos then
      let pos' = match_end () in
      if accept_newline then
	let c, str'', pos'' = read str pos' in if c = '\n' then fn str'' pos''
					     else str, pos'
      else str, pos'
    else str, pos
  in
  fn

type key = int

let next_key =
  let c = ref 0 in
  fun () ->
  let k = !c in c := k + 1; k

type 'a grammar = {
  mutable firsts : charset Lazy.t;
  mutable firsts_sym : string list Lazy.t;
  mutable accept_empty : bool Lazy.t;
  mutable parse : 'b. blank -> buffer -> int -> charset option -> key -> (buffer -> int -> buffer -> int -> 'a -> 'b) -> 'b PosMap.t ;
}

let parse_error key msg line pos =
  let fn s l = if s = "" then l else s::l in 
  let (line', pos', msgs) = try Hashtbl.find max_hash key with Not_found -> (line, -1, []) in
  begin
    let c = Pos.compare (line, pos) (line', pos') in
    if c = 0 then Hashtbl.replace max_hash key (line, pos, fn msg msgs) 
    else if c > 0 then Hashtbl.replace max_hash key (line, pos, fn msg [])
  end;
IFDEF DEBUG THEN
  Printf.eprintf "parse_error (%d, %d, %s)\n%!" (line_num line) pos msg; 
ENDIF;
  raise Give_up

let parse_errors key msg line pos =
  let (line', pos', msgs) = try Hashtbl.find max_hash key with Not_found -> (line, -1, []) in
  begin
    let c = Pos.compare (line, pos) (line', pos') in
    if c = 0 then Hashtbl.replace max_hash key (line, pos, msg @ msgs) 
    else if c > 0 then Hashtbl.replace max_hash key (line, pos, msg)
  end;
IFDEF DEBUG THEN
  Printf.eprintf "parse_error (%d, %s)\n%!" pos (String.concat "|" msg)
ENDIF;
  raise Give_up
  
let accept_empty g = Lazy.force g.accept_empty
let firsts g = Lazy.force g.firsts
let firsts_sym g = Lazy.force g.firsts_sym

let test s str p =
  let r = 
    match s with
      None -> true
    | Some s -> 
       let c, _, _ = read str p in
       get s c
  in
IFDEF DEBUG THEN
  Printf.eprintf "test %a %d => %b\n%!" print_charset s p r
ENDIF;
  r

let not_ready _ = failwith "not_ready"

let declare_grammar () = {
  firsts = Lazy.from_fun not_ready;
  firsts_sym = Lazy.from_fun not_ready;
  accept_empty = Lazy.from_fun not_ready;
  parse = not_ready;
}

let set_grammar p1 p2 =
  if p1.parse != not_ready then failwith "this grammar can not be set";
  p1.firsts <- p2.firsts;
  p1.firsts_sym <- p2.firsts_sym;
  p1.accept_empty <- p2.accept_empty;
  p1.parse <- p2.parse

let grammar_family ?param_to_string () =
  let tbl = Hashtbl.create 101 in
  let in_build = ref true in 
  let gn = fun param ->
    try Hashtbl.find tbl param with Not_found ->
      let message = match param_to_string with
	  None -> "Too late to introduce a new grammar in a family"
	| Some f -> Printf.sprintf "Too late to introduce %a in a family" f param
      in
      if not !in_build then failwith message;
      let g = declare_grammar () in
      Hashtbl.add tbl param g;
      g
  in gn,
  (fun fn seeds ->
    List.iter (fun k -> ignore (fn k)) seeds;
    List.iter (fun k -> ignore (gn k)) seeds;
    Hashtbl.iter (fun key g -> set_grammar g (fn key)) tbl;
    in_build := false)

let apply : ('a -> 'b) -> 'a grammar -> 'b grammar
  = fun f l -> 
    { firsts = lazy (firsts l);
      firsts_sym = lazy (firsts_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun blank str pos next key g ->
	  l.parse blank str pos next key (fun l c l' c' x -> g l c l' c' (f x));
    }

let list_apply : ('a -> 'b) -> 'a list grammar -> 'b list grammar
  = fun f l -> apply (List.map f) l

let position : 'a grammar -> (string * int * int * int * int * 'a) grammar
  = fun l -> 
    { firsts = lazy (firsts l);
      firsts_sym = lazy (firsts_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun blank str pos next key g ->
	  l.parse blank str pos next key (fun l c l' c' x -> g l c l' c' (fname str, line_num l, c, line_num l', c', x))
    }

let filter_position : 'a grammar -> (string -> int -> int -> int -> int -> 'b) -> ('b * 'a) grammar
  = fun l filter -> 
    { firsts = lazy (firsts l);
      firsts_sym = lazy (firsts_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
	fun blank str pos next key g ->
	  l.parse blank str pos next key (fun l c l' c' x -> g l c l' c' (filter (fname str) (line_num l) c (line_num l') c', x))
    }

let eof : 'a -> 'a grammar
  = fun a ->
    let set = singleton '\255' in
    { firsts = Lazy.from_val set;
      firsts_sym = Lazy.from_val ["EOF"];
      accept_empty = Lazy.from_val false;
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d: Eof\n%!" pos
ENDIF;
	  if is_empty str then single str pos (g str pos str pos a) else parse_error key "EOF" str pos
    }

let list_eof : 'a -> 'a list grammar = 
  fun x -> eof [x]

let empty : 'a -> 'a grammar = fun a ->
  { firsts = Lazy.from_val empty_charset;
    firsts_sym = Lazy.from_val [];
    accept_empty = Lazy.from_val true;
    parse = fun blank str pos next key g -> 
	    single str pos (g str pos str pos a) }

let debug : string -> 'a -> 'a grammar = fun msg a ->
  { firsts = Lazy.from_val empty_charset;
    firsts_sym = Lazy.from_val [];
    accept_empty = Lazy.from_val true;
    parse = fun blank str pos next key g ->
	    let line = line str in
	    let current = String.sub line pos (min (String.length line - pos) 10) in
	    Printf.eprintf "%s(%d): %S\n" msg pos current;
	    single str pos (g str pos str pos a) }

let fail : string -> 'a grammar = fun msg ->
  { firsts = Lazy.from_val empty_charset;
    firsts_sym = Lazy.from_val [];
    accept_empty =  Lazy.from_val false;
    parse = fun blank str pos next key g -> 
	     parse_error key msg str pos }

let  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool -> string -> 'a grammar =
  (fun fn set empty msg ->
   { firsts = Lazy.from_val set;
    firsts_sym = Lazy.from_val [msg];
     accept_empty = Lazy.from_val empty;
     parse = fun blank str pos next key g ->
	     let a, str', pos' = try fn str pos with Give_up -> parse_error key msg str pos in
	     let str'', pos'' = blank str' pos' in 
	     single str'' pos'' (g str pos str' pos' a) })

let char : char -> 'a -> 'a grammar 
  = fun s a -> 
    let set = singleton s in
    let s' = String.make 1 s in
    { firsts = Lazy.from_val set;
      firsts_sym = Lazy.from_val [s'];
      accept_empty = Lazy.from_val false;
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN	
	  Printf.eprintf "%d: Char(%s) %s\n%!" pos (Char.escaped s) (let c, _,_ = read str pos in Char.escaped c)
END;
          let c, str', pos' = read str pos in
	  if c <> s then parse_error key s' str pos;
	  let str'', pos'' = blank str' pos' in
	  single str'' pos'' (g str pos str' pos' a)
    }

let string : string -> 'a -> 'a grammar 
  = fun s a -> 
   let len_s = String.length s in
    if len_s = 0 then failwith "string: illegal empty string";
    let set = singleton s.[0] in
    { firsts = Lazy.from_val set;
      firsts_sym = Lazy.from_val [s];
      accept_empty = Lazy.from_val false;
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN	 
	  Printf.eprintf "%d: String(%s) %s\n%!" pos (String.escaped s) (let c, _,_ = read str pos in Char.escaped c)
END;
          let str' = ref str in
	  let pos' = ref pos in
	  for i = 0 to len_s - 1 do
	    let c, _str', _pos' = read !str' !pos' in
	    if c <> s.[i] then parse_error key s str pos;
	    str' := _str'; pos' := _pos'
	  done;
	  let str' = !str' and pos' = !pos' in 
	  let str'', pos'' = blank str' pos' in
	  single str'' pos'' (g str pos str' pos' a)
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
    { firsts = Lazy.from_val set;
      firsts_sym = Lazy.from_val [name];
      accept_empty = Lazy.from_val (Str.string_match r "" 0);
      parse = 
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d: Regexp(%s) %s\n%!" pos name (let c, _,_ = read str pos in Char.escaped c)
END;
	  if string_match r (line str) pos then
	    let f n = matched_group n (line str) in
	    let pos' = match_end () in
	    let str'', pos'' = blank str pos' in
	    let res = single str'' pos'' (g str pos str pos' (a f)) in
IFDEF DEBUG THEN
	    Printf.eprintf "%d: Regexp OK\n%!" pos'
END;
	    res
	  else (
IFDEF DEBUG THEN
	    Printf.eprintf "%d: Regexp Failed\n%!" pos
END;
	    parse_error key name str pos)
    }


let list_regexp : string -> ((int -> string) -> 'a) -> 'a list grammar
  = fun r f -> regexp r (fun groupe -> [f groupe])

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

let union_firsts_sym l1 l2 =
  if accept_empty l1 then 
    firsts_sym l1 @ firsts_sym l2
  else
    firsts_sym l1

let union'' l s =
  match s with None -> None | Some s -> Some (union s (firsts l))

let union' l s =
  if accept_empty l then union'' l s
  else Some (firsts l)

let sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 l2 f ->
  let flag = ref false in
    { firsts = lazy (union_firsts l1 l2);
      firsts_sym = lazy (union_firsts_sym l1 l2);
      accept_empty = mk_empty flag (fun () -> accept_empty l1 && accept_empty l2);
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d %a: Sequence\n%!" pos print_charset next
END;
	  let la = l1.parse blank str pos (union' l2 next) key (fun _ _ _ _ x -> x) in
	  let res = PosMap.fold (fun (str',pos') a acc ->
IFDEF DEBUG THEN
	    Printf.eprintf "%d,%d: Sequence step 2\n%!" pos pos'
END;
	    try
	      let res = merge_map1 str pos acc (l2.parse blank str' pos' next key
							 (fun _ _ l' pos' x -> g str pos l' pos' (f a x))) in
IFDEF DEBUG THEN
	      Printf.eprintf "%d,%d: Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc
	  ) la PosMap.empty in
	  if res = PosMap.empty then raise Give_up else res
    }

let merge_sequence : ('c -> 'c -> 'c) -> 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun merge l1 l2 f ->
  let merge (l, pos, l', pos', a) (_, _, _, _, b) = (l, pos, l', pos', merge a b) in
  let flag = ref false in
    { firsts = lazy (union_firsts l1 l2);
      firsts_sym = lazy (union_firsts_sym l1 l2);
      accept_empty = mk_empty flag (fun () -> accept_empty l1 && accept_empty l2);
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d %a: Merge Sequence\n%!" pos print_charset next
END;
	  let la = l1.parse blank str pos (union' l2 next) key (fun _ _ _ _ x -> x) in
	  let res = PosMap.fold (fun (str', pos') a acc ->
IFDEF DEBUG THEN
	    Printf.eprintf "%d,%d: Merge Sequence step 2\n%!" pos pos'
END;
	    try	      
	      let res = merge_map2 merge acc (l2.parse blank str' pos' next key (fun _ _ l' pos' x -> str, pos, l', pos', f a x)) in
IFDEF DEBUG THEN
	      Printf.eprintf "%d,%d: Merge Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc
	  ) la PosMap.empty in
	  if res = PosMap.empty then raise Give_up else PosMap.map (fun (l, pos, l', pos', x) -> g l pos l' pos' x) res
    }

let flat_map f l = List.fold_left (fun l x -> List.rev_append (f x) l) [] l 

let list_sequence : 'a list grammar -> 'b list grammar -> ('a -> 'b -> 'c) -> 'c list grammar =
  fun l1 l2 action ->
    merge_sequence List.append l1 l2 (fun al bl -> flat_map (fun a -> List.map (fun b -> action a b) bl) al)

let fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence l1 l2 (fun x f -> f x)

let sequence3 : 'a grammar -> 'b grammar -> 'c grammar -> ('a -> 'b -> 'c -> 'd) -> 'd grammar
  = fun l1 l2 l3 g ->
    sequence (sequence l1 l2 (fun x y z -> g x y z)) l3 (fun f -> f)

let dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun l1 f2 ->
  let flag = ref false in
    { firsts = lazy (firsts l1);
      firsts_sym = lazy (firsts_sym l1);
      accept_empty = mk_empty flag (fun () -> 
        let res = accept_empty l1 in 
	if res then failwith "initial rule must not parse empty sequence in dependent sequence";
	false);
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d: Dependent Sequence\n%!" pos
END;
	  let la = l1.parse blank str pos None key (fun  _ _ _ _ x -> x) in
	  let res = PosMap.fold (fun (str', pos') a acc ->
IFDEF DEBUG THEN
	    Printf.eprintf "%d,%d: Dependent Sequence step 2\n%!" pos pos'
END;
	    try
	      let res = merge_map1 str pos acc ((f2 a).parse blank str' pos' next key (fun _ _ l' pos' -> g str pos l' pos')) in
IFDEF DEBUG THEN
	      Printf.eprintf "%d,%d: Dependent Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc
	  ) la PosMap.empty in
	  if res = PosMap.empty then raise Give_up else res
    }

let iter : 'a grammar grammar -> 'a grammar 
  = fun g -> dependent_sequence g (fun x -> x)

let dependent_merge_sequence : ('b -> 'b -> 'b) -> 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun merge l1 f2 ->
  let merge (l, pos, l', pos', a) (_, _, _, _, b) = (l, pos, l', pos', merge a b) in
  let flag = ref false in
    { firsts = lazy (firsts l1);
      firsts_sym = lazy (firsts_sym l1);
      accept_empty = mk_empty flag (fun () -> 
        let res = accept_empty l1 in 
	if res then failwith "initial rule must not parse empty sequence in dependent sequence";
	false);
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d: Dependent Sequence\n%!" pos
END;
	  let la = l1.parse blank str pos None key (fun _ _ _ _ x -> x) in
	  let res = PosMap.fold (fun (str', pos') a acc ->
IFDEF DEBUG THEN
	    Printf.eprintf "%d,%d: Dependent Sequence step 2\n%!" pos pos'
END;
	    try
	      let res = merge_map2 merge acc ((f2 a).parse blank str' pos' next key (fun _ _ l' pos' x -> str, pos, l', pos', x)) in
IFDEF DEBUG THEN
	      Printf.eprintf "%d,%d: Dependent Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc
	  ) la PosMap.empty in
	  if res = PosMap.empty then raise Give_up else PosMap.map (fun (l, pos, l', pos', x) -> g l pos l' pos' x) res
    }

let iter_merge : ('a -> 'a -> 'a) -> 'a grammar grammar -> 'a grammar 
  = fun f g -> dependent_merge_sequence f g (fun x -> x)

let dependent_list_sequence : 'a list grammar -> ('a -> 'b list grammar) -> 'b list grammar
  = fun l1 f2 ->
  let flag = ref false in
  let merge (l, pos, l', pos', a) (_, _, _, _, b) = (l, pos, l', pos', a @ b) in
    { firsts = lazy (firsts l1);
      firsts_sym = lazy (firsts_sym l1);
      accept_empty = mk_empty flag (fun () -> 
        let res = accept_empty l1 in 
	if res then failwith "initial rule must not parse empty sequence in dependent sequence";
	false);
      parse =
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d: Dependent list Sequence\n%!" pos
END;
	  let la = l1.parse blank str pos None key (fun _ _ _ _ x -> x) in
	  let res = PosMap.fold (fun (str', pos') a acc -> List.fold_left (fun acc a ->
IFDEF DEBUG THEN
	    Printf.eprintf "%d,%d: Dependent list Sequence step 2\n%!" pos pos'
END;
	    try
	      let res = merge_map2 merge acc ((f2 a).parse blank str' pos' next key (fun _ _ l' pos' x -> (str, pos, l', pos', x))) in
IFDEF DEBUG THEN
	      Printf.eprintf "%d,%d: Dependent list Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc
	  ) acc a) la PosMap.empty in
	  if res = PosMap.empty then raise Give_up else PosMap.map (fun (l, pos, l', pos', x) -> g l pos l' pos' x) res
    }

let iter_list : 'a list grammar list grammar -> 'a list grammar 
  = fun l1 -> dependent_list_sequence l1 (fun x -> x)

(*
let dependent_list_sequence : 'a list grammar -> ('a -> 'b list grammar) -> 'b list grammar =
  costly ... should be avoided ?
*)

let change_layout : 'a grammar -> blank -> 'a grammar
  = fun l1 blank1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
    { firsts = lazy (firsts l1);
      firsts_sym = lazy (firsts_sym l1);
      accept_empty = Lazy.from_fun (fun () -> accept_empty l1);
      parse =
	fun blank str pos next key g  ->
	  PosMap.fold (fun (str', pos') a acc ->
		       let str', pos' = blank str' pos' in
		       PosMap.add (str', pos') a acc) 
		      (l1.parse blank1 str pos None key g)
		      PosMap.empty
    }


let option' : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
    { firsts = lazy (firsts l);
      firsts_sym = lazy (firsts_sym l);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next key g ->
	  let acc = if test next str pos then single str pos (g str pos str pos a) else PosMap.empty in
	  try
IFDEF DEBUG THEN
	    Printf.eprintf "%d %a: Option\n%!" pos print_charset next
END;
	    let res = merge_map1 str pos acc (l.parse blank str pos next key g) in
IFDEF DEBUG THEN
	    Printf.eprintf "%d: Option OK\n%!" pos
END;
	    res
	  with
	    Give_up ->
	      if acc = PosMap.empty then raise Give_up;
	      acc
    }

let merge_option : ('a -> 'a -> 'a) -> 'a -> 'a grammar -> 'a grammar
  = fun merge a l ->
    let merge (l, pos, l', pos', a) (_, _, _, _, b) = (l, pos, l', pos', merge a b) in
    { firsts = lazy (firsts l);
      firsts_sym = lazy (firsts_sym l);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next key g ->
	  let acc = if test next str pos then single str pos (str, pos, str, pos, a) else PosMap.empty in
	  try
IFDEF DEBUG THEN
	    Printf.eprintf "%d %a: Option\n%!" pos print_charset next
END;
	    let res = merge_map2 merge acc (l.parse blank str pos next key (fun l pos l' pos' x -> l, pos, l', pos', x)) in
IFDEF DEBUG THEN
	    Printf.eprintf "%d: Option OK\n%!" pos
END;
	    PosMap.map (fun (l, pos, l', pos', x) -> g l pos l' pos' x) res
	  with
	    Give_up ->
	      if acc = PosMap.empty then raise Give_up;
	      PosMap.map (fun (l, pos, l', pos', x) -> g l pos l' pos' x) acc
    }

let list_option' : 'a -> 'a list grammar -> 'a list grammar =
  (fun a l ->
    merge_option List.append [a] l)

let option : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
    { firsts = lazy (firsts l);
      firsts_sym = lazy (firsts_sym l);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next key g ->
	  try
IFDEF DEBUG THEN
	    Printf.eprintf "%d %a: Option\n%!" pos print_charset next
END;
	    let res = l.parse blank str pos next key g in
IFDEF DEBUG THEN
	    Printf.eprintf "%d: Option OK\n%!" pos
END;
	    res
	  with
	    Give_up ->
	      if test next str pos then single str pos (g str pos str pos a) else raise Give_up;
    }

let list_option : 'a -> 'a list grammar -> 'a list grammar =
  (fun a l -> option' [a] l)

let fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = lazy (firsts f1);
      firsts_sym = lazy (firsts_sym f1);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next key g ->
	  let next' = union'' f1 next in
	  let rec fn acc la =
	    PosMap.fold (fun (str', pos') a acc ->
IFDEF DEBUG THEN
	      Printf.eprintf "%d %a: Fixpoint\n%!" pos' print_charset next
END;
	      let acc = if test next str' pos' then merge_map1 str pos (single str' pos' (g str pos str' pos' a)) acc else acc in
	      (try
		 let r = f1.parse blank str' pos' next' key (fun _ _ _ _ f -> f a) in
		 fun () -> fn acc r
	       with
		 Give_up -> fun () -> acc) ()) la acc
	  in
	  let res = fn PosMap.empty (single str pos a) in
	  if res = PosMap.empty then raise Give_up;
	  res
    }

let merge_fixpoint : ('a -> 'a -> 'a) -> 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun merge a f1 ->
    let merge (l, pos, l', pos', a) (_, _, _, _, b) = (l, pos, l', pos', merge a b) in
    { firsts = lazy (firsts f1);
      firsts_sym = lazy (firsts_sym f1);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next key g ->
	  let next' = union'' f1 next in
	  let rec fn acc la =
	    PosMap.fold (fun (str', pos') a acc ->
IFDEF DEBUG THEN
	      Printf.eprintf "%d %a: Merge Fixpoint\n%!" pos' print_charset next
END;
	      let acc = if test next str' pos' then merge_map2 merge (single str' pos' (str, pos, str', pos', a)) acc else acc in
	      (try
		 let r = f1.parse blank str' pos' next' key (fun _ _ _ _ f -> f a) in
		 fun () -> fn acc r
	       with
		 Give_up ->
		   fun () -> acc) ()) la acc
	  in
	  let res = fn PosMap.empty (single str pos a) in
	  if res = PosMap.empty then raise Give_up;
	  PosMap.map (fun (l, pos, l', pos', x) -> g l pos l' pos' x) res
    }

let list_fixpoint' : 'a -> ('a -> 'a list) grammar -> 'a list grammar
  = fun a f1 ->
    merge_fixpoint List.append [a] (apply (fun f l -> flat_map f l) f1)

let fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = lazy (firsts f1);
      firsts_sym = lazy (firsts_sym f1);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next key g ->
	  let next' = union'' f1 next in
	  let rec fn acc la =
	    PosMap.fold (fun (str'', pos'') (str', pos', a) acc ->
IFDEF DEBUG THEN
	      Printf.eprintf "%d %a: Fixpoint\n%!" pos' print_charset next
END;
	      (try
		 let r = f1.parse blank str'' pos'' next' key (fun _ _ l' pos' f -> l', pos', f a) in
		 fun () -> fn acc r
	       with 
		 Give_up ->
		 if test next str'' pos'' then (fun () -> single str'' pos'' (g str pos str' pos' a)) else raise Give_up) ()) la acc
	  in
	  fn PosMap.empty (single str pos (str, pos, a))
    }

let list_fixpoint : 'a -> ('a -> 'a list) grammar -> 'a list grammar
  = fun a f1 ->
    fixpoint' [a] (apply (fun f l -> flat_map f l) f1)

let alternatives' : 'a grammar list -> 'a grammar 
  = fun ls ->
  let flag = ref false in
  { firsts = lazy (List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    firsts_sym = lazy (List.fold_left (fun s p -> s @ (firsts_sym p)) [] ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d: Alternatives'\n%!" pos
END;
	  let ls, rej = List.partition (fun g ->
				(accept_empty g) || 
				  let c, _, _ = read str pos in
				  Charset.get (firsts g) c) ls in
	  let rec fn acc = function
	    | [] ->
	       if acc = PosMap.empty then
		 let msgs = flat_map firsts_sym rej in
		 parse_errors key msgs str pos
	       else acc
	    | l::ls ->
IFDEF DEBUG THEN
	      Printf.eprintf "%d: Alternatives' step2 (remain %d)\n%!" pos (List.length ls);
END;
	      let acc = 
		try
		  let acc = merge_map1 str pos acc (l.parse blank str pos next key g) in
		  acc
		with
		  Give_up -> acc
	      in fn acc ls
	  in
	  fn PosMap.empty ls
    }

let merge_alternatives : ('a -> 'a -> 'a) -> 'a grammar list -> 'a grammar 
  = fun merge ls ->
  let flag = ref false in
  let merge (l, pos, l', pos', a) (_, _, _, _, b) = (l, pos, l', pos', merge a b) in
  { firsts = lazy (List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    firsts_sym = lazy (List.fold_left (fun s p -> s @ (firsts_sym p)) [] ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun blank str pos next key g ->
IFDEF DEBUG THEN
	  Printf.eprintf "%d: Merge Alternatives\n%!" pos
END;
	  let ls, rej = List.partition (fun g ->
				(accept_empty g) || 
				  let c, _, _ = read str pos in
				  Charset.get (firsts g) c) ls in
	  let rec fn acc = function
	    | [] -> 
	       if acc = PosMap.empty then
		 let msgs = flat_map firsts_sym rej in
		 parse_errors key msgs str pos
	       else PosMap.map (function (l,pos,l',pos',x) -> g l pos l' pos' x) acc
	    | l::ls ->
IFDEF DEBUG THEN
	      Printf.eprintf "%d: Merge Alternatives step2 (remain %d)\n%!" pos (List.length ls);
END;
	      let acc = 
		try
		  merge_map2 merge acc (l.parse blank str pos next key (fun l pos l' pos' x -> l, pos, l', pos', x))
		with
		  Give_up -> acc
	      in fn acc ls
	  in
	  fn PosMap.empty ls
    }

let list_alternatives' : 'a list grammar list -> 'a list grammar 
  = fun ls ->
    merge_alternatives List.append ls

let alternatives : 'a grammar list -> 'a grammar 
  = fun ls ->
  let flag = ref false in
  { firsts = lazy (List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    firsts_sym = lazy (List.fold_left (fun s p -> s @ (firsts_sym p)) [] ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun blank str pos next key g ->
IFDEF DEBUG THEN
          Printf.eprintf "%d,%d: Alternatives\n%!" (line_num str) pos;
END;
	  let ls, rej = List.partition (fun g ->
				(accept_empty g) || 
				  let c, _, _ = read str pos in
				  Charset.get (firsts g) c) ls in
	  let rec fn = function
	    | [] ->
	       let msgs = flat_map firsts_sym rej in
	       parse_errors key msgs str pos
	    | l::ls ->
IFDEF DEBUG THEN
              Printf.eprintf "%d: Alternatives step2 (remain %d)\n%!" pos (List.length ls);
END;
	      try
		l.parse blank str pos next key g
	      with
		Give_up -> 
IFDEF DEBUG THEN
                  Printf.eprintf "%d: Alternatives Give_up\n%!" pos;
END;
		  fn ls
	  in
	  fn ls
    }

let list_alternatives = alternatives'

let list_merge : ('a -> 'a -> 'a) -> 'a list grammar -> 'a grammar
  = fun f l ->
    apply (function
      [] -> raise Give_up
    | x::l -> List.fold_left f x l) l

let remove_duplicate l = 
  List.sort compare
	    (List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) [] l)

let parse_buffer grammar blank str = 
  let key = next_key () in
  let la = try 	  let str, pos = blank str 0 in
		  grammar.parse blank str pos None key (fun _ _ _ _ x -> x) 
	   with Give_up -> 
		let str, pos, msgs = Hashtbl.find max_hash key in
		raise (Parse_error (fname str, line_num str, pos, remove_duplicate msgs))
  in
  let la = PosMap.fold (fun pos a acc -> (pos,a)::acc) la [] in
  match la with
  | [] -> assert false
  | [n,r] -> r
  | (n,_)::_ -> assert false

let partial_parse_buffer grammar blank str pos = 
  let key = next_key () in
  let la = try 	  let str, pos = blank str pos in
		  grammar.parse blank str pos None key (fun _ _ _ _ x -> x)
 	   with Give_up -> 
		let str, pos, msgs = Hashtbl.find max_hash key in
		raise (Parse_error (fname str, line_num str, pos, remove_duplicate msgs))
  in
  let la = PosMap.fold (fun (_,pos) a acc -> (pos,a)::acc) la [] in
  match la with
  | [] -> assert false
  | [n,r] -> n,r
  | (n,_)::_ -> assert false

let partial_parse_string grammar blank name str = 
  let str = buffer_from_string name str in
  partial_parse_buffer grammar blank str

let parse_string grammar blank name str = 
  let str = buffer_from_string name str in
  parse_buffer grammar blank str

let parse_channel grammar blank name ic  =
  let str = buffer_from_channel name ic in
  parse_buffer grammar blank str

let parse_file grammar blank name  =
  let str = buffer_from_file name in
  parse_buffer grammar blank str



