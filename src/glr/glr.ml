open Str
open Charset

exception Ambiguity of int * int
exception Parse_error of int
exception Give_up

module Int = struct
  type t = int
  let compare = (-)
end

module IntMap = Umap.Make(Int)

let single pos a = IntMap.add pos a IntMap.empty

let rec merge_map1 = fun pos la lb ->
(*
    List.iter (fun (pos,a) -> Printf.printf "%d:%d" pos (Obj.magic a)) la;
    Printf.printf " ++ ";
    List.iter (fun (pos,a) -> Printf.printf "%d:%d" pos (Obj.magic a)) lb;
    Printf.printf "\n";
*)
  IntMap.union (fun n a b ->
    if a == b then a else raise (Ambiguity(pos,n)))
    la lb

let rec merge_map2 = fun fn la lb ->
  IntMap.union (fun n a b -> fn a b) la lb

type blank = string -> int -> int

let blank_regexp r str pos =
  if string_match r str pos then
    let pos' = match_end () in
    pos'
  else
    pos

type 'a grammar = {
  mutable firsts : charset Lazy.t;
  mutable accept_empty : bool Lazy.t;
  mutable parse : 'b. blank -> string -> int -> charset option -> ('a -> 'b) -> 'b IntMap.t ;
}

let accept_empty g = Lazy.force g.accept_empty
let firsts g = Lazy.force g.firsts

let test blank s str p =
  let r = 
    match s with
      None -> true
    | Some s -> 
      let p = blank str p in 
      if p >= String.length str then get s '\255' else get s str.[p]
  in
IFDEF DEBUG THEN
  Printf.fprintf stderr "test %a %d => %b\n%!" print_charset s p r
ENDIF;
  r

let declare_grammar () = {
  firsts = Lazy.from_fun (fun () -> failwith "not_ready");
  accept_empty = Lazy.from_fun (fun () -> failwith "not_ready");
  parse = (fun _ -> failwith "not_ready");
}

let set_grammar p1 p2 =
  p1.firsts <- p2.firsts;
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
    { firsts = Lazy.from_fun (fun () -> firsts l);
      accept_empty = Lazy.from_fun (fun () -> accept_empty l);
      parse =
	fun blank str pos next g ->
	  l.parse blank str pos next (fun x -> g (f x))
    }

let list_apply : ('a -> 'b) -> 'a list grammar -> 'b list grammar
  = fun f l -> apply (List.map f) l

let position : 'a grammar -> (int * int * 'a) grammar
  = fun l -> 
    { firsts = Lazy.from_fun (fun () -> firsts l);
      accept_empty = Lazy.from_fun (fun () -> accept_empty l);
      parse =
	fun blank str pos next g ->
	  let pos0 = blank str pos in
	  let r = l.parse blank str pos next (fun x -> x) in
	  IntMap.mapi (fun pos' x -> g (pos0, pos', x)) r
    }

let filter_position : 'a grammar -> (string -> int -> int -> 'b) -> ('b * 'a) grammar
  = fun l filter -> 
    { firsts = Lazy.from_fun (fun () -> firsts l);
      accept_empty = Lazy.from_fun (fun () -> accept_empty l);
      parse =
	fun blank str pos next g ->
	  let pos0 = blank str pos in
	  let r = l.parse blank str pos next (fun x -> x) in
	  IntMap.mapi (fun pos' x -> g (filter str pos0 pos', x)) r
    }

let eof : 'a -> 'a grammar
  = fun a ->
    let set = singleton '\255' in
    { firsts = Lazy.from_val set;
      accept_empty = Lazy.from_val false;
      parse =
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d: Eof\n%!" pos
ENDIF;
	  let pos' = blank str pos in
	  if pos' >= String.length str then single pos' (g a) else raise (Parse_error pos')
    }

let list_eof : 'a -> 'a list grammar = 
  fun x -> eof [x]

let empty : 'a -> 'a grammar = fun a ->
  { firsts = Lazy.from_val empty_charset;
    accept_empty = Lazy.from_val true;
    parse = fun blank str pos next g -> 
	    single pos (g a) }

let fail : unit -> 'a grammar = fun () ->
  { firsts = Lazy.from_val empty_charset;
    accept_empty =  Lazy.from_val false;
    parse = fun blank str pos next g -> 
	     raise (Parse_error pos) }

let  black_box : (string -> int -> 'a * int) -> charset -> bool -> 'a grammar =
  (fun fn set empty ->
   { firsts = Lazy.from_val set;
     accept_empty = Lazy.from_val empty;
     parse = fun blank str pos next g -> 
	     let a, pos = fn str pos in
	     single pos (g a) })

let string : string -> 'a -> 'a grammar 
  = fun s a -> 
    if String.length s = 0 then failwith "string: illegal empty string";
    let set = singleton s.[0] in
    { firsts = Lazy.from_val set;
      accept_empty = Lazy.from_val false;
      parse =
	fun blank str pos next g ->
	  let pos' = blank str pos in
IFDEF DEBUG THEN	 
	  Printf.fprintf stderr "%d,%d: String(%s) %s\n%!" pos pos' (String.escaped s) (if pos' < String.length str then Char.escaped str.[pos'] else "EOF")
END;
	  let len = String.length s in
	  if String.length str >= pos' + len && String.sub str pos' len = s then
	    single (pos'+len) (g a)
	  else
	    raise (Parse_error pos')
    }

let list_string : string -> 'a -> 'a list grammar
  = fun s a -> string s [a]

let regexp : string -> ((int -> string) -> 'a) -> 'a grammar
  = fun r0 a ->
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
      accept_empty = Lazy.from_val (Str.string_match r "" 0);
      parse = 
	fun blank str pos next g ->
	  let pos' = blank str pos in
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d,%d: Regexp(%s) %s\n%!" pos pos' (String.escaped r0) (if pos' < String.length str then Char.escaped str.[pos'] else "EOF")
END;
	  if string_match r str pos' then
	    let f n = matched_group n str in
	    let pos' = match_end () in
	    let res = single pos' (g (a f)) in
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d: Regexp OK\n%!" pos'
END;
	    res
	  else (
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d: Regexp Failed\n%!" pos'
END;
	    raise (Parse_error pos'))
    }

let list_regexp : string -> ((int -> string) -> 'a) -> 'a list grammar
  = fun r f -> regexp r (fun groupe -> [f groupe])

let mk_empty in_analysis fn =
  Lazy.from_fun (fun () ->
		 if !in_analysis then failwith "illegal left recursion";
		 in_analysis := true;
		 let r = fn () in
		 in_analysis := false;
		 r)

let mk_firsts fn = Lazy.from_fun fn 
		 
let union_firsts l1 l2 () =
  if accept_empty l1 then 
    union (firsts l1) (firsts l2)
  else
    (firsts l1)

let union'' l s =
  match s with None -> None | Some s -> Some (union s (firsts l))

let union' l s =
  if accept_empty l then union'' l s
  else Some (firsts l)

let sequence : 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun l1 l2 f ->
  let flag = ref false in
    { firsts = mk_firsts (union_firsts l1 l2);
      accept_empty = mk_empty flag (fun () -> accept_empty l1 && accept_empty l2);
      parse =
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d %a: Sequence\n%!" pos print_charset next
END;
	  let la = l1.parse blank str pos (union' l2 next) (fun x -> x) in
	  let best = ref pos in
	  let res = IntMap.fold (fun pos' a acc ->
	    if pos' > !best then best := pos';
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d,%d,%d: Sequence step 2\n%!" pos pos' !best
END;
	    try
	      let res = merge_map1 pos acc (l2.parse blank str pos' next (fun x -> g (f a x))) in
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d,%d: Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc | Parse_error pos' ->
	      if pos' > !best then best := pos';
	      acc
	  ) la IntMap.empty in
	  if res = IntMap.empty then raise (Parse_error !best) else res
    }

let merge_sequence : ('c -> 'c -> 'c) -> 'a grammar -> 'b grammar -> ('a -> 'b -> 'c) -> 'c grammar
  = fun merge l1 l2 f ->
  let flag = ref false in
    { firsts = mk_firsts (union_firsts l1 l2);
      accept_empty = mk_empty flag (fun () -> accept_empty l1 && accept_empty l2);
      parse =
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d %a: Merge Sequence\n%!" pos print_charset next
END;
	  let la = l1.parse blank str pos (union' l2 next) (fun x -> x) in
	  let best = ref pos in
	  let res = IntMap.fold (fun pos' a acc ->
	    if pos' > !best then best := pos';
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d,%d,%d: Merge Sequence step 2\n%!" pos pos' !best
END;
	    try	      
	      let res = merge_map2 merge acc (l2.parse blank str pos' next (fun x -> f a x)) in
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d,%d: Merge Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc | Parse_error pos' ->
	      if pos' > !best then best := pos';
	      acc
	  ) la IntMap.empty in
	  if res = IntMap.empty then raise (Parse_error !best) else IntMap.map g res
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
    { firsts = Lazy.from_fun (fun () -> firsts l1);
      accept_empty = mk_empty flag (fun () -> 
        let res = accept_empty l1 in 
	if res then failwith "initial rule must not parse empty sequence in dependent sequence";
	false);
      parse =
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d: Dependant Sequence\n%!" pos
END;
	  let la = l1.parse blank str pos None (fun x -> x) in
	  let best = ref pos in
	  let res = IntMap.fold (fun pos' a acc ->
	    if pos' > !best then best := pos';
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d,%d,%d: Dependant Sequence step 2\n%!" pos pos' !best
END;
	    try
	      let res = merge_map1 pos acc ((f2 a).parse blank str pos' next g) in
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d,%d: Dependant Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc | Parse_error pos' ->
	      if pos' > !best then best := pos';
	      acc
	  ) la IntMap.empty in
	  if res = IntMap.empty then raise (Parse_error !best) else res
    }

let dependent_merge_sequence : ('b -> 'b -> 'b) -> 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun merge l1 f2 ->
  let flag = ref false in
    { firsts = Lazy.from_fun (fun () -> firsts l1);
      accept_empty = mk_empty flag (fun () -> 
        let res = accept_empty l1 in 
	if res then failwith "initial rule must not parse empty sequence in dependent sequence";
	false);
      parse =
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d: Dependant Sequence\n%!" pos
END;
	  let la = l1.parse blank str pos None (fun x -> x) in
	  let best = ref pos in
	  let res = IntMap.fold (fun pos' a acc ->
	    if pos' > !best then best := pos';
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d,%d,%d: Dependant Sequence step 2\n%!" pos pos' !best
END;
	    try
	      let res = merge_map2 merge acc ((f2 a).parse blank str pos' next (fun x -> x)) in
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d,%d: Dependant Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc | Parse_error pos' ->
	      if pos' > !best then best := pos';
	      acc
	  ) la IntMap.empty in
	  if res = IntMap.empty then raise (Parse_error !best) else IntMap.map g res
    }

let dependent_list_sequence : 'a list grammar -> ('a -> 'b list grammar) -> 'b list grammar
  = fun l1 f2 ->
  let flag = ref false in
    { firsts = Lazy.from_fun (fun () -> firsts l1);
      accept_empty = mk_empty flag (fun () -> 
        let res = accept_empty l1 in 
	if res then failwith "initial rule must not parse empty sequence in dependent sequence";
	false);
      parse =
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d: Dependant list Sequence\n%!" pos
END;
	  let la = l1.parse blank str pos None (fun x -> x) in
	  let best = ref pos in
	  let res = IntMap.fold (fun pos' a acc -> List.fold_left (fun acc a ->
	    if pos' > !best then best := pos';
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d,%d,%d: Dependant list Sequence step 2\n%!" pos pos' !best
END;
	    try
	      let res = merge_map2 List.append acc ((f2 a).parse blank str pos' next (fun x -> x)) in
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d,%d: Dependant list Sequence step 2 OK\n%!" pos pos'
END;
	      res
	    with Give_up -> acc | Parse_error pos' ->
	      if pos' > !best then best := pos';
	      acc
	  ) acc a) la IntMap.empty in
	  if res = IntMap.empty then raise (Parse_error !best) else IntMap.map g res
    }

(*
let dependent_list_sequence : 'a list grammar -> ('a -> 'b list grammar) -> 'b list grammar =
  costly ... should be avoided ?
*)

let change_layout : 'a grammar -> blank -> 'a grammar
  = fun l1 blank1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
    { firsts = Lazy.from_fun (fun () -> firsts l1);
      accept_empty = Lazy.from_fun (fun () -> accept_empty l1);
      parse =
	fun blank str pos next g  ->
	  let pos = blank str pos in
	  l1.parse blank1 str pos None g
    }


let option : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
    { firsts = Lazy.from_fun (fun () -> firsts l);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next g ->
	  let acc = if test blank next str pos then single pos (g a) else IntMap.empty in
	  try
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d %a: Option\n%!" pos print_charset next
END;
	    let res = merge_map1 pos acc (l.parse blank str pos next g) in
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d: Option OK\n%!" pos
END;
	    res
	  with
	    Give_up ->
	      if acc = IntMap.empty then raise (Parse_error pos);
	      acc
	  | Parse_error pos as exc -> 
	      if acc = IntMap.empty then raise exc;
	      acc
    }

let merge_option : ('a -> 'a -> 'a) -> 'a -> 'a grammar -> 'a grammar
  = fun merge a l ->
    { firsts = Lazy.from_fun (fun () -> firsts l);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next g ->
	  let acc = if test blank next str pos then single pos a else IntMap.empty in
	  try
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d %a: Option\n%!" pos print_charset next
END;
	    let res = merge_map2 merge acc (l.parse blank str pos next (fun x -> x)) in
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d: Option OK\n%!" pos
END;
	    IntMap.map g res
	  with
	    Give_up ->
	      if acc = IntMap.empty then raise (Parse_error pos);
	      IntMap.map g acc
	  | Parse_error pos as exc -> 
	      if acc = IntMap.empty then raise exc;
	      IntMap.map g acc
    }

let list_option : 'a -> 'a list grammar -> 'a list grammar =
  (fun a l ->
    merge_option List.append [a] l)

let option' : 'a -> 'a grammar -> 'a grammar
  = fun a l ->
    { firsts = Lazy.from_fun (fun () -> firsts l);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next g ->
	  try
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d %a: Option\n%!" pos print_charset next
END;
	    let res = l.parse blank str pos next g in
IFDEF DEBUG THEN
	    Printf.fprintf stderr "%d: Option OK\n%!" pos
END;
	    res
	  with
	    Give_up ->
	      if test blank next str pos then single pos (g a) else raise (Parse_error pos);
	  | Parse_error pos' as exc ->
	      if test blank next str pos then single pos (g a) else raise exc
    }

let list_option' : 'a -> 'a list grammar -> 'a list grammar =
  (fun a l -> option' [a] l)

let fixpoint : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = Lazy.from_fun (fun () -> firsts f1);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next g ->
	  let next' = union'' f1 next in
	  let best = ref pos in
	  let rec fn acc la =
	    IntMap.fold (fun pos' a acc ->
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d %a: Fixpoint\n%!" pos' print_charset next
END;
	      let acc = if test blank next str pos' then merge_map1 pos (single pos' (g a)) acc else acc in
	      (try
		 let r = f1.parse blank str pos' next' (fun f -> f a) in
		 let r = IntMap.fold (fun pos a acc ->
IFDEF DEBUG THEN
                   Printf.fprintf stderr "%d: Fixpoint step 2\n%!" pos
END;
		   IntMap.add pos a acc) r IntMap.empty in
		 fun () -> fn acc r
	       with
		 Give_up ->
		   if pos' > !best then best := pos';
		   fun () -> acc
	       | Parse_error pos ->
		   if pos > !best then best := pos;
		   fun () -> acc) ()) la acc
	  in
	  let res = fn IntMap.empty (single pos a) in
	  if res = IntMap.empty then raise (Parse_error !best);
	  res
    }

let merge_fixpoint : ('a -> 'a -> 'a) -> 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun merge a f1 ->
    { firsts = Lazy.from_fun (fun () -> firsts f1);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next g ->
	  let next' = union'' f1 next in
	  let best = ref pos in
	  let rec fn acc la =
	    IntMap.fold (fun pos' a acc ->
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d %a: Merge Fixpoint\n%!" pos' print_charset next
END;
	      let acc = if test blank next str pos' then merge_map2 merge (single pos' a) acc else acc in
	      (try
		 let r = f1.parse blank str pos' next' (fun f -> f a) in
		 let r = IntMap.fold (fun pos a acc ->
IFDEF DEBUG THEN
                   Printf.fprintf stderr "%d: Merge Fixpoint step 2\n%!" pos
END;
		   IntMap.add pos a acc) r IntMap.empty in
		 fun () -> fn acc r
	       with
		 Give_up ->
		   if pos' > !best then best := pos';
		   fun () -> acc
	       | Parse_error pos ->
		   if pos > !best then best := pos;
		   fun () -> acc) ()) la acc
	  in
	  let res = fn IntMap.empty (single pos a) in
	  if res = IntMap.empty then raise (Parse_error !best);
	  IntMap.map g res
    }

let list_fixpoint : 'a -> ('a -> 'a list) grammar -> 'a list grammar
  = fun a f1 ->
    merge_fixpoint List.append [a] (apply (fun f l -> flat_map f l) f1)

let fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = Lazy.from_fun (fun () -> firsts f1);
      accept_empty = Lazy.from_val true;
      parse =
	fun blank str pos next g ->
	  let next' = union'' f1 next in
	  let rec fn acc la =
	    IntMap.fold (fun pos' a acc ->
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d %a: Fixpoint'\n%!" pos' print_charset next
END;
	      (try
		 let r = f1.parse blank str pos' next' (fun f -> f a) in
		 let r = IntMap.fold (fun pos a acc ->
IFDEF DEBUG THEN
                   Printf.fprintf stderr "%d: Fixpoint' step 2\n%!" pos
END;
		   IntMap.add pos a acc) r IntMap.empty in
		 fun () -> fn acc r
	       with 
		 Give_up ->
		 if test blank next str pos' then (fun () -> single pos' (g a)) else raise (Parse_error pos')
	       | Parse_error pos'' as exc ->
		 if test blank next str pos' then (fun () -> single pos' (g a)) else raise exc
	      ) ()) la acc
	  in
	  fn IntMap.empty (single pos a)
    }

let list_fixpoint' : 'a -> ('a -> 'a list) grammar -> 'a list grammar
  = fun a f1 ->
    fixpoint' [a] (apply (fun f l -> flat_map f l) f1)

let alternatives : 'a grammar list -> 'a grammar 
  = fun ls ->
  let flag = ref false in
  { firsts = mk_firsts (fun () -> List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d: Alternatives\n%!" pos
END;
	  let rec fn acc max_pos = function
	    | [] -> if acc = IntMap.empty then raise (Parse_error max_pos) else acc
	    | l::ls ->
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d, %d: Alternatives step2\n%!" pos max_pos;
END;
	      let acc, p = 
		try
		  let acc = merge_map1 pos acc (l.parse blank str pos next g) in
		  acc, max_pos
		with
		  Give_up -> acc, max_pos
		| Parse_error pos -> acc, max pos max_pos
	      in fn acc p ls
	  in
	  fn IntMap.empty pos ls
    }

let merge_alternatives : ('a -> 'a -> 'a) -> 'a grammar list -> 'a grammar 
  = fun merge ls ->
  let flag = ref false in
  { firsts = mk_firsts (fun () -> List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d: Merge Alternatives\n%!" pos
END;
	  let rec fn acc max_pos = function
	    | [] -> if acc = IntMap.empty then raise (Parse_error max_pos) else IntMap.map g acc
	    | l::ls ->
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d, %d: Merge Alternatives step2\n%!" pos max_pos;
END;
	      let acc, p = 
		try
		  let acc = merge_map2 merge acc (l.parse blank str pos next (fun x -> x)) in
		  acc, max_pos
		with
		  Give_up -> acc, max_pos
		| Parse_error pos -> acc, max pos max_pos
	      in fn acc p ls
	  in
	  fn IntMap.empty pos ls
    }

let list_alternatives : 'a list grammar list -> 'a list grammar 
  = fun ls ->
    merge_alternatives List.append ls

let alternatives' : 'a grammar list -> 'a grammar 
  = fun ls ->
  let flag = ref false in
  { firsts = mk_firsts (fun () -> List.fold_left (fun s p -> union s (firsts p)) empty_charset ls);
    accept_empty = mk_empty flag (fun () -> List.exists accept_empty ls);
    parse = 
	fun blank str pos next g ->
IFDEF DEBUG THEN
	  Printf.fprintf stderr "%d: Alternatives'\n%!" pos
END;
	  let rec fn max_pos = function
	    | [] -> raise (Parse_error max_pos)
	    | l::ls ->
IFDEF DEBUG THEN
	      Printf.fprintf stderr "%d, %d: Alternatives' step2\n%!" pos max_pos;
END;
	      try
		l.parse blank str pos next g
	      with
		Give_up -> fn max_pos ls
	      | Parse_error pos -> fn (max pos max_pos) ls
	  in
	  fn pos ls
    }

let list_alternatives' = alternatives'

let list_merge : ('a -> 'a -> 'a) -> 'a list grammar -> 'a grammar
  = fun f l ->
    apply (function
      [] -> raise Give_up
    | x::l -> List.fold_left f x l) l

let parse_string grammar blank str = 
  let la = try grammar.parse blank str 0 None (fun x -> x) with Give_up -> raise (Parse_error 0) in
  let la = IntMap.fold (fun pos a acc -> (pos,a)::acc) la [] in
  match la with
  | [] -> assert false
  | [n,r] -> r
  | (n,_)::_ -> assert false

let partial_parse_string grammar blank str pos = 
  let la = try grammar.parse blank str pos None (fun x -> x) with Give_up -> raise (Parse_error 0) in
  let la = IntMap.fold (fun pos a acc -> (pos,a)::acc) la [] in
  match la with
  | [] -> assert false
  | [n,r] -> n,r
  | (n,_)::_ -> assert false

(* works only for a real file because of in_channel_length *)
let parse_channel grammar blank ic  =
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  parse_string grammar blank s 


