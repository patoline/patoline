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
    if string_match r (line str) pos then
      let pos' = match_end () in
      if accept_newline then (
        let c, str'', pos'' = read str pos' in if c = '\n' then fn str'' pos''
                                             else str, pos')
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

type grouped = {
  blank: blank;
  err_info: err_info;
}

type next = {
  accepted_char: charset;
  first_syms : string_tree;
}

type ('a, 'b) continuation = buffer -> int -> buffer -> int -> buffer -> int -> 'a -> 'b

type 'a grammar = {
  mutable firsts : charset Lazy.t;
  mutable first_sym : string_tree Lazy.t;
  mutable accept_empty : bool Lazy.t;
  mutable parse : 'b. grouped -> buffer -> int -> next -> ('a, 'b) continuation -> 'b;
}

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

let accept_empty g = Lazy.force g.accept_empty
let firsts g = Lazy.force g.firsts
let first_sym g = Lazy.force g.first_sym
let next_sym g ={
                 accepted_char = firsts g;
                 first_syms = first_sym g;
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

let grammar_family ?(param_to_string=fun _ -> "<param>") name =
  let tbl = Hashtbl.create 101 in
  let definition = ref None in
  let seeds = ref [] in
  let record p = seeds := p::!seeds in
  let do_fix fn =
    while !seeds <> [] do
      let new_seeds = !seeds in
      seeds := [];
      List.iter (fun key ->
		 let g = Hashtbl.find tbl key in
		 set_grammar g (fn key)) new_seeds;
    done;
  in
  let gn = fun param ->
    try Hashtbl.find tbl param
    with Not_found ->
      record param;
      let g = declare_grammar (name ^ ":" ^ (param_to_string param)) in
      Hashtbl.add tbl param g;
      (match !definition with
       | Some f ->
          do_fix f;
       | None -> ());
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
        l.parse grouped str pos next 
	   (fun l c l' c' l'' c'' x -> 
	    let r = try f x with Give_up msg -> parse_error grouped (~!msg) l' c' in
	    g l c l' c' l'' c'' r);
    }

let delim : 'a grammar -> 'a grammar
  = fun l ->
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
        fun grouped str pos next g ->
        let cont l c l' c' l'' c'' x () = g l c l' c' l'' c'' x in
        l.parse grouped str pos next cont ()
    }

let merge : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'a grammar -> 'b grammar
  = fun unit merge l ->
    { firsts = lazy (firsts l);
      first_sym = lazy (first_sym l);
      accept_empty = lazy (accept_empty l);
      parse =
        fun grouped str pos next g ->
        let m = ref PosMap.empty in
        let cont l c l' c' l'' c'' x =
	  let x = try unit x with Give_up msg -> parse_error grouped (~!msg) l' c' in
          (try
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
                         res := Some (g str pos str' pos' str'' pos'' x);
                         raise Exit
                       with
                         Error -> ()) !m;
          match !res with
            None -> raise Error
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
          l.parse grouped str pos next (fun l c l' c' l'' c'' x ->
                                          g l c l' c' l'' c'' (
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
                  (fun l c l' c' l'' c'' x -> 
		   let r = try f x l c l' c' with Give_up msg -> parse_error grouped (~!msg) l' c' in
		   g l c l' c' l'' c'' r)
    }

let eof : 'a -> 'a grammar
  = fun a ->
    let set = singleton '\255' in
    { firsts = lazy set;
      first_sym = lazy (~~ "EOF");
      accept_empty = lazy false;
      parse =
        fun grouped str pos next g ->
          if get str pos = '\255' then g str pos str pos str pos a else parse_error grouped (~~ "EOF") str pos
    }

let empty : 'a -> 'a grammar = fun a ->
  { firsts = lazy empty_charset;
    first_sym = lazy Empty;
    accept_empty = lazy true;
    parse = fun grouped str pos next g -> g str pos str pos str pos a }

let debug : string -> unit grammar = fun msg ->
  { firsts = lazy empty_charset;
    first_sym = lazy Empty;
    accept_empty = lazy true;
    parse = fun grouped str pos next g ->
            let l = line str in
            let current = String.sub l pos (min (String.length l - pos) 10) in
            Printf.eprintf "%s(%d,%d): %S %a\n%!" msg (line_num str) pos current print_charset next.accepted_char;
            g str pos str pos str pos () }

let fail : string -> 'a grammar = fun msg ->
  { firsts = lazy empty_charset;
    first_sym = lazy Empty;
    accept_empty =  lazy false;
    parse = fun grouped str pos next g ->
             parse_error grouped (~~ msg) str pos }

let  black_box : (buffer -> int -> 'a * buffer * int) -> charset -> bool -> string -> 'a grammar =
  (fun fn set empty name ->
   { firsts = lazy set;
     first_sym = lazy (~~ name);
     accept_empty = lazy empty;
     parse = fun grouped str pos next g ->
             let a, str', pos' = try fn str pos with Give_up msg -> parse_error grouped (~! msg) str pos in
             let str'', pos'' = apply_blank grouped str' pos' in
             g str pos str' pos' str'' pos'' a })

let char : char -> 'a -> 'a grammar
  = fun s a ->
    let set = singleton s in
    let s' = String.make 1 s in
    { firsts = lazy set;
      first_sym = lazy (~~ s');
      accept_empty = lazy false;
      parse =
        fun grouped str pos next g ->
          let c, str', pos' = read str pos in
          if c <> s then parse_error grouped (~~ s') str pos;
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' a
    }

let any : char grammar
  = let set = del full_charset '\255' in
    { firsts = lazy set;
      first_sym = lazy (~~ "ANY");
      accept_empty = lazy false;
      parse =
        fun grouped str pos next g ->
          let c, str', pos' = read str pos in
          if c = '\255' then parse_error grouped (~~ "ANY") str pos;
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' c
    }

let string : string -> 'a -> 'a grammar
  = fun s a ->
   let len_s = String.length s in
    let set = if len_s > 0 then singleton s.[0] else empty_charset in
    { firsts = lazy set;
      first_sym = lazy (if len_s > 0 then (~~ s) else Empty);
      accept_empty = lazy (len_s = 0);
      parse =
        fun grouped str pos next g ->
          let str' = ref str in
          let pos' = ref pos in
          for i = 0 to len_s - 1 do
            let c, _str', _pos' = read !str' !pos' in
            if c <> s.[i] then parse_error grouped (~~s) str pos;
            str' := _str'; pos' := _pos'
          done;
          let str' = !str' and pos' = !pos' in
          let str'', pos'' = apply_blank grouped str' pos' in
          g str pos str' pos' str'' pos'' a
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
    { firsts = lazy set;
      first_sym = lazy (~~ name);
      accept_empty = lazy (Str.string_match r "" 0);
      parse =
        fun grouped str pos next g ->
        let l = line str in
        if pos > String.length l then
          parse_error grouped (~~ name) str pos;
        if string_match r l pos then
          let f n = matched_group n l in
          let pos' = match_end () in
	  let res = try a f with Give_up msg -> parse_error grouped (~!msg) str pos' in
          let str'', pos'' = apply_blank grouped str pos' in
          g str pos str pos' str'' pos'' res
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
  { accepted_char = union next.accepted_char (firsts gram);
    first_syms = next.first_syms @@ first_sym gram;
  }

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
                   (fun str pos str0' pos0' str'' pos'' a ->
                    l2.parse grouped str'' pos'' next
                             (fun str0 pos0 str' pos' str'' pos'' x ->
                              let str', pos' = if str' == str0 && pos' == pos0 then str0', pos0' else str', pos' in
			      let res = try f a x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                              g str pos str' pos' str'' pos'' res))
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
                   (fun str pos str0' pos0' str'' pos'' a ->
                    l2.parse grouped str'' pos'' next
                             (fun str0 pos0 str' pos' str'' pos'' b ->
                              let str', pos' = if str' == str0 && pos' == pos0 then str0', pos0' else str', pos' in
			      let res = try f a b str pos str' pos' with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                              g str pos str' pos' str'' pos'' res))
    }

let fsequence : 'a grammar -> ('a -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence l1 l2 (fun x f -> f x)

let fsequence_position : 'a grammar -> ('a -> buffer -> int -> buffer -> int -> 'b) grammar -> 'b grammar
  = fun l1 l2 -> sequence_position l1 l2 (fun x f -> f x)

let sequence3 : 'a grammar -> 'b grammar -> 'c grammar -> ('a -> 'b -> 'c -> 'd) -> 'd grammar
  = fun l1 l2 l3 g ->
    sequence (sequence l1 l2 (fun x y z -> g x y z)) l3 (fun f -> f)

let all_next = 
  { accepted_char = full_charset; 
    first_syms = Empty }

let dependent_sequence : 'a grammar -> ('a -> 'b grammar) -> 'b grammar
  = fun l1 f2 ->
  let ae = lazy (accept_empty l1) in
    { firsts = lazy (if Lazy.force ae then firsts l1 else full_charset);
      first_sym = lazy (first_sym l1);
      accept_empty = ae;
      parse =
        fun grouped str pos next g ->
          l1.parse grouped str pos all_next
                   (fun str pos str0' pos0' str'' pos'' a ->
		    let g2 = try f2 a with Give_up msg -> parse_error grouped (~!msg) str0' pos0' in
                    g2.parse grouped str'' pos'' next
                          (fun str0 pos0 str' pos' str'' pos'' b ->
                              let str', pos' = if str' == str0 && pos' == pos0 then str0', pos0' else str', pos' in
                              g str pos str' pos' str'' pos'' b))
    }

let iter : 'a grammar grammar -> 'a grammar
  = fun g -> dependent_sequence g (fun x -> x)

let change_layout : ?new_blank_before:bool -> ?old_blank_after:bool -> 'a grammar -> blank -> 'a grammar
  = fun ?(new_blank_before=true) ?(old_blank_after=true) l1 blank1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
    { firsts = lazy (firsts l1);
      first_sym = lazy (first_sym l1);
      accept_empty = lazy (accept_empty l1);
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

let ignore_next_blank : 'a grammar -> 'a grammar
  = fun l1 ->
    (* if not l1.ready then failwith "change_layout: illegal recursion"; *)
    { firsts = lazy (firsts l1);
      first_sym = lazy (first_sym l1);
      accept_empty = lazy (accept_empty l1);
      parse =
        fun grouped str pos next g ->
          l1.parse grouped str pos all_next (fun s p s' p' s'' p'' -> g s p s' p' s' p')
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
          | Error ->
            g str pos str pos str pos a
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
                    (fun s p s' p' s'' p'' x () -> g s p s' p' s'' p'' x)
          with
          | Error ->
            fun () -> g str pos str pos str pos a) ()
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
          let rec fn str' pos' str'' pos'' x =
            if test grouped next str'' pos'' then
              try
                f1.parse grouped str'' pos'' next'
                         (fun _ _  str' pos' str'' pos'' f ->
			  let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                          fn str' pos' str'' pos'' res)
              with
              | Error -> g str pos str' pos' str'' pos'' x
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' f ->
			let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                        fn str' pos' str'' pos'' res)
          in fn str pos str pos a
    }

let fixpoint' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = lazy (firsts f1);
      first_sym = lazy (first_sym f1);
      accept_empty = lazy true;
      parse =
        fun grouped str pos next g ->
          let next' = union'' f1 next in
          let rec fn str' pos' str'' pos'' x () =
            if test grouped next str'' pos'' then
              (try
                  f1.parse grouped str'' pos'' next'
                           (fun _ _ str' pos' str'' pos'' f ->
			    let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                            fn str' pos' str'' pos'' res)
              with
              | Error -> fun () -> g str pos str' pos' str'' pos'' x) ()
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' f ->
			let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                        fn str' pos' str'' pos'' res) ()
          in
          fn str pos str pos a ()
    }

let fixpoint1 : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = lazy (firsts f1);
      first_sym = lazy (first_sym f1);
      accept_empty = lazy true;
      parse =
        fun grouped str pos next g ->
          let next' = union'' f1 next in
          let rec fn str' pos' str'' pos'' x =
            if test grouped next str'' pos'' then
              try
                f1.parse grouped str'' pos'' next'
                         (fun _ _ str' pos' str'' pos'' f ->
			  let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                          fn str' pos' str'' pos'' res)
              with
              | Error -> g str pos str' pos' str'' pos'' x
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' f ->
			let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                        fn str' pos' str'' pos'' res)
          in f1.parse grouped str pos next'
                         (fun _ _  str' pos' str'' pos'' f ->
			  let res = try f a with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                          fn str' pos' str'' pos'' res)
    }


let fixpoint1' : 'a -> ('a -> 'a) grammar -> 'a grammar
  = fun a f1 ->
    { firsts = lazy (firsts f1);
      first_sym = lazy (first_sym f1);
      accept_empty = lazy true;
      parse =
        fun grouped str pos next g ->
          let next' = union'' f1 next in
          let rec fn str' pos' str'' pos'' x () =
            if test grouped next str'' pos'' then
              (try
                  f1.parse grouped str'' pos'' next'
                           (fun _ _ str' pos' str'' pos'' f ->
			    let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                            fn str' pos' str'' pos'' res)
              with
              | Error -> fun () -> g str pos str' pos' str'' pos'' x) ()
            else
              f1.parse grouped str'' pos'' next'
                       (fun _ _ str' pos' str'' pos'' f ->
			let res = try f x with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                        fn str' pos' str'' pos'' res) ()
          in
          f1.parse grouped str pos next'
                           (fun _ _ str' pos' str'' pos'' f ->
			    let res = try f a with Give_up msg -> parse_error grouped (~!msg) str' pos' in
                            fn str' pos' str'' pos'' res) ()
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
	  if ls = [] then raise Error else
          let rec fn = function
            | [l] -> l.parse grouped str pos next g
            | l::ls ->
              (try
                l.parse grouped str pos next g
              with
                Error -> fn ls)
	    | _ -> assert false
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
	  if ls = [] then raise Error else
          let rec fn = function
            | [l] ->
                l.parse grouped str pos next
                        (fun s p s' p' s'' p'' x () ->  g s p s' p' s'' p'' x)
            | l::ls ->
              (try
                l.parse grouped str pos next
                        (fun s p s' p' s'' p'' x () ->  g s p s' p' s'' p'' x)
              with
                Error -> fn ls)
	    | _ -> assert false
          in
          fn ls ()
    }

let parse_buffer grammar blank str =
  let grammar = sequence grammar (eof ()) (fun x _ -> x) in
  let grouped = { blank;
                  err_info = {max_err_pos = -1;
                              max_err_buf = str;
                              max_err_col = -1;
                              err_msgs = Empty };
                }
  in
  let str, pos = apply_blank grouped str 0 in
  try
      grammar.parse grouped str pos all_next (fun _ _ _ _ _ _ x -> x)
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
                }
  in
  let cont l c l' c' l'' c'' x = (l'',c'',x) in
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
