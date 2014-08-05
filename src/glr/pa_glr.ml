open Camlp4.PreCast

module Id : Camlp4.Sig.Id =
    struct
      let name = "glr"
      let version = "0.1"
    end

type ('a,'b) action =
  | Default 
  | Normal of 'a
  | DepSeq of 'b

module Extension (Syntax : Camlp4.Sig.Camlp4Syntax) =
	struct
include Syntax

let glr_let = Gram.Entry.mk "glr_let"
let glr_rule = Gram.Entry.mk "glr_rule"
let glr_rules = Gram.Entry.mk "glr_rules"
let glr_rules_aux = Gram.Entry.mk "glr_rules"
let glr_option = Gram.Entry.mk "glr_option"
let glr_cond = Gram.Entry.mk "glr_cond"
let glr_left_member = Gram.Entry.mk "glr_left_member"
let glr_sequence = Gram.Entry.mk "glr_sequence"
let glr_opt_expr = Gram.Entry.mk "glr_opt_expr"
let glr_ident = Gram.Entry.mk "glr_ident"
let glr_action = Gram.Entry.mk "glr_action"

let glr_list_rule = Gram.Entry.mk "glr_list_rule"
let glr_list_rules = Gram.Entry.mk "glr_list_rules"
let glr_list_rules_aux = Gram.Entry.mk "glr_list_rules"
let glr_list_sequence = Gram.Entry.mk "glr_list_sequence"
let glr_list_left_member = Gram.Entry.mk "glr_list_left_member"

let do_locate = ref None

let mkpatt _loc (id, (p:Ast.patt option)) = match p, !do_locate with 
    None, _ -> <:patt<$lid:id$>>
  | Some p, None -> <:patt< ($p$ as $lid:id$) >>
  | Some p, Some _ -> <:patt< ((_, $p$) as $lid:id$) >>
				  
let rec apply _loc ids e =
  let e = match !do_locate with
      None -> e
    | Some(_,merge) ->
      match ids with
      | [] -> e
      | [id,_] -> <:expr<let $lid:"_loc"$ = $lid:"_loc_"^id$ in $e$>>
      | (first,_)::ids ->
	let (last,_) = List.hd (List.rev ids) in
	<:expr<let $lid:"_loc"$ = $merge$ $lid:"_loc_"^first$ $lid:"_loc_"^last$ in $e$>>
  in
  List.fold_left (fun e id -> 
    match !do_locate with
      None ->
	<:expr<fun $mkpatt _loc id$ -> $e$>>
    | Some(_) ->  
      <:expr<fun $mkpatt _loc id$ -> let $lid:"_loc_"^fst id$ = fst $lid:fst id$ in let $lid:fst id$ = snd $lid:fst id$ in $e$>>
  ) e (List.rev ids)

let filter _loc r =
  match !do_locate with
    None -> r
  | Some(f,_) -> <:expr<$f$ $r$>>
 
let apply_option _loc opt e = 
  filter _loc (match opt with
    `Once -> e
  | `Option d ->
    (match d with None ->
      <:expr<Glr.option None (Glr.apply (fun x -> Some x) $e$)>>
    | Some d ->
      <:expr<Glr.option $d$ $e$>>)
  | `OptionPrime d ->
    (match d with None ->
      <:expr<Glr.option' None (Glr.apply (fun x -> Some x) $e$)>>
    | Some d ->
      <:expr<Glr.option' $d$ $e$>>)
  | `Fixpoint d ->
    (match d with None ->
      <:expr<Glr.apply List.rev (Glr.fixpoint [] (Glr.apply (fun x l -> [x :: l]) $e$))>>
    | Some d ->
      <:expr<Glr.fixpoint $d$ $e$>>)
  | `FixpointPrime d ->
    (match d with None ->
      <:expr<Glr.apply List.rev (Glr.fixpoint' [] (Glr.apply (fun x l -> [x :: l]) $e$))>>
    | Some d ->
      <:expr<Glr.fixpoint $d$ $e$>>)
  | `Fixpoint1 d ->
   (match d with None ->
       <:expr<Glr.sequence $e$ (Glr.fixpoint [] (Glr.apply (fun x l -> [x :: l]) $e$)) (fun e l -> [e::List.rev l])>>
   | Some d ->
      <:expr<Glr.dependent_sequence $e$ (fun x -> Glr.fixpoint (x $d$) $e$)>>)
  | `Fixpoint1Prime d ->
   (match d with None ->
      <:expr<Glr.sequence $e$ (Glr.fixpoint' [] (Glr.apply (fun x l -> [x :: l]) $e$)) (fun e l -> [e::List.rev l])>>
   | Some d ->
      <:expr<Glr.dependent_sequence $e$ (fun x -> Glr.fixpoint' (x $d$) $e$)>>))

let apply_list_option _loc opt e = 
  filter _loc (match opt with
    `Once -> e
  | `Option d ->
    (match d with None ->
      <:expr<Glr.option [] $e$>>
    | Some d ->
      <:expr<Glr.option [$d$] $e$>>)
  | `OptionPrime d ->
    (match d with None ->
      <:expr<Glr.option' [] $e$>>
    | Some d ->
      <:expr<Glr.option' [$d$] $e$>>)
  | `Fixpoint d ->
    (match d with None ->
      <:expr<Glr.apply (List.map List.rev) (Glr.list_fixpoint [] (Glr.apply (fun x l -> List.map (fun x -> [x :: l]) x) $e$))>>
    | Some d ->
      <:expr<Glr.list_fixpoint $d$ $e$>>)
  | `FixpointPrime d ->
    (match d with None ->
      <:expr<Glr.apply (List.map List.rev) (Glr.list_fixpoint' [] (Glr.apply (fun x l -> List.map (fun x -> [x :: l]) x) $e$))>>
    | Some d ->
      <:expr<Glr.list_fixpoint' $d$ $e$>>)
  | `Fixpoint1 d ->
   (match d with None ->
       <:expr<Glr.list_sequence $e$ (Glr.list_fixpoint [] (Glr.apply (fun x l -> List.map (fun x -> [x :: l]) x) $e$)) (fun e l -> [e::List.rev l])>>
   | Some d ->
      <:expr<Glr.list_dependent_sequence $e$ (fun x -> Glr.list_fixpoint (x $d$) $e$)>>)

  | `Fixpoint1Prime d ->
   (match d with None ->
      <:expr<Glr.list_sequence $e$ (Glr.list_fixpoint' [] (Glr.apply (fun x l -> List.map (fun x -> [x :: l]) x) $e$)) (fun e l -> [e::List.rev l])>>
   | Some d ->
      <:expr<Glr.list_dependent_sequence $e$ (fun x -> Glr.list_fixpoint' (x $d$) $e$)>>))
	 
let default_action _loc l =
  let l = List.filter (function (("_",_),_,_) -> false | _ -> true) l in
  let rec fn =
    function
      [] -> failwith "No default action can be found"
    | [(id,_),_,_] -> <:expr<$lid:id$>>
    | ((id,_),_,_)::l -> <:expr<$lid:id$, $fn l$ >>
  in fn l


EXTEND Gram
  expr: LEVEL "simple" [ [
    "glr_locate"; filter = expr LEVEL "simple"; merge = expr LEVEL "simple" ->
      do_locate := Some(filter,merge); <:expr<()>>
  ] ];

  expr: LEVEL "simple" [ [
    "glr"; p = glr_rules; "end" -> p 
  | "glr"; "*"; p = glr_list_rules; "end" -> p 
  ] ];


  glr_option: [ [
    -> `Once
  | "*"; e =  glr_opt_expr -> `Fixpoint e
  | "**"; e =  glr_opt_expr -> `FixpointPrime e
  | "+"; e =  glr_opt_expr -> `Fixpoint1 e
  | "++"; e =  glr_opt_expr-> `Fixpoint1Prime e
  | "?"; e =  glr_opt_expr-> `Option e
  | "??"; e =  glr_opt_expr-> `OptionPrime e
  ] ];

  glr_rules_aux: [ [
    OPT "|" ; l = LIST1 glr_rule SEP "|" ->
      match l with
	[] -> assert false
      | [e] -> e
      | l -> 
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def <:expr<[$x$::$y$]>>
          | Some c -> 
	      def <:expr<let y = $y$ in if $c$ then [$x$::y] else y>>
	) l <:expr<[]>> in
	(fun x -> x), None, <:expr<Glr.alternatives $l$ >>
  ] ];

  glr_list_rules_aux: [ [
    OPT "|" ; l = LIST1 glr_list_rule SEP "|" ->
      match l with
	[] -> assert false
      | [e] -> e
      | l -> 
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def <:expr<[$x$::$y$]>>
          | Some c -> 
	      def <:expr<let y = $y$ in if $c$ then [$x$::y] else y>>
	) l <:expr<[]>> in
	(fun x -> x), None, <:expr<Glr.list_alternatives $l$ >>
  ] ];

  glr_rules: [ [
    l = LIST1 glr_rules_aux SEP "else" ->
      match l with
	[] -> assert false
      | [def, cond,e] -> (
	match cond with
	  None -> def e
        | Some c -> 
	      def <:expr<if $c$ then $e$ else Glr.fail>>)
      | l -> 
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def <:expr<[$x$::$y$]>>
          | Some c -> 
	      def <:expr<let y = $y$ in if $c$ then [$x$::y] else y>>
	) l <:expr<[]>> in
	<:expr<Glr.alternatives' $l$ >>
  ] ];

  glr_list_rules: [ [
    l = LIST1 glr_list_rules_aux SEP "else" ->
      match l with
	[] -> assert false
      | [def,cond,e] ->  (
	match cond with
	  None -> def e
        | Some c -> 
	  def <:expr<if $c$ then $e$ else Glr.fail>>)
      | l -> 
	let l = List.fold_right (fun (def,cond,x) y -> 
	  match cond with
	    None ->
	      def <:expr<[$x$::$y$]>>
          | Some c -> 
	      def <:expr<let y = $y$ in if $c$ then [$x$::y] else y>>
	) l <:expr<[]>> in
	<:expr<Glr.list_alternatives' $l$ >>
  ] ];
  
  glr_action: [ [
    "->"; action = expr -> Normal action
  | "->>"; (def, cond, r) = glr_rule -> DepSeq (def, cond, r)
  | -> Default
  ] ];

  glr_cond: [ [
    "when"; c = expr -> Some c
  | -> None
  ] ];

  glr_let: [ [
    "let"; r = opt_rec; bi = binding; "in"; l = glr_let -> fun x -> <:expr< let $rec:r$ $bi$ in $l x$ >>
  | -> fun x -> x
  ] ];

  glr_rule: [ [
    def = glr_let ; l = glr_left_member; condition = glr_cond; action = glr_action ->
    let iter, action = match action with
	Normal a -> false, a
      | Default -> false, default_action _loc l
      | DepSeq(def, cond, a) -> true, match cond with None -> def a | Some cond -> def <:expr< if $cond$ then $a$ else fail ()>>
    in
    let rec fn ids l = match l with
      [] -> assert false
    | [id,e,opt] ->
      let e = apply_option _loc opt e in
      <:expr<Glr.apply $apply _loc (id::ids) action$ $e$>>
    | [ (id,e,opt); (id',e',opt') ] ->
      let e = apply_option _loc opt e in
      let e' = apply_option _loc opt' e' in
      <:expr<Glr.sequence $e'$ $e$ $apply _loc (id'::id::ids) action$>>
    | (id,e,opt) :: ls ->
      let e = apply_option _loc opt e in      
      <:expr<Glr.sequence $fn (id::ids) ls$ $e$ (fun x -> x)>>
    in
    let res = fn [] (List.rev l) in
    let res = if iter then <:expr<iter $res$>> else res in
    def, condition, res
  ] ];

  glr_list_rule: [ [
    def = glr_let ; l = glr_list_left_member; condition = glr_cond; action = glr_action ->
    let iter, action = match action with
	Normal a -> false, a
      | Default -> false, default_action _loc l
      | DepSeq(def, cond, a) -> true, match cond with None -> def a | Some cond -> def <:expr< if $cond$ then $a$ else fail ()>>
    in	
    let rec fn ids l = match l with
      [] -> assert false
    | [id,e,opt] ->
      let e = apply_list_option _loc opt e in
      <:expr<Glr.apply $apply _loc (id::ids) action$ $e$>>
    | [ (id,e,opt); (id',e',opt') ] ->
      let e = apply_list_option _loc opt e in
      let e' = apply_list_option _loc opt' e' in
      <:expr<Glr.list_sequence $e'$ $e$ $apply _loc (id'::id::ids) action$>>
    | (id,e,opt) :: ls ->
      let e = apply_list_option _loc opt e in      
      <:expr<Glr.list_sequence $fn (id::ids) ls$ $e$ (fun x -> x)>>
    in
    let res = fn [] (List.rev l) in
    let res = if iter then <:expr<iter_list $res$>> else res in
    def, condition, res
  ] ];

  glr_left_member: [ [
    id = glr_ident; s = glr_sequence; opt = glr_option -> [id, s, opt]
  | id = glr_ident; s = glr_sequence; opt = glr_option; l = glr_left_member -> (id, s, opt)::l
  ] ];

  glr_list_left_member: [ [
    id = glr_ident; s = glr_list_sequence; opt = glr_option -> [id, s, opt]
  | id = glr_ident; s = glr_list_sequence; opt = glr_option; l = glr_list_left_member -> (id, s, opt)::l
  ] ];

  glr_opt_expr: [ [
    -> None
  | "["; e = expr; "]" -> Some e
  ] ];

  glr_ident: [ [
    "(" ; p = patt ; ")" ; ":" -> 
	       (match p with <:patt< ($p$ as $lid:id$)>> -> id, Some p
                          | <:patt< ($lid:id$)>> -> id, None
			  | _ -> "_", Some p)
  | id = LIDENT; ":" -> id, None
  | -> "_", None
  ] ];

  glr_sequence: [ [
    "{"; r = glr_rules; "}" -> r

  | "EOF"; opt = glr_opt_expr ->
      let e = match opt with None -> <:expr<()>> | Some e -> e in
      <:expr<Glr.eof $e$>>

  | "EMPTY" ->
      <:expr<Glr.empty ()>>

  | "FAIL" ->
      <:expr<Glr.fail ()>>

  | "STR"; str = expr LEVEL "simple"; opt = glr_opt_expr ->
      let e = match opt with None -> <:expr<()>> | Some e -> e in
      <:expr< Glr.string $str$  $e$>>

  | "RE"; str = expr LEVEL "simple"; opt = glr_opt_expr ->
      let e = match opt with None -> <:expr<groupe 0>> | Some e -> <:expr<$e$>> in
      <:expr<Glr.regexp $str$ (fun groupe -> $e$)>>

  | e = expr LEVEL "simple" -> e

  ] ];

  glr_list_sequence: [ [
    "{"; r = glr_list_rules; "}" -> r

  | "EOF"; opt = glr_opt_expr ->
      let e = match opt with None -> <:expr<()>> | Some e -> e in
      <:expr<Glr.list_eof $e$>>

  | "STR"; str = expr LEVEL "simple"; opt = glr_opt_expr ->
      let e = match opt with None -> <:expr<()>> | Some e -> e in
      <:expr< Glr.list_string $str$  $e$>>

  | "RE"; str = expr LEVEL "simple"; opt = glr_opt_expr ->
      let e = match opt with None -> <:expr<groupe 0>> | Some e -> <:expr<$e$>> in
      <:expr<Glr.list_regexp $str$ (fun groupe -> $e$)>>

  | e = expr LEVEL "simple" -> e

  ] ];
	END
;;

end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Extension)
