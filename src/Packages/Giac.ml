
type giac =
  | Symbol of string
  | Number of string
  | Sum of giac * giac
  | Pro of giac * giac
  | Opp of giac
  | Inv of giac
  | Pow of giac * giac
  | App of giac * giac list
  | Ind of giac * giac list
  | Vec of giac list
  | Eq  of giac * giac

let rec zero = function
  | Vec l -> Vec (List.map zero l)
  | _ -> Number "0"

let int n = Number (string_of_int n)

type prio = PAtm | PPow | PPro | PSum | Pred

let pTop = Pred

let prev = function
  | PAtm -> PAtm
  | PPow -> PAtm
  | PPro -> PPow
  | PSum -> PPro
  | Pred -> PSum

let parser giac lvl =
  | s:''[0-9]+''                    when lvl = PAtm -> Number s
  | s:''[a-zA-Z_][a-zA-Z0-9_]*''    when lvl = PAtm -> Symbol s
  | "inv" '(' e:(giac pTop) ')'     when lvl = PAtm -> Inv(e)
  | e:(giac PAtm) '(' l:gs ')'      when lvl = PAtm ->
           if e = Symbol "inv" then Earley.give_up (); App(e,l)
  | e:(giac PAtm) '[' l:gs ']'      when lvl = PAtm -> Ind(e,l)
  | '(' e:(giac pTop) ')'           when lvl = PAtm -> e
  | '[' l:gs ']'                    when lvl = PAtm -> Vec(l)
  | e:(giac PAtm) '^' f:(giac PPow) when lvl = PPow -> Pow(e,f)
  | '-' e:(giac PPow)               when lvl = PPow -> Opp(e)
  | e:(giac PPro) '*' f:(giac PPow) when lvl = PPro -> Pro(e,f)
  | e:(giac PPro) '/' f:(giac PPow) when lvl = PPro -> Pro(e,Inv f)
  | e:(giac PSum) '+' f:(giac PPro) when lvl = PSum -> Sum(e,f)
  | e:(giac PSum) '-' f:(giac PPro) when lvl = PSum -> Sum(e,Opp f)
  | e:(giac PSum) "==" f:(giac PSum) when lvl = Pred -> Eq(e,f)
  | e:(giac (prev lvl))             when lvl > PAtm -> e

and gs =
  | EMPTY -> []
  | e:(giac pTop) l:{_:',' (giac pTop)}* -> e::l

let giac = giac pTop

let blank = Earley.blank_regexp ''[ \t\n\r]*''

let rec print lvl ch e =
  let pr fmt = Printf.fprintf ch fmt in
  let pp = prev in
  match e with
  | Symbol s | Number s -> pr "%s" s
  | Eq(e,f) ->
     let fmt : ('a,'b,'c) format = if lvl < Pred then "(%a == %a)" else "%a == %a" in
     pr fmt (print Pred) e (print (pp Pred)) f
  | Sum(e,Opp f) ->
     let fmt : ('a,'b,'c) format = if lvl < PSum then "(%a - %a)" else "%a - %a" in
     pr fmt (print PSum) e (print (pp PSum)) f
  | Sum(e,f) ->
     let fmt : ('a,'b,'c) format = if lvl < PSum then "(%a + %a)" else "%a + %a" in
     pr fmt (print PSum) e (print (pp PSum)) f
  | Pro(e,Inv f) ->
     let fmt : ('a,'b,'c) format = if lvl < PPro then "(%a / %a)" else "%a / %a" in
     pr fmt (print PPro) e (print (pp PPro)) f
  | Pro(e,f) ->
     let fmt : ('a,'b,'c) format = if lvl < PPro then "(%a * %a)" else "%a * %a" in
     pr fmt (print PPro) e (print (pp PPro)) f
  | Pow(e,f) ->
     let fmt : ('a,'b,'c) format = if lvl < PPow then "(%a ^ %a)" else "%a ^ %a" in
     pr fmt (print (pp PPow)) e (print PPow) f
  | Inv e ->
     pr "inv(%a)" (print pTop) e
  | Opp e ->
     let fmt : ('a1,'b1,'c1) format = if lvl < PPow then "(-%a)" else "-%a" in
     pr fmt (print (pp PPow)) e
  | App(e,l) ->
     pr "%a(%a)" (print PAtm) e prl l
  | Ind(e,l) ->
     pr "%a[%a]" (print PAtm) e prl l
  | Vec(l) ->
     pr "[%a]" prl l

and prl ch l =
  match l with
  | [] -> ()
  | [e] -> print pTop ch e
  | e::l -> Printf.fprintf ch "%a, %a" (print pTop) e prl l

let print = print pTop
let prev_prio = prev

open Macros
open Maths
open Document
open Box

let idvec n ls =
    List.for_all (function Vec l -> List.length l = n | _ -> false) ls

let id x = x

let rec use_times = function
  | Number _ -> true
  | Pow(e,f) -> use_times e
  | _ -> false

let rec gmath lvl e =
  let pp = prev_prio in
  match e with (* TODO derive et integrale multiple *)
  | App(Symbol("sqrt"), [f] ) ->
     <$ \sqrt{\gmath(pTop)(f)} $>
  | App(Symbol("derive"|"diff"), [f] ) ->
     <$ {\partial \gmath(pTop)(f)} \over {\partial \gmath(pTop)(Symbol "x") } $>
  | App(Symbol("derive"|"diff"), [f; Symbol(_) as s]) ->
     <$ {\partial \gmath(pTop)(f)} \over {\partial \gmath(pTop)(s) } $>
  | App(Symbol("int"|"integrate"|"integration"), [f] ) ->
     <$ \int \gmath(pTop)(f) d x $>
  | App(Symbol("int"|"integrate"|"integration"), [f; Symbol(_) as s] ) ->
     <$ \int \gmath(pTop)(f) d \gmath(pTop)(s) $>
  | App(Symbol("int"|"integrate"|"integration"), [f; Symbol(_) as s;a;b] ) ->
     <$ \int_{\gmath(pTop)(a)}^{\gmath(pTop)(b)} \gmath(pTop)(f) d \gmath(pTop)(s) $>
  | Number s | Symbol s ->
     [Maths.Ordinary (Maths.node (Maths.glyphs s))]
  | Eq(e,f) ->
     if (lvl < Pred) then <$ (\gmath(pp Pred)(e) = \gmath(pp Pred)(f)) $>
     else  <$ \gmath(pp Pred)(e) = \gmath(pp Pred)(f) $>
  | Sum(e,Opp f) ->
     if (lvl < PSum) then <$ (\gmath(PSum)(e) - \gmath(pp PSum)(f)) $>
     else  <$ \gmath(PSum)(e) - \gmath(pp PSum)(f) $>
  | Sum(e,f) ->
     if (lvl < PSum) then <$ (\gmath(PSum)(e) + \gmath(pp PSum)(f)) $>
     else  <$ \gmath(PSum)(e) + \gmath(pp PSum)(f) $>
  | Pro(e,Inv f) ->
     <$ {\gmath(PSum)(e)} \over \gmath(PSum)(f) $>
  | Pro(e, f) when use_times f ->
     if (lvl < PPro) then <$ (\gmath(PPro)(e) \times \gmath(pp PPro)(f)) $>
     else  <$ \gmath(PPro)(e) \times \gmath(pp PPro)(f) $>
  | Pro(e,f) ->
     if (lvl < PPro) then <$ (\gmath(PPro)(e) \gmath(pp PPro)(f)) $>
     else  <$ \gmath(PPro)(e) \gmath(pp PPro)(f) $>
  | Vec (Vec l :: ls as l0s) when idvec (List.length l) ls ->
     let l = List.map (function Vec l -> List.map (gmath lvl) l
                              | _ -> assert false) l0s in
     <$ ( \matrix(l) ) $>
  | Vec l ->
     let l = List.map (gmath lvl) l in
     <$ ( \lineMatrix(l) ) $>
  | Pow(e,f) ->
     if (lvl < PPow) then <$ (\gmath(pp PPow)(e)^{\gmath(PPow)(f)}) $>
     else <$ \gmath(pp PPow)(e)^{\gmath(PPow)(f)} $>
  | Inv e ->
     if (lvl < PPow) then <$ (\gmath(pp PPow)(e)^{-1}) $>
     else <$ \gmath(pp PPow)(e)^{-1} $>
  | Opp e ->
     if (lvl < PPow) then <$ (-\gmath(pp PPow)(e)) $>
     else <$ -\gmath(pp PPow)(e) $>
  | App(e,l) ->
     <$ \gmath(PAtm)(e)(\gmathl(l)) $>
  | Ind(e,l) ->
     <$ \gmath(PPow)(e)_{\gmathl(l)} $>

and gmathl l =
  match l with
  | [] -> []
  | [e] -> gmath pTop e
  | e::l -> <$ \gmath(pTop)(e), \gmathl(l) $>


let gmath = gmath pTop

let giac_name = try Sys.getenv "GIAC" with Not_found -> "giac"

let (inc,ouc,erc) as proc =
  Unix.open_process_full (giac_name) (Unix.environment ())

let input_all delay =
  let open Unix in
  let inc = descr_of_in_channel inc in
  let erc = descr_of_in_channel erc in
  set_nonblock inc;
  set_nonblock erc;
  let s = Bytes.make 1024 ' ' in
  let bstd = Buffer.create 1024 in
  let berr = Buffer.create 1024 in
  try
    while true do
      let ins, _, _ = select [inc;erc] [] [] delay in
      if ins = [] then raise Exit;
      List.iter (fun ch ->
          let n = read ch s 0 1024 in
          let b = if ch == inc then bstd else berr in
          Buffer.add_subbytes b s 0 n) ins
    done;
    assert false
  with Exit ->
    (Buffer.contents bstd, Buffer.contents berr)

let giac_wait = ref 0.1

let _ = input_all !giac_wait

type t = Giac of giac | Error of string

let parse_string s =
  try
    Earley.parse_string giac blank s
  with _ ->
    Printf.eprintf "Fail to parse giac %S\n%!" s; Symbol "PARSE ERROR"

let lock_fd =
  let open Unix in
  let temp = Filename.temp_file "giac_patoline" ".lock" in
  let fd = openfile temp [O_CLOEXEC;O_EXCL;O_RDWR] 0o700 in
  fd

let run fmt =
  let open Unix in
  lockf lock_fd F_LOCK 0;
  let cont ouc =
    let (std, err) = input_all !giac_wait in
    lockf lock_fd F_ULOCK 0;
    (*Printf.eprintf ">>>>> %S\n%S\n%!" std err;*)
    let l = Str.(split (regexp_string "\n") std) in
    let len = List.length l in
    if len < 2 then failwith "Unexpected giac answer";
    let std = (List.nth l 1) in
    try
      Giac (parse_string std)
    with
    | e ->
       Printf.eprintf "GIAC ERROR(%s):\n%s\n%s\n%!" (Printexc.to_string e) std err;
       Error std
  in
  Printf.kfprintf cont ouc fmt

let eval g =
  match run "%a;\n%!" print g with
  | Giac g -> g
  | Error msg -> Printf.eprintf "giac error: %s\n%!" msg; exit 1
