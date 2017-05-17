
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
  | Tup of giac list (* En fait sequence, a revoir avec juste un Cons et Nil *)

let int n = Number (string_of_int n)

type prio = PAtm | PPow | PPro | PSum

let prev = function
  | PAtm -> PAtm
  | PPow -> PAtm
  | PPro -> PPow
  | PSum -> PPro

let parser giac lvl =
  | s:''[0-9]+''                    when lvl = PAtm -> Number s
  | s:''[a-zA-Z_][a-zA-Z0-9_]*''    when lvl = PAtm -> Symbol s
  | "inv" '(' e:(giac PSum) ')'     when lvl = PAtm -> Inv(e)
  | e:(giac PAtm) '(' l:gs ')'      when lvl = PAtm ->
           if e = Symbol "inv" then Earley.give_up (); App(e,l)
  | e:(giac PAtm) '[' l:gs ']'      when lvl = PAtm -> Ind(e,l)
  | '(' l:gs ')'                    when lvl = PAtm -> Tup(l)
  | '[' l:gs ']'                    when lvl = PAtm -> Vec(l)
  | e:(giac PAtm) '^' f:(giac PPow) when lvl = PPow -> Pow(e,f)
  | '-' e:(giac PPow)               when lvl = PPow -> Opp(e)
  | e:(giac PPro) '*' f:(giac PPow) when lvl = PPro -> Pro(e,f)
  | e:(giac PPro) '/' f:(giac PPow) when lvl = PPro -> Pro(e,Inv f)
  | e:(giac PSum) '+' f:(giac PPro) when lvl = PSum -> Sum(e,f)
  | e:(giac PSum) '-' f:(giac PPro) when lvl = PSum -> Sum(e,Opp f)
  | e:(giac (prev lvl))             when lvl > PAtm -> e

and gs =
  | EMPTY -> []
  | e:(giac PSum) l:{_:',' (giac PSum)}* -> e::l

let giac = giac PSum

let blank = Earley.blank_regexp ''[ \t\n\r]*''

let rec print lvl ch e =
  let pr fmt = Printf.fprintf ch fmt in
  let lvl' = prev lvl in
  match e with
  | Symbol s | Number s -> pr "%s" s
  | Sum(e,Opp f) ->
     let fmt : ('a,'b,'c) format = if lvl < PSum then "(%a - %a)" else "%a - %a" in
     pr fmt (print lvl) e (print lvl') f
  | Sum(e,f) ->
     let fmt : ('a,'b,'c) format = if lvl < PSum then "(%a + %a)" else "%a + %a" in
     pr fmt (print lvl) e (print lvl') f
  | Pro(e,Inv f) ->
     let fmt : ('a,'b,'c) format = if lvl < PPro then "(%a / %a)" else "%a / %a" in
     pr fmt (print lvl) e (print lvl') f
  | Pro(e,f) ->
     let fmt : ('a,'b,'c) format = if lvl < PPro then "(%a * %a)" else "%a * %a" in
     pr fmt (print lvl) e (print lvl') f
  | Pow(e,f) ->
     let fmt : ('a,'b,'c) format = if lvl < PPow then "(%a ^ %a)" else "%a ^ %a" in
     pr fmt (print lvl') e (print lvl) f
  | Inv e ->
     pr "inv(%a)" (print PSum) e
  | Opp e ->
     let fmt : ('a1,'b1,'c1) format = if lvl < PPow then "(-%a)" else "%a" in
     pr fmt (print lvl) e
  | App(e,l) ->
     pr "%a(%a)" (print PAtm) e prl l
  | Ind(e,l) ->
     pr "%a[%a]" (print PAtm) e prl l
  | Tup(l) ->
     pr "(%a)" prl l
  | Vec(l) ->
     pr "[%a]" prl l

and prl ch l =
  match l with
  | [] -> ()
  | [e] -> print PSum ch e
  | e::l -> Printf.fprintf ch "%a, %a" (print PSum) e prl l

let print = print PSum
let prev_prio = prev

open DefaultFormat
open Maths
open MathsFormat
open Document
open Typography.Diagrams
open Box

let idvec n ls =
    List.for_all (function Vec l -> List.length l = n | _ -> false) ls

let rec gmath lvl e =
  let lvl' = prev_prio lvl in
  match e with
  | Number s | Symbol s ->
    [Maths.Ordinary (Maths.node (Maths.glyphs s))]
  | Sum(e,Opp f) ->
     if (lvl < PSum) then <$ (\gmath(lvl)(e) - \gmath(lvl)(f)) $>
     else  <$ \gmath(lvl)(e) - \gmath(lvl')(f) $>
  | Sum(e,f) ->
     if (lvl < PSum) then <$ (\gmath(lvl)(e) + \gmath(lvl)(f)) $>
     else  <$ \gmath(lvl)(e) - \gmath(lvl')(f) $>
  | Pro(e,Inv f) ->
     <$ {\gmath(PSum)(e)} \over \gmath(PSum)(f) $>
  | Pro(e,f) ->
     if (lvl < PPro) then <$ (\gmath(lvl)(e) * \gmath(lvl)(f)) $>
     else  <$ \gmath(lvl)(e) \gmath(lvl')(f) $>
  | Vec (Vec l :: ls as l0s) when idvec (List.length l) ls ->
     <$ (\mathsText{\diagram(
      let m,ms = array (List.map (fun _ -> `Main) l)
      (List.map (function Vec l -> List.map (gmath lvl) l
                       | _ -> assert false) l0s))}) $>
  | Vec l ->
     <$ (\mathsText{\diagram(
                       let m,ms = array [`Main] (List.map (fun x -> [gmath lvl x]) l))}) $>
  | Pow(e,f) ->
     if (lvl < PPow) then <$ (\gmath(lvl)(e)^{\gmath(lvl)(f)}) $>
     else <$ \gmath(lvl)(e)^{\gmath(lvl)(f)} $>
  | Inv e ->
     if (lvl < PPow) then <$ (\gmath(lvl)(e)^{-1}) $>
     else <$ \gmath(lvl)(e)^{-1} $>
  | Opp e ->
     if (lvl < PPow) then <$ (-\gmath(lvl)(e)) $>
     else <$ -\gmath(lvl)(e) $>
  | App(e,l) ->
     <$ \gmath(PAtm)(e)(\gmathl(l)) $>
  | Ind(e,l) ->
     <$ \gmath(PPow)(e)_{\gmathl(l)} $>
  | _ -> assert false
(*
  | Tup(l) ->
     pr "(%a)" prl l
 *)
and gmathl l =
  match l with
  | [] -> []
  | [e] -> gmath PSum e
  | e::l -> <$ \gmath(PSum)(e), \gmathl(l) $>


let gmath = gmath PSum

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

let parse_string = Earley.parse_string giac blank

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
