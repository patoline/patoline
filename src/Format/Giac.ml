
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
  | Tup of giac list

type prio = PAtm | PPow | PPro | PSum

let prev = function
  | PAtm -> assert false
  | PPow -> PAtm
  | PPro -> PPow
  | PSum -> PPro

let parser giac lvl =
  | s:''[0-9]+''                      when lvl = PAtm -> Number s
  | s:''[a-zA-Z_][a-zA-Z0-9_]''       when lvl = PAtm -> Symbol s
  | e:(giac PAtm) '(' l:(gs true) ')' when lvl = PAtm -> App(e,l)
  | e:(giac PAtm) '[' l:(gs true) ']' when lvl = PAtm -> Ind(e,l)
  | '(' l:(gs false) ')'              when lvl = PAtm -> Tup(l)
  | '[' l:(gs true) ']'               when lvl = PAtm -> Vec(l)
  | e:(giac PAtm) '^' f:(giac PPow)   when lvl = PPow -> Pow(e,f)
  | '-' e:(giac PPow)                 when lvl = PPow -> Opp(e)
  | e:(giac PPro) '*' f:(giac PAtm)   when lvl = PPro -> Pro(e,f)
  | e:(giac PPro) '/' f:(giac PAtm)   when lvl = PPro -> Pro(e,Inv f)
  | e:(giac PSum) '+' f:(giac PPro)   when lvl = PSum -> Sum(e,f)
  | e:(giac PSum) '-' f:(giac PPro)   when lvl = PSum -> Sum(e,Opp f)
  | e:(giac (prev lvl))               when lvl <> PAtm -> e

and gs empty=
  | EMPTY when empty -> []
  | e:(giac PSum) l:{_:',' (giac PSum)}* -> e::l
