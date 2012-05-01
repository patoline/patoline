open Util

type math =
     Var of string
   | Symbol of string
   | Fun of string
   | Num of string   
   | Prefix of int * string * bool * math
   | Postfix of int * math * bool * string
   | Binary of int * math * bool * string * bool * math
   | Indices of indices * math
   | Apply of math * math
   | MathMacro of string * math list
   | Delim of string * math * string  
   | MScope of math list
and indices = { up_right : math option;
    down_right : math option; 
    up_left : math option;
    down_left : math option }

module Str = struct
  type t = string
  let compare = compare
end

module StrSet = Set.Make(Str)

type local_data = { infixes : StrSet.t;  other_symbols : StrSet.t; verbose : bool }

type structType = Absolute of int | Relative of doc list

and doc =
     Paragraph of content list
   (* FIXME : construct the type of the parsing device instead of Obj.t *)
   | Caml of local_data * Obj.t * int * int * (texprime_section * int * int) list
   | Math of math
   | Verbatim of string option * string list
   | Struct of content list * bool * structType
   | Macro of [ `Single | `Module | `Begin | `End | `Include | `Preproc ] * string * doc list
   | Preproc of string
   | Ignore

and texprime_section = TxpMath | TxpText

and content =
     TC of string
   | FC of bool * math
   | MC of [ `Single | `Module | `Begin | `End | `Include | `Preproc ] * string * doc list
   | GC

let par_macros:(string->content list) StrMap.t ref=ref StrMap.empty
let macros:(string->string) StrMap.t ref=ref StrMap.empty
