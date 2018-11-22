  (* File lexer.mll *)
  {
    open Obj_parser        (* The type token is defined in parser.mli *)
    exception Eof
  }
    rule token = parse
    [' ''\r''\t''\n']     { token lexbuf }     (* skip blanks *)

      | ['0'-'9']+('.'['0'-'9']*)? as lxm { NUMBER(float_of_string lxm) }

      | ['0'-'9']+[' ''\r''\t''\n']+['0'-'9']+[' ''\r''\t''\n']+'R' as ind { INDIRECT ind }
      | '/'['a'-'z''A'-'Z']+ as name { NAME name }
      | "<<"            { DICT_BEGIN }
      | ">>"            { DICT_END }
      | "["            { ARRAY_BEGIN }
      | "]"            { ARRAY_END }
      | "("([^')'] | "\\)")*")" as str  { STRING str }
      | "<"(['0'-'9''a'-'z''A'-'Z']*)">" as str  { STRING str }

      | eof            { raise Eof }
