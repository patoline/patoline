        {
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
        }
        rule token = parse
            [' ' '\t' '\n' '\r']     { token lexbuf }     (* skip blanks *)
          | eof        { EOL }
          | ['0'-'9']+(['.']['0'-'9']+)?(['e''E']['-']?['0'-'9']+)? as lxm { FLOAT(float_of_string lxm) }
          | '+'            { PLUS }
          | '-'            { MINUS }
          | '*'            { TIMES }
          | '*' '*'        { POW }
          | '/'            { DIV }
          | '('            { LPAREN }
          | ')'            { RPAREN }
