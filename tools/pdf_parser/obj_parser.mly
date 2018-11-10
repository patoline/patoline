/* File parser.mly */
%{
  open Pdfutil
  open Extra
%}
%token <float> NUMBER
%token <string> NAME
%token <string> INDIRECT
%token <string> STRING
%token DICT_BEGIN DICT_END ARRAY_BEGIN ARRAY_END STRING_BEGIN STRING_END

%start main
%type <Pdfutil.obj> main
%%
main:
expr { $1 }

expr:
DICT_BEGIN dict DICT_END {Dict $2}
| NUMBER { Number $1 }
| INDIRECT {
  let s= $1 in
  let rec make_int i x=
    if s.[i]>='0' && s.[i]<='9' then
      make_int (i+1) (x*10+(int_of_char s.[i]-int_of_char '0'))
    else
      (x,i)
  in
  let rec skip_spaces i=if s.[i]>='0' || s.[i]<='9' then i else skip_spaces (i+1) in
  let x,i0=make_int 0 0 in
  let i1=skip_spaces i0 in
  let y,_=make_int i1 0 in
  Indirect (x,y)
}
| NAME { Name $1 }
| STRING { String $1 }
| ARRAY_BEGIN expr_seq ARRAY_END { Array $2 }

dict:
{ StrMap.empty }
| NAME expr dict { StrMap.add $1 $2 $3 }

expr_seq:
{[]}
| expr expr_seq {$1::$2}
