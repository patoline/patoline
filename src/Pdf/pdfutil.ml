open Typography.Util

type obj=
    Dict of obj StrMap.t
  | Number of float
  | Name of string
  | String of string
  | Indirect of int*int
  | Array of obj list

let rec print_obj o=match o with
    Number i->Printf.printf "Number %f" i
  | Indirect (i,j)->Printf.printf "Indirect(%d,%d)" i j
  | Name s->Printf.printf "Name %s" s
  | String s->Printf.printf "String %s" s
  | Dict d->Printf.printf "Dict [";
    StrMap.iter (fun k a->Printf.printf "(%S, " k;print_obj a;Printf.printf ")") d;
    Printf.printf "]"
  | Array a->Printf.printf "Array [";
    List.iter (fun a->Printf.printf "(";print_obj a;Printf.printf ")") a;
    Printf.printf "]"
