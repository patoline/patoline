#!/bin/bash

ocamlc -dparsetree                 $1 2> /tmp/$1.ocaml
ocamlc -dparsetree -pp ../pa_ocaml $1 2> /tmp/$1.pa_ocaml

cat /tmp/$1.ocaml    | sed -e 's/(.*\.ml\[.*\]\.\.\[.*\])\( ghost\)\?//' > /tmp/$1.ocaml.out
cat /tmp/$1.pa_ocaml | sed -e 's/(.*\.ml\[.*\]\.\.\[.*\])\( ghost\)\?//' > /tmp/$1.pa_ocaml.out

# diff -y /tmp/$1.ocaml.out /tmp/$1.pa_ocaml.out | less
diff $2 /tmp/$1.ocaml.out /tmp/$1.pa_ocaml.out
