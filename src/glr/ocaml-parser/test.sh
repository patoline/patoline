#!/bin/bash

ocaml=`ocamlc -where`

files="test.ml $ocaml/pervasives.ml $ocaml/list.ml $ocaml/list.mli \
       $ocaml/set.ml $ocaml/set.mli $ocaml/map.ml $ocaml/map.mli"

echo $files

for f in $files; do
  /usr/bin/time --format="%C: %e" ./pa_ocaml $f > /dev/null
  /usr/bin/time --format="%C: %e" camlp4o $f > /dev/null
done

echo "test of the extensions to the syntax"
/usr/bin/time --format="%C: %e" ./pa_ocaml $f > /dev/null
