#!/bin/bash

ocaml=`ocamlc -where`
local=./tests

files="$local/test.ml $ocaml/pervasives.ml $ocaml/pervasives.mli $ocaml/list.ml $ocaml/list.mli \
       $ocaml/set.ml $ocaml/set.mli $ocaml/map.ml $ocaml/map.mli $local/bigarray.ml $ocaml/bigarray.mli \
       $ocaml/string.ml $ocaml/string.mli $ocaml/array.ml $ocaml/array.mli"

echo $files

for f in $files; do
  /usr/bin/time --format="%C: %e" ./pa_ocaml $f > /dev/null
  /usr/bin/time --format="%C: %e" camlp4o $f > /dev/null
done

echo "test of the extensions to the syntax"
/usr/bin/time --format="%C: %e" ./pa_ocaml $local/test_ext.ml > /dev/null
