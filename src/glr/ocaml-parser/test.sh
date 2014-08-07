#!/bin/bash

ocaml=`ocamlc -where`
local=./tests

files="$local/test.ml $local/objects.ml $ocaml/pervasives.ml $ocaml/pervasives.mli $ocaml/list.ml $ocaml/list.mli \
       $ocaml/set.ml $ocaml/set.mli $ocaml/map.ml $ocaml/map.mli $local/bigarray.ml $ocaml/bigarray.mli \
       $ocaml/string.ml $ocaml/string.mli $ocaml/array.ml $ocaml/array.mli $ocaml/char.ml $ocaml/char.mli \
       $ocaml/arg.ml $ocaml/arg.mli $ocaml/arrayLabels.ml $ocaml/arrayLabels.mli $ocaml/buffer.ml \
       $ocaml/buffer.mli $ocaml/complex.ml $ocaml/complex.mli $ocaml/digest.ml $ocaml/digest.mli \
       $ocaml/dynlink.mli $ocaml/filename.ml $ocaml/filename.mli $ocaml/format.ml $ocaml/gc.ml \
       $ocaml/gc.mli $ocaml/genlex.ml $ocaml/genlex.mli $ocaml/hashtbl.ml $ocaml/hashtbl.mli \
       $ocaml/lexing.ml $ocaml/lexing.mli $ocaml/listLabels.ml $ocaml/listLabels.mli $ocaml/moreLabels.ml \
       $ocaml/moreLabels.mli"

#echo $files

for f in $files; do
  /usr/bin/time --format="%C: %e" ./pa_ocaml $f > /dev/null
  /usr/bin/time --format="%C: %e" camlp4o.opt $f > /dev/null
#  ocamlc.opt -c -o /tmp/foo.cmo -pp ./pa_ocaml $f
#  ocamlc.opt -c -o /tmp/bar.cmo -pp camlp4o.opt $f
#  diff /tmp/foo.cmo /tmp/bar.cmo
#  echo
done

echo "test of the extensions to the syntax"
/usr/bin/time --format="%C: %e" ./pa_ocaml $local/test_ext.ml > /dev/null
