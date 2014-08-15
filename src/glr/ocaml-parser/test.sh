#!/bin/bash

ocaml=`ocamlc -where`
local=./tests
diff=./tests

files="$local/test.ml $local/objects.ml $local/variants.ml \
       $ocaml/pervasives.ml $ocaml/pervasives.mli $ocaml/list.ml $ocaml/list.mli \
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
  echo "File: $f"
  /usr/bin/time --format="%C: %e" ./pa_ocaml $f > /dev/null
  /usr/bin/time --format="%C: %e" camlp4o.opt $f > /dev/null

  ocamlc.opt -c -dparsetree -o /tmp/foo.cmo -pp ./pa_ocaml  $f 2> /tmp/foo.tree
  ocamlc.opt -c -dparsetree -o /tmp/bar.cmo                 $f 2> /tmp/bar.tree
#  ocamlc.opt -c -dparsetree -o /tmp/bar.cmo -pp camlp4o.opt $f 2> /tmp/bar.tree
#  diff /tmp/foo.cmo /tmp/bar.cmo

  cat /tmp/foo.tree | sed -e 's/(.*\.mli\?\[.*\]\.\.\[.*\])\( ghost\)\?//' > /tmp/foo.tree2
  cat /tmp/bar.tree | sed -e 's/(.*\.mli\?\[.*\]\.\.\[.*\])\( ghost\)\?//' > /tmp/bar.tree2
  diff /tmp/foo.tree2 /tmp/bar.tree2 > $diff/$(basename $f).diff
  echo diff size: $(wc $diff/$(basename $f).diff)
  echo
done

echo "test of the extensions to the syntax"
/usr/bin/time --format="%C: %e" ocamlc -c -pp ./pa_ocaml $local/test_ext.ml
/usr/bin/time --format="%C: %e" ocamlc -i -c -pp ./pa_ocaml $local/test_quotation.ml
