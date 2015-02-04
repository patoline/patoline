#!/bin/bash

ocaml=`ocamlc -where`
local=./tests_pa_ocaml
examples=./doc
diff=./tests_pa_ocaml
ocamlversion=`ocamlc -version`

files="$local/bibi.ml $local/UTF16.ml $local/test_offset.ml $local/image*.ml $local/decap.ml \
       $local/pa_byt_main.ml $local/pa_ocaml_prelude.ml $local/pa_parser.ml \
       $local/test.ml $local/objects.ml $local/variants.ml $local/prefix.ml\
       $local/mixin2.ml $local/mixev.ml $local/mixev2.ml $local/mixmod.ml $local/mixmod5.ml $local/mixobj.ml \
       $ocaml/pervasives.ml $ocaml/pervasives.mli $ocaml/list.ml $ocaml/list.mli \
       $ocaml/set.ml $ocaml/set.mli $ocaml/map.ml $ocaml/map.mli $local/bigarray.ml $ocaml/bigarray.mli \
       $ocaml/string.ml $ocaml/string.mli $ocaml/array.ml $ocaml/array.mli $ocaml/char.ml $ocaml/char.mli \
       $ocaml/arg.ml $ocaml/arg.mli $ocaml/arrayLabels.ml $ocaml/arrayLabels.mli $ocaml/buffer.ml \
       $ocaml/buffer.mli $ocaml/complex.ml $ocaml/complex.mli $ocaml/digest.ml $ocaml/digest.mli \
       $ocaml/dynlink.mli $ocaml/filename.ml $ocaml/filename.mli $ocaml/format.ml $ocaml/gc.ml \
       $ocaml/gc.mli $ocaml/genlex.ml $ocaml/genlex.mli $ocaml/hashtbl.ml $ocaml/hashtbl.mli \
       $ocaml/lexing.ml $ocaml/lexing.mli $ocaml/listLabels.ml $ocaml/listLabels.mli $ocaml/moreLabels.ml \
       $ocaml/moreLabels.mli
"

#files only working on ocaml 4
files4="$local/test4.ml"

#if [ `ocamlc -version` != 3.12.1 ] ; then
#    files="$files $files4"
#fi

echo $files

make $MAKEOPTS test_parsers

./test_parsers $files

for f in $files; do
  echo "File: $f"
  /usr/bin/time --format="%C: %U,%S,%E" ./pa_ocaml $f > /dev/null
#  /usr/bin/time --format="%C: %U,%S,%E" camlp4o.opt $f > /dev/null

  ocamlc -rectypes -c -dparsetree -o /tmp/foo.cmo -pp ./pa_ocaml  $f 2> $diff/$(basename $f).pa_ocaml.full
  ocamlc -rectypes -c -dparsetree -o /tmp/bar.cmo                 $f 2> /tmp/bar.tree
#  ocamlc.opt -c -dparsetree -o /tmp/bar.cmo -pp camlp4o.opt $f 2> /tmp/bar.tree
#  diff /tmp/foo.cmo /tmp/bar.cmo

  cat $diff/$(basename $f).pa_ocaml.full | sed -e 's/(.*\.mli\?\[.*\]\.\.\([^[]*\.mli\?\)\?\[.*\])\( ghost\)\?//' > $diff/$(basename $f).pa_ocaml
  cat /tmp/bar.tree | sed -e 's/(.*\.mli\?\[.*\]\.\.\([^[]*\.mli\?\)\?\[.*\])\( ghost\)\?//' > $diff/$(basename $f).ocamlc
  cat /tmp/bar.tree | sed -e 's/) ghost/)/g' > $diff/$(basename $f).ocamlc.full
  diff $diff/$(basename $f).pa_ocaml  $diff/$(basename $f).ocamlc > $diff/$(basename $f).diff
  diff $diff/$(basename $f).pa_ocaml.full $diff/$(basename $f).ocamlc.full > $diff/$(basename $f).fulldiff
  echo diff size: $(wc $diff/$(basename $f).diff)
  echo diff size with pos: $(wc $diff/$(basename $f).fulldiff)
  echo
done

echo "test of the extensions to the syntax"
/usr/bin/time --format="%C: %e" ocamlc -c -pp ./pa_ocaml $local/test_ext.ml
/usr/bin/time --format="%C: %e" ocamlc -i -c -pp ./pa_ocaml -I +compiler-libs -I bootstrap/$ocamlversion $local/test_quotation.ml
make $examples/pa_do_try
/usr/bin/time --format="%C: %e" ocamlc -i -c -pp $examples/pa_do_try $local/test_extension.ml
echo

# echo "test of parser extension"
# /usr/bin/time --format="%C: %e" ocamlc -c -I .. -pp ./pa_ocaml ./examples/calc.ml
# /usr/bin/time --format="%C: %e" ocamlc -c -I .. -pp ./pa_ocaml ./examples/calc_all.ml
# cp ./pa_ocaml_prelude.ml $local/
# /usr/bin/time --format="%C: %e" ocamlc -I +compiler-libs -c -I bootstrap/$ocamlversion -pp ./pa_ocaml $local/pa_ocaml_prelude.ml
# cp ./pa_parser.ml $local/
# /usr/bin/time --format="%C: %e" ocamlc -I +compiler-libs -c -I bootstrap/$ocamlversion -pp ./pa_ocaml $local/pa_parser.ml
# cp ./pa_ocaml.ml $local/
# /usr/bin/time --format="%C: %e" ocamlc -I +compiler-libs -c -I bootstrap/$ocamlversion -pp ./pa_ocaml $local/pa_ocaml.ml

echo "********************************************"
echo TOTAL diff size:
wc $diff/*.diff | grep total
echo TOTAL diff size with pos:
wc $diff/*.fulldiff | grep total
echo "********************************************"
