#!/bin/bash

# Compile documentation
cd doc
patoline doc.typ
cd ..

# Remove old archive
rm decap.tar.gz

# Create archive
tar  --transform 's,^,/decap/,' -cvf decap.tar --exclude copyright.ml \
  *.ml *.mli META Makefile bootstrap/*/*.ml README \
  doc/*.ml doc/Makefile doc/README doc/doc.pdf \
  Licence_CeCILL-B_V1-en.txt Licence_CeCILL-B_V1-fr.txt
gzip decap.tar
