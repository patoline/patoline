#!/bin/bash

# Compile documentation
patoline doc/doc.typ

# Create archive
tar  --transform 's,^,/decap/,' -cvf decap.tar \
  *.ml *.mli META Makefile bootstrap/*/*.ml README \
  doc/*.ml doc/Makefile doc/README doc/doc.pdf \
  Licence_CeCILL-B_V1-en.txt Licence_CeCILL-B_V1-fr.txt
