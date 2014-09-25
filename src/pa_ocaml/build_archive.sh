#!/bin/bash

# Compile documentation
patoline doc/doc.typ

# Create archive
tar  --transform 's,^,/decap/,' -cvf decap.tar \
  *.ml *.mli META Makefile all_boot.sh bootstrap/*/*.ml \
  doc/*.ml doc/Makefile doc/doc.pdf
