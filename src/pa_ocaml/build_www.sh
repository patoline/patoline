#!/bin/bash

# Remove old generated files
rm -rf www/ocamldoc
rm -rf www/files

# Creating directories
mkdir www/ocamldoc
mkdir www/files

# Creating release archive
sh ./build_archive.sh
cp decap.tar www/files/

# Copying ocamldoc documentation
cp html/* www/ocamldoc/

# Copying documentation
cp doc/doc.pdf www/files/decap_user_guide.pdf
tar --transform 's,^doc/,/decap_user_guide/,' \
  -cvf www/files/decap_user_guide.tar \
  doc/*.ml doc/Makefile doc/doc.pdf

echo "Done!"
