#!/bin/bash

OPATH=$PATH
MAKEOPTS="-D OCAMLFIND="
MAKE=make $(MAKEOPTS)

$(MAKE) pa_ocaml

export PATH=/usr/local402.1/bin:$OPATH
$(MAKE) clean boot && rm pa_ocaml && $(MAKE) pa_ocaml && ./tests_pa_ocaml.sh

export PATH=/usr/local402/bin:$OPATH
$(MAKE) clean boot && rm pa_ocaml && $(MAKE) pa_ocaml && ./tests_pa_ocaml.sh

export PATH=/usr/local401/bin:$OPATH
$(MAKE) clean boot && rm pa_ocaml && $(MAKE) pa_ocaml && ./tests_pa_ocaml.sh

cp -f pa_ocaml pa_ocaml401 #we need a pa_ocaml able to bootstrap and 4.00 and 3.12 compiler-libs can't print ast

export PATH=/usr/local400/bin:$OPATH
$(MAKE) clean boot && rm pa_ocaml && $(MAKE) pa_ocaml && ./tests_pa_ocaml.sh

cp -f pa_ocaml401 pa_ocaml

export PATH=/usr/local312/bin:$OPATH
$(MAKE) clean boot && rm pa_ocaml && $(MAKE) pa_ocaml && ./tests_pa_ocaml.sh

export PATH=$OPATH
$(MAKE) distclean #to make sure not to have a pa_ocaml which can not bootstrap
