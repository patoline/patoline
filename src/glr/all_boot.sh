
OPATH=$PATH

make pa_ocaml

export PATH=/usr/local402/bin:$OPATH
make clean boot && rm pa_ocaml && make pa_ocaml && ./tests_pa_ocaml.sh

export PATH=/usr/local401/bin:$OPATH
make clean boot && rm pa_ocaml && make pa_ocaml && ./tests_pa_ocaml.sh

cp -f pa_ocaml pa_ocaml401 #we need a pa_ocaml able to bootstrap and 4.00 and 3.12 compiler-libs can't print ast

export PATH=/usr/local400/bin:$OPATH
make clean boot && rm pa_ocaml && make pa_ocaml && ./tests_pa_ocaml.sh

cp -f pa_ocaml401 pa_ocaml

export PATH=/usr/local312/bin:$OPATH
make clean boot && rm pa_ocaml && make pa_ocaml && ./tests_pa_ocaml.sh

export PATH=$OPATH
make distclean #to make sure not to have a pa_ocaml which can not bootstrap
