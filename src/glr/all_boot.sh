
OPATH=$PATH

export PATH=/usr/local402/bin:$OPATH
make clean boot pa_ocaml && ./tests_pa_ocaml.sh

export PATH=/usr/local401/bin:$OPATH
make clean boot pa_ocaml && ./tests_pa_ocaml.sh

export PATH=/usr/local400/bin:$OPATH
make clean boot pa_ocaml && ./tests_pa_ocaml.sh

export PATH=$OPATH
make clean pa_ocaml #to make sure not to have a pa_ocaml which can not bootstrap

