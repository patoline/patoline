
OPATH=$PATH

export PATH=/usr/local400/bin:$OPATH
make boot 

export PATH=/usr/local401/bin:$OPATH
make clean boot pa_ocaml && ./tests_pa_ocaml.sh

export PATH=/usr/local402/bin:$OPATH
make clean boot pa_ocaml && ./tests_pa_ocaml.sh

make clean #to make sure not to have a pa_ocaml with the wrong version

