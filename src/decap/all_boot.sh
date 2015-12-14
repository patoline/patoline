#!/bin/bash

make pa_ocaml

export OPATH=$PATH
export MAKEOPTS="OCAMLFIND= OCAMLOPT=ocamlopt.opt OCAMLC=ocamlc.opt"
export MAKE="make $MAKEOPTS"

function build {
    export PATH=/usr/local-$1/bin:$OPATH
    export OCAMLVERSION=$1
    echo ==========================================================
    echo $PATH
    echo $MAKE clean boot
    which ocamlopt.opt
    echo ==========================================================
    $MAKE clean boot
    if [ -x ./pa_ocaml ]; then rm pa_ocaml; fi
    echo ==========================================================
    echo $MAKE pa_ocaml
    echo ==========================================================
    $MAKE pa_ocaml
    echo ==========================================================
    echo $MAKE
    echo ==========================================================
    $MAKE
    echo ==========================================================
    echo cd ast_tools
    echo $MAKE distclean
    echo $MAKE compare.ml
    echo ==========================================================
    cd ast_tools
    $MAKE distclean
    $MAKE compare.ml
    cd ..
    echo ==========================================================
    echo cp ast_tools/compare.ml bootstrap/$1/compare.ml
    echo $MAKE
    echo ==========================================================
    cp ast_tools/compare.ml bootstrap/$1/compare.ml
    $MAKE
    echo ==========================================================
    echo ./tests_pa_ocaml.sh
    echo ==========================================================
    ./tests_pa_ocaml.sh
}

build 4.02.3
cp -f pa_ocaml pa_ocaml-4.02.3

build 4.02.2
cp -f pa_ocaml pa_ocaml-4.02.2

build 4.02.1
cp -f pa_ocaml pa_ocaml-4.02.1

build 4.02.0
cp -f pa_ocaml pa_ocaml-4.02.0

build 4.01.0
cp -f pa_ocaml pa_ocaml-4.01.0

# starting to remove support for 3.12.1 and 4.00.1 that
# can not print ast.
#cp -f pa_ocaml-4.01.0 pa_ocaml
#build 4.00.1
#
#cp -f pa_ocaml-4.01.0 pa_ocaml
#build 3.12.1

export PATH=$OPATH

#make sure not to have a pa_ocaml which can not bootstrap
cp -f pa_ocaml-`ocamlc -vnum` pa_ocaml
$MAKE distclean
