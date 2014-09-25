#!/bin/sh 

ocamlyacc parser.mly     # generates parser.ml and parser.mli
ocamllex lexer.mll       # generates lexer.ml
ocamlopt -c parser.mli
ocamlopt -c lexer.ml
ocamlopt -c parser.ml
ocamlopt -c calc.ml
ocamlopt -o calc lexer.cmx parser.cmx unix.cmxa calc.cmx