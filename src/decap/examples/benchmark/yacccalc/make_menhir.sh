#!/bin/sh 

menhir parser.mly     # generates parser.ml and parser.mli
ocamllex lexer.mll       # generates lexer.ml
ocamlopt -c parser.mli
ocamlopt -c lexer.ml
ocamlopt -c parser.ml
ocamlopt -c calc.ml
ocamlopt -o mcalc lexer.cmx parser.cmx unix.cmxa calc.cmx