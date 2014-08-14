# Author: Rodolphe Lepigre <rodolphe.lepigre@univ-savoie.fr>

all: pa_ocaml

pa_ocaml.cmo: pa_ocaml.ml ../pa_glr
	ocamlfind ocamlc -I +compiler-libs -package glr -pp ../pa_glr -c pa_ocaml.ml

pa_ocaml.cmx: pa_ocaml.ml ../pa_glr
	ocamlfind ocamlopt -I +compiler-libs -package glr -pp ../pa_glr -c pa_ocaml.ml

pa_ocaml.cma: pa_ocaml.cmo
	ocamlfind ocamlc -I +compiler-libs -package glr,camlp4 -a -o pa_ocaml.cma pa_ocaml.cmo

pa_ocaml: pa_ocaml.cmx
	ocamlfind ocamlopt -I +compiler-libs -package glr -o $@ unix.cmxa str.cmxa glr.cmxa ocamlcommon.cmxa $<

clean:
	rm -rf *.cmi *.cmo *.cma *.o *.a *.cmx
