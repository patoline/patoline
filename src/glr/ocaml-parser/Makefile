# Author: Rodolphe Lepigre <rodolphe.lepigre@univ-savoie.fr>

all: pa_ocaml

blank.cmx: blank.ml
	ocamlfind ocamlopt -c blank.ml

memoize.cmx: memoize.ml
	ocamlfind ocamlopt -c memoize.ml

atoms.cmx: atoms.ml blank.cmx memoize.cmx
	ocamlfind ocamlopt -I +compiler-libs -package glr -pp ../pa_glr -c atoms.ml

pa_ocaml.cmx: pa_ocaml.ml ../pa_glr blank.cmx atoms.cmx memoize.cmx
	ocamlfind ocamlopt -I +compiler-libs -package glr -pp ../pa_glr -c pa_ocaml.ml

pa_ocaml: pa_ocaml.cmx blank.cmx atoms.cmx
	ocamlfind ocamlopt -I +compiler-libs -package glr -o $@ str.cmxa glr.cmxa ocamlcommon.cmxa blank.cmx memoize.cmx atoms.cmx pa_ocaml.cmx

clean:
	rm -rf *.cmi *.cmo *.cma *.o *.a *.cmx
