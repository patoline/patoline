all: pa_ast

pa_ast: pa_ast.ml
	ocamlopt -pp pa_ocaml -I +decap -I +compiler-libs -o $@ ocamlcommon.cmxa unix.cmxa str.cmxa decap.cmxa decap_ocaml.cmxa $<

test: pa_ast
	pa_ocaml --ascii asttypes.mli | ./pa_ast
	pa_ocaml --ascii parsetree.mli | ./pa_ast

clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o

distclean: clean
	rm -f pa_ast
