all: pa_ast

pa_ast: pa_ast.ml
	ocamlopt -pp pa_ocaml -I +decap -I +compiler-libs -o $@ ocamlcommon.cmxa unix.cmxa str.cmxa decap.cmxa decap_ocaml.cmxa $<

compare.ml: pa_ast generic.ml
	echo "open Asttypes" > $@
	echo "open Parsetree" >> $@
	echo "(* Generic functions *)" >> $@
	cat generic.ml >> $@
	echo "(* asttypes.mli *)" >> $@
	pa_ocaml --ascii asttypes.mli | ./pa_ast 2> /dev/null | pa_ocaml --ascii >> $@
	echo "(* parsetree.mli *)" >> $@
	pa_ocaml --ascii parsetree.mli | ./pa_ast 2> /dev/null | pa_ocaml --ascii >> $@

compare.cmx: compare.ml
	ocamlopt -I +compiler-libs $<

clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o

distclean: clean
	rm -f pa_ast compare.ml
