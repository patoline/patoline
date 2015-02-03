OCAMLVERSION=$(shell ocamlc -version)
DECAP=..
PA_OCAML=$(DECAP)/pa_ocaml

all: pa_ast compare.cmx

pa_ast: pa_ast.ml
	ocamlopt -pp $(PA_OCAML) -I $(DECAP) -I +compiler-libs -o $@ ocamlcommon.cmxa unix.cmxa str.cmxa decap.cmxa decap_ocaml.cmxa $<



asttypes.mli:
	cp ast-$(OCAMLVERSION)/asttypes.mli .

parsetree.mli:
	cp ast-$(OCAMLVERSION)/parsetree.mli .

# FIXME target construction does not fail if pa_ast fails...
compare.ml: pa_ast generic.ml parsetree.mli asttypes.mli
	cat generic.ml >> $@
	@echo "(* asttypes.mli *)" >> $@
	$(PA_OCAML) --ascii asttypes.mli | ./pa_ast | $(PA_OCAML) --ascii >> $@
	@echo "" >> $@
	@echo "(* parsetree.mli *)" >> $@
	$(PA_OCAML) --ascii parsetree.mli | ./pa_ast | $(PA_OCAML) --ascii >> $@

compare.cmx: compare.ml
	ocamlopt -I +compiler-libs $<

clean:
	rm -f *.cmi *.cmx *.cmo *.cma *.cmxa *.o

distclean: clean
	rm -f pa_ast compare.ml asttypes.mli parsetree.mli
