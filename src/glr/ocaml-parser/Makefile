OCAMLOPT = ocamlfind ocamlopt -I +compiler-libs -package glr
OCAMLDEP = ocamlfind ocamldep -I +compiler-libs -package glr
PP = -pp ../pa_glr

MODULES = blank memoize atoms pa_ocaml
MODULES_ML = $(addsuffix .ml, $(MODULES))
MODULES_CMX = $(addsuffix .cmx, $(MODULES))

all: pa_ocaml

depend: $(MODULES_ML)
	$(OCAMLDEP) $(PP) $(MODULES_ML) > .depend

include .depend

%.cmx: %.ml
	$(OCAMLOPT) $(PP) -c $<

pa_ocaml: $(MODULES_CMX)
	$(OCAMLOPT) -o $@ str.cmxa glr.cmxa ocamlcommon.cmxa $(MODULES_CMX)

clean:
	rm -rf *.cmi *.cmo *.cma *.o *.a *.cmx pa_ocaml

distclean: clean
	echo "" > .depend
