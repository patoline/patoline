include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -I ../Typography -I ../Rbuffer $(PACK)
CAMLC=ocamlfind ocamlc -pp "cpp -w" -I ../Typography -I ../Rbuffer $(PACK)

all:

%.cmxa:
	ocamlbuild -use-ocamlfind $@
#	$(CAMLOPT) -a -o $@ Euler.cmx Numerals.cmx TableOfContents.cmx Sections.cmx DefaultFormat.ml
%.cma:
	ocamlbuild -use-ocamlfind $@
#	$(CAMLC) -a -o $@ Euler.cmo Numerals.cmo TableOfContents.cmo Sections.cmo DefaultFormat.ml


clean:
	rm -Rf *~ _build
