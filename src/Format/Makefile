include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -I ../Typography $(PACK)
CAMLC=ocamlfind ocamlc -pp "cpp -w" -I ../Typography $(PACK)

DefaultFormat.cmxa:Euler.cmx Numerals.cmx TableOfContents.cmx Sections.cmx ../Typography/Typography.cmxa
	$(CAMLOPT) -a -o $@ Euler.cmx Numerals.cmx TableOfContents.cmx Sections.cmx DefaultFormat.ml
DefaultFormat.cma:Euler.cmo Numerals.cmo TableOfContents.cmo Sections.cmo ../Typography/Typography.cma
	$(CAMLC) -a -o $@ Euler.cmo Numerals.cmo TableOfContents.cmo Sections.cmo DefaultFormat.ml

%.cmx:%.ml ../Typography/Typography.cmxa
	$(CAMLOPT) -c Typography.cmxa -o $@ $*.ml
%.cmo:%.ml ../Typography/Typography.cma
	$(CAMLC) -c -o $@ $*.ml
%.cmi:%.mli ../Typography/Typography.cmi
	$(CAMLC) -c -o $@ $*.ml

clean:
	rm -f *~ \#*\# *.o *.cm[ioxa] *.cmxa
