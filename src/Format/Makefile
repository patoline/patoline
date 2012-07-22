include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -I ../Typography $(PACK)
CAMLC=ocamlfind ocamlc -pp "cpp -w" -I ../Typography $(PACK)

DefaultFormat.cmxa:Euler.cmx Numerals.cmx TableOfContents.cmx Sections.cmx ../Typography/Typography.cmi
	$(CAMLOPT) -a -o $@ Euler.cmx Numerals.cmx TableOfContents.cmx Sections.cmx DefaultFormat.ml
DefaultFormat.cma:Euler.cmo Numerals.cmo TableOfContents.cmo Sections.cmo ../Typography/Typography.cmi
	$(CAMLC) -a -o $@ Euler.cmo Numerals.cmo TableOfContents.cmo Sections.cmo DefaultFormat.ml

%.cmx:%.ml ../Typography/Typography.cmi
	$(CAMLOPT) -c Typography.cmxa -o $@ $<
%.cmo:%.ml ../Typography/Typography.cmi
	$(CAMLC) -c -o $@ $<
%.cmi:%.mli ../Typography/Typography.cmi
	$(CAMLC) -c -o $@ $<

clean:
	rm -f *~ \#*\# *.o *.cm[ioxa] *.cmxa
