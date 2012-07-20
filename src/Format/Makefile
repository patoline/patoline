include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -I ../Typography $(PACK)
CAMLC=ocamlfind ocamlc -pp "cpp -w" -I ../Typography $(PACK)

DefaultFormat.cmxa:Euler.cmx Numerals.cmx TableOfContents.cmx Sections.cmx
	$(CAMLOPT) -a -o $@ Euler.cmx Numerals.cmx TableOfContents.cmx Sections.cmx DefaultFormat.ml
DefaultFormat.cma:Euler.cmo Numerals.cmo TableOfContents.cmo Sections.cmo
	$(CAMLC) -a -o $@ Euler.cmo Numerals.cmo TableOfContents.cmo Sections.cmo DefaultFormat.ml

%.cmx:%.ml
	$(CAMLOPT) -c Typography.cmxa -o $@ $<
%.cmo:%.ml
	$(CAMLC) -c -o $@ $<
%.cmi:%.mli
	$(CAMLC) -c -o $@ $<

clean:
	rm -f *~ \#*\# *.o *.cm[ioxa] *.cmxa
