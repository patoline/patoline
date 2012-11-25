include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -I ../Typography/_build -I ../Rbuffer $(PACK)
CAMLC=ocamlfind ocamlc -pp "cpp -w" -I ../Typography/_build -I ../Rbuffer $(PACK)

all:

DefaultFormat.cmxa:Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml
	$(CAMLOPT) -a -o $@ Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml

%.cmxa:DefaultFormat.cmxa %.ml
	$(CAMLOPT) -a -o $@ $*.ml


clean:
	rm -Rf *~ *.cm[oixa] *.cmxa
