include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -I ../Typography/_build -I ../Rbuffer $(PACK)
CAMLC=ocamlfind ocamlc -pp "cpp -w" -I ../Typography/_build -I ../Rbuffer $(PACK)

all:

DefaultFormat.cmxa:Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml ../Typography/_build/Typography.cmi ../Typography/_build/Typography.cmx
	$(CAMLOPT) -a -o $@ Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml

DefaultFormat.cma:Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml ../Typography/_build/Typography.cmi ../Typography/_build/Typography.cmo
	$(CAMLC) -a -o $@ Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml

DefaultFormat.p.cmxa:Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml  ../Typography/_build/Typography.cmi ../Typography/_build/Typography.cmx
	$(CAMLOPT) -p -a -o $@ Euler.ml Numerals.ml TableOfContents.ml Sections.ml DefaultFormat.ml

%.cmxa:DefaultFormat.cmxa %.ml
	$(CAMLOPT) -a -o $@ $*.ml

%.cma:DefaultFormat.cma %.ml
	$(CAMLC) -a -o $@ $*.ml

%.p.cmxa:DefaultFormat.p.cmxa %.ml
	$(CAMLOPT) -p -a -o $@ $*.ml


clean:
	rm -Rf *~ *.cm[oixa] *.cmxa
