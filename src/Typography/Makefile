include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt -I ../Rbuffer $(PACK)
CAMLC=ocamlfind ocamlc -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt -I ../Rbuffer $(PACK)

DOCDIR=Typography.doc
CAMLDOC=ocamlfind ocamldoc -d $(DOCDIR) -html -charset UTF-8 -package camomile,rbuffer -rectypes -pp 'cpp -C -w' -I _build -I _build/Output -I _build/Fonts

FILES=$(shell ls *.ml */*.ml */*/*.ml) $(shell ls *.mli */*.mli */*/*.mli)

_build/Typography.cmxa:_build/Typography.cmx
	cd _build;ocamlopt -a -o Typography.cmxa Typography.cmx
_build/Typography.cmx: $(FILES) ../Rbuffer/rbuffer.cmxa
	ocamlbuild -use-ocamlfind -classic-display Typography.cmx

_build/Typography.cma:_build/Typography.cmo
	cd _build;ocamlc -a -o Typography.cma Typography.cmo
_build/Typography.cmo: $(FILES) ../Rbuffer/rbuffer.cma
	ocamlbuild -use-ocamlfind -classic-display Typography.cmo

_build/Typography.p.cmxa:_build/Typography.p.cmx
	cd _build;ocamlopt -a -o Typography.p.cmxa Typography.p.cmx
_build/Typography.p.cmx: $(FILES) ../Rbuffer/rbuffer.cmxa
	ocamlbuild -use-ocamlfind -classic-display Typography.p.cmx

doc:
	mkdir -p $(DOCDIR)
	$(CAMLDOC) $(FILES)



clean:
	rm -Rf _build
