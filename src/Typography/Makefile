include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt -I ../Rbuffer $(PACK)
CAMLC=ocamlfind ocamlc -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt -I ../Rbuffer $(PACK)

.PHONY:_build/Typography.cmx _build/Typography.cmo _build/Typography.p.cmx

_build/Typography.cmxa:_build/Typography.cmx
	cd _build;ocamlopt -a -o Typography.cmxa Typography.cmx
_build/Typography.cmx:
	ocamlbuild -use-ocamlfind -classic-display Typography.cmx

_build/Typography.cma:_build/Typography.cmo
	cd _build;ocamlc -a -o Typography.cma Typography.cmo
_build/Typography.cmo:
	ocamlbuild -use-ocamlfind -classic-display Typography.cmo

_build/Typography.p.cmxa:_build/Typography.p.cmx
	cd _build;ocamlopt -a -o Typography.p.cmxa Typography.p.cmx
_build/Typography.p.cmx:
	ocamlbuild -use-ocamlfind -classic-display Typography.p.cmx


clean:
	rm -Rf _build
