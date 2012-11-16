include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt -I ../Rbuffer $(PACK)
CAMLC=ocamlfind ocamlc -pp $(CPP) -I Fonts -I Output -I Fonts/Sfnt -I ../Rbuffer $(PACK)

Typography.cmxa:
	ocamlbuild -use-ocamlfind -classic-display Typography.cmx
	cd _build;ocamlopt -a -o Typography.cmxa Typography.cmx
Typography.cma:
	ocamlbuild -use-ocamlfind -classic-display Typography.cmo
	cd _build;ocamlc -a -o Typography.cma Typography.cmo
Typography.p.cmxa:
	ocamlbuild -use-ocamlfind -classic-display Typography.p.cmx
	cd _build;ocamlopt -a -o Typography.p.cmxa Typography.p.cmx

clean:
	rm -Rf _build
