include ../Makefile.config
CAMLOPT=ocamlfind ocamlopt $(PACK)

cesure:cesure.ml
	$(CAMLOPT) -I ../Typography/_build -I ../Rbuffer -o cesure rbuffer.cmxa Typography.cmxa str.cmxa -linkpkg cesure.ml

clean:
	- rm cesure
