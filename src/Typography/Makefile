.PHONY:Typography.cma Typography.cmxa

all : Typography.cma Typography.cmxa

Typography.cma:
	ocamlbuild Typography.cma

Typography.cmxa:
	ocamlbuild Typography.cmxa

doc:
	mkdir -p _build
	ocamlc -I +ocamldoc -c -o _build/custom_doc.cmo custom_doc.ml
	ocamlbuild Typography.docdir/index.html

clean:
	rm -Rf _build *~
