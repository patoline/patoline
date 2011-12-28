all:
	ocamlbuild -use-ocamlfind -pp cpp test2.native

doc:
	ocamlbuild -use-ocamlfind -pp cpp doc.docdir/index.html
clean:
	rm -Rf _build *~ \#*\#