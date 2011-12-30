all:
	ocamlbuild -use-ocamlfind -pp cpp Texprime.native

doc:
	ocamlbuild -use-ocamlfind -pp cpp doc.docdir/index.html
clean:
	rm -Rf _build *~ \#*\#