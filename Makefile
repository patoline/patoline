all: Parser.ml
	ocamlbuild -use-ocamlfind -pp cpp Texprime.native

doc:
	ocamlbuild -use-ocamlfind -pp cpp doc.docdir/index.html

%.ml: %.dyp 
	dypgen --no-mli Parser.dyp


clean:
	rm -Rf _build *~ \#*\#