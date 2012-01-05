all: Parser.ml
	ocamlbuild -use-ocamlfind -pp cpp Texprime.native

doc:
	ocamlbuild -use-ocamlfind -pp cpp doc.docdir/index.html

push:
	darcs push www.lama:/home/pmeun/www/darcs/texprime

pull:
	darcs pull http://www.lama.univ-savoie.fr/~raffalli/texprime/

%.ml: %.dyp 
	dypgen --no-mli Parser.dyp


clean:
	rm -Rf _build *~ \#*\#