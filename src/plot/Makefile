plot.cmxa:plot.ml
	ocamlfind ocamlopt -package camomile -I ../Typography/_build -I ../Format -a -o plot.cmxa plot.ml
plot.p.cmxa:plot.ml
	ocamlfind ocamlopt -package camomile -p -I ../Typography/_build -I ../Format -a -o plot.p.cmxa plot.ml
plot.cma:plot.ml
	ocamlfind ocamlc -package camomile -I ../Typography/_build -I ../Format -a -o plot.cma plot.ml

clean:
	rm -f *.o *.cm[ioxa] *.cmxa *~ *.a
