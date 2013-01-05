plot.cmxa:plot.ml
	ocamlopt -I ../Typography/_build -I ../Format -a -o plot.cmxa plot.ml
plot.p.cmxa:plot.ml
	ocamlopt -p -I ../Typography/_build -I ../Format -a -o plot.p.cmxa plot.ml
plot.cma:plot.ml
	ocamlc -I ../Typography/_build -I ../Format -a -o plot.cma plot.ml

clean:
	rm -f *.o *.cm[ioxa] *.cmxa *~ *.a
