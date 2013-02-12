all:caml.cmxs

caml.cmxs:caml.ml
	ocamlfind ocamlopt -I ../Patoline -shared -o $@ unix.cmxa -linkpkg $<

clean:
	rm -f *.cmxs *.cmxa *.o *~ *.cm[ioxa]
