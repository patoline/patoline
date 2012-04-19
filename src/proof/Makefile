CAMLOPT = ocamlfind ocamlopt -package camomile,dyp -linkpkg -pp "cpp -w" -I ../src/Typography ../src/Typography/Typography.cmxa

texprime-proof: proof.ml
	$(CAMLOPT) -o texprime-proof proof.ml

install:texprime-proof
	install -m 755 texprime-proof $(DESTDIR)$(PREFIX)

clean:
	rm -f *.cm[iox] *.o *~