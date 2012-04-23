OPTS=-I Format
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) -pp "cpp -w" -g str.cma
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) -pp "cpp -w" -g str.cmxa

formats:
	make -C Format Diagrams.cmxa Article.cmxa

clean:
	make -C Format clean
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# *.tml
