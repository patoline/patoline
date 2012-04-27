OPTS=-I Format -I /usr/local/lib/ocaml
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) str.cma -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) str.cmxa -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi  $(OPTS) -pp "cpp -w"


%.mli: %.ml
	$(CAMLC) $(OPTS) -i $*.ml > $*.mli

%.cmi: %.mli
	$(CAMLC) $(OPTS) -c $<

%.cma: %.ml
	$(CAMLC) $(OPTS) -a -o $@ $<

%.cmx: %.ml %.cmi
	$(CAMLOPT) $(OPTS) -c $<

%.cmxs: %.ml %.cmi
	$(CAMLOPT) -shared $(OPTS) -o $@ $<

%.cmxa: %.ml
	$(CAMLOPTA) $(OPTS) -a -o $@ $<

%.tmx: %.tml
	cp $*.tml $*.ml
	$(CAMLOPT) $(OPTS) -o $@ Diagrams.cmxa Article.cmxa Euler.cmxa $*.ml

formats:
	make -C Format Diagrams.cmxa Article.cmxa

clean:
	make -C Format clean
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# *.tml
