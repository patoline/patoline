OPTS=-I Format -I /usr/local/lib/ocaml
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) str.cma -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) str.cmxa -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi  $(OPTS) -pp "cpp -w"

LIBS=Format/Asana.cmx Format/Diagrams.cmxa

all: Format/Diagrams.cmxa asana.tgx
	texprime --ml --caml "-I /usr/local/lib/ocaml Diagrams.cmxa" --extra-fonts-dir Format --format Format/Article Dcats.txp
	cp Dcats.tml Dcats.ml
	ocamlbuild -no-hygiene -use-ocamlfind -cflags -I,/usr/local/lib/ocaml/ Dcats.native
	./Dcats.native

asana.tgx:
	make -C Format Asana.cmx
	cp Format/asana.tgx .

%.pdf: %.txp $(LIBS)
	texprime --ml --caml "-I /usr/local/lib/ocaml" --format Format/Article $*.txp
	cp $*.tml $*.ml
	ocamlbuild -use-ocamlfind -cflags -I,/usr/local/lib/ocaml/ $*.native
	./$*.native

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

%.cmx: $(LIBS) %.txp
	texprime --ml --caml "-I /usr/local/lib/ocaml" --format Format/Article $*.txp
	cp $*.tml $*.ml
	ocamlbuild -use-ocamlfind -cflags -I,/usr/local/lib/ocaml/,-I,Format $*.cmx

formats:
	make -C Format Diagrams.cmxa Article.cmxa 

clean:
	make -C Format clean
	rm -f *.cm[ioxa] *.cmxa *.cmxs *.o *.a *~ \#*\# *.tml
