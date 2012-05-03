#OPTS=-I Format -I /Users/pe/Projets/texprime/src/Typography/_build -I /Users/pe/Projets/texprime/src/_build/Format
#CAMLC = ocamlfind ocamlc -package camomile,dyp -linkpkg $(OPTS) str.cma -pp "cpp -w" -g
#CAMLOPT = ocamlfind ocamlopt -package camomile,dyp -linkpkg $(OPTS) str.cmxa -pp "cpp -w"
#CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp  $(OPTS) -pp "cpp -w"
OPTS=-I Format -I /usr/local/lib/ocaml
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography,bibi -linkpkg $(OPTS) str.cma -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography,bibi -linkpkg $(OPTS) str.cmxa -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp,Typography,bibi  $(OPTS) -pp "cpp -w"

LIBS=Asana.cmx Diagrams.cmxa

all: Diagrams.cmxa Article.cmxa 

clean:
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# Article.mli


%.mli: %.ml
	$(CAMLC) $(OPTS) -i $*.ml > $*.mli

%.cmi: %.mli
	$(CAMLC) $(OPTS) -c $<

%.cma: %.ml
	$(CAMLC) $(OPTS) -a -o $@ $<

%.cmx: %.ml %.cmi
	$(CAMLOPT) $(OPTS) -c $<

%.cmxa: %.ml
	$(CAMLOPTA) $(OPTS) -a -o $@ $<

%.pdf: %.txp $(LIBS)
	texprime --ml --caml "-I /usr/local/lib/ocaml" --format Format/Article $*.txp
	cp $*.tml $*.ml
	ocamlbuild -use-ocamlfind -cflags -I,/usr/local/lib/ocaml/ $*.native
	./$*.native

%.cmxs: %.ml %.cmi
	$(CAMLOPT) -shared $(OPTS) -o $@ $<

%.cmx: %.txp
	texprime --noamble --ml --caml "-I /usr/local/lib/ocaml" --format Article $*.txp
	cp $*.tml $*.ml
	ocamlbuild -use-ocamlfind -cflags -I,/usr/local/lib/ocaml/ $*.cmx

