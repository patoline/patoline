#OPTS=-I Format -I /Users/pe/Projets/texprime/src/Typography/_build -I /Users/pe/Projets/texprime/src/_build/Format -I /Users/pe/Projets/ocaml-bibi/_build
#CAMLC = ocamlfind ocamlc -package camomile,dyp -linkpkg $(OPTS) Typography.cma DefaultFormat.cma bibi.cma -pp "cpp -w" -g
#CAMLOPT = ocamlfind ocamlopt -package camomile,dyp -linkpkg $(OPTS) Typography.cmxa DefaultFormat.cmxa bibi.cmxa -pp "cpp -w" -g str.cmxa
OPTS=-I Format
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) -pp "cpp -w" -g str.cma
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) -pp "cpp -w" -g str.cmxa

TEXPRIME=/Users/pe/Projets/texprime/src/texprime --extra-grammars-dir /Users/pe/Projets/texprime/src/

Dcats.pdf:Dcats.tmx
	./Dcats.tmx --extra-fonts-dir /Users/pe/Projets/texprime/Otf --extra-hyph-dir /Users/pe/Projets/texprime/Hyphenation

Dcats.tmx:Dcats.tml Format/Article.cmxa Format/Diagrams.cmxa
	$(CAMLOPT) -o Dcats.tmx Format/Diagrams.cmxa Format/Article.cmxa -impl Dcats.tml
Dcats.tml:Dcats.txp
	$(TEXPRIME) --ml --format Format/Article Dcats.txp

Format/Article.cma:
	make -C Format Article.cma
Format/Article.cmxa:
	make -C Format Article.cmxa
Format/Diagrams.cma:
	make -C Format Diagrams.cma
Format/Diagrams.cmxa:
	make -C Format Diagrams.cmxa

clean:
	make -C Format clean
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# *.tml
