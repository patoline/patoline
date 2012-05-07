OPTS=-I Format -I /usr/local/lib/ocaml
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) str.cma -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi -linkpkg $(OPTS) str.cmxa -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp,Typography,sqlite3,bibi  $(OPTS) -pp "cpp -w"

LIBS=Format/Asana.cmx Format/Diagrams.cmxa

all:
	ocamlbuild -use-ocamlfind Dcats.pdf

plugin: 
	ocamlbuild -clean 
	ocamlbuild -ocamlopt "ocamlopt.opt str.cmxa" myocamlbuild.cmx  
	_build/sanitize.sh 
	ocamlbuild -ocamlopt "ocamlopt.opt str.cmxa" myocamlbuild.cmx

%.pdf: %.txp $(LIBS)
	ocamlbuild -use-ocamlfind %.pdf

clean:
	ocamlbuild -clean
