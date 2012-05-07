
all:
	ocamlbuild -use-ocamlfind Dcats.pdf

plugin: 
	ocamlbuild -clean 
	ocamlbuild -ocamlopt "ocamlopt.opt str.cmxa" myocamlbuild.cmx  
	_build/sanitize.sh 
	ocamlbuild -ocamlopt "ocamlopt.opt str.cmxa" myocamlbuild.cmx

%.pdf: %.txp $(LIBS)
	ocamlbuild -use-ocamlfind %.pdf

%.cmxa:
	ocamlbuild -use-ocamlfind $@

%.cmxs:
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean
