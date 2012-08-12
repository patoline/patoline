include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -package dyp $(PACK)

patoline: PatolineLanguage.cmx PatolineConfig.cmx Parser.cmx Generateur.cmx SimpleGenerateur.cmx Main.cmx
	$(CAMLOPT) -linkpkg -o patoline str.cmxa PatolineLanguage.cmx PatolineConfig.cmx Parser.cmx Generateur.cmx SimpleGenerateur.cmx Main.cmx

PatolineGL:PatolineGL.ml
	$(CAMLOPT) $(GL_PACK) -linkpkg -I ../Drivers -I ../Typography str.cmxa Typography.cmxa GL.cmxa  -o $@ $<

Main.cmx:Generateur.cmx SimpleGenerateur.cmx

Generateur.cmx:Parser.cmx Generateur.ml
Parser.cmx:Parser.ml  PatolineConfig.cmx PatolineLanguage.cmx
Parser.ml:Parser.dyp
	dypgen --no-mli --merge-warning Parser.dyp

%.cmx:%.ml
	$(CAMLOPT) -rectypes -c -o $@ $<

Build.native:Build.ml
	ocamlbuild -ocamlopt "ocamlopt -thread -I +threads unix.cmxa str.cmxa threads.cmxa" -ocamlc "ocamlc -thread thread.cma" Build.native


clean:
	rm -f *~ \#*\# *.o *.cm[iox] Parser.ml
