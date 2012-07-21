include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -package dyp $(PACK)

patoline:Main.cmx PatolineLanguage.cmx PatolineConfig.cmx
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

clean:
	rm -f *~ \#*\# *.o *.cm[iox] Parser.ml
