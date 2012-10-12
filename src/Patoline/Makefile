include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp "cpp -w" -package dyp $(PACK)

patoline: Language.cmx Config.cmx Parser.cmx Generateur.cmx SimpleGenerateur.cmx Main.cmx
	$(CAMLOPT) -linkpkg -o patoline -I +threads str.cmxa threads.cmxa dynlink.cmxa Util.cmx Language.cmx Config.cmx Parser.cmx Build.cmx Generateur.cmx SimpleGenerateur.cmx Main.cmx

PatolineGL:PatolineGL.ml ../Typography/Typography.cmxa ../Drivers/GL.cmxa
	$(CAMLOPT) $(GL_PACK) -package netcgi2,nethttpd -linkpkg -I ../Rbuffer -I ../Drivers ../Rbuffer/rbuffer.cmxa ../Typography/Typography.cmxa GL.cmxa  -o $@ $<

PatolineGL2:PatolineGL2.ml ../Typography/Typography.cmxa ../Drivers/GL2.cmxa
	$(CAMLOPT) $(GLGTK_PACK) -package lablgtk2-gl.gtkgl -linkpkg -I ../Rbuffer -I ../Drivers str.cmxa ../Rbuffer/rbuffer.cmxa ../Typography/Typography.cmxa gtkInit.cmx GL2.cmxa  -o $@ $<

Main.cmx:Generateur.cmx SimpleGenerateur.cmx Main.ml Build.cmx
	ocamlopt -thread -rectypes -I +threads -c -o Main.cmx Main.ml

Generateur.cmx:Parser.cmx Generateur.ml Build.cmx
Parser.cmx:Parser.ml Config.cmx Language.cmx Util.cmx
Parser.ml:Parser.dyp
	dypgen --no-mli --merge-warning Parser.dyp

Parser.cmx:Parser.ml
	$(CAMLOPT) -rectypes -c -o $@ $<
Generateur.cmx:Generateur.ml
	$(CAMLOPT) -rectypes -c -o $@ $<

%.cmx:%.ml
	$(CAMLOPT) -c -o $@ $<
Util.cmi:Util.cmx

Build.cmx:Build.ml Build.cmi Util.cmx
	$(CAMLOPT) -thread -c -o $@ $<
Build.cmi:Build.mli Util.cmi
	$(CAMLOPT) -thread -c -o $@ $<

clean:
	rm -f *~ \#*\# *.o *.cm[iox] Parser.ml
