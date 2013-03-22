include ../Makefile.config

CAMLOPT=ocamlfind ocamlopt -pp $(CPP) -package dyp -package camomile -I ../Rbuffer
CAMLC=ocamlfind ocamlc -pp $(CPP) -package dyp -package camomile -I ../Rbuffer

patoline: Language.cmx Config.cmx Parser.cmx Generateur.cmx SimpleGenerateur.cmx Main.cmx
	$(CAMLOPT) -linkpkg -o patoline -I +threads str.cmxa threads.cmxa rbuffer.cmxa dynlink.cmxa Util.cmx Language.cmx Config.cmx Build.cmx Parser.cmx Generateur.cmx SimpleGenerateur.cmx Main.cmx

PatolineGL:PatolineGL.ml ../Typography/_build/Typography.cmxa
	$(CAMLOPT) -package $(PACK_DRIVER_DriverGL) -linkpkg -I ../Rbuffer -I ../Drivers ../Rbuffer/rbuffer.cmxa ../Typography/_build/Typography.cmxa DriverGL.cmxa  -o $@ $<

PatolineGL2:PatolineGL2.ml ../Typography/_build/Typography.cmxa ../Drivers/GL2.cmxa
	$(CAMLOPT) -package $(PACK_DRIVER_GL2) -linkpkg -I ../Rbuffer -I ../Drivers ../Rbuffer/rbuffer.cmxa ../Typography/_build/Typography.cmxa gtkInit.cmx GL2.cmxa  -o $@ $<

Main.cmx:Generateur.cmx SimpleGenerateur.cmx Main.ml Build.cmx
	ocamlopt -thread -rectypes -I +threads -c -o Main.cmx Main.ml

Generateur.cmx:Parser.cmx Generateur.ml Build.cmx
Parser.cmx:Parser.ml Config.cmx Language.cmx Util.cmx Build.cmx
SubSuper.dyp:UnicodeScripts.ml UnicodeData.txt
	$(CAMLC) -o UnicodeScripts -package bigarray -package camomile -linkpkg UnicodeScripts.ml
	./UnicodeScripts
Parser.ml:Parser.dyp SubSuper.dyp
	cat  Parser.dyp SubSuper.dyp > tmp.dyp
	dypgen --no-mli --merge-warning tmp.dyp
	mv tmp.ml Parser.ml

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
	rm -f *~ \#*\# *.o *.cm[iox] Parser.ml SubSuper.dyp
