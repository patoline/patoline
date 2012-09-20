include Makefile.config

FORMAT=$(shell ls Format/Format*.ml)
DRIVERS=Drivers/Pdf.cmxa Drivers/Bin.cmxa Drivers/GL.cmxa Drivers/Html.cmxa Drivers/SVG.cmxa

ifdef GL_PACK
PACK+=$(GL_PACK)
NATIVE_EXTRA+=Patoline/PatolineGL
else
  DRIVERS:=$(filter-out Drivers/GL.ml,$(DRIVERS))
endif

CAMLOPT=ocamlfind ocamlopt $(PACK)
CAMLC=ocamlfind ocamlc $(PACK)

VPATH=Typography:Drivers:Format

all:native byte prof

native:Typography/Typography.cmxa $(FORMAT:.ml=.cmxa) $(DRIVERS:.ml=.cmxa) Patoline/patoline DefaultGrammar.cmx ocaml-bibi/bibi.cmxa proof/proof $(NATIVE_EXTRA)
byte:Typography/Typography.cma ocaml-bibi/bibi.cma $(FORMAT:.ml=.cma) $(DRIVERS:.ml=.cma)

prof:Typography/Typography.p.cmxa ocaml-bibi/bibi.p.cmxa $(FORMAT:.mllib=.p.cmxa)

Patoline/patoline:Patoline
	make -C Patoline patoline
Patoline/PatolineGL:Patoline GL.cmxa
	make -C Patoline PatolineGL

.PHONY:test
test: Typography/Typography.cmxa Format/FormatDiapo.cmxa $(DRIVERS) Patoline/patoline DefaultGrammar.ttml
	Patoline/patoline --format FormatDiapo --driver FormatDiapo.M --ml test.txp
	ocamlfind ocamlopt -package Typography -I Format FormatDiapo.cmxa -linkpkg -o 'test.tmx'  -impl 'test.tml'
	./test

ocaml-bibi/bibi.cma:Typography/Typography.cma ocaml-bibi
	make -C ocaml-bibi bibi.cma
ocaml-bibi/bibi.cmxa:Typography/Typography.cmxa ocaml-bibi
	make -C ocaml-bibi bibi.cmxa
ocaml-bibi/bibi.p.cmxa:Typography/Typography.p.cmxa ocaml-bibi
	make -C ocaml-bibi bibi.p.cmxa

proof/proof:Typography.cmxa Drivers/Pdf.cmxa proof/proof.ml Rbuffer/rbuffer.cmxa
	cd proof; $(CAMLOPT) -I ../Typography -I ../Drivers -I ../Rbuffer -linkpkg -o proof rbuffer.cmxa Typography.cmxa Pdf.cmxa proof.ml

Format/DefaultFormat.cmxa:Format Typography.cmxa
	make -C Format DefaultFormat.cmxa
Format/DefaultFormat.cma:Format Typography.cma
	make -C Format DefaultFormat.cma
Format/DefaultFormat.p.cmax:Format Typography.p.cmxa
	make -C Format DefaultFormat.p.cmxa

Format/%.cmxa:DefaultFormat.cmxa Typography.cmxa Format/%.ml Rbuffer/rbuffer.cmxa
	$(CAMLOPT) -I Rbuffer -a -I Typography -I Format -o $@ Format/$*.ml
Format/%.p.cmxa:DefaultFormat.cmxa Typography.p.cmxa  Format/%.ml Rbuffer/rbuffer.p.cmxa
	$(CAMLOPT) -I Rbuffer  -p -I Typography -I Format -o $@ Format/$*.ml
Format/%.cma:DefaultFormat.cma Typography.cma  Format/%.ml Rbuffer/rbuffer.cma
	$(CAMLC) -I Rbuffer  -a -I Typography -I Format -o $@ Format/$*.ml


Typography/Typography.cma:Typography Rbuffer/rbuffer.cma
	make -C Typography Typography.cma
Typography/Typography.cmxa:Typography Rbuffer/rbuffer.cmxa
	make -C Typography Typography.cmxa
Typography/Typography.p.cmxa:Typography Rbuffer/rbuffer.p.cmxa
	make -C Typography Typography.p.cmxa
Typography/Typography.cmi:Typography.cmxa

Rbuffer/rbuffer.cma:Rbuffer
	make -C Rbuffer rbuffer.cma
Rbuffer/rbuffer.cmxa:Rbuffer
	make -C Rbuffer rbuffer.cmxa
Rbuffer/rbuffer.p.cmxa:Rbuffer
	make -C Rbuffer rbuffer.p.cmxa

Drivers/%.cmxa:Drivers Typography/Typography.cmxa Typography/Typography.cmi
	make -C Drivers $(shell basename $@)
Drivers/%.cma:Drivers Typography/Typography.cma Typography/Typography.cmi
	make -C Drivers $(shell basename $@)
Drivers/%.p.cmxa:Drivers Typography/Typography.p.cmxa Typography/Typography.cmi
	make -C Drivers $(shell basename $@)


DefaultGrammar.ttml:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --no-grammar --ml -c DefaultGrammar.txp

DefaultGrammar.cmx:Typography/Typography.cmxa
DefaultGrammar.cmx:Format/DefaultFormat.cmxa Drivers/Pdf.cmxa DefaultGrammar.ttml
	$(CAMLOPT) -c -o DefaultGrammar.cmx -I Rbuffer -I Typography -I Format -I Drivers DefaultFormat.cmxa Typography.cmxa Pdf.cmxa -impl DefaultGrammar.ttml

.PHONY :doc
doc:
	ocamlfind ocamldoc -package camomile -html -d doc -pp cpp -I Typography -I Typography/Output Typography/Document.ml


clean:
	make -C Typography clean
	make -C Format clean
	make -C Patoline clean
	make -C ocaml-bibi clean
	cd Drivers; rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd proof; rm -f *~ \#*\# *.cm[xoi] *.o
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.tgo *.tgx *.tml *.o *.a
