include Makefile.config

FORMAT=$(shell ls Format/Format*.ml)
DRIVERS=$(shell ls Drivers/*.ml)

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


ocaml-bibi/bibi.cma:
	make -C ocaml-bibi bibi.cma
ocaml-bibi/bibi.cmxa:
	make -C ocaml-bibi bibi.cmxa
ocaml-bibi/bibi.p.cmxa:
	make -C ocaml-bibi bibi.p.cmxa

proof/proof:Typography.cmxa Drivers/Pdf.cmxa proof/proof.ml
	cd proof; $(CAMLOPT) -I ../Typography -I ../Drivers -linkpkg -o proof Typography.cmxa Pdf.cmxa proof.ml

Format/DefaultFormat.cmxa:Format Typography.cmxa
	make -C Format DefaultFormat.cmxa
Format/DefaultFormat.cma:Format Typography.cma
	make -C Format DefaultFormat.cma
Format/DefaultFormat.p.cmax:Format Typography.p.cmxa
	make -C Format DefaultFormat.p.cmxa

Format/%.cmxa:DefaultFormat.cmxa Typography.cmxa Format/%.ml
	$(CAMLOPT) -a -I Typography -I Format -o $@ Format/$*.ml
Format/%.p.cmxa:DefaultFormat.cmxa Typography.p.cmxa  Format/%.ml
	$(CAMLOPT) -p -I Typography -I Format -o $@ Format/$*.ml
Format/%.cma:DefaultFormat.cma Typography.cma  Format/%.ml
	$(CAMLC) -a -I Typography -I Format -o $@ Format/$*.ml


Typography/Typography.cma:Typography
	make -C Typography Typography.cma
Typography/Typography.cmxa:Typography
	make -C Typography Typography.cmxa
Typography/Typography.p.cmxa:Typography
	make -C Typography Typography.p.cmxa

Drivers/%.cmxa:Drivers Typography.cmxa
	make -C Drivers $(shell basename $@)
Drivers/%.cma:Drivers Typography.cma
	make -C Drivers $(shell basename $@)
Drivers/%.p.cmxa:Drivers Typography.p.cmxa
	make -C Drivers $(shell basename $@)


DefaultGrammar.ttml:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --no-grammar --ml -c DefaultGrammar.txp

DefaultGrammar.cmx:Typography/Typography.cmxa
DefaultGrammar.cmx:Format/DefaultFormat.cmxa Drivers/Pdf.cmxa DefaultGrammar.ttml
	$(CAMLOPT) -c -o DefaultGrammar.cmx -I Typography -I Format -I Drivers DefaultFormat.cmxa Typography.cmxa Pdf.cmxa -impl DefaultGrammar.ttml

clean:
	make -C Typography clean
	make -C Format clean
	make -C Patoline clean
	make -C ocaml-bibi clean
	cd Drivers; rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd proof; rm -f *~ \#*\# *.cm[xoi] *.o
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.tgo *.tgx *.tml *.o *.a
