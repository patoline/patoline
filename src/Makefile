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

all:native byte prof

native:Typography/Typography.cmxa $(FORMAT:.ml=.cmxa) $(DRIVERS:.ml=.cmxa) Patoline/patoline DefaultGrammar.cmx proof/proof $(NATIVE_EXTRA)
byte:Typography/Typography.cma $(FORMAT:.ml=.cma) $(DRIVERS:.ml=.cma)

prof:Typography/Typography.p.cmxa $(FORMAT:.mllib=.p.cmxa)

Patoline/patoline:Patoline
	make -C Patoline patoline
Patoline/PatolineGL:Patoline Drivers/GL.cmxa
	make -C Patoline PatolineGL

proof/proof:Typography/Typography.cmxa Drivers/Pdf.cmxa proof/proof.ml
	cd proof; $(CAMLOPT) -I ../Typography -I ../Drivers -linkpkg -o proof Typography.cmxa Pdf.cmxa proof.ml

Format/DefaultFormat.cmxa:Format Typography/Typography.cmxa
	make -C Format DefaultFormat.cmxa
Format/DefaultFormat.cma:Format Typography/Typography.cma
	make -C Format DefaultFormat.cma
Format/DefaultFormat.p.cmax:Format Typography/Typography.p.cmxa
	make -C Format DefaultFormat.p.cmxa

Format/%.cmxa:Format/DefaultFormat.cmxa Format/%.ml Format/%.cma
	$(CAMLOPT) -a -I Typography -I Format -o $@ Format/$*.ml
Format/%.p.cmxa:Format/DefaultFormat.cmxa Format/%.ml Format/%.cma
	$(CAMLOPT) -p -I Typography -I Format -o $@ Format/$*.ml
Format/%.cma:Format/DefaultFormat.cma Format/%.ml
	$(CAMLC) -a -I Typography -I Format -o $@ Format/$*.ml

Typography/Typography.cma:Typography
	make -C Typography Typography.cma
Typography/Typography.cmxa:Typography
	make -C Typography Typography.cmxa
Typography/Typography.p.cmxa:Typography
	make -C Typography Typography.p.cmxa


Drivers/%.cmxa:Drivers/%.ml Typography/Typography.cmxa
	cd Drivers; ocamlfind ocamlopt -package camomile $(PACK) -a -pp $(CPP) -I ../Typography -o $*.cmxa $*.ml
Drivers/%.cma:Drivers/%.ml Typography/Typography.cma
	cd Drivers; ocamlfind ocamlc -package camomile $(PACK) -a -pp $(CPP) -I ../Typography Typography.cma -o $*.cma $*.ml
Drivers/%.p.cmxa:Drivers/%.ml Typography/Typography.cmxa
	cd Drivers; ocamlfind ocamlopt -package camomile $(PACK) -a -p -pp $(CPP) -I ../Typography -o $*.p.cmxa $*.ml


DefaultGrammar.ttml:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --no-grammar --ml -c DefaultGrammar.txp
DefaultGrammar.cmx: Typography/Typography.cmxa Format/DefaultFormat.cmxa Drivers/Pdf.cmxa DefaultGrammar.ttml
	$(CAMLOPT) -c -o DefaultGrammar.cmx -I Typography -I Format -I Drivers DefaultFormat.cmxa Typography.cmxa Pdf.cmxa -impl DefaultGrammar.ttml

clean:
	make -C Typography clean
	make -C Format clean
	make -C Patoline clean
	cd Drivers; rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd proof; rm -f *~ \#*\# *.cm[xoi] *.o
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.tgo *.tgx *.tml *.o *.a
