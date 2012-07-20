include Makefile.config

OCAMLBUILD=ocamlbuild
CAMLOPT=ocamlfind ocamlopt $(PACK)
CAMLC=ocamlfind ocamlc $(PACK)

FORMAT=$(shell ls Format/Format*.ml)
DRIVERS=$(shell ls Drivers/*.ml)

all:native byte prof

native:Typography/Typography.cmxa $(FORMAT:.ml=.cmxa) $(DRIVERS:.ml=.cmxa) Patoline/patoline Patoline/PatolineGL DefaultGrammar.cmx proof/proof
byte:Typography/Typography.cma $(FORMAT:.ml=.cma) $(DRIVERS:.ml=.cma)

prof:Typography/Typography.p.cmxa $(FORMAT:.mllib=.p.cmxa)

Patoline/patoline:
	make -C Patoline patoline

Patoline/PatolineGL:Drivers/GL.cmxa
	make -C Patoline PatolineGL

proof/proof:Typography/Typography.cmxa Drivers/Pdf.cmxa proof/proof.ml
	cd proof; $(CAMLOPT) -I ../Typography -I ../Drivers -linkpkg -o proof Typography.cmxa Pdf.cmxa proof.ml

Format/DefaultFormat.cmxa: Typography/Typography.cmxa
	make -C Format DefaultFormat.cmxa
Format/DefaultFormat.cma: Typography/Typography.cma
	make -C Format DefaultFormat.cma
Format/DefaultFormat.p.cmax: Typography/Typography.p.cmxa
	make -C Format DefaultFormat.p.cmxa

Format/%.cmxa:Format/DefaultFormat.cmxa Format/%.ml
	$(CAMLOPT) -a -I Typography -I Format -o $@ Format/$*.ml
Format/%.p.cmxa:Format/DefaultFormat.cmxa Format/%.ml
	$(CAMLOPT) -p -I Typography -I Format -o $@ Format/$*.ml
Format/%.cma:Format/DefaultFormat.cma Format/%.ml
	$(CAMLC) -a -I Typography -I Format -o $@ Format/$*.ml

Typography/Typography.cma:
	make -C Typography Typography.cma
Typography/Typography.cmxa:
	make -C Typography Typography.cmxa
Typography/Typography.p.cmxa:
	make -C Typography Typography.p.cmxa


Drivers/%.cmxa:Drivers/%.ml Typography/Typography.cmxa
	cd Drivers; ocamlfind ocamlopt -package camomile $(PACK) -a -pp $(CPP) -I ../Typography -o $*.cmxa $*.ml
Drivers/%.cma:Drivers/%.ml Typography/Typography.cma
	cd Drivers; ocamlfind ocamlc -package camomile $(PACK) -a -pp $(CPP) -I ../Typography Typography.cma -o $*.cma $*.ml
Drivers/%.p.cmxa:Drivers/%.ml Typography/Typography.cmxa
	cd Drivers; ocamlfind ocamlopt -package camomile $(PACK) -a -p -pp $(CPP) -I ../Typography -o $*.p.cmxa $*.ml


DefaultGrammar.tml:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --no-grammar --ml DefaultGrammar.txp
DefaultGrammar.cmx: Typography/Typography.cmxa Format/DefaultFormat.cmxa Drivers/Pdf.cmxa DefaultGrammar.tml
	$(CAMLOPT) -c -o DefaultGrammar.cmx -I Typography -I Format -I Drivers DefaultFormat.cmxa Typography.cmxa Pdf.cmxa -impl DefaultGrammar.tml

%.pdf: Main.native Typography/Typography.cmxa Format/DefaultFormat.cmxa DefaultGrammar.tgx %.txp
	cp $*.txp _build
	cd _build; Patoline/Main.native --ml --extra-fonts-dir ../../Fonts  $*.txp

	cd _build; $(CAMLOPT) -o $*.tmx -I Typography -I Format Typography/Typography.cmxa Format/DefaultFormat.cmxa -impl $*.tml
	cd _build; ./$*.tmx --extra-fonts-dir ../../Fonts --extra-hyph-dir ../../Hyphenation
	ln -sf _build/$*.pdf .



DefaultGrammar.pdf: Main.native Typography/Typography.cmxa Format/DefaultFormat.cmxa DefaultGrammar.txp
	cp DefaultGrammar.txp _build
	cd _build; Patoline/Main.native --ml --no-grammar --extra-fonts-dir ../../Fonts  DefaultGrammar.txp
	cd _build; $(CAMLOPT) -o DefaultGrammar.tmx -I Typography -I Format Typography/Typography.cmxa Format/DefaultFormat.cmxa -impl DefaultGrammar.tml
	cd _build; ./DefaultGrammar.tmx --extra-fonts-dir ../../Fonts --extra-hyph-dir ../../Hyphenation
	ln -sf _build/DefaultGrammar.pdf .

clean:
	make -C Typography clean
	make -C Format clean
	make -C Patoline clean
	cd Drivers; rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd proof; rm -f *~ \#*\# *.cm[xoi] *.o
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.tgo *.tgx *.tml *.o *.a
