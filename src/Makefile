include Makefile.config

OCAMLBUILD=ocamlbuild
CAMLOPT=ocamlfind ocamlopt -package camomile,camlzip,camlimages.all_formats -linkpkg

FORMAT=$(shell ls Format/*.mllib)
DRIVERS=$(shell ls Drivers/*.ml)

all:native byte prof

native:Typography/Typography.cmxa Main.native DefaultGrammar.cmx $(DRIVERS:.ml=.cmxa) proof.native
byte:Typography/Typography.cma $(FORMAT:.mllib=.cma) $(DRIVERS:.ml=.cma)

prof:Typography/Typography.p.cmxa $(FORMAT:.mllib=.p.cmxa)

doc:
	$(OCAMLBUILD) Typography/Typography.docdir/index.html

Main.native: $(FORMAT:.mllib=.cmxa)
	$(OCAMLBUILD) -I Typography Patoline/Main.native

proof.native:Typography/Typography.cmxa Drivers/Pdf.cmxa proof/proof.ml
	mkdir -p _build;cp -R proof _build; cd _build/proof; $(CAMLOPT) -I ../Typography -I ../Drivers -o proof Typography.cmxa Pdf.cmxa proof.ml

Format/%.cmxa:
	$(OCAMLBUILD) Format/$*.cmxa
Format/%.cma:
	$(OCAMLBUILD) Format/$*.cma

Typography/Typography.cmxa:
	$(OCAMLBUILD) -I Typography Typography/Typography.cmxa
Typography/Typography.cma:
	$(OCAMLBUILD) -I Typography Typography/Typography.cma

Typography/Typography.p.cmxa:
	$(OCAMLBUILD) -I Typography Typography/Typography.p.cmxa
Typography/Typography.p.cma:
	$(OCAMLBUILD) -I Typography Typography/Typography.p.cma

Drivers/%.cmxa:Drivers/%.ml
	mkdir -p _build; cp -R Drivers _build; cd _build/Drivers; ocamlfind ocamlopt -package camomile $(PACK) -a -pp $(CPP) -I ../Typography -o $*.cmxa $*.ml
Drivers/%.p.cmxa:Drivers/%.ml
	mkdir -p _build; cp -R Drivers _build; cd _build/Drivers; ocamlfind ocamlopt -package camomile $(PACK) -a -p -pp $(CPP) -I ../Typography -o $*.p.cmxa $*.ml
Drivers/%.cma:Drivers/%.ml
	mkdir -p _build; cp -R Drivers _build; cd _build/Drivers; ocamlfind ocamlc -package camomile $(PACK) -a -pp $(CPP) -I ../Typography -o $*.cma $*.ml


DefaultGrammar.cmx: _build/Patoline/Main.native DefaultGrammar.txp
	$(OCAMLBUILD) -I Typography -I Format DefaultGrammar.cmx

%.cmx:%.ml
	$(OCAMLBUILD) $@
%.cmo:%.ml
	$(OCAMLBUILD) $@

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
	rm -f *~ \#*\# *.tgo *.tgx
	rm -Rf _build
	rm -f *.tml
