OCAMLBUILD=ocamlbuild
CAMLOPT=ocamlfind ocamlopt -package camomile,camlzip,camlimages.all_formats -linkpkg

FORMAT=$(shell ls Format/*.mllib)

all:native byte prof

native:Main.native DefaultGrammar.tgx proof.native
byte:Typography/Typography.cma $(FORMAT:.mllib=.cma)

prof:Typography/Typography.p.cmxa $(FORMAT:.mllib=.p.cmxa)

doc:
	$(OCAMLBUILD) Typography/Typography.docdir/index.html

Main.native: $(FORMAT:.mllib=.cmxa)
	$(OCAMLBUILD) -I Typography Patoline/Main.native

proof.native:Typography/Typography.cmxa
	$(OCAMLBUILD) proof/proof.native

Format/%.cmxa:Typography/Typography.cmxa
	$(OCAMLBUILD) Format/$*.cmxa
Format/%.cma:Typography/Typography.cma
	$(OCAMLBUILD) Format/$*.cma

Typography/Typography.cmxa:
	$(OCAMLBUILD) -I Typography Typography/Typography.cmxa
Typography/Typography.cma:
	$(OCAMLBUILD) -I Typography Typography/Typography.cma

Typography/Typography.p.cmxa:
	$(OCAMLBUILD) -I Typography Typography/Typography.p.cmxa
Typography/Typography.p.cma:
	$(OCAMLBUILD) -I Typography Typography/Typography.p.cma


DefaultGrammar.tgx: _build/Patoline/Main.native DefaultGrammar.txp
	rm -f _build/DefaultGrammar.tgx
	$(OCAMLBUILD)  DefaultGrammar.tgx

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
