OCAMLBUILD=ocamlbuild
CAMLOPT=ocamlfind ocamlopt -package camomile,camlzip -linkpkg

FORMAT=$(shell ls Format/*Format*.ml)

all:native

native:Main.native DefaultGrammar.tgx proof.native DefaultGrammar.cmx
byte:Typography/Typography.cma

prof:Typography/Typography.p.cmxa $(FORMAT:.ml=.p.cmxa)

Main.native: $(FORMAT:.ml=.cmxa)
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


DefaultGrammar.ml:DefaultGrammar.tgx
	cp DefaultGrammar.tml DefaultGrammar.ml

DefaultGrammar.tgx: _build/Patoline/Main.native DefaultGrammar.txp
	rm -f patolineDefault.tgx
	./Main.native --ml --noamble --no-grammar --extra-fonts-dir ../Fonts DefaultGrammar.txp


%.cmx:%.ml
	$(OCAMLBUILD) $@
%.cmo:%.ml
	$(OCAMLBUILD) $@

%.pdf: Main.native Typography/Typography.cmxa Format/DefaultFormat.cmxa patolineDefault.tgx %.txp
	cp $*.txp patolineDefault.tgx _build
	cd _build; ../patoline --ml --extra-fonts-dir ../../Fonts  $*.txp

	cd _build; $(CAMLOPT) -o $*.tmx -I ../Typography/_build -I Format ../Typography/_build/Typography.cmxa Format/DefaultFormat.cmxa  -impl $*.tml
	cd _build; ./$*.tmx --extra-fonts-dir ../../Fonts
	ln -sf _build/$*.pdf .

clean:
	rm -f *~ \#*\# *.tgo *.tgx
	rm -Rf _build
	rm -f *.tml
