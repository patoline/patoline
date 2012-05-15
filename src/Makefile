OCAMLBUILD=ocamlbuild
CAMLOPT=ocamlfind ocamlopt -package camomile,camlzip -linkpkg

all:native byte

native:Main.native DefaultGrammar.tgx proof.native DefaultGrammar.cmx
byte:Main.byte DefaultGrammar.tgo proof.byte DefaultGrammar.cmo

Main.native:Format/DefaultFormat.cmxa Patoline/Parser.ml
	cd Patoline; $(OCAMLBUILD) Main.native
	cp Patoline/Main.native patoline
Main.byte:Format/DefaultFormat.cma Patoline/Parser.ml
	cd Patoline; $(OCAMLBUILD) Main.byte
	cp Patoline/Main.byte patoline.byte
proof.native:Typography/Typography.cmxa
	rm -Rf proof.native _build/proof
	$(OCAMLBUILD) proof/proof.native
proof.byte:Typography/Typography.cma
	rm -Rf proof.byte _build/proof
	$(OCAMLBUILD) proof/proof.byte

Format/DefaultFormat.cmxa:Typography/Typography.cmxa
	$(OCAMLBUILD) Format/DefaultFormat.cmxa
Format/DefaultFormat.cma:Typography/Typography.cma
	$(OCAMLBUILD) Format/DefaultFormat.cma

Patoline/Parser.ml:Patoline/Parser.dyp
	dypgen --no-mli Patoline/Parser.dyp
Typography/Typography.cmxa:
	make -C Typography Typography.cmxa
Typography/Typography.cma:
	make -C Typography Typography.cma


DefaultGrammar.ml:DefaultGrammar.tgx
	cp DefaultGrammar.tml DefaultGrammar.ml
DefaultGrammar.tgo: Main.byte DefaultGrammar.txp
	rm -f patolineDefault.tgo
	Patoline/Main.byte --ml --noamble --no-grammar --extra-fonts-dir ../Fonts DefaultGrammar.txp

DefaultGrammar.tgx: Main.native DefaultGrammar.txp
	rm -f patolineDefault.tgx
	Patoline/Main.native --ml --noamble --no-grammar --extra-fonts-dir ../Fonts DefaultGrammar.txp


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
	rm -Rf Patoline/_build
	make -C Typography clean
	rm -f Patoline/Parser.ml
	rm -f patoline patoline.byte proof.native proof/proof.native proof.byte proof/proof.byte
	rm -f *.tml
