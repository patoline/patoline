OCAMLBUILD=ocamlbuild
CAMLOPT=ocamlfind ocamlopt -package camomile,camlzip -linkpkg

all:native byte

native:Main.native texprimeDefault.tgx proof.native
byte:Main.byte texprimeDefault.tgo proof.byte

Main.native:Format/DefaultFormat.cmxa Texprime/Parser.ml
	cd Texprime; $(OCAMLBUILD) Main.native
	cp Texprime/Main.native texprime
Main.byte:Format/DefaultFormat.cma Texprime/Parser.ml
	cd Texprime; $(OCAMLBUILD) Main.byte
	cp Texprime/Main.byte texprime.byte
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

Texprime/Parser.ml:Texprime/Parser.dyp
	dypgen --no-mli Texprime/Parser.dyp
Typography/Typography.cmxa:
	make -C Typography Typography.cmxa
Typography/Typography.cma:
	make -C Typography Typography.cma



texprimeDefault.tgo: Texprime/Main.byte DefaultGrammar.txp
	rm -f texprimeDefault.tgo
	Texprime/Main.byte --ml --no-grammar --extra-fonts-dir ../Fonts DefaultGrammar.txp

texprimeDefault.tgx: Texprime/Main.native DefaultGrammar.txp
	rm -f texprimeDefault.tgx
	Texprime/Main.native --ml --no-grammar --extra-fonts-dir ../Fonts DefaultGrammar.txp


%.pdf: Main.native Typography/Typography.cmxa Format/DefaultFormat.cmxa texprimeDefault.tgx %.txp
	cp $*.txp _build
	cd _build; ../texprime --ml --extra-fonts-dir ../../Fonts  $*.txp

	cd _build; $(CAMLOPT) -o $*.tmx -I ../Typography/_build -I Format ../Typography/_build/Typography.cmxa Format/DefaultFormat.cmxa  -impl $*.tml
	cd _build; ./$*.tmx --extra-fonts-dir ../../Fonts


clean:
	rm -f *~ \#*\# *.tgo *.tgx
	rm -Rf _build
	make -C Typography clean
	rm -f Texprime/Parser.ml
	rm -f texprime texprime.byte proof.native proof/proof.native proof.byte proof/proof.byte
	rm -f *.tml