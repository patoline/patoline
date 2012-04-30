OCAMLBUILD=ocamlbuild
CAMLOPT=ocamlfind ocamlopt -package camomile -linkpkg

all:native byte

native:Main.native texprimeDefault.tgx proof.native
byte:Main.byte texprimeDefault.tgo proof.byte

Main.native:Format/DefaultFormat.cmxa Texprime/Parser.ml
	$(OCAMLBUILD) Texprime/Main.native
	cp Main.native texprime
Main.byte:Format/DefaultFormat.cma Texprime/Parser.ml
	$(OCAMLBUILD) Texprime/Main.byte
	cp Main.byte texprime.byte
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
	$(OCAMLBUILD) Typography/Typography.cmxa
Typography/Typography.cma:
	$(OCAMLBUILD) Typography/Typography.cma


texprimeDefault.tgo: Main.byte DefaultGrammar.txp
	rm -f texprimeDefault.tgo
	./Main.byte --ml --no-grammar --extra-fonts-dir ../Otf DefaultGrammar.txp

texprimeDefault.tgx: Main.native DefaultGrammar.txp
	rm -f texprimeDefault.tgx
	./Main.native --ml --no-grammar --extra-fonts-dir ../Otf DefaultGrammar.txp


%.pdf: Typography/Typography.cmxa Format/DefaultFormat.cmxa texprimeDefault.tgx %.tml

	$(CAMLOPT) -o $*.tmx -I _build/Typography -I _build/Format _build/Typography/Typography.cmxa _build/Format/DefaultFormat.cmxa  -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf

%.tml:texprime.native %.txp
	./texprime.native --ml --extra-fonts-dir ../Otf $*.txp


clean:
	rm -f *~ \#*\# *.tgo *.tgx
	rm -Rf _build
	make -C Typography clean
	rm -f Texprime/Parser.ml
	rm -f texprime texprime.byte proof.native proof/proof.native proof.byte proof/proof.byte
	rm -f *.tml