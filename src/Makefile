OCAMLBUILD=ocamlbuild

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
	$(OCAMLBUILD) proof/proof.native
	cp proof.native proof
proof.byte:Typography/Typography.cma
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
	rm -f texprimeDefault.tgo
	./Main.native --ml --no-grammar --extra-fonts-dir ../Otf DefaultGrammar.txp


%.pdf: texprime Typography/Typography.cma Format/DefaultFormat.cma texprimeDefault.tgo %.txp
	./texprime.opt --ml --extra-fonts-dir ../Otf $*.txp
	$(CAMLC)  -o $*.tmx Typography/_build/Typography.cma Format/_build/DefaultFormat.cma  -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf

%.opt.pdf: texprime.opt Typography/Typography.cmxa DefaultFormat.cmxa texprimeDefault.tgx %.txp
	./texprime.opt --ml --extra-fonts-dir ../Otf $*.txp
	$(CAMLOPT)  -o $*.tmx Typography/_build/Typography.cmxa Format/_build/DefaultFormat.cmxa  -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf
	cp $*.pdf $*.opt.pdf


clean:
	rm -f *~ \#*\# *.tgo *.tgx
	rm -Rf _build
	make -C Typography clean
	rm -f Texprime/Parser.ml
	rm -f texprime texprime.byte proof.native proof/proof.native proof.byte proof/proof.byte
	rm -f *.tml