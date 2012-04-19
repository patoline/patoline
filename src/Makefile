all:native byte

native:Main.native texprimeDefault.tgx proof.native
byte:Main.byte texprimeDefault.tgo proof.byte

Main.native:Format/DefaultFormat.cmxa Texprime/Parser.ml
	ocamlbuild Texprime/Main.native
	cp Main.native texprime
Main.byte:Format/DefaultFormat.cma Texprime/Parser.ml
	ocamlbuild Texprime/Main.byte
	cp Main.byte texprime
proof.native:Typography/Typography.cmxa
	ocamlbuild proof/proof.native
proof.byte:Typography/Typography.cma
	ocamlbuild proof/proof.byte

Format/DefaultFormat.cmxa:Typography/Typography.cmxa
	ocamlbuild Format/DefaultFormat.cmxa
Format/DefaultFormat.cma:Typography/Typography.cma
	ocamlbuild Format/DefaultFormat.cma

Texprime/Parser.ml:Texprime/Parser.dyp
	dypgen --no-mli Texprime/Parser.dyp
Typography/Typography.cmxa:
	make -C Typography Typography.cmxa
Typography/Typography.cma:
	make -C Typography Typography.cma


texprimeDefault.tgo: Main.byte DefaultGrammar.txp
	./Main.byte --ml --extra-fonts-dir ../Otf DefaultGrammar.txp

texprimeDefault.tgx: Main.native DefaultGrammar.txp
	./Main.native --ml --extra-fonts-dir ../Otf DefaultGrammar.txp


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
	make -C Format clean
	make -C Texprime clean
	make -C proof clean
