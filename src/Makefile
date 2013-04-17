include Makefile.config

FORMAT=$(shell cd Format; ls Format*.ml) LMFormat.ml

ifdef PACK_DRIVER_DriverGL
NATIVE_EXTRA+=Patoline/PatolineGL
endif

ifdef PACK_DRIVER_DriverGL2
NATIVE_EXTRA+=Patoline/PatolineGL2
endif

CAMLOPT=ocamlfind ocamlopt $(PACK)
CAMLC=ocamlfind ocamlc $(PACK)

VPATH=Typography:Pdf:Drivers:Format:cesure

all:native $(BIBI)

native:Typography/_build/Typography.cmxa Typography/_build/ParseMainArgs.cmx $(addprefix Format/,$(FORMAT:.ml=.cmxa)) $(DRIVERS) proof/proof Pdf/_build/pdf_parser.cmxa plot/plot.cmxa $(PATOLINE) $(NATIVE_EXTRA) cesure/cesure DefaultGrammar.tgx all_plugins

byte:Typography/_build/Typography.cma Typography/_build/ParseMainArgs.cmo $(addprefix Format/,$(FORMAT:.ml=.cma)) $(DRIVERS:.cmxa=.cma) Pdf/_build/pdf_parser.cma plot/plot.cma

prof:Typography/_build.p/Typography.p.cmxa $(addprefix Format/,$(FORMAT:.ml=.p.cmxa)) Pdf/_build/pdf_parser.p.cmxa plot/plot.p.cmxa

all_plugins:plugins Patoline/patoline
	make -C plugins all

Patoline/patoline:Patoline
	make -C Patoline patoline
Patoline/PatolineGL:Patoline Drivers/DriverGL.cmxa
	make -C Patoline PatolineGL
Patoline/PatolineGL2:Patoline GL2.cmxa
	make -C Patoline PatolineGL2

cesure/cesure:Typography/_build/Typography.cmxa cesure/cesure.ml
	make -C cesure cesure

ocaml-bibi/bibi.cma:Typography/_build/Typography.cma ocaml-bibi Format/DefaultFormat.cma
	make -C ocaml-bibi bibi.cma
ocaml-bibi/bibi.cmxa:Typography/_build/Typography.cmi ocaml-bibi Format/DefaultFormat.cmxa
	make -C ocaml-bibi bibi.cmxa
ocaml-bibi/bibi.p.cmxa:Typography/_build.p/Typography.p.cmxa ocaml-bibi Format/DefaultFormat.p.cmxa
	make -C ocaml-bibi bibi.p.cmxa


proof/proof:Drivers/Pdf.cmxa proof/proof.ml Rbuffer/rbuffer.cmxa
	cd proof; $(CAMLOPT) -I ../Typography/_build -I ../Drivers -I ../Rbuffer -linkpkg -o proof rbuffer.cmxa ../Typography/_build/Typography.cmxa ../Drivers/Pdf.cmxa proof.ml


Pdf/_build/pdf_parser.cmxa:Typography/_build/Typography.cmxa
	make -C Pdf pdf_parser.cmxa
Pdf/_build/pdf_parser.cma:Typography/_build/Typography.cma
	make -C Pdf pdf_parser.cma
Pdf/_build/pdf_parser.p.cmxa:Typography/_build.p/Typography.p.cmxa
	make -C Pdf pdf_parser.p.cmxa

Format/DefaultFormat.cmxa:Format Typography/_build/Typography.cmxa
	make -C Format DefaultFormat.cmxa
Format/DefaultFormat.cma:Format Typography/_build/Typography.cma
	make -C Format DefaultFormat.cma

Format/DefaultFormat.p.cmxa:Format Typography/_build.p/Typography.p.cmxa
	make -C Format DefaultFormat.p.cmxa

Format/%.cmxa:Format/DefaultFormat.cmxa Typography/_build/Typography.cmxa Format/%.ml
	make -C Format $(shell basename $@)
Format/%.p.cmxa:Format/DefaultFormat.p.cmxa Typography/_build.p/Typography.p.cmxa Format/%.ml
	make -C Format $(shell basename $@)
Format/%.cma:Format/DefaultFormat.cma Typography/_build/Typography.cma Format/%.ml
	make -C Format $(shell basename $@)

.PHONY:Typography/_build/Typography.cma Typography/_build/Typography.cmxa Typography/_build.p/Typography.p.cmxa
Typography/_build/Typography.cma:Rbuffer/rbuffer.cma
	make -C Typography _build/Typography.cma
Typography/_build/Typography.cmxa:Rbuffer/rbuffer.cmxa
	make -C Typography _build/Typography.cmxa
Typography/_build.p/Typography.p.cmxa:Rbuffer/rbuffer.p.cmxa
	make -C Typography _build.p/Typography.p.cmxa

Typography/_build/Typography.cmi:Typography/_build/Typography.cma

.PHONY:Typography/_build/ParseMainArgs.cmo Typography/_build/ParseMainArgs.cmx Typography/_build.p/ParseMainArgs.p.cmx
Typography/_build/ParseMainArgs.cmo:Rbuffer/rbuffer.cmo
	make -C Typography _build/ParseMainArgs.cmo
Typography/_build/ParseMainArgs.cmx:Rbuffer/rbuffer.cmx
	make -C Typography _build/ParseMainArgs.cmx
Typography/_build.p/ParseMainArgs.p.cmx:Rbuffer/rbuffer.p.cmx
	make -C Typography _build.p/ParseMainArgs.p.cmx

Rbuffer/rbuffer.cma:Rbuffer
	make -C Rbuffer rbuffer.cma
Rbuffer/rbuffer.cmxa:Rbuffer
	make -C Rbuffer rbuffer.cmxa
Rbuffer/rbuffer.p.cmxa:Rbuffer
	make -C Rbuffer rbuffer.p.cmxa

Drivers/%.cmxa:Typography/_build/Typography.cmxa Drivers
	make -C Drivers $(shell basename $@)
Drivers/%.cma:Typography/_build/Typography.cma Drivers
	make -C Drivers $(shell basename $@)
Drivers/%.p.cmxa:Typography/_build.p/Typography.p.cmxa Drivers
	make -C Drivers $(shell basename $@)

#Drivers/Typography.cmi:Typography/_build/Typography.cmi
#	cp Typography/_build/Typography.cmi Drivers

plot/plot.cmxa:plot Format/DefaultFormat.cmxa
	make -C plot plot.cmxa
plot/plot.cma:plot Format/DefaultFormat.cmaa
	make -C plot plot.cma
plot/plot.p.cmxa:plot Format/DefaultFormat.p.cmxa
	make -C plot plot.p.cmxa

DefaultGrammar.tgx:DefaultGrammar.pdf

DefaultGrammar.pdf:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --main-ml --driver Pdf -o DefaultGrammar_.tml DefaultGrammar.txp
	Patoline/patoline --ml --driver Pdf -o DefaultGrammar.ttml DefaultGrammar.txp
	ocamlfind ocamlopt -package camomile -I Format -I Typography/_build Typography.cmxa -c -o DefaultGrammar.cmx -impl DefaultGrammar.ttml
	ocamlfind ocamlopt -package camomile,zip,camlimages.all_formats -I Rbuffer -I Format -I Drivers -I Typography/_build rbuffer.cmxa Typography.cmxa DefaultFormat.cmxa Pdf.cmxa -linkpkg -o DefaultGrammar.tmx ParseMainArgs.cmx DefaultGrammar.cmx -impl DefaultGrammar_.tml
	./DefaultGrammar.tmx --extra-fonts-dir ../Fonts --extra-hyph-dir ../Hyphenation

.PHONY: test.pdf test

test.pdf:test.txp Format/DefaultFormat.cmxa Drivers/Pdf.cmxa
	Patoline/patoline --ml test.txp
	$(CAMLOPT) -o test.tmx -I Rbuffer -I Typography/_build -I Format -I Drivers -I Rbuffer rbuffer.cmxa Typography.cmxa DefaultFormat.cmxa str.cmxa FormatThese.cmxa Pdf.cmxa -linkpkg -impl test.tml
	./test.tmx

test:test.txp Format/DefaultFormat.cmxa Format/FormatSlides.cmxa Drivers/SVG.cmxa
	Patoline/patoline --ml test.txp
	$(CAMLOPT) -o test.tmx -I Rbuffer -I Typography/_build -I Format -I Drivers -I Rbuffer rbuffer.cmxa Typography.cmxa DefaultFormat.cmxa str.cmxa FormatSlides.cmxa SVG.cmxa -linkpkg -impl test.tml
	./test.tmx



.PHONY :doc
doc:
	make -C Typography doc

install:
	make -C .. install

clean:
	make -C Typography clean
	make -C Format clean
	make -C Drivers clean
	make -C Patoline clean
	make -C ocaml-bibi clean
	make -C Pdf clean
	make -C cesure clean
	cd Drivers; rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd proof; rm -f *~ \#*\# *.cm[xoi] *.o
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.tgo *.tgx *.tml *.ttml *.o *.a *.tmx
