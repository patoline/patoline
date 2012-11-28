include Makefile.config

FORMAT=$(shell cd Format; ls Format*.ml) LMFormat.ml

ifdef PACK_DRIVER_GL
NATIVE_EXTRA+=Patoline/PatolineGL
endif

ifdef PACK_DRIVER_GL2
NATIVE_EXTRA+=Patoline/PatolineGL2
endif

CAMLOPT=ocamlfind ocamlopt $(PACK)
CAMLC=ocamlfind ocamlc $(PACK)

VPATH=Typography:Pdf:Drivers:Format

all:native prof DefaultGrammar.pdf

native:Typography/_build/Typography.cmxa $(addprefix Format/,$(FORMAT:.ml=.cmxa)) $(DRIVERS) Patoline/patoline DefaultGrammar.cmx ocaml-bibi/bibi.cmxa proof/proof Pdf/pdf_parser.cmxa $(NATIVE_EXTRA)
byte:Typography/_build/Typography.cma ocaml-bibi/bibi.cma $(addprefix Format/,$(FORMAT:.ml=.cma)) $(DRIVERS:.cmxa=.cma) Pdf/pdf_parser.cma

prof:Typography/_build/Typography.p.cmxa ocaml-bibi/bibi.p.cmxa $(addprefix Format/,$(FORMAT:.ml=.p.cmxa)) Pdf/pdf_parser.p.cmxa

Patoline/patoline:Patoline
	make -C Patoline patoline
Patoline/PatolineGL:Patoline GL.cmxa
	make -C Patoline PatolineGL
Patoline/PatolineGL2:Patoline GL2.cmxa
	make -C Patoline PatolineGL2


ocaml-bibi/bibi.cma:Typography/_build/Typography.cma ocaml-bibi
	make -C ocaml-bibi bibi.cma
ocaml-bibi/bibi.cmxa:Typography/_build/Typography.cmi ocaml-bibi
	make -C ocaml-bibi bibi.cmxa
ocaml-bibi/bibi.p.cmxa:Typography/_build/Typography.p.cmxa ocaml-bibi
	make -C ocaml-bibi bibi.p.cmxa


proof/proof:Drivers/Pdf.cmxa proof/proof.ml Rbuffer/rbuffer.cmxa
	cd proof; $(CAMLOPT) -I ../Typography/_build -I ../Drivers -I ../Rbuffer -linkpkg -o proof rbuffer.cmxa ../Typography/_build/Typography.cmxa ../Drivers/Pdf.cmxa proof.ml


Pdf/pdf_parser.cmxa:Typography/_build/Typography.cmxa
	make -C Pdf pdf_parser.cmxa
Pdf/pdf_parser.cma:Typography/_build/Typography.cma
	make -C Pdf pdf_parser.cma
Pdf/pdf_parser.p.cmxa:Typography/_build/Typography.p.cmxa
	make -C Pdf pdf_parser.p.cmxa

Format/DefaultFormat.cmxa:Format Typography/_build/Typography.cmxa
	make -C Format DefaultFormat.cmxa
Format/DefaultFormat.cma:Format Typography/_build/Typography.cma
	make -C Format DefaultFormat.cma

Format/DefaultFormat.p.cmxa:Format Typography/_build/Typography.p.cmxa
	make -C Format DefaultFormat.p.cmxa

Format/%.cmxa:Format/DefaultFormat.cmxa Typography/_build/Typography.cmxa Format/%.ml
	make -C Format $(shell basename $@)
Format/%.p.cmxa:Format/DefaultFormat.p.cmxa Typography/_build/Typography.p.cmxa Format/%.ml
	make -C Format $(shell basename $@)
Format/%.cma:Format/DefaultFormat.cma Typography/_build/Typography.cma Format/%.ml
	make -C Format $(shell basename $@)

.PHONY:Typography/_build/Typography.cma Typography/_build/Typography.cmxa Typography/_build/Typography.p.cmxa
Typography/_build/Typography.cma:Rbuffer/rbuffer.cma
	make -C Typography _build/Typography.cma
Typography/_build/Typography.cmxa:Rbuffer/rbuffer.cmxa
	make -C Typography _build/Typography.cmxa
Typography/_build/Typography.p.cmxa:Rbuffer/rbuffer.p.cmxa
	make -C Typography _build/Typography.p.cmxa

Typography/_build/Typography.cmi:Typography/_build/Typography.cma

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
Drivers/%.p.cmxa:Typography/_build/Typography.p.cmxa Drivers
	make -C Drivers $(shell basename $@)

#Drivers/Typography.cmi:Typography/_build/Typography.cmi
#	cp Typography/_build/Typography.cmi Drivers

DefaultGrammar.ttml:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --no-grammar --ml -c DefaultGrammar.txp
DefaultGrammar.tml:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --no-grammar --ml DefaultGrammar.txp

DefaultGrammar.cmx:Format/DefaultFormat.cmxa Drivers/Pdf.cmxa DefaultGrammar.ttml
	$(CAMLOPT) -c -o DefaultGrammar.cmx -I Rbuffer -I Typography/_build -I Format -I Drivers DefaultFormat.cmxa Typography.cmxa Pdf.cmxa -impl DefaultGrammar.ttml

DefaultGrammar.pdf:DefaultGrammar.tml Drivers/Pdf.cmxa
	$(CAMLOPT) -o DefaultGrammar.tmx -I Rbuffer -I Typography/_build -I Format -I Drivers -I Rbuffer rbuffer.cmxa Typography.cmxa DefaultFormat.cmxa Pdf.cmxa -linkpkg -impl DefaultGrammar.tml
	./DefaultGrammar.tmx

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
	cd Drivers; rm -f *~ \#*\# *.cm[xoia] *.cmxa *.o *.a
	cd proof; rm -f *~ \#*\# *.cm[xoi] *.o
	rm -f *~ \#*\# *.cm[xoia] *.cmxa *.tgo *.tgx *.tml *.o *.a
