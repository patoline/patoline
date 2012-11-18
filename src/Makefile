include Makefile.config

FORMAT=$(shell ls Format/Format*.ml) Format/LMFormat.ml


ifdef PACK_DRIVER_GL
NATIVE_EXTRA+=Patoline/PatolineGL
endif

ifdef PACK_DRIVER_GL2
NATIVE_EXTRA+=Patoline/PatolineGL2
endif

CAMLOPT=ocamlfind ocamlopt $(PACK)
CAMLC=ocamlfind ocamlc $(PACK)

VPATH=Typography:Pdf:Drivers:Format

all:native byte prof

native:Typography/Typography.cmxa $(FORMAT:.ml=.cmxa) $(DRIVERS:.ml=.cmxa) Patoline/patoline DefaultGrammar.cmx ocaml-bibi/bibi.cmxa proof/proof Pdf/pdf_parser.cmxa $(NATIVE_EXTRA)
byte:Typography/Typography.cma ocaml-bibi/bibi.cma $(FORMAT:.ml=.cma) $(DRIVERS:.ml=.cma) Pdf/pdf_parser.cma

prof:Typography/Typography.p.cmxa ocaml-bibi/bibi.p.cmxa $(FORMAT:.mllib=.p.cmxa) Pdf/pdf_parser.p.cmxa

Patoline/patoline:Patoline
	make -C Patoline patoline
Patoline/PatolineGL:Patoline GL.cmxa
	make -C Patoline PatolineGL
Patoline/PatolineGL2:Patoline GL2.cmxa
	make -C Patoline PatolineGL2


ocaml-bibi/bibi.cma:Typography/Typography.cma ocaml-bibi ocaml-bibi/Typography.cmi
	make -C ocaml-bibi bibi.cma
ocaml-bibi/bibi.cmxa:Typography/Typography.cmxa ocaml-bibi ocaml-bibi/Typography.cmi
	make -C ocaml-bibi bibi.cmxa
ocaml-bibi/bibi.p.cmxa:Typography/Typography.p.cmxa ocaml-bibi ocaml-bibi/Typography.cmi
	make -C ocaml-bibi bibi.p.cmxa
ocaml-bibi/Typography.cmi:Typography/_build/Typography.cmi
	cp Typography/_build/Typography.cmi ocaml-bibi


proof/proof:proof/Typography.cmi Drivers/Pdf.cmxa proof/proof.ml Rbuffer/rbuffer.cmxa
	cd proof; $(CAMLOPT) -I ../Typography/_build -I ../Drivers -I ../Rbuffer -linkpkg -o proof rbuffer.cmxa ../Typography/_build/Typography.cmxa ../Drivers/Pdf.cmxa proof.ml

proof/Typography.cmi:Typography/_build/Typography.cmi
	cp Typography/_build/Typography.cmi proof

Pdf/pdf_parser.cmxa:Typography/Typography.cmxa Pdf/Typography.cmi
	make -C Pdf pdf_parser.cmxa
Pdf/pdf_parser.cma:Typography/Typography.cma Pdf/Typography.cmi
	make -C Pdf pdf_parser.cma
Pdf/pdf_parser.p.cmxa:Typography/Typography.p.cmxa Pdf/Typography.cmi
	make -C Pdf pdf_parser.p.cmxa
Pdf/Typography.cmi:Typography/_build/Typography.cmi
	cp Typography/_build/Typography.cmi Pdf


Format/DefaultFormat.cmxa:Format Typography.cmxa
	cp Typography/_build/Typography.cmi Format
	make -C Format DefaultFormat.cmxa
Format/DefaultFormat.cma:Format Typography.cma
	cp Typography/_build/Typography.cmi Format
	make -C Format DefaultFormat.cma
Format/DefaultFormat.p.cmax:Format Typography.p.cmxa
	cp Typography/_build/Typography.cmi Format
	make -C Format DefaultFormat.p.cmxa
Format/Typography.cmi:Typography/_build/Typography.cmi
	cp Typography/_build/Typography.cmi Formats

Format/%.cmxa:DefaultFormat.cmxa Typography.cmxa Format/%.ml Format/Typography.cmi
	make -C Format $(shell basename $@)
Format/%.p.cmxa:DefaultFormat.cmxa Typography.p.cmxa Format/%.ml Format/Typography.cmi
	make -C Format $(shell basename $@)
Format/%.cma:DefaultFormat.cma Typography.cma Format/%.ml Format/Typography.cmi
	make -C Format $(shell basename $@)


Typography/Typography.cma:Typography Typography/rbuffer.cmi
	make -C Typography _build/Typography.cma
Typography/Typography.cmxa:Typography Typography/rbuffer.cmi
	make -C Typography _build/Typography.cmxa
Typography/Typography.p.cmxa:Typography Typography/rbuffer.cmi
	make -C Typography _build/Typography.p.cmxa

Typography/rbuffer.cmi:Rbuffer/rbuffer.cma
	cp Rbuffer/rbuffer.cmi Typography

Typography/Typography.cmi:Typography.cmxa

Rbuffer/rbuffer.cma:Rbuffer
	make -C Rbuffer rbuffer.cma
Rbuffer/rbuffer.cmxa:Rbuffer
	make -C Rbuffer rbuffer.cmxa
Rbuffer/rbuffer.p.cmxa:Rbuffer
	make -C Rbuffer rbuffer.p.cmxa

Drivers/%.cmxa:Drivers Typography/Typography.cmxa Drivers/Typography.cmi
	make -C Drivers $(shell basename $@)
Drivers/%.cma:Drivers Typography/Typography.cma Drivers/Typography.cmi
	make -C Drivers $(shell basename $@)
Drivers/%.p.cmxa:Drivers Typography/Typography.p.cmxa Drivers/Typography.cmi
	make -C Drivers $(shell basename $@)

Drivers/Typography.cmi:Typography/_build/Typography.cmi
	cp Typography/_build/Typography.cmi Drivers

DefaultGrammar.ttml:DefaultGrammar.txp Patoline/patoline
	Patoline/patoline --no-grammar --ml -c DefaultGrammar.txp

DefaultGrammar.cmx:Format/_build/DefaultFormat.cmxa Drivers/Pdf.cmxa DefaultGrammar.ttml
	$(CAMLOPT) -c -o DefaultGrammar.cmx -I Rbuffer -I Typography/_build -I Format/_build -I Drivers/_build DefaultFormat.cmxa Typography.cmxa Pdf.cmxa -impl DefaultGrammar.ttml

.PHONY :doc
doc:
	mkdir -p doc
	ocamlfind ocamldoc -package camomile -html -d doc -pp cpp -I Typography -I Typography/Output Typography/Document.ml

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
