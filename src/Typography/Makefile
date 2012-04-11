BASE=Bezier.mli Bezier.ml new_map.mli new_map.ml Binary.ml

FONTS0=Fonts/FTypes.mli Fonts/FTypes.ml Fonts/CFF.ml Fonts/Opentype.ml
FONTS=$(FONTS0) Fonts.mli Fonts.ml

DRIVERS=Output/OutputPaper.mli Output/Drivers/Pdf.mli Output/OutputPaper.ml Output/Drivers/Pdf.ml
FORMATS=Format/TableOfContents.ml Format/Sections.ml DefaultFormat.ml

SOURCES0 = Config.ml Constants.ml
SOURCES2 = $(BASE) $(FONTS) Hyphenate.ml Output/OutputCommon.ml Output/OutputCommon.mli Util.mli Util.ml Badness.mli Badness.ml Log.ml Break.mli Break.ml Parameters.ml Output/OutputDrawing.ml Document.ml $(DRIVERS) Diag.ml Maths.ml

SOURCES_ALL=$(SOURCES0) $(SOURCES2)


LIBS_ML=$(filter %.ml, $(SOURCES_LIBS))

########## Compilers ##########

OPTS= -I Fonts -I Output -I Output/Drivers
CAMLC = ocamlfind ocamlc -package camomile,dyp -linkpkg $(OPTS) -pp "cpp -w" -g
CAMLCA = ocamlfind ocamlc -package camomile,dyp $(OPTS) -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package bigarray,camomile,dyp -linkpkg $(OPTS) -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package bigarray,camomile,dyp $(OPTS) -pp "cpp -w"

CAMLDEP = ocamlfind ocamldep -pp "cpp -w"


###############################

all : Typography.cma Typography.cmxa

Typography.cma: $(filter %.cmo, $(SOURCES_ALL:.ml=.cmo))
	$(CAMLCA) -pack -o Typography.cmo $(filter %.cmo, $(SOURCES_ALL:.ml=.cmo))
	$(CAMLCA) -a -o Typography.cma Typography.cmo

Typography.cmxa: $(filter %.cmx, $(SOURCES_ALL:.ml=.cmx))
	$(CAMLOPTA) -pack -o Typography.cmx $(filter %.cmx, $(SOURCES_ALL:.ml=.cmx))
	$(CAMLOPTA) -a -o Typography.cmxa Typography.cmx

%.cmo: %.ml
	$(CAMLC) -c -for-pack Typography $<

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmx: %.ml
	$(CAMLOPT) -c -for-pack Typography $<

clean:
	rm -f *.cm[ioxa] *.cmxa *.o *~ \#*\# DefaultFormat.mli
	make -C Fonts clean
	make -C Output clean
	rm -f *.tml *.tmx *.tgo *.tgx
	rm -Rf doc
	rm .depend*

.depend.input: Makefile
	@echo -n '--Checking Ocaml input files: '
	@(ls $(SOURCES_ALL)  $(SOURCES_ALL:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (echo 'unchanged'; rm -f .depend.new) || \
	    (echo 'changed'; mv .depend.new .depend.input)

depend : .depend

.depend: $(SOURCES_ALL) .depend.input
	@echo '--Re-building dependencies'
	$(CAMLDEP)  $(SOURCES_ALL) > .depend


include .depend
