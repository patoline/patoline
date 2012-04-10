BEZIER=Bezier.mli Bezier.ml new_map.mli new_map.ml Binary.ml

FONTS0=Fonts/FTypes.mli Fonts/FTypes.ml Fonts/CFF.ml Fonts/Opentype.ml
FONTS=$(FONTS0) Fonts.mli Fonts.ml

DRIVERS=Output/OutputPaper.mli Output/Drivers/Pdf.mli Output/OutputPaper.ml Output/Drivers/Pdf.ml
FORMATS=Format/TableOfContents.ml Format/Sections.ml

SOURCES0 = Config.ml Constants.ml
SOURCES1 = Parser.ml Texprime.ml
SOURCES2 = $(BEZIER) $(FONTS) Hyphenate.ml Output/OutputCommon.ml Output/OutputCommon.mli Util.mli Util.ml Badness.mli Badness.ml Break.mli Break.ml Parameters.ml Output/OutputDrawing.mli Output/OutputDrawing.ml Document.ml $(DRIVERS) $(FORMATS) Diag.ml Maths.ml DefaultFormat.ml Typography.ml
SOURCES_EXEC=$(SOURCES0) $(SOURCES1)
SOURCES_LIBS=$(SOURCES0) $(SOURCES2)
SOURCES_ALL=$(SOURCES0) $(SOURCES1) $(SOURCES2)
DOC=Bezier.mli Output/OutputCommon.ml Fonts/FTypes.ml Fonts.mli Hyphenate.mli Util.mli Typeset.mli Document.ml

EXEC = texprime

LIBS=fonts.cma typography.cma

########## Targets ##########

OBJS = $(filter %.cmo, $(SOURCES_EXEC:.ml=.cmo))
OPTOBJS = $(OBJS:.cmo=.cmx)

TEST = $(filter %.ml, $(SOURCES_LIBS))
TESTOBJS = $(TEST:.ml=.cmo)
TESTOPTOBJS = $(TEST:.ml=.cmx)

LIBS_ML=$(filter %.ml, $(SOURCES_LIBS))

########## Compilers ##########

OPTS= -I Fonts -I Output -I Output/Drivers -I Format
CAMLC = ocamlfind ocamlc -package camomile,dyp -linkpkg $(OPTS) -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package bigarray,camomile,dyp -linkpkg $(OPTS) -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package bigarray,camomile,dyp $(OPTS) -pp "cpp -w"

CAMLMKTOP = ocamlfind ocamlmktop -package camomile -package dyp -linkpkg $(OPTS) -pp "cpp -w"
CAMLDOC = ocamlfind ocamldoc -package camomile -package dyp -html -I Fonts $(OPTS) -pp "cpp -w"
CAMLDEP = ocamlfind ocamldep -pp "cpp -w"


###############################

all : $(EXEC).opt bezier.cma bezier.cmxa fonts.cma fonts.cmxa typography.cma typography.cmxa texprimeDefault.tgo texprimeDefault.tgx

opt : $(EXEC).opt $(LIBS:.cma=.cmxa)


$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) dynlink.cmxa $(CUSTOM) -o $(EXEC).opt $(OPTOBJS)
	cp $(EXEC).opt $(EXEC)

$(EXEC): $(OBJS)
	$(CAMLC) dynlink.cma $(CUSTOM) -o $(EXEC) $(OBJS)

proof: $(TESTOBJS) tests/proof.ml
	$(CAMLC) -o proof $(TESTOBJS) tests/proof.ml

fonts.cma: bezier.cma $(filter %.cmo, $(FONTS:.ml=.cmo))
	$(CAMLC) -a -o fonts.cma $(filter %.cmo, $(FONTS:.ml=.cmo))

fonts.cmxa: bezier.cmxa $(filter %.cmx, $(FONTS:.ml=.cmx))
	$(CAMLOPTA) -a -o fonts.cmxa $(filter %.cmx, $(FONTS:.ml=.cmx))


bezier.cma:$(BEZIER:.ml=.cmo)
	$(CAMLC) -a -o bezier.cma $(BEZIER:.ml=.cmo)

bezier.cmxa:$(BEZIER:.ml=.cmx)
	$(CAMLOPTA) -a -o bezier.cmxa $(BEZIER:.ml=.cmx)


typography.cma: $(filter %.cmo, $(LIBS_ML:.ml=.cmo))
	$(CAMLC) -a -o typography.cma $(filter %.cmo, $(LIBS_ML:.ml=.cmo))

typography.cmxa: $(filter %.cmx, $(LIBS_ML:.ml=.cmx))
	$(CAMLOPTA) -a -o typography.cmxa $(filter %.cmx, $(LIBS_ML:.ml=.cmx))


graphics_font: $(FONTS:.ml=.cmo) $(BASE:.ml=.cmo) tests/graphics_font.ml
	$(CAMLC) -o graphics_font graphics.cma $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) tests/graphics_font.ml

graphics.opt: tests/graphics_font.ml $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx)
	$(CAMLOPT) graphics.cmxa -o graphics.opt $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx) tests/graphics_font.ml


doc:Makefile $(SOURCES0:.ml=.cmo)
	mkdir -p doc_html
	$(CAMLDOC) -d doc_html $(DOC)


texprimeDefault.tgo: DefaultGrammar.pdf
	true

texprimeDefault.tgx: DefaultGrammar.opt.pdf
	true

DefaultGrammar.pdf: texprime typography.cma DefaultGrammar.txp
	./texprime --extra-fonts-dir ../Otf DefaultGrammar.txp > DefaultGrammar.tml
	$(CAMLC)  -o DefaultGrammar.tmx typography.cma -impl DefaultGrammar.tml
	./DefaultGrammar.tmx --extra-fonts-dir ../Otf

DefaultGrammar.opt.pdf: texprime.opt typography.cmxa DefaultGrammar.txp
	./texprime.opt --extra-fonts-dir ../Otf DefaultGrammar.txp > DefaultGrammar.tml
	$(CAMLOPT)  -o DefaultGrammar.tmx typography.cmxa -impl DefaultGrammar.tml
	./DefaultGrammar.tmx --extra-fonts-dir ../Otf
	cp DefaultGrammar.pdf DefaultGrammar.opt.pdf

%.pdf: texprime typography.cma texprimeDefault.tgo %.txp
	./texprime --extra-fonts-dir ../Otf $*.txp > $*.tml
	$(CAMLC)  -o $*.tmx typography.cma -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf

%.opt.pdf: texprime.opt fonts.cmxa typography.cmxa texprimeDefault.tgx %.txp
	./texprime.opt --extra-fonts-dir ../Otf $*.txp > $*.tml
	$(CAMLOPT)  -o $*.tmx typography.cmxa -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf
	cp $*.pdf $*.opt.pdf

%.ml: %.dyp
	dypgen --no-mli $<

%.cmo: %.ml
	$(CAMLC) -c -o $@ $<

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmx: %.ml
	$(CAMLOPT) -c -o $@ $<

clean:
	rm -f *.cm[ioxa] *.cmxa *.o *~ \#*\#
	make -C Fonts clean
	make -C Output clean
	rm -f Parser.ml
	rm -f *.tml *.tmx *.tgo *.tgx
	rm -Rf doc

.depend.input: Makefile Parser.ml
	@echo -n '--Checking Ocaml input files: '
	@(ls $(SOURCES_ALL)  $(SOURCES_ALL:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (echo 'unchanged'; rm -f .depend.new) || \
	    (echo 'changed'; mv .depend.new .depend.input)

depend : .depend

.depend: $(FONTS) $(BEZIER) $(SOURCES_ALL) .depend.input Parser.ml
	@echo '--Re-building dependencies'
	$(CAMLDEP)  $(BEZIER) $(FONTS)  $(SOURCES_ALL) > .depend


include .depend
