BASE=Bezier.ml new_map.mli new_map.ml Config.ml Constants.ml Binary.ml

FONTS0=Fonts/FTypes.mli Fonts/FTypes.ml Fonts/CFF.ml Fonts/Opentype.ml
FONTS=$(FONTS0) Fonts.mli Fonts.ml

DRIVERS=Output/OutputPaper.ml Output/Drivers/Pdf.ml

SOURCES0 = $(BASE) $(FONTS) Output/OutputCommon.ml
SOURCES1 = Parser.ml Texprime.ml
SOURCES2 = Hyphenate.ml Util.mli Util.ml Badness.mli Badness.ml Typeset.mli Typeset.ml Parameters.ml Output/OutputDrawing.mli Output/OutputDrawing.ml Typography.ml $(DRIVERS) Diag.ml Maths.ml DefaultFormat.ml
SOURCES_EXEC=$(SOURCES0) $(SOURCES1)
SOURCES_LIBS=$(SOURCES0) $(SOURCES2)
SOURCES_ALL=$(SOURCES0) $(SOURCES1) $(SOURCES2)
DOC=Bezier.mli Output/OutputCommon.ml Fonts/FTypes.ml Fonts.mli Hyphenate.mli Util.mli Typeset.mli Typography.ml

EXEC = texprime

LIBS=fonts.cma texprime.cma

########## Targets ##########

OBJS = $(filter %.cmo, $(SOURCES_EXEC:.ml=.cmo))
OPTOBJS = $(OBJS:.cmo=.cmx)

TEST = $(filter %.ml, $(SOURCES_LIBS))
TESTOBJS = $(TEST:.ml=.cmo)
TESTOPTOBJS = $(TEST:.ml=.cmx)

LIBS_ML=$(filter %.ml, $(SOURCES_LIBS))

########## Compilers ##########

OPTS= -I Fonts -I Output -I Output/Drivers -g
CAMLC = ocamlfind ocamlc -package camomile,dyp -linkpkg $(OPTS) -pp "cpp -w" str.cma
CAMLOPT = ocamlfind ocamlopt -package bigarray,camomile,dyp -linkpkg $(OPTS) -pp "cpp -w" str.cmxa
CAMLOPTA = ocamlfind ocamlopt -package bigarray,camomile,dyp $(OPTS) -pp "cpp -w"

CAMLMKTOP = ocamlfind ocamlmktop -package camomile -package dyp -linkpkg $(OPTS) -pp "cpp -w"
CAMLDOC = ocamlfind ocamldoc -package camomile -package dyp -html -I Fonts $(OPTS) -pp "cpp -w"
CAMLDEP = ocamlfind ocamldep -pp "cpp -w"


###############################

all : $(EXEC).opt texprime.cma texprime.cmxa texprimeDefault.tgo texprimeDefault.tgx

opt : $(EXEC).opt $(LIBS:.cma=.cmxa)


$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) dynlink.cmxa $(CUSTOM) -o $(EXEC).opt $(OPTOBJS)
	cp $(EXEC).opt $(EXEC)

$(EXEC): $(OBJS)
	$(CAMLC) dynlink.cma $(CUSTOM) -o $(EXEC) $(OBJS)

proof: $(TESTOBJS) tests/proof.ml
	$(CAMLC) -o proof $(TESTOBJS) tests/proof.ml

fonts.cma: $(filter %.cmo, $(FONTS0:.ml=.cmo)) $(filter %.cmo, $(BASE:.ml=.cmo)) Fonts.cmi Fonts.ml
	$(CAMLC) -a -o fonts.cma $(filter %.cmo, $(BASE:.ml=.cmo)) $(filter %.cmo, $(FONTS0:.ml=.cmo)) Fonts.ml

fonts.cmxa: $(filter %.cmx, $(FONTS0:.ml=.cmx)) $(filter %.cmx, $(BASE:.ml=.cmx))
	$(CAMLOPTA) -a -o fonts.cmxa $(filter %.cmx, $(BASE:.ml=.cmx)) $(filter %.cmx, $(FONTS0:.ml=.cmx)) Fonts.cmx


texprime.cma: $(filter %.cmo, $(LIBS_ML:.ml=.cmo))
	$(CAMLC) -a -o texprime.cma $(filter %.cmo, $(LIBS_ML:.ml=.cmo))

texprime.cmxa: $(filter %.cmx, $(LIBS_ML:.ml=.cmx))
	$(CAMLOPTA) -a -o texprime.cmxa $(filter %.cmx, $(LIBS_ML:.ml=.cmx))


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

DefaultGrammar.pdf: texprime texprime.cma DefaultGrammar.txp
	./texprime --extra-fonts-dir ../Otf DefaultGrammar.txp > DefaultGrammar.tml
	$(CAMLC)  -o DefaultGrammar.tmx texprime.cma -impl DefaultGrammar.tml
	./DefaultGrammar.tmx --extra-fonts-dir ../Otf

DefaultGrammar.opt.pdf: texprime.opt texprime.cmxa DefaultGrammar.txp
	./texprime.opt --extra-fonts-dir ../Otf DefaultGrammar.txp > DefaultGrammar.tml
	$(CAMLOPT)  -o DefaultGrammar.tmx texprime.cmxa -impl DefaultGrammar.tml
	./DefaultGrammar.tmx --extra-fonts-dir ../Otf
	cp DefaultGrammar.pdf DefaultGrammar.opt.pdf

%.pdf: texprime texprime.cma texprimeDefault.tgo %.txp
	./texprime --extra-fonts-dir ../Otf $*.txp > $*.tml
	$(CAMLC)  -o $*.tmx texprime.cma -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf

%.opt.pdf: texprime.opt texprime.cmxa texprimeDefault.tgx %.txp
	./texprime.opt --extra-fonts-dir ../Otf $*.txp > $*.tml
	$(CAMLOPT)  -o $*.tmx texprime.cmxa -impl $*.tml
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
	rm -f Fonts/*.cm[ioxa] Fonts/*.cmxa Fonts/*.o Fonts/*~ Fonts/*.*~ Fonts/\#*\#
	rm -f Output/*.cm[ioxa] Output/*.cmxa Output/*.o Output/*~ Output/*.*~ Output/\#*\#
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

.depend: $(SOURCES_ALL) .depend.input Parser.ml
	@echo '--Re-building dependencies'
	$(CAMLDEP)  $(SOURCES_ALL)  $(SOURCES_ALL:.ml=.mli) > .depend


include .depend
