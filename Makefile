BASE=Bezier.ml new_map.mli new_map.ml Constants.ml Binary.ml

FONTS0=Fonts/FTypes.mli Fonts/FTypes.ml Fonts/CFF.ml Fonts/Opentype.ml
FONTS=$(FONTS0) Fonts.mli Fonts.ml

DRIVERS=Output/OutputPaper.ml Output/Drivers/Pdf.ml

SOURCES0 = $(BASE) $(FONTS) Output/OutputCommon.ml Hyphenate.ml Util.mli Util.ml Badness.mli Badness.ml Typeset.mli Typeset.ml Parameters.ml Output/OutputDrawing.ml Typography.ml $(DRIVERS) Diag.ml Maths.ml
SOURCES1 = Parser.ml Texprime.ml
SOURCES2 = DefaultFormat.ml
SOURCES_EXEC=$(SOURCES0) $(SOURCES1)
SOURCES_LIBS=$(SOURCES0) $(SOURCES2)
SOURCES_ALL=$(SOURCES0) $(SOURCES1) $(SOURCES2)
DOC=Bezier.mli Output/OutputCommon.ml Fonts/FTypes.ml Fonts.mli Hyphenate.mli Util.mli Typeset.mli Typography.ml

EXEC = texprime

LIBS=fonts.cma texprime.cma

########################## Advanced user's variables #####################
#
# The Caml compilers.
# You may fix here the path to access the Caml compiler on your machine
# You may also have to add various -I options.


INCL= -I Fonts -I Output -I Output/Drivers
CAMLC = ocamlfind ocamlc -package camomile,dyp -linkpkg $(INCL) -pp "cpp -w" -g # graphics.cma
CAMLMKTOP = ocamlfind ocamlmktop -package camomile -package dyp -linkpkg $(INCL) -pp "cpp -w"
CAMLDOC = ocamlfind ocamldoc -package camomile -package dyp -html -I Fonts $(INCL) -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package bigarray,camomile,dyp $(INCL) -g -pp "cpp -w"
CAMLOPT = ocamlfind ocamlopt -package bigarray,camomile,dyp -linkpkg $(INCL) -g -pp "cpp -w"
CAMLDEP = ocamlfind ocamldep -pp "cpp -w"



################ End of user's variables #####################


##############################################################
################ This part should be generic
################ Nothing to set up or fix here
##############################################################

all : $(EXEC) $(LIBS) Doc.pdf

opt : $(EXEC).opt $(LIBS:.cma=.cmxa) Doc.opt.pdf

#ocamlc -custom other options graphics.cma other files -cclib -lgraphics -cclib -lX11
#ocamlc -thread -custom other options threads.cma other files -cclib -lthreads
#ocamlc -custom other options str.cma other files -cclib -lstr
#ocamlc -custom other options nums.cma other files -cclib -lnums
#ocamlc -custom other options unix.cma other files -cclib -lunix
#ocamlc -custom other options dbm.cma other files -cclib -lmldbm -cclib -lndbm


OBJS = $(SOURCES_EXEC:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

TEST = $(filter %.ml, $(SOURCES_LIBS))
TESTOBJS = $(TEST:.ml=.cmo)
TESTOPTOBJS = $(TEST:.ml=.cmx)

LIBS_ML=$(filter %.ml, $(SOURCES_LIBS))

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) dynlink.cmxa $(CUSTOM) -o $(EXEC).opt $(OPTOBJS)
	cp $(EXEC).opt $(EXEC)

$(EXEC): $(OBJS)
	$(CAMLC) dynlink.cma $(CUSTOM) -o $(EXEC) $(OBJS)


typography.cma: $(TESTOBJS)
	$(CAMLC) -a -o typography.cma $(TESTOBJS) Typography.cmo

maths: $(TESTOTPOBJS) tests/test_maths.ml
	$(CAMLOPT) -o maths $(TESTOPTOBJS) tests/test_maths.ml

proof: $(TESTOBJOBJS:.cmx=.cmo) tests/proof.ml
	$(CAMLC) -o proof $(TESTOBJ:.cmx=.cmo) tests/proof.ml


test: $(TESTOBJS) tests/document.ml
	$(CAMLC) -o test $(TESTOBJS) tests/document.ml
test2: $(TESTOPTOBJS) tests/document2.ml
	$(CAMLOPT) -o test2 $(TESTOPTOBJS) tests/document2.ml

ot: $(filter %.cmo, $(FONTS:.ml=.cmo)) $(filter %.cmo, $(BASE:.ml=.cmo)) tests/test_opentype.ml
	$(CAMLC) -o ot $(filter %.cmo, $(BASE:.ml=.cmo)) $(filter %.cmo, $(FONTS:.ml=.cmo)) tests/test_opentype.ml

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

collisions: tests/collisions.ml $(TESTOBJS)
	$(CAMLC) $(TESTOBJS) graphics.cma -o collisions tests/collisions.ml

pdf_test: tests/pdf.ml $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) Drivers.cmo
	$(CAMLC) -o pdf_test $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) Drivers.cmo tests/pdf.ml

kerner: kerner.ml $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo)
	$(CAMLC) $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) -o kerner kerner.ml


doc:Makefile $(SOURCES0:.ml=.cmo)
	mkdir -p doc_html
	$(CAMLDOC) -d doc_html $(DOC)

texprimeDefault.tgo: DefaultGrammar.pdf
	true

texprimeDefault.tgx: DefaultGrammar.opt.pdf
	true

%.pdf: texprime texprime.cma texprimeDefault.tgo %.txp
	./texprime $*.txp > $*.tml
	$(CAMLC)  -o $*.tmx texprime.cma -impl $*.tml
	./$*.tmx

%.opt.pdf: texprime.opt texprime.cmxa texprimeDefault.tgx %.txp
	./texprime.opt $*.txp > $*.tml
	$(CAMLOPT)  -o $*.tmx texprime.cmxa -impl $*.tml
	./$*.tmx

top:
	 ocamlfind ocamlmktop -package camomile -pp cpp -o ftop -linkpkg -I Fonts Binary.ml Bezier.ml Fonts/FontsTypes.ml Fonts/FontCFF.ml Fonts/FontOpentype.ml Fonts.ml

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
	rm -Rf doc

.depend.input: Makefile Parser.ml
	@echo -n '--Checking Ocaml input files: '
	@(ls $(SOURCES_ALL)  $(SOURCES_ALL:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (echo 'unchanged'; rm -f .depend.new) || \
	    (echo 'changed'; mv .depend.new .depend.input)

depend : .depend

.depend: $(SMLIY) .depend.input Parser.ml
	@echo '--Re-building dependencies'
	$(CAMLDEP)  $(SOURCES_ALL)  $(SOURCES_ALL:.ml=.mli) > .depend


include .depend
