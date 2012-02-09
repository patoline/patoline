BASE=Bezier.ml new_map.mli new_map.ml Constants.ml Binary.ml

FONTS0=Fonts/FTypes.mli Fonts/FTypes.ml Fonts/CFF.ml Fonts/Opentype.ml
FONTS=$(FONTS0) Fonts.mli Fonts.ml
SOURCES0 = $(BASE) $(FONTS) Drivers.mli Drivers.ml Hyphenate.ml Util.mli Util.ml Badness.mli Badness.ml Typeset.mli Typeset.ml Output.ml Parameters.ml Typography.ml
SOURCES=$(SOURCES0) Section.ml Parser.dyp Texprime.ml

DOC=Bezier.mli Drivers.mli Fonts/FTypes.ml Fonts.mli Hyphenate.mli Util.mli Typeset.mli Output.ml Typography.ml

EXEC = texprime

LIBS=fonts.cma

########################## Advanced user's variables #####################
#
# The Caml compilers.
# You may fix here the path to access the Caml compiler on your machine
# You may also have to add various -I options.


CAMLC = ocamlfind ocamlc -package camomile -package dyp -linkpkg -I Fonts -pp "cpp -w" graphics.cma
CAMLMKTOP = ocamlfind ocamlmktop -package camomile -package dyp -linkpkg -I Fonts -pp "cpp -w"
CAMLDOC = ocamlfind ocamldoc -package camomile -package dyp -html -I Fonts -pp "cpp -w"
CAMLOPT = ocamlfind ocamlopt -package camomile -package dyp -linkpkg -I Fonts -pp "cpp -w"
CAMLDEP = ocamlfind ocamldep -pp "cpp -w"



################ End of user's variables #####################


##############################################################
################ This part should be generic
################ Nothing to set up or fix here
##############################################################

all : $(EXEC)

opt : $(EXEC).opt

#ocamlc -custom other options graphics.cma other files -cclib -lgraphics -cclib -lX11
#ocamlc -thread -custom other options threads.cma other files -cclib -lthreads
#ocamlc -custom other options str.cma other files -cclib -lstr
#ocamlc -custom other options nums.cma other files -cclib -lnums
#ocamlc -custom other options unix.cma other files -cclib -lunix
#ocamlc -custom other options dbm.cma other files -cclib -lmldbm -cclib -lndbm

SMLIY = $(SOURCES:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLDYP = $(SMLIYL:.dyp=.ml)
SMLYL = $(filter %.ml,$(SMLDYP))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

TEST = $(filter %.ml, $(SOURCES0))
TESTOBJ = $(TEST:.ml=.cmx)

$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) $(CUSTOM) -o $(EXEC).opt $(OPTOBJS)
	cp $(EXEC).opt $(EXEC)

typography.cma: $(TESTOBJ) Typography.cmo
	$(CAMLC) -a -o typography.cma $(TESTOBJ) Typography.cmo

test: $(TESTOBJ) Typography.cmx tests/document.ml
	$(CAMLOPT) -o test $(TESTOBJ) tests/document.ml

fonts.cma: $(FONTS:.ml=.cmo) $(BASE:.ml=.cmo)
	$(CAMLC) -a -o fonts.cma $(BASE:.ml=.cmo) $(FONTS0:.ml=.cmo) Fonts.cmo

fonts.cmxa: $(FONTS) $(BASE) $(OPTOBJS)
	$(CAMLOPT) -a -o fonts.cmxa $(BASE:.ml=.cmx) $(FONTS0) Fonts.ml

graphics_font: $(FONTS:.ml=.cmo) $(BASE:.ml=.cmo) tests/graphics_font.ml
	$(CAMLC) -o graphics_font $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) tests/graphics_font.ml

graphics.opt: tests/graphics_font.ml $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx)
	$(CAMLOPT) graphics.cmxa -o graphics.opt $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx) tests/graphics_font.ml

collisions: tests/collisions.ml $(OBJS)
	$(CAMLC) $(OBJS) graphics.cma -o collisions tests/collisions.ml

pdf_test: tests/pdf.ml $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) Drivers.cmo
	$(CAMLC) -o pdf_test $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) Drivers.cmo tests/pdf.ml

kerner: kerner.ml $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo)
	$(CAMLC) $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) -o kerner kerner.ml


doc:Makefile $(SOURCES0:.ml=.cmo)
	mkdir -p doc
	$(CAMLDOC) -d doc $(DOC)

top:
	 ocamlfind ocamlmktop -package camomile -pp cpp -o ftop -linkpkg -I Fonts Binary.ml Bezier.ml Fonts/FontsTypes.ml Fonts/FontCFF.ml Fonts/FontOpentype.ml Fonts.ml

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .dyp

.dyp.ml:
	dypgen --no-mli $<

.ml.cmo:
	$(CAMLC) -c -o $@ $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c -o $@ $<

clean:
	rm -f *.cm[iox] *.o *~ \#*\#
	rm -f Fonts/*.cm[iox] Fonts/*~ Fonts/*.*~ Fonts/\#*\#
	rm -Rf doc
	rm -f graphics_font
	rm -f *.o

.depend.input: Makefile
	@echo -n '--Checking Ocaml input files: '
	@(ls $(SMLIY) $(SMLIY:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (echo 'unchanged'; rm -f .depend.new) || \
	    (echo 'changed'; mv .depend.new .depend.input)

depend : .depend

.depend : $(SMLIY) .depend.input
	@echo '--Re-building dependencies'
	$(CAMLDEP) $(SMLIY) $(SMLIY:.ml=.mli) > .depend

include .depend
