BASE=Bezier.ml new_map.mli new_map.ml Constants.ml Binary.ml

FONTS0=Fonts/FTypes.mli Fonts/FTypes.ml Fonts/CFF.ml Fonts/Opentype.ml
FONTS=$(FONTS0) Fonts.ml
SOURCES = $(BASE) $(FONTS) Drivers.ml Hyphenate.ml Util.mli Util.ml Badness.ml Boxes.mli Boxes.ml Output.ml Section.ml Parser.dyp Texprime.ml

DOC=Drivers.mli Fonts/FTypes.ml Fonts.ml Hyphenate.mli Util.mli Boxes.mli Output.ml

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

$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) $(CUSTOM) -o $(EXEC) $(OPTOBJS)

fonts.cma: $(FONTS0:.ml=.cmo) $(BASE:.ml=.cmo)
	$(CAMLC) -a -o fonts.cma $(BASE:.ml=.cmo) $(FONTS0.ml=.cmo) Fonts.ml

fonts.cmxa: $(FONTS) $(BASE) $(OPTOBJS)
	$(CAMLOPT) -a -o fonts.cmxa $(BASE:.ml=.cmx) $(FONTS0) Fonts.ml

graphics_font: tests/graphics_font.ml fonts.cma
	$(CAMLC) fonts.cma graphics.cma -o graphics_font tests/graphics_font.ml

graphics.opt: tests/graphics_font.ml $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx)
	$(CAMLOPT) graphics.cmxa -o graphics.opt $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx) tests/graphics_font.ml

collisions: tests/collisions.ml $(OBJS)
	$(CAMLC) $(OBJS) graphics.cma -o collisions tests/collisions.ml

pdf_test: tests/pdf.ml $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) Drivers2.cmo
	$(CAMLC) -o pdf_test $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) Drivers2.cmo tests/pdf.ml

kerner: kerner.ml $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo)
	$(CAMLC) $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) -o kerner kerner.ml

test:test.ml $(BASE:.ml=.cmo) fonts.cma
	$(CAMLC) -o test $(BASE:.ml=.cmo) fonts.cma test.ml

doc:Makefile $(OBJS)
	mkdir -p doc
	$(CAMLDOC) -d doc $(DOC)

#Fonts.mli: Fonts.ml
#	$(CAMLC) $(CUSTOM) -i Fonts.ml | sed -e "s/->[ \t]*FTypes./-> Types./g" > Fonts.mli

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
