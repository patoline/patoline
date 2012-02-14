BASE=Bezier.ml new_map.ml new_map.mli Constants.ml Binary.ml

FONTS0=Fonts/FTypes.mli Fonts/FTypes.ml Fonts/CFF.ml Fonts/Opentype.ml
FONTS=$(FONTS0) Fonts.mli  Fonts.ml
SOURCES = $(BASE) $(FONTS) Drivers.mli Drivers.ml Hyphenate.ml Util.ml Badness.ml Boxes.mli Boxes.ml Output.ml Section.ml Parser.dyp Texprime.ml

DOC=Drivers.mli Fonts.ml Hyphenate.mli Util.mli Boxes.mli Output.ml

EXEC = texprime

LIBS=fonts.cma

########################## Advanced user's variables #####################
#
# The Caml compilers.
# You may fix here the path to access the Caml compiler on your machine
# You may also have to add various -I options.


CAMLC = ocamlfind ocamlc -package camomile -package dyp -linkpkg -I Fonts -pp "cpp -w"
CAMLMKTOP = ocamlfind ocamlmktop -package camomile -package dyp -linkpkg -I Fonts -pp "cpp -w"
CAMLDOC = ocamlfind ocamldoc -package camomile -package dyp -html -I Fonts -pp "cpp -w"
CAMLOPT = ocamlfind ocamlopt -package camomile -package dyp -linkpkg -I Fonts -pp "cpp -w"
CAMLDEP = ocamlfind ocamldep -pp "cpp -w"



################ End of user's variables #####################


##############################################################
################ This part should be generic
################ Nothing to set up or fix here
##############################################################

all:: .depend.input .depend $(EXEC)

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

FONTS_MLI=$(filter %.mli, $(FONTS))
FONTS_CMI=$(FONTS_MLI:.mli=.cmi)
FONTS_OBJS=Bezier.cmo new_map.cmo Binary.cmo $(filter %.cmo, $(FONTS:.ml=.cmo))
FONTS_OPTOBJS=$(FONTS_OBJS:.cmo=.cmx)

$(EXEC): $(OBJS)
	$(CAMLC) $(CUSTOM) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) $(CUSTOM) -o $(EXEC) $(OPTOBJS)

fonts.top: $(FONTS_CMI) $(FONTS_OBJS)
	$(CAMLMKTOP) $(CUSTOM) -o $@ $(FONTS_OBJS)

fonts.cma: $(FONTS_CMI) $(FONTS_OBJS)
	$(CAMLC) $(CUSTOM) -a -o $@ $(FONTS_OBJS)

graphics_font: tests/graphics_font.ml $(OBJS)
	$(CAMLC) $(OBJS) graphics.cma -o graphics_font tests/graphics_font.ml

collisions: tests/collisions.ml $(OBJS)
	$(CAMLC) $(OBJS) graphics.cma -o collisions tests/collisions.ml

test:$(OBJS) test.ml
	$(CAMLC) $(FONTS_OBJS) -o $@ $(OBJS) test.ml

doc:Makefile $(OBJS)
	mkdir -p doc
	$(CAMLDOC) -d doc $(DOC)

Fonts.mli: $(BASE:.ml=.cmo) $(FONTS0:.ml=.cmo)
	$(CAMLC) $(CUSTOM) -i Fonts.ml | sed -e "s/->[ \t]*FTypes./-> Types./g" > Fonts.mli

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly .dyp

.dyp.ml:
	dypgen --no-mli $<

.ml.cmo:
	$(CAMLC) -c -o $@ $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c -o $@ $<

clean::
	rm -f *.cm[iox] *~ .*~ \#*\#
	rm -f Fonts/*.cm[iox] Fonts/*~ Fonts/*.*~ Fonts/\#*\#
	rm -f $(EXEC)
	rm -f $(EXEC).opt
	rm -Rf doc
	rm -f graphics_font
	rm -f Fonts.mli

.depend.input: Makefile
	@echo -n '--Checking Ocaml input files: '
	@(ls $(SMLIY) $(SMLIY:.ml=.mli) 2>/dev/null || true) \
	     >  .depend.new
	@diff .depend.new .depend.input 2>/dev/null 1>/dev/null && \
	    (echo 'unchanged'; rm -f .depend.new) || \
	    (echo 'changed'; mv .depend.new .depend.input)

depend: .depend

.depend:: $(SMLIY) .depend.input
	@echo '--Re-building dependencies'
	$(CAMLDEP) $(SMLIY) $(SMLIY:.ml=.mli) > .depend

include .depend
