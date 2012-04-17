SOURCES1 = Parser.ml Texprime.ml Main.ml
SOURCES3 = Format/TableOfContents.ml Format/Sections.ml OutputDrawing.ml DefaultFormat.ml 
SOURCES_EXEC=$(SOURCES1)
SOURCES_ALL=$(SOURCES1) $(SOURCES3)

EXEC = texprime

DOC=Typography/Fonts.mli
########## Targets ##########

OBJS = $(filter %.cmo, $(SOURCES_EXEC:.ml=.cmo))
OPTOBJS = $(OBJS:.cmo=.cmx)

FORMATS_ML=$(filter %.ml, $(SOURCES3))

########## Compilers ##########

OPTS=-I Format
CAMLC = ocamlfind ocamlc -package camomile,dyp -linkpkg $(OPTS) -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp -linkpkg $(OPTS) -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp $(OPTS) -pp "cpp -w"

CAMLMKTOP = ocamlfind ocamlmktop -package camomile -package dyp -linkpkg $(OPTS) -pp "cpp -w"
CAMLDOC = ocamlfind ocamldoc -package camomile -package dyp -html -I Fonts $(OPTS) -pp "cpp -w"
CAMLDEP = ocamlfind ocamldep -pp "cpp -w"

.PHONY:Typography/Typography.cma Typography/Typography.cmxa
###############################

all : $(EXEC) $(EXEC).opt Typography/Typography.cma Typography/Typography.cmxa DefaultFormat.cma DefaultFormat.cmxa texprimeDefault.tgo texprimeDefault.tgx
	cp $(EXEC).opt $(EXEC)

opt : $(EXEC).opt Typography/Typography.cmxa DefaultFormat.cmxa texprimeDefault.tgx
	cp $(EXEC).opt $(EXEC)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) dynlink.cmxa Typography/Typography.cmxa $(CUSTOM) -o $(EXEC).opt $(OPTOBJS)

$(EXEC): $(OBJS)
	$(CAMLC) dynlink.cma Typography/Typography.cma $(CUSTOM) -o $(EXEC) $(OBJS)


Typography/Typography.cma:
	make -C Typography Typography.cma
	cp Typography/Typography.cmi .
	cp Typography/Typography.cma .
Typography/Typography.cmxa:
	make -C Typography Typography.cmxa
	cp Typography/Typography.cmi .
	cp Typography/Typography.cmxa .
	cp Typography/Typography.a .



DefaultFormat.mli:
	$(CAMLC) -i DefaultFormat.ml > DefaultFormat.mli

DefaultFormat.cma: Typography/Typography.cma $(filter %.cmo, $(FORMATS_ML:.ml=.cmo))
	$(CAMLC) -a -o DefaultFormat.cma $(filter %.cmo, $(FORMATS_ML:.ml=.cmo))

DefaultFormat.cmxa: Typography/Typography.cmxa $(filter %.cmx, $(FORMATS_ML:.ml=.cmx))
	$(CAMLOPTA) -a -o DefaultFormat.cmxa $(filter %.cmx, $(FORMATS_ML:.ml=.cmx))

graphics_font: $(FONTS:.ml=.cmo) $(BASE:.ml=.cmo) tests/graphics_font.ml
	$(CAMLC) -o graphics_font graphics.cma $(BASE:.ml=.cmo) $(FONTS:.ml=.cmo) tests/graphics_font.ml

graphics.opt: tests/graphics_font.ml $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx)
	$(CAMLOPT) graphics.cmxa -o graphics.opt $(BASE:.ml=.cmx) $(FONTS:.ml=.cmx) tests/graphics_font.ml


doc:Makefile $(filter %.cmo, $(DOC:.ml=.cmo)) $(filter %.cmi, $(DOC:.mli=.cmi))
	mkdir -p doc_html
	$(CAMLDOC) -d doc_html $(DOC)


texprimeDefault.tgo: DefaultGrammar.pdf
	true

texprimeDefault.tgx: DefaultGrammar.opt.pdf
	true

DefaultGrammar.pdf: texprime DefaultFormat.cmx Typography/Typography.cma DefaultGrammar.txp
	./texprime --ml --extra-fonts-dir ../Otf DefaultGrammar.txp
	$(CAMLC)  -o DefaultGrammar.tmx Typography/Typography.cma DefaultFormat.cma -impl DefaultGrammar.tml
	./DefaultGrammar.tmx --extra-fonts-dir ../Otf

DefaultGrammar.opt.pdf: texprime.opt DefaultFormat.cmxa Typography/Typography.cmxa DefaultGrammar.txp
	./texprime.opt --ml --extra-fonts-dir ../Otf DefaultGrammar.txp
	$(CAMLOPT)  -o DefaultGrammar.tmx Typography/Typography.cmxa DefaultFormat.cmxa  -impl DefaultGrammar.tml
	./DefaultGrammar.tmx --extra-fonts-dir ../Otf
	mv DefaultGrammar.pdf DefaultGrammar.opt.pdf

%.pdf: texprime Typography/Typography.cma DefaultFormat.cma texprimeDefault.tgo %.txp
	./texprime.opt --ml --extra-fonts-dir ../Otf $*.txp
	$(CAMLC)  -o $*.tmx Typography/Typography.cma DefaultFormat.cma  -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf

%.opt.pdf: texprime.opt Typography/Typography.cmxa DefaultFormat.cmxa texprimeDefault.tgx %.txp
	./texprime.opt --ml --extra-fonts-dir ../Otf $*.txp
	$(CAMLOPT)  -o $*.tmx Typography/Typography.cmxa DefaultFormat.cmxa  -impl $*.tml
	./$*.tmx --extra-fonts-dir ../Otf
	cp $*.pdf $*.opt.pdf

%.ml: %.dyp
	dypgen --no-mli $<

%.cmo: %.ml Typography/Typography.cma
	$(CAMLC) -c Typography/Typography.cma -o $@ $<

%.cmi: %.mli
	$(CAMLC) -c $<

%.cmx: %.ml Typography/Typography.cmxa
	$(CAMLOPT) -c Typography/Typography.cmxa $<


clean:
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# DefaultFormat.mli 
	make -C Typography clean
	make -C Format clean
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

.depend: $(SOURCES_ALL) Parser.ml
	@echo '--Re-building dependencies'
	$(CAMLDEP)  $(SOURCES_ALL) > .depend


include .depend
