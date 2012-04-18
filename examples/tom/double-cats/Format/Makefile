SOURCES1 = Parser.ml Texprime.ml Main.ml
SOURCES3 = DefaultFormat.ml 

SOURCES_EXEC=$(SOURCES1)
SOURCES_ALL=$(SOURCES1) $(SOURCES3)

EXEC = texprime

DOC=Typography/Fonts.mli
########## Targets ##########

OBJS = $(filter %.cmo, $(SOURCES_EXEC:.ml=.cmo))
OPTOBJS = $(OBJS:.cmo=.cmx)

FORMATS_ML=$(filter %.ml, $(SOURCES3))

########## Compilers ##########

OPTS= -I +bibi
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography -linkpkg $(OPTS) str.cma -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography -linkpkg $(OPTS) str.cmxa -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp,Typography  $(OPTS) -pp "cpp -w"

###############################

all : Article Diagram

opt : $(EXEC).opt Typography/Typography.cmxa DefaultFormat.cmxa ArticleFormat.cmxa texprimeDefault.tgx
	cp $(EXEC).opt $(EXEC)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) dynlink.cmxa Typography/Typography.cmxa $(CUSTOM) -o $(EXEC).opt $(OPTOBJS)

$(EXEC): $(OBJS)
	$(CAMLC) dynlink.cma Typography/Typography.cma $(CUSTOM) -o $(EXEC) $(OBJS)

%.mli: %.ml
	$(CAMLC) $(OPTS) -i $*.ml > $*.mli

Article: Article.cmi Article.cmx Article.cmxa 

Diagram: Diagram.cmi Diagram.cmx Diagram.cmxa 

%.ml: %.dyp
	dypgen --no-mli $<

%.cmi: %.mli
	$(CAMLC) $(OPTS) -c $< 

%.cma: %.cmi
	$(CAMLC) $(OPTS) -a -o $*.cma $(filter %.cmo, $(FORMATS_ML:.ml=.cmo))

%.cmx: %.ml %.cmi
	$(CAMLOPT) $(OPTS) -c $< 

%.cmxa: %.cmx %.cmi
	$(CAMLOPTA) $(OPTS) -a -o $@ $*.cmx


clean:
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# Article.mli
