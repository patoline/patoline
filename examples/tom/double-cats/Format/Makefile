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

OPTS=
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography -linkpkg $(OPTS) str.cma -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography -linkpkg $(OPTS) str.cmxa -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp,Typography  $(OPTS) -pp "cpp -w"

###############################

all : $(EXEC) $(EXEC).opt Typography/Typography.cma Typography/Typography.cmxa DefaultFormat.cma DefaultFormat.cmxa ArticleFormat.cma ArticleFormat.cmxa texprimeDefault.tgo texprimeDefault.tgx
	cp $(EXEC).opt $(EXEC)

opt : $(EXEC).opt Typography/Typography.cmxa DefaultFormat.cmxa ArticleFormat.cmxa texprimeDefault.tgx
	cp $(EXEC).opt $(EXEC)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) dynlink.cmxa Typography/Typography.cmxa $(CUSTOM) -o $(EXEC).opt $(OPTOBJS)

$(EXEC): $(OBJS)
	$(CAMLC) dynlink.cma Typography/Typography.cma $(CUSTOM) -o $(EXEC) $(OBJS)

%.mli: %.ml
	$(CAMLC) -i $*.ml > $*.mli

Article.cma: 
	$(CAMLC) -a -o Article.cma $(filter %.cmo, $(FORMATS_ML:.ml=.cmo))

Article.cmxa:
	$(CAMLOPTA) -a -o Article.cmxa Article.cmx

Article: Article.cmi Article.cmx Article.cmxa 

%.ml: %.dyp
	dypgen --no-mli $<

%.cmi: %.mli
	$(CAMLC) -c $< 

%.cma: %.cmi
	$(CAMLC) -a -o $*.cma $(filter %.cmo, $(FORMATS_ML:.ml=.cmo))

%.cmx: %.ml %.cmi
	$(CAMLOPT) -c $<

%.cmxa: %.cmx %.cmi
	$(CAMLOPTA) -a -o $@ $*.cmx


clean:
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# Article.mli
