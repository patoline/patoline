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

OPTS=-I Format
CAMLC = ocamlfind ocamlc -package camomile,dyp,Typography -linkpkg $(OPTS) -pp "cpp -w" -g
CAMLOPT = ocamlfind ocamlopt -package camomile,dyp,Typography -linkpkg $(OPTS) str.cmxa FormatThese.cmxa -pp "cpp -w"
CAMLOPTA = ocamlfind ocamlopt -package camomile,dyp,Typography  $(OPTS) -pp "cpp -w"

CAMLMKTOP = ocamlfind ocamlmktop -package camomile -package dyp -linkpkg $(OPTS) -pp "cpp -w"
CAMLDOC = ocamlfind ocamldoc -package camomile -package dyp -html -I Fonts $(OPTS) -pp "cpp -w"
CAMLDEP = ocamlfind ocamldep -pp "cpp -w"

.PHONY:Format Typography/Typography.cma Typography/Typography.cmxa
###############################

%.pdf: %.txp
	/usr/bin/texprime --format FormatThese $*.txp

%.tml: %.txp
	/usr/bin/texprime --format FormatThese --ml $*.txp

%.tmx: %.tml
	$(CAMLOPT)  -o $*.tmx -impl $*.tml

Format:
	cd Format ; make clean ; make Article ; make Diagram.cmxa ; make Diagram.cmi ; cd ..

clean:
	rm -f *.cm[ioxa] *.cmxa *.o *.a *~ \#*\# 
