SOURCES = calc_parser.dyp calc.ml
REP = -package dyp
CAMLC = ocamlfind ocamlopt -g $(REP)
DYPGEN = dypgen --pv-obj --no-mli
LIBS=dyp.cmxa

all: calc

SOURCES1 = $(SOURCES:.mll=.ml)
SOURCES2 = $(SOURCES1:.dyp=.ml)
OBJS = $(SOURCES2:.ml=.cmx)

calc: $(OBJS)
	$(CAMLC) -o calc $(LIBS) $(OBJS)

.SUFFIXES: .ml .cmx .dyp

.ml.cmx:
	$(CAMLC) -c $<

.dyp.ml:
	$(DYPGEN) $<

clean:
	rm -f *.cm[iox] *~ .*~ *.o
	rm -f calc
	rm -f *.extract_type *_temp.ml
	rm -f *parser.ml *parser.mli
